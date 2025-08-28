use std::collections::HashMap;
use crate::world_services::chunk::chunk_utils::*;
use bevy::prelude::*;
use bevy::tasks::AsyncComputeTaskPool;
use bevy::tasks::futures_lite::future;
use game_core::configuration::{GameConfig, WorldGenConfig};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{id_any, BlockId, BlockRegistry};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use crate::world_services::chunk::chunk_struct::*;

const MAX_INFLIGHT_MESH: usize = 64;
const MAX_INFLIGHT_GEN:  usize = 32;

#[derive(Resource, Default)]
struct ChunkMeshIndex {
    map: HashMap<(IVec2, u8, BlockId), Entity>,
}

pub struct ChunkService;

impl Plugin for ChunkService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<ChunkMeshIndex>()
            .init_resource::<MeshBacklog>()
            .init_resource::<PendingGen>()
            .init_resource::<PendingMesh>()
            .add_systems(Update, (
                schedule_chunk_generation,
                collect_generated_chunks,
                collect_meshed_subchunks,
                schedule_remesh_tasks_from_events.in_set(VoxelStage::Meshing),
                drain_mesh_backlog,
                unload_far_chunks,
            ).chain()
                .run_if(in_state(AppState::InGame(InGameStates::Game))));
    }
}

//System
fn schedule_chunk_generation(
    mut pending: ResMut<PendingGen>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    gen_cfg: Res<WorldGenConfig>,
    game_config: Res<GameConfig>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    if !can_spawn_gen(&pending) { return; }

    let ids = (
        id_any(&reg, &["grass_block","grass"]),
        id_any(&reg, &["dirt_block","dirt"]),
        id_any(&reg, &["stone_block","stone"]),
    );
    let cfg_clone = gen_cfg.clone();

    let mut budget = MAX_INFLIGHT_GEN.saturating_sub(pending.0.len()).min(8);
    let load_radius = game_config.graphics.chunk_range;

    for dz in -load_radius..=load_radius {
        for dx in -load_radius..=load_radius {
            if budget == 0 { return; }
            let c = IVec2::new(center_c.x + dx, center_c.y + dz);
            if chunk_map.chunks.contains_key(&c) || pending.0.contains_key(&c) { continue; }

            let pool = AsyncComputeTaskPool::get();
            let ids_copy = ids;
            let cfg = cfg_clone.clone();
            let task = pool.spawn(async move {
                let data = generate_chunk_async_noise(c, ids_copy, cfg).await;
                (c, data)
            });
            pending.0.insert(c, task);
            budget -= 1;
        }
    }
}

//System
fn drain_mesh_backlog(
    mut backlog: ResMut<MeshBacklog>,
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    if chunk_map.chunks.is_empty() { backlog.0.clear(); return; }

    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    while can_spawn_mesh(&pending_mesh) {
        let Some((coord, sub)) = backlog.0.pop_front() else { break; };
        if pending_mesh.0.contains_key(&(coord, sub)) { continue; }
        let Some(chunk) = chunk_map.chunks.get(&coord) else { continue; };

        let chunk_copy = chunk.clone();
        let reg_copy   = reg_lite.clone();
        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = snapshot_borders(&chunk_map, coord, y0, y1);

        let key = (coord, sub);
        let t = pool.spawn(async move {
            let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
            (key, builds)
        });
        pending_mesh.0.insert(key, t);
    }
}

//System
fn collect_generated_chunks(
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    mut chunk_map: ResMut<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    let reg_lite = RegLite::from_reg(&reg);
    let mut finished = Vec::new();

    for (coord, task) in pending_gen.0.iter_mut() {
        if let Some((c, data)) = future::block_on(future::poll_once(task)) {
            chunk_map.chunks.insert(c, data.clone());

            // Subchunks dieses Chunks
            let pool = AsyncComputeTaskPool::get();
            for sub in 0..SEC_COUNT {
                let key = (c, sub);
                let y0 = sub * SEC_H;
                let y1 = (y0 + SEC_H).min(CY);
                let borders = snapshot_borders(&chunk_map, c, y0, y1);

                if can_spawn_mesh(&pending_mesh) {
                    let chunk_copy = data.clone();
                    let reg_copy = reg_lite.clone();
                    let t = pool.spawn(async move {
                        let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                        ((c, sub), builds)
                    });
                    pending_mesh.0.insert(key, t);
                } else {
                    enqueue_mesh(&mut backlog, &pending_mesh, key);
                }
            }

            let neigh = [
                IVec2::new(c.x + 1, c.y),
                IVec2::new(c.x - 1, c.y),
                IVec2::new(c.x, c.y + 1),
                IVec2::new(c.x, c.y - 1),
            ];
            for n_coord in neigh {
                if let Some(n_chunk) = chunk_map.chunks.get(&n_coord) {
                    for sub in 0..SEC_COUNT {
                        let key = (n_coord, sub);
                        if pending_mesh.0.contains_key(&key) { continue; }

                        let y0 = sub * SEC_H;
                        let y1 = (y0 + SEC_H).min(CY);
                        let borders = snapshot_borders(&chunk_map, n_coord, y0, y1);

                        if can_spawn_mesh(&pending_mesh) {
                            let pool = AsyncComputeTaskPool::get();
                            let reg_copy = reg_lite.clone();
                            let chunk_copy = n_chunk.clone();
                            let t = pool.spawn(async move {
                                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                                (key, builds)
                            });
                            pending_mesh.0.insert(key, t);
                        } else {
                            enqueue_mesh(&mut backlog, &pending_mesh, key);
                        }
                    }
                }
            }

            finished.push(*coord);
        }
    }

    for c in finished { pending_gen.0.remove(&c); }
}

//System
fn collect_meshed_subchunks(
    mut commands: Commands,
    mut pending_mesh: ResMut<PendingMesh>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    reg: Res<BlockRegistry>,
    mut chunk_map: ResMut<ChunkMap>,
    q_mesh: Query<&Mesh3d>,
) {
    let mut done_keys = Vec::new();

    for (key, task) in pending_mesh.0.iter_mut() {
        if let Some(((coord, sub), builds)) = future::block_on(future::poll_once(task)) {
            let old_keys: Vec<_> = mesh_index.map
                .keys()
                .cloned()
                .filter(|(c, s, _)| c == &coord && *s as usize == sub)
                .collect();

            despawn_mesh_set(old_keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

            let s = 1.0;
            let origin = Vec3::new(
                (coord.x * CX as i32) as f32 * s,
                (Y_MIN as f32) * s,
                (coord.y * CZ as i32) as f32 * s,
            );

            for (bid, mb) in builds {
                if mb.pos.is_empty() { continue; }
                let mesh = mb.into_mesh();
                let ent = commands.spawn((
                    Mesh3d(meshes.add(mesh)),
                    MeshMaterial3d(reg.material(bid)),
                    Transform::from_translation(origin),
                    SubchunkMesh { coord, sub: sub as u8, block: bid },
                    Name::new(format!("chunk({},{}) sub{} block{}", coord.x, coord.y, sub, bid)),
                )).id();
                mesh_index.map.insert((coord, sub as u8, bid), ent);
            }

            if let Some(chunk) = chunk_map.chunks.get_mut(&coord) {
                chunk.clear_dirty(sub);
            }

            done_keys.push(*key);
        }
    }

    for k in done_keys { pending_mesh.0.remove(&k); }
}

//System
fn schedule_remesh_tasks_from_events(
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    mut backlog: ResMut<MeshBacklog>,
    mut ev_dirty: EventReader<SubchunkDirty>,
) {
    if chunk_map.chunks.is_empty() {
        ev_dirty.clear();
        return;
    }

    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    for e in ev_dirty.read().copied() {
        let coord = e.coord;
        let sub   = e.sub;
        let key   = (coord, sub);

        if pending_mesh.0.contains_key(&key) { continue; }

        let Some(chunk) = chunk_map.chunks.get(&coord) else {
            enqueue_mesh(&mut backlog, &pending_mesh, key);
            continue;
        };

        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = snapshot_borders(&chunk_map, coord, y0, y1);

        if can_spawn_mesh(&pending_mesh) {
            let chunk_copy = chunk.clone();
            let reg_copy   = reg_lite.clone();

            let t = pool.spawn(async move {
                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                (key, builds)
            });

            pending_mesh.0.insert(key, t);
        } else {
            enqueue_mesh(&mut backlog, &pending_mesh, key);
        }
    }
}

//System
fn unload_far_chunks(
    mut commands: Commands,
    mut chunk_map: ResMut<ChunkMap>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    game_config: Res<GameConfig>,
    q_mesh: Query<&Mesh3d>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    let keep_radius = game_config.graphics.chunk_range + 1;

    let to_remove: Vec<IVec2> = chunk_map.chunks
        .keys()
        .filter(|coord| {
            (coord.x - center_c.x).abs() > keep_radius
                || (coord.y - center_c.y).abs() > keep_radius
        })
        .cloned()
        .collect();

    for coord in &to_remove {
        pending_gen.0.remove(coord);
        pending_mesh.0.retain(|(c,_), _| c != coord);

        let keys: Vec<_> = mesh_index.map
            .keys()
            .cloned()
            .filter(|(c, _, _)| c == coord)
            .collect();

        despawn_mesh_set(keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

        chunk_map.chunks.remove(coord);
        backlog.0.retain(|(c, _)| c != coord);
    }
}

fn snapshot_borders(chunk_map: &ChunkMap, coord: IVec2, y0: usize, y1: usize) -> BorderSnapshot {
    let mut snap = BorderSnapshot { y0, y1, east: None, west: None, south: None, north: None };

    let take_xz = |c: &ChunkData, x: usize, z: usize, y: usize| -> BlockId { c.get(x,y,z) };

    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x + 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, 0, z, y)); } }
        snap.east = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x - 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, CX-1, z, y)); } }
        snap.west = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y + 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, 0, y)); } }
        snap.south = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y - 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, CZ-1, y)); } }
        snap.north = Some(v);
    }
    snap
}

fn despawn_mesh_set(
    keys: impl IntoIterator<Item = (IVec2, u8, BlockId)>,
    mesh_index: &mut ChunkMeshIndex,
    commands: &mut Commands,
    q_mesh: &Query<&Mesh3d>,
    meshes: &mut Assets<Mesh>,
) {
    for key in keys {
        if let Some(ent) = mesh_index.map.remove(&key) {
            if let Ok(Mesh3d(handle)) = q_mesh.get(ent) {
                meshes.remove(handle.id());
            }
            commands.entity(ent).despawn();
        }
    }
}

fn can_spawn_mesh(pending_mesh: &PendingMesh) -> bool {
    pending_mesh.0.len() < MAX_INFLIGHT_MESH
}
fn can_spawn_gen(pending_gen: &PendingGen) -> bool {
    pending_gen.0.len() < MAX_INFLIGHT_GEN
}

fn backlog_contains(backlog: &MeshBacklog, key: (IVec2, usize)) -> bool {
    backlog.0.iter().any(|&k| k == key)
}

fn enqueue_mesh(backlog: &mut MeshBacklog, pending: &PendingMesh, key: (IVec2, usize)) {
    if pending.0.contains_key(&key) { return; }
    if backlog_contains(backlog, key) { return; }
    backlog.0.push_back(key);
}