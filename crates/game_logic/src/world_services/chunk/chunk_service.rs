use crate::world_services::chunk::chunk_struct::*;
use crate::world_services::chunk::chunk_utils::*;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::*;
use bevy::tasks::futures_lite::future;
use bevy::tasks::AsyncComputeTaskPool;
use bevy_rapier3d::prelude::*;
use game_core::configuration::{GameConfig, WorldGenConfig};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{id_any, BlockRegistry};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use game_core::world::save::{RegionCache, WorldSave};
use std::collections::HashMap;

const MAX_APPLY_PER_FRAME: usize = 14;

#[derive(Resource, Default)]
struct ChunkColliderIndex(pub HashMap<(IVec2, u8), Entity>);

pub struct ChunkService;

impl Plugin for ChunkService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<ChunkMeshIndex>()
            .init_resource::<MeshBacklog>()
            .init_resource::<PendingGen>()
            .init_resource::<PendingMesh>()
            .init_resource::<ChunkColliderIndex>()
            .add_systems(Update, (
                schedule_chunk_generation,
                collect_generated_chunks,
                collect_meshed_subchunks,
                schedule_remesh_tasks_from_events.in_set(VoxelStage::Meshing),
                drain_mesh_backlog,
                unload_far_chunks,
            )
                .chain()
                .run_if(in_state(AppState::Loading)
                    .or(in_state(AppState::InGame(InGameStates::Game)))));

        app.add_systems(
            Update,
            check_initial_world_ready
                .run_if(in_state(AppState::Loading))
        );
    }
}

fn check_initial_world_ready(
    game_config: Res<GameConfig>,
    load_center: Res<LoadCenter>,
    chunk_map: Res<ChunkMap>,
    pending_gen: Res<PendingGen>,
    pending_mesh: Res<PendingMesh>,
    backlog: Res<MeshBacklog>,
    mut next: ResMut<NextState<AppState>>,
    mut commands: Commands,
) {
    let initial_radius = game_config.graphics.chunk_range.min(3);

    if area_ready(load_center.world_xz, initial_radius, &chunk_map, &pending_gen, &pending_mesh, &backlog) {
        commands.remove_resource::<LoadCenter>();
        next.set(AppState::InGame(InGameStates::Game));
    }
}

//System
fn schedule_chunk_generation(
    mut pending: ResMut<PendingGen>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    gen_cfg: Res<WorldGenConfig>,
    game_config: Res<GameConfig>,
    ws: Res<WorldSave>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
    load_center: Option<Res<LoadCenter>>,
) {
    let center_c = if let Ok(t) = q_cam.single() {
        let (c, _) = world_to_chunk_xz(t.translation().x.floor() as i32, t.translation().z.floor() as i32);
        c
    } else if let Some(lc) = load_center {
        lc.world_xz
    } else {
        IVec2::ZERO
    };

    if !can_spawn_gen(&pending) { return; }

    let load_radius = game_config.graphics.chunk_range;
    let mut budget = MAX_INFLIGHT_GEN.saturating_sub(pending.0.len()).min(8);

    let ids = (
        id_any(&reg, &["grass_block","grass"]),
        id_any(&reg, &["dirt_block","dirt"]),
        id_any(&reg, &["stone_block","stone"]),
    );
    let cfg_clone = gen_cfg.clone();
    let ws_root = ws.root.clone();
    let pool = AsyncComputeTaskPool::get();

    for dz in -load_radius..=load_radius {
        for dx in -load_radius..=load_radius {
            if budget == 0 { return; }
            let c = IVec2::new(center_c.x + dx, center_c.y + dz);
            if chunk_map.chunks.contains_key(&c) || pending.0.contains_key(&c) { continue; }

            let ids_copy = ids;
            let cfg = cfg_clone.clone();
            let root = ws_root.clone();
            let task = pool.spawn(async move {
                let data = load_or_gen_chunk_async(root, c, ids_copy, cfg).await;
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
    mut collider_index: ResMut<ChunkColliderIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    reg: Res<BlockRegistry>,
    mut chunk_map: ResMut<ChunkMap>,
    q_mesh: Query<&Mesh3d>,
) {
    let mut done_keys = Vec::new();
    let mut applied = 0;

    for (key, task) in pending_mesh.0.iter_mut() {
        if applied >= MAX_APPLY_PER_FRAME { break; }

        if let Some(((coord, sub), builds)) = future::block_on(future::poll_once(task)) {
            let old_keys: Vec<_> = mesh_index
                .map
                .keys()
                .cloned()
                .filter(|(c, s, _)| c == &coord && *s as usize == sub)
                .collect();
            despawn_mesh_set(old_keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

            if let Some(ent) = collider_index.0.remove(&(coord, sub as u8)) {
                commands.entity(ent).despawn();
            }

            let s = 1.0;
            let origin = Vec3::new(
                (coord.x * CX as i32) as f32 * s,
                (Y_MIN as f32) * s,
                (coord.y * CZ as i32) as f32 * s,
            );

            let mut phys_positions: Vec<[f32; 3]> = Vec::new();
            let mut phys_indices:   Vec<u32>       = Vec::new();

            for (bid, mb) in builds {
                if mb.pos.is_empty() { continue; }

                {
                    let base = phys_positions.len() as u32;
                    phys_positions.extend_from_slice(&mb.pos);
                    phys_indices.extend(mb.idx.iter().map(|i| base + *i));
                }

                let mesh = mb.into_mesh();
                let ent = commands
                    .spawn((
                        Mesh3d(meshes.add(mesh)),
                        MeshMaterial3d(reg.material(bid)),
                        Transform::from_translation(origin),
                        SubchunkMesh { coord, sub: sub as u8, block: bid },
                        Name::new(format!("chunk({},{}) sub{} block{}", coord.x, coord.y, sub, bid)),
                    ))
                    .id();
                mesh_index.map.insert((coord, sub as u8, bid), ent);
            }

            if !phys_positions.is_empty() {
                let mut pm = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
                pm.insert_attribute(Mesh::ATTRIBUTE_POSITION, phys_positions);
                pm.insert_indices(Indices::U32(phys_indices));

                let flags = TriMeshFlags::FIX_INTERNAL_EDGES
                    | TriMeshFlags::MERGE_DUPLICATE_VERTICES
                    | TriMeshFlags::DELETE_DEGENERATE_TRIANGLES
                    | TriMeshFlags::ORIENTED;

                if let Some(collider) = Collider::from_bevy_mesh(&pm, &ComputedColliderShape::TriMesh(flags)) {
                    let cent = commands
                        .spawn((
                            RigidBody::Fixed,
                            collider,
                            Transform::from_translation(origin),
                            Name::new(format!("collider chunk({},{}) sub{}", coord.x, coord.y, sub)),
                        ))
                        .id();
                    collider_index.0.insert((coord, sub as u8), cent);
                }

            }

            if let Some(chunk) = chunk_map.chunks.get_mut(&coord) {
                chunk.clear_dirty(sub);
            }

            applied += 1;
            done_keys.push(*key);
        }
    }

    for k in done_keys {
        pending_mesh.0.remove(&k);
    }
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
    mut collider_index: ResMut<ChunkColliderIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    game_config: Res<GameConfig>,
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    q_mesh: Query<&Mesh3d>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    let keep_radius = game_config.graphics.chunk_range + 1;

    let to_remove: Vec<IVec2> = chunk_map
        .chunks
        .keys()
        .filter(|coord| {
            (coord.x - center_c.x).abs() > keep_radius
                || (coord.y - center_c.y).abs() > keep_radius
        })
        .cloned()
        .collect();

    for coord in &to_remove {
        if let Some(chunk) = chunk_map.chunks.get(coord) {
            let _ = save_chunk_sync(&ws, &mut cache, *coord, chunk);
        }

        pending_gen.0.remove(coord);
        pending_mesh.0.retain(|(c, _), _| c != coord);

        let keys: Vec<_> = mesh_index
            .map
            .keys()
            .cloned()
            .filter(|(c, _, _)| c == coord)
            .collect();
        despawn_mesh_set(keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

        let col_keys: Vec<_> = collider_index
            .0
            .keys()
            .cloned()
            .filter(|(c, _)| c == coord)
            .collect();
        for k in col_keys {
            if let Some(ent) = collider_index.0.remove(&k) {
                commands.entity(ent).despawn();
            }
        }

        chunk_map.chunks.remove(coord);
        backlog.0.retain(|(c, _)| c != coord);
    }
}

