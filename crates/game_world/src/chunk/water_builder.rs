use crate::chunk::water_utils::*;
use bevy::pbr::{NotShadowCaster, NotShadowReceiver};
use bevy::prelude::*;
use bevy::tasks::AsyncComputeTaskPool;
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::block::{id_any, BlockRegistry, VOXEL_SIZE};
use game_core::world::chunk::{ChunkMap, SubchunkDirty, WaterChunkUnload};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidChunk, FluidMap, WaterMeshIndex};
use game_core::world::save::{RegionCache, WorldSave};
use std::collections::{HashMap, HashSet, VecDeque};

const WATER_GEN_BUDGET_PER_FRAME: usize = 48;
const MAX_INFLIGHT_WATER_LOAD: usize = 32;

const MAX_INFLIGHT_WATER_MESH: usize = 64;
const MAX_WATER_APPLY_PER_FRAME: usize = 12;

#[derive(Resource, Default)]
struct WaterGenQueue {
    work: VecDeque<IVec2>,
}

#[derive(Resource, Default)]
struct WaterSeen {
    known: HashSet<IVec2>,
}

#[derive(Resource, Default)]
struct WaterMeshingTodo(pub HashSet<IVec2>);

#[derive(Resource, Default)]
struct PendingWaterLoad(
    pub HashMap<IVec2, bevy::tasks::Task<(IVec2, FluidChunk)>>
);

#[derive(Resource, Default)]
struct WaterMeshBacklog(pub VecDeque<(IVec2, usize)>);

#[derive(Resource, Default)]
struct PendingWaterMesh(
    pub HashMap<(IVec2, usize), bevy::tasks::Task<((IVec2, usize), WaterMeshBuild)>>
);

pub struct WaterBuilder;

impl Plugin for WaterBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<WaterGenQueue>()
            .init_resource::<WaterSeen>()
            .init_resource::<WaterMeshIndex>()
            .init_resource::<FluidMap>()
            .init_resource::<WaterMeshingTodo>()
            .init_resource::<PendingWaterLoad>()
            .init_resource::<WaterMeshBacklog>()
            .init_resource::<PendingWaterMesh>()
            .add_event::<WaterChunkUnload>()
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::WaterGen)),
                water_gen_build_worklist
            )
            .add_systems(
                Update,
                (
                    water_mark_from_dirty,
                    water_track_new_chunks,

                    // Gen/Load
                    schedule_water_generation_jobs,
                    collect_water_generation_jobs,

                    // Mesh
                    water_backlog_from_todo,
                    water_drain_mesh_backlog,
                    water_collect_meshed_subchunks,

                    // Unload & Co.
                    water_unload_on_event,
                )
                    .run_if(
                        in_state(AppState::Loading(LoadingStates::WaterGen))
                            .or(in_state(AppState::InGame(InGameStates::Game)))
                    ),
            );
    }
}

fn water_gen_build_worklist(
    mut q: ResMut<WaterGenQueue>,
    mut seen: ResMut<WaterSeen>,
    chunk_map: Res<ChunkMap>,
) {
    q.work.clear();
    seen.known.clear();

    for &c in chunk_map.chunks.keys() {
        if seen.known.insert(c) {
            q.work.push_back(c);
        }
    }
}

fn water_track_new_chunks(
    mut q: ResMut<WaterGenQueue>,
    mut seen: ResMut<WaterSeen>,
    chunk_map: Res<ChunkMap>,
) {
    for &c in chunk_map.chunks.keys() {
        if seen.known.insert(c) {
            q.work.push_back(c);
        }
    }
}

fn schedule_water_generation_jobs(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    ws: Res<WorldSave>,
    gen_cfg: Res<WorldGenConfig>,
    mut pending: ResMut<PendingWaterLoad>,
) {
    if chunk_map.chunks.is_empty() { q.work.clear(); return; }

    let mut budget = WATER_GEN_BUDGET_PER_FRAME
        .min(MAX_INFLIGHT_WATER_LOAD.saturating_sub(pending.0.len()));
    if budget == 0 { return; }

    let pool = AsyncComputeTaskPool::get();
    let sea_level = 62i32;
    let seed = gen_cfg.seed as u32;

    while budget > 0 {
        let Some(coord) = q.work.pop_front() else { break; };
        if water.0.contains_key(&coord) { continue; }
        let Some(chunk) = chunk_map.chunks.get(&coord) else { continue; };
        if pending.0.contains_key(&coord) { continue; }

        let chunk_copy = chunk.clone();
        let root = ws.root.clone();

        let task = pool.spawn(async move {
            if let Some(wc) = load_water_chunk_from_disk(root.clone(), coord) {
                return (coord, wc);
            }
            let wc = generate_water_for_chunk(coord, &chunk_copy, sea_level, seed, false);
            (coord, wc)
        });

        pending.0.insert(coord, task);
        budget -= 1;
    }
}

fn collect_water_generation_jobs(
    mut pending: ResMut<PendingWaterLoad>,
    chunk_map: Res<ChunkMap>,
    mut water: ResMut<FluidMap>,
    mut to_mesh: ResMut<WaterMeshingTodo>,
    mut next: ResMut<NextState<AppState>>,
    app_state: Res<State<AppState>>,
    q: Res<WaterGenQueue>,
) {
    use bevy::tasks::futures_lite::future;

    let mut done: Vec<IVec2> = Vec::new();
    let mut applied = 0usize;

    for (_, task) in pending.0.iter_mut() {
        if applied >= WATER_GEN_BUDGET_PER_FRAME { break; }

        if let Some((c, wc)) = future::block_on(future::poll_once(task)) {
            if chunk_map.chunks.contains_key(&c) {
                water.0.insert(c, wc);
                to_mesh.0.insert(c);
                for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] { to_mesh.0.insert(c + d); }
                applied += 1;
            }
            done.push(c);
        }
    }

    for c in done { pending.0.remove(&c); }

    if matches!(app_state.get(), AppState::Loading(LoadingStates::WaterGen))
        && q.work.is_empty()
        && pending.0.is_empty()
    {
        next.set(AppState::InGame(InGameStates::Game));
    }
}

fn water_mark_from_dirty(
    mut ev: EventReader<SubchunkDirty>,
    mut todo: ResMut<WaterMeshingTodo>,
) {
    for e in ev.read() {
        todo.0.insert(e.coord);
        for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
            todo.0.insert(e.coord + d);
        }
    }
}

fn water_unload_on_event(
    mut commands: Commands,
    mut ev: EventReader<WaterChunkUnload>,
    mut water: ResMut<FluidMap>,
    mut windex: ResMut<WaterMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    q_mesh: Query<&Mesh3d>,
    mut q: ResMut<WaterGenQueue>,
    mut seen: ResMut<WaterSeen>,
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
) {
    for WaterChunkUnload { coord } in ev.read().copied() {
        if let Some(wc) = water.0.remove(&coord) {
            save_water_chunk_sync(&ws, &mut cache, coord, &wc);
        }

        let keys: Vec<_> = windex.0.keys().copied().filter(|(c, _)| *c == coord).collect();
        for key in keys {
            despawn_water_mesh(key, &mut windex, &mut commands, &q_mesh, &mut meshes);
        }

        q.work.retain(|c| *c != coord);
        seen.known.remove(&coord);
    }
}

fn water_backlog_from_todo(
    mut todo: ResMut<WaterMeshingTodo>,
    water: Res<FluidMap>,
    mut backlog: ResMut<WaterMeshBacklog>,
) {
    if todo.0.is_empty() { return; }
    let coords: Vec<_> = todo.0.drain().collect();
    for coord in coords {
        if let Some(fc) = water.0.get(&coord) {
            for sub in 0..SEC_COUNT {
                if fc.sub_has_any(sub) {
                    let key = (coord, sub);
                    if !backlog.0.iter().any(|k| *k == key) { backlog.0.push_back(key); }
                }
            }
        } else {
            for sub in 0..SEC_COUNT {
                let key = (coord, sub);
                if !backlog.0.iter().any(|k| *k == key) { backlog.0.push_back(key); }
            }
        }
    }
}

fn water_drain_mesh_backlog(
    mut backlog: ResMut<WaterMeshBacklog>,
    mut pending: ResMut<PendingWaterMesh>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
) {
    if pending.0.len() >= MAX_INFLIGHT_WATER_MESH { return; }
    let pool = AsyncComputeTaskPool::get();

    while pending.0.len() < MAX_INFLIGHT_WATER_MESH {
        let Some((coord, sub)) = backlog.0.pop_front() else { break; };

        let Some(fc) = water.0.get(&coord).cloned() else {
            let task = pool.spawn(async move { ((coord, sub), WaterMeshBuild{ pos:vec![], nor:vec![], uv0:vec![], idx:vec![] }) });
            pending.0.insert((coord, sub), task);
            continue;
        };

        // Snapshots auf dem Main-Thread
        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = water_snapshot_borders(&chunk_map, &water, coord, y0, y1, fc.sea_level);

        let chunk_copy = chunk_map.chunks.get(&coord).cloned();
        let Some(chunk_copy) = chunk_copy else {
            let task = pool.spawn(async move { ((coord, sub), WaterMeshBuild{ pos:vec![], nor:vec![], uv0:vec![], idx:vec![] }) });
            pending.0.insert((coord, sub), task);
            continue;
        };

        let task = pool.spawn(async move {
            build_water_mesh_subchunk_async(coord, sub, chunk_copy, fc, borders).await
        });
        pending.0.insert((coord, sub), task);
    }
}

fn water_collect_meshed_subchunks(
    mut commands: Commands,
    mut pending: ResMut<PendingWaterMesh>,
    mut windex: ResMut<WaterMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    q_mesh: Query<&Mesh3d>,
    reg: Res<BlockRegistry>,
) {
    use bevy::tasks::futures_lite::future;
    let water_mat = id_any(&reg, &["water_block", "water"]);
    if water_mat == 0 { warn!("water_mat not found"); return; }

    let mut done = Vec::new();
    let mut applied = 0usize;

    for (key, task) in pending.0.iter_mut() {
        if applied >= MAX_WATER_APPLY_PER_FRAME { break; }

        if let Some(((coord, sub), build)) = future::block_on(future::poll_once(task)) {
            despawn_water_mesh((coord, sub as u8), &mut windex, &mut commands, &q_mesh, &mut meshes);

            if !build.is_empty() {
                let mesh = build.into_mesh();

                let s = VOXEL_SIZE;
                let origin = Vec3::new(
                    (coord.x * CX as i32) as f32 * s,
                    (Y_MIN as f32) * s,
                    (coord.y * CZ as i32) as f32 * s,
                );
                let ent = commands
                    .spawn((
                        Mesh3d(meshes.add(mesh)),
                        MeshMaterial3d(reg.material(water_mat)),
                        Transform::from_translation(origin),
                        NotShadowReceiver,
                        NotShadowCaster,
                        Name::new(format!("water chunk({},{}) sub{}", coord.x, coord.y, sub)),
                    ))
                    .id();
                windex.0.insert((coord, sub as u8), ent);
            }
            applied += 1;
            done.push(*key);
        }
    }

    for k in done { pending.0.remove(&k); }
}