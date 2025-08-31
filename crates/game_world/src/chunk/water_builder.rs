use crate::chunk::water_utils::*;
use bevy::pbr::{NotShadowCaster, NotShadowReceiver};
use bevy::prelude::*;
use bevy::tasks::AsyncComputeTaskPool;
use futures_lite::future;
use game_core::configuration::WorldGenConfig;
use game_core::events::player_block_events::BlockBreakByPlayerEvent;
use game_core::shader::water_material::{WaterMatHandle, WaterMaterial};
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::block::{id_any, BlockRegistry, VOXEL_SIZE};
use game_core::world::chunk::{ChunkMap, SubchunkDirty, WaterChunkUnload, BIG, MAX_UPDATE_FRAMES};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FlowJob, FlowResult, FluidChunk, FluidMap, Seed, SolidSnapshot, WaterMeshIndex, WATER_FLOW_BUDGET_PER_FRAME, WATER_FLOW_CAP, WATER_FLOW_MAX_INFLIGHT};
use game_core::world::save::{RegionCache, WorldSave};
use std::collections::{HashMap, HashSet, VecDeque};

const WATER_GEN_BUDGET_PER_FRAME: usize = 48;
const MAX_INFLIGHT_WATER_LOAD: usize = 32;

const MAX_INFLIGHT_WATER_MESH: usize = 64;

#[derive(Resource, Default)]
struct WaterBoot {
    started: bool,
}

#[derive(Resource, Default)]
struct WaterGenQueue {
    work: VecDeque<IVec2>,
}

#[derive(Resource, Default)]
pub struct WaterMeshingTodo(pub HashSet<IVec2>);

#[derive(Resource, Default)]
pub struct PendingWaterLoad(
    pub HashMap<IVec2, bevy::tasks::Task<(IVec2, FluidChunk)>>
);

#[derive(Resource, Default)]
pub struct WaterMeshBacklog(pub VecDeque<(IVec2, usize)>);

#[derive(Resource, Default)]
pub struct PendingWaterMesh(
    pub HashMap<(IVec2, usize), bevy::tasks::Task<((IVec2, usize), WaterMeshBuild)>>
);

#[derive(Resource, Default)]
pub struct WaterFlowQueue(pub(crate) VecDeque<FlowJob>);

#[derive(Resource, Default)]
pub struct PendingWaterFlow(
    pub HashMap<u64, bevy::tasks::Task<(u64, FlowResult)>>
);

#[derive(Resource, Default)]
pub struct WaterFlowIds { next: u64 }
impl WaterFlowIds { fn next(&mut self) -> u64 { let id = self.next; self.next += 1; id } }

pub struct WaterBuilder;

impl Plugin for WaterBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<WaterBoot>()
            .init_resource::<WaterGenQueue>()
            .init_resource::<WaterMeshIndex>()
            .init_resource::<FluidMap>()
            .init_resource::<WaterMeshingTodo>()
            .init_resource::<PendingWaterLoad>()
            .init_resource::<WaterMeshBacklog>()
            .init_resource::<PendingWaterMesh>()
            .init_resource::<WaterFlowQueue>()
            .init_resource::<PendingWaterFlow>()
            .init_resource::<WaterFlowIds>()
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

                    enqueue_flow_on_block_removed,
                    schedule_flow_jobs,
                    collect_flow_jobs,

                    water_finish_check
                ).chain()
                    .run_if(
                        in_state(AppState::Loading(LoadingStates::WaterGen))
                            .or(in_state(AppState::InGame(InGameStates::Game)))
                    ),
            );

        app.add_systems(Last, save_all_water_on_exit
            .run_if(on_event::<AppExit>)
            );
    }
}

async fn flow_task_run(solid_snap: SolidSnapshot, water_snap: WaterSnap, mut job: FlowJob) -> FlowResult {
    use std::collections::{HashSet, VecDeque};
    let mut res = FlowResult::default();
    let mut q: VecDeque<Seed> = VecDeque::new();
    let mut seen: HashSet<(IVec2,i32,i32,i32)> = HashSet::new();

    for s in job.seeds.drain(..) {
        if seen.insert((s.c,s.x,s.y,s.z)) { q.push_back(s); }
    }

    let mut filled_count = 0usize;

    while let Some(cur) = q.pop_front() {
        let solid = match snap_is_solid(&solid_snap, cur.c, cur.x, cur.y, cur.z) {
            Some(b) => b,
            None => { res.spill.push(cur); continue; }
        };
        if solid { continue; }

        if let Some(true) = snap_has_water(&water_snap, cur.c, cur.x, cur.y, cur.z) {
            continue;
        }

        res.filled.push(cur);
        filled_count += 1;
        if filled_count >= job.cap {
            res.more.extend(q.drain(..));
            break;
        }

        let mut push = |c: IVec2, x:i32,y:i32,z:i32| {
            if y < 0 || y >= CY as i32 { return; }
            if let Some(true) = snap_has_water(&water_snap, c, x, y, z) { return; }
            let k = (c,x,y,z);
            if seen.insert(k) {
                if in_snapshot(&solid_snap, c) { q.push_back(Seed{ c, x, y, z }); }
                else { res.spill.push(Seed{ c, x, y, z }); }
            }
        };

        push(cur.c, cur.x, cur.y - 1, cur.z);
        let (c1,x1,z1) = neighbor_lookup_chunked(cur.c, cur.x+1, cur.z); push(c1, x1, cur.y, z1);
        let (c2,x2,z2) = neighbor_lookup_chunked(cur.c, cur.x-1, cur.z); push(c2, x2, cur.y, z2);
        let (c3,x3,z3) = neighbor_lookup_chunked(cur.c, cur.x, cur.z+1); push(c3, x3, cur.y, z3);
        let (c4,x4,z4) = neighbor_lookup_chunked(cur.c, cur.x, cur.z-1); push(c4, x4, cur.y, z4);
    }

    res
}
fn enqueue_flow_on_block_removed(
    mut ev: EventReader<BlockBreakByPlayerEvent>,
    fluids: Res<FluidMap>,
    chunks: Res<ChunkMap>,
    mut queue: ResMut<WaterFlowQueue>,
) {
    for e in ev.read() {
        let c = e.chunk_coord;
        if let Some(ch) = chunks.chunks.get(&c) {
            if ch.get(e.chunk_x as usize, e.chunk_y as usize, e.chunk_z as usize) != 0 { continue; }
        } else { continue; }

        let mut sea_level = None;
        let mut has_water = false;
        for (dx,dy,dz) in [(1,0,0),(-1,0,0),(0,0,1),(0,0,-1),(0,-1,0)] {
            let lx = e.chunk_x as i32 + dx;
            let ly = e.chunk_y as i32 + dy;
            let lz = e.chunk_z as i32 + dz;
            if ly < 0 || ly >= CY as i32 { continue; }
            let (nc, nx, nz) = neighbor_lookup_chunked(c, lx, lz);
            if let Some(fc) = fluids.0.get(&nc) {
                if fc.get(nx as usize, ly as usize, nz as usize) {
                    sea_level.get_or_insert(fc.sea_level);
                    has_water = true;
                    break;
                }
            }
        }
        if !has_water { continue; }

        queue.0.push_back(FlowJob {
            seeds: vec![Seed{ c, x: e.chunk_x as i32, y: e.chunk_y as i32, z: e.chunk_z as i32 }],
            sea_level: sea_level.unwrap_or(62),
            cap: WATER_FLOW_CAP,
        });
    }
}

fn schedule_flow_jobs(
    mut queue: ResMut<WaterFlowQueue>,
    mut pending: ResMut<PendingWaterFlow>,
    mut ids: ResMut<WaterFlowIds>,
    chunks: Res<ChunkMap>,
    fluids: Res<FluidMap>,
) {
    if queue.0.is_empty() { return; }
    let pool = AsyncComputeTaskPool::get();

    let mut budget = WATER_FLOW_BUDGET_PER_FRAME
        .min(WATER_FLOW_MAX_INFLIGHT.saturating_sub(pending.0.len()));
    if budget == 0 { return; }

    while budget > 0 {
        let Some(job) = queue.0.pop_front() else { break; };
        let anchor = job.seeds.get(0).map(|s| s.c).unwrap_or(IVec2::ZERO);
        let snapshot = build_solid_snapshot_3x3(&chunks, anchor);
        let water_snap = build_water_snapshot_3x3(&fluids, anchor);

        let id = ids.next();
        let task = pool.spawn(async move {
            let res = flow_task_run(snapshot, water_snap, job).await;
            (id, res)
        });
        pending.0.insert(id, task);
        budget -= 1;
    }
}

fn collect_flow_jobs(
    mut pending: ResMut<PendingWaterFlow>,
    mut fluids: ResMut<FluidMap>,
    mut backlog: ResMut<WaterMeshBacklog>,
    mut queue: ResMut<WaterFlowQueue>,
    chunks: Res<ChunkMap>,
) {
    let mut done_ids = Vec::new();

    for (id, task) in pending.0.iter_mut() {
        if let Some((_id, mut res)) = future::block_on(future::poll_once(task)) {

            res.filled.retain(|s| chunks.chunks.contains_key(&s.c));

            for s in res.filled {
                let fc = fluids.0.entry(s.c).or_insert_with(|| FluidChunk::new(62));

                if !fc.get(s.x as usize, s.y as usize, s.z as usize) {
                    fc.set(s.x as usize, s.y as usize, s.z as usize, true);

                    enqueue_mesh_for_cell(&mut backlog, s.c, s.x, s.y, s.z);
                }
            }

            let mut more_loaded: Vec<Seed>  = res.more.into_iter().filter(|s| chunks.chunks.contains_key(&s.c)).collect();
            let mut spill_loaded: Vec<Seed> = res.spill.into_iter().filter(|s| chunks.chunks.contains_key(&s.c)).collect();

            let mut push_job = |seeds: &mut Vec<Seed>| {
                if seeds.is_empty() { return; }
                let sea = fluids.0.get(&seeds[0].c).map(|f| f.sea_level).unwrap_or(62);
                queue.0.push_back(FlowJob { seeds: std::mem::take(seeds), sea_level: sea, cap: WATER_FLOW_CAP });
            };

            push_job(&mut more_loaded);
            push_job(&mut spill_loaded);

            done_ids.push(*id);
        }
    }
    for id in done_ids { pending.0.remove(&id); }
}

#[inline]
fn enqueue_mesh_for_cell(backlog: &mut WaterMeshBacklog, c: IVec2, x: i32, y: i32, z: i32) {
    let clamp = |v: i32, lo: i32, hi: i32| v.max(lo).min(hi);
    let y = clamp(y, 0, (CY as i32) - 1);
    let sub = (y as usize) / SEC_H;

    let mut push = |cc: IVec2, ss: usize| {
        let key = (cc, ss);
        if !backlog.0.iter().any(|k| *k == key) {
            backlog.0.push_back(key);
        }
    };

    push(c, sub);

    if x == 0               { push(c - IVec2::X, sub); }
    if x == (CX as i32 - 1) { push(c + IVec2::X, sub); }
    if z == 0               { push(c - IVec2::Y, sub); }
    if z == (CZ as i32 - 1) { push(c + IVec2::Y, sub); }
}


fn water_gen_build_worklist(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    pending: Res<PendingWaterLoad>,
    mut boot: ResMut<WaterBoot>,
) {
    q.work.clear();
    rebuild_water_work_queue_impl(&mut q.work, &chunk_map, &water, &pending);
    boot.started = true;
}

fn water_track_new_chunks(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    pending: Res<PendingWaterLoad>,
) {
    rebuild_water_work_queue_impl(&mut q.work, &chunk_map, &water, &pending);
}

fn schedule_water_generation_jobs(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    ws: Res<WorldSave>,
    gen_cfg: Res<WorldGenConfig>,
    mut pending: ResMut<PendingWaterLoad>,
    app_state: Res<State<AppState>>,
) {
    let max_inflight = if in_water_gen(&app_state) { BIG } else { MAX_INFLIGHT_WATER_LOAD };
    let per_frame    = if in_water_gen(&app_state) { BIG } else { WATER_GEN_BUDGET_PER_FRAME };

    if chunk_map.chunks.is_empty() { q.work.clear(); return; }

    let mut budget = max_inflight
        .saturating_sub(pending.0.len())
        .min(per_frame);
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
            if let Some((mut wc, ver)) = load_water_chunk_from_disk_any(root.clone(), coord) {
                if ver == WATER_MAGIC_V1 {
                    water_mask_with_solids(&mut wc, &chunk_copy);
                }
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
    app_state: Res<State<AppState>>,
) {
    let mut done: Vec<IVec2> = Vec::new();
    let apply_cap = if in_water_gen(&app_state) { BIG } else { WATER_GEN_BUDGET_PER_FRAME };
    let mut applied = 0usize;

    for (_, task) in pending.0.iter_mut() {
        if applied >= apply_cap { break; }

        if let Some((c, wc)) = future::block_on(future::poll_once(task)) {
            if chunk_map.chunks.contains_key(&c) {
                if let Some(chunk) = chunk_map.chunks.get(&c) {
                    let mut wc2 = wc;
                    water_mask_with_solids(&mut wc2, chunk);
                    water.0.insert(c, wc2);
                } else {
                    water.0.insert(c, wc);
                }
                to_mesh.0.insert(c);
                for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] { to_mesh.0.insert(c + d); }
                applied += 1;
            }
            done.push(c);
        }
    }
    for c in done { pending.0.remove(&c); }
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

fn water_finish_check(
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    q: Res<WaterGenQueue>,
    boot: Res<WaterBoot>,
    pending_load: Res<PendingWaterLoad>,
    to_mesh: Res<WaterMeshingTodo>,
    pending_mesh: Res<PendingWaterMesh>,
    backlog: Res<WaterMeshBacklog>,
    app_state: Res<State<AppState>>,
    mut next: ResMut<NextState<AppState>>,
) {
    let in_water_gen = matches!(app_state.get(), AppState::Loading(LoadingStates::WaterGen));
    if !in_water_gen { return; }

    let coverage_ok = all_chunks_have_water(&chunk_map, &water);

    let gen_done   = q.work.is_empty() && pending_load.0.is_empty();
    let mesh_done  = to_mesh.0.is_empty() && backlog.0.is_empty() && pending_mesh.0.is_empty();
    let world_ok   = !chunk_map.chunks.is_empty();

    if boot.started && world_ok && gen_done && coverage_ok && mesh_done {
        debug!("Water gen complete");
        next.set(AppState::InGame(InGameStates::Game));
    }
}

fn water_unload_on_event(
    mut commands: Commands,
    mut ev: EventReader<WaterChunkUnload>,
    mut water: ResMut<FluidMap>,
    mut windex: ResMut<WaterMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    q_mesh: Query<&Mesh3d>,
    chunk_map: Res<ChunkMap>,
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    mut todo: Option<ResMut<WaterMeshingTodo>>,
    mut backlog: Option<ResMut<WaterMeshBacklog>>,
    mut pending_mesh: Option<ResMut<PendingWaterMesh>>,
    mut pending_load: Option<ResMut<PendingWaterLoad>>,
    mut flow_q: Option<ResMut<WaterFlowQueue>>,
    mut q: Option<ResMut<WaterGenQueue>>,
) {
    for WaterChunkUnload { coord } in ev.read().copied() {
        if let Some(t) = todo.as_mut()          { t.0.remove(&coord); }
        if let Some(b) = backlog.as_mut()       { b.0.retain(|(c, _)| *c != coord); }
        if let Some(pm) = pending_mesh.as_mut() { pm.0.retain(|(c, _), _| *c != coord); }
        if let Some(pl) = pending_load.as_mut() { pl.0.remove(&coord); }
        if let Some(fq) = flow_q.as_mut()       { fq.0.retain(|job| job.seeds.iter().all(|s| s.c != coord)); }

        if let Some(mut wc) = water.0.remove(&coord) {
            if let Some(ch) = chunk_map.chunks.get(&coord) {
                water_mask_with_solids(&mut wc, ch);
            }
            save_water_chunk_sync(&ws, &mut cache, coord, &wc);
        }

        let dead: Vec<_> = windex.0.keys().copied().filter(|(c, _)| *c == coord).collect();
        for key in dead {
            despawn_water_mesh(key, &mut windex, &mut commands, &q_mesh, &mut meshes);
        }

        if let Some(q) = q.as_mut()     { q.work.retain(|c| *c != coord); }
    }
}

fn save_all_water_on_exit(
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    chunks: Res<ChunkMap>,
    water: Res<FluidMap>,
) {
    debug!("Exit: Saving all water");
    for (&coord, fc) in water.0.iter() {
        let mut w = fc.clone();
        if let Some(ch) = chunks.chunks.get(&coord) {
            water_mask_with_solids(&mut w, ch);
        }
        save_water_chunk_sync(&ws, &mut cache, coord, &w);
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
                    if !backlog.0.iter().any(|k| *k == key) {
                        backlog.0.push_back(key);
                    }
                }
            }
        }
    }
}

fn water_drain_mesh_backlog(
    mut backlog: ResMut<WaterMeshBacklog>,
    mut pending: ResMut<PendingWaterMesh>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    app_state: Res<State<AppState>>,
) {
    let max_inflight = if in_water_gen(&app_state) { BIG } else { MAX_INFLIGHT_WATER_MESH };
    let pool = AsyncComputeTaskPool::get();

    let mut processed = 0usize;
    let limit = backlog.0.len();

    while pending.0.len() < max_inflight && processed < limit {
        let Some((coord, sub)) = backlog.0.pop_front() else { break; };
        processed += 1;

        if !water_meshing_ready(coord, &water, &chunk_map) {
            backlog.0.push_back((coord, sub));
            continue;
        }

        let fc = match water.0.get(&coord).cloned() {
            Some(v) => v,
            None => { backlog.0.push_back((coord, sub)); continue; }
        };
        let chunk_copy = match chunk_map.chunks.get(&coord).cloned() {
            Some(v) => v,
            None => { backlog.0.push_back((coord, sub)); continue; }
        };

        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = water_snapshot_borders(&chunk_map, &water, coord, y0, y1, fc.sea_level);

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
    water_handle: Res<WaterMatHandle>,
    reg: Res<BlockRegistry>,
    app_state: Res<State<AppState>>,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
) {
    let water_mat = id_any(&reg, &["water_block", "water"]);
    if water_mat == 0 { warn!("water_mat not found"); return; }

    let apply_cap = if in_water_gen(&app_state) { BIG } else { MAX_UPDATE_FRAMES };
    let mut done = Vec::new();
    let mut applied = 0usize;

    for (key, task) in pending.0.iter_mut() {
        if applied >= apply_cap { break; }

        if let Some(((coord, sub), build)) = future::block_on(future::poll_once(task)) {
            if !chunk_map.chunks.contains_key(&coord) || !water.0.contains_key(&coord) {
                done.push(*key);
                continue;
            }

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
                        MeshMaterial3d::<WaterMaterial>(water_handle.0.clone()),
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

#[inline]
fn try_enqueue(
    c: IVec2,
    work: &mut VecDeque<IVec2>,
    queued: &mut HashSet<IVec2>,
    chunk_map: &ChunkMap,
    water: &FluidMap,
    pending: &PendingWaterLoad,
) {
    if !chunk_map.chunks.contains_key(&c) { return; }
    if water.0.contains_key(&c) || pending.0.contains_key(&c) { return; }
    if queued.insert(c) {
        work.push_back(c);
    }
}

fn rebuild_water_work_queue_impl(
    work: &mut VecDeque<IVec2>,
    chunk_map: &ChunkMap,
    water: &FluidMap,
    pending: &PendingWaterLoad,
) {
    work.retain(|c| chunk_map.chunks.contains_key(c));

    let mut queued: HashSet<IVec2> = work.iter().copied().collect();

    for &c in chunk_map.chunks.keys() {
        try_enqueue(c, work, &mut queued, chunk_map, water, pending);
        for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
            try_enqueue(c + d, work, &mut queued, chunk_map, water, pending);
        }
    }
}