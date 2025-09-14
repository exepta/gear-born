use bevy::prelude::*;
use bevy::tasks::{AsyncComputeTaskPool, Task};
use futures_lite::future;

use game_core::configuration::WorldGenConfig;
use game_core::events::chunk_events::SubChunkNeedRemeshEvent;
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;

use crate::chunk::cave_utils::{worm_edits_for_chunk, CaveBlockIds, CaveParams};

/// How many chunks get processed per frame (to avoid spikes).
#[derive(Resource, Debug, Clone)]
pub struct CaveBudget {
    pub chunks_per_frame: usize,
}
impl Default for CaveBudget {
    fn default() -> Self { Self { chunks_per_frame: 2 } }
}

/// Small wrapper to silence #[must_use] when stored in a tuple.
#[derive(Debug)]
pub struct CaveTask(pub Task<Vec<(u16, u16, u16)>>);

/// Async job container: running cave jobs and their results.
#[derive(Resource, Default)]
pub struct CaveJobs {
    /// (ChunkCoord, Task -> list of (lx, ly, lz) that should be carved to air)
    pub running: Vec<(IVec2, CaveTask)>,
}

pub struct CaveBuilder;

impl Plugin for CaveBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<CaveBudget>()
            .init_resource::<CaveTracker>()
            .init_resource::<CaveJobs>()
            // 1) When entering CaveGen, enqueue all currently loaded chunks.
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::CaveGen)),
                enqueue_all_loaded_chunks_for_caves,
            )
            // 2) While we are in CaveGen (loading) → carve step.
            .add_systems(
                Update,
                carve_caves_step.run_if(in_state(AppState::Loading(LoadingStates::CaveGen))),
            )
            // 3) Also carve during gameplay for newly loaded chunks.
            .add_systems(
                Update,
                (enqueue_newly_loaded_chunks_during_game, carve_caves_step)
                    .chain()
                    .run_if(in_state(AppState::InGame(InGameStates::Game))),
            )
            // 4) Defensive cleanup when leaving CaveGen (optional).
            .add_systems(
                OnExit(AppState::Loading(LoadingStates::CaveGen)),
                clear_cave_queue,
            );
    }
}

/* =========================
   Queue Management
   ========================= */

#[inline]
fn enqueue_pending_impl(tracker: &mut CaveTracker, chunk_map: &ChunkMap) {
    // Iterate keys once; push only if neither done nor already pending.
    for &coord in chunk_map.chunks.keys() {
        if tracker.done.contains(&coord) || tracker.pending.contains(&coord) {
            continue;
        }
        tracker.pending.push_back(coord);
    }
}

fn enqueue_all_loaded_chunks_for_caves(
    mut tracker: ResMut<CaveTracker>,
    chunk_map: Res<ChunkMap>,
) {
    // OnEnter: enqueue whatever is already loaded at state start.
    enqueue_pending_impl(&mut tracker, &chunk_map);
}

/// During gameplay: whenever a chunk appears and isn't processed yet, enqueue it.
fn enqueue_newly_loaded_chunks_during_game(
    mut tracker: ResMut<CaveTracker>,
    chunk_map: Res<ChunkMap>,
) {
    enqueue_pending_impl(&mut tracker, &chunk_map);
}

/// Clear the queue when leaving the state (safety net).
fn clear_cave_queue(mut tracker: ResMut<CaveTracker>) {
    tracker.pending.clear();
}

/* =========================
   Main Carving Step (async)
   ========================= */

fn carve_caves_step(
    budget: Res<CaveBudget>,
    mut tracker: ResMut<CaveTracker>,
    mut jobs: ResMut<CaveJobs>,
    mut next_state: ResMut<NextState<AppState>>,
    reg: Res<BlockRegistry>,
    mut chunk_map: ResMut<ChunkMap>,
    world_gen_config: Res<WorldGenConfig>,
    mut ev_remesh: EventWriter<SubChunkNeedRemeshEvent>,
) {
    // If nothing to do, exit CaveGen (or no-op in InGame).
    if tracker.pending.is_empty() && jobs.running.is_empty() {
        next_state.set(AppState::InGame(InGameStates::Game));
        return;
    }

    let air_id: u32 = reg.id_opt("air_block").unwrap_or(0) as u32;
    let water_id: u32 = reg.id_opt("water_block").unwrap_or(1) as u32;
    let border_id: u32 = reg.id_opt("border_block").unwrap_or(0) as u32;

    let _ids = CaveBlockIds { air: air_id, water: water_id, protected_1: None };

    // Tuned for walkable tunnels + rare big cavern clusters.
    let params_template = CaveParams {
        seed: world_gen_config.seed,

        /* tunnels vertical window */
        y_top: 52,
        y_bottom: -110,

        /* worms: a bit wider/longer */
        worms_per_region: 1.35,
        region_chunks: 3,
        base_radius: 4.2,
        radius_var: 3.0,
        step_len: 1.5,
        worm_len_steps: 360,

        /* small rooms along tunnels */
        room_event_chance: 0.1,
        room_radius_min: 6.0,
        room_radius_max: 10.5,

        /* normal caverns: uncommon mid-sized rooms */
        caverns_per_region: 0.5,
        cavern_room_count_min: 6,
        cavern_room_count_max: 11,
        cavern_room_radius_xz_min: 16.0,
        cavern_room_radius_xz_max: 34.0,
        cavern_room_radius_y_min: 9.0,
        cavern_room_radius_y_max: 21.0,
        cavern_connector_radius: 12.5,
        cavern_y_top: -10,
        cavern_y_bottom: -100,

        /* MEGA caverns: very rare, very large */
        mega_caverns_per_region: 0.075,
        mega_room_count_min: 1,
        mega_room_count_max: 3,
        mega_room_radius_xz_min: 45.0,
        mega_room_radius_xz_max: 144.0,
        mega_room_radius_y_min: 20.0,
        mega_room_radius_y_max: 46.0,
        mega_connector_radius: 8.0,
        mega_y_top: -30,
        mega_y_bottom: -105,

        /* entrances (NEW) */
        entrance_extra_top: 20,       // allow up to ~12 blocks above y_top
        entrance_chance: 0.75,        // ~35% chance when a segment hits the trigger band
        entrance_len_steps: 40,       // short spur climb
        entrance_radius_scale: 0.55,  // narrower than the main tunnel
        entrance_min_radius: 2.8,     // don't get thinner than this
        entrance_trigger_band: 12.0,   // start spurs within 8 blocks below y_top
    };

    // 1) Spawn a few jobs per frame.
    let pool = AsyncComputeTaskPool::get();
    let mut started = 0usize;

    while started < budget.chunks_per_frame {
        let Some(coord) = tracker.pending.pop_front() else { break; };
        if !chunk_map.is_loaded(coord) {
            // If the chunk is gone again, mark as done (no-op).
            tracker.done.insert(coord);
            continue;
        }

        let params = params_template.clone();

        let task = pool.spawn(async move {
            compute_cave_edits_for_chunk(params, coord).await
        });

        jobs.running.push((coord, CaveTask(task)));
        started += 1;
    }

    // 2) Reap completed jobs and apply edits.
    if !jobs.running.is_empty() {
        let mut finished: Vec<usize> = Vec::new();

        for (i, (coord, task_wrap)) in jobs.running.iter_mut().enumerate() {
            if let Some(edits) = future::block_on(future::poll_once(&mut task_wrap.0)) {
                // Track which subchunks got changed so we can remesh them (and neighbors).
                let mut touched = [false; SEC_COUNT];

                if let Some(chunk) = chunk_map.get_chunk_mut(*coord) {
                    for (lx, ly, lz) in edits {
                        let sub = (ly as usize) / SEC_H;
                        if sub < SEC_COUNT { touched[sub] = true; }

                        let cur = chunk.get(lx as usize, ly as usize, lz as usize);
                        if cur != 0
                            && cur != water_id as BlockId
                            && cur != border_id as BlockId
                        {
                            chunk.set(lx as usize, ly as usize, lz as usize, air_id as BlockId);
                        }
                    }
                }

                // Fire remesh events for changed subchunks in this chunk + 4-neighborhood.
                for sub in 0..SEC_COUNT {
                    if !touched[sub] { continue; }
                    ev_remesh.write(SubChunkNeedRemeshEvent { coord: *coord, sub });

                    const N4: [IVec2; 4] = [
                        IVec2::new( 1, 0),
                        IVec2::new(-1, 0),
                        IVec2::new( 0, 1),
                        IVec2::new( 0,-1),
                    ];
                    for d in N4 {
                        let nc = IVec2::new(coord.x + d.x, coord.y + d.y);
                        if chunk_map.is_loaded(nc) {
                            ev_remesh.write(SubChunkNeedRemeshEvent { coord: nc, sub });
                        }
                    }
                }

                tracker.done.insert(*coord);
                finished.push(i);
            }
        }

        // Remove finished tasks.
        for i in finished.into_iter().rev() {
            jobs.running.swap_remove(i);
        }
    }

    // If nothing is left, we can leave CaveGen.
    if tracker.pending.is_empty() && jobs.running.is_empty() {
        next_state.set(AppState::InGame(InGameStates::Game));
    }
}

/* =========================
   Async compute (off-thread)
   ========================= */

async fn compute_cave_edits_for_chunk(
    params: CaveParams,
    chunk_coord: IVec2,
) -> Vec<(u16, u16, u16)> {
    let chunk_size = IVec2::new(CX as i32, CZ as i32);
    worm_edits_for_chunk(&params, chunk_coord, chunk_size, Y_MIN, Y_MAX)
}

/* =========================
   Legacy helper
   ========================= */

#[allow(dead_code)]
fn carve_single_chunk(
    _chunk: &mut ChunkData,
    _field: &(),
    _ids: CaveBlockIds,
) {}
