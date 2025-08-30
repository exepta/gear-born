use bevy::prelude::*;
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::chunk::{ChunkMap, WaterChunkUnload};
use game_core::world::fluid::{FluidMap, WaterMeshIndex};
use std::collections::{HashSet, VecDeque};

const WATER_GEN_BUDGET_PER_FRAME: usize = 48;

#[derive(Resource, Default)]
struct WaterGenQueue {
    work: VecDeque<IVec2>,
}

#[derive(Resource, Default)]
struct WaterSeen {
    known: HashSet<IVec2>,
}

pub struct WaterBuilder;

impl Plugin for WaterBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<WaterGenQueue>()
            .init_resource::<WaterSeen>()
            .init_resource::<WaterMeshIndex>()
            .init_resource::<FluidMap>()
            .add_event::<WaterChunkUnload>()
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::WaterGen)),
                water_gen_build_worklist
            )
            .add_systems(
                Update,
                (
                    water_track_new_chunks
                        .run_if(
                            in_state(AppState::Loading(LoadingStates::WaterGen))
                                .or(in_state(AppState::InGame(InGameStates::Game)))
                        ),
                    water_gen_step
                        .run_if(
                            in_state(AppState::Loading(LoadingStates::WaterGen))
                                .or(in_state(AppState::InGame(InGameStates::Game)))
                        ),
                    water_unload_on_event
                        .run_if(
                            in_state(AppState::Loading(LoadingStates::WaterGen))
                                .or(in_state(AppState::InGame(InGameStates::Game)))
                        ),
                )
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
        enqueue_with_ring(&mut q, &mut seen, c);
    }
}

fn water_track_new_chunks(
    mut q: ResMut<WaterGenQueue>,
    mut seen: ResMut<WaterSeen>,
    chunk_map: Res<ChunkMap>,
) {
    for &c in chunk_map.chunks.keys() {
        if !seen.known.contains(&c) {
            enqueue_with_ring(&mut q, &mut seen, c);
        }
    }
}

fn water_gen_step(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    mut next: ResMut<NextState<AppState>>,
    app_state: Res<State<AppState>>,
) {
    let mut done = 0usize;

    while done < WATER_GEN_BUDGET_PER_FRAME {
        let Some(coord) = q.work.pop_front() else { break; };
        if !chunk_map.chunks.contains_key(&coord) { continue; }

        done += 1;
    }

    if matches!(app_state.get(), AppState::Loading(LoadingStates::WaterGen))
        && q.work.is_empty()
    {
        next.set(AppState::InGame(InGameStates::Game));
    }
}

fn water_unload_on_event(
    mut commands: Commands,
    mut ev: EventReader<WaterChunkUnload>,
    mut fluids: ResMut<FluidMap>,
    mut windex: ResMut<WaterMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    q_mesh: Query<&Mesh3d>,
    mut q: ResMut<WaterGenQueue>,
    mut seen: ResMut<WaterSeen>,
) {
    for WaterChunkUnload { coord } in ev.read().copied() {
        fluids.0.remove(&coord);

        let keys: Vec<_> = windex.0.keys().copied()
            .filter(|(c, _)| *c == coord)
            .collect();

        for key in keys {
            if let Some(ent) = windex.0.remove(&key) {
                if let Ok(Mesh3d(handle)) = q_mesh.get(ent) {
                    meshes.remove(handle.id());
                }
                commands.entity(ent).despawn();
            }
        }

        q.work.retain(|c| *c != coord);
        seen.known.remove(&coord);
    }
}

#[inline]
fn enqueue_with_ring(q: &mut WaterGenQueue, seen: &mut WaterSeen, c: IVec2) {
    if seen.known.insert(c) { q.work.push_back(c); }
    for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
        let n = c + d;
        if seen.known.insert(n) { q.work.push_back(n); }
    }
}