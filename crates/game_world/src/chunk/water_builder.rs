use crate::chunk::water_utils::{build_water_mesh_subchunk, despawn_water_mesh, generate_water_for_chunk, load_water_chunk_sync, save_water_chunk_sync};
use bevy::prelude::*;
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::block::{id_any, BlockRegistry, VOXEL_SIZE};
use game_core::world::chunk::{ChunkMap, SubchunkDirty, WaterChunkUnload};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidMap, WaterMeshIndex};
use game_core::world::save::{RegionCache, WorldSave};
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

#[derive(Resource, Default)]
struct WaterMeshingTodo(pub HashSet<IVec2>);

pub struct WaterBuilder;

impl Plugin for WaterBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<WaterGenQueue>()
            .init_resource::<WaterSeen>()
            .init_resource::<WaterMeshIndex>()
            .init_resource::<FluidMap>()
            .init_resource::<WaterMeshingTodo>()
            .add_event::<WaterChunkUnload>()
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::WaterGen)),
                water_gen_build_worklist
            )
            .add_systems(
                Update,
                (
                    water_mark_from_dirty
                        .run_if(
                            in_state(AppState::Loading(LoadingStates::WaterGen))
                                .or(in_state(AppState::InGame(InGameStates::Game)))
                        ),
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
                    (water_unload_on_event, water_collect_and_apply_meshes)
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

fn water_gen_step(
    mut q: ResMut<WaterGenQueue>,
    chunk_map: Res<ChunkMap>,
    gen_cfg: Res<WorldGenConfig>,
    mut water: ResMut<FluidMap>,
    mut next: ResMut<NextState<AppState>>,
    app_state: Res<State<AppState>>,
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    mut to_mesh: ResMut<WaterMeshingTodo>,
) {
    let mut done = 0usize;
    let sea_level = 62i32;

    while done < WATER_GEN_BUDGET_PER_FRAME {
        let Some(coord) = q.work.pop_front() else { break; };

        // Bereits erzeugt?
        if water.0.contains_key(&coord) {
            done += 1;
            continue;
        }

        // Basis-Chunk nicht (mehr) vorhanden? Überspringen – er wird später erneut eingereiht.
        let Some(chunk) = chunk_map.chunks.get(&coord) else {
            done += 1;
            continue;
        };

        // Laden oder erzeugen
        let wc = if let Some(wc) = load_water_chunk_sync(&ws, &mut cache, coord) {
            wc
        } else {
            generate_water_for_chunk(coord, chunk, sea_level, gen_cfg.seed as u32, false)
        };

        water.0.insert(coord, wc);

        // Für Meshing vormerken – inkl. Rand (Ring), damit Chunk-Grenzen sauber sind
        to_mesh.0.insert(coord);
        for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
            to_mesh.0.insert(coord + d);
        }

        done += 1;
    }

    if matches!(app_state.get(), AppState::Loading(LoadingStates::WaterGen))
        && q.work.is_empty()
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

fn water_collect_and_apply_meshes(
    mut commands: Commands,
    chunk_map: Res<ChunkMap>,
    water: Res<FluidMap>,
    reg: Res<BlockRegistry>,
    mut windex: ResMut<WaterMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    q_mesh: Query<&Mesh3d>,
    mut todo: ResMut<WaterMeshingTodo>,
) {
    if todo.0.is_empty() { return; }
    let water_mat = id_any(&reg, &["water_block", "water"]);
    if water_mat == 0 {
        warn!("water_mat not found");
        return;
    }

    let coords: Vec<IVec2> = todo.0.drain().collect();

    for coord in coords {
        let Some(wc) = water.0.get(&coord) else {
            for sub in 0..SEC_COUNT {
                despawn_water_mesh((coord, sub as u8), &mut windex, &mut commands, &q_mesh, &mut meshes);
            }
            continue;
        };

        for sub in 0..SEC_COUNT {
            if !wc.sub_has_any(sub) {
                despawn_water_mesh((coord, sub as u8), &mut windex, &mut commands, &q_mesh, &mut meshes);
                continue;
            }

            if let Some(mesh) = build_water_mesh_subchunk(coord, sub, &chunk_map, &water) {
                despawn_water_mesh((coord, sub as u8), &mut windex, &mut commands, &q_mesh, &mut meshes);

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
                        Name::new(format!("water chunk({},{}) sub{}", coord.x, coord.y, sub)),
                    ))
                    .id();
                windex.0.insert((coord, sub as u8), ent);
            } else {
                despawn_water_mesh((coord, sub as u8), &mut windex, &mut commands, &q_mesh, &mut meshes);
            }
        }
    }
}

/*#[inline]
fn enqueue_with_ring(q: &mut WaterGenQueue, seen: &mut WaterSeen, c: IVec2) {
    if seen.known.insert(c) { q.work.push_back(c); }

    for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
        let n = c + d;
        q.work.push_back(n);
    }
}*/