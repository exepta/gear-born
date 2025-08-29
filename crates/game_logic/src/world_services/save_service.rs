use std::collections::{HashMap, VecDeque};
use bevy::prelude::*;
use game_core::states::{AppState, InGameStates};
use game_core::world::chunk::{ChunkMap, SubchunkDirty};
use game_core::world::save::*;
use crate::world_services::chunk::chunk_utils::encode_chunk;

pub struct WorldSaveService;

#[derive(Resource, Default)]
struct SaveQueue(VecDeque<IVec2>);

#[derive(Resource, Default)]
struct SaveDebounce(HashMap<IVec2, Timer>);

const SAVE_DEBOUNCE_MS: u64 = 250;

impl Plugin for WorldSaveService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<RegionCache>()
            .init_resource::<SaveDebounce>()
            .init_resource::<SaveQueue>();
        app.add_systems(OnEnter(AppState::Preload), setup_world_save);
        app.add_systems(
            Update,
            (
                enqueue_save_on_dirty,
                tick_save_debounce,
                drain_save_queue,
            )
                .run_if(in_state(AppState::InGame(InGameStates::Game)))
        );
    }
}

fn setup_world_save(mut commands: Commands) {
    let world_root = std::env::current_dir()
        .unwrap_or_default()
        .join("saves")
        .join(&"world");

    std::fs::create_dir_all(world_root.join("region")).ok();

    commands.insert_resource(WorldSave { root: world_root });
}

fn enqueue_save_on_dirty(
    mut ev_dirty: EventReader<SubchunkDirty>,
    mut deb: ResMut<SaveDebounce>,
) {
    for e in ev_dirty.read().copied() {
        deb.0.entry(e.coord)
            .and_modify(|t| { t.reset(); })
            .or_insert_with(|| {
                let mut t = Timer::from_seconds(SAVE_DEBOUNCE_MS as f32 / 1000.0, TimerMode::Once);
                t.reset();
                t
            });
    }
}

fn tick_save_debounce(
    time: Res<Time>,
    mut deb: ResMut<SaveDebounce>,
    mut queue: ResMut<SaveQueue>,
) {
    let mut to_queue = Vec::new();

    for (coord, timer) in deb.0.iter_mut() {
        timer.tick(time.delta());
        if timer.finished() {
            to_queue.push(*coord);
        }
    }
    for coord in to_queue {
        deb.0.remove(&coord);
        if !queue.0.iter().any(|&c| c == coord) {
            queue.0.push_back(coord);
        }
    }
}

fn drain_save_queue(
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    chunk_map: Res<ChunkMap>,
    mut queue: ResMut<SaveQueue>,
) {
    while let Some(coord) = queue.0.pop_front() {
        if let Some(chunk) = chunk_map.chunks.get(&coord) {
            let rc = chunk_to_region(coord);
            if let Ok(rf) = cache.get_or_open(&ws, rc) {
                let buf = encode_chunk(chunk);
                let _ = rf.write_chunk(coord, &buf);
            }
        }
    }
}