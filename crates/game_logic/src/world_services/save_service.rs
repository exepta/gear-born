use std::fs;
use std::path::PathBuf;
use bevy::prelude::*;
use bevy::tasks::IoTaskPool;
use game_core::states::{AppState, InGameStates};
use game_core::world::chunk::{ChunkMap, SubchunkDirty};
use game_core::world::save::*;
use crate::world_services::chunk::chunk_utils::save_chunk_sync;

pub struct WorldSaveService;

impl Plugin for WorldSaveService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<PendingSave>()
            .init_resource::<SaveQueue>()
            .init_resource::<AutoSaveCfg>();
        app
            .add_systems(OnEnter(AppState::Preload), generate_world_save_dir)
            .add_systems(
            Update,
            (
                on_dirty_schedule_save,
                process_save_queue,
                collect_finished_saves,
            )
                .run_if(in_state(AppState::InGame(InGameStates::Game)))
        )
            .add_systems(
                OnExit(AppState::InGame(InGameStates::Game)),
                save_all_loaded_now
            );
    }
}

fn generate_world_save_dir(mut commands: Commands) {
    let name = "world";
    let root = PathBuf::from("saves").join(&name);
    let chunk_dir = root.join("chunks");
    fs::create_dir_all(&chunk_dir).ok();
    commands.insert_resource(WorldSave { root, chunk_dir, version: 1 });
    info!("Created world save for {}", name);
}

fn on_dirty_schedule_save(
    mut ev_dirty: EventReader<SubchunkDirty>,
    time: Res<Time>,
    cfg: Res<AutoSaveCfg>,
    mut queue: ResMut<SaveQueue>,
) {
    let now = time.elapsed_secs();
    for e in ev_dirty.read() {
        queue.due.insert(e.coord, now + cfg.debounce_sec);
    }
}

fn process_save_queue(
    ws: Res<WorldSave>,
    chunk_map: Res<ChunkMap>,
    cfg: Res<AutoSaveCfg>,
    time: Res<Time>,
    mut queue: ResMut<SaveQueue>,
    mut pending: ResMut<PendingSave>,
) {
    if chunk_map.chunks.is_empty() { queue.due.clear(); return; }

    let now = time.elapsed_secs();
    let io_pool = IoTaskPool::get();
    let mut budget = cfg.budget_per_tick;

    let ready: Vec<IVec2> = queue
        .due
        .iter()
        .filter_map(|(c, due)| if *due <= now { Some(*c) } else { None })
        .collect();

    for coord in ready {
        if budget == 0 { break; }
        if pending.0.contains_key(&coord) {
            queue.due.remove(&coord);
            continue;
        }
        let Some(chunk) = chunk_map.chunks.get(&coord) else {
            queue.due.remove(&coord);
            continue;
        };

        let ws_cl = ws.clone();
        let ch_cl = chunk.clone();
        let task = io_pool.spawn(async move {
            let ok = save_chunk_sync(&ws_cl, coord, &ch_cl).is_ok();
            (coord, ok)
        });
        pending.0.insert(coord, task);
        queue.due.remove(&coord);
        budget -= 1;
    }
}

fn collect_finished_saves(mut pending: ResMut<PendingSave>) {
    let mut done = Vec::new();
    for (coord, task) in pending.0.iter_mut() {
        if let Some((_c, _ok)) = futures_lite::future::block_on(
            futures_lite::future::poll_once(task)
        ) {
            done.push(*coord);
        }
    }
    for c in done { pending.0.remove(&c); }
}

fn save_all_loaded_now(ws: Res<WorldSave>, chunk_map: Res<ChunkMap>) {
    for (coord, ch) in chunk_map.chunks.iter() {
        let _ = save_chunk_sync(&ws, *coord, ch);
    }
}