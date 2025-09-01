use bevy::prelude::*;
use game_core::events::player_block_events::{BlockBreakByPlayerEvent, BlockPlaceByPlayerEvent};
use game_core::player::selection::SelectionState;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{break_time_for, get_block_world, BlockRegistry, MiningState, MiningTarget, SelectedBlock};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use game_core::world::fluid::FluidMap;
use game_core::world::{mark_dirty_block_and_neighbors, world_access_mut};

pub struct BlockEventHandler;

impl Plugin for BlockEventHandler {
    fn build(&self, app: &mut App) {
        app
            .add_systems(
            Update,
            (
                block_break_handler,
                block_place_handler,
            ).run_if(in_state(AppState::InGame(InGameStates::Game)))
        );
    }
}

fn block_break_handler(
    time: Res<Time>,
    buttons: Res<ButtonInput<MouseButton>>,
    selection: Res<SelectionState>,
    registry: Res<BlockRegistry>,

    mut state: ResMut<MiningState>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubchunkDirty>,
    mut break_ev: EventWriter<BlockBreakByPlayerEvent>,
) {
    if !buttons.pressed(MouseButton::Left) {
        state.target = None;
        return;
    }

    let Some(hit) = selection.hit else {
        state.target = None;
        return;
    };

    let id_now = get_block_world(&chunk_map, hit.block_pos);
    if id_now == 0 {
        state.target = None;
        return;
    }

    let now = time.elapsed_secs();

    let restart = match state.target {
        None => true,
        Some(target) => target.loc != hit.block_pos || target.id != id_now,
    };

    if restart {
        state.target = Some(MiningTarget {
            loc: hit.block_pos,
            id: id_now,
            started_at: now,
            duration: break_time_for(id_now, &registry),
        });
    }

    let target = state.target.as_ref().unwrap();
    let progress = (now - target.started_at) / target.duration;

    if progress < 1.0 { return; }

    let world_loc = target.loc;
    if let Some(mut access) = world_access_mut(&mut  chunk_map, world_loc) {
        access.set(0);
    }
    mark_dirty_block_and_neighbors(&mut chunk_map, world_loc, &mut ev_dirty);

    let (chunk_coord, l) = world_to_chunk_xz(world_loc.x, world_loc.z);
    let lx = l.x as u8;
    let lz = l.y as u8;
    let ly = (world_loc.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

    break_ev.write(BlockBreakByPlayerEvent {
        chunk_coord,
        location: world_loc,
        chunk_x: lx,
        chunk_y: ly as u16,
        chunk_z: lz,
        block_id: target.id,
        block_name: registry.name_opt(target.id).unwrap_or("").to_string(),
    });

    state.target = None;
}

fn block_place_handler(
    buttons: Res<ButtonInput<MouseButton>>,
    sel: Res<SelectionState>,
    selected: Res<SelectedBlock>,
    registry: Res<BlockRegistry>,

    mut fluids: ResMut<FluidMap>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubchunkDirty>,
    mut place_ev: EventWriter<BlockPlaceByPlayerEvent>,
) {
    if !buttons.just_pressed(MouseButton::Right) { return; }
    let id = selected.id;
    if id == 0 { return; }
    let Some(hit) = sel.hit else { return; };

    let world_pos = hit.place_pos;
    let (chunk_coord, l) = world_to_chunk_xz(world_pos.x, world_pos.z);
    let lx = l.x.clamp(0, (CX as i32 - 1) as u32) as usize;
    let lz = l.y.clamp(0, (CZ as i32 - 1) as u32) as usize;
    let ly = (world_pos.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

    let can_place = chunk_map
        .chunks
        .get(&chunk_coord)
        .map(|ch| ch.get(lx, ly, lz) == 0)
        .unwrap_or(false);
    if !can_place { return; }

    if let Some(fc) = fluids.0.get_mut(&chunk_coord) {
        fc.set(lx, ly, lz, false);
    }

    if let Some(mut access) = world_access_mut(&mut chunk_map, world_pos) {
        access.set(id);
    }

    mark_dirty_block_and_neighbors(&mut chunk_map, world_pos, &mut ev_dirty);

    let name = registry.name_opt(id).unwrap_or("").to_string();
    place_ev.write(BlockPlaceByPlayerEvent {
        location: world_pos,
        block_id: id,
        block_name: name,
    });
}