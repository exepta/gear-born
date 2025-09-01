use bevy::prelude::*;
use game_core::events::player_block_events::{BlockBreakByPlayerEvent, BlockPlaceByPlayerEvent};
use game_core::player::selection::SelectionState;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{BlockRegistry, SelectedBlock};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use game_core::world::fluid::FluidMap;
use game_core::world::{mark_dirty_block_and_neighbors, world_access_mut};

#[derive(Event, Debug, Clone)]
struct BlockBreakRequest {
    world_pos: IVec3,
}

#[derive(Event, Debug, Clone)]
struct BlockPlaceRequest {
    world_pos: IVec3,
    block_id: u16,
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
enum BlockEditSet {
    Input,
    Apply,
}

pub struct BlockEventHandler;

impl Plugin for BlockEventHandler {
    fn build(&self, app: &mut App) {
        app
            .add_event::<BlockBreakRequest>()
            .add_event::<BlockPlaceRequest>()
            .configure_sets(
            Update,
            (BlockEditSet::Input, BlockEditSet::Apply)
                .chain()
                .run_if(in_state(AppState::InGame(InGameStates::Game)))
        ).add_systems(
            Update,
            emit_click_requests
                .in_set(BlockEditSet::Input)
                .run_if(resource_exists::<BlockRegistry>)
        ).add_systems(
            Update,
            (
                apply_break_requests,
                apply_place_requests,
            ).in_set(BlockEditSet::Apply)
        );
    }
}

fn emit_click_requests(
    buttons: Res<ButtonInput<MouseButton>>,
    sel: Res<SelectionState>,
    selected: Res<SelectedBlock>,
    mut break_req: EventWriter<BlockBreakRequest>,
    mut place_req: EventWriter<BlockPlaceRequest>,
) {
    let Some(hit) = sel.hit else { return; };

    if buttons.just_pressed(MouseButton::Left) {
        break_req.write(BlockBreakRequest { world_pos: hit.block_pos });
    }

    if buttons.just_pressed(MouseButton::Right) {
        let id = selected.id;
        if id != 0 {
            place_req.write(BlockPlaceRequest {
                world_pos: hit.place_pos,
                block_id: id,
            });
        }
    }
}

fn apply_break_requests(
    mut reqs: EventReader<BlockBreakRequest>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubchunkDirty>,
    reg: Res<BlockRegistry>,
    mut break_ev: EventWriter<BlockBreakByPlayerEvent>,
) {
    for BlockBreakRequest { world_pos } in reqs.read() {
        let (chunk_coords, l) = world_to_chunk_xz(world_pos.x, world_pos.z);
        let lx = l.x as u8;
        let lz = l.y as u8;
        let ly = (world_pos.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

        let mut broken_id = 0u16;
        if let Some(mut access) = world_access_mut(&mut chunk_map, *world_pos) {
            broken_id = access.get();
            access.set(0);
        }

        mark_dirty_block_and_neighbors(&mut chunk_map, *world_pos, &mut ev_dirty);

        let block_name = reg.name_opt(broken_id).unwrap_or("").to_string();
        break_ev.write(BlockBreakByPlayerEvent {
            chunk_coord: chunk_coords,
            location: *world_pos,
            chunk_x: lx,
            chunk_y: ly as u16,
            chunk_z: lz,
            block_id: broken_id,
            block_name,
        });
    }
}

fn apply_place_requests(
    mut reqs: EventReader<BlockPlaceRequest>,
    mut fluids: ResMut<FluidMap>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubchunkDirty>,
    mut place_ev: EventWriter<BlockPlaceByPlayerEvent>,
    reg: Res<BlockRegistry>,
) {
    for BlockPlaceRequest { world_pos, block_id } in reqs.read() {
        let (chunk_coord, l) = world_to_chunk_xz(world_pos.x, world_pos.z);
        let lx = l.x.clamp(0, (CX as i32 - 1) as u32) as usize;
        let lz = l.y.clamp(0, (CZ as i32 - 1) as u32) as usize;
        let ly = (world_pos.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

        let can_place = chunk_map
            .chunks
            .get(&chunk_coord)
            .map(|ch| ch.get(lx, ly, lz) == 0)
            .unwrap_or(false);
        if !can_place { continue; }

        if let Some(fc) = fluids.0.get_mut(&chunk_coord) {
            fc.set(lx, ly, lz, false);
        }

        if let Some(mut access) = world_access_mut(&mut chunk_map, *world_pos) {
            access.set(*block_id);
        }

        mark_dirty_block_and_neighbors(&mut chunk_map, *world_pos, &mut ev_dirty);

        let name = reg.name_opt(*block_id).unwrap_or("").to_string();
        place_ev.write(BlockPlaceByPlayerEvent {
            location: *world_pos,
            block_id: *block_id,
            block_name: name,
        });
    }
}