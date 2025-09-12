pub mod player_block_events;
pub mod chunk_events;

use crate::events::player_block_events::*;
use bevy::prelude::*;
use crate::events::chunk_events::*;

pub struct EventModule;

impl Plugin for EventModule {
    fn build(&self, app: &mut App) {
        app
            .add_event::<BlockBreakByPlayerEvent>()
            .add_event::<BlockPlaceByPlayerEvent>()
            .add_event::<ChunkUnloadEvent>()
            .add_event::<ChunkGeneratedEvent>()
            .add_event::<SubChunkNeedRemeshEvent>();
    }
}