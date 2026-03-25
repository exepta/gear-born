pub mod chunk_events;
pub mod player_block_events;

use crate::events::chunk_events::*;
use crate::events::player_block_events::*;
use bevy::prelude::*;

pub struct EventModule;

impl Plugin for EventModule {
    fn build(&self, app: &mut App) {
        app.add_message::<BlockBreakByPlayerEvent>()
            .add_message::<BlockPlaceByPlayerEvent>()
            .add_message::<ChunkUnloadEvent>()
            .add_message::<ChunkGeneratedEvent>()
            .add_message::<SubChunkNeedRemeshEvent>();
    }
}
