pub mod player_block_events;

use crate::events::player_block_events::{BlockBreakByPlayerEvent, BlockPlaceByPlayerEvent};
use bevy::prelude::*;

pub struct EventModule;

impl Plugin for EventModule {
    fn build(&self, app: &mut App) {
        app
            .add_event::<BlockBreakByPlayerEvent>()
            .add_event::<BlockPlaceByPlayerEvent>();
    }
}