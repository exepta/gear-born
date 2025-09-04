#![feature(coverage_attribute)]

mod registry;
mod world;
mod player;
mod debug_overlay;
mod events;

use crate::debug_overlay::DebugOverlayPlugin;
use crate::events::EventsHandler;
use crate::player::PlayerServices;
use crate::registry::biome_registry::BiomeInternalRegistry;
use crate::registry::block_registry::BlockInternalRegistry;
use crate::world::WorldServices;
use bevy::prelude::*;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((
            BlockInternalRegistry,
            BiomeInternalRegistry,
            WorldServices,
            PlayerServices,
            EventsHandler,
            DebugOverlayPlugin
        ));
    }
}
