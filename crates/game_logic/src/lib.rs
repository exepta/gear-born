#![feature(coverage_attribute)]

mod registry;
mod world;
mod player;
mod debug_overlay;
mod events;

use crate::debug_overlay::DebugOverlayPlugin;
use crate::events::EventsHandler;
use crate::player::PlayerServices;
use crate::world::WorldServices;
use bevy::prelude::*;
use crate::registry::RegistryPlugin;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((
            RegistryPlugin,
            WorldServices,
            PlayerServices,
            EventsHandler,
            DebugOverlayPlugin
        ));
    }
}
