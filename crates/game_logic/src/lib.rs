#![feature(coverage_attribute)]

mod events;
mod debug_overlay;
mod player;
mod registry;
mod world;
use crate::debug_overlay::DebugOverlayPlugin;
use crate::events::EventsHandler;
use crate::player::PlayerServices;
use crate::registry::RegistryPlugin;
use crate::world::WorldServices;
use bevy::prelude::*;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((
            RegistryPlugin,
            WorldServices,
            PlayerServices,
            EventsHandler,
            DebugOverlayPlugin,
        ));
    }
}
