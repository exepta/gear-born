#![feature(coverage_attribute)]

mod camera;
mod registry;
mod world_services;
mod player_services;
mod debug_overlay;

use crate::camera::CameraPlugin;
use crate::debug_overlay::DebugOverlayPlugin;
use crate::player_services::PlayerServices;
use crate::registry::block_registry::BlockInternalRegistry;
use crate::world_services::WorldServices;
use bevy::prelude::*;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((
            BlockInternalRegistry,
            WorldServices,
            PlayerServices,
            CameraPlugin,
            DebugOverlayPlugin
        ));
    }
}
