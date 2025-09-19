#![feature(coverage_attribute)]

pub mod config;
pub mod states;
pub mod debug;

pub mod key_converter;
pub mod camera;
pub mod world;
pub mod serde_defaults;

use bevy::prelude::*;
use crate::world::GameWorldModule;

/// Core of all game relevant resources and structures. This Plugin initializes resources
/// with `init_resource` from bevy. This Plugin is registered at [`ManagerPlugin`] which is
/// a part of the main.rs file.
pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {

    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(GameWorldModule);
    }

}
