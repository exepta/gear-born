#![feature(coverage_attribute)]

pub mod config;
pub mod states;
pub mod debug;

pub mod key_converter;
pub mod camera;

use bevy::prelude::*;

/// Core of all game relevant resources and structures. This Plugin initialize resources
/// with `init_resource` from bevy. This Plugin is registered at [`ManagerPlugin`] which is
/// a part of the main.rs file.
pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {

    #[coverage(off)]
    fn build(&self, _app: &mut App) { }

}
