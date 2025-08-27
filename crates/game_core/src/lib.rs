#![feature(coverage_attribute)]

pub mod debug;
pub mod states;
pub mod configuration;
pub mod key_converter;
pub mod world;
pub mod player;

use crate::configuration::{CrosshairConfig, WorldGenConfig};
use crate::player::PlayerModule;
use bevy::prelude::*;

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.init_resource::<WorldGenConfig>();
        app.init_resource::<CrosshairConfig>();
        app.add_plugins(PlayerModule);
    }
}
