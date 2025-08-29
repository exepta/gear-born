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

#[derive(Resource, Clone)]
pub struct BuildInfo {
    pub app_name: &'static str,
    pub app_version: &'static str,
    pub bevy_version: &'static str,
}

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.init_resource::<WorldGenConfig>();
        app.init_resource::<CrosshairConfig>();
        app.add_plugins(PlayerModule);
    }
}
