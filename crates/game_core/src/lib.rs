#![feature(coverage_attribute)]

pub mod debug;
pub mod states;
pub mod configuration;
pub mod key_converter;
pub mod world;
pub mod player;

use bevy::prelude::*;
use crate::player::PlayerModule;

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(PlayerModule);
    }
}
