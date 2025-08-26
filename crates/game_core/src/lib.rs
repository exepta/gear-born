#![feature(coverage_attribute)]

pub mod debug;
pub mod states;
pub mod configuration;
pub mod key_converter;
pub mod world;

use bevy::prelude::*;

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {
    #[coverage(off)]
    fn build(&self, _app: &mut App) {

    }
}
