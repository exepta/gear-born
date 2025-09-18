#![feature(coverage_attribute)]

pub mod config;
pub mod states;
pub mod debug;

pub mod key_converter;

use bevy::prelude::*;

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {

    #[coverage(off)]
    fn build(&self, _app: &mut App) { }

}
