#![feature(coverage_attribute)]

use bevy::prelude::*;

pub struct GameServicePlugin;

impl Plugin for GameServicePlugin {

    #[coverage(off)]
    fn build(&self, _app: &mut App) { }

}