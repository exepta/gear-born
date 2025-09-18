#![feature(coverage_attribute)]

use bevy::prelude::*;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    
    #[coverage(off)]
    fn build(&self, _app: &mut App) { }
    
}
