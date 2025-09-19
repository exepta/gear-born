#![feature(coverage_attribute)]

use bevy::prelude::*;

pub struct GameWorldPlugin;

impl Plugin for GameWorldPlugin {
    
    #[coverage(off)]
    fn build(&self, _app: &mut App) {
    }
    
}