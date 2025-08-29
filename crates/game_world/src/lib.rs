#![feature(coverage_attribute)]

use crate::chunk::ChunkHandlerService;
use bevy::prelude::*;

pub mod chunk;

pub struct GameWorldPlugin;

impl Plugin for GameWorldPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(ChunkHandlerService);
    }
}
