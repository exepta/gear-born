mod chunk_builder;
pub mod chunk_utils;
mod chunk_struct;
mod water_builder;
mod water_utils;
mod chunk_gen;
mod river_utils;

use crate::chunk::chunk_builder::ChunkBuilder;
use crate::chunk::water_builder::WaterBuilder;
use bevy::prelude::*;
use game_core::world::chunk::ChunkMap;

pub struct ChunkHandlerService;

impl Plugin for ChunkHandlerService {
    fn build(&self, app: &mut App) {
        app.init_resource::<ChunkMap>();
        app.add_plugins((ChunkBuilder, WaterBuilder));
    }
}