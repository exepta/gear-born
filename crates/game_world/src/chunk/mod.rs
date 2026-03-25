mod cave_builder;
mod cave_utils;
mod chunk_builder;
mod chunk_gen;
mod chunk_struct;
pub mod chunk_utils;
mod river_utils;
mod water_builder;
mod water_utils;

use crate::chunk::cave_builder::CaveBuilder;
use crate::chunk::chunk_builder::ChunkBuilder;
use crate::chunk::water_builder::WaterBuilder;
use bevy::prelude::*;
use game_core::world::chunk::ChunkMap;

pub struct ChunkHandlerService;

impl Plugin for ChunkHandlerService {
    fn build(&self, app: &mut App) {
        app.init_resource::<ChunkMap>();
        app.add_plugins((ChunkBuilder, WaterBuilder, CaveBuilder));
    }
}
