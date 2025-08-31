mod chunk_builder;
pub mod chunk_utils;
mod chunk_struct;
mod water_builder;
mod water_utils;

use crate::chunk::chunk_builder::ChunkBuilder;
use crate::chunk::water_builder::WaterBuilder;
use bevy::prelude::*;
use game_core::world::chunk::{ChunkGenerated, ChunkMap, SubchunkDirty};

pub struct ChunkHandlerService;

impl Plugin for ChunkHandlerService {
    fn build(&self, app: &mut App) {
        app.add_event::<ChunkGenerated>();
        app.add_event::<SubchunkDirty>();
        app.init_resource::<ChunkMap>();
        app.add_plugins((ChunkBuilder, WaterBuilder));
    }
}