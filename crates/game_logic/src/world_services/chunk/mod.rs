mod chunk_service;
mod chunk_utils;
mod chunk_struct;

use bevy::prelude::*;
use game_core::world::chunk::{ChunkGenerated, ChunkMap, SubchunkDirty};
use crate::world_services::chunk::chunk_service::ChunkService;

pub struct ChunkHandlerService;

impl Plugin for ChunkHandlerService {
    fn build(&self, app: &mut App) {
        app.add_event::<ChunkGenerated>();
        app.add_event::<SubchunkDirty>();
        app.init_resource::<ChunkMap>();
        app.add_plugins(ChunkService);
    }
}