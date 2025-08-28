mod chunk_service;
mod chunk_utils;
mod chunk_struct;

use crate::world_services::chunk_service::ChunkService;
use bevy::prelude::*;
use game_core::world::chunk::{ChunkGenerated, ChunkMap, SubchunkDirty};

pub struct WorldServices;

impl Plugin for WorldServices {
    fn build(&self, app: &mut App) {
        app.add_event::<ChunkGenerated>();
        app.add_event::<SubchunkDirty>();
        app.init_resource::<ChunkMap>();
        app.add_plugins(ChunkService);
    }
}