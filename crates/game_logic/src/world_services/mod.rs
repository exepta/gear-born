mod chunk_service;

use bevy::prelude::*;
use game_core::world::chunk::{ChunkGenerated, ChunkMap};
use crate::world_services::chunk_service::ChunkService;

pub struct WorldServices;

impl Plugin for WorldServices {
    fn build(&self, app: &mut App) {
        app.add_event::<ChunkGenerated>();
        app.init_resource::<ChunkMap>();
        app.add_plugins(ChunkService);
    }
}