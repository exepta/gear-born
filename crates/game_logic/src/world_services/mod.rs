mod chunk;

use bevy::prelude::*;
use crate::world_services::chunk::ChunkHandlerService;

pub struct WorldServices;

impl Plugin for WorldServices {
    fn build(&self, app: &mut App) {
        app.add_plugins(ChunkHandlerService);
    }
}