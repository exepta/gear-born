mod biome_registry;
mod block_registry;

use crate::registry::biome_registry::BiomeInternalRegistry;
use crate::registry::block_registry::BlockInternalRegistry;
use bevy::prelude::*;

pub struct RegistryPlugin;

impl Plugin for RegistryPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((BlockInternalRegistry, BiomeInternalRegistry));
    }
}
