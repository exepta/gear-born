use bevy::prelude::*;
use game_core::events::registry_events::BlockRegistryEvent;
use game_core::world::biome_plate::{load_palettes_from_dir, BiomePaletteSet};
use game_core::world::block::BlockRegistry;

pub struct BiomeInternalRegistry;

impl Plugin for BiomeInternalRegistry {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, initialize_biome_palettes.run_if(on_event::<BlockRegistryEvent>));
    }
}

fn initialize_biome_palettes(mut commands: Commands, registry: Res<BlockRegistry>) {
    match load_palettes_from_dir("assets/biomes", &registry) {
        Ok(set) => {
            debug!("Registered Biome Palettes! Loaded Biomes: ( {} )", set.map.len());
            commands.insert_resource(set);
        }
        Err(e) => {
            error!("Failed to load biome palettes: {e:#}");
            commands.insert_resource(BiomePaletteSet::default());
        }
    }
}