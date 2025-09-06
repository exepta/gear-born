use bevy::prelude::*;
use game_core::states::AppState;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::biome::Biome;
use std::fs;
use std::path::Path;

pub struct BiomeInternalRegistry;

impl Plugin for BiomeInternalRegistry {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::Preload), load_biomes_from_folder);
    }
}

fn load_biomes_from_folder(mut registry: ResMut<BiomeRegistry>) {
    let dir = Path::new("assets/biomes");
    if !dir.exists() {
        warn!("Biome directory not found: {:?}", dir);
        return;
    }

    // Collect & sort paths to get stable load order (useful for debugging).
    let mut paths: Vec<_> = match fs::read_dir(dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| {
                p.extension()
                    .and_then(|e| e.to_str())
                    .map(|ext| ext.eq_ignore_ascii_case("json"))
                    .unwrap_or(false)
            })
            .collect(),
        Err(err) => {
            warn!("Failed to read biome directory {:?}: {}", dir, err);
            return;
        }
    };
    paths.sort();

    let mut loaded = 0usize;

    for path in paths {
        match fs::read_to_string(&path) {
            Ok(text) => match serde_json::from_str::<Biome>(&text) {
                Ok(mut biome) => {
                    // If no explicit name in JSON, fall back to the file stem.
                    if biome.name.trim().is_empty() {
                        if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                            biome.name = stem.to_string();
                        }
                    }

                    // Ensure uniqueness by name: last one wins; warn on overwriting.
                    if registry.by_name.contains_key(&biome.name) {
                        warn!(
                            "Duplicate biome name '{}' from file {:?} – overwriting previous entry.",
                            biome.name, path
                        );
                    }

                    registry.register(biome);
                    loaded += 1;
                }
                Err(err) => {
                    warn!("Invalid biome JSON in {:?}: {}", path, err);
                }
            },
            Err(err) => {
                warn!("Failed to read {:?}: {}", path, err);
            }
        }
    }

    info!("Loaded {} biome(s) from assets/biomes", loaded);
}
