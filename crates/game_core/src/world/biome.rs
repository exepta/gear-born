use crate::world::block::BlockId;
use bevy::prelude::*;
use serde::de::Error;
use serde::*;
use std::collections::HashMap;
use std::fs::*;
use std::path::Path;

/// A single weighted block entry (e.g. "sand_block:0.6").
/// If no weight is provided in JSON (e.g. "grass_block"), the weight defaults to 1.0.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockChoice {
    pub id: String,
    pub weight: f32,
}

impl<'de> Deserialize<'de> for BlockChoice {
    // Custom deserializer to accept strings like "id" or "id:0.6"
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        let mut parts = raw.split(':');

        let id = parts
            .next()
            .map(|s| s.trim().to_string())
            .ok_or_else(|| Error::custom("missing block id"))?;

        let weight = if let Some(w) = parts.next() {
            w.trim().parse::<f32>().map_err(|_| {
                Error::custom(format!("invalid weight for block '{}': '{}'", id, w))
            })?
        } else {
            1.0
        };

        Ok(BlockChoice { id, weight })
    }
}

/// Convenience alias for a layer consisting of multiple (possibly weighted) block choices.
pub type BlockLayer = Vec<BlockChoice>;

/// Biome climate category (extend as needed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BiomeClimate {
    Normal,
    Cold,
    Hot,
    #[serde(other)]
    Unknown,
}

/// Available biome size tags (extend as needed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BiomeSize {
    #[serde(rename = "small")]
    Small,
    #[serde(rename = "medium")]
    Medium,
    #[serde(rename = "large")]
    Large,
    #[serde(rename = "very_large")]
    VeryLarge,
    #[serde(rename = "gigantic")]
    Gigantic,
    #[serde(other)]
    Unknown,
}

/// Surface composition configuration for a biome.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BiomeSurface {
    pub top: BlockLayer,
    pub bottom: BlockLayer,
    #[serde(rename = "sea_floor")]
    pub sea_floor: BlockLayer,
    #[serde(rename = "upper_zero")]
    pub upper_zero: BlockLayer,
    #[serde(rename = "under_zero")]
    pub under_zero: BlockLayer,
}

/// Tunable numeric settings for a biome.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BiomeSettings {
    pub height_offset: f32,
    pub mountain_freq: f32,
}

/// Generation toggles for a biome.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BiomeGeneration {
    pub rivers: bool,
    pub river_fill_threshold: f32,
    pub coast: bool,
    #[serde(default = "default_true")]
    pub lakes: bool,
    pub lake_max_depth: Option<i32>,
    pub rift: bool,
}

fn default_true() -> bool { true }

#[derive(Clone, Copy, Default)]
pub struct BiomeEdgeBlend {
    /// Blend radius in blocks (per edge).
    pub radius: u8,
    /// Neighbor materials per edge: (top, bottom) block IDs if the neighbor is a different biome.
    pub west:  Option<EdgeMat>,
    pub east:  Option<EdgeMat>,
    pub north: Option<EdgeMat>,
    pub south: Option<EdgeMat>,
}

#[derive(Clone, Copy, Default)]
pub struct EdgeMat {
    pub top: BlockId,
    pub bottom: BlockId,
    /// Edge-specific salt so both sides of the border share the same seam curve
    pub salt: u32,
    pub height_offset: f32,
    pub rivers: bool,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct BiomeTerrainParams {
    /// Adds to the global basis height (e.g., mountains: +28.0)
    pub height_offset: f32,
    /// Multiplies the main height frequency (e.g. mountains: 1.2)
    pub mountain_freq: f32,
    /// Enables river carving for this biome.
    pub rivers: bool,
    pub river_fill_threshold: f32,
    pub coast: bool,
    pub allow_lakes: bool,
    pub lake_max_depth: i32,
}

/// Main biome data structure that mirrors your JSON.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Biome {
    pub localized_name: String,
    pub name: String,
    pub climate: BiomeClimate,
    pub rarity: f32,
    pub sizes: Vec<BiomeSize>,
    pub surface: BiomeSurface,
    pub settings: BiomeSettings,
    pub generation: BiomeGeneration,
}

impl Biome {
    /// Optional helper: parse a `Biome` from a JSON string.
    /// Errors if the JSON structure does not match this struct.
    pub fn from_json_str(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}

/// Bevy resource that stores all registered biomes.
/// No plugin here; just the resource and some convenience methods.
#[derive(Resource, Default, Clone)]
pub struct BiomeRegistry {
    by_name: HashMap<String, Biome>,
}

impl BiomeRegistry {
    /// Load all biomes from the "assets/biomes" directory without registering them.
    /// - Expects one JSON biome per *.json file.
    /// - Skips non-JSON files silently.
    /// - Returns an error if the directory is missing, unreadable, or if any JSON is invalid.
    pub fn load_all_from_assets_folder() -> std::io::Result<Vec<Biome>> {
        let dir = Path::new("assets/biomes");
        if !dir.is_dir() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("biomes directory not found: {}", dir.display()),
            ));
        }

        let mut out = Vec::new();

        for entry in read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            // Only process *.json files
            let is_json = path
                .extension()
                .and_then(|s| s.to_str())
                .map(|ext| ext.eq_ignore_ascii_case("json"))
                .unwrap_or(false);
            if !is_json {
                continue;
            }

            let text = read_to_string(&path)?;
            match serde_json::from_str::<Biome>(&text) {
                Ok(biome) => out.push(biome),
                Err(err) => {
                    // Include the filename in the error for easier debugging
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("failed to parse '{}': {}", path.display(), err),
                    ));
                }
            }
        }

        Ok(out)
    }
    /// Insert or replace a biome by its `name`. Returns the old value if it existed.
    pub fn insert(&mut self, biome: Biome) -> Option<Biome> {
        self.by_name.insert(biome.name.clone(), biome)
    }

    /// Get an immutable handle to a biome by name.
    pub fn get(&self, name: &str) -> Option<&Biome> {
        self.by_name.get(name)
    }

    /// Get a mutable handle to a biome by name.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Biome> {
        self.by_name.get_mut(name)
    }

    /// Check whether a biome with this name exists.
    pub fn contains(&self, name: &str) -> bool {
        self.by_name.contains_key(name)
    }

    /// Remove a biome by name. Returns the removed biome if it existed.
    pub fn remove(&mut self, name: &str) -> Option<Biome> {
        self.by_name.remove(name)
    }

    /// Clear the entire registry.
    pub fn clear(&mut self) {
        self.by_name.clear();
    }

    /// Number of stored biomes.
    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    /// Iterate over all biomes.
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Biome)> {
        self.by_name.iter()
    }
}