pub mod biome_func;
pub mod registry;

use bevy::prelude::*;
use serde::Deserialize;

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub struct Biome {
    pub localized_name: String,
    pub name: String,
    #[serde(default = "default_true")]
    pub stand_alone: bool,
    #[serde(default)]
    pub subs: Option<Vec<String>>,
    #[serde(default = "default_rarity")]
    pub rarity: f32,
    #[serde(default = "default_sizes")]
    pub sizes: Vec<BiomeSize>,
    #[serde(default)]
    pub surface: BiomeSurface,
    #[serde(default)]
    pub settings: BiomeSettings,
    #[serde(default)]
    pub generation: BiomeGeneration
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub enum BiomeSize {
    Tiny,
    Small,
    Medium,
    Large,
    Huge,
    Giant,
    Ocean
}

impl BiomeSize {
    pub fn from_str(s: &str) -> Self {
        match s {
            "tiny" => Self::Tiny, // Max 20 chunks
            "small" => Self::Small, // Max 56 chunks
            "medium" => Self::Medium, // Max 98 chunks
            "large" => Self::Large, // Max 196 chunks
            "huge" => Self::Huge, // Max 392 chunks
            "giant" => Self::Giant, // Max 560 chunks
            "ocean" => Self::Ocean, // Min 600 chunks
            _ => Self::Medium,
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub struct BiomeSurface {
    pub top: Vec<String>,
    pub bottom: Vec<String>,
    pub sea_floor: Vec<String>,
    pub upper_zero: Vec<String>,
    pub under_zero: Vec<String>,
}

impl Default for BiomeSurface {
    fn default() -> Self {
        Self {
            top: vec!["grass_block".to_string()],
            bottom: vec!["dirt_block".to_string()],
            sea_floor: vec!["sand_block".to_string()],
            upper_zero: vec!["stone_block".to_string()],
            under_zero: vec!["stone_block".to_string()]
        }
    }
}


#[derive(Debug, Deserialize, PartialEq, Clone, Default)]
pub struct BiomeSettings {
    #[serde(default)] pub height_offset: f32,

    // ocean-only
    #[serde(default)] pub seafloor_amp:  Option<f32>,
    #[serde(default)] pub seafloor_freq: Option<f32>,

    // plains/land
    #[serde(default)] pub land_amp:  Option<f32>,
    #[serde(default)] pub land_freq: Option<f32>,

    // mountains
    #[serde(default)] pub mount_amp:  Option<f32>,
    #[serde(default)] pub mount_freq: Option<f32>,
}

#[derive(Debug, Deserialize, PartialEq, Clone, Default)]
pub struct BiomeGeneration {
    #[serde(default)]
    pub rivers: bool,
}

fn default_true() -> bool { true }

fn default_rarity() -> f32 { 0.1 }

fn default_sizes() -> Vec<BiomeSize> { vec![BiomeSize::Medium] }