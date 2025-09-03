// biome.rs
//!
//! JSON-Schema (Example):
//! ```json
//! {
//!   "name": "plains",
//!   "grass_color": { "red":255, "green":255, "blue":255, "alpha":255 },
//!   "rarity": 0.3,
//!   "temperature": 0.35,
//!   "moist": 0.65,
//!   "surface": {
//!     "top_blocks": ["grass_block:0.8", "dirt_block:0.15", "sand_block:0.05"],
//!     "bottom_blocks": ["dirt_block:0.95", "sand_block:0.05"],
//!     "under_blocks": ["stone_block:0.9", "dirt_block:0.1"],
//!     "deep_under_blocks": ["stone_block:1.0"]
//!   },
//!   "generation": {
//!     "height_offset": 4.0,
//!     "ridge_amp_factor": 0.9,
//!     "rolling_amp_factor": 1.2
//!   }
//! }
//! ```

use bevy::prelude::*;
use serde::de::{Error, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Default, Resource)]
pub struct BiomeRegistry {
    pub by_name: HashMap<String, Handle<BiomeAsset>>,
    pub order: Vec<Handle<BiomeAsset>>,
}

impl BiomeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: impl Into<String>, handle: Handle<BiomeAsset>) {
        let name = name.into();
        if !self.by_name.contains_key(&name) {
            self.order.push(handle.clone());
        }
        self.by_name.insert(name, handle);
    }

    pub fn get(&self, name: &str) -> Option<&Handle<BiomeAsset>> {
        self.by_name.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &Handle<BiomeAsset>)> {
        self.order.iter().filter_map(move |h| {
            self.by_name
                .iter()
                .find(|(_, v)| *v == h)
                .map(|(k, v)| (k.as_str(), v))
        })
    }

    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    pub fn pick_by_rarity<'a>(
        &'a self,
        assets: &'a Assets<BiomeAsset>,
        r: f32,
    ) -> Option<&'a Handle<BiomeAsset>> {
        if self.order.is_empty() {
            return None;
        }
        let mut total = 0.0f32;
        let mut weights = Vec::with_capacity(self.order.len());
        for h in &self.order {
            if let Some(b) = assets.get(h) {
                let w = b.rarity.clamp(0.0, 1.0);
                total += w;
                weights.push((h, w));
            }
        }
        if total <= 0.0 {
            return self.order.first();
        }
        let mut acc = 0.0f32;
        let target = r.clamp(0.0, 0.999_999) * total;
        for (h, w) in weights {
            acc += w;
            if target <= acc {
                return Some(h);
            }
        }
        self.order.last()
    }
}

#[derive(Asset, TypePath, Debug, Clone, Deserialize)]
pub struct BiomeAsset {
    pub name: String,
    pub grass_color: GrassColor,
    #[serde(default = "default_rarity")]
    pub rarity: f32,
    pub temperature: f32,
    pub moist: f32,
    #[serde(default)]
    pub sizes: Vec<BiomeSize>,
    pub surface: BiomeSurface,
    pub generation: BiomeGeneration,
}

#[allow(dead_code)]
fn default_rarity() -> f32 {
    1.0
}

impl BiomeAsset {
    pub fn grass_color(&self) -> Color {
        self.grass_color.into()
    }

    pub fn clamped(&self) -> Self {
        let mut copy = self.clone();
        copy.rarity = copy.rarity.clamp(0.0, 1.0);
        copy.temperature = copy.temperature.clamp(0.0, 1.0);
        copy.moist = copy.moist.clamp(0.0, 1.0);
        copy.generation.ridge_amp_factor = copy.generation.ridge_amp_factor.max(0.0);
        copy.generation.rolling_amp_factor = copy.generation.rolling_amp_factor.max(0.0);
        copy
    }
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub struct GrassColor {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    #[serde(default = "default_alpha_255")]
    pub alpha: u8,
}

#[allow(dead_code)]
const fn default_alpha_255() -> u8 {
    255
}

impl From<GrassColor> for Color {
    fn from(value: GrassColor) -> Self {
        Color::srgba_u8(value.red, value.green, value.blue, value.alpha)
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct BiomeSurface {
    #[serde(deserialize_with = "de_weighted_blocks")]
    pub top_blocks: Vec<WeightedBlock>,
    #[serde(deserialize_with = "de_weighted_blocks")]
    pub bottom_blocks: Vec<WeightedBlock>,
    #[serde(deserialize_with = "de_weighted_blocks")]
    pub under_blocks: Vec<WeightedBlock>,
    #[serde(deserialize_with = "de_weighted_blocks")]
    pub deep_under_blocks: Vec<WeightedBlock>,
}

#[allow(dead_code)]
fn de_weighted_blocks<'de, D>(deserializer: D) -> Result<Vec<WeightedBlock>, D::Error>
where
    D: Deserializer<'de>,
{
    struct WBVisitor;

    impl<'de> Visitor<'de> for WBVisitor {
        type Value = Vec<WeightedBlock>;

        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("a sequence of strings like \"block_name:weight\"")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut out = Vec::new();
            while let Some(item) = seq.next_element::<String>()? {
                let wb = WeightedBlock::parse(&item)
                    .map_err(|e| Error::custom(format!("invalid weighted block: {e}")))?;
                out.push(wb);
            }
            Ok(out)
        }
    }

    deserializer.deserialize_seq(WBVisitor)
}

impl BiomeSurface {
    pub fn normalized(list: &[WeightedBlock]) -> Vec<WeightedBlock> {
        let sum: f32 = list.iter().map(|w| w.weight.max(0.0)).sum();
        if sum > 0.0 {
            list.iter()
                .map(|w| WeightedBlock {
                    name: w.name.clone(),
                    weight: w.weight.max(0.0) / sum,
                })
                .collect()
        } else {
            let n = list.len().max(1) as f32;
            list.iter()
                .map(|w| WeightedBlock {
                    name: w.name.clone(),
                    weight: 1.0 / n,
                })
                .collect()
        }
    }

    pub fn pick(list: &[WeightedBlock], rarity: f32) -> Option<&str> {
        if list.is_empty() {
            return None;
        }

        let mut acc: f32 = 0.0;
        let clamped = rarity.clamp(0.0, 0.999_999);
        for wb in Self::normalized(list).iter() {
            acc += wb.weight;
            if clamped <= acc {
                Some(wb.name.as_str());
            }
        }
        Some(list.last().unwrap().name.as_str())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WeightedBlock {
    pub name: String,
    pub weight: f32,
}

impl WeightedBlock {
    pub fn parse(value: &str) -> Result<Self, String> {
        let value = value.trim();
        if value.is_empty() {
            return Err("empty block entry".into());
        }
        if let Some((name, w_str)) = value.split_once(':') {
            let name = name.trim();
            if name.is_empty() {
                return Err("missing block name".into());
            }
            let weight: f32 = w_str
                .trim()
                .parse()
                .map_err(|_| format!("invalid weight for '{name}': '{w_str}'"))?;
            if !weight.is_finite() || weight < 0.0 {
                return Err(format!("weight must be >= 0 and finite for '{name}'"));
            }
            Ok(WeightedBlock {
                name: name.to_string(),
                weight,
            })
        } else {
            Ok(WeightedBlock {
                name: value.to_string(),
                weight: 1.0,
            })
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct BiomeGeneration {
    #[serde(default)]
    pub height_offset: f32,
    #[serde(default = "default_one")]
    pub ridge_amp_factor: f32,
    #[serde(default = "default_one")]
    pub rolling_amp_factor: f32,
    #[serde(default = "default_true")]
    pub river_allowed: bool,
}

#[allow(dead_code)]
const fn default_one() -> f32 {
    1.0
}

#[allow(dead_code)]
fn default_true() -> bool { true }

impl Default for BiomeGeneration {
    fn default() -> Self {
        Self {
            height_offset: 0.0,
            ridge_amp_factor: 1.0,
            rolling_amp_factor: 1.0,
            river_allowed: true,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum BiomeSize {
    Small,
    Medium,
    Large,
    Gigantic,
}