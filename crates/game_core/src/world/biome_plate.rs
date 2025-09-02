use crate::world::biome::BiomeType;
use crate::world::block::{id_any, BlockId, BlockRegistry};
use bevy::prelude::*;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Resource, Clone, Debug, Default)]
pub struct BiomePaletteSet {
    pub map: HashMap<BiomeType, BiomePalette>,
    pub fallback: BiomePalette,
}

impl BiomePaletteSet {
    pub fn get(&self, id: BiomeType) -> BiomePalette {
        *self.map.get(&id).unwrap_or(&self.fallback)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct BiomePalette {
    pub top: BlockId,
    pub filler: BlockId,
    pub stone: BlockId,
    pub seabed: BlockId,
    pub beach_top: BlockId
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BiomeIdStr {
    Desert,
    WasteLand,
    Jungle,
    Plains,
    Forest,
    Swamp,
    Mountain,
    SnowPlains,
    SnowMountain,
    Beach,
    Ocean,
    BloodForest
}

impl From<BiomeIdStr> for BiomeType {
    fn from(s: BiomeIdStr) -> Self {
        use BiomeIdStr::*;
        match s {
            Desert => BiomeType::Desert,
            WasteLand => BiomeType::WasteLand,
            Jungle => BiomeType::Jungle,
            Plains => BiomeType::Plains,
            Forest => BiomeType::Forest,
            Swamp => BiomeType::Swamp,
            Mountain => BiomeType::Mountain,
            BloodForest => BiomeType::BloodForest,
            SnowPlains => BiomeType::SnowPlains,
            SnowMountain => BiomeType::SnowMountain,
            Beach => BiomeType::Beach,
            Ocean => BiomeType::Ocean,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Candidates {
    One(String),
    Many(Vec<String>),
}

impl Candidates {
    fn as_slice(&self) -> Vec<&str> {
        match self {
            Candidates::One(s) => vec![s.as_str()],
            Candidates::Many(v) => v.iter().map(|s| s.as_str()).collect(),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct BiomePaletteJson {
    pub biome: BiomeIdStr,
    pub top: Candidates,
    pub filler: Candidates,
    pub stone: Candidates,
    pub seabed: Candidates,
    #[serde(default = "default_beach_top")]
    pub beach_top: Candidates,
}

fn default_beach_top() -> Candidates {
    Candidates::Many(vec!["sand_block".into(),"sand".into()])
}

pub fn load_palettes_from_dir<P: AsRef<Path>>(dir: P, reg: &BlockRegistry) -> anyhow::Result<BiomePaletteSet> {
    let dir = dir.as_ref();
    let mut set = BiomePaletteSet::default();

    // Fallback: simple „grass/dirt/stone/sand“
    let fallback = {
        let grass = id_any(reg, &["grass_block","grass"]);
        let dirt  = id_any(reg, &["dirt_block","dirt"]);
        let sand  = id_any(reg, &["sand_block","sand"]);
        let stone = id_any(reg, &["stone_block","stone"]);
        BiomePalette { top: grass, filler: dirt, stone, seabed: sand, beach_top: sand }
    };
    set.fallback = fallback;

    if !dir.exists() {
        error!("Biome Palette Directory does not exist: {}", dir.display());
        return Ok(set);
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path  = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            warn!("Skipping non-json file in Biome Palette Directory: {}", path.display());
            continue;
        }
        let bytes = fs::read(&path)?;
        let def: BiomePaletteJson = serde_json::from_slice(&bytes)?;

        let res = |candidates: &Candidates| -> BlockId {
            let names = candidates.as_slice();
            id_any(reg, &names)
        };

        let pal = BiomePalette {
            top:       res(&def.top),
            filler:    res(&def.filler),
            stone:     res(&def.stone),
            seabed:    res(&def.seabed),
            beach_top: res(&def.beach_top),
        };
        
        set.map.insert(def.biome.into(), pal);
    }

    Ok(set)
}