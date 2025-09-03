use crate::world::biome::BiomeAsset;
use bevy::asset::io::Reader;
use bevy::asset::{AssetLoader, LoadContext};
use bevy::prelude::*;

pub struct BiomeJsonLoader {
    extensions: &'static [&'static str],
}

impl BiomeJsonLoader {
    pub const fn new(extensions: &'static [&'static str]) -> Self {
        Self { extensions }
    }
}

impl AssetLoader for BiomeJsonLoader {
    type Asset = BiomeAsset;
    type Settings = ();
    type Error = anyhow::Error;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        let mut biome: BiomeAsset = serde_json::from_slice(&bytes)?;

        biome.rarity = biome.rarity.clamp(0.0, 1.0);
        biome.temperature = biome.temperature.clamp(0.0, 1.0);
        biome.moist = biome.moist.clamp(0.0, 1.0);
        biome.generation.ridge_amp_factor = biome.generation.ridge_amp_factor.max(0.0);
        biome.generation.rolling_amp_factor = biome.generation.rolling_amp_factor.max(0.0);

        Ok(biome)
    }

    fn extensions(&self) -> &[&str] {
        self.extensions
    }
}