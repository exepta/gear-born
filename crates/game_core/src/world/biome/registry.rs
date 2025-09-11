use crate::world::biome::*;
use bevy::prelude::*;
use rand::distr::weighted::WeightedIndex;
use rand::distr::Distribution;
use rand::rng;
use std::collections::HashMap;

#[derive(Resource, Debug, Default, Clone)]
pub struct BiomeRegistry {
    pub by_name: HashMap<String, Biome>,
    pub ordered_names: Vec<String>,
    pub weights: Vec<f32>,
}

impl BiomeRegistry {
    pub fn register(&mut self, biome: Biome) {
        let key = biome.name.clone();
        self.by_name.insert(key, biome);
        self.rebuild_cache();
    }

    pub fn get(&self, name: &str) -> Option<&Biome> {
        self.by_name.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &Biome)> {
        self.by_name.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Number of registered biomes.
    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    pub fn random_by_rarity(&self) -> Option<&Biome> {
        if self.ordered_names.is_empty() {
            return None;
        }
        // Safety net: WeightedIndex requires strictly positive weights.
        if self.weights.iter().all(|w| *w <= 0.0) {
            return None;
        }
        let dist = WeightedIndex::new(&self.weights).ok()?;
        let mut rng = rng();
        let idx = dist.sample(&mut rng);
        let name = &self.ordered_names[idx];
        self.by_name.get(name)
    }

    fn rebuild_cache(&mut self) {
        let mut names: Vec<_> = self.by_name.keys().cloned().collect();
        names.sort(); // deterministic order for reproducibility
        let weights = names
            .iter()
            .map(|n| self.by_name[n].rarity.max(0.0))
            .collect();
        self.ordered_names = names;
        self.weights = weights;
    }
}