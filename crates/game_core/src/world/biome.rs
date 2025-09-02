use bevy::prelude::*;
use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::RangeInclusive;

// ======================================
//                General
// ======================================

pub type WorldSeed = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum RegionScale {
    Small,
    Medium,
    Large,
    VeryLarge,
}

impl RegionScale {
    pub fn size_chunks(self) -> IVec2 {
        match self {
            RegionScale::Small => IVec2::new(12, 12),
            RegionScale::Medium => IVec2::new(32, 32),
            RegionScale::Large => IVec2::new(56, 56),
            RegionScale::VeryLarge => IVec2::new(96, 96),
        }
    }
}


#[derive(Debug, Clone, Copy, Default, Reflect)]
pub struct Climate {
    pub temperature: f32,
    pub moisture: f32,
    pub elevation: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum BiomeType {
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

#[derive(Debug, Clone, Copy, Reflect)]
pub struct BiomeNoiseProfile {
    pub seed: u64,
    pub frequency: f32,
    pub lacunarity: f32,
    pub gain: f32,
    pub octaves: u8,
}

#[derive(Debug, Clone, Reflect)]
pub struct BiomeDef {
    pub _type: BiomeType,
    pub name: &'static str,
    pub temp_range: RangeInclusive<f32>,
    pub moisture_range: RangeInclusive<f32>,
    pub elevation_bias: f32,
    pub noise: BiomeNoiseProfile,
}

#[derive(Resource, Debug, Clone, Reflect)]
pub struct BiomeRegistry {
    pub items: Vec<BiomeDef>,
}

impl BiomeRegistry {
    pub fn get(&self, _type: BiomeType) -> &BiomeDef {
        self.items.iter().find(|b| b._type == _type).expect("Biome not found")
    }
}

#[derive(Clone)]
pub struct BiomeConfig {
    pub sea_level: i32,
    pub temp_freq: f32,
    pub moist_freq: f32,
    pub elev_freq: f32,
    pub warp_freq: f32,

    // NEW: enforce macro-sized biome cells
    pub biome_cell_blocks: i32, // e.g. 256 = ~16×16-Chunks bei 16er Chunks
    pub cell_stick: f32,        // 0..1, how strongly we snap to a cell center
}

impl Default for BiomeConfig {
    fn default() -> Self {
        Self {
            sea_level: 62,
            temp_freq: 0.0009,   // lower = larger features
            moist_freq: 0.0010,
            elev_freq: 0.0007,
            warp_freq: 0.0025,   // a bit gentler to avoid filaments

            biome_cell_blocks: 256, // try 192..384 to taste
            cell_stick: 0.85,       // 0.7..0.9 = chunky but still varied
        }
    }
}
// ======================================
//                Partition
// ======================================

#[derive(Debug, Clone, Reflect)]
pub struct RegionTier {
    pub scale: RegionScale,
    pub weight: u32,
    pub border_jitter: f32
}

impl RegionTier {
    pub fn size_chunks(&self) -> IVec2 {
        self.scale.size_chunks()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub struct RegionKey {
    pub scale: RegionScale,
    pub region_x: i32,
    pub region_z: i32,
}

// ======================================
//                  Store
// ======================================

#[derive(Resource, Clone)]
pub struct BiomeStore {
    pub seed: u32,
    pub cfg: BiomeConfig,
}

impl BiomeStore {
    pub fn new(seed: u32) -> Self {
        Self { seed, cfg: BiomeConfig::default() }
    }

    /// Create a per-thread sampler with prebuilt noise instances (fast to call in tight loops).
    pub fn make_sampler(&self) -> BiomeSampler {
        // distinct, valid 32-bit constants for sub-seeds
        const K_TEMP:  u32 = 0xA1B2_C3D5;
        const K_MOIST: u32 = 0xB4C5_D6E7;
        const K_ELEV:  u32 = 0xC7D8_E9FA;
        const K_WARP:  u32 = 0xDADB_CF01;

        // helper to derive an i32 seed expected by FastNoiseLite
        #[inline]
        fn d(base: u32, k: u32) -> i32 {
            // simple xor + scramble to reduce correlation
            (base.wrapping_add(0x9E37_79B9).rotate_left(5) ^ k) as i32
        }

        let mut temp = FastNoiseLite::with_seed(d(self.seed, K_TEMP));
        temp.set_noise_type(Some(NoiseType::OpenSimplex2));
        temp.set_frequency(Some(self.cfg.temp_freq));
        temp.set_fractal_type(Some(FractalType::FBm));
        temp.set_fractal_octaves(Some(3));
        temp.set_fractal_gain(Some(0.5));

        let mut moist = FastNoiseLite::with_seed(d(self.seed, K_MOIST));
        moist.set_noise_type(Some(NoiseType::OpenSimplex2));
        moist.set_frequency(Some(self.cfg.moist_freq));
        moist.set_fractal_type(Some(FractalType::FBm));
        moist.set_fractal_octaves(Some(3));
        moist.set_fractal_gain(Some(0.5));

        let mut elev = FastNoiseLite::with_seed(d(self.seed, K_ELEV));
        elev.set_noise_type(Some(NoiseType::OpenSimplex2));
        elev.set_frequency(Some(self.cfg.elev_freq));
        elev.set_fractal_type(Some(FractalType::FBm));
        elev.set_fractal_octaves(Some(4));
        elev.set_fractal_gain(Some(0.5));

        let mut warp = FastNoiseLite::with_seed(d(self.seed, K_WARP));
        warp.set_noise_type(Some(NoiseType::OpenSimplex2));
        warp.set_frequency(Some(self.cfg.warp_freq));

        BiomeSampler { cfg: self.cfg.clone(), temp, moist, elev, warp }
    }
}

#[allow(dead_code)]
pub struct BiomeSampler {
    cfg: BiomeConfig,
    temp: FastNoiseLite,
    moist: FastNoiseLite,
    elev: FastNoiseLite,
    warp: FastNoiseLite,
}

impl BiomeSampler {
    #[inline] fn map01(x: f32) -> f32 { x * 0.5 + 0.5 }

    // Helper: snap world coords to a “cell center” and blend toward it
    #[inline]
    fn quantize(&self, x: f32, z: f32) -> (f32, f32) {
        let cell = self.cfg.biome_cell_blocks.max(1);
        let cx_i = ((x as i32).div_euclid(cell) * cell + cell / 2) as f32;
        let cz_i = ((z as i32).div_euclid(cell) * cell + cell / 2) as f32;
        let a = self.cfg.cell_stick.clamp(0.0, 1.0);
        let qx = x + (cx_i - x) * a;
        let qz = z + (cz_i - z) * a;
        (qx, qz)
    }

    /// Sample climate fields in [0..1] with light domain-warp and macro-cell quantization.
    #[inline]
    pub fn climate(&self, x: f32, z: f32) -> (f32, f32, f32) {
        // First, push (x,z) toward the center of a large cell
        let (qx, qz) = self.quantize(x, z);

        // Then a gentle warp so borders aren’t straight lines
        let dx = self.warp.get_noise_2d(qx, qz) * 10.0;                     // was 30.0
        let dz = self.warp.get_noise_2d(qx + 1000.0, qz - 1000.0) * 10.0;

        let t = Self::map01(self.temp.get_noise_2d(qx + dx, qz + dz));
        let m = Self::map01(self.moist.get_noise_2d(qx - dz, qz + dx));
        let e = Self::map01(self.elev.get_noise_2d(qx * 0.7 + dx * 0.2, qz * 0.7 + dz * 0.2));
        (t, m, e)
    }

    /// Pick a biome from climate bands with sensible clustering.
    #[inline]
    pub fn biome_at_xz(&self, wx: i32, wz: i32) -> BiomeType {
        let (t, m, e) = self.climate(wx as f32, wz as f32);

        // Elevation-first override for mountains and snow caps.
        if e > 0.82 { return BiomeType::Mountain; }
        if e > 0.76 && t < 0.35 { return BiomeType::SnowPlains; }

        // Temperature bands
        let temp_band = if t < 0.33 { 0 } else if t < 0.66 { 1 } else { 2 };
        // Moisture bands
        let moist_band = if m < 0.33 { 0 } else if m < 0.66 { 1 } else { 2 };

        match (temp_band, moist_band) {
            // Cold
            (0, 2) => BiomeType::SnowPlains,
            (0, 1) => if e > 0.6 { BiomeType::SnowPlains } else { BiomeType::SnowMountain },
            (0, 0) => if e > 0.55 { BiomeType::SnowMountain } else { BiomeType::SnowPlains },

            // Temperate
            (1, 2) => if e < 0.45 { BiomeType::Swamp } else { BiomeType::Forest },
            (1, 1) => if e > 0.55 { BiomeType::Forest } else { BiomeType::Plains },
            (1, 0) => if e > 0.6  { BiomeType::WasteLand } else { BiomeType::Plains },

            // Hot
            (2, 2) => BiomeType::Jungle,
            (2, 1) => BiomeType::BloodForest,
            (2, 0) => if e < 0.55 { BiomeType::Desert } else { BiomeType::WasteLand },

            _ => BiomeType::Plains,
        }
    }
}

// ======================================
//           Default Registry
// ======================================

pub fn build_default_biome_registry(world_seed: WorldSeed) -> BiomeRegistry {
    let mut items = Vec::new();
    let mut mk = |_type: BiomeType,
                  name: &'static str,
                  tr: RangeInclusive<f32>,
                  mr: RangeInclusive<f32>,
                  elev_bias: f32,
                  freq: f32,
                  oct: u8| {
        let noise = BiomeNoiseProfile {
            seed: hash_with(world_seed, _type),
            frequency: freq,
            lacunarity: 2.0,
            gain: 0.5,
            octaves: oct,
        };
        items.push(BiomeDef { _type, name, temp_range: tr, moisture_range: mr, elevation_bias: elev_bias, noise });
    };

    // Hot
    mk(BiomeType::Desert,   "Desert",   0.75..=1.0, 0.00..=0.35, -0.1, 0.003, 3);
    mk(BiomeType::WasteLand,"Wasteland",0.70..=1.0, 0.30..=0.60,  0.0, 0.004, 3);
    mk(BiomeType::Jungle,   "Jungle",   0.75..=1.0, 0.65..=1.00,  0.1, 0.006, 4);
    mk(BiomeType::BloodForest,  "Blood Forest",  0.65..=0.85,0.35..=0.65, -0.05,0.003, 3);

    // Normal
    mk(BiomeType::Plains,   "Plains",   0.40..=0.70,0.35..=0.70, -0.10,0.002, 2);
    mk(BiomeType::Forest,   "Forest",   0.45..=0.75,0.55..=1.00,  0.00,0.005, 4);
    mk(BiomeType::Swamp,    "Swamp",    0.50..=0.90,0.85..=1.00, -0.25,0.007, 4);
    mk(BiomeType::Mountain,"Mountains",0.35..=0.75,0.20..=0.80,  0.40,0.008, 5);

    // Cold
    mk(BiomeType::SnowPlains,    "Snowy Plains",    0.25..=0.45,0.40..=1.00,  0.05,0.005, 4);
    mk(BiomeType::SnowMountain,   "Snowy Mountains",   0.10..=0.30,0.20..=0.80,  0.10,0.004, 3);

    // Ocean / Shore
    mk(BiomeType::Beach,    "Beach",    0.30..=0.90,0.30..=0.90, -0.30,0.002, 2);
    mk(BiomeType::Ocean,    "Ocean",    0.10..=0.90,0.30..=1.00, -0.50,0.001, 1);

    BiomeRegistry { items }
}

// ======================================
//                  Utils
// ======================================

fn hash_with<T: Hash>(seed: WorldSeed, salt: T) -> u64 {
    let mut h = DefaultHasher::new();
    seed.hash(&mut h);
    salt.hash(&mut h);
    h.finish()
}