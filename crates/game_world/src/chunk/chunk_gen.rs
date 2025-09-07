use crate::chunk::chunk_utils::{col_rand_u32, map01};
use bevy::prelude::*;
use fastnoise_lite::*;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::biome::{Biome, BiomeSize};
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::{ChunkData, SEA_LEVEL};
use game_core::world::chunk_dim::{CX, CY, CZ, Y_MIN};

const OCEAN_FREQ: f32 = 0.012;
const OCEAN_AMP:  f32 = 12.0;
const PLAINS_FREQ: f32 = 0.008;
const PLAINS_AMP:  f32 = 22.0;

/* ========================= BIOME FIELD PARAMS ========================== */

const BASE_CELL_CHUNKS: i32 = 8;        // coarse grid cell size in chunks
const SEARCH_RADIUS_CELLS: i32 = 3;     // neighborhood for nearest-site search
const JITTER_FRAC: f32 = 0.35;          // site jitter inside a cell (fraction of cell size)

// salts for deterministic hashing
const SALT_PICK_BIOME: u32 = 0xB10E_55ED;
const SALT_PICK_SIZE:  u32 = 0x51AE_0001 ^ 0x0000_1234; // any fixed number is fine
const SALT_JITTER_X:   u32 = 0xA11E_D00F;
const SALT_JITTER_Z:   u32 = 0xC0FF_EE00;

/* ========================= GENERATOR =================================== */

pub(crate) async fn generate_chunk_async_biome(
    coord: IVec2,
    reg: &BlockRegistry,
    cfg_seed: i32,
    biomes: &BiomeRegistry,
) -> ChunkData {

    let (biome, _site_pos_chunks, _site_radius_chunks) =
        choose_biome_for_chunk(biomes, coord, cfg_seed);

    let sea_floor_freq = biome.settings.seafloor_freq.unwrap_or(OCEAN_FREQ);
    let sea_floor_amp = biome.settings.seafloor_amp.unwrap_or(OCEAN_AMP);

    let plains_freq = biome.settings.land_freq.unwrap_or(PLAINS_FREQ);
    let plains_amp = biome.settings.land_amp.unwrap_or(PLAINS_AMP);

    // --- build needed noise(s) for this biome ---
    let mut seafloor_n = None;
    let mut plains_n   = None;

    let is_ocean = biome.name.eq_ignore_ascii_case("ocean");
    if is_ocean {
        seafloor_n = Some(make_seafloor_noise(cfg_seed, sea_floor_freq));
    } else {
        plains_n = Some(make_plains_noise(cfg_seed, plains_freq));
    }

    // column pick seed
    let pick_seed: u32 = (cfg_seed as u32) ^ 0x0CE4_11CE;

    let mut chunk = ChunkData::new();

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            // sample height per biome
            let h_f = if is_ocean {
                let n = seafloor_n.as_ref().unwrap();
                sample_ocean_height(n, wxf, wzf, SEA_LEVEL, biome.settings.height_offset, sea_floor_amp)
            } else {
                let n = plains_n.as_ref().unwrap();
                sample_plains_height(n, wxf, wzf, SEA_LEVEL, biome.settings.height_offset, plains_amp)
            };
            let h_final = h_f.round() as i32;

            // resolve surface blocks
            let top_name        = pick(&biome.surface.top,        wx, wz, pick_seed ^ 0x11);
            let bottom_name     = pick(&biome.surface.bottom,     wx, wz, pick_seed ^ 0x22);
            let sea_floor_name  = pick(&biome.surface.sea_floor,  wx, wz, pick_seed ^ 0x33);
            let upper_zero_name = pick(&biome.surface.upper_zero, wx, wz, pick_seed ^ 0x44);
            let under_zero_name = pick(&biome.surface.under_zero, wx, wz, pick_seed ^ 0x55);

            let id_top        = reg.id_or_air(top_name);
            let id_bottom     = reg.id_or_air(bottom_name);
            let id_sea_floor  = reg.id_or_air(sea_floor_name);
            let id_upper_zero = reg.id_or_air(upper_zero_name);
            let id_under_zero = reg.id_or_air(under_zero_name);

            // layer thickness
            let seabed_cap  = 2;
            let subsoil_cap = 4;
            let beach_cap   = 3;
            let soil_cap    = 3;

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let underwater = h_final < SEA_LEVEL;

                let id: BlockId = if underwater {
                    if wy >= h_final - (seabed_cap - 1) { id_sea_floor }
                    else if wy >= h_final - (seabed_cap + subsoil_cap - 1) { id_under_zero }
                    else { id_upper_zero }
                } else if is_ocean {
                    if wy == h_final { id_top }
                    else if wy >= h_final - beach_cap { id_bottom }
                    else { id_upper_zero }
                } else {
                    if wy == h_final { id_top }
                    else if wy >= h_final - soil_cap { id_bottom }
                    else { id_upper_zero }
                };

                if id != 0 { chunk.set(lx, ly, lz, id); }
            }
        }
    }

    chunk
}

// ============================= Noises =======================================

/// Ocean
fn make_seafloor_noise(seed: i32, freq: f32) -> FastNoiseLite {
    let mut n = FastNoiseLite::with_seed(seed);
    n.set_noise_type(Some(NoiseType::OpenSimplex2));
    n.set_frequency(Some(freq));
    n.set_fractal_type(Some(FractalType::FBm));
    n.set_fractal_octaves(Some(3));
    n.set_fractal_gain(Some(0.5));
    n.set_fractal_lacunarity(Some(2.0));
    n
}

/// Plains
fn make_plains_noise(seed: i32, freq: f32) -> FastNoiseLite {
    let mut n = FastNoiseLite::with_seed(seed ^ 0x504C_4149);
    n.set_noise_type(Some(NoiseType::OpenSimplex2));
    n.set_frequency(Some(freq));
    n.set_fractal_type(Some(FractalType::FBm));
    n.set_fractal_octaves(Some(4));
    n.set_fractal_gain(Some(0.5));
    n.set_fractal_lacunarity(Some(2.0));
    n
}

// ============================= Helpers =======================================

#[inline]
fn sample_ocean_height(
    n: &FastNoiseLite,
    wxf: f32,
    wzf: f32,
    sea_level: i32,
    height_offset: f32,
    seafloor_amp: f32,
) -> f32 {
    // Domain scale kept slightly coarser than a block scale
    let s = 0.9;
    let hn = map01(n.get_noise_2d(wxf * s, wzf * s));
    let undulation = (hn - 0.5) * seafloor_amp; // controls relief
    let base = sea_level as f32 + height_offset;
    clamp_world_y(base + undulation).min((sea_level - 2) as f32)
}

#[inline]
fn sample_plains_height(
    n: &FastNoiseLite,
    wxf: f32,
    wzf: f32,
    sea_level: i32,
    height_offset: f32,
    land_amp: f32,
) -> f32 {
    let hn = map01(n.get_noise_2d(wxf, wzf));
    let undulation = (hn - 0.5) * land_amp;
    let base = sea_level as f32 + height_offset;
    clamp_world_y(base + undulation)
}

fn choose_biome_for_chunk(
    biomes: &BiomeRegistry,
    chunk_c: IVec2,          // in chunks
    world_seed: i32,
) -> (&Biome, Vec2, f32) {
    // convert chunk -> coarse grid cell
    let gx = div_floor(chunk_c.x, BASE_CELL_CHUNKS);
    let gz = div_floor(chunk_c.y, BASE_CELL_CHUNKS);

    let mut best_score = f32::INFINITY;
    let mut best: Option<(&Biome, Vec2, f32)> = None;

    for dz in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
        for dx in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
            let cx = gx + dx;
            let cz = gz + dz;

            // site properties for this cell
            let (site_pos, site_biome, site_radius) =
                site_properties_for_cell(biomes, cx, cz, world_seed);

            // distance in chunks from current chunk center to site center
            let p = Vec2::new(chunk_c.x as f32 + 0.5, chunk_c.y as f32 + 0.5);
            let d = p.distance(site_pos);

            // variable-radius Voronoi score
            let score = d / site_radius.max(1.0);

            if score < best_score {
                best_score = score;
                best = Some((site_biome, site_pos, site_radius));
            }
        }
    }

    best.expect("No biome sites found")
}

fn site_properties_for_cell(
    biomes: &BiomeRegistry,
    cell_x: i32,
    cell_z: i32,
    world_seed: i32,
) -> (Vec2, &Biome, f32) {
    // jitter inside the cell (in chunks)
    let cell_w = BASE_CELL_CHUNKS as f32;
    let jx = (rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_JITTER_X) - 0.5) * 2.0 * JITTER_FRAC * cell_w;
    let jz = (rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_JITTER_Z) - 0.5) * 2.0 * JITTER_FRAC * cell_w;

    let center_x = (cell_x as f32 + 0.5) * cell_w + jx;
    let center_z = (cell_z as f32 + 0.5) * cell_w + jz;
    let pos = Vec2::new(center_x, center_z);

    // rarity-based biome pick (deterministic)
    let r = rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_BIOME) as f64;
    let biome = rarity_pick(biomes, r).expect("No biomes registered");

    // pick a size entry from the biome.sizes list (random among listed)
    let size = {
        let sizes = if biome.sizes.is_empty() {
            // default to Medium if not specified
            vec![BiomeSize::Medium]
        } else {
            biome.sizes.clone()
        };
        let idx = (rand_u32(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_SIZE) as usize) % sizes.len();
        sizes[idx].clone()
    };

    // convert size -> target area (chunks) -> radius (chunks)
    let target_area_chunks = size_to_area_chunks(&size);
    let mut radius_chunks = (target_area_chunks / std::f32::consts::PI).sqrt();

    // slight per-site radius variation to break uniform outlines
    let jitter = 0.85 + 0.30 * rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xDEAD_BEEF));
    radius_chunks *= jitter;

    (pos, biome, radius_chunks.max(1.0))
}

fn rarity_pick(biomes: &BiomeRegistry, r01: f64) -> Option<&Biome> {
    if biomes.ordered_names.is_empty() { return None; }
    let mut sum = 0.0_f64;
    for &w in &biomes.weights {
        sum += (w as f64).max(0.0);
    }
    if sum <= 0.0 {
        let name = &biomes.ordered_names[0];
        return biomes.by_name.get(name);
    }
    let mut acc = 0.0_f64;
    let target = (r01.min(0.999_999_999)).max(0.0) * sum;
    for (i, &w) in biomes.weights.iter().enumerate() {
        acc += (w as f64).max(0.0);
        if acc > target {
            let name = &biomes.ordered_names[i];
            return biomes.by_name.get(name);
        }
    }
    let last = biomes.ordered_names.last().unwrap();
    biomes.by_name.get(last)
}

fn size_to_area_chunks(size: &BiomeSize) -> f32 {
    match size {
        BiomeSize::Tiny   => 20.0,
        BiomeSize::Small  => 56.0,
        BiomeSize::Medium => 98.0,
        BiomeSize::Large  => 196.0,
        BiomeSize::Huge   => 392.0,
        BiomeSize::Giant  => 560.0,
        BiomeSize::Ocean  => 600.0,
    }
}

// ============================= Internal =======================================
#[inline]
fn pick(list: &[String], wx: i32, wz: i32, seed: u32) -> &str {
    // Deterministic pick from a non-empty list using per-column rng
    // (If a list is empty, we fall back to a safe default below.)
    if list.is_empty() { return "stone_block"; }
    let r = col_rand_u32(wx, wz, seed);
    let idx = (r as usize) % list.len();
    &list[idx]
}

#[inline]
fn clamp_world_y(y: f32) -> f32 {
    ((Y_MIN + 1) as f32).max(y)
}

#[inline]
fn div_floor(a: i32, b: i32) -> i32 {
    let d = a / b;
    let r = a % b;
    if (r != 0) && ((r > 0) != (b > 0)) { d - 1 } else { d }
}

#[inline]
fn rand_u32(x: i32, z: i32, seed: u32) -> u32 {
    col_rand_u32(x, z, seed)
}

#[inline]
fn rand01(x: i32, z: i32, seed: u32) -> f32 {
    let u = rand_u32(x, z, seed) as f64;
    (u / ((u32::MAX as f64) + 1.0)) as f32
}