use crate::chunk::chunk_utils::{col_rand_u32, map01};
use bevy::prelude::*;
use fastnoise_lite::*;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::biome::{Biome, BiomeSize};
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::{ChunkData, SEA_LEVEL};
use game_core::world::chunk_dim::{CX, CY, CZ, Y_MIN};

/* ========================= Defaults ========================== */

const OCEAN_FREQ:  f32 = 0.012;
const OCEAN_AMP:   f32 = 12.0;
const PLAINS_FREQ: f32 = 0.008;
const PLAINS_AMP:  f32 = 22.0;

/* ========================= Field / Coast Params ========================== */

const BASE_CELL_CHUNKS: i32 = 8;
const SEARCH_RADIUS_CELLS: i32 = 3;
const JITTER_FRAC: f32 = 0.35;

// accept land only a bit beyond its nominal radius
const LAND_SCORE_MAX: f32 = 1.02;

// min area = 3/4 of max (per size)
const SIZE_MIN_FRAC: f32 = 0.75;

// --- Oceans: larger + more common ---
const OCEAN_MIN_AREA: f32 = 4000.0;
const OCEAN_MAX_AREA: f32 = 30000.0;
const OCEAN_WEIGHT_MULTI: f64 = 3.5; // stronger bias towards oceans

// label smoothing (fallback only)
const SMOOTH_RADIUS_CH: i32 = 1;
const SMOOTH_ITERS: usize  = 1;

// coast shaping (now driven by land-score only)
const COAST_INSET_SCORE: f32 = 0.12;  // slight beach inside land (<1.0)
const COAST_BAND_SCORE:  f32 = 0.35;  // width outside land (>1.0)
const COAST_NOISE_FREQ:  f32 = 0.03;
const COAST_NOISE_AMP_SCORE: f32 = 0.10;
const COAST_DETAIL_FREQ: f32 = 0.12;
const BEACH_MIN: i32 = 3;
const BEACH_MAX: i32 = 8;

/* ========================= Salts ========================== */

const SALT_PICK_BIOME: u32 = 0xB10E_55ED;
const SALT_PICK_SIZE:  u32 = 0x51AE_0001 ^ 0x0000_1234;
const SALT_JITTER_X:   u32 = 0xA11E_D00F;
const SALT_JITTER_Z:   u32 = 0xC0FF_EE00;
const SALT_COAST:      i32 = 0x00C0_4751;
const SALT_COAST2:     i32 = 0xB34C_0001u32 as i32;

/* ========================= Generator =================================== */

pub(crate) async fn generate_chunk_async_biome(
    coord: IVec2,
    reg: &BlockRegistry,
    cfg_seed: i32,
    biomes: &BiomeRegistry,
) -> ChunkData {
    // Fallback label (only used when we need a default material set)
    let fallback_label = choose_biome_label_smoothed(biomes, coord, cfg_seed);

    // Per-chunk noises
    let seafloor_n = make_seafloor_noise(cfg_seed, OCEAN_FREQ);
    let plains_n   = make_plains_noise(cfg_seed,   PLAINS_FREQ);
    let coast_n    = make_coast_noise(cfg_seed ^ SALT_COAST,  COAST_NOISE_FREQ);
    let coast_d    = make_coast_noise(cfg_seed ^ SALT_COAST2, COAST_DETAIL_FREQ);

    let pick_seed: u32 = (cfg_seed as u32) ^ 0x0CE4_11CE;
    let mut chunk = ChunkData::new();

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx  = coord.x * CX as i32 + lx as i32;
            let wz  = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            // Column position in chunk units (with sub-chunk precision)
            let px = coord.x as f32 + (lx as f32 + 0.5) / (CX as f32);
            let pz = coord.y as f32 + (lz as f32 + 0.5) / (CZ as f32);

            // Find the nearest land site (score = distance / radius) and nearest ocean site (only for materials/offsets)
            let (best_land, best_ocean) = best_land_and_ocean_sites(biomes, Vec2::new(px, pz), cfg_seed);

            // Land height (or fallback)
            let (land_biome, h_land, s_land) = if let Some((b, _pos, _r, s)) = best_land {
                let amp = b.settings.land_amp.unwrap_or(PLAINS_AMP);
                let off = b.settings.height_offset;
                (b, sample_plains_height(&plains_n, wxf, wzf, SEA_LEVEL, off, amp), s)
            } else {
                let b = fallback_label;
                let amp = b.settings.land_amp.unwrap_or(PLAINS_AMP);
                let off = b.settings.height_offset;
                (b, sample_plains_height(&plains_n, wxf, wzf, SEA_LEVEL, off, amp), f32::INFINITY)
            };

            // Ocean biome/materials (if none nearby, use any ocean biome from the registry as default)
            let ocean_biome = if let Some((b, _p, _r, _s)) = best_ocean {
                b
            } else {
                any_ocean_biome(biomes).unwrap_or(land_biome) // safe fallback
            };

            // Ocean height (offset/amp from ocean biome if possible)
            let h_ocean = {
                let amp = ocean_biome.settings.seafloor_amp.unwrap_or(OCEAN_AMP);
                let off = ocean_biome.settings.height_offset;
                sample_ocean_height(&seafloor_n, wxf, wzf, SEA_LEVEL, off, amp)
            };

            // --- Land-driven coast mask ---
            // Ocean weight depends ONLY on land score:
            // s_land << 1.0 -> inland (t_ocean ~ 0)
            // s_land ~ 1.0 -> coast (blend)
            // s_land >> 1.0 -> open ocean (t_ocean ~ 1)
            let coast_offset = (map01(coast_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0 * COAST_NOISE_AMP_SCORE;
            let t_ocean = smoothstep(
                1.0 - COAST_INSET_SCORE + coast_offset,
                1.0 + COAST_BAND_SCORE  + coast_offset,
                s_land
            );
            let t_land  = 1.0 - t_ocean;

            // Final height with a gentle slope into the sea
            let mut h_f = lerp(h_land, h_ocean, t_ocean)
                .clamp((Y_MIN + 1) as f32, (SEA_LEVEL + 170) as f32);
            if t_ocean > 0.55 { h_f = h_f.min((SEA_LEVEL - 1) as f32); }
            let h_final = h_f.round() as i32;

            // Dominant biome for column materials (beaches handled below)
            let dom_biome = if t_land >= 0.5 { land_biome } else { ocean_biome };

            // Resolve names/ids
            let top_name        = pick(&dom_biome.surface.top,        wx, wz, pick_seed ^ 0x11);
            let bottom_name     = pick(&dom_biome.surface.bottom,     wx, wz, pick_seed ^ 0x22);
            // beaches use ocean sea_floor (sand)
            let sea_floor_name  = pick(&ocean_biome.surface.sea_floor,wx, wz, pick_seed ^ 0x33);
            let upper_zero_name = pick(&dom_biome.surface.upper_zero, wx, wz, pick_seed ^ 0x44);

            let id_top        = reg.id_or_air(top_name);
            let id_bottom     = reg.id_or_air(bottom_name);
            let id_sea_floor  = reg.id_or_air(sea_floor_name); // SAND underwater and for beaches
            let id_upper_zero = reg.id_or_air(upper_zero_name);

            // Beach width jitter
            let bw_noise = map01(coast_d.get_noise_2d(wxf, wzf));
            let beach_cap = BEACH_MIN + ((BEACH_MAX - BEACH_MIN) as f32 * bw_noise).round() as i32;

            // On-land soil thickness
            let soil_cap = 3;

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let underwater = h_final < SEA_LEVEL;

                let id: BlockId = if underwater {
                    // Only sand underwater
                    id_sea_floor
                } else {
                    // Sand cap at the coast for beaches
                    let near_coast = t_ocean > 0.10 && t_ocean < 0.90 && (h_final - SEA_LEVEL).abs() <= 5;
                    if wy == h_final {
                        if near_coast { id_sea_floor } else { id_top }
                    } else if wy >= h_final - if near_coast { beach_cap } else { soil_cap } {
                        if near_coast { id_sea_floor } else { id_bottom }
                    } else {
                        id_upper_zero
                    }
                };

                if id != 0 { chunk.set(lx, ly, lz, id); }
            }
        }
    }

    chunk
}

/* ============================= Noises ======================================= */

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

fn make_plains_noise(seed: i32, freq: f32) -> FastNoiseLite {
    const SEED_SALT_PLAINS: i32 = 0x504C_4149; // 'PLAI'
    let mut n = FastNoiseLite::with_seed(seed ^ SEED_SALT_PLAINS);
    n.set_noise_type(Some(NoiseType::OpenSimplex2));
    n.set_frequency(Some(freq));
    n.set_fractal_type(Some(FractalType::FBm));
    n.set_fractal_octaves(Some(4));
    n.set_fractal_gain(Some(0.5));
    n.set_fractal_lacunarity(Some(2.0));
    n
}

fn make_coast_noise(seed: i32, freq: f32) -> FastNoiseLite {
    let mut n = FastNoiseLite::with_seed(seed);
    n.set_noise_type(Some(NoiseType::OpenSimplex2));
    n.set_frequency(Some(freq));
    n.set_fractal_type(Some(FractalType::FBm));
    n.set_fractal_octaves(Some(3));
    n.set_fractal_gain(Some(0.5));
    n.set_fractal_lacunarity(Some(2.0));
    n
}

/* ============================= Height Samplers =============================== */

#[inline]
fn sample_ocean_height(
    n: &FastNoiseLite,
    wxf: f32,
    wzf: f32,
    sea_level: i32,
    height_offset: f32,
    seafloor_amp: f32,
) -> f32 {
    let s = 0.9;
    let hn = map01(n.get_noise_2d(wxf * s, wzf * s));
    let undulation = (hn - 0.5) * seafloor_amp;
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

/* ============================= Biome Choice ================================= */

fn choose_biome_label_smoothed<'a>(
    biomes: &'a BiomeRegistry,
    coord: IVec2,
    seed: i32,
) -> &'a Biome {
    if SMOOTH_RADIUS_CH <= 0 || SMOOTH_ITERS == 0 {
        return choose_biome_label_thresholded(biomes, coord, seed);
    }

    let mut label = choose_biome_label_thresholded(biomes, coord, seed);

    for _ in 0..SMOOTH_ITERS {
        let mut counts: Vec<(&'a Biome, u32)> = Vec::new();

        for dz in -SMOOTH_RADIUS_CH..=SMOOTH_RADIUS_CH {
            for dx in -SMOOTH_RADIUS_CH..=SMOOTH_RADIUS_CH {
                let b = choose_biome_label_thresholded(
                    biomes,
                    IVec2::new(coord.x + dx, coord.y + dz),
                    seed,
                );
                if let Some((_, c)) = counts.iter_mut().find(|(bi, _)| std::ptr::eq(*bi, b)) {
                    *c += 1;
                } else {
                    counts.push((b, 1));
                }
            }
        }

        counts.sort_by(|(a, ca), (b, cb)| {
            cb.cmp(ca)
                .then_with(|| b.rarity.partial_cmp(&a.rarity).unwrap_or(std::cmp::Ordering::Equal))
        });

        if let Some((b, _)) = counts.first() { label = *b; }
    }

    label
}

fn choose_biome_label_thresholded(
    biomes: &BiomeRegistry,
    coord: IVec2,
    seed: i32,
) -> &Biome {
    let px = coord.x as f32 + 0.5;
    let pz = coord.y as f32 + 0.5;
    let (best_land, best_ocean) = best_land_and_ocean_sites(biomes, Vec2::new(px, pz), seed);

    if let Some((b, _pos, _r, s)) = best_land {
        if s <= LAND_SCORE_MAX { return b; }
    }
    if let Some((b, _pos, _r, _s)) = best_ocean { return b; }
    biomes.by_name.get(&biomes.ordered_names[0]).unwrap()
}

/* -------- Sites & Sizes -------- */

fn best_land_and_ocean_sites<'a>(
    biomes: &'a BiomeRegistry,
    p_chunks: Vec2,
    world_seed: i32,
) -> (Option<(&'a Biome, Vec2, f32, f32)>, Option<(&'a Biome, Vec2, f32, f32)>) {
    let gx = (p_chunks.x.floor() as i32).div_euclid(BASE_CELL_CHUNKS);
    let gz = (p_chunks.y.floor() as i32).div_euclid(BASE_CELL_CHUNKS);

    let mut best_land:  Option<(&'a Biome, Vec2, f32, f32)> = None;
    let mut best_ocean: Option<(&'a Biome, Vec2, f32, f32)> = None;

    for dz in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
        for dx in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
            let cx = gx + dx;
            let cz = gz + dz;

            let (site_pos, site_biome, site_radius) =
                site_properties_for_cell(biomes, cx, cz, world_seed);

            let d = p_chunks.distance(site_pos);
            let score = d / site_radius.max(1.0);

            if is_ocean_biome(site_biome) {
                if best_ocean.map_or(true, |(_,_,_,s)| score < s) {
                    best_ocean = Some((site_biome, site_pos, site_radius, score));
                }
            } else {
                if best_land.map_or(true, |(_,_,_,s)| score < s) {
                    best_land = Some((site_biome, site_pos, site_radius, score));
                }
            }
        }
    }

    (best_land, best_ocean)
}

fn site_properties_for_cell(
    biomes: &BiomeRegistry,
    cell_x: i32,
    cell_z: i32,
    world_seed: i32,
) -> (Vec2, &Biome, f32) {
    let cell_w = BASE_CELL_CHUNKS as f32;
    let jx = (rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_JITTER_X) - 0.5) * 2.0 * JITTER_FRAC * cell_w;
    let jz = (rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_JITTER_Z) - 0.5) * 2.0 * JITTER_FRAC * cell_w;

    let center_x = (cell_x as f32 + 0.5) * cell_w + jx;
    let center_z = (cell_z as f32 + 0.5) * cell_w + jz;
    let pos = Vec2::new(center_x, center_z);

    let r = rand01(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_BIOME) as f64;
    let biome = rarity_pick(biomes, r).expect("No biomes registered");

    // draw area uniformly from [min, max]
    let (area_min, area_max) = size_to_area_bounds(
        if biome.sizes.is_empty() { &BiomeSize::Medium } else { &biome.sizes[ (rand_u32(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_SIZE) as usize) % biome.sizes.len() ] }
    );
    let t = rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xFACE_FEED));
    let target_area_chunks = area_min + t * (area_max - area_min);

    // area -> radius with tiny jitter, enforce minimum
    let mut radius_chunks = (target_area_chunks / std::f32::consts::PI).sqrt();
    let jitter = 0.95 + 0.10 * rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xDEAD_BEEF));
    radius_chunks *= jitter;
    let min_r = (area_min / std::f32::consts::PI).sqrt();
    radius_chunks = radius_chunks.max(min_r * 0.98);

    (pos, biome, radius_chunks.max(1.0))
}

fn rarity_pick(biomes: &BiomeRegistry, r01: f64) -> Option<&Biome> {
    if biomes.ordered_names.is_empty() { return None; }

    // Adjusted weights: slight boost for oceans so they appear more often.
    let mut adj: Vec<f64> = Vec::with_capacity(biomes.ordered_names.len());
    for (i, &w) in biomes.weights.iter().enumerate() {
        let name = &biomes.ordered_names[i];
        let base = (w as f64).max(0.0);
        let multi = if let Some(b) = biomes.by_name.get(name) {
            if is_ocean_biome(b) { OCEAN_WEIGHT_MULTI } else { 1.0 }
        } else { 1.0 };
        adj.push(base * multi);
    }

    let sum: f64 = adj.iter().sum();
    if sum <= 0.0 {
        let name = &biomes.ordered_names[0];
        return biomes.by_name.get(name);
    }

    let target = r01.min(0.999_999_999).max(0.0) * sum;
    let mut acc = 0.0_f64;
    for (i, w) in adj.iter().enumerate() {
        acc += *w;
        if acc > target {
            let name = &biomes.ordered_names[i];
            return biomes.by_name.get(name);
        }
    }
    let last = biomes.ordered_names.last().unwrap();
    biomes.by_name.get(last)
}

fn size_to_area_bounds(size: &BiomeSize) -> (f32, f32) {
    // numbers are interpreted as "Max chunks"; min = 0.75 * Max
    match size {
        BiomeSize::Tiny   => (SIZE_MIN_FRAC * 20.0,  20.0),
        BiomeSize::Small  => (SIZE_MIN_FRAC * 56.0,  56.0),
        BiomeSize::Medium => (SIZE_MIN_FRAC * 96.0,  96.0),
        BiomeSize::Large  => (SIZE_MIN_FRAC * 196.0, 196.0),
        BiomeSize::Huge   => (SIZE_MIN_FRAC * 392.0, 392.0),
        BiomeSize::Giant  => (SIZE_MIN_FRAC * 560.0, 560.0),
        BiomeSize::Ocean  => (OCEAN_MIN_AREA, OCEAN_MAX_AREA),
    }
}

/* ============================= Internal ======================================= */

#[inline]
fn pick(list: &[String], wx: i32, wz: i32, seed: u32) -> &str {
    if list.is_empty() { return "stone_block"; }
    let r = col_rand_u32(wx, wz, seed);
    let idx = (r as usize) % list.len();
    &list[idx]
}

#[inline] fn lerp(a: f32, b: f32, t: f32) -> f32 { a + (b - a) * t }

#[inline]
fn smoothstep(e0: f32, e1: f32, x: f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

#[inline]
fn clamp_world_y(y: f32) -> f32 {
    ((Y_MIN + 1) as f32).max(y)
}

#[inline]
fn rand_u32(x: i32, z: i32, seed: u32) -> u32 { col_rand_u32(x, z, seed) }

#[inline]
fn rand01(x: i32, z: i32, seed: u32) -> f32 {
    let u = rand_u32(x, z, seed) as f64;
    (u / ((u32::MAX as f64) + 1.0)) as f32
}

#[inline]
fn is_ocean_biome(b: &Biome) -> bool {
    if b.name.eq_ignore_ascii_case("ocean") { return true; }
    b.sizes.iter().any(|s| matches!(s, BiomeSize::Ocean))
}

// Pick any ocean biome from a registry (used as a generic ocean material/offset source)
fn any_ocean_biome(biomes: &BiomeRegistry) -> Option<&Biome> {
    for b in biomes.by_name.values() {
        if is_ocean_biome(b) { return Some(b); }
    }
    None
}
