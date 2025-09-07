use crate::world::biome::registry::BiomeRegistry;
use crate::world::biome::{Biome, BiomeSize};
use bevy::prelude::*;

/* ========================= Defaults ========================== */

pub const OCEAN_FREQ:  f32 = 0.012;
pub const OCEAN_AMP:   f32 = 12.0;
pub const PLAINS_FREQ: f32 = 0.008;
pub const PLAINS_AMP:  f32 = 22.0;

/* ========================= Field / Coast Params ========================== */

pub const BASE_CELL_CHUNKS: i32 = 8;
pub const SEARCH_RADIUS_CELLS: i32 = 3;
pub const JITTER_FRAC: f32 = 0.35;

pub const LAND_SCORE_MAX: f32 = 1.02;
pub const SIZE_MIN_FRAC: f32 = 0.75;

pub const OCEAN_MIN_AREA: f32 = 4000.0;
pub const OCEAN_MAX_AREA: f32 = 30000.0;
pub const OCEAN_WEIGHT_MULTI: f64 = 3.5;

pub const SMOOTH_RADIUS_CH: i32 = 1;
pub const SMOOTH_ITERS: usize  = 1;

pub const COAST_INSET_SCORE: f32 = 0.12;
pub const COAST_BAND_SCORE:  f32 = 0.35;
pub const COAST_NOISE_FREQ:  f32 = 0.03;
pub const COAST_NOISE_AMP_SCORE: f32 = 0.10;
pub const COAST_DETAIL_FREQ: f32 = 0.12;
pub const BEACH_MIN: i32 = 3;
pub const BEACH_MAX: i32 = 8;

/* ========================= Sub-biome control ==================== */

pub const SUB_COAST_LIMIT: f32 = 1.15;
pub const SUB_PRESENT_MIN: f32 = 0.05;
pub const SUB_PRESENT_MAX: f32 = 0.70;

pub const SUB_CORE_START: f32 = 0.28;
pub const SUB_CORE_END: f32   = 1.05;
pub const SUB_EDGE_NOISE_FREQ: f32 = 0.02;
pub const SUB_EDGE_NOISE_AMP:  f32 = 0.06;

/* ========================= Mountain shaping ================= */

pub const MNT_BASE_FREQ: f32 = 0.02;
pub const MNT_DOME_GAIN: f32 = 0.55;
pub const MNT_DOME_EXP:  f32 = 1.55;
pub const MNT_DETAIL_EDGE_FADE_START: f32 = 0.10;
pub const MNT_DETAIL_EDGE_FADE_END:   f32 = 0.40;
pub const MNT_WORLD_SLOPE: f32 = 0.90;

pub const FOREIGN_GUARD_START: f32 = 0.95;
pub const FOREIGN_GUARD_END:   f32 = 1.20;

/* ========================= Salts ========================== */

pub const SALT_PICK_BIOME: u32 = 0xB10E_55ED;
pub const SALT_PICK_SIZE:  u32 = 0x51AE_0001 ^ 0x0000_1234;
pub const SALT_JITTER_X:   u32 = 0xA11E_D00F;
pub const SALT_JITTER_Z:   u32 = 0xC0FF_EE00;
pub const SALT_COAST:      i32 = 0x00C0_4751;
pub const SALT_COAST2:     i32 = 0xB34C_0001u32 as i32;
pub const SALT_SUB_SITES:  u32 = 0x5AB5_1735;
pub const SALT_SUB_EDGE:   i32 = 0x53AB_CAFEi32;

/* ============================= Biome Choice ================================= */

pub fn choose_biome_label_smoothed<'a>(
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
                let b = choose_biome_label_thresholded(biomes, IVec2::new(coord.x + dx, coord.y + dz), seed);
                if let Some((_, c)) = counts.iter_mut().find(|(bi, _)| std::ptr::eq(*bi, b)) { *c += 1; }
                else { counts.push((b, 1)); }
            }
        }
        counts.sort_by(|(a, ca), (b, cb)| cb.cmp(ca).then_with(|| b.rarity.partial_cmp(&a.rarity).unwrap_or(std::cmp::Ordering::Equal)));
        if let Some((b, _)) = counts.first() { label = *b; }
    }
    label
}

pub fn choose_biome_label_thresholded(
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

pub fn best_land_and_ocean_sites<'a>(
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

/* -- nearest "another" land site for land/land blending -- */
pub fn best_second_land_site<'a>(
    biomes: &'a BiomeRegistry,
    p_chunks: Vec2,
    world_seed: i32,
    host_site_pos: Vec2,
) -> Option<(&'a Biome, Vec2, f32, f32)> {
    let gx = (p_chunks.x.floor() as i32).div_euclid(BASE_CELL_CHUNKS);
    let gz = (p_chunks.y.floor() as i32).div_euclid(BASE_CELL_CHUNKS);

    let mut best: Option<(&'a Biome, Vec2, f32, f32)> = None;

    for dz in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
        for dx in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
            let cx = gx + dx;
            let cz = gz + dz;

            let (site_pos, site_biome, site_radius) =
                site_properties_for_cell(biomes, cx, cz, world_seed);

            // only land
            if is_ocean_biome(site_biome) { continue; }
            // skip ONLY the exact same site (same center as host)
            if (site_pos - host_site_pos).length_squared() < 1e-6 { continue; }

            let d = p_chunks.distance(site_pos);
            let score = d / site_radius.max(1.0);

            if best.map_or(true, |(_, _, _, s)| score < s) {
                best = Some((site_biome, site_pos, site_radius, score));
            }
        }
    }

    best
}

pub fn site_properties_for_cell(
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

    // Sites may only come from standalone land biomes and oceans
    let biome = rarity_pick_site(biomes, r).expect("No biomes registered");

    // Draw area uniformly from [min, max]
    let (area_min, area_max) = size_to_area_bounds(
        if biome.sizes.is_empty() { &BiomeSize::Medium } else {
            &biome.sizes[(rand_u32(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_SIZE) as usize) % biome.sizes.len()]
        }
    );

    let t = rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xFACE_FEED));
    let target_area_chunks = area_min + t * (area_max - area_min);

    // area -> radius with small jitter, enforce a minimum radius
    let mut radius_chunks = (target_area_chunks / std::f32::consts::PI).sqrt();
    let jitter = 0.95 + 0.10 * rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xDEAD_BEEF));
    radius_chunks *= jitter;
    let min_r = (area_min / std::f32::consts::PI).sqrt();
    radius_chunks = radius_chunks.max(min_r * 0.98);

    (pos, biome, radius_chunks.max(1.0))
}

pub fn rarity_pick_site(biomes: &BiomeRegistry, r01: f64) -> Option<&Biome> {
    if biomes.ordered_names.is_empty() { return None; }

    let mut total = 0.0f64;
    let mut eff: Vec<f64> = Vec::with_capacity(biomes.ordered_names.len());
    for (i, &w) in biomes.weights.iter().enumerate() {
        let name = &biomes.ordered_names[i];
        let base = (w as f64).max(0.0);
        let multi = if let Some(b) = biomes.by_name.get(name) {
            if is_ocean_biome(b) { OCEAN_WEIGHT_MULTI }
            else if b.stand_alone { 1.0 } else { 0.0 }
        } else { 0.0 };
        let v = base * multi;
        eff.push(v);
        total += v;
    }

    if total <= 0.0 {
        let name = &biomes.ordered_names[0];
        return biomes.by_name.get(name);
    }

    let target = r01.min(0.999_999_999).max(0.0) * total;
    let mut acc = 0.0;
    for (i, v) in eff.iter().enumerate() {
        acc += *v;
        if acc > target {
            let name = &biomes.ordered_names[i];
            return biomes.by_name.get(name);
        }
    }
    let last = biomes.ordered_names.last().unwrap();
    biomes.by_name.get(last)
}

pub fn slope_to_soil_cap(core: f32, delta_c: f32, n8: [f32; 8]) -> i32 {
    if core <= 0.0 { return 3; } // plains/default

    // max absolute difference to center (proxy for a slope)
    let mut max_diff = 0.0f32;
    let mut sum = 0.0f32;
    for v in n8 {
        let d = (v - delta_c).abs();
        if d > max_diff { max_diff = d; }
        sum += v;
    }
    let mean = sum * 0.125;
    let mean_diff = (mean - delta_c).abs();

    // roughness combines sharp contrast and average tilt; scaled by core
    let rough = (max_diff * 0.85 + mean_diff * 0.40) * core;

    // Convert to extra soil thickness in blocks
    let extra = (rough * 2.6).round() as i32; // tune factor to taste
    (3 + extra).clamp(3, 12)
}

pub fn size_to_area_bounds(size: &BiomeSize) -> (f32, f32) {
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

#[inline]
pub fn col_rand_u32(x: i32, z: i32, seed: u32) -> u32 {
    // Mix (x, z, seed) into 64-bit, then avalanche (MurMur/xxHash-style).
    let mut h = (x as u64).wrapping_mul(0x517C_C1B7_2722_0A95);
    h ^= (z as u64).wrapping_mul(0x2545_F491_4F6C_DD1D);
    h ^= (seed as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);

    // Finalizers (from Murmur3 fix64)
    h ^= h >> 33;
    h = h.wrapping_mul(0xFF51_AFD7_ED55_8CCD);
    h ^= h >> 33;
    h = h.wrapping_mul(0xC4CE_B9FE_1A85_EC53);
    h ^= h >> 33;

    (h & 0xFFFF_FFFF) as u32
}

/// Same hash but as [0,1) float. Handy for thresholds / roulette wheel.
#[inline]
pub fn col_rand_f32(x: i32, z: i32, seed: u32) -> f32 {
    let u = col_rand_u32(x, z, seed) as f64;
    (u / ((u32::MAX as f64) + 1.0)) as f32
}

/// Integer range helper: inclusive [lo, hi].
#[inline]
pub fn col_rand_range_u32(x: i32, z: i32, seed: u32, lo: u32, hi: u32) -> u32 {
    if lo >= hi { return lo; }
    lo + (col_rand_u32(x, z, seed) % (hi - lo + 1))
}

#[inline]
pub fn rand_u32(x: i32, z: i32, seed: u32) -> u32 { col_rand_u32(x, z, seed) }

#[inline]
pub fn rand01(x: i32, z: i32, seed: u32) -> f32 {
    let u = rand_u32(x, z, seed) as f64;
    (u / ((u32::MAX as f64) + 1.0)) as f32
}

#[inline]
pub fn is_ocean_biome(b: &Biome) -> bool {
    if b.name.eq_ignore_ascii_case("ocean") { return true; }
    b.sizes.iter().any(|s| matches!(s, BiomeSize::Ocean))
}

pub fn any_ocean_biome(biomes: &BiomeRegistry) -> Option<&Biome> {
    for b in biomes.by_name.values() {
        if is_ocean_biome(b) { return Some(b); }
    }
    None
}
