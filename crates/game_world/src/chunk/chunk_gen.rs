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

const LAND_SCORE_MAX: f32 = 1.02;
const SIZE_MIN_FRAC: f32 = 0.75;

const OCEAN_MIN_AREA: f32 = 4000.0;
const OCEAN_MAX_AREA: f32 = 30000.0;
const OCEAN_WEIGHT_MULTI: f64 = 3.5;

const SMOOTH_RADIUS_CH: i32 = 1;
const SMOOTH_ITERS: usize  = 1;

const COAST_INSET_SCORE: f32 = 0.12;
const COAST_BAND_SCORE:  f32 = 0.35;
const COAST_NOISE_FREQ:  f32 = 0.03;
const COAST_NOISE_AMP_SCORE: f32 = 0.10;
const COAST_DETAIL_FREQ: f32 = 0.12;
const BEACH_MIN: i32 = 3;
const BEACH_MAX: i32 = 8;

/* ========================= Sub-biome control ==================== */

const SUB_COAST_LIMIT: f32 = 1.15;
const SUB_PRESENT_MIN: f32 = 0.05;
const SUB_PRESENT_MAX: f32 = 0.70;

const SUB_CORE_START: f32 = 0.28;
const SUB_CORE_END: f32   = 1.05;
const SUB_EDGE_NOISE_FREQ: f32 = 0.02;
const SUB_EDGE_NOISE_AMP:  f32 = 0.06;

/* ========================= Mountain shaping ================= */

const MNT_BASE_FREQ: f32 = 0.02;
const MNT_DOME_GAIN: f32 = 0.55;
const MNT_DOME_EXP:  f32 = 1.55;
const MNT_DETAIL_EDGE_FADE_START: f32 = 0.10;
const MNT_DETAIL_EDGE_FADE_END:   f32 = 0.40;
const MNT_WORLD_SLOPE: f32 = 0.90;

const FOREIGN_GUARD_START: f32 = 0.95;
const FOREIGN_GUARD_END:   f32 = 1.20;

/* ========================= Salts ========================== */

const SALT_PICK_BIOME: u32 = 0xB10E_55ED;
const SALT_PICK_SIZE:  u32 = 0x51AE_0001 ^ 0x0000_1234;
const SALT_JITTER_X:   u32 = 0xA11E_D00F;
const SALT_JITTER_Z:   u32 = 0xC0FF_EE00;
const SALT_COAST:      i32 = 0x00C0_4751;
const SALT_COAST2:     i32 = 0xB34C_0001u32 as i32;
const SALT_SUB_SITES:  u32 = 0x5AB5_1735;
const SALT_SUB_EDGE:   i32 = 0x53AB_CAFEi32;

/* ========================= Generator =================================== */

pub(crate) async fn generate_chunk_async_biome(
    coord: IVec2,
    reg: &BlockRegistry,
    cfg_seed: i32,
    biomes: &BiomeRegistry,
) -> ChunkData {
    let fallback_label = choose_biome_label_smoothed(biomes, coord, cfg_seed);

    // noises
    let seafloor_n = make_seafloor_noise(cfg_seed, OCEAN_FREQ);
    let plains_n   = make_plains_noise(cfg_seed,   PLAINS_FREQ);
    let coast_n    = make_coast_noise(cfg_seed ^ SALT_COAST,  COAST_NOISE_FREQ);
    let coast_d    = make_coast_noise(cfg_seed ^ SALT_COAST2, COAST_DETAIL_FREQ);
    let sub_edge_n = make_coast_noise(cfg_seed ^ SALT_SUB_EDGE, SUB_EDGE_NOISE_FREQ);

    let pick_seed: u32 = (cfg_seed as u32) ^ 0x0CE4_11CE;
    let mut chunk = ChunkData::new();

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx  = coord.x * CX as i32 + lx as i32;
            let wz  = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            let px = coord.x as f32 + (lx as f32 + 0.5) / (CX as f32);
            let pz = coord.y as f32 + (lz as f32 + 0.5) / (CZ as f32);
            let p_chunks = Vec2::new(px, pz);

            let (best_land, best_ocean) = best_land_and_ocean_sites(biomes, p_chunks, cfg_seed);

            let (land_host, host_pos, host_r, s_land) = if let Some((b, pos, r, s)) = best_land {
                (b, pos, r, s)
            } else {
                (fallback_label, Vec2::ZERO, 1.0, f32::INFINITY)
            };

            let mut land_biome_for_materials = land_host;
            let plains_amp = land_host.settings.land_amp.unwrap_or(PLAINS_AMP);
            let plains_off = land_host.settings.height_offset;
            let mut h_land = sample_plains_height(&plains_n, wxf, wzf, SEA_LEVEL, plains_off, plains_amp);

            if land_host.stand_alone && s_land.is_finite() && s_land < SUB_COAST_LIMIT {
                if let Some((sub_biome, s_sub)) = pick_sub_biome_in_host(biomes, land_host, host_pos, host_r, p_chunks, cfg_seed) {
                    let edge_jit = (map01(sub_edge_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0 * SUB_EDGE_NOISE_AMP;
                    let mut core = sub_core_factor(s_sub + edge_jit);

                    let adj = adjacency_support_factor(biomes, p_chunks, cfg_seed, land_host, &sub_biome.name);
                    core *= adj;

                    if core > 0.0 && (sub_biome.settings.mount_amp.is_some() || sub_biome.settings.mount_freq.is_some()) {
                        let amp  = sub_biome.settings.mount_amp.unwrap_or(32.0);
                        let freq = sub_biome.settings.mount_freq.unwrap_or(0.015);
                        let scale = (freq / MNT_BASE_FREQ).max(0.01);

                        let ux = wxf * scale;
                        let uz = wzf * scale;

                        let delta_c = sample_plains_mountain_delta(&plains_n, ux, uz, amp);

                        let step_w = scale.max(0.001);

                        let d_e  = sample_plains_mountain_delta(&plains_n, ux + step_w, uz,           amp);
                        let d_w  = sample_plains_mountain_delta(&plains_n, ux - step_w, uz,           amp);
                        let d_n  = sample_plains_mountain_delta(&plains_n, ux,           uz + step_w, amp);
                        let d_s  = sample_plains_mountain_delta(&plains_n, ux,           uz - step_w, amp);
                        let d_ne = sample_plains_mountain_delta(&plains_n, ux + step_w,  uz + step_w, amp);
                        let d_nw = sample_plains_mountain_delta(&plains_n, ux - step_w,  uz + step_w, amp);
                        let d_se = sample_plains_mountain_delta(&plains_n, ux + step_w,  uz - step_w, amp);
                        let d_sw = sample_plains_mountain_delta(&plains_n, ux - step_w,  uz - step_w, amp);

                        let neigh_avg8 = (d_e + d_w + d_n + d_s + d_ne + d_nw + d_se + d_sw) * 0.125;
                        let neigh_max8 = d_e.max(d_w).max(d_n.max(d_s)).max(d_ne.max(d_nw).max(d_se.max(d_sw)));

                        let delta_smooth = (delta_c * 2.0 + neigh_avg8) / 3.0;

                        let slope_allow = MNT_WORLD_SLOPE * (0.5 + 0.5 * core);
                        let delta_clamped = delta_smooth
                            .min(neigh_avg8 + slope_allow)
                            .min(neigh_max8 + slope_allow * 0.65);

                        let edge_fade = smoothstep(MNT_DETAIL_EDGE_FADE_START, MNT_DETAIL_EDGE_FADE_END, core);
                        let delta = delta_clamped * edge_fade;

                        let dome = (amp * MNT_DOME_GAIN) * core.powf(MNT_DOME_EXP);
                        h_land += dome + core * delta;
                        land_biome_for_materials = sub_biome;
                    }
                }
            }

            // Ocean biome/materials
            let ocean_biome = if let Some((b, _p, _r, _s)) = best_ocean {
                b
            } else {
                any_ocean_biome(biomes).unwrap_or(land_biome_for_materials)
            };

            // Ocean height
            let h_ocean = {
                let amp = ocean_biome.settings.seafloor_amp.unwrap_or(OCEAN_AMP);
                let off = ocean_biome.settings.height_offset;
                sample_ocean_height(&seafloor_n, wxf, wzf, SEA_LEVEL, off, amp)
            };

            // Coast mask
            let coast_offset = (map01(coast_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0 * COAST_NOISE_AMP_SCORE;
            let t_ocean = smoothstep(
                1.0 - COAST_INSET_SCORE + coast_offset,
                1.0 + COAST_BAND_SCORE  + coast_offset,
                s_land
            );
            let t_land = 1.0 - t_ocean;

            // Final height
            let mut h_f = lerp(h_land, h_ocean, t_ocean)
                .clamp((Y_MIN + 1) as f32, (SEA_LEVEL + 170) as f32);
            if t_ocean > 0.55 { h_f = h_f.min((SEA_LEVEL - 1) as f32); }
            let h_final = h_f.round() as i32;

            // Dominant materials
            let dom_biome = if t_land >= 0.5 { land_biome_for_materials } else { ocean_biome };

            // Resolve block ids
            let top_name        = pick(&dom_biome.surface.top,        wx, wz, pick_seed ^ 0x11);
            let bottom_name     = pick(&dom_biome.surface.bottom,     wx, wz, pick_seed ^ 0x22);
            let sea_floor_name  = pick(&ocean_biome.surface.sea_floor,wx, wz, pick_seed ^ 0x33);
            let upper_zero_name = pick(&dom_biome.surface.upper_zero, wx, wz, pick_seed ^ 0x44);

            let id_top        = reg.id_or_air(top_name);
            let id_bottom     = reg.id_or_air(bottom_name);
            let id_sea_floor  = reg.id_or_air(sea_floor_name);
            let id_upper_zero = reg.id_or_air(upper_zero_name);

            let bw_noise = map01(coast_d.get_noise_2d(wxf, wzf));
            let beach_cap = BEACH_MIN + ((BEACH_MAX - BEACH_MIN) as f32 * bw_noise).round() as i32;

            let soil_cap = 3;

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let underwater = h_final < SEA_LEVEL;

                let id: BlockId = if underwater {
                    id_sea_floor
                } else {
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
    const SEED_SALT_PLAINS: i32 = 0x504C_4149;
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

/* ============= Mountains =================== */

#[inline]
fn sample_plains_mountain_delta(n: &FastNoiseLite, ux: f32, uz: f32, amp_json: f32) -> f32 {
    let b = map01(n.get_noise_2d(ux * 0.60, uz * 0.60));
    let d = map01(n.get_noise_2d(ux * 1.20, uz * 1.20));
    let s = 0.85 * b + 0.15 * d;

    let peak = smoothstep(0.48, 0.86, s).powf(1.05);

    let a_mod = 0.90 + 0.20 * map01(n.get_noise_2d(ux * 0.25, uz * 0.25));

    amp_json * a_mod * peak
}


/* ============================= Sub-biome ============================= */

// 0..1 (center→edge)
#[inline]
fn sub_core_factor(s_norm: f32) -> f32 {
    let s = s_norm.clamp(0.0, 1.4);
    let t = ((s - SUB_CORE_END) / (SUB_CORE_START - SUB_CORE_END)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

#[inline]
fn supports_sub(host: &Biome, sub_name: &str) -> bool {
    if !host.stand_alone { return false; }
    if let Some(list) = &host.subs {
        for s in list { if s.eq_ignore_ascii_case(sub_name) { return true; } }
    }
    false
}

fn pick_sub_biome_in_host<'a>(
    biomes: &'a BiomeRegistry,
    host: &'a Biome,
    host_pos: Vec2,
    host_r: f32,
    p: Vec2,
    world_seed: i32,
) -> Option<(&'a Biome, f32)> {
    let subs = host.subs.as_ref()?;
    if subs.is_empty() { return None; }

    let mut best: Option<(&Biome, f32)> = None;

    for (si, sub_raw) in subs.iter().enumerate() {
        let sub = match get_biome_case_insensitive(biomes, sub_raw) { Some(b) => b, None => continue };
        let (area_min, area_max) = {
            if sub.sizes.is_empty() {
                size_to_area_bounds(&BiomeSize::Small)
            } else {
                let idx = (rand_u32(si as i32, world_seed, SALT_PICK_SIZE) as usize) % sub.sizes.len();
                size_to_area_bounds(&sub.sizes[idx])
            }
        };

        let rr = sub.rarity.clamp(SUB_PRESENT_MIN, SUB_PRESENT_MAX);
        let n_sites = (1.0 + rr * 5.0).round() as i32;

        for k in 0..n_sites.max(1) {
            let s_seed = (world_seed as u32) ^ SALT_SUB_SITES ^ hash32_str(&host.name) ^ hash32_str(&sub.name) ^ (k as u32);
            let t_r = rand01(host_pos.x as i32 + k, host_pos.y as i32 - k, s_seed ^ 0xA1);
            let area_site = area_min + t_r * (area_max - area_min);
            let mut r_site = (area_site / std::f32::consts::PI).sqrt();
            r_site = r_site.min(host_r * 0.75).max(4.0);

            let u = rand01(host_pos.x as i32 - 13 * k, host_pos.y as i32 + 19 * k, s_seed ^ 0xB7);
            let d_edge_bias = u.powf(0.25);
            let max_d = (host_r - r_site).max(1.0);
            let min_d = (0.55 * host_r).min(max_d);
            let d = min_d + (max_d - min_d) * d_edge_bias;

            let ang = (rand01(host_pos.x as i32 + 23 * k, host_pos.y as i32 - 29 * k, s_seed ^ 0xC7) * std::f32::consts::TAU)
                + 0.17 * (k as f32);
            let sub_pos = host_pos + Vec2::new(ang.cos(), ang.sin()) * d;

            let s = p.distance(sub_pos) / r_site.max(1.0);
            if best.map_or(true, |(_, sb)| s < sb) {
                best = Some((sub, s));
            }
        }
    }

    best
}

/* ------- (Adjacency-Guard) -------- */

fn adjacency_support_factor(
    biomes: &BiomeRegistry,
    p_chunks: Vec2,
    world_seed: i32,
    host_biome: &Biome,
    sub_name: &str,
) -> f32 {
    let gx = (p_chunks.x.floor() as i32).div_euclid(BASE_CELL_CHUNKS);
    let gz = (p_chunks.y.floor() as i32).div_euclid(BASE_CELL_CHUNKS);

    let mut best: Option<(f32, bool)> = None;

    for dz in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
        for dx in -SEARCH_RADIUS_CELLS..=SEARCH_RADIUS_CELLS {
            let cx = gx + dx;
            let cz = gz + dz;

            let (site_pos, site_biome, site_radius) =
                site_properties_for_cell(biomes, cx, cz, world_seed);

            if is_ocean_biome(site_biome) { continue; }
            if std::ptr::eq(site_biome, host_biome) { continue; }
            if site_biome.name.eq_ignore_ascii_case(&host_biome.name) { continue; }

            let d = p_chunks.distance(site_pos);
            let score = d / site_radius.max(1.0);
            let neighbor_supports = supports_sub(site_biome, sub_name);

            if best.map_or(true, |(s, _)| score < s) {
                best = Some((score, neighbor_supports));
            }
        }
    }

    if let Some((s, ok)) = best {
        if ok { 1.0 } else { smoothstep(FOREIGN_GUARD_START, FOREIGN_GUARD_END, s) }
    } else {
        1.0
    }
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

    let biome = rarity_pick_site(biomes, r).expect("No biomes registered");

    let (area_min, area_max) = size_to_area_bounds(
        if biome.sizes.is_empty() { &BiomeSize::Medium } else {
            &biome.sizes[(rand_u32(cell_x, cell_z, (world_seed as u32) ^ SALT_PICK_SIZE) as usize) % biome.sizes.len()]
        }
    );

    let t = rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xFACE_FEED));
    let target_area_chunks = area_min + t * (area_max - area_min);

    let mut radius_chunks = (target_area_chunks / std::f32::consts::PI).sqrt();
    let jitter = 0.95 + 0.10 * rand01(cell_x, cell_z, (world_seed as u32).wrapping_add(0xDEAD_BEEF));
    radius_chunks *= jitter;
    let min_r = (area_min / std::f32::consts::PI).sqrt();
    radius_chunks = radius_chunks.max(min_r * 0.98);

    (pos, biome, radius_chunks.max(1.0))
}

fn rarity_pick_site(biomes: &BiomeRegistry, r01: f64) -> Option<&Biome> {
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

fn size_to_area_bounds(size: &BiomeSize) -> (f32, f32) {
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

fn any_ocean_biome(biomes: &BiomeRegistry) -> Option<&Biome> {
    for b in biomes.by_name.values() {
        if is_ocean_biome(b) { return Some(b); }
    }
    None
}

// Case-insensitive Lookup
#[inline]
fn get_biome_case_insensitive<'a>(biomes: &'a BiomeRegistry, name: &str) -> Option<&'a Biome> {
    if let Some(b) = biomes.by_name.get(name) { return Some(b); }
    for b in biomes.by_name.values() {
        if b.name.eq_ignore_ascii_case(name) { return Some(b); }
    }
    None
}

// FNV-1a 32-bit
#[inline]
fn hash32_str(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for &b in s.as_bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}
