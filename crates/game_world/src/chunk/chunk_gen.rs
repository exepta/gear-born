use crate::chunk::chunk_utils::{col_rand_u32, map01};
use bevy::prelude::*;
use fastnoise_lite::*;
use game_core::world::biome::biome_func::*;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::biome::{Biome, BiomeSize};
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::{ChunkData, SEA_LEVEL};
use game_core::world::chunk_dim::{CX, CY, CZ, Y_MIN};


/* ========================= Generator =================================== */

pub(crate) async fn generate_chunk_async_biome(
    coord: IVec2,
    reg: &BlockRegistry,
    cfg_seed: i32,
    biomes: &BiomeRegistry,
) -> ChunkData {
    let fallback_label = choose_biome_label_smoothed(biomes, coord, cfg_seed);

    // Per-chunk noises
    let seafloor_n = make_seafloor_noise(cfg_seed, OCEAN_FREQ);
    let plains_n   = make_plains_noise(cfg_seed,   PLAINS_FREQ);
    let coast_n    = make_coast_noise(cfg_seed ^ SALT_COAST,  COAST_NOISE_FREQ);
    let coast_d    = make_coast_noise(cfg_seed ^ SALT_COAST2, COAST_DETAIL_FREQ);
    let sub_edge_n = make_coast_noise(cfg_seed ^ SALT_SUB_EDGE, SUB_EDGE_NOISE_FREQ);

    let pick_seed: u32 = (cfg_seed as u32) ^ 0x0CE4_11CE;
    let mut chunk = ChunkData::new();

    // Small helper: compute total land height (base and optional mountains) for one land site
    // and tell which biome should provide surface materials for this column if that site dominates.
    #[inline]
    fn site_total_height<'a>(
        plains_n: &FastNoiseLite,
        sub_edge_n: &FastNoiseLite,
        biomes: &'a BiomeRegistry,
        // site definition
        site_biome: &'a Biome,
        site_pos: Vec2,
        site_r: f32,
        // world position
        wxf: f32,
        wzf: f32,
        p_chunks: Vec2,
        // mixing with other sites
        w_site: f32,
        w_sum: f32,
        // world seed
        cfg_seed: i32,
    ) -> (f32, &'a Biome, i32) {
        // --- base plains height from this site's own settings
        let base_off = site_biome.settings.height_offset;
        let base_amp = site_biome.settings.land_amp.unwrap_or(PLAINS_AMP);
        let mut h = sample_plains_height(plains_n, wxf, wzf, SEA_LEVEL, base_off, base_amp);

        // default materials/soil
        let mut mat_biome = site_biome;
        let mut soil_cap_local = 3;

        // --- optional mountains from this site (if it has a matching sub-biome active here)
        // convert the per-site "distance score" for this column
        let s_site = p_chunks.distance(site_pos) / site_r.max(1.0);

        if site_biome.stand_alone && s_site.is_finite() && s_site < SUB_COAST_LIMIT {
            if let Some((sub_b, s_sub)) =
                pick_sub_biome_in_host(biomes, site_biome, site_pos, site_r, p_chunks, cfg_seed)
            {
                // core weight with a little edge noise
                let edge_jit = (map01(sub_edge_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0 * SUB_EDGE_NOISE_AMP;
                let mut core = sub_core_factor(s_sub + edge_jit);

                // sub-biome adjacency guard
                let adj = adjacency_support_factor(biomes, p_chunks, cfg_seed, site_biome, &sub_b.name);
                core *= adj;

                // fade mountains as this site loses influence against others
                let site_influence = (w_site / w_sum).clamp(0.0, 1.0);
                core *= site_influence;

                if core > 0.0 && (sub_b.settings.mount_amp.is_some() || sub_b.settings.mount_freq.is_some()) {
                    let amp  = sub_b.settings.mount_amp.unwrap_or(32.0);
                    let freq = sub_b.settings.mount_freq.unwrap_or(0.015);
                    let scale = (freq / MNT_BASE_FREQ).max(0.01);

                    let ux = wxf * scale;
                    let uz = wzf * scale;

                    // mountain delta from plain noise
                    let delta_c = sample_plains_mountain_delta(plains_n, ux, uz, amp);

                    // 8-neighborhood sampling for smoothing & slope limiting
                    let step_w = scale.max(0.001);
                    let d_e  = sample_plains_mountain_delta(plains_n, ux + step_w, uz,           amp);
                    let d_w  = sample_plains_mountain_delta(plains_n, ux - step_w, uz,           amp);
                    let d_n  = sample_plains_mountain_delta(plains_n, ux,           uz + step_w, amp);
                    let d_s  = sample_plains_mountain_delta(plains_n, ux,           uz - step_w, amp);
                    let d_ne = sample_plains_mountain_delta(plains_n, ux + step_w,  uz + step_w, amp);
                    let d_nw = sample_plains_mountain_delta(plains_n, ux - step_w,  uz + step_w, amp);
                    let d_se = sample_plains_mountain_delta(plains_n, ux + step_w,  uz - step_w, amp);
                    let d_sw = sample_plains_mountain_delta(plains_n, ux - step_w,  uz - step_w, amp);

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
                    h += dome + core * delta;

                    // surface materials taken from the sub-biome when the site dominates locally
                    if site_influence > 0.5 { mat_biome = sub_b; }

                    // allow caller to use thicker soil on steeper spots
                    soil_cap_local = slope_to_soil_cap(core, delta_c, [d_e, d_w, d_n, d_s, d_ne, d_nw, d_se, d_sw]);
                }
            }
        }

        (h, mat_biome, soil_cap_local)
    }

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx  = coord.x * CX as i32 + lx as i32;
            let wz  = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            // Column position in "chunk units" with sub-chunk precision
            let px = coord.x as f32 + (lx as f32 + 0.5) / (CX as f32);
            let pz = coord.y as f32 + (lz as f32 + 0.5) / (CZ as f32);
            let p_chunks = Vec2::new(px, pz);

            // Nearest land and ocean sites (host = best land)
            let (best_land, best_ocean) = best_land_and_ocean_sites(biomes, p_chunks, cfg_seed);

            // Host land biome (fallback if none was found)
            let (land0, pos0, r0, s0) = if let Some((b, pos, r, s)) = best_land {
                (b, pos, r, s)
            } else {
                (fallback_label, Vec2::ZERO, 1.0, f32::INFINITY)
            };

            // Second-best distinct land site (neighbor)
            let (land1_opt, pos1, r1, s1) = if let Some((b2, p2, rr2, ss2)) =
                best_second_land_site(biomes, p_chunks, cfg_seed, pos0)
            {
                (Some(b2), p2, rr2, ss2)
            } else {
                (None, Vec2::ZERO, 1.0, f32::INFINITY)
            };

            // Inverse-square weights (robust 0-division guard)
            let w0 = land_weight_from_score(s0);
            let w1 = land_weight_from_score(s1);
            let w_sum = (w0 + w1).max(1e-6);

            // --- total height for site 0 (host)
            let (h0_total, mats0, soil0) =
                site_total_height(&plains_n, &sub_edge_n, biomes, land0, pos0, r0, wxf, wzf, p_chunks, w0, w_sum, cfg_seed);

            // --- total height for site 1 (neighbor) – if none, reuse site 0 so blend is identity
            let (h1_total, mats1, soil1) = if let Some(land1) = land1_opt {
                site_total_height(&plains_n, &sub_edge_n, biomes, land1, pos1, r1, wxf, wzf, p_chunks, w1, w_sum, cfg_seed)
            } else {
                (h0_total, land0, soil0)
            };

            // Final land height = distance-weighted blend of both *total* heights.
            // This is the key change that removes the cliff: the mountain contribution
            // from the host is blended out continuously as the neighbor site takes over.
            let h_land = (h0_total * w0 + h1_total * w1) / w_sum;

            // Choose materials from whichever land site dominates locally
            let land_biome_for_materials = if w0 >= w1 { mats0 } else { mats1 };

            // Ocean biome/materials (fallback to any ocean or current land materials)
            let ocean_biome = if let Some((b, _p, _r, _s)) = best_ocean {
                b
            } else {
                any_ocean_biome(biomes).unwrap_or(land_biome_for_materials)
            };

            // Ocean height from ocean biome settings
            let h_ocean = {
                let amp = ocean_biome.settings.seafloor_amp.unwrap_or(OCEAN_AMP);
                let off = ocean_biome.settings.height_offset;
                sample_ocean_height(&seafloor_n, wxf, wzf, SEA_LEVEL, off, amp)
            };

            // Coast mask: only fade to ocean if outside *all* land regions
            let s_for_coast = s0.min(s1);
            let coast_offset = (map01(coast_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0 * COAST_NOISE_AMP_SCORE;
            let t_ocean = smoothstep(
                1.0 - COAST_INSET_SCORE + coast_offset,
                1.0 + COAST_BAND_SCORE  + coast_offset,
                s_for_coast
            );
            let t_land = 1.0 - t_ocean;

            // Final height with optional sea floor clamp near open ocean
            let mut h_f = lerp(h_land, h_ocean, t_ocean)
                .clamp((Y_MIN + 1) as f32, (SEA_LEVEL + 170) as f32);
            if t_ocean > 0.55 { h_f = h_f.min((SEA_LEVEL - 1) as f32); }
            let h_final = h_f.round() as i32;

            // Dominant materials from land vs. ocean
            let dom_biome = if t_land >= 0.5 { land_biome_for_materials } else { ocean_biome };

            // Resolve block ids for surface/strata
            let top_name        = pick(&dom_biome.surface.top,        wx, wz, pick_seed ^ 0x11);
            let bottom_name     = pick(&dom_biome.surface.bottom,     wx, wz, pick_seed ^ 0x22);
            let sea_floor_name  = pick(&ocean_biome.surface.sea_floor,wx, wz, pick_seed ^ 0x33);
            let upper_zero_name = pick(&dom_biome.surface.upper_zero, wx, wz, pick_seed ^ 0x44);

            let id_top        = reg.id_or_air(top_name);
            let id_bottom     = reg.id_or_air(bottom_name);
            let id_sea_floor  = reg.id_or_air(sea_floor_name);
            let id_upper_zero = reg.id_or_air(upper_zero_name);

            // Beach cap width with detail noise
            let bw_noise = map01(coast_d.get_noise_2d(wxf, wzf));
            let beach_cap = BEACH_MIN + ((BEACH_MAX - BEACH_MIN) as f32 * bw_noise).round() as i32;

            // Adaptive soil from the dominating land site
            let soil_cap = if w0 >= w1 { soil0 } else { soil1 };

            // Write column blocks
            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let underwater = h_final < SEA_LEVEL;

                let id: BlockId = if underwater {
                    // Sand-only underwater
                    id_sea_floor
                } else {
                    // Sand cap around coasts for beaches
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
    // Slightly slower noise to make a sea floor undulate more gently
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
    // Land uses FBm around the biome's base offset
    let hn = map01(n.get_noise_2d(wxf, wzf));
    let undulation = (hn - 0.5) * land_amp;
    let base = sea_level as f32 + height_offset;
    clamp_world_y(base + undulation)
}

/* ============= Mountains (plains-based delta composer) =================== */

#[inline]
fn sample_plains_mountain_delta(n: &FastNoiseLite, ux: f32, uz: f32, amp_json: f32) -> f32 {
    // Two bands at different scales mixed towards broader shapes
    let b = map01(n.get_noise_2d(ux * 0.60, uz * 0.60));
    let d = map01(n.get_noise_2d(ux * 1.20, uz * 1.20));
    let s = 0.85 * b + 0.15 * d;

    // Emphasize peaks while keeping foothills shallow
    let peak = smoothstep(0.48, 0.86, s).powf(1.05);

    // Slight amplitude modulation to avoid uniformity
    let a_mod = 0.90 + 0.20 * map01(n.get_noise_2d(ux * 0.25, uz * 0.25));

    amp_json * a_mod * peak
}

/* ============================= Sub-biome ==================================== */

// Convert a distance-normalized score (center→edge) into a smooth core weight (0..1)
#[inline]
fn sub_core_factor(s_norm: f32) -> f32 {
    let s = s_norm.clamp(0.0, 1.4);
    let t = ((s - SUB_CORE_END) / (SUB_CORE_START - SUB_CORE_END)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/* ----- Helper: inverse-square weight from site score (robust) ----- */
#[inline]
fn land_weight_from_score(s: f32) -> f32 {
    // Lower scores mean "closer to center" -> higher weight
    let sc = s.max(0.001);
    1.0 / (sc * sc)
}

#[inline]
fn supports_sub(host: &Biome, sub_name: &str) -> bool {
    if !host.stand_alone { return false; }
    if let Some(list) = &host.subs {
        for s in list { if s.eq_ignore_ascii_case(sub_name) { return true; } }
    }
    false
}

// Pick the most suitable sub-biome of `host` near `p` (dynamic, no fixed names)
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
        // Determine a subarea from its size list (default Small)
        let (area_min, area_max) = {
            if sub.sizes.is_empty() {
                size_to_area_bounds(&BiomeSize::Small)
            } else {
                let idx = (rand_u32(si as i32, world_seed, SALT_PICK_SIZE) as usize) % sub.sizes.len();
                size_to_area_bounds(&sub.sizes[idx])
            }
        };

        // The number of subsites scales softly with rarity
        let rr = sub.rarity.clamp(SUB_PRESENT_MIN, SUB_PRESENT_MAX);
        let n_sites = (1.0 + rr * 5.0).round() as i32;

        for k in 0..n_sites.max(1) {
            let s_seed = (world_seed as u32) ^ SALT_SUB_SITES ^ hash32_str(&host.name) ^ hash32_str(&sub.name) ^ (k as u32);
            // Draw area in [min.max]
            let t_r = rand01(host_pos.x as i32 + k, host_pos.y as i32 - k, s_seed ^ 0xA1);
            let area_site = area_min + t_r * (area_max - area_min);
            let mut r_site = (area_site / std::f32::consts::PI).sqrt();
            r_site = r_site.min(host_r * 0.75).max(4.0);

            // Bias placement toward mid/outer ring to reduce overlap with a host center
            let u = rand01(host_pos.x as i32 - 13 * k, host_pos.y as i32 + 19 * k, s_seed ^ 0xB7);
            let d_edge_bias = u.powf(0.25);
            let max_d = (host_r - r_site).max(1.0);
            let min_d = (0.55 * host_r).min(max_d);
            let d = min_d + (max_d - min_d) * d_edge_bias;

            // Random angle with slight rotation per site index
            let ang = (rand01(host_pos.x as i32 + 23 * k, host_pos.y as i32 - 29 * k, s_seed ^ 0xC7) * std::f32::consts::TAU)
                + 0.17 * (k as f32);
            let sub_pos = host_pos + Vec2::new(ang.cos(), ang.sin()) * d;

            // Score = normalized distance to the sub site
            let s = p.distance(sub_pos) / r_site.max(1.0);
            if best.map_or(true, |(_, sb)| s < sb) {
                best = Some((sub, s));
            }
        }
    }

    best
}

/* ------- Adjacency guard: allow sub-biome only next to supporting hosts ------- */

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

            // Keep the closest different land site, defaulting to true on None.
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

// Case-insensitive lookup for dynamic sub-biome references
#[inline]
fn get_biome_case_insensitive<'a>(biomes: &'a BiomeRegistry, name: &str) -> Option<&'a Biome> {
    if let Some(b) = biomes.by_name.get(name) { return Some(b); }
    for b in biomes.by_name.values() {
        if b.name.eq_ignore_ascii_case(name) { return Some(b); }
    }
    None
}

// FNV-1a 32-bit string hash (used only for deterministic seeding)
#[inline]
fn hash32_str(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for &b in s.as_bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}
