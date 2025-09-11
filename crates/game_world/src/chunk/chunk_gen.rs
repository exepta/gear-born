use crate::chunk::chunk_utils::map01;
use bevy::prelude::*;
use fastnoise_lite::*;
use game_core::world::biome::biome_func::*;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::biome::Biome;
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::{ChunkData, SEA_LEVEL};
use game_core::world::chunk_dim::{CX, CY, CZ, Y_MIN};
use crate::chunk::river_utils::RiverSystem;
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

    // --- Rivers: init once per chunk -------------------------------------
    // Deterministic river system, fed by world seed.
    let river = RiverSystem::new(cfg_seed);

    // Chunk world origin in block coordinates
    let chunk_origin_wx = coord.x * CX as i32;
    let chunk_origin_wz = coord.y * CZ as i32;

    // Use a stable probe width to detect whether a river is entering from neighbors.
    // This is only used for "continuation" when a biome has rivers=true but chance==0.0.
    let probe_width = 12;
    let neighbor_has_river = river.neighbor_continuation_for_chunk(
        chunk_origin_wx,
        chunk_origin_wz,
        CX, CZ,
        probe_width,
    );

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
            let (_, best_ocean) = best_land_and_ocean_sites(biomes, p_chunks, cfg_seed);

            // Second-best distinct land site (neighbor)
            let (land0, pos0, r0, s0, land1_opt, pos1, r1, s1) =
                best_two_land_sites(biomes, p_chunks, cfg_seed, fallback_label);

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

            {
                // 1) Local river permission from the two land sites (soft biome fade).
                let allow0 = if land0.generation.rivers { w0 } else { 0.0 };
                let allow1 = if let Some(land1) = land1_opt {
                    if land1.generation.rivers { w1 } else { 0.0 }
                } else { 0.0 };
                let perm_here = ((allow0 + allow1) / w_sum).clamp(0.0, 1.0);

                let generate = &land_biome_for_materials.generation;

                if generate.rivers && perm_here > 0.02 {
                    // Chance gate (your original rule)
                    let keep   = river.tile_keep_value(wx, wz);
                    let chance = generate.river_chance.max(0.0).min(1.0);
                    let allow_spawn_here = keep < chance;
                    let may_carve        = allow_spawn_here || (chance == 0.0 && neighbor_has_river);

                    // Width is stable per tile
                    let width_blocks = river.tile_width_blocks(wx, wz, generate.river_size_between);

                    // Core potential + flow direction (unit tangent)
                    let (p0_raw, t_dir) = river.potential_with_dir(wxf, wzf, width_blocks);

                    // Lightweight neighbor smoothing (does not inflate width)
                    let pn = river.potential(wxf,       wzf + 1.0, width_blocks);
                    let ps = river.potential(wxf,       wzf - 1.0, width_blocks);
                    let pe = river.potential(wxf + 1.0, wzf,       width_blocks);
                    let pw = river.potential(wxf - 1.0, wzf,       width_blocks);
                    let p_core = 0.75 * p0_raw + 0.25 * (pn + ps + pe + pw) * 0.25;

                    // Permission softens depth inside the allowed biome
                    let width_fac = (width_blocks as f32 / 16.0).clamp(0.75, 2.0);
                    let mut p_eff = p_core * perm_here.powf(1.15 + 0.60 * width_fac);

                    // --- Border smoothing (THE IMPORTANT PART) -------------------------
                    // If the river is about to enter a blocking area (rivers:false),
                    // reduce depth *ahead* of us so we don't end with a vertical wall.
                    // We sample the biome permission a short distance along the course.
                    let mut border_fade = 1.0;
                    if t_dir.length_squared() > 0.0 {
                        // Convert arbitrary world (x,z) -> the "p_chunks" space used by site queries.
                        // NOTE: In this world the mapping equals (wx + 0.5) / CX etc. (see your code).
                        let perm_at_world = |x: f32, z: f32| -> f32 {
                            let px_q = (x + 0.5) / (CX as f32);
                            let pz_q = (z + 0.5) / (CZ as f32);
                            let p_chunks_q = Vec2::new(px_q, pz_q);

                            let (l0q, _p0, _r0, s0q, l1q_opt, _p1, _r1, s1q) =
                                best_two_land_sites(biomes, p_chunks_q, cfg_seed, fallback_label);

                            let w0q = land_weight_from_score(s0q);
                            let w1q = land_weight_from_score(s1q);
                            let w_sum_q = (w0q + w1q).max(1e-6);

                            let a0 = if l0q.generation.rivers { w0q } else { 0.0 };
                            let a1 = if let Some(l1q) = l1q_opt {
                                if l1q.generation.rivers { w1q } else { 0.0 }
                            } else { 0.0 };

                            ((a0 + a1) / w_sum_q).clamp(0.0, 1.0)
                        };

                        // Look ahead one and two half-widths.
                        let look = (width_blocks as f32 * 1.2).clamp(6.0, 18.0);
                        let ax1 = wxf + t_dir.x * (0.5 * look);
                        let az1 = wzf + t_dir.y * (0.5 * look);
                        let ax2 = wxf + t_dir.x * (1.0 * look);
                        let az2 = wzf + t_dir.y * (1.0 * look);

                        let perm_ahead = 0.6 * perm_at_world(ax1, az1) + 0.4 * perm_at_world(ax2, az2);

                        // If permission drops ahead, fade our carving now.
                        let drop = (perm_here - perm_ahead).clamp(0.0, 1.0);
                        // Smooth drop -> 1..0 factor
                        border_fade = 1.0 - smoothstep(0.10, 0.90, drop);
                    }

                    // Apply forward fade. Also apply a cheap tail when carving stops completely.
                    if may_carve {
                        p_eff *= border_fade.powf(1.35);
                    } else if p_core > 0.0 && t_dir.length_squared() > 0.0 {
                        // Short upstream tail (prevents a hard end even if this column is disallowed)
                        let seg = (width_blocks as f32 * 0.9).clamp(6.0, 14.0);
                        let bx = wxf - t_dir.x * seg;
                        let bz = wzf - t_dir.y * seg;
                        let p_tail = river.potential(bx, bz, width_blocks);
                        p_eff = (0.85 * p_tail).min(p_core); // gentle, never deeper than local profile
                    }
                    // -------------------------------------------------------------------

                    if p_eff > 0.0 {
                        let h_before = h_f;
                        let mut h_carved = river
                            .carve_height(h_before, p_eff, width_blocks, SEA_LEVEL as f32)
                            .clamp((Y_MIN + 1) as f32, (SEA_LEVEL + 170) as f32);

                        // Extra safety: limit the last cut near a strong drop ahead (round the lip)
                        if border_fade < 0.95 {
                            let severity = (1.0 - border_fade).clamp(0.0, 1.0);
                            let max_cut_far = 2.6 + 0.34 * (width_blocks as f32); // ≈2.6..7.0
                            let max_cut = (1.0 - smoothstep(0.10, 0.80, severity)) * max_cut_far;
                            let cut = (h_before - h_carved).max(0.0);
                            if cut > max_cut { h_carved = h_before - max_cut; }
                            h_carved = h_carved + (h_before - h_carved) * (0.25 * severity);
                        }

                        h_f = h_carved;
                    }
                }
            }

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
