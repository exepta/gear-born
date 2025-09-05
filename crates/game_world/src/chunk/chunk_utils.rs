use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, LoadingStates};
use game_core::world::biome::{BiomeEdgeBlend, BiomeTerrainParams};
use game_core::world::block::{BlockId, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex, SEA_LEVEL};
use game_core::world::chunk_dim::*;
use game_core::world::save::*;
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::HashMap;
use std::path::PathBuf;

pub const MAX_INFLIGHT_MESH: usize = 32;
pub const MAX_INFLIGHT_GEN:  usize = 32;

pub(crate) const DIR4: [IVec2; 4] = [
    IVec2::new( 1,  0),
    IVec2::new(-1,  0),
    IVec2::new( 0,  1),
    IVec2::new( 0, -1),
];

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId, BlockId, BlockId), // (top, bottom, stone, seafloor, beach)
    blend: BiomeEdgeBlend,                               // world-space border blending
    b_params: BiomeTerrainParams,                        // per-biome controls
    cfg: WorldGenConfig,
) -> ChunkData {
    use std::f32::consts::TAU;

    /* -------------------- Constants -------------------- */

    // World height clamps
    const SEA_FLOOR_MIN: i32 = -5;
    const MOUNTAIN_MAX: i32 = 192;

    // Beaches / coast
    const BEACH_TOP_BAND: i32 = 1;
    const BEACH_SUB_BAND: i32 = 2;
    const COAST_WIDTH: f32 = 6.0;
    const COAST_RING_R: f32 = 6.0;
    const COAST_RING_SAMPLES: usize = 12;
    const BEACH_MAX_SLOPE: i32 = 2;

    // ---- River controls ----
    // Frequency of the "river field" (lower => further apart rivers)
    const RIVER_FREQ: f32       = 0.0012;
    // Large-scale regions where rivers are allowed (rarity mask)
    const RIVER_MASK_FREQ: f32  = 0.00075;
    const RIVER_RARITY: f32     = 0.62; // higher => rarer rivers
    // Target width (in blocks). We'll shape the core by an estimated distance field.
    const RIVER_WIDTH_MIN: f32  = 16.0;
    const RIVER_WIDTH_MAX: f32  = 24.0;
    const RIVER_DEPTH: f32        = 3.0;
    const RIVER_BANK_SOFTNESS: f32 = 1.35;
    const RIVER_BANK_EXP: f32      = 1.6;
    // Meandering: multi-scale domain warp
    const RIVER_WARP_L_FREQ: f32 = 0.003;  // large, slow bends
    const RIVER_WARP_L_AMP:  f32 = 65.0;
    const RIVER_WARP_M_FREQ: f32 = 0.010;  // medium wiggles
    const RIVER_WARP_M_AMP:  f32 = 22.0;
    const RIVER_WARP_S_FREQ: f32 = 0.015;  // fine detail (legacy)
    const RIVER_WARP_S_AMP:  f32 = 14.0;
    // Distance estimation: gradient by finite differences (in world units)
    const RIVER_GRAD_EPS: f32 = 3.0;

    // Per-biome toggles
    let enable_rivers: bool = b_params.rivers;
    let enable_coast: bool  = b_params.coast;

    let (block_top, block_bottom, block_stone, block_seafloor, block_beach) = ids;
    let mut out = ChunkData::new();

    /* -------------------- Noises -------------------- */

    // Scale main height frequency by biome.mountain_freq
    let base_freq = (cfg.height_freq * b_params.mountain_freq.max(0.01)).max(0.0001);
    let roll_freq = (cfg.height_freq * 2.1 * b_params.mountain_freq.max(0.01)).max(0.0001);

    let mut base_n = FastNoiseLite::with_seed(cfg.seed);
    base_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    base_n.set_frequency(Some(base_freq));
    base_n.set_fractal_type(Some(FractalType::FBm));
    base_n.set_fractal_octaves(Some(5));
    base_n.set_fractal_gain(Some(0.5));
    base_n.set_fractal_lacunarity(Some(2.0));

    let mut roll_n = FastNoiseLite::with_seed(cfg.seed ^ 0xA1A1_5151u32 as i32);
    roll_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    roll_n.set_frequency(Some(roll_freq));
    roll_n.set_fractal_type(Some(FractalType::FBm));
    roll_n.set_fractal_octaves(Some(3));
    roll_n.set_fractal_gain(Some(0.55));
    roll_n.set_fractal_lacunarity(Some(2.0));

    let mut warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00DE_AD11);
    warp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    warp_n.set_frequency(Some(cfg.warp_freq.max(0.0001)));
    warp_n.set_fractal_type(Some(FractalType::FBm));
    warp_n.set_fractal_octaves(Some(3));
    warp_n.set_fractal_gain(Some(0.5));
    warp_n.set_fractal_lacunarity(Some(2.0));

    let mut plains_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00A1_00A1);
    plains_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    plains_n.set_frequency(Some(cfg.plains_freq.max(0.0001)));

    let mut coast_n = FastNoiseLite::with_seed(cfg.seed ^ 0x000C_0457);
    coast_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    coast_n.set_frequency(Some(0.010));

    // Rivers: field, warps, width variation, rarity mask
    let mut river_n = FastNoiseLite::with_seed(cfg.seed ^ 0x1357_9BDF);
    river_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_n.set_frequency(Some(RIVER_FREQ));

    let mut river_warp_l = FastNoiseLite::with_seed(cfg.seed ^ 0xDEAD_BEEFu32 as i32);
    river_warp_l.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_warp_l.set_frequency(Some(RIVER_WARP_L_FREQ));

    let mut river_warp_m = FastNoiseLite::with_seed(cfg.seed ^ 0xACAC_ACACu32 as i32);
    river_warp_m.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_warp_m.set_frequency(Some(RIVER_WARP_M_FREQ));

    let mut river_warp_s = FastNoiseLite::with_seed(cfg.seed ^ 0x2468_ACF1);
    river_warp_s.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_warp_s.set_frequency(Some(RIVER_WARP_S_FREQ));

    let mut river_width_n = FastNoiseLite::with_seed(cfg.seed ^ 0x4433_2211);
    river_width_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_width_n.set_frequency(Some(0.0011)); // slow width changes

    let mut river_mask_n = FastNoiseLite::with_seed(cfg.seed ^ 0x9C37_AA11u32 as i32);
    river_mask_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_mask_n.set_frequency(Some(RIVER_MASK_FREQ));

    // Coherent mask for dithering in the seam band
    let mut mix_mask_n = FastNoiseLite::with_seed(cfg.seed ^ 0xB1D3_B10Du32 as i32);
    mix_mask_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    mix_mask_n.set_frequency(Some(0.045));
    mix_mask_n.set_fractal_type(Some(FractalType::FBm));
    mix_mask_n.set_fractal_octaves(Some(2));
    mix_mask_n.set_fractal_gain(Some(0.5));
    mix_mask_n.set_fractal_lacunarity(Some(2.0));

    // Seam curving noises
    let mut seam_large  = FastNoiseLite::with_seed(cfg.seed ^ 0x7A61_5EAF);
    seam_large.set_noise_type(Some(NoiseType::OpenSimplex2));
    seam_large.set_frequency(Some(0.019));

    let mut seam_detail = FastNoiseLite::with_seed(cfg.seed ^ 0x51DE_77A1);
    seam_detail.set_noise_type(Some(NoiseType::OpenSimplex2));
    seam_detail.set_frequency(Some(0.065));
    seam_detail.set_fractal_type(Some(FractalType::FBm));
    seam_detail.set_fractal_octaves(Some(2));
    seam_detail.set_fractal_gain(Some(0.5));
    seam_detail.set_fractal_lacunarity(Some(2.0));

    // Tiny tie-break jitter
    let mut tie_jitter = FastNoiseLite::with_seed(cfg.seed ^ 0x00C1_E55E);
    tie_jitter.set_noise_type(Some(NoiseType::Perlin));
    tie_jitter.set_frequency(Some(0.11));

    /* -------------------- Helpers -------------------- */

    #[inline] fn map01(x: f32) -> f32 { (x * 0.5) + 0.5 }
    #[inline]
    fn smoothstep(a: f32, b: f32, x: f32) -> f32 {
        let t = ((x - a) / (b - a)).clamp(0.0, 1.0);
        t * t * (3.0 - 2.0 * t)
    }
    #[inline] fn lerp(a: f32, b: f32, t: f32) -> f32 { a + (b - a) * t }

    // Base terrain WITHOUT river lowering; returns also a "river core" 0..1 by distance.
    let height_no_river_and_core = |xf: f32, zf: f32| -> (f32 /*h_base*/, f32 /*river_core*/) {
        // ----- base terrain -----
        let p_raw = map01(plains_n.get_noise_2d(xf, zf));
        let plains_mask = smoothstep(
            (cfg.plains_threshold - cfg.plains_blend).clamp(0.0, 1.0),
            (cfg.plains_threshold + cfg.plains_blend).clamp(0.0, 1.0),
            p_raw,
        );
        let warp_amp = lerp(cfg.warp_amp_plains, cfg.warp_amp, 1.0 - plains_mask);
        let dx = warp_n.get_noise_2d(xf, zf) * warp_amp;
        let dz = warp_n.get_noise_2d(xf + 1000.0, zf - 1000.0) * warp_amp;

        let h01  = map01(base_n.get_noise_2d(xf + dx, zf + dz));
        let roll = map01(roll_n.get_noise_2d(xf + dx * 0.35, zf + dz * 0.35));

        let local_span = lerp(cfg.plains_span as f32, cfg.height_span as f32, 1.0 - plains_mask);
        let mid = SEA_LEVEL as f32 + cfg.base_height;

        let mut h = mid + (h01 - 0.5) * local_span;
        h += (roll - 0.5) * 3.0;

        if cfg.plains_flatten > 0.0 {
            h = lerp(mid, h, 1.0 - plains_mask * cfg.plains_flatten);
        }

        // peak-only lift for mountain biomes
        let mountain_mask = 1.0 - plains_mask;
        let peak = smoothstep(0.55, 0.95, h01);
        h += b_params.height_offset * mountain_mask * peak;

        // coast shaping
        if enable_coast {
            let dist = h - SEA_LEVEL as f32;
            let ramp = 1.0 - smoothstep(0.0, COAST_WIDTH, dist.abs());
            let compress = lerp(0.85, 1.0, 1.0 - ramp);
            h = SEA_LEVEL as f32 + dist * compress;

            let dunes = coast_n.get_noise_2d(xf * 1.7, zf * 1.7);
            h += dunes * 0.15 * (1.0 - smoothstep(0.0, COAST_WIDTH * 0.7, dist.abs()));
        }

        // ----- meandering river field (distance-based core) -----
        // multi-scale domain warp for snakier paths
        let mut rx = xf
            + river_warp_l.get_noise_2d(xf * 0.6, zf * 0.6) * RIVER_WARP_L_AMP
            + river_warp_m.get_noise_2d(xf * 1.2 + 1000.0, zf * 1.2 - 1000.0) * RIVER_WARP_M_AMP;
        let mut rz = zf
            + river_warp_l.get_noise_2d(xf * 0.6 + 123.0, zf * 0.6 - 123.0) * RIVER_WARP_L_AMP
            + river_warp_m.get_noise_2d(xf * 1.2, zf * 1.2) * RIVER_WARP_M_AMP;

        // small-scale extra wiggle, evaluated in already-warped space
        rx += river_warp_s.get_noise_2d(rx * 1.0, rz * 1.0) * RIVER_WARP_S_AMP;
        rz += river_warp_s.get_noise_2d(rx * 1.0 + 1000.0, rz * 1.0 - 1000.0) * RIVER_WARP_S_AMP;

        // river "ridge" value and gradient (finite differences)
        let n = river_n.get_noise_2d(rx, rz);
        let r = n.abs();

        let nx1 = river_n.get_noise_2d(rx + RIVER_GRAD_EPS, rz);
        let nx0 = river_n.get_noise_2d(rx - RIVER_GRAD_EPS, rz);
        let nz1 = river_n.get_noise_2d(rx, rz + RIVER_GRAD_EPS);
        let nz0 = river_n.get_noise_2d(rx, rz - RIVER_GRAD_EPS);
        let gx = (nx1 - nx0) / (2.0 * RIVER_GRAD_EPS);
        let gz = (nz1 - nz0) / (2.0 * RIVER_GRAD_EPS);
        let grad = (gx * gx + gz * gz).sqrt().max(1e-5);

        // approximate physical distance (in world blocks) to the centerline
        let dist_to_axis = r / grad;

        // width variation in blocks (follow the warped river space so it changes along the path)
        let w_noise = map01(river_width_n.get_noise_2d(rx * 0.004, rz * 0.004));
        let width_blocks = lerp(RIVER_WIDTH_MIN, RIVER_WIDTH_MAX, w_noise);
        let half = width_blocks * 0.5;

        let t = (dist_to_axis / (half * RIVER_BANK_SOFTNESS)).clamp(0.0, 1.0);

        let mut bank_profile = 1.0 - (t * t * (3.0 - 2.0 * t));

        // exponent softens the banks even more
        bank_profile = bank_profile.powf(RIVER_BANK_EXP);

        // Large-scale mask: turn whole river networks on/off (rarity)
        let m = map01(river_mask_n.get_noise_2d(xf * 0.6, zf * 0.6));
        let mask_gate = 1.0 - smoothstep(RIVER_RARITY, (RIVER_RARITY + 0.12).min(0.99), m);

        // this function returns (height_without_lowering, river_weight)
        // river_weight is our soft bank weight (0..1), not a hard core anymore
        (h, (bank_profile * mask_gate).max(0.0))
    };

    // Height WITH river lowering (no seam blending).
    let height_at = |xf: f32, zf: f32| -> f32 {
        let (mut h, core) = height_no_river_and_core(xf, zf);
        if enable_rivers && core > 0.0 {
            let target = SEA_LEVEL as f32 - RIVER_DEPTH;
            let w = (core * 0.95).clamp(0.0, 0.95);
            h = lerp(h, target, w);
        }
        h
    };

    /* -------------------- seam_weight / column_block -------------------- */

    #[inline]
    fn seam_weight(
        world_x: f32,
        world_z: f32,
        edge_pos: f32,
        axis_x: bool,
        r: f32,
        salt: u32,
        large: &FastNoiseLite,
        detail: &FastNoiseLite,
    ) -> (f32 /*score*/, f32 /*perp_dist*/) {
        if r <= 0.5 { return (0.0, 1e9); }
        let ox = ((salt & 0xFFFF) as f32) * 0.00091;
        let oz = ((salt >> 16) as f32) * 0.00123;
        let dir = if (salt & 2) == 0 { 1.0 } else { -1.0 };
        let base_phase = (salt as f32) * 0.00025;

        let (p_along, p_perp) = if axis_x { (world_z, world_x) } else { (world_x, world_z) };
        let perp = (p_perp - edge_pos).abs();

        let r_var = r * (0.80 + 0.30 * ((detail.get_noise_2d(world_x * 0.05 + ox * 2.0, world_z * 0.05 + oz * 2.0) * 0.5) + 0.5));
        let base_amp  = r_var * 0.18;
        let base_freq = 0.035;
        let off_b = (p_along * base_freq + base_phase).sin() * base_amp;

        let off_l = large .get_noise_2d(world_x * 0.06 + ox, world_z * 0.06 + oz) * (r_var * 0.45);
        let off_s = detail.get_noise_2d(world_z * 0.12 - oz, world_x * 0.12 + ox) * (r_var * 0.15);
        let off   = ((off_l + off_s + off_b) * dir).clamp(-0.60 * r_var, 0.60 * r_var);

        let s = ((p_perp - edge_pos) - off).abs();
        let base = if s >= r_var { 0.0 } else {
            let u = 1.0 - (s / r_var);
            u * u * (3.0 - 2.0 * u)
        };

        let gate = 1.0 - ((perp - (0.45 * r_var)) / ((0.95 * r_var) - (0.45 * r_var))).clamp(0.0, 1.0);
        (base * gate, perp)
    }

    #[inline]
    fn column_block(
        wy: i32,
        h_final: i32,
        beach_top_ok: bool,
        beach_sub_ok: bool,
        dirt_under_top: i32,
        sand_under_beach: i32,
        after_sand_dirt: i32,
        ids: (BlockId, BlockId, BlockId, BlockId, BlockId),
        chosen_top: BlockId,
        chosen_bottom: BlockId,
    ) -> BlockId {
        let (_, block_bottom, block_stone, block_seafloor, block_beach) = ids;

        if h_final >= SEA_LEVEL {
            if beach_top_ok {
                if wy == h_final { block_beach }
                else if wy >= h_final - sand_under_beach { block_beach }
                else if wy >= h_final - sand_under_beach - after_sand_dirt { block_bottom }
                else { block_stone }
            } else {
                if wy == h_final { chosen_top }
                else if wy >= h_final - dirt_under_top { chosen_bottom }
                else { block_stone }
            }
        } else {
            if beach_sub_ok {
                if wy == h_final { block_seafloor }
                else if wy >= h_final - 2 { block_seafloor }
                else { block_bottom }
            } else {
                if wy == h_final { block_seafloor }
                else if wy >= h_final - 2 { block_seafloor }
                else { block_bottom }
            }
        }
    }

    /* -------------------- Main loop -------------------- */

    // blend radius only affects the edge belt
    let r_f = (blend.radius.max(1) as f32).min(12.0);

    let (wx0, wz0) = (coord.x * CX as i32, coord.y * CZ as i32);
    let wx0f = wx0 as f32;
    let wz0f = wz0 as f32;
    let wx1f = (wx0 + CX as i32) as f32;
    let wz1f = (wz0 + CZ as i32) as f32;

    #[derive(Copy, Clone)] enum EdgeOri { X(f32), Z(f32) }
    #[derive(Copy, Clone)]
    struct ActiveEdge { top: BlockId, bottom: BlockId, salt: u32, ori: EdgeOri, h_off: f32, has_rivers: bool }

    let mut edges: smallvec::SmallVec<[ActiveEdge; 4]> = smallvec::SmallVec::new();
    if let Some(e) = blend.west  { edges.push(ActiveEdge{ top: e.top, bottom: e.bottom, salt: e.salt, ori: EdgeOri::X(wx0f), h_off: e.height_offset, has_rivers: e.rivers }); }
    if let Some(e) = blend.east  { edges.push(ActiveEdge{ top: e.top, bottom: e.bottom, salt: e.salt, ori: EdgeOri::X(wx1f), h_off: e.height_offset, has_rivers: e.rivers }); }
    if let Some(e) = blend.north { edges.push(ActiveEdge{ top: e.top, bottom: e.bottom, salt: e.salt, ori: EdgeOri::Z(wz0f), h_off: e.height_offset, has_rivers: e.rivers }); }
    if let Some(e) = blend.south { edges.push(ActiveEdge{ top: e.top, bottom: e.bottom, salt: e.salt, ori: EdgeOri::Z(wz1f), h_off: e.height_offset, has_rivers: e.rivers }); }

    for lz in 0..CZ {
        for lx in 0..CX {
            let wx = wx0 + lx as i32;
            let wz = wz0 + lz as i32;
            let xf = wx as f32;
            let zf = wz as f32;

            // heights (with and without river)
            let h_here = height_at(xf, zf);
            let (h_no_river, core_here) = height_no_river_and_core(xf, zf);

            // slope for beaches, etc.
            let h_xp = height_at(xf + 1.0, zf);
            let h_xm = height_at(xf - 1.0, zf);
            let h_zp = height_at(xf, zf + 1.0);
            let h_zm = height_at(xf, zf - 1.0);
            let slope_x = (h_here - h_xp).abs().max((h_here - h_xm).abs());
            let slope_z = (h_here - h_zp).abs().max((h_here - h_zm).abs());
            let slope_blocks = slope_x.max(slope_z).round() as i32;

            // small ring probe for coast/biome beaches
            let probe_r = COAST_RING_R.max(RIVER_WIDTH_MAX * 0.75);
            let mut ring_has_water = false;
            let mut ring_has_land  = false;
            for i in 0..COAST_RING_SAMPLES {
                let a = (i as f32) * (TAU / COAST_RING_SAMPLES as f32);
                let sx = xf + a.cos() * probe_r;
                let sz = zf + a.sin() * probe_r;
                let hh = height_at(sx, sz);
                if hh < SEA_LEVEL as f32 { ring_has_water = true; } else { ring_has_land = true; }
                if ring_has_water && ring_has_land { break; }
            }
            let river_near = enable_rivers && core_here > 0.20;
            let has_water = ring_has_water || river_near;
            let has_land  = ring_has_land  || !river_near;



            // choose nearest world-space curved seam
            let mut best_score = 0.0f32;
            let mut best_perp  = 1e9f32;
            let mut nei_top    = block_top;
            let mut nei_bottom = block_bottom;
            let mut nei_h_off  = b_params.height_offset;
            let mut nei_has_rivers = b_params.rivers;

            let mut near_x = 1e9f32;
            let mut near_z = 1e9f32;

            for e in edges.iter().copied() {
                let (edge_pos, axis_x) = match e.ori { EdgeOri::X(v) => (v, true), EdgeOri::Z(v) => (v, false) };
                let (sc, perp) = seam_weight(xf, zf, edge_pos, axis_x, r_f, e.salt, &seam_large, &seam_detail);
                if axis_x { near_x = near_x.min(perp); } else { near_z = near_z.min(perp); }

                if sc > 0.0 {
                    let c_and = sc + 0.006 * tie_jitter.get_noise_2d(xf * 0.5, zf * 0.5);
                    if c_and > best_score || (c_and == best_score && perp < best_perp) {
                        best_score = c_and;
                        best_perp  = perp;
                        nei_top    = e.top;
                        nei_bottom = e.bottom;
                        nei_h_off  = e.h_off;
                        nei_has_rivers = e.has_rivers;
                    }
                }
            }

            // corner relaxer
            let corner_mix = {
                let ax = smoothstep(0.0, r_f * 0.40, r_f - near_x.min(r_f));
                let az = smoothstep(0.0, r_f * 0.40, r_f - near_z.min(r_f));
                (ax * az).clamp(0.0, 1.0)
            };

            // soft border boost
            let bx = (lx as f32).min(CX as f32 - 1.0 - lx as f32).max(0.0);
            let bz = (lz as f32).min(CZ as f32 - 1.0 - lz as f32).max(0.0);
            let b  = bx.min(bz);
            let border_boost = 1.0 - (b / 2.0).clamp(0.0, 1.0);

            // slope factor
            let slope_factor = 1.0 - smoothstep(1.0, 4.0, slope_blocks as f32);

            // coverage for surface dithering
            let mut coverage = ((best_score * 1.40).min(1.0) * slope_factor).powf(0.80);
            coverage *= 1.0 - 0.45 * corner_mix;
            coverage = (coverage + border_boost * 0.18).clamp(0.0, 1.0);

            // choose column surface set
            let mask = map01(mix_mask_n.get_noise_2d(xf * 0.45, zf * 0.45));
            let (col_top, col_bottom) = if best_score > 0.0 && mask < coverage {
                (nei_top, nei_bottom)
            } else {
                (block_top, block_bottom)
            };

            // ---- RIVER TAPER across biome seam (river enable blends across edges) ----
            let river_delta_here = h_here - h_no_river; // <= 0 if river lowered the height
            let edge_w = if best_score > 0.0 {
                let t = ((r_f - best_perp) / r_f).clamp(0.0, 1.0);
                t * t * (3.0 - 2.0 * t)
            } else { 0.0 };
            let self_r = if enable_rivers { 1.0 } else { 0.0 };
            let nei_r  = if nei_has_rivers { 1.0 } else { 0.0 };
            let river_factor = if edge_w > 0.0 { lerp(self_r, nei_r, edge_w) } else { self_r };
            let mut h_comp = h_no_river + river_delta_here * river_factor;

            // ---- HEIGHT STITCH (peak-lift only to avoid walls) ----
            let p_raw_st = map01(plains_n.get_noise_2d(xf, zf));
            let plains_mask_st = smoothstep(
                (cfg.plains_threshold - cfg.plains_blend).clamp(0.0, 1.0),
                (cfg.plains_threshold + cfg.plains_blend).clamp(0.0, 1.0),
                p_raw_st,
            );
            let warp_amp_st = lerp(cfg.warp_amp_plains, cfg.warp_amp, 1.0 - plains_mask_st);
            let dx_st = warp_n.get_noise_2d(xf, zf) * warp_amp_st;
            let dz_st = warp_n.get_noise_2d(xf + 1000.0, zf - 1000.0) * warp_amp_st;
            let h01_st = map01(base_n.get_noise_2d(xf + dx_st, zf + dz_st));

            let mountain_mask_st = 1.0 - plains_mask_st;
            let peak_st = smoothstep(0.55, 0.95, h01_st);
            let delta_peak = (nei_h_off - b_params.height_offset) * mountain_mask_st * peak_st;

            if edge_w > 0.0 {
                h_comp = (h_comp + delta_peak * edge_w)
                    .clamp(SEA_FLOOR_MIN as f32, MOUNTAIN_MAX as f32);
            }

            // final height
            let mut h_final = if edge_w == 0.0 { h_here } else { h_comp };
            h_final = h_final.clamp(SEA_FLOOR_MIN as f32, MOUNTAIN_MAX as f32);
            let h_final = h_final.round() as i32;

            // beaches
            let near_top = h_final >= SEA_LEVEL && (h_final - SEA_LEVEL) <= BEACH_TOP_BAND;
            let near_sub = h_final <  SEA_LEVEL && (SEA_LEVEL - h_final) <= BEACH_SUB_BAND;
            let beach_top_ok = near_top && has_water && has_land && slope_blocks <= BEACH_MAX_SLOPE;
            let beach_sub_ok = near_sub;

            // column fill
            let r_seed: u32 = (cfg.seed as u32) ^ 0xABCD_1234 ^ (wx as u32).rotate_left(7) ^ (wz as u32).rotate_left(13);
            let mut dirt_under_top   = (2 + (r_seed & 1)) as i32;
            let mut sand_under_beach = (2 + ((r_seed >> 1) & 1)) as i32;
            let     after_sand_dirt  = (1 + ((r_seed >> 2) & 1)) as i32;

            let soil_depth = 2.6 + (BEACH_MAX_SLOPE - slope_blocks).max(0) as f32 * 0.2;
            dirt_under_top   = dirt_under_top  .min((soil_depth.floor() as i32).max(0));
            sand_under_beach = sand_under_beach.min((soil_depth.ceil()  as i32).max(0));

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }
                let id = column_block(
                    wy, h_final,
                    beach_top_ok, beach_sub_ok,
                    dirt_under_top, sand_under_beach, after_sand_dirt,
                    (block_top, block_bottom, block_stone, block_seafloor, block_beach),
                    col_top, col_bottom,
                );
                if id != 0 { out.set(lx, ly, lz, id); }
            }
        }
    }

    out
}


pub async fn mesh_subchunk_async(
    chunk: &ChunkData,
    reg: &RegLite,
    sub: usize,
    block_size: f32,
    borders: Option<BorderSnapshot>,
) -> Vec<(BlockId, MeshBuild)> {
    let mut by_block: HashMap<BlockId, MeshBuild> = HashMap::new();
    let s = block_size;
    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);

    let (east, west, south, north, snap_y0, _snap_y1) = if let Some(b) = borders {
        debug_assert_eq!(b.y0, y0, "BorderSnapshot.y0 != sub y0");
        debug_assert_eq!(b.y1, y1, "BorderSnapshot.y1 != sub y1");
        (b.east, b.west, b.south, b.north, b.y0, b.y1)
    } else {
        (None, None, None, None, y0, y1)
    };

    let sample_opt = |opt: &Option<Vec<BlockId>>, y: usize, i: usize, stride: usize| -> Option<BlockId> {
        opt.as_ref().map(|v| {
            let iy = y - snap_y0;
            v[iy * stride + i]
        })
    };

    let east_at_opt  = |y: usize, z: usize| sample_opt(&east,  y, z, CZ);
    let west_at_opt  = |y: usize, z: usize| sample_opt(&west,  y, z, CZ);
    let south_at_opt = |y: usize, x: usize| sample_opt(&south, y, x, CX);
    let north_at_opt = |y: usize, x: usize| sample_opt(&north, y, x, CX);

    let get = |x:isize,y:isize,z:isize| -> BlockId {
        if x < 0 || y < 0 || z < 0 || x >= CX as isize || y >= CY as isize || z >= CZ as isize { 0 }
        else { chunk.blocks[((y as usize)*CZ + (z as usize))*CX + (x as usize)] }
    };
    let uvq = |u0:f32,v0:f32,u1:f32,v1:f32, flip_v:bool| -> [[f32;2];4] {
        if !flip_v { [[u0,v0],[u1,v0],[u1,v1],[u0,v1]] } else { [[u0,v1],[u1,v1],[u1,v0],[u0,v0]] }
    };

    for y in y0..y1 {
        for z in 0..CZ {
            for x in 0..CX {
                let id = chunk.get(x,y,z);
                if id == 0 { continue; }

                let wx = x as f32 * s; let wy = y as f32 * s; let wz = z as f32 * s;
                let b = by_block.entry(id).or_insert_with(MeshBuild::new);

                // +Y (Top)
                if !(get(x as isize, y as isize+1, z as isize) != 0 && reg.opaque(get(x as isize, y as isize+1, z as isize))) {
                    let u = reg.uv(id, Face::Top);
                    b.quad([[wx,wy+s,wz+s],[wx+s,wy+s,wz+s],[wx+s,wy+s,wz],[wx,wy+s,wz]],[0.0,1.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,false));
                }
                // -Y (Bottom)
                if !(get(x as isize, y as isize-1, z as isize) != 0 && reg.opaque(get(x as isize, y as isize-1, z as isize))) {
                    let u = reg.uv(id, Face::Bottom);
                    b.quad([[wx,wy,wz],[wx+s,wy,wz],[wx+s,wy,wz+s],[wx,wy,wz+s]],[0.0,-1.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,false));
                }
                // +X (East)
                let n_east = if x + 1 < CX {
                    Some(get(x as isize + 1, y as isize, z as isize))
                } else {
                    east_at_opt(y, z)
                };
                if let Some(nei) = n_east {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::East);
                        b.quad(
                            [[wx+s,wy,wz+s],[wx+s,wy,wz],[wx+s,wy+s,wz],[wx+s,wy+s,wz+s]],
                            [1.0,0.0,0.0],
                            uvq(u.u0,u.v0,u.u1,u.v1,true)
                        );
                    }
                }

                // -X (West)
                let n_west = if x > 0 {
                    Some(get(x as isize - 1, y as isize, z as isize))
                } else {
                    west_at_opt(y, z)
                };
                if let Some(nei) = n_west {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::West);
                        b.quad(
                            [[wx,wy,wz],[wx,wy,wz+s],[wx,wy+s,wz+s],[wx,wy+s,wz]],
                            [-1.0,0.0,0.0],
                            uvq(u.u0,u.v0,u.u1,u.v1,true)
                        );
                    }
                }

                // +Z (South)
                let n_south = if z + 1 < CZ {
                    Some(get(x as isize, y as isize, z as isize + 1))
                } else {
                    south_at_opt(y, x)
                };
                if let Some(nei) = n_south {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::South);
                        b.quad(
                            [[wx,wy,wz+s],[wx+s,wy,wz+s],[wx+s,wy+s,wz+s],[wx,wy+s,wz+s]],
                            [0.0,0.0,1.0],
                            uvq(u.u0,u.v0,u.u1,u.v1,true)
                        );
                    }
                }

                // -Z (North)
                let n_north = if z > 0 {
                    Some(get(x as isize, y as isize, z as isize - 1))
                } else {
                    north_at_opt(y, x)
                };
                if let Some(nei) = n_north {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::North);
                        b.quad(
                            [[wx+s,wy,wz],[wx,wy,wz],[wx,wy+s,wz],[wx+s,wy+s,wz]],
                            [0.0,0.0,-1.0],
                            uvq(u.u0,u.v0,u.u1,u.v1,true)
                        );
                    }
                }
            }
        }
    }

    by_block.into_iter().map(|(k,b)| (k,b)).collect()
}

pub fn save_chunk_sync(
    ws: &WorldSave,
    cache: &mut RegionCache,
    coord: IVec2,
    ch: &ChunkData,
) -> std::io::Result<()> {
    let blocks = encode_chunk(ch);

    let old = cache.read_chunk(ws, coord).ok().flatten();
    let merged = container_upsert(old.as_deref(), TAG_BLK1, &blocks);

    cache.write_chunk_replace(ws, coord, &merged)
}

pub async fn load_or_gen_chunk_async(
    ws_root: PathBuf,
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId, BlockId, BlockId),
    blend: BiomeEdgeBlend,
    params: BiomeTerrainParams,
    cfg: WorldGenConfig,
) -> ChunkData {
    let (r_coord, _) = chunk_to_region_slot(coord);
    let path = ws_root.join("region").join(format!("r.{}.{}.region", r_coord.x, r_coord.y));
    if let Ok(mut rf) = RegionFile::open(&path) {
        if let Ok(Some(buf)) = rf.read_chunk(coord) {
            let data = if slot_is_container(&buf) {
                container_find(&buf, TAG_BLK1).map(|b| b.to_vec())
            } else {
                Some(buf)
            };
            if let Some(b) = data {
                if let Ok(c) = decode_chunk(&b) {
                    return c;
                }
            }
        }
    }
    generate_chunk_async_noise(coord, ids, blend, params, cfg).await
}

pub fn snapshot_borders(chunk_map: &ChunkMap, coord: IVec2, y0: usize, y1: usize) -> BorderSnapshot {
    let mut snap = BorderSnapshot { y0, y1, east: None, west: None, south: None, north: None };

    let take_xz = |c: &ChunkData, x: usize, z: usize, y: usize| -> BlockId { c.get(x,y,z) };

    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x + 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, 0, z, y)); } }
        snap.east = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x - 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, CX-1, z, y)); } }
        snap.west = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y + 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, 0, y)); } }
        snap.south = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y - 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, CZ-1, y)); } }
        snap.north = Some(v);
    }
    snap
}

pub fn area_ready(
    center: IVec2,
    radius: i32,
    chunk_map: &ChunkMap,
    pending_gen: &PendingGen,
    pending_mesh: &PendingMesh,
    backlog: &MeshBacklog,
) -> bool {
    for dz in -radius..=radius {
        for dx in -radius..=radius {
            let c = IVec2::new(center.x + dx, center.y + dz);
            if !chunk_map.chunks.contains_key(&c) { return false; }
            if pending_gen.0.contains_key(&c) { return false; }
            if pending_mesh.0.keys().any(|(cc, _)| *cc == c) { return false; }
            if backlog.0.iter().any(|(cc, _)| *cc == c) { return false; }
        }
    }
    true
}

pub fn despawn_mesh_set(
    keys: impl IntoIterator<Item = (IVec2, u8, BlockId)>,
    mesh_index: &mut ChunkMeshIndex,
    commands: &mut Commands,
    q_mesh: &Query<&Mesh3d>,
    meshes: &mut Assets<Mesh>,
) {
    for key in keys {
        if let Some(ent) = mesh_index.map.remove(&key) {
            if let Ok(Mesh3d(handle)) = q_mesh.get(ent) {
                meshes.remove(handle.id());
            }
            commands.entity(ent).despawn();
        }
    }
}

pub fn can_spawn_mesh(pending_mesh: &PendingMesh) -> bool {
    pending_mesh.0.len() < MAX_INFLIGHT_MESH
}
pub fn can_spawn_gen(pending_gen: &PendingGen) -> bool {
    pending_gen.0.len() < MAX_INFLIGHT_GEN
}

pub fn backlog_contains(backlog: &MeshBacklog, key: (IVec2, usize)) -> bool {
    backlog.0.iter().any(|&k| k == key)
}

pub fn enqueue_mesh(backlog: &mut MeshBacklog, pending: &PendingMesh, key: (IVec2, usize)) {
    if pending.0.contains_key(&key) { return; }
    if backlog_contains(backlog, key) { return; }
    backlog.0.push_back(key);
}

pub fn encode_chunk(ch: &ChunkData) -> Vec<u8> {
    let cfg = config::standard();
    let ser = encode_to_vec(&ch.blocks, cfg).expect("encode blocks");
    compress_prepend_size(&ser)
}

fn decode_chunk(buf: &[u8]) -> std::io::Result<ChunkData> {
    let de = decompress_size_prepended(buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
    config::standard();

    let (blocks, _len): (Vec<BlockId>, usize) = decode_from_slice(&de, config::standard())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    if blocks.len() != CX * CY * CZ {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "block array size mismatch",
        ));
    }

    let mut c = ChunkData::new();
    c.blocks.copy_from_slice(&blocks);
    Ok(c)
}

#[inline] pub fn leap(a:f32, b:f32, t:f32) -> f32 { a + (b - a) * t }
#[inline] pub fn smoothstep(e0:f32, e1:f32, x:f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

#[inline] pub fn map01(x: f32) -> f32 { x * 0.5 + 0.5 }

#[inline]
pub fn chunk_to_region_slot(c: IVec2) -> (IVec2, usize) {
    let rx = div_floor(c.x, REGION_SIZE);
    let rz = div_floor(c.y, REGION_SIZE);
    let lx = mod_floor(c.x, REGION_SIZE) as usize;
    let lz = mod_floor(c.y, REGION_SIZE) as usize;
    let idx = lz * (REGION_SIZE as usize) + lx;
    (IVec2::new(rx, rz), idx)
}

#[inline]
pub(crate) fn col_rand_u32(x: i32, z: i32, seed: u32) -> u32 {
    let mut n = (x as u32).wrapping_mul(374761393)
        ^ (z as u32).wrapping_mul(668265263)
        ^ seed;
    n ^= n >> 13;
    n = n.wrapping_mul(1274126177);
    n ^ (n >> 16)
}

#[inline] fn div_floor(a: i32, b: i32) -> i32 { (a as f32 / b as f32).floor() as i32 }
#[inline] fn mod_floor(a: i32, b: i32) -> i32 { a - div_floor(a,b)*b }

#[inline]
pub fn is_waiting(state: &State<AppState>) -> bool {
    matches!(state.get(),
        AppState::Loading(LoadingStates::BaseGen)
    )
}

#[inline]
pub(crate) fn neighbors_ready(chunk_map: &ChunkMap, c: IVec2) -> bool {
    neighbors4_iter(c).all(|nc| chunk_map.chunks.contains_key(&nc))
}

#[inline]
pub(crate) fn neighbors4_iter(c: IVec2) -> impl Iterator<Item = IVec2> {
    DIR4.into_iter().map(move |d| c + d)
}