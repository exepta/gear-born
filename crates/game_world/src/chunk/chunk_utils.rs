use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, LoadingStates};
use game_core::world::biome::BiomeEdgeBlend;
use game_core::world::block::{BlockId, BlockRegistry, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex};
use game_core::world::chunk_dim::*;
use game_core::world::save::*;
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::HashMap;
use std::f32::consts::TAU;
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
    cfg: WorldGenConfig,
    _reg: &BlockRegistry,
) -> ChunkData {
    // ---- MC-like constants ----
    const SEA_LEVEL: i32 = 63;
    const SEA_FLOOR_MIN: i32 = 5;
    const MOUNTAIN_MAX: i32 = 192;

    const BEACH_TOP_BAND: i32 = 1;
    const BEACH_SUB_BAND: i32 = 2;

    const COAST_WIDTH: f32 = 10.0;
    const COAST_RING_R: f32 = 8.0;
    const COAST_RING_SAMPLES: usize = 12;
    const BEACH_MAX_SLOPE: i32 = 2;

    const ENABLE_RIVERS: bool = false;
    const ENABLE_COAST: bool  = true;

    let (block_top, block_bottom, block_stone, block_seafloor, block_beach) = ids;
    let mut out = ChunkData::new();

    // ---- Noises (height & masks) ----
    let mut base_n = FastNoiseLite::with_seed(cfg.seed);
    base_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    base_n.set_frequency(Some(cfg.height_freq.max(0.0001)));
    base_n.set_fractal_type(Some(FractalType::FBm));
    base_n.set_fractal_octaves(Some(5));
    base_n.set_fractal_gain(Some(0.5));
    base_n.set_fractal_lacunarity(Some(2.0));

    let mut roll_n = FastNoiseLite::with_seed(cfg.seed ^ 0xA1A1_5151u32 as i32);
    roll_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    roll_n.set_frequency(Some((cfg.height_freq * 2.1).max(0.0001)));
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

    // Rivers (kept even if disabled → compiles)
    let mut river_n = FastNoiseLite::with_seed(cfg.seed ^ 0x1357_9BDF);
    river_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_n.set_frequency(Some(0.0016));
    let mut river_warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x2468_ACF1);
    river_warp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_warp_n.set_frequency(Some(0.015));

    // coherent mask inside the seam band (bigger blobs)
    let mut mix_mask_n = FastNoiseLite::with_seed(cfg.seed ^ 0xB1D3_B10Du32 as i32);
    mix_mask_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    mix_mask_n.set_frequency(Some(0.045));
    mix_mask_n.set_fractal_type(Some(FractalType::FBm));
    mix_mask_n.set_fractal_octaves(Some(2));
    mix_mask_n.set_fractal_gain(Some(0.5));
    mix_mask_n.set_fractal_lacunarity(Some(2.0));

    // Large-scale curve of the seam (meanders along the border)
    let mut seam_large = FastNoiseLite::with_seed(cfg.seed ^ 0x7A61_5EAF);
    seam_large.set_noise_type(Some(NoiseType::OpenSimplex2));
    seam_large.set_frequency(Some(0.019));

    // Small detail wobble on top
    let mut seam_detail = FastNoiseLite::with_seed(cfg.seed ^ 0x51DE_77A1);
    seam_detail.set_noise_type(Some(NoiseType::OpenSimplex2));
    seam_detail.set_frequency(Some(0.065));
    seam_detail.set_fractal_type(Some(FractalType::FBm));
    seam_detail.set_fractal_octaves(Some(2));
    seam_detail.set_fractal_gain(Some(0.5));
    seam_detail.set_fractal_lacunarity(Some(2.0));

    // Seam field (2D) → produces rounded bulges, shared across chunks.
    let mut seam_n = FastNoiseLite::with_seed(cfg.seed ^ 0x7A61_5EAF);
    seam_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    seam_n.set_frequency(Some(0.035));
    seam_n.set_fractal_type(Some(FractalType::FBm));
    seam_n.set_fractal_octaves(Some(3));
    seam_n.set_fractal_gain(Some(0.5));
    seam_n.set_fractal_lacunarity(Some(2.0));

    // ---- helpers ----
    #[inline] fn map01(x: f32) -> f32 { (x * 0.5) + 0.5 }
    #[inline]
    fn smoothstep(a: f32, b: f32, x: f32) -> f32 {
        let t = ((x - a) / (b - a)).clamp(0.0, 1.0);
        t * t * (3.0 - 2.0 * t)
    }
    #[inline] fn lerp(a: f32, b: f32, t: f32) -> f32 { a + (b - a) * t }

    // Height function; unchanged in principle (MC-like)
    let height_at = |xf: f32, zf: f32| -> f32 {
        let p_raw = map01(plains_n.get_noise_2d(xf, zf));
        let plains_mask = smoothstep(
            (cfg.plains_threshold - cfg.plains_blend).clamp(0.0, 1.0),
            (cfg.plains_threshold + cfg.plains_blend).clamp(0.0, 1.0),
            p_raw,
        );

        let warp_amp = lerp(cfg.warp_amp_plains, cfg.warp_amp, 1.0 - plains_mask);
        let dx = warp_n.get_noise_2d(xf, zf) * warp_amp;
        let dz = warp_n.get_noise_2d(xf + 1000.0, zf - 1000.0) * warp_amp;

        let h01 = map01(base_n.get_noise_2d(xf + dx, zf + dz));
        let roll = map01(roll_n.get_noise_2d(xf + dx * 0.35, zf + dz * 0.35));

        let local_span = lerp(cfg.plains_span as f32, cfg.height_span as f32, 1.0 - plains_mask);
        let mid = SEA_LEVEL as f32 + cfg.base_height;
        let mut h = mid + (h01 - 0.5) * local_span;
        h += (roll - 0.5) * 3.0;

        if cfg.plains_flatten > 0.0 {
            h = lerp(mid, h, 1.0 - plains_mask * cfg.plains_flatten);
        }

        if ENABLE_COAST {
            let dist = h - SEA_LEVEL as f32;
            let ramp = 1.0 - smoothstep(0.0, COAST_WIDTH, dist.abs());
            let compress = lerp(0.70, 1.0, 1.0 - ramp);
            h = SEA_LEVEL as f32 + dist * compress;

            let dunes = coast_n.get_noise_2d(xf * 1.7, zf * 1.7);
            h += dunes * 0.35 * (1.0 - smoothstep(0.0, COAST_WIDTH * 0.7, dist.abs()));
        }

        if ENABLE_RIVERS {
            let rx = xf + river_warp_n.get_noise_2d(xf * 0.015, zf * 0.015) * 24.0;
            let rz = zf + river_warp_n.get_noise_2d(xf * 0.015 + 1000.0, zf * 0.015 - 1000.0) * 24.0;
            let r = river_n.get_noise_2d(rx, rz).abs();
            let core = 1.0 - smoothstep(0.02, 0.10, r);
            if core > 0.0 {
                h = lerp(h, SEA_LEVEL as f32 - 1.0, (core * 0.85).clamp(0.0, 0.85));
            }
        }

        h
    };

    // ---- seam math (world-space, curved) ----
    #[inline]
    fn seam_weight_x(
        world_x: f32, world_z: f32,
        edge_x: f32, r: f32, salt: u32,
        large: &FastNoiseLite, detail: &FastNoiseLite
    ) -> (f32 /*score*/, f32 /*perp_dist*/) {
        if r <= 0.5 { return (0.0, 1e9); }

        // Order-independent pair offset, consistent on both sides
        let ox = ((salt & 0xFFFF) as f32) * 0.00091;
        let oz = ((salt >> 16) as f32) * 0.00123;
        let dir = if (salt & 2) == 0 { 1.0 } else { -1.0 };

        // Perpendicular distance to the true border
        let perp = (world_x - edge_x).abs();

        // Variable radius (mild), limited to keep the seam near the border
        let r_var = r * (0.80 + 0.30 * ((detail.get_noise_2d(world_z * 0.05 + ox * 2.0, world_x * 0.05 + oz * 2.0) * 0.5) + 0.5));

        // Limit how far the center line can drift away from the border: <= 0.6 * r_var
        let off_l = large .get_noise_2d(world_z * 0.06 + ox, world_x * 0.06 + oz) * (r_var * 0.45);
        let off_s = detail.get_noise_2d(world_x * 0.12 - oz, world_z * 0.12 + ox) * (r_var * 0.15);
        let off   = (off_l + off_s) * dir;

        // Bell around the (limited) center line
        let s = ((world_x - edge_x) - off).abs();
        let base = if s >= r_var { 0.0 } else {
            let u = 1.0 - (s / r_var);
            u * u * (3.0 - 2.0 * u) // 0..1
        };

        // Gate by distance to the *true* edge to keep the blend band tight
        // fully on up to 0.45*r, fades to zero by 0.95*r
        let gate = 1.0 - ((perp - (0.45 * r_var)) / ((0.95 * r_var) - (0.45 * r_var))).clamp(0.0, 1.0);
        let score = base * gate;

        (score, perp)
    }

    #[inline]
    fn seam_weight_z(
        world_x: f32, world_z: f32,
        edge_z: f32, r: f32, salt: u32,
        large: &FastNoiseLite, detail: &FastNoiseLite
    ) -> (f32 /*score*/, f32 /*perp_dist*/) {
        if r <= 0.5 { return (0.0, 1e9); }

        let ox = ((salt & 0xFFFF) as f32) * 0.00091;
        let oz = ((salt >> 16) as f32) * 0.00123;
        let dir = if (salt & 2) == 0 { 1.0 } else { -1.0 };

        let perp = (world_z - edge_z).abs();

        let r_var = r * (0.80 + 0.30 * ((detail.get_noise_2d(world_x * 0.05 + ox * 2.0, world_z * 0.05 + oz * 2.0) * 0.5) + 0.5));

        let off_l = large .get_noise_2d(world_x * 0.06 + ox, world_z * 0.06 + oz) * (r_var * 0.45);
        let off_s = detail.get_noise_2d(world_z * 0.12 - oz, world_x * 0.12 + ox) * (r_var * 0.15);
        let off   = (off_l + off_s) * dir;

        let s = ((world_z - edge_z) - off).abs();
        let base = if s >= r_var { 0.0 } else {
            let u = 1.0 - (s / r_var);
            u * u * (3.0 - 2.0 * u)
        };

        let gate = 1.0 - ((perp - (0.45 * r_var)) / ((0.95 * r_var) - (0.45 * r_var))).clamp(0.0, 1.0);
        let score = base * gate;

        (score, perp)
    }

    let r_f = blend.radius.max(1) as f32;

    let (wx0, wz0) = (coord.x * CX as i32, coord.y * CZ as i32);
    let wx0f = wx0 as f32;
    let wz0f = wz0 as f32;
    let wx1f = (wx0 + CX as i32) as f32;
    let wz1f = (wz0 + CZ as i32) as f32;

    for lz in 0..CZ {
        for lx in 0..CX {
            let wx = wx0 + lx as i32;
            let wz = wz0 + lz as i32;
            let xf = wx as f32;
            let zf = wz as f32;

            // ---- height/slope/coast ----
            let h_here = height_at(xf, zf);
            let mut h_final = h_here.round() as i32;
            h_final = h_final.clamp(SEA_FLOOR_MIN, MOUNTAIN_MAX);

            let h_xp = height_at(xf + 1.0, zf);
            let h_xm = height_at(xf - 1.0, zf);
            let h_zp = height_at(xf, zf + 1.0);
            let h_zm = height_at(xf, zf - 1.0);
            let slope_x = (h_here - h_xp).abs().max((h_here - h_xm).abs());
            let slope_z = (h_here - h_zp).abs().max((h_here - h_zm).abs());
            let slope_blocks = slope_x.max(slope_z).round() as i32;

            // coast ring
            let mut has_water = false;
            let mut has_land  = false;
            for i in 0..COAST_RING_SAMPLES {
                let a = (i as f32) * (TAU / COAST_RING_SAMPLES as f32);
                let sx = xf + a.cos() * COAST_RING_R;
                let sz = zf + a.sin() * COAST_RING_R;
                let hh = height_at(sx, sz);
                if hh < SEA_LEVEL as f32 { has_water = true; } else { has_land = true; }
                if has_water && has_land { break; }
            }
            let near_top = h_final >= SEA_LEVEL && (h_final - SEA_LEVEL) <= BEACH_TOP_BAND;
            let near_sub = h_final <  SEA_LEVEL && (SEA_LEVEL - h_final) <= BEACH_SUB_BAND;
            let beach_top_ok = near_top && has_water && has_land && slope_blocks <= BEACH_MAX_SLOPE;
            let beach_sub_ok = near_sub;

            // ---- world-space curved seam weight (max over active edges) ----
            let mut best_score = 0.0f32;
            let mut best_perp  = 1e9f32;
            let mut nei_top    = block_top;
            let mut nei_bottom = block_bottom;

            // west
            if let Some(em) = blend.west  {
                let (sc, perp) = seam_weight_x(xf, zf, wx0f, r_f, em.salt, &seam_large, &seam_detail);
                if sc > 0.0 && (sc > best_score || perp < best_perp) {
                    best_score = sc; best_perp = perp; nei_top = em.top; nei_bottom = em.bottom;
                }
            }
            // east
            if let Some(em) = blend.east  {
                let (sc, perp) = seam_weight_x(xf, zf, wx1f, r_f, em.salt, &seam_large, &seam_detail);
                if sc > 0.0 && (sc > best_score || perp < best_perp) {
                    best_score = sc; best_perp = perp; nei_top = em.top; nei_bottom = em.bottom;
                }
            }
            // north
            if let Some(em) = blend.north {
                let (sc, perp) = seam_weight_z(xf, zf, wz0f, r_f, em.salt, &seam_large, &seam_detail);
                if sc > 0.0 && (sc > best_score || perp < best_perp) {
                    best_score = sc; best_perp = perp; nei_top = em.top; nei_bottom = em.bottom;
                }
            }
            // south
            if let Some(em) = blend.south {
                let (sc, perp) = seam_weight_z(xf, zf, wz1f, r_f, em.salt, &seam_large, &seam_detail);
                if sc > 0.0 && (sc > best_score || perp < best_perp) {
                    best_score = sc; best_perp = perp; nei_top = em.top; nei_bottom = em.bottom;
                }
            }

            // Slope slightly reduces takeover
            let slope_factor = 1.0 - smoothstep(1.0, 4.0, slope_blocks as f32);

            // 1-block forced border to kill any seam-grid artifacts
            const FORCED_BORDER: i32 = 1;
            let at_forced_border =
                (blend.west.is_some()  && (lx as i32) < FORCED_BORDER) ||
                    (blend.east.is_some()  && (lx as i32) >= (CX as i32 - FORCED_BORDER)) ||
                    (blend.north.is_some() && (lz as i32) < FORCED_BORDER) ||
                    (blend.south.is_some() && (lz as i32) >= (CZ as i32 - FORCED_BORDER));

            // More tempered coverage (no deep wedges)
            let coverage = (best_score * 1.25).min(1.0) * slope_factor;

            // Coherent mask for dithering inside the band
            let mask = map01(mix_mask_n.get_noise_2d(xf * 0.52, zf * 0.52));

            // Final material: only the *winning* nearest edge may replace the column
            let (col_top, col_bottom) = if at_forced_border || (best_score > 0.0 && mask < coverage) {
                (nei_top, nei_bottom)
            } else {
                (block_top, block_bottom)
            };

            // ---- column fill (MC-like layering) ----
            // small per-column variation
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

                let id = if h_final >= SEA_LEVEL {
                    if beach_top_ok {
                        if wy == h_final { block_beach }
                        else if wy >= h_final - sand_under_beach { block_beach }
                        else if wy >= h_final - sand_under_beach - after_sand_dirt { block_bottom }
                        else { block_stone }
                    } else {
                        if wy == h_final { col_top }
                        else if wy >= h_final - dirt_under_top { col_bottom }
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
                };

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
    reg: &BlockRegistry,
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
    generate_chunk_async_noise(coord, ids, blend, cfg, reg).await
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