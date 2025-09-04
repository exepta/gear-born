// chunk_utils.rs
use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, LoadingStates};
use game_core::world::biome::*;
use game_core::world::block::{BlockId, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex};
use game_core::world::chunk_dim::*;
use game_core::world::save::*;
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::OnceLock;

pub const MAX_INFLIGHT_MESH: usize = 32;
pub const MAX_INFLIGHT_GEN:  usize = 32;

pub(crate) const DIR4: [IVec2; 4] = [
    IVec2::new( 1,  0),
    IVec2::new(-1,  0),
    IVec2::new( 0,  1),
    IVec2::new( 0, -1),
];

#[inline] pub fn leap(a:f32, b:f32, t:f32) -> f32 { a + (b - a) * t }
#[inline] pub fn smoothstep(e0:f32, e1:f32, x:f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}
#[inline] pub fn map01(x: f32) -> f32 { x * 0.5 + 0.5 }

#[derive(Clone, Default)]
struct WName { name: String, w: f32 }
#[derive(Clone, Default)]
struct SurfaceByName {
    top:       Vec<WName>,
    bottom:    Vec<WName>,
    under:     Vec<WName>,
    deep_under:Vec<WName>,
}
static SURFACE_BANK: OnceLock<Vec<SurfaceByName>> = OnceLock::new();

#[inline]
fn wb_list_to_w_name(list: &[WeightedBlock]) -> Vec<WName> {
    let sum: f32 = list.iter().map(|w| w.weight.max(0.0)).sum();
    if sum > 0.0 {
        list.iter()
            .map(|w| WName { name: w.name.clone(), w: w.weight.max(0.0) / sum })
            .collect()
    } else {
        let n = list.len().max(1) as f32;
        list.iter().map(|w| WName { name: w.name.clone(), w: 1.0 / n }).collect()
    }
}
#[inline]
fn from_surface(s: &BiomeSurface) -> SurfaceByName {
    SurfaceByName {
        top:        wb_list_to_w_name(&s.top_blocks),
        bottom:     wb_list_to_w_name(&s.bottom_blocks),
        under:      wb_list_to_w_name(&s.under_blocks),
        deep_under: wb_list_to_w_name(&s.deep_under_blocks),
    }
}
#[inline]
fn pick_weighted_name(list: &[WName], r_u32: u32) -> Option<&str> {
    if list.is_empty() { return None; }
    let sum: f32 = list.iter().map(|w| w.w).sum();
    if sum <= 0.0 { return Some(list.last().unwrap().name.as_str()); }
    let rf = (r_u32 as f64) / (u32::MAX as f64);
    let target = (rf as f32) * sum;
    let mut acc = 0.0;
    for w in list {
        acc += w.w;
        if target <= acc { return Some(w.name.as_str()); }
    }
    Some(list.last().unwrap().name.as_str())
}
#[inline]
fn name_to_id(name: &str, ids: (BlockId, BlockId, BlockId, BlockId)) -> BlockId {
    let (grass, dirt, stone, sand) = ids;
    match name {
        "grass_block" => grass,
        "dirt_block"  => dirt,
        "stone_block" => stone,
        "sand_block"  => sand,
        "clay_block"  => dirt,
        _ => stone,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Clim { Cold, Normal, Hot }
impl From<Climate> for Clim {
    fn from(c: Climate) -> Self {
        match c {
            Climate::Cold   => Clim::Cold,
            Climate::Normal => Clim::Normal,
            Climate::Hot    => Clim::Hot,
        }
    }
}

#[derive(Clone, Copy)]
struct SizeRange { min: i32, max: i32 }
const SMALL:      SizeRange = SizeRange{min:  20, max:  24};
const MEDIUM:     SizeRange = SizeRange{min: 48, max: 64};
const LARGE:      SizeRange = SizeRange{min: 80, max: 140};
const VERY_LARGE: SizeRange = SizeRange{min: 160, max: 240};
const GIGANTIC:   SizeRange = SizeRange{min:260, max:420};

const BASE_CHUNK: i32 = 12;

#[inline]
fn h2u(x: i32, z: i32, seed: u32) -> u32 {
    let mut n = (x as u32).wrapping_mul(374761393)
        ^ (z as u32).wrapping_mul(668265263)
        ^ seed;
    n ^= n >> 13;
    n = n.wrapping_mul(1274126177);
    n ^ (n >> 16)
}
#[inline]
fn u_rand01(x: i32, z: i32, seed: u32) -> f32 {
    (h2u(x,z,seed) as f64 / (u32::MAX as f64)) as f32
}

struct BioAcc<'a> {
    table: &'a BiomeTable,
    surfaces: &'a [SurfaceByName],
}
impl<'a> BioAcc<'a> {
    fn climate(&self, i: usize) -> Clim {
        Clim::from(self.table.climate[i])
    }
    fn forbids(&self, a: usize, b: usize) -> bool {
        self.table.forbids(a, b)
    }
    fn surface(&self, i: usize) -> Option<&'a SurfaceByName> {
        self.surfaces.get(i)
    }
}

#[derive(Clone, Copy)]
struct Site {
    biome_idx: usize,
    radius_chunks: f32,
    jx: f32, jz: f32,
}

#[derive(Clone, Copy)]
struct BiomeAt {
    primary: usize,
    secondary: usize,
    edge_mix: f32,
}

#[inline]
fn chunks_to_world(c: f32) -> f32 {
    c * (CX as f32)
}

fn allowed_neighbors(acc: &BioAcc, a: usize, b: usize) -> bool {
    if acc.forbids(a, b) { return false; }
    match (acc.climate(a), acc.climate(b)) {
        (Clim::Cold, Clim::Hot) | (Clim::Hot, Clim::Cold) => false,
        _ => true,
    }
}

fn pick_size_chunks(xc: i32, zc: i32, seed: u32) -> i32 {
    let r0 = u_rand01(xc, zc, seed ^ 0x51A_0001);
    let size_rng = if r0 < 0.28 {
        SMALL
    } else if r0 < 0.55 {
        MEDIUM
    } else if r0 < 0.78 {
        LARGE
    } else if r0 < 0.93 {
        VERY_LARGE
    } else {
        GIGANTIC
    };
    let r1 = u_rand01(xc, zc, seed ^ 0x51A_0002);
    (size_rng.min as f32 + (size_rng.max - size_rng.min) as f32 * r1).round() as i32
}

fn pick_biome_for_cell(
    acc: &BioAcc,
    xc: i32, zc: i32, seed: u32,
    west: Option<usize>, north: Option<usize>,
) -> usize {
    let tries = 8;
    for t in 0..tries {
        let r = u_rand01(xc + t, zc - t, seed ^ 0xB10_0001);
        let mut total = 0.0f32;
        for b in &acc.table.list { total += b.rarity.max(0.0); }
        let target = r * total.max(0.000_001);
        let mut acc_w = 0.0f32;
        let mut pick = 0usize;
        for (i, b) in acc.table.list.iter().enumerate() {
            acc_w += b.rarity.max(0.0);
            if target <= acc_w { pick = i; break; }
        }
        let ok_w = west.map_or(true, |w| allowed_neighbors(acc, pick, w));
        let ok_n = north.map_or(true, |n| allowed_neighbors(acc, pick, n));
        if ok_w && ok_n { return pick; }
    }
    for i in 0..acc.table.list.len() {
        let ok_w = west.map_or(true, |w| allowed_neighbors(acc, i, w));
        let ok_n = north.map_or(true, |n| allowed_neighbors(acc, i, n));
        if ok_w && ok_n { return i; }
    }
    0
}

fn site_of_cell(acc: &BioAcc, xc: i32, zc: i32, seed: u32) -> Site {
    let west  = (xc > i32::MIN).then(|| pick_biome_for_cell(acc, xc-1, zc, seed, None, None));
    let north = (zc > i32::MIN).then(|| pick_biome_for_cell(acc, xc, zc-1, seed, west, None));
    let biome = pick_biome_for_cell(acc, xc, zc, seed, west, north);

    let edge_chunks = pick_size_chunks(xc, zc, seed);
    let radius_chunks = (edge_chunks as f32) * 0.5;

    let base_world = chunks_to_world(BASE_CHUNK as f32);
    let jx = (u_rand01(xc, zc, seed ^ 0xABCD_1001) - 0.5) * 0.70 * base_world;
    let jz = (u_rand01(xc, zc, seed ^ 0xABCD_1002) - 0.5) * 0.70 * base_world;

    Site { biome_idx: biome, radius_chunks: radius_chunks.max(2.0), jx, jz }
}

fn biome_query(acc: &BioAcc, seed: u32, wxf: f32, wzf: f32) -> BiomeAt {
    let base_world = chunks_to_world(BASE_CHUNK as f32);
    let gx = (wxf / base_world).floor() as i32;
    let gz = (wzf / base_world).floor() as i32;

    let mut best = (f32::INFINITY, 0usize);
    let mut second = (f32::INFINITY, 0usize);
    for dz in -1..=1 {
        for dx in -1..=1 {
            let cx = gx + dx;
            let cz = gz + dz;
            let site = site_of_cell(acc, cx, cz, seed);

            let sx = (cx as f32 + 0.5) * base_world + site.jx;
            let sz = (cz as f32 + 0.5) * base_world + site.jz;
            let dxw = (gx as f32) - sx;
            let dzw = (gz as f32) - sz;

            let r_world = chunks_to_world(site.radius_chunks);
            let d = (dxw*dxw + dzw*dzw).sqrt() / r_world.max(1.0);

            if d < best.0 {
                second = best;
                best = (d, site.biome_idx);
            } else if d < second.0 {
                second = (d, site.biome_idx);
            }
        }
    }

    let k = ((second.0 - best.0) / 0.35).clamp(0.0, 1.0);
    let edge_mix = 1.0 - k; // 0 .. 1
    BiomeAt { primary: best.1, secondary: second.1, edge_mix }
}

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId, BlockId),
    cfg: WorldGenConfig,
    biomes: BiomeTable,
) -> ChunkData {
    use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};

    const SEA_LEVEL:      i32 = 58;
    const COAST_MAX:      i32 = 58;
    const SEA_FLOOR_MIN:  i32 = 5;
    const MOUNTAIN_MAX:   i32 = 180;
    const LAND_LIFT:   f32 = 5.0;
    const OCEAN_DEEPEN: f32 = 12.0;

    let mut c = ChunkData::new();

    // ---- base noises
    let mut height_n = FastNoiseLite::with_seed(cfg.seed);
    height_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    height_n.set_frequency(Some(cfg.height_freq));
    height_n.set_fractal_type(Some(FractalType::FBm));
    height_n.set_fractal_octaves(Some(5));
    height_n.set_fractal_gain(Some(0.5));
    height_n.set_fractal_lacunarity(Some(2.0));

    let mut warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x005E_EDBA);
    warp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    warp_n.set_frequency(Some(cfg.warp_freq));

    let mut plains_n = FastNoiseLite::with_seed(cfg.seed ^ 0x000B_10E);
    plains_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    plains_n.set_frequency(Some(cfg.plains_freq));

    let mut blend_n = FastNoiseLite::with_seed((cfg.seed as u32 ^ 0xB1E0_A45E_u32) as i32);
    blend_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    blend_n.set_frequency(Some(1.0 / 48.0));

    let mut bwx = FastNoiseLite::with_seed((cfg.seed as u32 ^ 0xB1E0_A45C_u32) as i32);
    bwx.set_noise_type(Some(NoiseType::OpenSimplex2));
    bwx.set_frequency(Some(0.0022));

    let mut bwz = FastNoiseLite::with_seed((cfg.seed as u32 ^ 0xB1E0_A45D_u32) as i32);
    bwz.set_noise_type(Some(NoiseType::OpenSimplex2));
    bwz.set_frequency(Some(0.0022));

    let mut belts_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00DE_ADB1);
    belts_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    belts_n.set_frequency(Some(0.0009));
    belts_n.set_fractal_type(Some(FractalType::FBm));
    belts_n.set_fractal_octaves(Some(2));

    let mut continent_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00C0_FFEE);
    continent_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    continent_n.set_frequency(Some(0.0010));
    continent_n.set_fractal_type(Some(FractalType::FBm));
    continent_n.set_fractal_octaves(Some(3));

    let mut macro_plains_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00A1_A1A1);
    macro_plains_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    macro_plains_n.set_frequency(Some(0.0018));

    let mut mount_base_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00B0_55E0);
    mount_base_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    mount_base_n.set_frequency(Some(0.0018));
    mount_base_n.set_fractal_type(Some(FractalType::FBm));
    mount_base_n.set_fractal_octaves(Some(4));
    mount_base_n.set_fractal_gain(Some(0.5));
    mount_base_n.set_fractal_lacunarity(Some(2.0));

    let mut ridges_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00F1_ABCD);
    ridges_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    ridges_n.set_frequency(Some(0.0032));
    ridges_n.set_fractal_type(Some(FractalType::Ridged));
    ridges_n.set_fractal_octaves(Some(3));

    let mut ridge_long_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00F0_0D11);
    ridge_long_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    ridge_long_n.set_frequency(Some(0.0018));
    ridge_long_n.set_fractal_type(Some(FractalType::Ridged));
    ridge_long_n.set_fractal_octaves(Some(3));

    let mut orient_n = FastNoiseLite::with_seed(cfg.seed ^ 0x009A_BEEF);
    orient_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    orient_n.set_frequency(Some(0.0008));

    let mut strata_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0057_12A7);
    strata_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    strata_n.set_frequency(Some(0.015));
    strata_n.set_fractal_type(Some(FractalType::FBm));
    strata_n.set_fractal_octaves(Some(2));

    let mut coast_n = FastNoiseLite::with_seed(cfg.seed ^ 0x000C_0457);
    coast_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    coast_n.set_frequency(Some(0.012));

    let mut rolling_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0099_77AA);
    rolling_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    rolling_n.set_frequency(Some(0.010));
    rolling_n.set_fractal_type(Some(FractalType::FBm));
    rolling_n.set_fractal_octaves(Some(2));

    let mut seafloor_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0000_51A);
    seafloor_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    seafloor_n.set_frequency(Some(0.020));
    seafloor_n.set_fractal_type(Some(FractalType::FBm));
    seafloor_n.set_fractal_octaves(Some(3));

    let mut patches_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0000_D17);
    patches_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    patches_n.set_frequency(Some(0.008));
    patches_n.set_fractal_type(Some(FractalType::FBm));
    patches_n.set_fractal_octaves(Some(2));

    let mut valley_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00AA_5511);
    valley_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    valley_n.set_frequency(Some(0.0035));
    valley_n.set_fractal_type(Some(FractalType::FBm));
    valley_n.set_fractal_octaves(Some(3));

    // --- Biome-Access (Surfaces etc.) ---
    let surf_bank = SURFACE_BANK.get().expect("SURFACE_BANK missing – call build_biome_table first");
    let acc = BioAcc { table: &biomes, surfaces: surf_bank };

    let sample_height = |wxf: f32, wzf: f32| -> f32 {
        let plains_mask = map01(plains_n.get_noise_2d(wxf, wzf));
        let plains_fac  = smoothstep(
            cfg.plains_threshold - cfg.plains_blend,
            cfg.plains_threshold + cfg.plains_blend,
            plains_mask,
        );
        let warp_amp = leap(cfg.warp_amp_plains, cfg.warp_amp, plains_fac);

        let dx = warp_n.get_noise_2d(wxf,          wzf) * warp_amp;
        let dz = warp_n.get_noise_2d(wxf + 1000.0, wzf - 1000.0) * warp_amp;

        let mut h01 = map01(height_n.get_noise_2d(wxf + dx, wzf + dz));
        let flatten = (1.0 - plains_fac) * cfg.plains_flatten;
        if flatten > 0.0 { h01 = leap(0.5, h01, 1.0 - flatten); }

        let cont     = map01(continent_n.get_noise_2d(wxf * 0.5, wzf * 0.5));
        let ocean_f  = 1.0 - smoothstep(0.35, 0.65, cont);
        let inland_f = 1.0 - ocean_f;

        let macro_pl = smoothstep(0.55, 0.78, map01(macro_plains_n.get_noise_2d(wxf * 0.6, wzf * 0.6)));

        let ridge_gate = 1.0;

        let relief_fac = 1.0;
        let amp0 = leap(cfg.plains_span as f32, cfg.height_span as f32, plains_fac);
        let amp  = amp0 * relief_fac;

        let land_mid = (SEA_LEVEL as f32 - 5.0) + inland_f * 18.0;
        let mut h_land = land_mid + (h01 - 0.5) * amp;

        let rolling = (map01(rolling_n.get_noise_2d(wxf, wzf)) - 0.5) * 8.0;
        h_land += rolling * (0.35 + 0.65 * macro_pl);

        let base_m    = (map01(mount_base_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0;
        let ridge_raw = (map01(ridges_n.get_noise_2d(wxf, wzf))     - 0.5) * 2.0;
        let ridged    = ridge_raw * 0.6 + ridge_raw * ridge_raw * ridge_raw * 0.4;

        let belts = smoothstep(0.50, 0.70, map01(belts_n.get_noise_2d(wxf*0.55, wzf*0.55)));
        let inland_mountain_mask =
            (inland_f * smoothstep((SEA_LEVEL+4) as f32, (SEA_LEVEL+34) as f32, h_land)).clamp(0.0, 1.0);

        let mut mountain_w = (inland_mountain_mask * ridge_gate).clamp(0.0, 1.0);
        mountain_w *= 0.35 + 0.65 * belts;

        let base_amp   = leap(0.0, 14.0, mountain_w * (1.0 - macro_pl * 0.85));
        let ridged_amp = leap(0.0,  8.0, mountain_w * (1.0 - macro_pl * 0.85));
        h_land += base_m * base_amp + ridged * ridged_amp;

        let ang    = orient_n.get_noise_2d(wxf * 0.4, wzf * 0.4) * std::f32::consts::PI;
        let (ux, uz) = rot2(wxf, wzf, ang);
        let ridge_dir = (map01(ridge_long_n.get_noise_2d(ux * 0.06, uz * 0.45)) - 0.5) * 2.0;
        let ridge_dir_sharp = ridge_dir.signum() * ridge_dir.abs().powf(1.35);
        h_land += ridge_dir_sharp * (8.0 * mountain_w);

        let above_sea = (h_land - SEA_LEVEL as f32).max(0.0);
        let cliff_w   = smoothstep(0.45, 0.85, mountain_w);
        let expo      = leap(1.0, 1.55, cliff_w);
        h_land = SEA_LEVEL as f32 + above_sea.powf(expo);

        let strata_jit = (map01(strata_n.get_noise_2d(wxf * 0.8, wzf * 0.8)) - 0.5) * 0.6;
        let step_h     = leap(3.0, 7.0, cliff_w) * (1.0 + strata_jit * 0.25);
        h_land = terrace(h_land, step_h.max(2.5), 0.55 * cliff_w);

        let valley = map01(valley_n.get_noise_2d(wxf, wzf));
        let valley_cut0 = ((valley - 0.65).max(0.0) * 10.0) * inland_f * (1.0 - macro_pl * 0.7);
        let valley_cut  = valley_cut0 * (0.6 + 0.4 * (1.0 - mountain_w));
        h_land -= valley_cut;

        h_land += LAND_LIFT;

        let dist_to_sea = h_land - SEA_LEVEL as f32;
        let ramp        = 1.0 - smoothstep(0.0, 18.0, dist_to_sea.abs());
        let comp_pos    = leap(0.70, 1.0, 1.0 - ramp);
        let comp_neg    = leap(0.60, 1.0, 1.0 - ramp);
        let compress    = if dist_to_sea >= 0.0 { comp_pos } else { comp_neg };
        h_land = SEA_LEVEL as f32 + dist_to_sea * compress;

        let dunes = coast_n.get_noise_2d(wxf, wzf);
        let coast_target = (COAST_MAX as f32 + dunes * 1.6)
            .clamp(SEA_FLOOR_MIN as f32 + 2.0, COAST_MAX as f32);
        let d_to_sea = (h_land - SEA_LEVEL as f32).abs();
        let coast_w  = 1.0 - smoothstep(8.0, 24.0, d_to_sea);
        h_land = leap(h_land, coast_target, coast_w * inland_f);

        let sea_hills  = map01(seafloor_n.get_noise_2d(wxf, wzf));
        let deep_ocean_base = (SEA_FLOOR_MIN as f32 - OCEAN_DEEPEN).max((Y_MIN + 1) as f32);
        let deep_ocean      = deep_ocean_base + sea_hills * 20.0;
        let shelf      = (COAST_MAX as f32 + 2.0 + dunes * 0.8).clamp(56.0, 60.0);
        let deep_mix = smoothstep(0.40, 0.75, ocean_f);
        let mut h_ocean = leap(shelf, deep_ocean.min((SEA_LEVEL-2) as f32), deep_mix);
        if h_ocean < SEA_LEVEL as f32 {
            let d    = (SEA_LEVEL as f32 - h_ocean).min(20.0);
            let r    = 1.0 - smoothstep(0.0, 20.0, d);
            let comp = leap(0.50, 1.0, 1.0 - r);
            h_ocean = SEA_LEVEL as f32 - d * comp + ((map01(rolling_n.get_noise_2d(wxf, wzf)) - 0.5) * 12.0 * 0.25);
        }
        leap(h_land, h_ocean, 1.0 - inland_f)
    };

    for lz in 0..CZ {
        for lx in 0..CX {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            // 1) High fields
            let h_f = sample_height(wxf, wzf).clamp((Y_MIN + 1) as f32, MOUNTAIN_MAX as f32);
            let h_final = h_f.round() as i32;

            // 2) Biome (Voronoi)
            let base_world = chunks_to_world(BASE_CHUNK as f32);
            let warp_amp   = 0.35 * base_world;
            let wx_f = wx as f32 + bwx.get_noise_2d(wx as f32, wz as f32) * warp_amp;
            let wz_f = wz as f32 + bwz.get_noise_2d(wx as f32 + 1000.0, wz as f32 - 1000.0) * warp_amp;

            let bq = biome_query(&acc, cfg.seed as u32, wx_f, wz_f);
            let b0 = bq.primary;
            let b1 = bq.secondary;
            let edge_mix = bq.edge_mix;

            // 3) Layer height
            let h_e = sample_height(wxf + 1.0, wzf);
            let h_n = sample_height(wxf, wzf + 1.0);
            let slope = (h_e - h_f).abs().max((h_n - h_f).abs()).clamp(0.0, 6.0);
            let mut soil_depth = 3.2 - slope * 0.6;
            soil_depth = soil_depth.clamp(1.0, 4.0);
            let bottom_thick: i32 = soil_depth.ceil() as i32;
            let under_thick:  i32 = 2;

            // 4) smooth layers
            let r_seed: u32 = (cfg.seed as u32) ^ 0xABCD_1234;
            let r_top    = h2u(wx, wz, r_seed ^ 0x10);
            let r_bottom = h2u(wx, wz, r_seed ^ 0x20);
            let r_under  = h2u(wx, wz, r_seed ^ 0x30);
            let r_deep   = h2u(wx, wz, r_seed ^ 0x40);

            // Primary
            let (p_top, p_bottom, p_under, p_deep) = if let Some(s) = acc.surface(b0) {
                (
                    name_to_id(pick_weighted_name(&s.top,        r_top   ).unwrap_or("grass_block"), ids),
                    name_to_id(pick_weighted_name(&s.bottom,     r_bottom).unwrap_or("dirt_block" ), ids),
                    name_to_id(pick_weighted_name(&s.under,      r_under ).unwrap_or("stone_block"), ids),
                    name_to_id(pick_weighted_name(&s.deep_under, r_deep  ).unwrap_or("stone_block"), ids),
                )
            } else { (ids.0, ids.1, ids.2, ids.2) };

            // Secondary
            let s_top = if let Some(s) = acc.surface(b1) {
                name_to_id(pick_weighted_name(&s.top, r_top).unwrap_or("grass_block"), ids)
            } else { p_top };

            // Blend
            let mix_max = (edge_mix * 0.45).clamp(0.0, 0.45);
            let t = map01(blend_n.get_noise_2d(wx as f32 * 0.03, wz as f32 * 0.03));
            let mask = smoothstep(0.5 - mix_max, 0.5 + mix_max, t);
            let top_id_blended = if mask < 0.5 { p_top } else { s_top };

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let mut id = if wy == h_final {
                    top_id_blended
                } else if wy == h_final - 1 {
                    let t2 = (t - 0.12).clamp(0.0, 1.0);
                    if t2 > 0.62 { s_top } else { p_bottom }
                } else if wy >= h_final - bottom_thick {
                    p_bottom
                } else if wy >= h_final - bottom_thick - under_thick {
                    p_under
                } else {
                    p_deep
                };

                if h_final < SEA_LEVEL {
                    if wy >= h_final - 3 && id == p_top && p_top == ids.0 {
                        id = ids.3; // sand
                    }
                }

                if id != 0 { c.set(lx, ly, lz, id); }
            }
        }
    }

    c
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
                let n_east = if x + 1 < CX { Some(get(x as isize + 1, y as isize, z as isize)) } else { east_at_opt(y, z) };
                if let Some(nei) = n_east {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::East);
                        b.quad([[wx+s,wy,wz+s],[wx+s,wy,wz],[wx+s,wy+s,wz],[wx+s,wy+s,wz+s]],[1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                    }
                }
                // -X (West)
                let n_west = if x > 0 { Some(get(x as isize - 1, y as isize, z as isize)) } else { west_at_opt(y, z) };
                if let Some(nei) = n_west {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::West);
                        b.quad([[wx,wy,wz],[wx,wy,wz+s],[wx,wy+s,wz+s],[wx,wy+s,wz]],[-1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                    }
                }
                // +Z (South)
                let n_south = if z + 1 < CZ { Some(get(x as isize, y as isize, z as isize + 1)) } else { south_at_opt(y, x) };
                if let Some(nei) = n_south {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::South);
                        b.quad([[wx,wy,wz+s],[wx+s,wy,wz+s],[wx+s,wy+s,wz+s],[wx,wy+s,wz+s]],[0.0,0.0,1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                    }
                }
                // -Z (North)
                let n_north = if z > 0 { Some(get(x as isize, y as isize, z as isize - 1)) } else { north_at_opt(y, x) };
                if let Some(nei) = n_north {
                    if !(nei != 0 && reg.opaque(nei)) {
                        let u = reg.uv(id, Face::North);
                        b.quad([[wx+s,wy,wz],[wx,wy,wz],[wx,wy+s,wz],[wx+s,wy+s,wz]],[0.0,0.0,-1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
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
    ids: (BlockId, BlockId, BlockId, BlockId),
    cfg: WorldGenConfig,
    biomes: BiomeTable,
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
    generate_chunk_async_noise(coord, ids, cfg, biomes).await
}

pub(crate) fn build_biome_table(
    assets: &Assets<BiomeAsset>,
    reg: &BiomeRegistry,
) -> BiomeTable {
    #[inline]
    fn climate_from_temp(t: f32) -> Climate {
        if t <= 0.33 { Climate::Cold }
        else if t >= 0.66 { Climate::Hot }
        else { Climate::Normal }
    }

    let mut list: Vec<BiomeLite>       = Vec::new();
    let mut names: Vec<String>         = Vec::new();
    let mut climates: Vec<Climate>     = Vec::new();
    let mut surfaces_by_name: Vec<SurfaceByName> = Vec::new();

    for (name, handle) in reg.iter() {
        names.push(name.to_string());

        if let Some(b) = assets.get(handle) {
            list.push(BiomeLite {
                temperature: b.temperature.clamp(0.0, 1.0),
                moist:       b.moist.clamp(0.0, 1.0),
                rarity:      b.rarity.clamp(0.0, 1.0),
                generation:  b.generation.clone(),
                climate: climate_from_temp(b.temperature)
            });

            climates.push(climate_from_temp(b.temperature));

            surfaces_by_name.push(from_surface(&b.surface));
        } else {
            list.push(BiomeLite {
                temperature: 0.5,
                moist: 0.5,
                rarity: 0.0,
                generation: Default::default(),
                climate: Climate::Normal,
            });
            climates.push(Climate::Normal);
            surfaces_by_name.push(SurfaceByName::default());
        }
    }

    let _ = SURFACE_BANK.set(surfaces_by_name);

    let mut forbidden: Vec<HashSet<usize>> = vec![HashSet::new(); names.len()];
    for (i, (name, handle)) in reg.iter().enumerate() {
        if let Some(b) = assets.get(handle) {
            for blk in &b.blacklist {
                if let Some(j) = names.iter().position(|n| n == blk) {
                    forbidden[i].insert(j);
                    forbidden[j].insert(i);
                } else {
                    warn!("black_list entry '{}' not found among loaded biomes (referenced by '{}')", blk, name);
                }
            }
        }
    }

    BiomeTable {
        list,
        names,
        forbidden,
        climate: climates,
    }
}

pub fn voronoi_biome_label_at(
    wx: i32,
    wz: i32,
    seed: i32,
    assets: &Assets<BiomeAsset>,
    reg: &BiomeRegistry,
) -> String {

    let table = build_biome_table(assets, reg);
    let surf_bank = SURFACE_BANK
        .get()
        .expect("SURFACE_BANK missing – build_biome_table must run at least once");
    let acc = BioAcc { table: &table, surfaces: surf_bank };

    let base_world = chunks_to_world(BASE_CHUNK as f32);
    let gx = (wx as f32 / base_world).floor() as i32;
    let gz = (wz as f32 / base_world).floor() as i32;

    let west  = (gx > i32::MIN).then(|| pick_biome_for_cell(&acc, gx - 1, gz, seed as u32, None, None));
    let north = (gz > i32::MIN).then(|| pick_biome_for_cell(&acc, gx, gz - 1, seed as u32, west, None));
    let b_idx = pick_biome_for_cell(&acc, gx, gz, seed as u32, west, north);
    let name  = table.names.get(b_idx).cloned().unwrap_or_else(|| "—".to_string());

    let r0 = u_rand01(gx, gz, (seed as u32) ^ 0x51A_0001);
    let label = if r0 < 0.28 {
        "Small"
    } else if r0 < 0.55 {
        "Medium"
    } else if r0 < 0.78 {
        "Large"
    } else if r0 < 0.93 {
        "Very Large"
    } else {
        "Gigantic"
    };

    format!("{name} ({label})")
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
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "block array size mismatch"));
    }

    let mut c = ChunkData::new();
    c.blocks.copy_from_slice(&blocks);
    Ok(c)
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

pub fn can_spawn_mesh(pending_mesh: &PendingMesh) -> bool { pending_mesh.0.len() < MAX_INFLIGHT_MESH }
pub fn can_spawn_gen (pending_gen:  &PendingGen ) -> bool { pending_gen .0.len() < MAX_INFLIGHT_GEN  }

pub fn backlog_contains(backlog: &MeshBacklog, key: (IVec2, usize)) -> bool {
    backlog.0.iter().any(|&k| k == key)
}
pub fn enqueue_mesh(backlog: &mut MeshBacklog, pending: &PendingMesh, key: (IVec2, usize)) {
    if pending.0.contains_key(&key) { return; }
    if backlog_contains(backlog, key) { return; }
    backlog.0.push_back(key);
}


#[inline]
pub fn chunk_to_region_slot(c: IVec2) -> (IVec2, usize) {
    let rx = div_floor(c.x, REGION_SIZE);
    let rz = div_floor(c.y, REGION_SIZE);
    let lx = mod_floor(c.x, REGION_SIZE) as usize;
    let lz = mod_floor(c.y, REGION_SIZE) as usize;
    let idx = lz * (REGION_SIZE as usize) + lx;
    (IVec2::new(rx, rz), idx)
}
#[inline] fn div_floor(a: i32, b: i32) -> i32 { (a as f32 / b as f32).floor() as i32 }
#[inline] fn mod_floor(a: i32, b: i32) -> i32 { a - div_floor(a,b)*b }

#[inline]
pub fn is_waiting(state: &State<AppState>) -> bool {
    matches!(state.get(), AppState::Loading(LoadingStates::BaseGen))
}
#[inline]
pub(crate) fn neighbors_ready(chunk_map: &ChunkMap, c: IVec2) -> bool {
    neighbors4_iter(c).all(|nc| chunk_map.chunks.contains_key(&nc))
}
#[inline]
pub(crate) fn neighbors4_iter(c: IVec2) -> impl Iterator<Item = IVec2> {
    DIR4.into_iter().map(move |d| c + d)
}

#[inline]
fn rot2(x: f32, z: f32, ang: f32) -> (f32, f32) {
    let (ca, sa) = (ang.cos(), ang.sin());
    (x * ca - z * sa, x * sa + z * ca)
}

#[inline]
fn terrace(h: f32, step: f32, k: f32) -> f32 {
    if step <= 0.0001 || k <= 0.0 { return h; }
    let q = h / step;
    let base = q.floor();
    let frac = q - base;
    let eased = frac.powf((1.0 - k).max(0.05));
    (base + eased) * step
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
