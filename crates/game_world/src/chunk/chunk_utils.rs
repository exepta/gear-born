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

// ---------------------------- Size-Gating ----------------------------
#[derive(Clone, Copy, Default)]
struct SizeInfo { freq: f32 }
static SIZE_BANK: OnceLock<Vec<SizeInfo>> = OnceLock::new();

pub const MAX_INFLIGHT_MESH: usize = 32;
pub const MAX_INFLIGHT_GEN:  usize = 32;

pub(crate) const DIR4: [IVec2; 4] = [
    IVec2::new( 1,  0),
    IVec2::new(-1,  0),
    IVec2::new( 0,  1),
    IVec2::new( 0, -1),
];

// ------------------------- Surface (gewichtet) -----------------------
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
    let rf = (r_u32 as f64) / (u32::MAX as f64); // [0,1)
    let target = (rf as f32) * sum;
    let mut acc = 0.0;
    for w in list {
        acc += w.w;
        if target <= acc {
            return Some(w.name.as_str());
        }
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
        "clay_block"  => dirt,   // fallback falls kein eigener Clay vorhanden
        _ => stone,              // unbekannt -> neutraler Fallback
    }
}

// ===============================================================
//                     Chunk Generation (Noise)
// ===============================================================

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId, BlockId), // (grass, dirt, stone, sand)
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

    let (grass, dirt, stone, sand) = ids;
    let mut c = ChunkData::new();

    // ---- base noises ----
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

    // --- BIOMES (climate) ---
    let mut temp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0B10_0001);
    temp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    temp_n.set_frequency(Some(BIOME_TEMP_FREQ));

    let mut moist_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0B10_0002);
    moist_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    moist_n.set_frequency(Some(BIOME_MOIST_FREQ));

    // --- Size-Gate Hilfsfunktion (Seed als 1. Arg) ---
    fn size_gate_at(cfg_seed: i32, idx: usize, x: f32, z: f32) -> f32 {
        use fastnoise_lite::{FastNoiseLite, NoiseType};
        if let Some(info) = SIZE_BANK.get().and_then(|v| v.get(idx)) {
            let salt: i32 = 0x512E_0000;
            let seed_i32 = cfg_seed ^ salt.wrapping_add(idx as i32);

            let mut n = FastNoiseLite::with_seed(seed_i32);
            n.set_noise_type(Some(NoiseType::OpenSimplex2));
            n.set_frequency(Some(info.freq.max(0.0003)));
            0.1 + 0.9 * smoothstep(0.25, 0.75, map01(n.get_noise_2d(x, z)))
        } else {
            1.0
        }
    }

    // ---------- biome mixing mit Gate ----------
    #[inline]
    fn blend_biome_gen_climate(
        t: f32, m: f32, table: &BiomeTable
    ) -> (f32, f32, f32, f32) {
        if table.list.is_empty() { return (0.0, 1.0, 1.0, 0.0); }
        let sigma2 = (BIOME_BLEND_SIGMA * BIOME_BLEND_SIGMA).max(1e-6);

        let mut items: Vec<(usize, f32)> = table.list.iter().enumerate().map(|(i,b)| {
            let dt = t - b.temperature;
            let dm = m - b.moist;
            let d2 = dt*dt + dm*dm;
            let w  = (-d2 / (2.0 * sigma2)).exp() * (0.0001 + b.rarity.clamp(0.0, 1.0));
            (i, w)
        }).collect();
        items.sort_by(|a,b| b.1.partial_cmp(&a.1).unwrap());

        let k = BIOME_TOP_K.min(items.len());
        let mut sum = 0.0;
        for i in 0..k { sum += items[i].1; }
        if sum <= 0.0 { return (0.0, 1.0, 1.0, 0.0); }

        let w0 = items[0].1;
        let w1 = if k > 1 { items[1].1 } else { 0.0 };
        let dominance = ((w0 - w1) / sum).clamp(0.0, 1.0);

        let mut h_off = 0.0;
        let mut ridge = 0.0;
        let mut roll  = 0.0;

        for i in 0..k {
            let (idx, w) = items[i];
            let bw = w / sum;
            let g = &table.list[idx].generation;
            h_off += bw * g.height_offset;
            ridge += bw * g.ridge_amp_factor.max(0.0);
            roll  += bw * g.rolling_amp_factor.max(0.0);
        }
        (h_off.clamp(-120.0, 180.0), ridge.clamp(0.5, 2.5), roll.clamp(0.5, 2.0), dominance)
    }

    #[inline]
    fn biome_candidates_sized(
        t: f32, m: f32, x: f32, z: f32, table: &BiomeTable, cfg_seed: i32, top_k: usize
    ) -> Vec<(usize, f32)> {
        if table.list.is_empty() { return vec![(0, 0.0)]; }
        let sigma2 = (BIOME_BLEND_SIGMA * BIOME_BLEND_SIGMA).max(1e-6);

        let mut items: Vec<(usize, f32)> = table.list.iter().enumerate().map(|(i, b)| {
            let dt = t - b.temperature;
            let dm = m - b.moist;
            let d2 = dt*dt + dm*dm;
            let base = (-d2 / (2.0 * sigma2)).exp() * (0.0001 + b.rarity.clamp(0.0, 1.0));
            let gate = size_gate_at(cfg_seed, i, x, z);
            (i, base * gate)
        }).collect();

        items.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        let k = top_k.min(items.len());
        items.truncate(k);
        items
    }

    #[inline]
    pub fn col_rand_range(x: i32, z: i32, seed: u32, lo: u32, hi: u32) -> u32 {
        let r = col_rand_u32(x, z, seed);
        let range = hi.saturating_sub(lo);
        lo + (r % (range + 1))
    }

    // ---------- height sampler ----------
    let sample_height = |wxf: f32, wzf: f32| -> f32 {
        // plain mask & warp
        let plains_mask = map01(plains_n.get_noise_2d(wxf, wzf));
        let plains_fac  = smoothstep(
            cfg.plains_threshold - cfg.plains_blend,
            cfg.plains_threshold + cfg.plains_blend,
            plains_mask,
        );
        let warp_amp = leap(cfg.warp_amp_plains, cfg.warp_amp, plains_fac);

        // domain warp
        let dx = warp_n.get_noise_2d(wxf,          wzf) * warp_amp;
        let dz = warp_n.get_noise_2d(wxf + 1000.0, wzf - 1000.0) * warp_amp;

        // base height
        let mut h01 = map01(height_n.get_noise_2d(wxf + dx, wzf + dz));
        let flatten = (1.0 - plains_fac) * cfg.plains_flatten;
        if flatten > 0.0 { h01 = leap(0.5, h01, 1.0 - flatten); }

        // land/ocean split
        let cont     = map01(continent_n.get_noise_2d(wxf * 0.5, wzf * 0.5));
        let ocean_f  = 1.0 - smoothstep(0.35, 0.65, cont);
        let inland_f = 1.0 - ocean_f;

        let macro_pl = smoothstep(0.55, 0.78, map01(macro_plains_n.get_noise_2d(wxf * 0.6, wzf * 0.6)));

        // biome mix (Größe beeinflusst NUR Auswahl, nicht Relief)
        let t = map01(temp_n.get_noise_2d(wxf, wzf));
        let m = map01(moist_n.get_noise_2d(wxf, wzf));
        let (h_off, ridge_fac, roll_fac, dom) =
            blend_biome_gen_climate(t, m, &biomes);
        let center_w = dom.powf(1.5);

        let ridge_gate = smoothstep(0.8, 1.3, ridge_fac);

        // amplitude vs plains + relief
        let relief_fac = (0.7 * roll_fac + 0.3 * ridge_fac).clamp(0.25, 1.0);
        let amp0 = leap(cfg.plains_span as f32, cfg.height_span as f32, plains_fac);
        let amp  = amp0 * relief_fac;

        // land baseline
        let land_mid = (SEA_LEVEL as f32 - 5.0) + inland_f * 18.0;
        let mut h_land = land_mid + (h01 - 0.5) * amp;

        // rolling hills
        let rolling = (map01(rolling_n.get_noise_2d(wxf, wzf)) - 0.5) * 8.0;
        h_land += rolling * (0.35 + 0.65 * macro_pl) * roll_fac;

        // mountains (basal + ridged)
        let base_m    = (map01(mount_base_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0;
        let ridge_raw = (map01(ridges_n.get_noise_2d(wxf, wzf))     - 0.5) * 2.0;
        let ridged    = ridge_raw * 0.6 + ridge_raw * ridge_raw * ridge_raw * 0.4;

        let belts = smoothstep(0.50, 0.70, map01(belts_n.get_noise_2d(wxf*0.55, wzf*0.55)));
        let inland_mountain_mask =
            (inland_f * smoothstep((SEA_LEVEL+4) as f32, (SEA_LEVEL+34) as f32, h_land)).clamp(0.0, 1.0);

        let mut mountain_w = (inland_mountain_mask * ridge_gate * center_w).clamp(0.0, 1.0);
        mountain_w *= 0.35 + 0.65 * belts;

        let base_amp   = leap(0.0, 14.0, mountain_w * (1.0 - macro_pl * 0.85)) * roll_fac;
        let ridged_amp = leap(0.0,  8.0, mountain_w * (1.0 - macro_pl * 0.85)) * ridge_fac;
        h_land += base_m * base_amp + ridged * ridged_amp;

        // gerichtete Rücken
        let ang    = orient_n.get_noise_2d(wxf * 0.4, wzf * 0.4) * std::f32::consts::PI;
        let (ux, uz) = rot2(wxf, wzf, ang);
        let ridge_dir = (map01(ridge_long_n.get_noise_2d(ux * 0.06, uz * 0.45)) - 0.5) * 2.0;
        let ridge_dir_sharp = ridge_dir.signum() * ridge_dir.abs().powf(1.35);
        h_land += ridge_dir_sharp * (8.0 * mountain_w);

        // highland shaping
        let above_sea = (h_land - SEA_LEVEL as f32).max(0.0);
        let cliff_w   = smoothstep(0.45, 0.85, mountain_w);
        let expo      = leap(1.0, 1.55, cliff_w);
        h_land = SEA_LEVEL as f32 + above_sea.powf(expo);

        let strata_jit = (map01(strata_n.get_noise_2d(wxf * 0.8, wzf * 0.8)) - 0.5) * 0.6;
        let step_h     = leap(3.0, 7.0, cliff_w) * (1.0 + strata_jit * 0.25);
        h_land = terrace(h_land, step_h.max(2.5), 0.55 * cliff_w);

        // valleys
        let valley = map01(valley_n.get_noise_2d(wxf, wzf));
        let valley_cut0 = ((valley - 0.65).max(0.0) * 10.0) * inland_f * (1.0 - macro_pl * 0.7);
        let valley_cut  = valley_cut0 * (0.6 + 0.4 * (1.0 - mountain_w));
        h_land -= valley_cut;

        // biome offsets (keine Größenänderung!)
        h_land += LAND_LIFT;
        let coastal_scale = 0.25 + 0.75 * inland_f;
        let macro_scale   = 0.40 + 0.60 * macro_pl;
        let edge_scale    = 0.35 + 0.65 * center_w;
        h_land += h_off * coastal_scale * macro_scale * edge_scale;

        // near sea compression
        let dist_to_sea = h_land - SEA_LEVEL as f32;
        let ramp        = 1.0 - smoothstep(0.0, 18.0, dist_to_sea.abs());
        let comp_pos    = leap(0.70, 1.0, 1.0 - ramp);
        let comp_neg    = leap(leap(0.50, 0.70, mountain_w), 1.0, 1.0 - ramp);
        let compress    = if dist_to_sea >= 0.0 { comp_pos } else { comp_neg };
        h_land = SEA_LEVEL as f32 + dist_to_sea * compress;

        // coast shaping
        let dunes = coast_n.get_noise_2d(wxf, wzf);
        let coast_target = (COAST_MAX as f32 + dunes * 1.6)
            .clamp(SEA_FLOOR_MIN as f32 + 2.0, COAST_MAX as f32);
        let d_to_sea = (h_land - SEA_LEVEL as f32).abs();
        let coast_w  = 1.0 - smoothstep(8.0, 24.0, d_to_sea);
        h_land = leap(h_land, coast_target, coast_w * inland_f);

        // ocean shaping
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

        leap(h_land, h_ocean, ocean_f)
    };

    // --------- block fill (mit Blacklist-Filter & nahtsicherer Prognose) ----------
    let mut biome_idx_grid = vec![0usize; CX * CZ];
    let at = |x: usize, z: usize| -> usize { z * CX + x };

    for lz in 0..CZ {         // z-outer, x-inner => Nord/West sind deterministisch gesetzt
        for lx in 0..CX {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            let h_f = sample_height(wxf, wzf).clamp((Y_MIN + 1) as f32, MOUNTAIN_MAX as f32);
            let h_final = h_f.round() as i32;

            // Spalten-Parameter
            let h_e = sample_height(wxf + 1.0, wzf);
            let h_n = sample_height(wxf, wzf + 1.0);
            let slope = (h_e - h_f).abs().max((h_n - h_f).abs());
            let macro_pl = smoothstep(0.55, 0.78, map01(macro_plains_n.get_noise_2d(wxf * 0.6, wzf * 0.6)));
            let mut soil_depth = 2.6 + 1.2 * macro_pl;
            let alt            = (h_f - 90.0).max(0.0) / 50.0;
            soil_depth -= slope * 1.1;
            soil_depth -= alt.clamp(0.0, 1.0) * 1.3;
            soil_depth = soil_depth.clamp(0.0, 4.0);

            // Biomekandidaten (Top-K)
            let t_here = map01(temp_n.get_noise_2d(wxf, wzf));
            let m_here = map01(moist_n.get_noise_2d(wxf, wzf));
            const TOP_K: usize = 6;
            let candidates = biome_candidates_sized(t_here, m_here, wxf, wzf, &biomes, cfg.seed, TOP_K);

            // Nachbarn: lokal (innerhalb des Chunks) ...
            let west_local  = (lx > 0).then(|| biome_idx_grid[at(lx - 1, lz)]);
            let north_local = (lz > 0).then(|| biome_idx_grid[at(lx, lz - 1)]);

            // ... und nahtsichere Prognose für fehlende Nachbarn (andere Chunks)
            let west_pred = if lx == 0 {
                let wx_west = wxf - 1.0;
                let t_w = map01(temp_n.get_noise_2d(wx_west, wzf));
                let m_w = map01(moist_n.get_noise_2d(wx_west, wzf));
                let cand_w = biome_candidates_sized(t_w, m_w, wx_west, wzf, &biomes, cfg.seed, 1);
                Some(cand_w[0].0)
            } else { None };

            let north_pred = if lz == 0 {
                let wz_north = wzf - 1.0;
                let t_n = map01(temp_n.get_noise_2d(wxf, wz_north));
                let m_n = map01(moist_n.get_noise_2d(wxf, wz_north));
                let cand_n = biome_candidates_sized(t_n, m_n, wxf, wz_north, &biomes, cfg.seed, 1);
                Some(cand_n[0].0)
            } else { None };

            let west  = west_local.or(west_pred);
            let north = north_local.or(north_pred);

            // Kandidat wählen: erster, der keine verbotene Nachbarschaft verletzt
            let mut chosen_idx = candidates[0].0; // Fallback: bester Kandidat
            'pick: for &(cand_i, _w) in &candidates {
                if let Some(wi) = west  { if biomes.forbids(cand_i, wi) { continue; } }
                if let Some(ni) = north { if biomes.forbids(cand_i, ni) { continue; } }
                chosen_idx = cand_i;
                break 'pick;
            }
            biome_idx_grid[at(lx, lz)] = chosen_idx;

            let surf_bank = SURFACE_BANK.get();
            let surf_opt  = surf_bank.and_then(|v| v.get(chosen_idx));

            // deterministische Zufallswerte pro Schichttyp
            let r_seed: u32 = (cfg.seed as u32) ^ 0xABCD_1234;
            let r_top    = col_rand_u32(wx, wz, r_seed ^ 0x10);
            let r_bottom = col_rand_u32(wx, wz, r_seed ^ 0x20);
            let r_under  = col_rand_u32(wx, wz, r_seed ^ 0x30);
            let r_deep   = col_rand_u32(wx, wz, r_seed ^ 0x40);

            // IDs aus Surface (oder Fallback)
            let (top_id, bottom_id, under_id, deep_id) = if let Some(s) = surf_opt {
                let top_name    = pick_weighted_name(&s.top,        r_top   ).unwrap_or("grass_block");
                let bottom_name = pick_weighted_name(&s.bottom,     r_bottom).unwrap_or("dirt_block");
                let under_name  = pick_weighted_name(&s.under,      r_under ).unwrap_or("stone_block");
                let deep_name   = pick_weighted_name(&s.deep_under, r_deep  ).unwrap_or("stone_block");
                (
                    name_to_id(top_name, ids),
                    name_to_id(bottom_name, ids),
                    name_to_id(under_name, ids),
                    name_to_id(deep_name, ids),
                )
            } else {
                (grass, dirt, stone, stone)
            };

            // Schichtdicken
            let bottom_thick: i32 = soil_depth.ceil() as i32;                  // 0..4
            let under_thick:  i32 = 2 + (col_rand_range(wx, wz, r_seed ^ 0x55, 0, 2) as i32); // 2..4

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let mut id = if wy == h_final {
                    top_id
                } else if wy >= h_final - bottom_thick {
                    bottom_id
                } else if wy >= h_final - bottom_thick - under_thick {
                    under_id
                } else {
                    deep_id
                };

                // simple Wasser-/Ufer-Anpassung als Fallback
                if h_final < SEA_LEVEL {
                    if wy >= h_final - 3 && (id == top_id) && top_id == grass {
                        id = sand;
                    }
                }

                if id != 0 { c.set(lx, ly, lz, id); }
            }
        }
    }

    c
}


// ===============================================================
//                       Meshing (per Subchunk)
// ===============================================================
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

// ===============================================================
//                       Save / Load
// ===============================================================
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

// ===============================================================
//                       Border Snapshot
// ===============================================================
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

// ===============================================================
//               Scheduling / Mesh lifecycle helpers
// ===============================================================
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

// ===============================================================
//                         Encoding
// ===============================================================
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

// ===============================================================
//                       Biome table helper
// ===============================================================
pub(crate) fn build_biome_table(assets: &Assets<BiomeAsset>, reg: &BiomeRegistry) -> BiomeTable {
    let mut list = Vec::new();
    let mut surfaces: Vec<SurfaceByName> = Vec::new();
    let mut sizes: Vec<SizeInfo> = Vec::new();

    let mut names: Vec<String> = Vec::new();

    for (name, handle) in reg.iter() {
        names.push(name.to_string());

        if let Some(b) = assets.get(handle) {
            list.push(BiomeLite {
                temperature: b.temperature.clamp(0.0, 1.0),
                moist:       b.moist.clamp(0.0, 1.0),
                rarity:      b.rarity.clamp(0.0, 1.0),
                generation:  b.generation.clone(),
            });
            surfaces.push(from_surface(&b.surface));

            let freq = sizes_to_frequency(&b.sizes);
            sizes.push(SizeInfo { freq });
        } else {
            warn!("Biome handle not yet loaded");
            list.push(BiomeLite {
                temperature: 0.5, moist: 0.5, rarity: 0.0, generation: Default::default()
            });
            surfaces.push(SurfaceByName::default());
            sizes.push(SizeInfo { freq: sizes_to_frequency(&[]) });
        }
    }

    let mut index_by_name: HashMap<String, usize> = HashMap::new();
    for (i, n) in names.iter().enumerate() {
        index_by_name.insert(n.clone(), i);
    }

    let mut forbidden: Vec<HashSet<usize>> = vec![HashSet::new(); list.len()];

    for (i, (_name, handle)) in reg.iter().enumerate() {
        if let Some(b) = assets.get(handle) {
            for other in &b.blacklist {
                if let Some(&j) = index_by_name.get(other) {
                    if i != j {
                        forbidden[i].insert(j);
                        forbidden[j].insert(i);
                    }
                } else {
                    warn!("Biome '{}' blacklists unknown biome '{}'", b.name, other);
                }
            }
        }
    }

    let _ = SURFACE_BANK.set(surfaces);
    let _ = SIZE_BANK.set(sizes);

    BiomeTable { list, forbidden }
}

fn size_len_chunks(s: &BiomeSize) -> f32 {
    match s {
        BiomeSize::Small    => ((2.0   +  8.0)  * 0.5f32).sqrt(),   // ~2.236
        BiomeSize::Medium   => ((18.0  + 22.0)  * 0.5f32).sqrt(),   // ~4.472
        BiomeSize::Large    => ((54.0  + 60.0)  * 0.5f32).sqrt(),   // ~7.549
        BiomeSize::Gigantic => ((120.0 + 128.0) * 0.5f32).sqrt(),   // ~11.136
    }
}

fn sizes_to_frequency(sizes: &[BiomeSize]) -> f32 {
    // Mittelwert der Kantenlängen in Chunk-Einheiten -> Frequenz
    let len_chunks = if sizes.is_empty() {
        size_len_chunks(&BiomeSize::Medium)
    } else {
        let sum: f32 = sizes.iter().map(size_len_chunks).sum();
        sum / (sizes.len() as f32)
    };
    1.0 / (len_chunks * (CX as f32))
}

// ===============================================================
//                         Small utils
// ===============================================================
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
