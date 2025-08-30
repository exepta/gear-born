use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use game_core::configuration::WorldGenConfig;
use game_core::world::block::{BlockId, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex};
use game_core::world::chunk_dim::*;
use game_core::world::save::*;
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::HashMap;
use std::path::PathBuf;

pub const MAX_INFLIGHT_MESH: usize = 64;
pub const MAX_INFLIGHT_GEN:  usize = 32;

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId, BlockId), // (grass, dirt, stone, sand)
    cfg: WorldGenConfig,
) -> ChunkData {
    use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};

    const SEA_LEVEL:      i32 = 62;
    const COAST_MAX:      i32 = 58;
    const SEA_FLOOR_MIN:  i32 = 5;
    const MOUNTAIN_MAX:   i32 = 170;

    let (grass, dirt, stone, sand) = ids;
    let mut c = ChunkData::new();

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
    mount_base_n.set_frequency(Some(0.0028));
    mount_base_n.set_fractal_type(Some(FractalType::FBm));
    mount_base_n.set_fractal_octaves(Some(4));
    mount_base_n.set_fractal_gain(Some(0.5));
    mount_base_n.set_fractal_lacunarity(Some(2.0));

    let mut ridges_n = FastNoiseLite::with_seed(cfg.seed ^ 0x00F1_ABCD);
    ridges_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    ridges_n.set_frequency(Some(0.0045));
    ridges_n.set_fractal_type(Some(FractalType::Ridged));
    ridges_n.set_fractal_octaves(Some(3));

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

    // --- NEU: Rivers ---
    let mut river_n = FastNoiseLite::with_seed(cfg.seed ^ 0x1357_9BDF);
    river_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_n.set_frequency(Some(0.0016));

    let mut river_warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x2468_ACF1);
    river_warp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    river_warp_n.set_frequency(Some(0.015));

    // ---------- Helpers ----------
    #[inline] fn map01(x: f32) -> f32 { (x * 0.5) + 0.5 }
    #[inline]
    fn smoothstep(a: f32, b: f32, x: f32) -> f32 {
        let t = ((x - a) / (b - a)).clamp(0.0, 1.0);
        t * t * (3.0 - 2.0 * t)
    }
    #[inline] fn lerp(a: f32, b: f32, t: f32) -> f32 { a + (b - a) * t }
    #[inline]
    fn col_rand_range(x: i32, z: i32, seed: u32, lo: u32, hi: u32) -> u32 {
        let r = col_rand_u32(x, z, seed);
        lo + (r % (hi - lo + 1))
    }

    let river_strength_at = |wxf: f32, wzf: f32, h_local: f32, inland_f: f32| -> f32 {
        let rx = wxf + river_warp_n.get_noise_2d(wxf * 0.015, wzf * 0.015) * 24.0;
        let rz = wzf + river_warp_n.get_noise_2d(wxf * 0.015 + 1000.0, wzf * 0.015 - 1000.0) * 24.0;
        let r = river_n.get_noise_2d(rx, rz).abs();
        let core = 1.0 - smoothstep(0.02, 0.10, r);

        let elev_above = (h_local - SEA_LEVEL as f32).max(0.0);
        let floodplain = 1.0 - smoothstep(2.0, 8.0, elev_above);

        let coast_ok = 0.30 + 0.70 * inland_f;

        (core * floodplain * coast_ok).clamp(0.0, 1.0)
    };

    let sample_height = |wxf: f32, wzf: f32| -> f32 {
        let plains_mask = map01(plains_n.get_noise_2d(wxf, wzf));
        let plains_fac  = smoothstep(
            cfg.plains_threshold - cfg.plains_blend,
            cfg.plains_threshold + cfg.plains_blend,
            plains_mask,
        );
        let amp      = lerp(cfg.plains_span as f32, cfg.height_span as f32, plains_fac);
        let warp_amp = lerp(cfg.warp_amp_plains,              cfg.warp_amp,  plains_fac);

        // Domain warp
        let dx = warp_n.get_noise_2d(wxf,          wzf) * warp_amp;
        let dz = warp_n.get_noise_2d(wxf + 1000.0, wzf - 1000.0) * warp_amp;

        let mut h01 = map01(height_n.get_noise_2d(wxf + dx, wzf + dz));
        let flatten = (1.0 - plains_fac) * cfg.plains_flatten;
        if flatten > 0.0 { h01 = lerp(0.5, h01, 1.0 - flatten); }

        let cont     = map01(continent_n.get_noise_2d(wxf * 0.5, wzf * 0.5));
        let ocean_f  = 1.0 - smoothstep(0.35, 0.65, cont);
        let inland_f = 1.0 - ocean_f;

        let macro_pl = smoothstep(0.55, 0.78, map01(macro_plains_n.get_noise_2d(wxf * 0.6, wzf * 0.6)));

        let land_mid = (SEA_LEVEL as f32 - 8.0) + inland_f * 60.0;
        let mut h_land = land_mid + (h01 - 0.5) * amp;

        let rolling = (map01(rolling_n.get_noise_2d(wxf, wzf)) - 0.5) * 12.0;
        h_land += rolling * (0.5 + 0.5 * macro_pl);

        let base_m = (map01(mount_base_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0;
        let ridge_raw = (map01(ridges_n.get_noise_2d(wxf, wzf)) - 0.5) * 2.0;
        let ridged = ridge_raw * 0.6 + ridge_raw * ridge_raw * ridge_raw * 0.4;
        let inland_mountain_mask =
            (inland_f * smoothstep((SEA_LEVEL+4) as f32, (SEA_LEVEL+34) as f32, h_land)).clamp(0.0, 1.0);
        let base_amp   = lerp(0.0, 48.0, inland_mountain_mask * (1.0 - macro_pl * 0.85));
        let ridged_amp = lerp(0.0, 28.0, inland_mountain_mask * (1.0 - macro_pl * 0.85));
        h_land += base_m * base_amp + ridged * ridged_amp;

        // Täler
        let valley = map01(valley_n.get_noise_2d(wxf, wzf));
        let valley_cut = ((valley - 0.65).max(0.0) * 18.0) * inland_f * (1.0 - macro_pl * 0.7);
        h_land -= valley_cut;

        let r_strength = river_strength_at(wxf, wzf, h_land, inland_f);
        if r_strength > 0.0 {
            let target = (SEA_LEVEL - 1) as f32;
            h_land = lerp(h_land, target, (r_strength * 0.85).clamp(0.0, 0.85));
            let above_sea = (h_land - SEA_LEVEL as f32).max(0.0);
            let width_f   = (1.0 - (above_sea / 28.0)).clamp(0.0, 1.0);
            h_land -= r_strength * (1.0 + 2.0 * width_f);
        }

        let dist_to_sea = h_land - SEA_LEVEL as f32;
        let ramp        = 1.0 - smoothstep(0.0, 18.0, dist_to_sea.abs());
        let compress    = lerp(0.40, 1.0, 1.0 - ramp);
        h_land = SEA_LEVEL as f32 + dist_to_sea * compress;

        let dunes = coast_n.get_noise_2d(wxf, wzf);
        let coast_target = (COAST_MAX as f32 + dunes * 1.6)
            .clamp(SEA_FLOOR_MIN as f32 + 2.0, COAST_MAX as f32);
        let d_to_sea = (h_land - SEA_LEVEL as f32).abs();
        let coast_w  = 1.0 - smoothstep(8.0, 24.0, d_to_sea);
        h_land = lerp(h_land, coast_target, coast_w * inland_f);

        let r_strength = river_strength_at(wxf, wzf, h_land, inland_f);
        if r_strength > 0.0 {
            let target = (SEA_LEVEL - 1) as f32;
            let pull = ((h_land - target).max(0.0)) * (r_strength * 0.85).clamp(0.0, 0.85);
            h_land = (h_land - pull) - (0.8 * r_strength);
            let core = (1.0 - smoothstep(0.0, 0.04,
                                         river_n.get_noise_2d(
                                             wxf + river_warp_n.get_noise_2d(wxf*0.015, wzf*0.015)*24.0,
                                             wzf + river_warp_n.get_noise_2d(wxf*0.015+1000.0, wzf*0.015-1000.0)*24.0
                                         ).abs())).clamp(0.0, 1.0);
            if core > 0.0 && h_land > (SEA_LEVEL as f32 - 0.5) {
                h_land = SEA_LEVEL as f32 - 0.5;
            }
        }

        let sea_hills  = map01(seafloor_n.get_noise_2d(wxf, wzf));
        let deep_ocean = SEA_FLOOR_MIN as f32 + sea_hills * 20.0;
        let shelf      = (COAST_MAX as f32 + 2.0 + dunes * 0.8).clamp(56.0, 60.0);
        let deep_mix   = smoothstep(0.40, 0.75, 1.0 - inland_f);
        let mut h_ocean = lerp(shelf, deep_ocean.min((SEA_LEVEL-2) as f32), deep_mix);
        if h_ocean < SEA_LEVEL as f32 {
            let d    = (SEA_LEVEL as f32 - h_ocean).min(20.0);
            let r    = 1.0 - smoothstep(0.0, 20.0, d);
            let comp = lerp(0.50, 1.0, 1.0 - r);
            h_ocean = SEA_LEVEL as f32 - d * comp + ((map01(rolling_n.get_noise_2d(wxf, wzf)) - 0.5) * 12.0 * 0.25);
        }

        lerp(h_land, h_ocean, 1.0 - inland_f)
    };

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx  = coord.x * CX as i32 + lx as i32;
            let wz  = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            let h_f = sample_height(wxf, wzf)
                .clamp((Y_MIN + 1) as f32, MOUNTAIN_MAX as f32);
            let h_final = h_f.round() as i32;

            let cont     = map01(continent_n.get_noise_2d(wxf * 0.5, wzf * 0.5));
            let inland_f = 1.0 - (1.0 - smoothstep(0.35, 0.65, cont));
            let r_here   = river_strength_at(wxf, wzf, h_f, inland_f);

            let h_e = sample_height(wxf + 1.0, wzf);
            let h_n = sample_height(wxf, wzf + 1.0);
            let slope = (h_e - h_f).abs().max((h_n - h_f).abs());
            let alt   = (h_f - 90.0).max(0.0) / 50.0;

            let macro_pl = smoothstep(0.55, 0.78, map01(macro_plains_n.get_noise_2d(wxf * 0.6, wzf * 0.6)));
            let mut soil_depth = 2.6 + 1.2 * macro_pl;
            soil_depth -= slope * 1.1;
            soil_depth -= alt.clamp(0.0, 1.0) * 1.3;
            soil_depth = soil_depth.clamp(0.0, 4.0);

            let near_coast = ((h_final as f32) - (SEA_LEVEL as f32)).abs() <= 4.0;
            let coast_mix  = map01(coast_n.get_noise_2d(wxf * 2.0, wzf * 2.0)) > 0.62;
            let patch_val  = map01(patches_n.get_noise_2d(wxf, wzf));
            let seabed_is_dirt = (h_final < SEA_LEVEL) && (patch_val > 0.75);

            let r_seed: u32 = (cfg.seed as u32) ^ 0xABCD_1234;
            let mut dirt_under_grass = col_rand_range(wx, wz, r_seed ^ 0x11, 2, 3) as i32;
            let mut sand_under_sand  = col_rand_range(wx, wz, r_seed ^ 0x22, 2, 5) as i32;
            let     dirt_after_sand  = col_rand_range(wx, wz, r_seed ^ 0x33, 1, 2) as i32;

            dirt_under_grass = dirt_under_grass.min((soil_depth.floor() as i32).max(0));
            sand_under_sand  = sand_under_sand .min((soil_depth.ceil()  as i32).max(0));

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let id = if h_final >= SEA_LEVEL {
                    // LAND
                    if r_here > 0.18 {
                        if wy == h_final { sand }
                        else if wy >= h_final - 2 { sand }
                        else if wy >= h_final - 3 { dirt }
                        else { stone }
                    } else if near_coast && coast_mix {
                        if wy == h_final { sand }
                        else if wy >= h_final - sand_under_sand { sand }
                        else if wy >= h_final - sand_under_sand - dirt_after_sand { dirt }
                        else { stone }
                    } else {
                        if wy == h_final { grass }
                        else if wy >= h_final - dirt_under_grass { dirt }
                        else { stone }
                    }
                } else {
                    if r_here > 0.18 {
                        if wy == h_final { sand }
                        else if wy >= h_final - 2 { sand }
                        else { stone }
                    } else if seabed_is_dirt {
                        if wy == h_final { dirt }
                        else if wy >= h_final - 1 { dirt }
                        else { stone }
                    } else {
                        if wy == h_final { sand }
                        else if wy >= h_final - 3 { sand }
                        else { stone }
                    }
                };

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
    ids: (BlockId, BlockId, BlockId, BlockId),
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
    generate_chunk_async_noise(coord, ids, cfg).await
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
