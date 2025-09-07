use crate::chunk::chunk_utils::{col_rand_u32, map01};
use bevy::prelude::*;
use fastnoise_lite::*;
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::block::{BlockId, BlockRegistry};
use game_core::world::chunk::{ChunkData, SEA_LEVEL};
use game_core::world::chunk_dim::{CX, CY, CZ, Y_MIN};

pub(crate) async fn generate_chunk_async_biome(
    coord: IVec2,
    reg: &BlockRegistry,
    cfg_seed: i32,
    biomes: &BiomeRegistry,
) -> ChunkData {

    let biome = biomes.get("Ocean")
        .or_else(|| biomes.get("ocean"))
        .expect("Ocean biome must be registered");

    let sea_floor_amp  = biome.settings.seafloor_amp.unwrap_or(12.0);
    let sea_floor_freq = biome.settings.seafloor_freq.unwrap_or(0.012);

    let mut seafloor_n = FastNoiseLite::with_seed(cfg_seed);
    seafloor_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    seafloor_n.set_frequency(Some(sea_floor_freq));
    seafloor_n.set_fractal_type(Some(FractalType::FBm));
    seafloor_n.set_fractal_octaves(Some(3));
    seafloor_n.set_fractal_gain(Some(0.5));
    seafloor_n.set_fractal_lacunarity(Some(2.0));

    let pick_seed: u32 = (cfg_seed as u32) ^ 0x0CE4_11CE;

    let sample_height_ocean = |wxf: f32, wzf: f32| -> f32 {
        let n = map01(seafloor_n.get_noise_2d(wxf * 0.9, wzf * 0.9));
        let undulation = (n - 0.5) * sea_floor_amp;
        let base = SEA_LEVEL as f32 + biome.settings.height_offset;
        clamp_world_y(base + undulation).min((SEA_LEVEL - 2) as f32)
    };

    let mut chunk = ChunkData::new();

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32;
            let wzf = wz as f32;

            let h_f = sample_height_ocean(wxf, wzf);
            let h_final = h_f.round() as i32;

            let top_name        = pick(&biome.surface.top,        wx, wz, pick_seed ^ 0x11);
            let bottom_name     = pick(&biome.surface.bottom,     wx, wz, pick_seed ^ 0x22);
            let sea_floor_name  = pick(&biome.surface.sea_floor,  wx, wz, pick_seed ^ 0x33);
            let upper_zero_name = pick(&biome.surface.upper_zero, wx, wz, pick_seed ^ 0x44);
            let under_zero_name = pick(&biome.surface.under_zero, wx, wz, pick_seed ^ 0x55);

            let id_top        = reg.id_or_air(top_name);
            let id_bottom     = reg.id_or_air(bottom_name);
            let id_sea_floor  = reg.id_or_air(sea_floor_name);
            let id_upper_zero = reg.id_or_air(upper_zero_name);
            let id_under_zero = reg.id_or_air(under_zero_name);

            let seabed_cap  = 2;
            let subsoil_cap = 4;
            let beach_cap   = 3;

            for ly in 0..CY {
                let wy = Y_MIN + ly as i32;
                if wy > h_final { break; }

                let underwater = h_final < SEA_LEVEL;

                let id: BlockId = if underwater {
                    if wy >= h_final - (seabed_cap - 1) {
                        id_sea_floor
                    } else if wy >= h_final - (seabed_cap + subsoil_cap - 1) {
                        id_under_zero
                    } else {
                        id_upper_zero
                    }
                } else {
                    if wy == h_final {
                        id_top
                    } else if wy >= h_final - beach_cap {
                        id_bottom
                    } else {
                        id_upper_zero
                    }
                };

                if id != 0 {
                    chunk.set(lx, ly, lz, id);
                }
            }
        }
    }

    chunk
}

#[inline]
fn pick(list: &[String], wx: i32, wz: i32, seed: u32) -> &str {
    // Deterministic pick from a non-empty list using per-column rng
    // (If a list is empty, we fall back to a safe default below.)
    if list.is_empty() { return "stone_block"; }
    let r = col_rand_u32(wx, wz, seed);
    let idx = (r as usize) % list.len();
    &list[idx]
}

#[inline]
fn clamp_world_y(y: f32) -> f32 {
    ((Y_MIN + 1) as f32).max(y)
}