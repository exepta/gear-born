use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};
use game_core::configuration::WorldGenConfig;
use game_core::world::block::{BlockId, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex};
use game_core::world::chunk_dim::*;
use game_core::world::save::{RegionCache, RegionFile, WorldSave, REGION_SIZE};
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::HashMap;
use std::path::PathBuf;

pub const MAX_INFLIGHT_MESH: usize = 64;
pub const MAX_INFLIGHT_GEN:  usize = 32;

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId),
    cfg: WorldGenConfig,
) -> ChunkData {
    let (grass, dirt, stone) = ids;
    let mut c = ChunkData::new();

    let mut height_n = FastNoiseLite::with_seed(cfg.seed);
    height_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    height_n.set_frequency(Option::from(cfg.height_freq));
    height_n.set_fractal_type(Option::from(FractalType::FBm));
    height_n.set_fractal_octaves(Option::from(5));
    height_n.set_fractal_gain(Some(0.5));
    height_n.set_fractal_lacunarity(Some(2.0));

    let mut warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x5EED_BA5Ei32);
    warp_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    warp_n.set_frequency(Option::from(cfg.warp_freq));

    let mut plains_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0B10Ei32);
    plains_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    plains_n.set_frequency(Option::from(cfg.plains_freq));

    let (_cave_n, _cavern_n, _cave_region_n, _entrance_gate_n) = (None::<FastNoiseLite>, None::<FastNoiseLite>, None::<FastNoiseLite>, None::<FastNoiseLite>);

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32; let wzf = wz as f32;

            let mask_raw = map01(plains_n.get_noise_2d(wxf, wzf));
            let plains_factor = smoothstep(
                cfg.plains_threshold - cfg.plains_blend,
                cfg.plains_threshold + cfg.plains_blend,
                mask_raw,
            );

            let amp = leap(cfg.plains_span as f32, cfg.height_span as f32, plains_factor);
            let warp_amp = leap(cfg.warp_amp_plains, cfg.warp_amp, plains_factor);

            // Domain warp
            let dx = warp_n.get_noise_2d(wxf, wzf) * warp_amp;
            let dz = warp_n.get_noise_2d(wxf + 1000.0, wzf - 1000.0) * warp_amp;

            // Basis-Höhe
            let mut h01 = map01(height_n.get_noise_2d(wxf + dx, wzf + dz));

            let flatten = (1.0 - plains_factor) * cfg.plains_flatten;
            if flatten > 0.0 {
                h01 = leap(0.5, h01, 1.0 - flatten);
            }

            let mut h = cfg.base_height + (h01 * amp) as i32;
            h = h.clamp(Y_MIN + 1, Y_MAX - 1);

            for ly in 0..CY {
                let wy = local_y_to_world(ly);

                let id = if wy < h - 2 { stone }
                else if wy < h { dirt }
                else if wy == h { grass }
                else { 0 };

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

pub fn save_chunk_sync(ws: &WorldSave, cache: &mut RegionCache, coord: IVec2, ch: &ChunkData) -> std::io::Result<()> {
    let (r_coord, idx) = chunk_to_region_slot(coord);
    let path = ws.region_path(r_coord);
    let rf = cache.0.entry(r_coord).or_insert(RegionFile::open(&path)?);
    let data = encode_chunk(ch);
    rf.write_slot_append(idx, &data)
}

/*pub fn try_load_chunk_sync(ws: &WorldSave, cache: &mut RegionCache, coord: IVec2) -> std::io::Result<Option<ChunkData>> {
    let (r_coord, idx) = chunk_to_region_slot(coord);
    let path = ws.region_path(r_coord);
    if !path.exists() { return Ok(None); }
    let rf = cache.0.entry(r_coord).or_insert(RegionFile::open(&path)?);
    if let Some(buf) = rf.read_slot(idx)? {
        let c = decode_chunk(&buf)?;
        Ok(Some(c))
    } else {
        Ok(None)
    }
}*/

pub async fn load_or_gen_chunk_async(
    ws_root: PathBuf,
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId),
    cfg: WorldGenConfig,
) -> ChunkData {
    let (r_coord, idx) = chunk_to_region_slot(coord);
    let path = ws_root.join("region").join(format!("r.{}.{}.region", r_coord.x, r_coord.y));
    if let Ok(mut rf) = RegionFile::open(&path) {
        if let Ok(Some(buf)) = rf.read_slot(idx) {
            if let Ok(c) = decode_chunk(&buf) {
                return c;
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

#[inline] fn div_floor(a: i32, b: i32) -> i32 { (a as f32 / b as f32).floor() as i32 }
#[inline] fn mod_floor(a: i32, b: i32) -> i32 { a - div_floor(a,b)*b }
