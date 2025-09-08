use crate::chunk::chunk_gen::generate_chunk_async_biome;
use crate::chunk::chunk_struct::*;
use bevy::prelude::*;
use bincode::{config, decode_from_slice, encode_to_vec};
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, LoadingStates};
use game_core::world::biome::registry::BiomeRegistry;
use game_core::world::block::{BlockId, BlockRegistry, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, ChunkMeshIndex};
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
    reg: &BlockRegistry,        // ⟵ NEW: pass the registry
    biomes: &BiomeRegistry,     // ⟵ NEW: pass the biome registry
    cfg: WorldGenConfig,        // we only need cfg.seed right now
) -> ChunkData {
    // Try to load from a region file first
    let (r_coord, _) = chunk_to_region_slot(coord);
    let path = ws_root.join("region").join(format!("r.{}.{}.region", r_coord.x, r_coord.y));
    if let Ok(mut rf) = RegionFile::open(&path) {
        if let Ok(Some(buf)) = rf.read_chunk(coord) {
            // Detect legacy container-wrapped blobs
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

    // Fallback: generate fresh chunk via biome-based generator
    // Note: new generator expects (coord, &BlockRegistry, seed, &BiomeRegistry)
    generate_chunk_async_biome(coord, reg, cfg.seed, biomes).await
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