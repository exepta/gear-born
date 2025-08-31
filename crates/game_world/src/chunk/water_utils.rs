use crate::chunk::chunk_utils::col_rand_u32;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use game_core::states::{AppState, LoadingStates};
use game_core::world::block::VOXEL_SIZE;
use game_core::world::chunk::{ChunkData, ChunkMap};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidChunk, FluidMap, SolidSnapshot, WaterMeshIndex};
use game_core::world::save::{container_find, container_upsert, slot_is_container, unpack_slot_bytes, RegionCache, RegionFile, WorldSave, REGION_SIZE, TAG_WAT1};
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::collections::HashMap;

pub(crate) const WATER_MAGIC_V1: u32 = 0x3154_4157;
pub(crate) const WATER_MAGIC_V2: u32 = 0x3254_4157;

pub struct WaterMeshBuild {
    pub pos: Vec<[f32;3]>,
    pub nor: Vec<[f32;3]>,
    pub uv0: Vec<[f32;2]>,
    pub idx: Vec<u32>,
}

impl WaterMeshBuild {
    pub fn is_empty(&self) -> bool { self.pos.is_empty() }
    pub fn into_mesh(self) -> Mesh {
        use bevy::render::mesh::{Indices, Mesh, PrimitiveTopology};
        let mut m = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, self.pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   self.nor);
        m.insert_attribute(Mesh::ATTRIBUTE_UV_0,     self.uv0);
        m.insert_indices(Indices::U32(self.idx));
        m
    }
}

#[derive(Clone)]
pub(crate) struct WaterBorderSnapshot {
    pub(crate) y0: usize,
    pub(crate) y1: usize,

    pub(crate) east:  Option<Vec<bool>>,
    pub(crate) west:  Option<Vec<bool>>,
    pub(crate) south: Option<Vec<bool>>,
    pub(crate) north: Option<Vec<bool>>,
}

pub fn water_snapshot_borders(
    _chunks: &ChunkMap,
    fluids: &FluidMap,
    coord: IVec2,
    y0: usize,
    y1: usize,
    _sea_level: i32,
) -> WaterBorderSnapshot {
    let take = |c: IVec2, x: usize, y: usize, z: usize| -> bool {
        fluids.0.get(&c).map(|f| f.get(x, y, z)).unwrap_or(false)
    };

    let mut east: Option<Vec<bool>>  = None;
    let mut west: Option<Vec<bool>>  = None;
    let mut south: Option<Vec<bool>> = None;
    let mut north: Option<Vec<bool>> = None;

    let nc = coord + IVec2::X;
    if fluids.0.contains_key(&nc) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take(nc, 0, y, z)); } }
        east = Some(v);
    }

    let nc = coord - IVec2::X;
    if fluids.0.contains_key(&nc) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take(nc, CX - 1, y, z)); } }
        west = Some(v);
    }

    let nc = coord + IVec2::Y;
    if fluids.0.contains_key(&nc) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take(nc, x, y, 0)); } }
        south = Some(v);
    }

    let nc = coord - IVec2::Y;
    if fluids.0.contains_key(&nc) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take(nc, x, y, CZ - 1)); } }
        north = Some(v);
    }

    WaterBorderSnapshot { y0, y1, east, west, south, north }
}

pub(crate) fn generate_water_for_chunk(
    coord: IVec2,
    chunk: &ChunkData,
    sea_level: i32,
    seed: u32,
    lakes: bool,
) -> FluidChunk {
    let mut w = FluidChunk::new(sea_level);

    for z in 0..CZ {
        for x in 0..CX {
            let wx = coord.x * CX as i32 + x as i32;
            let wz = coord.y * CZ as i32 + z as i32;

            let top = column_top_world_y(chunk, x, z);

            if top < sea_level {
                w.fill_column(x, z, top + 1, sea_level);
                continue;
            }

            let top_e = top;

            let h_e = if x+1 < CX { column_top_world_y(chunk, x+1, z) } else { top_e };
            let h_n = if z+1 < CZ { column_top_world_y(chunk, x, z+1) } else { top_e };
            let slope = (h_e - top_e).abs().max((h_n - top_e).abs());

            if lakes {
                let low_enough  = top_e <= sea_level - 1;
                let flat_enough = slope <= 1;

                if low_enough && flat_enough {
                    let seed_l = seed ^ 0xA1B2_C3D4;
                    let p_here = col_rand_f01(wx, wz, seed_l) > 0.90;
                    let neigh_ok =
                        col_rand_f01(wx+1, wz,   seed_l) > 0.90 &&
                            col_rand_f01(wx-1, wz,   seed_l) > 0.90 &&
                            col_rand_f01(wx,   wz+1, seed_l) > 0.90 &&
                            col_rand_f01(wx,   wz-1, seed_l) > 0.90;

                    if p_here && neigh_ok {
                        let down = (col_rand_f01(wx, wz, seed ^ 0xDEAD_BEEF) * 3.0).floor() as i32; // 0..2
                        let lake_level = (sea_level - 1 - down).max(top_e + 1);

                        if lake_level > top_e {
                            w.fill_column(x, z, top_e + 1, lake_level);
                        }
                    }
                }
            }
        }
    }

    w
}

pub async fn build_water_mesh_subchunk_async(
    coord: IVec2,
    sub: usize,
    chunk: ChunkData,
    fc: FluidChunk,
    borders: WaterBorderSnapshot,
) -> ((IVec2, usize), WaterMeshBuild) {
    let mut build = WaterMeshBuild { pos: vec![], nor: vec![], uv0: vec![], idx: vec![] };

    let s = VOXEL_SIZE;
    let hh = 0.5 * s;
    let eps = 0.01 * s;
    let skirt_h = 0.20 * s;

    let y0 = borders.y0;
    let y1 = borders.y1;

    let sample_opt = |opt: &Option<Vec<bool>>, y: usize, i: usize, stride: usize| -> Option<bool> {
        opt.as_ref().map(|v| {
            let iy = y - y0;
            v[iy * stride + i]
        })
    };
    let east_at  = |y: usize, z: usize| sample_opt(&borders.east,  y, z, CZ);
    let west_at  = |y: usize, z: usize| sample_opt(&borders.west,  y, z, CZ);
    let south_at = |y: usize, x: usize| sample_opt(&borders.south, y, x, CX);
    let north_at = |y: usize, x: usize| sample_opt(&borders.north, y, x, CX);

    let water_at = |lx: i32, ly: i32, lz: i32| -> bool {
        if ly < 0 || ly >= CY as i32 { return false; }
        if lx >= 0 && lx < CX as i32 && lz >= 0 && lz < CZ as i32 {
            return fc.get(lx as usize, ly as usize, lz as usize);
        }
        if lx == CX as i32 && lz >= 0 && lz < CZ as i32 {
            return east_at(ly as usize, lz as usize).unwrap_or(false);
        }
        if lx == -1 && lz >= 0 && lz < CZ as i32 {
            return west_at(ly as usize, lz as usize).unwrap_or(false);
        }
        if lz == CZ as i32 && lx >= 0 && lx < CX as i32 {
            return south_at(ly as usize, lx as usize).unwrap_or(false);
        }
        if lz == -1 && lx >= 0 && lx < CX as i32 {
            return north_at(ly as usize, lx as usize).unwrap_or(false);
        }
        false
    };

    for z in 0..CZ {
        for x in 0..CX {
            for ly in y0..y1 {
                if !fc.get(x, ly, z) { continue; }

                let cx = (x as f32 + 0.5) * s;
                let cy = (ly as f32 + 0.5) * s;
                let cz = (z as f32 + 0.5) * s;

                let wa = |dx: i32, dy: i32, dz: i32| water_at(x as i32 + dx, ly as i32 + dy, z as i32 + dz);
                let sa = |dx: i32, dy: i32, dz: i32| {
                    if x as i32 + dx < 0 || x as i32 + dx >= CX as i32 ||
                        z as i32 + dz < 0 || z as i32 + dz >= CZ as i32 ||
                        ly as i32 + dy < 0 || ly as i32 + dy >= CY as i32 {
                        false
                    } else {
                        chunk.get((x as i32 + dx) as usize, (ly as i32 + dy) as usize, (z as i32 + dz) as usize) != 0
                    }
                };

                let surface_here = !wa(0, 1, 0) && !sa(0, 1, 0);

                if surface_here {
                    let y_top = cy + hh;
                    let y_bot = y_top - skirt_h;

                    // +X
                    if !wa(1, 0, 0) {
                        let x_face = cx + hh - eps;
                        emit_quad_build(&mut build,
                                        [x_face, y_top, cz - hh],
                                        [x_face, y_top, cz + hh],
                                        [x_face, y_bot, cz + hh],
                                        [x_face, y_bot, cz - hh],
                                        [1.0, 0.0, 0.0],
                        );
                    }
                    // -X
                    if !wa(-1, 0, 0) {
                        let x_face = cx - hh + eps;
                        emit_quad_build(&mut build,
                                        [x_face, y_top, cz + hh],
                                        [x_face, y_top, cz - hh],
                                        [x_face, y_bot, cz - hh],
                                        [x_face, y_bot, cz + hh],
                                        [-1.0, 0.0, 0.0],
                        );
                    }
                    // +Z
                    if !wa(0, 0, 1) {
                        let z_face = cz + hh - eps;
                        emit_quad_build(&mut build,
                                        [cx - hh, y_bot, z_face],
                                        [cx + hh, y_bot, z_face],
                                        [cx + hh, y_top, z_face],
                                        [cx - hh, y_top, z_face],
                                        [0.0, 0.0, 1.0],
                        );
                    }
                    // -Z
                    if !wa(0, 0, -1) {
                        let z_face = cz - hh + eps;
                        emit_quad_build(&mut build,
                                        [cx + hh, y_bot, z_face],
                                        [cx - hh, y_bot, z_face],
                                        [cx - hh, y_top, z_face],
                                        [cx + hh, y_top, z_face],
                                        [0.0, 0.0, -1.0],
                        );
                    }
                }

                // ► TOP
                if !wa(0, 1, 0) && !sa(0, 1, 0) {
                    emit_quad_build(
                        &mut build,
                        [cx - hh, cy + hh, cz + hh],
                        [cx + hh, cy + hh, cz + hh],
                        [cx + hh, cy + hh, cz - hh],
                        [cx - hh, cy + hh, cz - hh],
                        [0.0, 1.0, 0.0],
                    );
                }

                // ► BOTTOM
                if !wa(0, -1, 0) && !sa(0, -1, 0) {
                    emit_quad_build(
                        &mut build,
                        [cx - hh, cy - hh, cz - hh],
                        [cx + hh, cy - hh, cz - hh],
                        [cx + hh, cy - hh, cz + hh],
                        [cx - hh, cy - hh, cz + hh],
                        [0.0, -1.0, 0.0],
                    );
                }
            }
        }
    }

    ((coord, sub), build)
}

#[inline]
fn emit_quad_build(
    build: &mut WaterMeshBuild,
    a: [f32; 3], b: [f32; 3], c: [f32; 3], d: [f32; 3],
    n: [f32; 3],
) {
    let base = build.pos.len() as u32;
    build.pos.extend_from_slice(&[a, b, c, d]);
    build.nor.extend_from_slice(&[n, n, n, n]);
    build.uv0.extend_from_slice(&[[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]]);
    build.idx.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 3]);
}

pub fn save_water_chunk_sync(
    ws: &WorldSave,
    cache: &mut RegionCache,
    coord: IVec2,
    w: &FluidChunk,
) {
    let wat = encode_fluid_chunk(w);
    let old = cache.read_chunk(ws, coord).ok().flatten();
    let merged = container_upsert(old.as_deref(), TAG_WAT1, &wat);
    let _ = cache.write_chunk_replace(ws, coord, &merged);
}

#[inline]
fn col_rand_f01(x: i32, z: i32, seed: u32) -> f32 {
    (col_rand_u32(x,z,seed) as f32) / (u32::MAX as f32)
}

fn column_top_world_y(chunk: &ChunkData, x: usize, z: usize) -> i32 {
    for ly in (0..CY).rev() {
        if chunk.get(x, ly, z) != 0 { return Y_MIN + ly as i32; }
    }
    Y_MIN - 1
}

pub(crate) fn despawn_water_mesh(
    key: (IVec2, u8),
    windex: &mut WaterMeshIndex,
    commands: &mut Commands,
    q_mesh: &Query<&Mesh3d>,
    meshes: &mut Assets<Mesh>,
) {
    if let Some(ent) = windex.0.remove(&key) {
        if let Ok(Mesh3d(handle)) = q_mesh.get(ent) {
            meshes.remove(handle.id());
        }
        commands.entity(ent).despawn();
    }
}

pub fn load_water_chunk_from_disk_any(ws_root: std::path::PathBuf, coord: IVec2)
                                      -> Option<(FluidChunk, u32)>
{
    let (r_coord, _) = chunk_to_region_slot(coord);
    let path = ws_root.join("region").join(format!("r.{}.{}.region", r_coord.x, r_coord.y));
    if let Ok(mut rf) = RegionFile::open(&path) {
        if let Ok(Some(buf)) = rf.read_chunk(coord) {
            // 1) SLOT-Container-Format
            if slot_is_container(&buf) {
                if let Some(rec) = container_find(&buf, TAG_WAT1) {
                    return decode_fluid_chunk_with_version(rec);
                }
            } else {
                // 2) GBW1-Format
                let (_, w_bytes) = unpack_slot_bytes(&buf);
                if let Some(w) = w_bytes {
                    if let Some(res) = decode_fluid_chunk_with_version(w) {
                        return Some(res);
                    }
                }
            }
        }
    }
    None
}

pub fn water_mask_with_solids(fc: &mut FluidChunk, chunk: &ChunkData) {
    for y in 0..CY {
        for z in 0..CZ {
            for x in 0..CX {
                if fc.get(x,y,z) && chunk.get(x,y,z) != 0 {
                    fc.set(x,y,z, false);
                }
            }
        }
    }
}

pub fn encode_fluid_chunk(w: &FluidChunk) -> Vec<u8> {
    let mut raw: Vec<u8> = Vec::with_capacity(w.bits.len() * 8);
    for &word in &w.bits {
        raw.extend_from_slice(&word.to_le_bytes());
    }
    let comp = compress_prepend_size(&raw);

    let mut out = Vec::with_capacity(4 + 4 + 4 + comp.len());
    out.extend_from_slice(&WATER_MAGIC_V2.to_le_bytes());
    out.extend_from_slice(&w.sea_level.to_le_bytes());
    let words = w.bits.len() as u32;
    out.extend_from_slice(&words.to_le_bytes());
    out.extend_from_slice(&comp);
    out
}

pub fn decode_fluid_chunk_with_version(buf: &[u8]) -> Option<(FluidChunk, u32)> {
    if buf.len() < 8 { return None; }
    let magic = u32::from_le_bytes(buf[0..4].try_into().ok()?);

    if magic == WATER_MAGIC_V2 {
        if buf.len() < 12 { return None; }
        let sea_level = i32::from_le_bytes(buf[4..8].try_into().ok()?);
        let words = u32::from_le_bytes(buf[8..12].try_into().ok()?) as usize;
        let de = decompress_size_prepended(&buf[12..]).ok()?;
        if de.len() != words * 8 { return None; }

        let mut bits = vec![0u64; words];
        for (i, chunk) in de.chunks_exact(8).enumerate() {
            bits[i] = u64::from_le_bytes(chunk.try_into().unwrap());
        }
        Some((FluidChunk { sea_level, bits }, WATER_MAGIC_V2))
    } else if magic == WATER_MAGIC_V1 {
        let sea_level = i32::from_le_bytes(buf[4..8].try_into().ok()?);
        let expected = 8 + CX * CZ * 4;
        if buf.len() < expected { return None; }

        let mut w = FluidChunk::new(sea_level);
        let mut p = 8;
        for z in 0..CZ {
            for x in 0..CX {
                let y0 = i16::from_le_bytes(buf[p..p+2].try_into().ok()?); p += 2;
                let y1 = i16::from_le_bytes(buf[p..p+2].try_into().ok()?); p += 2;
                if y0 != i16::MIN && y1 != i16::MIN {
                    w.fill_column(x, z, y0 as i32, y1 as i32);
                }
            }
        }
        Some((w, WATER_MAGIC_V1))
    } else {
        None
    }
}

#[inline] fn div_floor(a: i32, b: i32) -> i32 { (a as f32 / b as f32).floor() as i32 }
#[inline] fn mod_floor(a: i32, b: i32) -> i32 { a - div_floor(a,b)*b }
#[inline]
fn chunk_to_region_slot(c: IVec2) -> (IVec2, usize) {
    let rx = div_floor(c.x, REGION_SIZE);
    let rz = div_floor(c.y, REGION_SIZE);
    let lx = mod_floor(c.x, REGION_SIZE) as usize;
    let lz = mod_floor(c.y, REGION_SIZE) as usize;
    let idx = lz * (REGION_SIZE as usize) + lx;
    (IVec2::new(rx, rz), idx)
}

#[inline]
pub(crate) fn in_water_gen(state: &State<AppState>) -> bool {
    matches!(state.get(), AppState::Loading(LoadingStates::WaterGen))
}

#[inline]
pub(crate) fn all_chunks_have_water(chunk_map: &ChunkMap, water: &FluidMap) -> bool {
    if chunk_map.chunks.is_empty() { return false; }
    chunk_map.chunks.keys().all(|c| water.0.contains_key(c))
}

pub(crate) fn build_solid_snapshot_3x3(chunks: &ChunkMap, center: IVec2) -> SolidSnapshot {
    let mut bits = HashMap::new();
    for dz in -1..=1 {
        for dx in -1..=1 {
            let c = IVec2::new(center.x + dx, center.y + dz);
            let mut v = vec![0u8; CX*CY*CZ];
            if let Some(ch) = chunks.chunks.get(&c) {
                for y in 0..CY {
                    for z in 0..CZ {
                        for x in 0..CX {
                            let solid = ch.get(x, y, z) != 0;
                            let i = (y * CZ + z) * CX + x;
                            v[i] = if solid {1} else {0};
                        }
                    }
                }
            } else {
                v.fill(1);
            }
            bits.insert(c, v);
        }
    }
    SolidSnapshot { center, bits }
}

#[inline]
pub(crate) fn snap_is_solid(s: &SolidSnapshot, c: IVec2, x: i32, y: i32, z: i32) -> Option<bool> {
    if y < 0 || y >= CY as i32 || x < 0 || x >= CX as i32 || z < 0 || z >= CZ as i32 { return Some(true); }
    let v = s.bits.get(&c)?;
    let i = ((y as usize) * CZ + (z as usize)) * CX + (x as usize);
    Some(v[i] != 0)
}

#[inline]
pub(crate) fn in_snapshot(s: &SolidSnapshot, c: IVec2) -> bool {
    s.bits.contains_key(&c)
}

#[inline]
pub(crate) fn neighbor_lookup_chunked(coord: IVec2, lx: i32, lz: i32) -> (IVec2, i32, i32) {
    let mut nx = lx; let mut nz = lz; let mut nc = coord;
    if nx < 0 { nx += CX as i32; nc.x -= 1; }
    if nx >= CX as i32 { nx -= CX as i32; nc.x += 1; }
    if nz < 0 { nz += CZ as i32; nc.y -= 1; }
    if nz >= CZ as i32 { nz -= CZ as i32; nc.y += 1; }
    (nc, nx, nz)
}

#[inline]
pub(crate) fn water_meshing_ready(coord: IVec2, water: &FluidMap, chunks: &ChunkMap) -> bool {
    if !chunks.chunks.contains_key(&coord) { return false; }
    if !water.0.contains_key(&coord) { return false; }

    for d in [IVec2::X, -IVec2::X, IVec2::Y, -IVec2::Y] {
        let n = coord + d;
        if chunks.chunks.contains_key(&n) && !water.0.contains_key(&n) {
            return false;
        }
    }
    true
}