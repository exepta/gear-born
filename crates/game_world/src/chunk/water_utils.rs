use crate::chunk::chunk_utils::col_rand_u32;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use game_core::states::{AppState, LoadingStates};
use game_core::world::block::VOXEL_SIZE;
use game_core::world::chunk::{ChunkData, ChunkMap};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidChunk, FluidMap, WaterMeshIndex};
use game_core::world::save::{container_find, container_upsert, slot_is_container, RegionCache, RegionFile, WorldSave, REGION_SIZE, TAG_WAT1};
use lz4_flex::{compress_prepend_size, decompress_size_prepended};
use std::path::PathBuf;

const WATER_MAGIC_V1: u32 = 0x3154_4157;
const WATER_MAGIC_V2: u32 = 0x3254_4157;

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
    pub(crate) solid_east:  Option<Vec<bool>>, // (y1-y0)*CZ, index = (y - y0)*CZ + z
    pub(crate) solid_west:  Option<Vec<bool>>,
    pub(crate) solid_south: Option<Vec<bool>>, // (y1-y0)*CX, index = (y - y0)*CX + x
    pub(crate) solid_north: Option<Vec<bool>>,

    pub(crate) fluid_east:  Option<Vec<bool>>,
    pub(crate) fluid_west:  Option<Vec<bool>>,
    pub(crate) fluid_south: Option<Vec<bool>>,
    pub(crate) fluid_north: Option<Vec<bool>>,
}

pub(crate) fn water_snapshot_borders(
    chunk_map: &ChunkMap,
    fluids: &FluidMap,
    coord: IVec2,
    y0: usize, y1: usize,
    sea_level: i32,
) -> WaterBorderSnapshot {
    let mut snap = WaterBorderSnapshot {
        y0,
        solid_east: None, solid_west: None, solid_south: None, solid_north: None,
        fluid_east: None, fluid_west: None, fluid_south: None, fluid_north: None,
    };

    // Helpers
    let solid_col = |ch: &ChunkData, x: usize, z: usize, y: usize| -> bool { ch.get(x,y,z) != 0 };


    if let Some(nc) = chunk_map.chunks.get(&IVec2::new(coord.x + 1, coord.y)) {
        let mut vs = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { vs.push(solid_col(nc, 0, z, y)); } }
        snap.solid_east = Some(vs);

        let mut vf = Vec::with_capacity((y1 - y0) * CZ);
        if let Some(nf) = fluids.0.get(&IVec2::new(coord.x + 1, coord.y)) {
            for y in y0..y1 { for z in 0..CZ { vf.push(nf.get(0, y, z)); } }
        } else {
            for y in y0..y1 { let wy = Y_MIN + y as i32; for z in 0..CZ {
                let s = snap.solid_east.as_ref().unwrap()[(y - y0) * CZ + z];
                vf.push(wy <= sea_level && !s);
            }}
        }
        snap.fluid_east = Some(vf);
    }

    if let Some(nc) = chunk_map.chunks.get(&IVec2::new(coord.x - 1, coord.y)) {
        let mut vs = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { vs.push(solid_col(nc, CX-1, z, y)); } }
        snap.solid_west = Some(vs);

        let mut vf = Vec::with_capacity((y1 - y0) * CZ);
        if let Some(nf) = fluids.0.get(&IVec2::new(coord.x - 1, coord.y)) {
            for y in y0..y1 { for z in 0..CZ { vf.push(nf.get(CX-1, y, z)); } }
        } else {
            for y in y0..y1 { let wy = Y_MIN + y as i32; for z in 0..CZ {
                let s = snap.solid_west.as_ref().unwrap()[(y - y0) * CZ + z];
                vf.push(wy <= sea_level && !s);
            }}
        }
        snap.fluid_west = Some(vf);
    }

    if let Some(nc) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y + 1)) {
        let mut vs = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { vs.push(solid_col(nc, x, 0, y)); } }
        snap.solid_south = Some(vs);

        let mut vf = Vec::with_capacity((y1 - y0) * CX);
        if let Some(nf) = fluids.0.get(&IVec2::new(coord.x, coord.y + 1)) {
            for y in y0..y1 { for x in 0..CX { vf.push(nf.get(x, y, 0)); } }
        } else {
            for y in y0..y1 { let wy = Y_MIN + y as i32; for x in 0..CX {
                let s = snap.solid_south.as_ref().unwrap()[(y - y0) * CX + x];
                vf.push(wy <= sea_level && !s);
            }}
        }
        snap.fluid_south = Some(vf);
    }

    if let Some(nc) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y - 1)) {
        let mut vs = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { vs.push(solid_col(nc, x, CZ-1, y)); } }
        snap.solid_north = Some(vs);

        let mut vf = Vec::with_capacity((y1 - y0) * CX);
        if let Some(nf) = fluids.0.get(&IVec2::new(coord.x, coord.y - 1)) {
            for y in y0..y1 { for x in 0..CX { vf.push(nf.get(x, y, CZ-1)); } }
        } else {
            for y in y0..y1 { let wy = Y_MIN + y as i32; for x in 0..CX {
                let s = snap.solid_north.as_ref().unwrap()[(y - y0) * CX + x];
                vf.push(wy <= sea_level && !s);
            }}
        }
        snap.fluid_north = Some(vf);
    }

    snap
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

pub(crate) async fn build_water_mesh_subchunk_async(
    coord: IVec2,
    sub: usize,
    chunk_copy: ChunkData,
    fc: FluidChunk,
    borders: WaterBorderSnapshot,
) -> ((IVec2, usize), WaterMeshBuild) {
    let mut pos = Vec::new();
    let mut nor = Vec::new();
    let mut uv0 = Vec::new();
    let mut idx = Vec::new();

    let s  = VOXEL_SIZE;
    let hh = 0.5 * s;
    let eps = 0.01 * s;

    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);

    // Helpers
    let solid_local = |x:i32,y:i32,z:i32| -> bool {
        if x < 0 || y < 0 || z < 0 || x >= CX as i32 || y >= CY as i32 || z >= CZ as i32 { false }
        else { chunk_copy.get(x as usize, y as usize, z as usize) != 0 }
    };

    let sample_sb = |opt:&Option<Vec<bool>>, y:usize, i:usize, stride:usize| -> bool {
        opt.as_ref().map(|v| v[(y - borders.y0) * stride + i]).unwrap_or(false)
    };

    let solid_at = |x:i32,y:i32,z:i32| -> bool {
        if y < 0 || y >= CY as i32 { return false; }
        if x >= 0 && x < CX as i32 && z >= 0 && z < CZ as i32 {
            return solid_local(x,y,z);
        }

        if x == -1           { return sample_sb(&borders.solid_west,  y as usize, z as usize, CZ); }
        if x == CX as i32    { return sample_sb(&borders.solid_east,  y as usize, z as usize, CZ); }
        if z == -1           { return sample_sb(&borders.solid_north, y as usize, x as usize, CX); }
        if z == CZ as i32    { return sample_sb(&borders.solid_south, y as usize, x as usize, CX); }
        false
    };

    let water_local = |x:i32,y:i32,z:i32| -> bool {
        if x < 0 || y < 0 || z < 0 || x >= CX as i32 || y >= CY as i32 || z >= CZ as i32 { false }
        else { fc.get(x as usize, y as usize, z as usize) }
    };

    let water_edge = |x:i32,y:i32,z:i32| -> bool {
        if y < 0 || y >= CY as i32 { return false; }
        if x == -1        { return sample_sb(&borders.fluid_west,  y as usize, z as usize, CZ); }
        if x == CX as i32 { return sample_sb(&borders.fluid_east,  y as usize, z as usize, CZ); }
        if z == -1        { return sample_sb(&borders.fluid_north, y as usize, x as usize, CX); }
        if z == CZ as i32 { return sample_sb(&borders.fluid_south, y as usize, x as usize, CX); }
        false
    };

    let water_at = |x:i32,y:i32,z:i32| -> bool {
        if x >= 0 && x < CX as i32 && z >= 0 && z < CZ as i32 {
            return water_local(x,y,z);
        }
        // Rand
        let w = water_edge(x,y,z);
        if !w { false } else { true }
    };

    // Emit helper
    let mut emit = |a:[f32;3], b:[f32;3], c:[f32;3], d:[f32;3], n:[f32;3]| {
        let base = pos.len() as u32;
        pos.extend_from_slice(&[a,b,c,d]);
        nor.extend_from_slice(&[n,n,n,n]);
        uv0.extend_from_slice(&[[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0]]);
        idx.extend_from_slice(&[base, base+1, base+2, base, base+2, base+3]);
    };

    for z in 0..CZ {
        for x in 0..CX {
            for ly in y0..y1 {
                if !fc.get(x, ly, z) { continue; }

                let cx = (x as f32 + 0.5) * s;
                let cy = (ly as f32 + 0.5) * s;
                let cz = (z as f32 + 0.5) * s;

                let wa = |dx:i32,dy:i32,dz:i32| water_at(x as i32 + dx, ly as i32 + dy, z as i32 + dz);
                let sa = |dx:i32,dy:i32,dz:i32| solid_at(x as i32 + dx, ly as i32 + dy, z as i32 + dz);

                if !wa( 1,0,0) { emit([cx+hh,cy+hh,cz-hh],[cx+hh,cy+hh,cz+hh],[cx+hh,cy-hh,cz+hh],[cx+hh,cy-hh,cz-hh],[ 1.0,0.0,0.0]); }
                if !wa(-1,0,0) { emit([cx-hh,cy+hh,cz+hh],[cx-hh,cy+hh,cz-hh],[cx-hh,cy-hh,cz-hh],[cx-hh,cy-hh,cz+hh],[-1.0,0.0,0.0]); }
                if !wa(0,0, 1) { emit([cx-hh,cy-hh,cz+hh],[cx+hh,cy-hh,cz+hh],[cx+hh,cy+hh,cz+hh],[cx-hh,cy+hh,cz+hh],[ 0.0,0.0,1.0]); }
                if !wa(0,0,-1) { emit([cx+hh,cy-hh,cz-hh],[cx-hh,cy-hh,cz-hh],[cx-hh,cy+hh,cz-hh],[cx+hh,cy+hh,cz-hh],[ 0.0,0.0,-1.0]); }
                if !wa(0,1,0) && !sa(0,1,0) {
                    emit([cx-hh,cy+hh+eps,cz+hh],[cx+hh,cy+hh+eps,cz+hh],[cx+hh,cy+hh+eps,cz-hh],[cx-hh,cy+hh+eps,cz-hh],[0.0,1.0,0.0]);
                }
                if !wa(0,-1,0) && !sa(0,-1,0) {
                    emit([cx-hh,cy-hh,cz-hh],[cx+hh,cy-hh,cz-hh],[cx+hh,cy-hh,cz+hh],[cx-hh,cy-hh,cz+hh],[0.0,-1.0,0.0]);
                }
            }
        }
    }

    ((coord, sub), WaterMeshBuild { pos, nor, uv0, idx })
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

pub fn load_water_chunk_from_disk(ws_root: PathBuf, coord: IVec2) -> Option<FluidChunk> {
    let (r_coord, _) = chunk_to_region_slot(coord);
    let path = ws_root.join("region").join(format!("r.{}.{}.region", r_coord.x, r_coord.y));
    if let Ok(mut rf) = RegionFile::open(&path) {
        if let Ok(Some(buf)) = rf.read_chunk(coord) {
            if slot_is_container(&buf) {
                if let Some(rec) = container_find(&buf, TAG_WAT1) {
                    return decode_fluid_chunk(rec);
                }
            }
        }
    }
    None
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

fn encode_fluid_chunk(w: &FluidChunk) -> Vec<u8> {
    // [magic V2][sea_level][u32 n_words][n_words * u64]
    let n = w.bits.len() as u32;
    let mut raw = Vec::with_capacity(12 + n as usize * 8);
    raw.extend_from_slice(&WATER_MAGIC_V2.to_le_bytes());
    raw.extend_from_slice(&w.sea_level.to_le_bytes());
    raw.extend_from_slice(&n.to_le_bytes());
    for &word in &w.bits {
        raw.extend_from_slice(&word.to_le_bytes());
    }
    compress_prepend_size(&raw)
}

fn decode_fluid_chunk(buf: &[u8]) -> Option<FluidChunk> {
    if let Ok(de) = decompress_size_prepended(buf) {
        if de.len() >= 12 {
            let magic = u32::from_le_bytes(de[0..4].try_into().ok()?);
            if magic == WATER_MAGIC_V2 {
                let sea_level = i32::from_le_bytes(de[4..8].try_into().ok()?);
                let n = u32::from_le_bytes(de[8..12].try_into().ok()?) as usize;
                if de.len() >= 12 + n * 8 {
                    let mut bits = vec![0u64; n];
                    let mut p = 12;
                    for i in 0..n {
                        bits[i] = u64::from_le_bytes(de[p..p+8].try_into().ok()?);
                        p += 8;
                    }
                    return Some(FluidChunk { sea_level, bits });
                }
            }
        }
    }

    // Legacy V1 (unkomprimiert): [magic][sea_level][(x,z)->y0,y1]  => rekonstruiere Bitmaske
    if buf.len() >= 8 {
        let magic = u32::from_le_bytes(buf[0..4].try_into().ok()?);
        if magic == WATER_MAGIC_V1 {
            let sea_level = i32::from_le_bytes(buf[4..8].try_into().ok()?);
            let expected = 8 + CX*CZ*4;
            if buf.len() >= expected {
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
                return Some(w);
            }
        }
    }

    None
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