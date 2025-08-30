use crate::chunk::chunk_utils::col_rand_u32;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use game_core::world::block::VOXEL_SIZE;
use game_core::world::chunk::{ChunkData, ChunkMap};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidChunk, FluidMap, WaterMeshIndex};
use game_core::world::save::{container_find, container_upsert, slot_is_container, RegionCache, WorldSave, TAG_WAT1};

const WATER_MAGIC: u32 = 0x3154_4157;
const EMPTY_I16: i16 = i16::MIN;

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

pub(crate) fn build_water_mesh_subchunk(
    coord: IVec2,
    sub: usize,
    chunks: &ChunkMap,
    fluids: &FluidMap,
) -> Option<Mesh> {
    let mut pos: Vec<[f32; 3]> = Vec::new();
    let mut nor: Vec<[f32; 3]> = Vec::new();
    let mut uv0: Vec<[f32; 2]> = Vec::new();
    let mut idx: Vec<u32>      = Vec::new();

    let s  = VOXEL_SIZE;
    let hh = 0.5 * s;
    let eps = 0.01 * s;

    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);

    let fc = match fluids.0.get(&coord) { Some(f) => f, None => return None };

    let solid_at = |lx: i32, ly: i32, lz: i32| -> bool {
        if ly < 0 || ly >= CY as i32 { return false; }
        let (nc, nx, nz) = neighbor_lookup(coord, lx, lz);
        if let Some(ch) = chunks.chunks.get(&nc) {
            ch.get(nx as usize, ly as usize, nz as usize) != 0
        } else {
            false
        }
    };

    let water_at = |lx: i32, ly: i32, lz: i32| -> bool {
        if ly < 0 || ly >= CY as i32 { return false; }
        let (nc, nx, nz) = neighbor_lookup(coord, lx, lz);
        if let Some(fcn) = fluids.0.get(&nc) {
            fcn.get(nx as usize, ly as usize, nz as usize)
        } else {
            let wy = Y_MIN + ly;
            wy <= fc.sea_level && !solid_at(lx, ly, lz)
        }
    };

    for z in 0..CZ {
        for x in 0..CX {
            for ly in y0..y1 {
                if !fc.get(x, ly, z) { continue; }

                let cx = (x as f32 + 0.5) * s;
                let cy = (ly as f32 + 0.5) * s;
                let cz = (z as f32 + 0.5) * s;

                let wa = |dx: i32, dy: i32, dz: i32| water_at(x as i32 + dx, ly as i32 + dy, z as i32 + dz);
                let sa = |dx: i32, dy: i32, dz: i32| solid_at(x as i32 + dx, ly as i32 + dy, z as i32 + dz);

                // +X
                if !wa(1,0,0) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx+hh, cy+hh, cz-hh], // a: top-left
                              [cx+hh, cy+hh, cz+hh], // b: top-right
                              [cx+hh, cy-hh, cz+hh], // c: bottom-right
                              [cx+hh, cy-hh, cz-hh], // d: bottom-left
                              [1.0,0.0,0.0]);
                }

                // -X
                if !wa(-1,0,0) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx-hh, cy+hh, cz+hh], // a: top-left
                              [cx-hh, cy+hh, cz-hh], // b: top-right
                              [cx-hh, cy-hh, cz-hh], // c: bottom-right
                              [cx-hh, cy-hh, cz+hh], // d: bottom-left
                              [-1.0,0.0,0.0]);
                }

                // +Z
                if !wa(0,0,1) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx-hh, cy-hh, cz+hh], // a: bottom-left
                              [cx+hh, cy-hh, cz+hh], // b: bottom-right
                              [cx+hh, cy+hh, cz+hh], // c: top-right
                              [cx-hh, cy+hh, cz+hh], // d: top-left
                              [0.0,0.0,1.0]);
                }

                // -Z
                if !wa(0,0,-1) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx+hh, cy-hh, cz-hh], // a: bottom-left
                              [cx-hh, cy-hh, cz-hh], // b: bottom-right
                              [cx-hh, cy+hh, cz-hh], // c: top-right
                              [cx+hh, cy+hh, cz-hh], // d: top-left
                              [0.0,0.0,-1.0]);
                }
                // +Y (Top)
                if !wa(0,1,0) && !sa(0,1,0) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx-hh, cy+hh+eps, cz+hh],[cx+hh, cy+hh+eps, cz+hh],
                              [cx+hh, cy+hh+eps, cz-hh],[cx-hh, cy+hh+eps, cz-hh],
                              [0.0,1.0,0.0]);
                }
                // -Y (Bottom)
                if !wa(0,-1,0) && !sa(0,-1,0) {
                    emit_quad(&mut pos,&mut nor,&mut uv0,&mut idx,
                              [cx-hh, cy-hh, cz-hh],[cx+hh, cy-hh, cz-hh],
                              [cx+hh, cy-hh, cz+hh],[cx-hh, cy-hh, cz+hh],
                              [0.0,-1.0,0.0]);
                }
            }
        }
    }

    if pos.is_empty() { return None; }

    let mut m = Mesh::new(PrimitiveTopology::TriangleList, bevy::asset::RenderAssetUsages::RENDER_WORLD);
    m.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
    m.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   nor);
    m.insert_attribute(Mesh::ATTRIBUTE_UV_0,     uv0);
    m.insert_indices(Indices::U32(idx));
    Some(m)
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

pub fn load_water_chunk_sync(
    ws: &WorldSave,
    cache: &mut RegionCache,
    coord: IVec2,
) -> Option<FluidChunk> {
    if let Ok(Some(buf)) = cache.read_chunk(ws, coord) {
        if !slot_is_container(&buf) { return None; }
        if let Some(rec) = container_find(&buf, TAG_WAT1) {
            return decode_fluid_chunk(rec);
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

fn emit_quad(
    pos: &mut Vec<[f32;3]>, nor: &mut Vec<[f32;3]>, uv0: &mut Vec<[f32;2]>, idx: &mut Vec<u32>,
    a:[f32;3], b:[f32;3], c:[f32;3], d:[f32;3], n:[f32;3]
){
    let base = pos.len() as u32;
    pos.extend_from_slice(&[a,b,c,d]);
    nor.extend_from_slice(&[n,n,n,n]);
    uv0.extend_from_slice(&[[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0]]);
    idx.extend_from_slice(&[base, base+1, base+2, base, base+2, base+3]);
}

fn neighbor_lookup(coord: IVec2, lx: i32, lz: i32) -> (IVec2, i32, i32) {
    let mut nx = lx;
    let mut nz = lz;
    let mut nc = coord;

    if nx < 0        { nx += CX as i32; nc.x -= 1; }
    if nx >= CX as i32 { nx -= CX as i32; nc.x += 1; }
    if nz < 0        { nz += CZ as i32; nc.y -= 1; }
    if nz >= CZ as i32 { nz -= CZ as i32; nc.y += 1; }

    (nc, nx, nz)
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
    let mut out = Vec::with_capacity(8 + CX*CZ*4);
    out.extend_from_slice(&WATER_MAGIC.to_le_bytes());
    out.extend_from_slice(&w.sea_level.to_le_bytes());

    for z in 0..CZ {
        for x in 0..CX {
            let mut y0: i16 = EMPTY_I16;
            let mut y1: i16 = EMPTY_I16;

            for ly in 0..CY {
                if w.get(x, ly, z) {
                    y0 = (Y_MIN + ly as i32) as i16;
                    break;
                }
            }
            if y0 != EMPTY_I16 {
                for ly in (0..CY).rev() {
                    if w.get(x, ly, z) {
                        y1 = (Y_MIN + ly as i32) as i16;
                        break;
                    }
                }
            }
            out.extend_from_slice(&y0.to_le_bytes());
            out.extend_from_slice(&y1.to_le_bytes());
        }
    }
    out
}

fn decode_fluid_chunk(buf: &[u8]) -> Option<FluidChunk> {
    if buf.len() < 8 { return None; }
    let magic = u32::from_le_bytes(buf[0..4].try_into().ok()?);
    if magic != WATER_MAGIC { return None; }
    let sea_level = i32::from_le_bytes(buf[4..8].try_into().ok()?);

    let expected = 8 + CX*CZ*4;
    if buf.len() < expected { return None; }

    let mut w = FluidChunk::new(sea_level);
    let mut p = 8;

    for z in 0..CZ {
        for x in 0..CX {
            let y0 = i16::from_le_bytes(buf[p..p+2].try_into().ok()?); p += 2;
            let y1 = i16::from_le_bytes(buf[p..p+2].try_into().ok()?); p += 2;
            if y0 != EMPTY_I16 && y1 != EMPTY_I16 {
                w.fill_column(x, z, y0 as i32, y1 as i32);
            }
        }
    }
    Some(w)
}