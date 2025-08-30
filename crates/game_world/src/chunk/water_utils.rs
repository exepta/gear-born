use crate::chunk::chunk_utils::col_rand_u32;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use game_core::world::block::VOXEL_SIZE;
use game_core::world::chunk::{ChunkData, ChunkMap};
use game_core::world::chunk_dim::*;
use game_core::world::fluid::{FluidChunk, FluidMap, WaterMeshIndex};
use game_core::world::save::{chunk_to_region, pack_slot_bytes, unpack_slot_bytes, RegionCache, WorldSave};

const WATER_MAGIC: u32 = 0x3152_5457;

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

pub fn save_water_chunk_sync(ws: &WorldSave, cache: &mut RegionCache, coord: IVec2, w: &FluidChunk) {
    let rc = chunk_to_region(coord);
    let rf = match cache.get_or_open(ws, rc) {
        Ok(rf) => rf,
        Err(e) => { warn!("save_water_chunk_sync: open failed: {}", e); return; }
    };

    let existing = rf.read_chunk(coord).unwrap_or_else(|e| {
        warn!("read_slot failed: {}", e);
        None
    });
    let (old_chunk, _old_water) = if let Some(ref buf) = existing { unpack_slot_bytes(buf) } else { (None, None) };

    let w_bytes = encode_fluid_chunk(w);
    let packed = pack_slot_bytes(old_chunk, Some(&w_bytes));
    if let Err(e) = rf.write_chunk(coord, &packed) {
        warn!("save_water_chunk_sync: write failed: {}", e);
    }
}

pub fn load_water_chunk_sync(ws: &WorldSave, cache: &mut RegionCache, coord: IVec2) -> Option<FluidChunk> {
    let rc = chunk_to_region(coord);
    let rf = cache.get_or_open(ws, rc).ok()?;
    let buf = rf.read_chunk(coord).ok()??;
    let (_cb, wb) = unpack_slot_bytes(&buf);
    if let Some(wb) = wb { decode_fluid_chunk(wb) } else { None }
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
    let total_bits = CX * CY * CZ;
    let n_words = (total_bits + 63) / 64;
    let mut words = vec![0u64; n_words];

    let mut i = 0usize;
    for y in 0..CY { for z in 0..CZ { for x in 0..CX {
        if w.get(x,y,z) {
            words[i >> 6] |= 1u64 << (i & 63);
        }
        i += 1;
    }}}

    let mut buf = Vec::with_capacity(12 + n_words * 8);
    buf.extend_from_slice(&WATER_MAGIC.to_le_bytes());
    buf.extend_from_slice(&w.sea_level.to_le_bytes());
    buf.extend_from_slice(&(n_words as u32).to_le_bytes());
    for w64 in words { buf.extend_from_slice(&w64.to_le_bytes()); }
    buf
}

fn decode_fluid_chunk(bytes: &[u8]) -> Option<FluidChunk> {
    if bytes.len() < 12 { return None; }
    let mut p = 0usize;

    let mut b4 = [0u8;4]; b4.copy_from_slice(&bytes[p..p+4]); p+=4;
    if u32::from_le_bytes(b4) != WATER_MAGIC { return None; }

    let mut b_sea = [0u8;4]; b_sea.copy_from_slice(&bytes[p..p+4]); p+=4;
    let sea_level = i32::from_le_bytes(b_sea);

    let mut b_n = [0u8;4]; b_n.copy_from_slice(&bytes[p..p+4]); p+=4;
    let n_words = u32::from_le_bytes(b_n) as usize;

    if bytes.len() < p + n_words*8 { return None; }

    let mut words = Vec::with_capacity(n_words);
    for _ in 0..n_words {
        let mut w64b = [0u8;8];
        w64b.copy_from_slice(&bytes[p..p+8]); p+=8;
        words.push(u64::from_le_bytes(w64b));
    }

    let mut fc = FluidChunk::new(sea_level);
    let mut i = 0usize;
    for y in 0..CY { for z in 0..CZ { for x in 0..CX {
        let wi = i >> 6; let bi = i & 63;
        if wi < words.len() && ((words[wi] >> bi) & 1) == 1 { fc.set(x,y,z,true); }
        i += 1;
    }}}
    Some(fc)
}