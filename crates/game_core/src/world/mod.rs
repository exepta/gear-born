pub mod block;
pub mod chunk_dim;
pub mod chunk;
pub mod save;
pub mod fluid;
pub mod biome;
pub mod biome_plate;

use crate::events::chunk_events::SubChunkNeedRemeshEvent;
use crate::player::selection::BlockHit;
use crate::world::block::{get_block_world, BlockId, Face};
use crate::world::chunk::{ChunkData, ChunkMap};
use crate::world::chunk_dim::{world_to_chunk_xz, world_y_to_local, CX, CY, CZ, SEC_COUNT, SEC_H, Y_MAX, Y_MIN};
use bevy::prelude::*;

pub struct WorldMutAccess<'a> {
    pub chunk: &'a mut ChunkData,
    pub lx: usize,
    pub ly: usize,
    pub lz: usize,
    pub sub: usize,
}
impl<'a> WorldMutAccess<'a> {
    #[inline] pub fn get(&self) -> BlockId { self.chunk.get(self.lx, self.ly, self.lz) }
    #[inline] pub fn set(&mut self, id: BlockId) {
        self.chunk.set(self.lx, self.ly, self.lz, id);
        self.chunk.mark_dirty_local_y(self.sub);
    }
}

pub fn world_access_mut(chunk_map: &'_ mut ChunkMap, wp: IVec3)
                        -> Option<WorldMutAccess<'_>>
{
    if wp.y < Y_MIN || wp.y > Y_MAX { return None; }
    let (cc, local) = world_to_chunk_xz(wp.x, wp.z);
    let chunk = chunk_map.chunks.get_mut(&cc)?;
    let lx = local.x as usize;
    let lz = local.y as usize;
    let ly = world_y_to_local(wp.y);
    if lx >= CX || lz >= CZ || ly >= CY { return None; }
    let sub = ly / SEC_H;
    Some(WorldMutAccess { chunk, lx, ly, lz, sub })
}

pub fn mark_dirty_block_and_neighbors(
    chunk_map: &mut ChunkMap,
    wp: IVec3,
    ev: &mut EventWriter<SubChunkNeedRemeshEvent>,
) {
    const OFFS: [IVec3; 7] = [
        IVec3::new(0, 0, 0),
        IVec3::new( 1, 0,  0),
        IVec3::new(-1, 0,  0),
        IVec3::new( 0, 1,  0),
        IVec3::new( 0,-1,  0),
        IVec3::new( 0, 0,  1),
        IVec3::new( 0, 0, -1),
    ];

    for o in OFFS {
        let p = wp + o;
        if p.y < Y_MIN || p.y > Y_MAX { continue; }

        let (coord, local_xz) = world_to_chunk_xz(p.x, p.z);
        let ly = world_y_to_local(p.y);
        let sub = ly / SEC_H;

        let mut mark = |c: IVec2, s: usize| {
            if s < SEC_COUNT {
                if let Some(ch) = chunk_map.chunks.get_mut(&c) {
                    ch.mark_dirty_local_y(s);
                    ev.write(SubChunkNeedRemeshEvent { coord: c, sub: s });
                }
            }
        };

        mark(coord, sub);
        if ly % SEC_H == 0 && sub > 0            { mark(coord, sub - 1); }
        if ly % SEC_H == SEC_H - 1 && sub + 1 < SEC_COUNT { mark(coord, sub + 1); }

        let _ = local_xz;
    }
}

pub fn ray_cast_voxels(origin: Vec3, dir_in: Vec3, max_dist: f32, chunk_map: &ChunkMap) -> Option<BlockHit> {
    let dir = dir_in.normalize_or_zero();
    if dir == Vec3::ZERO { return None; }

    let mut x = origin.x.floor() as i32;
    let mut y = origin.y.floor() as i32;
    let mut z = origin.z.floor() as i32;

    let step_x = if dir.x > 0.0 { 1 } else if dir.x < 0.0 { -1 } else { 0 };
    let step_y = if dir.y > 0.0 { 1 } else if dir.y < 0.0 { -1 } else { 0 };
    let step_z = if dir.z > 0.0 { 1 } else if dir.z < 0.0 { -1 } else { 0 };

    let next_boundary = |p: f32, step: i32| -> f32 {
        if step > 0 { p.floor() + 1.0 } else { p.ceil() - 1.0 }
    };

    let mut t_max_x = if step_x != 0 { (next_boundary(origin.x, step_x) - origin.x) / dir.x } else { f32::INFINITY };
    let mut t_max_y = if step_y != 0 { (next_boundary(origin.y, step_y) - origin.y) / dir.y } else { f32::INFINITY };
    let mut t_max_z = if step_z != 0 { (next_boundary(origin.z, step_z) - origin.z) / dir.z } else { f32::INFINITY };

    let t_delta_x = if step_x != 0 { 1.0 / dir.x.abs() } else { f32::INFINITY };
    let t_delta_y = if step_y != 0 { 1.0 / dir.y.abs() } else { f32::INFINITY };
    let t_delta_z = if step_z != 0 { 1.0 / dir.z.abs() } else { f32::INFINITY };

    let mut last_empty = IVec3::new(x, y, z);
    let mut t: f32;

    for _ in 0..512 {
        let id = get_block_world(chunk_map, IVec3::new(x, y, z));
        if id != 0 {
            let face = if t_max_x < t_max_y && t_max_x < t_max_z {
                if step_x > 0 { Face::West } else { Face::East }
            } else if t_max_y < t_max_z {
                if step_y > 0 { Face::Bottom } else { Face::Top }
            } else {
                if step_z > 0 { Face::North } else { Face::South }
            };

            return Some(BlockHit {
                block_pos: IVec3::new(x, y, z),
                face,
                place_pos: last_empty,
            });
        }

        last_empty = IVec3::new(x, y, z);

        if t_max_x < t_max_y && t_max_x < t_max_z {
            x += step_x;
            t = t_max_x;
            t_max_x += t_delta_x;
        } else if t_max_y < t_max_z {
            y += step_y;
            t = t_max_y;
            t_max_y += t_delta_y;
        } else {
            z += step_z;
            t = t_max_z;
            t_max_z += t_delta_z;
        }

        if t > max_dist { break; }
        if y < Y_MIN || y > Y_MAX { break; }
    }

    None
}