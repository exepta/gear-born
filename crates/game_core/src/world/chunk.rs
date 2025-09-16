use bevy::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::world::block::{BlockId, DIR4_XZ};
use crate::world::chunk_dim::*;

pub const BIG: usize = 175;
pub const MAX_UPDATE_FRAMES: usize = 32;

pub const SEA_LEVEL: i32 = 60;

#[inline]
pub fn idx(x: usize, y: usize, z: usize) -> usize { (y * CZ + z) * CX + x }

#[inline]
pub fn index3_to_xyz(i: usize) -> (usize, usize, usize) {
    let y_z = i / CX;
    let x   = i - y_z * CX;
    let y   = y_z / CZ;
    let z   = y_z - y * CZ;
    (x, y, z)
}

#[inline] pub fn in_bounds(x: usize, y: usize, z: usize) -> bool { x < CX && y < CY && z < CZ }

/// Tracks which chunks still need caves and which are already done.
#[derive(Resource, Default, Debug)]
pub struct CaveTracker {
    /// Pending chunk coords to process (FIFO).
    pub pending: VecDeque<IVec2>,
    /// Set of chunks that have already been carved (to avoid double work).
    pub done: HashSet<IVec2>,
}

#[derive(Clone)]
pub struct ChunkData {
    pub blocks: Vec<BlockId>,
    pub dirty_mask: u32,
}

impl ChunkData {
    pub fn new() -> Self {
        Self {
            blocks: vec![0; CX * CY * CZ],
            dirty_mask: u32::MAX >> (32 - SEC_COUNT),
        }
    }

    #[inline]
    pub fn get(&self, x: usize, y: usize, z: usize) -> BlockId { self.blocks[idx(x, y, z)] }

    #[inline]
    pub fn set(&mut self, x: usize, y: usize, z: usize, id: BlockId) {
        self.blocks[idx(x, y, z)] = id;
        self.mark_dirty_local_y(y);
    }

    #[inline]
    pub fn get_opt(&self, x: usize, y: usize, z: usize) -> Option<BlockId> {
        if in_bounds(x, y, z) { Some(self.get(x, y, z)) } else { None }
    }

    #[inline]
    pub fn swap_set(&mut self, x: usize, y: usize, z: usize, id: BlockId) -> BlockId {
        let i = idx(x, y, z);
        let old = self.blocks[i];
        self.blocks[i] = id;
        self.mark_dirty_local_y(y);
        old
    }

    #[inline]
    pub fn mark_dirty_local_y(&mut self, ly: usize) {
        let s = ly / SEC_H;
        self.dirty_mask |= 1 << s;
    }

    #[inline]
    pub fn mark_dirty_sub(&mut self, sub: usize) { self.dirty_mask |= 1 << sub; }

    #[inline]
    pub fn clear_dirty(&mut self, sub: usize) { self.dirty_mask &= !(1 << sub); }

    #[inline]
    pub fn is_dirty(&self, sub: usize) -> bool { (self.dirty_mask & (1 << sub)) != 0 }

    #[inline]
    pub fn mark_all_dirty(&mut self) { self.dirty_mask = u32::MAX >> (32 - SEC_COUNT); }

    #[inline]
    pub fn clear_all_dirty(&mut self) { self.dirty_mask = 0; }

    #[inline]
    pub fn iter_dirty_subs(&self) -> DirtySubsIter {
        DirtySubsIter { mask: self.dirty_mask, i: 0 }
    }

    pub fn fill_layer_y(&mut self, ly: usize, id: BlockId) {
        if ly >= CY { return; }
        let base = ly * CZ * CX;
        for z in 0..CZ {
            let row = base + z * CX;
            for x in 0..CX {
                self.blocks[row + x] = id;
            }
        }
        self.mark_dirty_local_y(ly);
    }

    pub fn fill_column(&mut self, x: usize, z: usize, y0: usize, y1: usize, id: BlockId) {
        if x >= CX || z >= CZ { return; }
        let y1 = y1.min(CY);
        for y in y0.min(y1)..y1 {
            self.set(x, y, z, id);
        }
    }

    pub fn fill_box(&mut self, x0: usize, y0: usize, z0: usize,
                    x1: usize, y1: usize, z1: usize, id: BlockId) {
        let x1 = x1.min(CX);
        let y1 = y1.min(CY);
        let z1 = z1.min(CZ);
        for y in y0.min(y1)..y1 {
            for z in z0.min(z1)..z1 {
                for x in x0.min(x1)..x1 {
                    self.set(x, y, z, id);
                }
            }
        }
    }

    pub fn column_top_local_y(&self, x: usize, z: usize) -> Option<usize> {
        if x >= CX || z >= CZ { return None; }
        for ly in (0..CY).rev() {
            if self.get(x, ly, z) != 0 { return Some(ly); }
        }
        None
    }

    pub fn neighbor_ids6(&self, x: usize, y: usize, z: usize) -> [Option<BlockId>; 6] {
        let mut out = [None; 6];
        let neigh = [
            (x.wrapping_add(1), y, z),
            (x.wrapping_sub(1), y, z),
            (x, y.wrapping_add(1), z),
            (x, y.wrapping_sub(1), z),
            (x, y, z.wrapping_add(1)),
            (x, y, z.wrapping_sub(1)),
        ];
        for (i, (nx, ny, nz)) in neigh.into_iter().enumerate() {
            out[i] = self.get_opt(nx, ny, nz);
        }
        out
    }

    pub fn neighbor_solid_mask6(&self, x: usize, y: usize, z: usize) -> u8 {
        let ids = self.neighbor_ids6(x, y, z);
        let mut m = 0u8;
        for (i, id) in ids.into_iter().enumerate() {
            if id.unwrap_or(0) != 0 { m |= 1 << i; }
        }
        m
    }
}

/* =========================
   Dirty-Iterator
   ========================= */

pub struct DirtySubsIter {
    mask: u32,
    i: usize,
}
impl Iterator for DirtySubsIter {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        while self.i < SEC_COUNT {
            let bit = 1u32 << self.i;
            let idx = self.i;
            self.i += 1;
            if (self.mask & bit) != 0 { return Some(idx); }
        }
        None
    }
}

#[derive(Component, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ChunkCoord(pub IVec3);

#[derive(Component, Default)]
pub struct ChunkDirty;

#[derive(Component)]
pub struct SubchunkMesh {
    pub coord: IVec2,
    pub sub: u8,
    pub block: BlockId,
}

#[derive(Resource, Default)]
pub struct ChunkMeshIndex {
    pub map: HashMap<(IVec2, u8, BlockId), Entity>,
}

#[derive(Resource, Default)]
pub struct ChunkMap {
    pub chunks: HashMap<IVec2, ChunkData>,
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub enum VoxelStage { Input, WorldEdit, Meshing }

#[derive(Resource, Clone, Copy)]
pub struct LoadCenter { pub world_xz: IVec2 }

#[inline] pub fn sub_of_local_y(ly: usize) -> usize { ly / SEC_H }

#[inline]
pub fn sub_range(sub: usize) -> (usize, usize) {
    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);
    (y0, y1)
}

#[inline]
pub fn world_y_to_local_y(wy: i32) -> Option<usize> {
    if wy < Y_MIN || wy > Y_MAX { return None; }
    Some((wy - Y_MIN) as usize)
}

#[inline]
pub fn world_y_to_sub(wy: i32) -> Option<usize> {
    world_y_to_local_y(wy).map(|ly| ly / SEC_H)
}

/* =========================
   ChunkMap-Helpers
   ========================= */

impl ChunkMap {
    #[inline]
    pub fn is_loaded(&self, coord: IVec2) -> bool { self.chunks.contains_key(&coord) }

    #[inline]
    pub fn get_chunk(&self, coord: IVec2) -> Option<&ChunkData> { self.chunks.get(&coord) }

    #[inline]
    pub fn get_chunk_mut(&mut self, coord: IVec2) -> Option<&mut ChunkData> { self.chunks.get_mut(&coord) }

    pub fn ensure_chunk(&mut self, coord: IVec2) -> &mut ChunkData {
        self.chunks.entry(coord).or_insert_with(ChunkData::new)
    }

    pub fn get_world(&self, wx: i32, wy: i32, wz: i32) -> BlockId {
        if wy < Y_MIN || wy > Y_MAX { return 0; }
        let (cc, local) = world_to_chunk_xz(wx, wz);
        let Some(ch) = self.chunks.get(&cc) else { return 0; };
        let lx = local.x as usize;
        let lz = local.y as usize;
        let ly = (wy - Y_MIN) as usize;
        if lx < CX && ly < CY && lz < CZ { ch.get(lx, ly, lz) } else { 0 }
    }

    pub fn set_world(&mut self, wx: i32, wy: i32, wz: i32, id: BlockId) -> Option<BlockId> {
        let (cc, local) = world_to_chunk_xz(wx, wz);
        let lx = local.x as usize;
        let lz = local.y as usize;
        let ly = world_y_to_local_y(wy)?;
        let ch = self.ensure_chunk(cc);
        Some(ch.swap_set(lx, ly, lz, id))
    }

    pub fn mark_dirty_world(&mut self, wx: i32, wy: i32, _wz: i32) {
        if let Some(ly) = world_y_to_local_y(wy) {
            let (cc, _local) = world_to_chunk_xz(wx, _wz);
            if let Some(ch) = self.chunks.get_mut(&cc) {
                ch.mark_dirty_local_y(ly);
            }
        }
    }

    #[inline]
    pub fn neighbors_xz(coord: IVec2) -> [IVec2; 4] {
        let [e, w, s, n] = DIR4_XZ;
        [
            IVec2::new(coord.x + e.x, coord.y + e.y),
            IVec2::new(coord.x + w.x, coord.y + w.y),
            IVec2::new(coord.x + s.x, coord.y + s.y),
            IVec2::new(coord.x + n.x, coord.y + n.y),
        ]
    }

    #[inline]
    pub fn neighbors_ready(&self, coord: IVec2) -> bool {
        Self::neighbors_xz(coord).into_iter().all(|c| self.is_loaded(c))
    }

    pub fn column_tops_iter<'a>(&'a self, coord: IVec2)
                                -> impl 'a + Iterator<Item = ((usize, usize), Option<usize>)>
    {
        self.chunks.get(&coord).into_iter().flat_map(|ch| {
            (0..CZ).flat_map(move |z| (0..CX).map(move |x| ((x, z), ch.column_top_local_y(x, z))))
        })
    }

    pub fn dirty_subs_of(&self, coord: IVec2) -> impl Iterator<Item = usize> + '_ {
        self.chunks.get(&coord).into_iter().flat_map(|c| c.iter_dirty_subs())
    }
}

pub fn sub_priority_order(center_sub: usize) -> impl Iterator<Item = usize> {
    let total = SEC_COUNT;
    let mut seq = Vec::with_capacity(total);
    seq.push(center_sub.min(total - 1));
    let mut d = 1i32;
    while seq.len() < total {
        let a = center_sub as i32 + d;
        if a >= 0 && (a as usize) < total { seq.push(a as usize); }
        d = if d > 0 { -d } else { -d + 1 };
    }
    seq.into_iter()
}

pub fn for_each_chunk_in_radius<F: FnMut(IVec2)>(center: IVec2, radius: i32, mut f: F) {
    for dz in -radius..=radius {
        for dx in -radius..=radius {
            f(IVec2::new(center.x + dx, center.y + dz));
        }
    }
}
