use crate::world::block::BlockId;
use crate::world::chunk_dim::*;
use bevy::prelude::*;
use std::collections::HashMap;

#[inline] pub fn idx(x:usize, y:usize, z:usize) ->usize { (y*CZ + z)*CX + x }

#[derive(Clone)]
pub struct ChunkData {
    pub blocks: Vec<BlockId>,   // len = CX*CY*CZ = 393_216
    pub dirty_mask: u32,        // 24 Bits (SEC_COUNT=24)
}
impl ChunkData {
    pub fn new() -> Self {
        Self { blocks: vec![0; CX*CY*CZ], dirty_mask: u32::MAX >> (32-SEC_COUNT) }
    }
    #[inline] pub fn get(&self,x:usize,y:usize,z:usize)->BlockId { self.blocks[idx(x,y,z)] }
    #[inline] pub fn set(&mut self,x:usize,y:usize,z:usize,id:BlockId){
        self.blocks[idx(x,y,z)] = id;
        self.mark_dirty_local_y(y);
    }
    #[inline] pub fn mark_dirty_local_y(&mut self, ly: usize) {
        let s = ly / SEC_H;
        self.dirty_mask |= 1 << s;
    }
    #[inline] pub fn clear_dirty(&mut self, sub: usize) { self.dirty_mask &= !(1 << sub); }
    #[inline] pub fn is_dirty(&self, sub: usize) -> bool { (self.dirty_mask & (1 << sub)) != 0 }
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

#[derive(Event, Clone, Copy)]
pub struct ChunkGenerated {
    pub coord: IVec2,
}

#[derive(Event, Clone, Copy)]
pub struct SubchunkDirty {
    pub coord: IVec2,
    pub sub: usize,
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub enum VoxelStage { Input, WorldEdit, Meshing }