use crate::world::block::BlockId;
use crate::world::chunk_dim::*;
use bevy::prelude::*;
use std::collections::HashMap;

pub const BIG: usize = 160;
pub const MAX_UPDATE_FRAMES: usize = 12;

/// Computes the linear (row-major) index into a `CX × CY × CZ` 3D array.
///
/// Layout: Y-major slices of `CZ` rows, each with `CX` columns:
/// `index = (y * CZ + z) * CX + x`
///
/// # Preconditions
/// Caller must ensure `x < CX`, `y < CY`, `z < CZ`.
#[inline]
pub fn idx(x: usize, y: usize, z: usize) -> usize { (y * CZ + z) * CX + x }

/// Dense voxel payload for a single chunk plus a per-section dirty bitmask.
///
/// - `blocks` is a flat vector of length `CX*CY*CZ`, indexed via [`idx`].
/// - `dirty_mask` uses one bit per vertical section (`SEC_H` tall), LSB = section 0.
///   A set bit means **dirty** (needs re-mesh / re-upload).
#[derive(Clone)]
pub struct ChunkData {
    /// Flat array of block IDs sized `CX*CY*CZ` (row-major, see [`idx`]).
    pub blocks: Vec<BlockId>,
    /// Per-section dirty bits; `bit s` corresponds to Y-range
    /// `s*SEC_H. s*SEC_H + SEC_H`.
    pub dirty_mask: u32,
}

impl ChunkData {
    /// Creates a new chunk initialized with block ID `0` and all sections marked dirty.
    ///
    /// Initializes `dirty_mask` to `((1 << SEC_COUNT) - 1)`.
    pub fn new() -> Self {
        Self { blocks: vec![0; CX * CY * CZ], dirty_mask: u32::MAX >> (32 - SEC_COUNT) }
    }

    /// Returns the block at local coordinates `(x, y, z)`.
    ///
    /// # Panics if indices are out of bounds.
    #[inline]
    pub fn get(&self, x: usize, y: usize, z: usize) -> BlockId { self.blocks[idx(x, y, z)] }

    /// Sets the block at local coordinates `(x, y, z)` and marks the
    /// corresponding vertical section dirty.
    ///
    /// # Panics if indices are out of bounds.
    #[inline]
    pub fn set(&mut self, x: usize, y: usize, z: usize, id: BlockId) {
        self.blocks[idx(x, y, z)] = id;
        self.mark_dirty_local_y(y);
    }

    /// Marks the vertical section containing local Y `ly` as dirty.
    ///
    /// Section index is `ly / SEC_H`.
    #[inline]
    pub fn mark_dirty_local_y(&mut self, ly: usize) {
        let s = ly / SEC_H;
        self.dirty_mask |= 1 << s;
    }

    /// Clears the dirty bit for section `sub` (marks it clean).
    #[inline]
    pub fn clear_dirty(&mut self, sub: usize) { self.dirty_mask &= !(1 << sub); }

    /// Returns `true` if section `sub` is dirty.
    #[inline]
    pub fn is_dirty(&self, sub: usize) -> bool { (self.dirty_mask & (1 << sub)) != 0 }
}

/// World-space integer chunk coordinate (X, Y, Z).
#[derive(Component, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ChunkCoord(pub IVec3);

/// Marker component indicating a chunk requires processing (e.g., meshing/upload).
#[derive(Component, Default)]
pub struct ChunkDirty;

/// Render data for a single subchunk mesh (one vertical section of a chunk).
///
/// Keys used in [`ChunkMeshIndex`] mirror these fields.
#[derive(Component)]
pub struct SubchunkMesh {
    /// Chunk coordinate in XZ (Y is implied by `sub`).
    pub coord: IVec2,
    /// Vertical section index within the chunk (`0.SEC_COUNT`).
    pub sub: u8,
    /// Block/material identifier for this mesh bucket (if meshed per block ID).
    pub block: BlockId,
}

/// Lookup from `(chunk_xz, sub_index, block_id)` to the Bevy `Entity` of the mesh.
///
/// Useful for incremental updates and deduplication of subchunk meshes.
#[derive(Resource, Default)]
pub struct ChunkMeshIndex {
    pub map: HashMap<(IVec2, u8, BlockId), Entity>,
}

/// In-memory map of loaded/generated chunk payloads keyed by XZ chunk coordinate.
#[derive(Resource, Default)]
pub struct ChunkMap {
    pub chunks: HashMap<IVec2, ChunkData>,
}

/// Event emitted when a chunk at `coord` (XZ) has been generated and is ready.
#[derive(Event, Clone, Copy)]
pub struct ChunkGenerated {
    pub coord: IVec2,
}

/// Event signaling that a specific subchunk needs re-meshing / re-upload.
///
/// `sub` is the vertical section index (`0.SEC_COUNT`).
#[derive(Event, Clone, Copy)]
pub struct SubchunkDirty {
    pub coord: IVec2,
    pub sub: usize,
}

#[derive(Event, Clone, Copy, Debug)]
pub struct WaterChunkUnload {
    pub coord: IVec2,
}

/// System set ordering for the voxel pipeline.
///
/// Typical flow: `Input` → `WorldEdit` → `Meshing`.
#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub enum VoxelStage { Input, WorldEdit, Meshing }

/// Center point (in world-space XZ) used to prioritize loading around the player/camera.
#[derive(Resource, Clone, Copy)]
pub struct LoadCenter { pub world_xz: IVec2 }