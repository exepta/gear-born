use crate::world::block::Face;
use bevy::prelude::*;

/// Global selection state for the player/editor.
///
/// Stores the most recent block recast result (`hit`), or `None` if nothing
/// is under the cursor / in reach.
#[derive(Resource, Default)]
pub struct SelectionState {
    /// Last block intersection, if any.
    pub hit: Option<BlockHit>,
}

/// Result of a voxel/block recast.
///
/// Semantics used throughout typical voxel editors:
/// - `block_pos`: integer world coordinates of the **solid** block that was hit.
/// - `face`: which face of `block_pos` was intersected (e.g. +X, -Y, etc.).
/// - `place_pos`: the adjacent block position on the **outside** of `block_pos`
///   along `face` — i.e., where you'd place a new block when targeting this face.
#[derive(Clone, Copy, Debug)]
pub struct BlockHit {
    /// World-space integer coordinates of the hit block.
    pub block_pos: IVec3,
    /// The face of the hit block that was intersected.
    pub face: Face,
    /// Neighbor cell where a new block would be placed when clicking this face.
    pub place_pos: IVec3,
}