use bevy::prelude::*;
use crate::world::block::Face;

#[derive(Resource, Default)]
pub struct SelectionState {
    pub hit: Option<BlockHit>,
}

#[derive(Clone, Copy, Debug)]
pub struct BlockHit {
    pub block_pos: IVec3,
    pub face: Face,
    pub place_pos: IVec3,
}