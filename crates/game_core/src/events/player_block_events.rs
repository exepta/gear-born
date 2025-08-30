use bevy::prelude::*;

#[derive(Event, Clone, Debug)]
pub struct BlockBreakByPlayerEvent {
    pub chunk_coord: IVec2,
    pub location: IVec3,
    pub chunk_x: u8,
    pub chunk_y: u16,
    pub chunk_z: u8,
    pub block_id: u16,
    pub block_name: String
}

#[derive(Event, Clone, Debug)]
pub struct BlockPlaceByPlayerEvent {
    pub location: IVec3,
    pub block_id: u16,
    pub block_name: String
}