use bevy::prelude::*;

/// Event emitted when a chunk at `coord` (XZ) has been generated and is ready.
#[derive(Message, Clone, Copy)]
pub struct ChunkGeneratedEvent {
    pub coord: IVec2,
}

/// Event signaling that a specific subchunk needs re-meshing / re-upload.
///
/// `sub` is the vertical section index (`0.SEC_COUNT`).
#[derive(Message, Clone, Copy)]
pub struct SubChunkNeedRemeshEvent {
    pub coord: IVec2,
    pub sub: usize,
}

#[derive(Message, Clone, Copy, Debug)]
pub struct ChunkUnloadEvent {
    pub coord: IVec2,
}
