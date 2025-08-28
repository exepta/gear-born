use std::collections::{HashMap, VecDeque};
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology, VertexAttributeValues};
use bevy::tasks::Task;
use game_core::world::block::{BlockId, BlockRegistry, Face, UvRect};
use game_core::world::chunk::ChunkData;

#[derive(Resource, Default)]
pub(crate) struct MeshBacklog(pub(crate) VecDeque<(IVec2, usize)>);

/// Pending Chunk-Generate-Tasks
#[derive(Resource, Default)]
pub(crate) struct PendingGen(pub(crate) HashMap<IVec2, Task<(IVec2, ChunkData)>>);

/// Pending Mesh-Tasks pro (coord, sub)
#[derive(Resource, Default)]
pub(crate) struct PendingMesh(pub(crate) HashMap<(IVec2, usize), Task<((IVec2, usize), Vec<(BlockId, MeshBuild)>)>>);

#[derive(Clone, Copy)]
pub(crate) struct RegLiteEntry {
    pub(crate) top: UvRect,
    pub(crate) bottom: UvRect,
    pub(crate) north: UvRect,
    pub(crate) east: UvRect,
    pub(crate) south: UvRect,
    pub(crate) west: UvRect,
    pub(crate) opaque: bool,
}
#[derive(Clone)]
pub(crate) struct RegLite {
    pub(crate) map: HashMap<BlockId, RegLiteEntry>,
}

impl RegLite {
    pub(crate) fn from_reg(reg: &BlockRegistry) -> Self {
        let mut map = HashMap::new();
        for &id in reg.name_to_id.values() {
            if id == 0 { continue; }
            map.insert(id, RegLiteEntry {
                top: reg.uv(id, Face::Top),
                bottom: reg.uv(id, Face::Bottom),
                north: reg.uv(id, Face::North),
                east: reg.uv(id, Face::East),
                south: reg.uv(id, Face::South),
                west: reg.uv(id, Face::West),
                opaque: reg.def(id).stats.opaque,
            });
        }
        Self { map }
    }
    #[inline] pub(crate) fn uv(&self, id: BlockId, face: Face) -> UvRect {
        let e = self.map.get(&id).expect("unknown id");
        match face {
            Face::Top => e.top, Face::Bottom => e.bottom, Face::North => e.north,
            Face::East => e.east, Face::South => e.south, Face::West => e.west,
        }
    }
    #[inline] pub(crate) fn opaque(&self, id: BlockId) -> bool {
        self.map.get(&id).map(|e| e.opaque).unwrap_or(false)
    }
}

pub(crate) struct MeshBuild {
    pub(crate) pos: Vec<[f32;3]>,
    pub(crate) nrm: Vec<[f32;3]>,
    pub(crate) uv:  Vec<[f32;2]>,
    pub(crate) idx: Vec<u32>,
}

impl MeshBuild {
    pub(crate) fn new() -> Self { Self { pos:vec![], nrm:vec![], uv:vec![], idx:vec![] } }
    pub(crate) fn quad(&mut self, q:[[f32;3];4], n:[f32;3], uv:[[f32;2];4]) {
        let base = self.pos.len() as u32;
        self.pos.extend_from_slice(&q);
        self.nrm.extend_from_slice(&[n;4]);
        self.uv.extend_from_slice(&uv);
        self.idx.extend_from_slice(&[base,base+1,base+2, base,base+2,base+3]);
    }
    pub(crate) fn into_mesh(self) -> Mesh {
        let mut m = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, self.pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   self.nrm);
        m.insert_attribute(Mesh::ATTRIBUTE_UV_0,     self.uv);

        // <= 65k Vertices? -> U16-Indices
        if self.idx.len() <= u16::MAX as usize {
            let idx_u16: Vec<u16> = self.idx.into_iter().map(|i| i as u16).collect();
            m.insert_indices(Indices::U16(idx_u16));
        } else {
            m.insert_indices(Indices::U32(self.idx));
        }

        m
    }

    #[allow(dead_code)]
    pub(crate) fn mesh_is_empty(m: &Mesh) -> bool {
        match m.attribute(Mesh::ATTRIBUTE_POSITION) {
            Some(VertexAttributeValues::Float32x3(v)) => v.is_empty(),
            Some(VertexAttributeValues::Float32(v)) => v.is_empty(),
            _ => true,
        }
    }
}

#[derive(Clone)]
pub(crate) struct BorderSnapshot {
    pub(crate) y0: usize,
    pub(crate) y1: usize,
    pub(crate) east:  Option<Vec<BlockId>>,
    pub(crate) west:  Option<Vec<BlockId>>,
    pub(crate) south: Option<Vec<BlockId>>,
    pub(crate) north: Option<Vec<BlockId>>,
}