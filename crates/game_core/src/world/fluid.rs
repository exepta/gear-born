use crate::world::chunk_dim::*;
use bevy::prelude::*;
use std::collections::HashMap;

#[derive(Resource, Default)]
pub struct FluidMap(pub HashMap<IVec2, FluidChunk>);

#[derive(Resource, Default)]
pub struct WaterMeshIndex(pub HashMap<(IVec2, u8), Entity>);

#[derive(Clone)]
pub struct FluidChunk {
    bits: Vec<u64>,
}

impl Default for FluidChunk {
    fn default() -> Self {
        let tot = (CX * CY * CZ) as usize;
        let words = (tot + 63) / 64;
        Self { bits: vec![0u64; words] }
    }
}

impl FluidChunk {
    #[inline]
    fn idx(x: usize, y: usize, z: usize) -> usize {
        (y * CZ + z) * CX + x
    }

    #[inline]
    pub fn get(&self, x: usize, y: usize, z: usize) -> bool {
        debug_assert!(x < CX && y < CY && z < CZ);
        let i = Self::idx(x, y, z);
        let w = i >> 6;
        let b = i & 63;
        (self.bits[w] >> b) & 1 == 1
    }

    #[inline]
    pub fn set(&mut self, x: usize, y: usize, z: usize, on: bool) {
        debug_assert!(x < CX && y < CY && z < CZ);
        let i = Self::idx(x, y, z);
        let w = i >> 6;
        let b = i & 63;
        let mask = 1u64 << b;
        if on { self.bits[w] |= mask; } else { self.bits[w] &= !mask; }
    }

    pub fn sub_has_any(&self, sub: usize) -> bool {
        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        for y in y0..y1 {
            for z in 0..CZ {
                for x in 0..CX {
                    if self.get(x, y, z) { return true; }
                }
            }
        }
        false
    }
}