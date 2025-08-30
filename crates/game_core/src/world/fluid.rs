use crate::world::chunk_dim::*;
use bevy::prelude::*;
use std::collections::HashMap;

#[derive(Resource, Default)]
pub struct FluidMap(pub HashMap<IVec2, FluidChunk>);

#[derive(Resource, Default)]
pub struct WaterMeshIndex(pub HashMap<(IVec2, u8), Entity>);

#[derive(Clone)]
pub struct FluidChunk {
    pub sea_level: i32,
    pub bits: Vec<u64>,
}

impl FluidChunk {
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


    #[inline]
    fn bit_len() -> usize { CX * CY * CZ }
    #[inline]
    fn idx(x: usize, y: usize, z: usize) -> usize {
        (y * CZ + z) * CX + x
    }
    pub fn new(sea_level: i32) -> Self {
        let n = (Self::bit_len() + 63) / 64;
        Self { sea_level, bits: vec![0u64; n] }
    }
    #[inline]
    pub fn get(&self, x: usize, y: usize, z: usize) -> bool {
        let i = Self::idx(x,y,z);
        (self.bits[i>>6] >> (i & 63)) & 1 == 1
    }
    #[inline]
    pub fn set(&mut self, x: usize, y: usize, z: usize, on: bool) {
        let i = Self::idx(x,y,z);
        let w = &mut self.bits[i>>6];
        let m = 1u64 << (i & 63);
        if on { *w |= m; } else { *w &= !m; }
    }
    #[inline]
    pub fn fill_column(&mut self, x: usize, z: usize, y0: i32, y1: i32) {
        let lo = y0.max(Y_MIN);
        let hi = y1.min(Y_MIN + CY as i32 - 1);
        if lo > hi { return; }

        for wy in lo..=hi {
            let ly = (wy - Y_MIN) as usize; // 0..CY-1
            self.set(x, ly, z, true);
        }
    }
}