use bevy::prelude::*;

pub const CX: usize = 32;
pub const CY: usize = 384;
pub const CZ: usize = 32;

pub const Y_MIN: i32 = -128;
pub const Y_MAX: i32 = Y_MIN + CY as i32 - 1;

pub const SEC_H: usize = 16;
pub const SEC_COUNT: usize = CY / SEC_H;

#[inline]
pub fn world_to_chunk_xz(x: i32, z: i32) -> (IVec2, UVec2) {
    let cx = x.div_euclid(CX as i32);
    let cz = z.div_euclid(CZ as i32);
    let lx = x.rem_euclid(CX as i32) as u32;
    let lz = z.rem_euclid(CZ as i32) as u32;
    (IVec2::new(cx, cz), UVec2::new(lx, lz))
}

#[inline]
pub fn world_y_to_local(y: i32) -> usize {
    debug_assert!((Y_MIN..=Y_MAX).contains(&y));
    (y - Y_MIN) as usize
}

pub fn local_y_to_world(ly: usize) -> i32 {
    (ly as i32) + Y_MIN
}