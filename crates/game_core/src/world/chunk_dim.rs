use bevy::prelude::*;

/// Chunk dimensions in cells (X × Y × Z).
pub const CX: usize = 32;
pub const CY: usize = 384;
pub const CZ: usize = 32;

/// Inclusive world-space vertical bounds covered by a single column of chunks.
///
/// Invariants: `CY == (Y_MAX - Y_MIN + 1)`.
pub const Y_MIN: i32 = -128;
pub const Y_MAX: i32 = Y_MIN + CY as i32 - 1;

/// Height of a vertical section within a chunk (in cells).
pub const SEC_H: usize = 16;

/// Number of vertical sections per chunk (`CY / SEC_H`).
pub const SEC_COUNT: usize = CY / SEC_H;

/// Converts world-space `(x, z)` to:
/// - the chunk coordinate (`IVec2`) that contains the position, and
/// - the local-in-chunk coordinate (`UVec2`) in the range `0.CX` × `0.CZ`.
///
/// Uses **Euclidean division** (`div_euclid` / `rem_euclid`) so negative world
/// coordinates map correctly to chunks and local coordinates.
///
/// # Examples
/// ```
/// // World (-1, -1 ) is in chunk ( -1, -1 ), local ( 31, 31 ) for CX=CZ=32.
/// let (cc, lc) = world_to_chunk_xz(-1, -1);
/// assert_eq!(cc, IVec2::new(-1, -1));
/// assert_eq!(lc, UVec2::new(31, 31));
/// ```
#[inline]
pub fn world_to_chunk_xz(x: i32, z: i32) -> (IVec2, UVec2) {
    let cx = x.div_euclid(CX as i32);
    let cz = z.div_euclid(CZ as i32);
    let lx = x.rem_euclid(CX as i32) as u32;
    let lz = z.rem_euclid(CZ as i32) as u32;
    (IVec2::new(cx, cz), UVec2::new(lx, lz))
}

/// Converts a world-space Y coordinate to its local-in-chunk index `0.CY`.
///
/// Panics in the debug build if `y` is outside `[Y_MIN, Y_MAX]`.
#[inline]
pub fn world_y_to_local(y: i32) -> usize {
    debug_assert!((Y_MIN..=Y_MAX).contains(&y));
    (y - Y_MIN) as usize
}

/// Converts a local-in-chunk Y index back to world-space Y.
///
/// Caller must ensure `ly < CY`.
#[inline]
pub fn local_y_to_world(ly: usize) -> i32 {
    (ly as i32) + Y_MIN
}
