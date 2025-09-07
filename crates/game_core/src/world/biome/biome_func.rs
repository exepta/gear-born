#[inline]
pub fn col_rand_u32(x: i32, z: i32, seed: u32) -> u32 {
    // Mix (x, z, seed) into 64-bit, then avalanche (MurMur/xxHash-style).
    let mut h = (x as u64).wrapping_mul(0x517C_C1B7_2722_0A95);
    h ^= (z as u64).wrapping_mul(0x2545_F491_4F6C_DD1D);
    h ^= (seed as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);

    // Finalizers (from Murmur3 fix64)
    h ^= h >> 33;
    h = h.wrapping_mul(0xFF51_AFD7_ED55_8CCD);
    h ^= h >> 33;
    h = h.wrapping_mul(0xC4CE_B9FE_1A85_EC53);
    h ^= h >> 33;

    (h & 0xFFFF_FFFF) as u32
}

/// Same hash but as [0,1) float. Handy for thresholds / roulette wheel.
#[inline]
pub fn col_rand_f32(x: i32, z: i32, seed: u32) -> f32 {
    let u = col_rand_u32(x, z, seed) as f64;
    (u / ((u32::MAX as f64) + 1.0)) as f32
}

/// Integer range helper: inclusive [lo, hi].
#[inline]
pub fn col_rand_range_u32(x: i32, z: i32, seed: u32, lo: u32, hi: u32) -> u32 {
    if lo >= hi { return lo; }
    lo + (col_rand_u32(x, z, seed) % (hi - lo + 1))
}