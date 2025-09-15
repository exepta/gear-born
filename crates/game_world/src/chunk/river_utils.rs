use bevy::prelude::*;
// river_utils.rs
use fastnoise_lite::*;

/// All calculations are deterministic per world seed.
/// The river "network" is continuously noise-based and then gated per biome
/// (enabled/disabled + chance), with neighbor continuation support.
///
/// Design notes:
/// - We compute a global river *potential* from ridged OpenSimplex + small warp,
///   then convert it to a soft mask [0..1] per column.
/// - A coarse tile hash (stable per ~512x512 world cells) produces a "keep" value
///   and a random width for any river bits in that tile, so widths stay stable across chunks.
/// - Chance gating: if `keep < chance`, rivers can appear locally. If chance==0.0
///   we only allow carving if a neighbor edge already has strong potential (continuation).
/// - `rivers == false` hard-blocks generation and pass-through at sample points.
/// - **Patch**: smooth tile gating and longitudinal smoothing to avoid hard river ends.

pub struct RiverSystem {
    seed: i32,
    river_n: FastNoiseLite,
    warp_n: FastNoiseLite,
}

impl RiverSystem {
    pub fn new(seed: i32) -> Self {
        // Base noise making long, meandering bands near zero
        let mut river_n = FastNoiseLite::with_seed(seed ^ 0x52495631); // 'RIV1'
        river_n.set_noise_type(Some(NoiseType::OpenSimplex2));
        river_n.set_frequency(Some(0.0018));      // ~555 blocks period
        river_n.set_fractal_type(Some(FractalType::FBm));
        river_n.set_fractal_octaves(Some(3));
        river_n.set_fractal_gain(Some(0.5));
        river_n.set_fractal_lacunarity(Some(2.0));

        // Small warp for meanders
        let mut warp_n = FastNoiseLite::with_seed(seed ^ 0x57415250); // 'WARP'
        warp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
        warp_n.set_frequency(Some(0.010));        // faster
        warp_n.set_fractal_type(Some(FractalType::FBm));
        warp_n.set_fractal_octaves(Some(2));
        warp_n.set_fractal_gain(Some(0.5));
        warp_n.set_fractal_lacunarity(Some(2.0));

        Self { seed, river_n, warp_n }
    }

    /// Potential [0..1] – smooth Gaussian around the river center line.
    /// This yields soft banks and avoids hard, terrace-like edges.
    pub fn potential(&self, wxf: f32, wzf: f32, width_blocks: i32) -> f32 {
        if width_blocks <= 0 { return 0.0; }

        // Small warp for meanders (world-space)
        let wxw = wxf + self.warp_n.get_noise_2d(wxf * 0.06, wzf * 0.06) * 12.0;
        let wzw = wzf + self.warp_n.get_noise_2d(wxf * 0.07, wzf * 0.07) * 12.0;

        // Base noise and finite-difference gradient in world units (~blocks)
        let n   = self.river_n.get_noise_2d(wxw, wzw);
        let eps = 1.0; // 1 block
        let gx  = (self.river_n.get_noise_2d(wxw + eps, wzw) - self.river_n.get_noise_2d(wxw - eps, wzw)) * 0.5;
        let gz  = (self.river_n.get_noise_2d(wxw, wzw + eps) - self.river_n.get_noise_2d(wxw, wzw - eps)) * 0.5;
        let grad = (gx * gx + gz * gz).sqrt().max(1e-4);

        // Approximate world-space distance to the river center line
        let d_world = n.abs() / grad;

        // Interpret width as full bank-to-bank at ~p=0.1. half = width/2.
        let half = (width_blocks as f32).max(2.0) * 0.5;

        // For a Gaussian p(d)=exp(-0.5*(d/sigma)^2), choose sigma so that p(half)=0.1:
        // sigma = half / sqrt(2*ln(10))  ≈ half / 2.146
        let sigma = half / 2.146;

        // Hard cap to avoid any spread beyond ~125% of the requested half-width
        let max_d = half * 1.25;
        if d_world > max_d { return 0.0; }

        // Final smooth potential
        let p = (-0.5 * (d_world / sigma).powi(2)).exp();
        p.clamp(0.0, 1.0)
    }

    /// Channel shaping function: returns the *new* carved height (lower = deeper).
    /// - Banks end just below sea level to "kiss" shoreline blocks.
    /// - Center depth scales with requested river width.
    /// - Nonlinear bias keeps the bottom from becoming unnaturally flat.
    pub fn carve_height(
        &self,
        current_h: f32,
        potential: f32,     // [0..1], typically from potential_gated_smoothed
        width_blocks: i32,  // full bank-to-bank width in blocks
        sea_level: f32,
    ) -> f32 {
        if potential <= 0.001 || width_blocks <= 0 {
            return current_h;
        }

        // Bias towards a center to make banks soft and the thalweg deeper
        let p_center = potential.powf(1.35);

        // Target profile parameters derived from width
        let bank_submerge = 0.60 + (width_blocks as f32) * 0.02; // ~0.6. blocks under sea at banks
        let center_depth  = 1.20 + (width_blocks as f32) * 0.22; // ~1.2. + scales with width

        let bank_target   = sea_level - bank_submerge;
        let center_target = sea_level - center_depth;

        // Interpolate desired profile with central bias
        let desired = bank_target + (center_target - bank_target) * p_center;

        // Move height towards the profile; carve down strongly, lift very little
        if current_h > desired {
            current_h - (current_h - desired) * p_center
        } else {
            // Already below the target: apply a tiny smoothing to avoid hard shelves
            current_h - 0.35 * p_center
        }
    }

    /// Coarse, stable "tile id" (~512x512) for chance and width hashing.
    #[inline]
    pub fn tile_of(wx: i32, wz: i32) -> IVec2 {
        const TILE: i32 = 512;
        IVec2::new(div_floor(wx, TILE), div_floor(wz, TILE))
    }

    /// Deterministic keep value [0..1] per coarse tile (independent of biome),
    /// compared against the biome's `river_chance`.
    pub fn tile_keep_value(&self, wx: i32, wz: i32) -> f32 {
        let t = Self::tile_of(wx, wz);
        u64_to_unit01(split_mix_64(hash3(self.seed as i64, t.x as i64, t.y as i64)))
    }

    /// Deterministic river width (in blocks) for the tile, within [min,max].
    pub fn tile_width_blocks(&self, wx: i32, wz: i32, range: (i32, i32)) -> i32 {
        let (lo, hi) = range;
        let lo = lo.max(1);
        let hi = hi.max(lo);
        let span = (hi - lo + 1) as u32;

        let t = Self::tile_of(wx, wz);
        let r = split_mix_64(hash3(self.seed as i64 ^ 0x57494454, t.x as i64, t.y as i64)); // ^ 'WIDTH'
        lo + (r as u32 % span) as i32
    }

    /// Detect whether any neighbor *edge* (just outside this chunk of bounds) has
    /// strong river potential. Used to let rivers *continue through* a biome
    /// even when its `river_chance == 0.0`.
    pub fn neighbor_continuation_for_chunk(
        &self,
        chunk_origin_wx: i32,
        chunk_origin_wz: i32,
        cx: usize,
        cz: usize,
        test_width: i32,
    ) -> bool {
        let mut max_p = 0.0f32;

        // sample every 2 cells along each edge, 1 cell outside
        let step = 2i32;
        let x0 = chunk_origin_wx;
        let z0 = chunk_origin_wz;
        let x1 = x0 + cx as i32 - 1;
        let z1 = z0 + cz as i32 - 1;

        // left/right edges (x-1 and x1+1)
        for dz in (0..cz as i32).step_by(step as usize) {
            let z = z0 + dz;
            max_p = max_p.max(self.potential((x0 - 1) as f32, z as f32, test_width));
            max_p = max_p.max(self.potential((x1 + 1) as f32, z as f32, test_width));
        }
        // top/bottom edges (z-1 and z1+1)
        for dx in (0..cx as i32).step_by(step as usize) {
            let x = x0 + dx;
            max_p = max_p.max(self.potential(x as f32, (z0 - 1) as f32, test_width));
            max_p = max_p.max(self.potential(x as f32, (z1 + 1) as f32, test_width));
        }

        max_p >= 0.60
    }

    pub fn potential_with_dir(&self, wxf: f32, wzf: f32, width_blocks: i32) -> (f32, Vec2) {
        if width_blocks <= 0 { return (0.0, Vec2::ZERO); }

        let wxw = wxf + self.warp_n.get_noise_2d(wxf * 0.06, wzf * 0.06) * 12.0;
        let wzw = wzf + self.warp_n.get_noise_2d(wxf * 0.07, wzf * 0.07) * 12.0;

        let n   = self.river_n.get_noise_2d(wxw, wzw);
        let eps = 1.0;
        let gx  = (self.river_n.get_noise_2d(wxw + eps, wzw) - self.river_n.get_noise_2d(wxw - eps, wzw)) * 0.5;
        let gz  = (self.river_n.get_noise_2d(wxw, wzw + eps) - self.river_n.get_noise_2d(wxw, wzw - eps)) * 0.5;
        let grad = (gx * gx + gz * gz).sqrt().max(1e-4);

        // Tangent along the course (perpendicular to gradient)
        let mut t = Vec2::new(-gz, gx);
        let len = t.length();
        if len > 1e-4 { t /= len; } else { t = Vec2::ZERO; }

        // Distance-field potential (same as in the gradient-normalized version)
        let d_world = n.abs() / grad;
        let half = (width_blocks as f32).max(2.0) * 0.5;
        let sigma = half / 2.146;
        let max_d = half * 1.25;
        let p = if d_world > max_d { 0.0 } else { (-0.5 * (d_world / sigma).powi(2)).exp() };

        (p.clamp(0.0, 1.0), t)
    }

    /* ------------ PATCH ADDITIONS: smooth gating & end smoothing -------- */

    /// Bilinear interpolation of the coarse "keep" field over 512x512 tiles.
    /// English: makes chance gating spatially smooth instead of abrupt per-tile.
    fn tile_keep_bi_lerp(&self, wx: i32, wz: i32) -> f32 {
        const TILE: i32 = 512;
        let tx = div_floor(wx, TILE);
        let tz = div_floor(wz, TILE);
        let fx = (wx - tx * TILE) as f32 / TILE as f32;
        let fz = (wz - tz * TILE) as f32 / TILE as f32;

        let k00 = self.tile_keep_value(tx * TILE,     tz * TILE    );
        let k10 = self.tile_keep_value((tx+1)*TILE,   tz * TILE    );
        let k01 = self.tile_keep_value(tx * TILE,    (tz+1)*TILE   );
        let k11 = self.tile_keep_value((tx+1)*TILE,  (tz+1)*TILE   );

        let k0 = k00 + (k10 - k00) * fx;
        let k1 = k01 + (k11 - k01) * fx;
        k0 + (k1 - k0) * fz
    }

    /// Smooth gate in [0,1] for a given local river chance.
    /// English: returns how much of the river you keep at (wx,wz).
    fn gate_mask(&self, wx: i32, wz: i32, river_chance: f32) -> f32 {
        // soften the threshold by ~±0.08 to avoid "knife" transitions
        let thr = river_chance.clamp(0.0, 1.0);
        let span = 0.08;
        let k = self.tile_keep_bi_lerp(wx, wz);
        // smoothstep(lo,hi, k)
        let t = ((k - (thr - span)) / (2.0 * span)).clamp(0.0, 1.0);
        t * t * (3.0 - 2.0 * t)
    }

    /// Potential with soft gating and longitudinal smoothing.
    /// - gates by biome/tile chance smoothly
    /// - additionally smooths along the river tangent, so ends fade out
    pub fn potential_gated_smoothed(
        &self,
        wxf: f32,
        wzf: f32,
        width_blocks: i32,
        river_chance: f32,
        rivers_enabled: bool,
    ) -> f32 {
        if width_blocks <= 0 { return 0.0; }

        let (p0, t) = self.potential_with_dir(wxf, wzf, width_blocks);

        // If biome toggles rivers off, a gate becomes 0 (still allow smoothing to soften ends)
        let gate = if rivers_enabled {
            self.gate_mask(wxf.floor() as i32, wzf.floor() as i32, river_chance)
        } else {
            0.0
        };

        // --- Longitudinal smoothing (samples along tangent) ---
        // English: average p a little bit along the course to avoid a step at the end.
        // Distances in world blocks; keep small to preserve detail.
        let s1 = 8.0;
        let s2 = 16.0;
        let (tx, tz) = if t.length_squared() > 1e-6 { (t.x, t.y) } else { (0.0, 0.0) };

        let p_f  = self.potential(wxf + tx * s1, wzf + tz * s1, width_blocks);
        let p_b  = self.potential(wxf - tx * s1, wzf - tz * s1, width_blocks);
        let p_f2 = self.potential(wxf + tx * s2, wzf + tz * s2, width_blocks);
        let p_b2 = self.potential(wxf - tx * s2, wzf - tz * s2, width_blocks);

        // Also gate neighbors smoothly (prevents a gated neighbor from dropping to zero)
        let g_f  = self.gate_mask((wxf + tx * s1).floor() as i32, (wzf + tz * s1).floor() as i32, river_chance);
        let g_b  = self.gate_mask((wxf - tx * s1).floor() as i32, (wzf - tz * s1).floor() as i32, river_chance);
        let g_f2 = self.gate_mask((wxf + tx * s2).floor() as i32, (wzf + tz * s2).floor() as i32, river_chance);
        let g_b2 = self.gate_mask((wxf - tx * s2).floor() as i32, (wzf - tz * s2).floor() as i32, river_chance);

        // 5-tap, symmetric weights (sum=1)
        let p_smooth =
            0.40 * (p0 * gate) +
                0.20 * (p_f * g_f + p_b * g_b) +
                0.10 * (p_f2 * g_f2 + p_b2 * g_b2);

        p_smooth.clamp(0.0, 1.0)
    }
}

/* ===================== small helpers ====================== */

#[inline]
fn div_floor(a: i32, b: i32) -> i32 {
    // floor division for negatives
    if (a ^ b) >= 0 { a / b } else { -(((-a) + b - 1) / b) }
}

#[inline]
fn hash3(a: i64, b: i64, c: i64) -> u64 {
    // 64-bit mix from 3 inputs
    let mut x = (a as u64).wrapping_mul(0x9E3779B97F4A7C15);
    x ^= (b as u64).rotate_left(21).wrapping_mul(0xBF58476D1CE4E5B9);
    x ^= (c as u64).rotate_left(43).wrapping_mul(0x94D049BB133111EB);
    split_mix_64(x)
}

#[inline]
fn split_mix_64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E3779B97F4A7C15);
    let mut z = x;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
    z ^ (z >> 31)
}

#[inline]
fn u64_to_unit01(x: u64) -> f32 {
    // to [0,1)
    const INV: f64 = 1.0 / ((1u64 << 53) as f64);
    let v = (x >> 11) as f64 * INV;
    v as f32
}
