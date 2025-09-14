use bevy::prelude::*;
use fastnoise_lite::*;

/* =========================
   Safety / globals
   ========================= */
const Y_CLEARANCE: i32 = 6;

/* =========================
   Parameters & IDs
   ========================= */

#[derive(Debug, Clone)]
pub struct CaveParams {
    /* -------- global / tunnels -------- */
    pub seed: i32,
    pub y_top: i32,
    pub y_bottom: i32,
    pub worms_per_region: f32,
    pub region_chunks: i32,
    pub base_radius: f32,
    pub radius_var: f32,
    pub step_len: f32,
    pub worm_len_steps: i32,
    pub room_event_chance: f32,
    pub room_radius_min: f32,
    pub room_radius_max: f32,

    /* -------- caverns (normal clusters) -------- */
    pub caverns_per_region: f32,
    pub cavern_room_count_min: i32,
    pub cavern_room_count_max: i32,
    pub cavern_room_radius_xz_min: f32,
    pub cavern_room_radius_xz_max: f32,
    pub cavern_room_radius_y_min: f32,
    pub cavern_room_radius_y_max: f32,
    pub cavern_connector_radius: f32,
    pub cavern_y_top: i32,
    pub cavern_y_bottom: i32,

    /* -------- MEGA caverns (rare, very large, noisy) -------- */
    pub mega_caverns_per_region: f32,
    pub mega_room_count_min: i32,
    pub mega_room_count_max: i32,
    pub mega_room_radius_xz_min: f32,
    pub mega_room_radius_xz_max: f32,
    pub mega_room_radius_y_min: f32,
    pub mega_room_radius_y_max: f32,
    pub mega_connector_radius: f32,
    pub mega_y_top: i32,
    pub mega_y_bottom: i32,

    /* -------- entrances (upward spurs near top window) -------- */
    pub entrance_extra_top: i32,     // how far above y_top we may continue carving
    pub entrance_chance: f32,        // probability to spawn an entrance spur near top
    pub entrance_len_steps: i32,     // how many steps an entrance spur will try to climb
    pub entrance_radius_scale: f32,  // scale of entrance radius relative to local tunnel radius
    pub entrance_min_radius: f32,    // clamp minimum radius for the spur
    pub entrance_trigger_band: f32,  // vertical band below y_top where spurs are allowed to start
}

/// IDs kept for compatibility.
#[derive(Copy, Clone, Debug)]
#[allow(dead_code)]
pub struct CaveBlockIds {
    pub air: u32,
    pub water: u32,
    pub protected_1: Option<u32>,
}

/* =========================
   Tiny deterministic RNG
   ========================= */

#[derive(Clone)]
struct Rng(u64);
impl Rng {
    #[inline] fn new(seed: u64) -> Self { Self(seed | 1) }
    #[inline] fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13; x ^= x >> 7; x ^= x << 17;
        self.0 = x; x
    }
    #[inline] fn f01(&mut self) -> f32 {
        (self.next_u64() >> 11) as f32 * (1.0 / ((1u64<<53) as f32))
    }
    #[inline] fn range_f(&mut self, a: f32, b: f32) -> f32 { a + (b - a) * self.f01() }
    #[inline] fn range_i(&mut self, a: i32, b: i32) -> i32 {
        a + ((self.next_u64() % (1 + (b - a) as u64)) as i32)
    }
    #[inline] fn prob(&mut self, p: f32) -> bool { self.f01() < p }
}

#[inline]
fn div_floor(a: i32, b: i32) -> i32 {
    let d = a / b;
    let r = a % b;
    if (r != 0) && ((r < 0) != (b < 0)) { d - 1 } else { d }
}

fn region_seed(world_seed: i32, region: IVec2) -> u64 {
    let a = region.x as i64;
    let b = region.y as i64;
    let mut h = (a.wrapping_mul(0x9E3779B185EBCA87u64 as i64)
        ^ b.wrapping_mul(0xC2B2AE3D27D4EB4Fu64 as i64)) as u64;
    h ^= (world_seed as i64 as u64).wrapping_mul(0xD6E8FEB86659FD93);
    h
}

/// Convert chunk coord to region coord (square grid).
pub fn chunk_to_region(chunk: IVec2, region_chunks: i32) -> IVec2 {
    IVec2::new(div_floor(chunk.x, region_chunks), div_floor(chunk.y, region_chunks))
}

/* =========================
   Tunnels (worms)
   ========================= */

#[derive(Clone, Debug)]
struct Worm {
    start: Vec3,
    dir: Vec3,
    steps: i32,
    base_r: f32,  // horizontal radius
    var_r: f32,   // extra widening
    step_len: f32,
}

fn worms_for_region(params: &CaveParams, region: IVec2, chunk_size: IVec2) -> Vec<Worm> {
    let mut rng = Rng::new(region_seed(params.seed, region));

    let expected = (params.worms_per_region * 1.05).max(0.0);
    let mut count = expected.floor() as i32;
    if rng.prob(expected.fract()) { count += 1; }
    if count == 0 { return Vec::new(); }

    let reg_min = IVec3::new(
        region.x * params.region_chunks * chunk_size.x,
        params.y_bottom,
        region.y * params.region_chunks * chunk_size.y,
    );
    let reg_max = IVec3::new(
        reg_min.x + params.region_chunks * chunk_size.x - 1,
        params.y_top,
        reg_min.z + params.region_chunks * chunk_size.y - 1,
    );

    let mut worms = Vec::with_capacity(count as usize);
    for _ in 0..count {
        let sx = rng.range_i(reg_min.x, reg_max.x) as f32 + 0.5;
        let sz = rng.range_i(reg_min.z, reg_max.z) as f32 + 0.5;
        let sy = rng.range_i(params.y_bottom, params.y_top) as f32 + 0.5;

        let yaw   = rng.range_f(0.0, std::f32::consts::TAU);
        let pitch = rng.range_f(-0.18, 0.18);
        let dir = Vec3::new(yaw.cos() * pitch.cos(), pitch.sin(), yaw.sin() * pitch.cos()).normalize();

        let steps   = (params.worm_len_steps as f32 * rng.range_f(0.9, 1.3)).round() as i32;
        let base_r  = params.base_radius * rng.range_f(0.95, 1.25);
        let step_len = params.step_len.min(base_r * 0.8).max(0.5);

        worms.push(Worm { start: Vec3::new(sx, sy, sz), dir, steps, base_r, var_r: params.radius_var, step_len });
    }
    worms
}

/* =========================
   Caverns (clusters of big rooms)
   ========================= */

#[derive(Clone)]
struct CavernRoom { center: Vec3, rx: f32, ry: f32, rz: f32 }

#[derive(Clone)]
struct CavernCluster {
    rooms: Vec<CavernRoom>,
    connector_r: f32,
    noisy: bool,
}

fn caverns_for_region(params: &CaveParams, region: IVec2, chunk_size: IVec2) -> Vec<CavernCluster> {
    let mut rng = Rng::new(region_seed(params.seed.wrapping_add(0xA5A5_0001u32 as i32), region));
    let expected = params.caverns_per_region.max(0.0);
    let mut count = expected.floor() as i32;
    if rng.prob(expected.fract()) { count += 1; }
    if count == 0 { return Vec::new(); }

    let reg_min = IVec3::new(
        region.x * params.region_chunks * chunk_size.x,
        params.cavern_y_bottom,
        region.y * params.region_chunks * chunk_size.y,
    );
    let reg_max = IVec3::new(
        reg_min.x + params.region_chunks * chunk_size.x - 1,
        params.cavern_y_top,
        reg_min.z + params.region_chunks * chunk_size.y - 1,
    );

    let mut out = Vec::with_capacity(count as usize);
    for _ in 0..count {
        let n = rng.range_i(params.cavern_room_count_min, params.cavern_room_count_max).max(1);
        let mut rooms = Vec::with_capacity(n as usize);

        let cx = rng.range_i(reg_min.x, reg_max.x) as f32 + 0.5;
        let cz = rng.range_i(reg_min.z, reg_max.z) as f32 + 0.5;
        let cy = rng.range_i(params.cavern_y_bottom, params.cavern_y_top) as f32 + 0.5;
        let center = Vec3::new(cx, cy, cz);

        for _ in 0..n {
            let dx = rng.range_f(-24.0, 24.0);
            let dz = rng.range_f(-24.0, 24.0);
            let dy = rng.range_f(-8.0, 8.0);

            let rx = rng.range_f(params.cavern_room_radius_xz_min, params.cavern_room_radius_xz_max);
            let rz = rng.range_f(params.cavern_room_radius_xz_min, params.cavern_room_radius_xz_max);
            let ry = rng.range_f(params.cavern_room_radius_y_min,  params.cavern_room_radius_y_max);

            rooms.push(CavernRoom { center: center + Vec3::new(dx, dy, dz), rx, ry, rz });
        }

        out.push(CavernCluster { rooms, connector_r: params.cavern_connector_radius, noisy: true });
    }

    out
}

fn mega_caverns_for_region(params: &CaveParams, region: IVec2, chunk_size: IVec2) -> Vec<CavernCluster> {
    let mut rng = Rng::new(region_seed(params.seed.wrapping_add(0xDEAD_BEAFu32 as i32), region));
    let expected = params.mega_caverns_per_region.max(0.0);
    let mut count = expected.floor() as i32;
    if rng.prob(expected.fract()) { count += 1; }
    if count == 0 { return Vec::new(); }

    let reg_min = IVec3::new(
        region.x * params.region_chunks * chunk_size.x,
        params.mega_y_bottom,
        region.y * params.region_chunks * chunk_size.y,
    );
    let reg_max = IVec3::new(
        reg_min.x + params.region_chunks * chunk_size.x - 1,
        params.mega_y_top,
        reg_min.z + params.region_chunks * chunk_size.y - 1,
    );

    let mut out = Vec::with_capacity(count as usize);
    for _ in 0..count {
        let n = rng.range_i(params.mega_room_count_min, params.mega_room_count_max).clamp(1, 6);
        let mut rooms = Vec::with_capacity(n as usize);

        let cx = rng.range_i(reg_min.x, reg_max.x) as f32 + 0.5;
        let cz = rng.range_i(reg_min.z, reg_max.z) as f32 + 0.5;
        let cy = rng.range_i(params.mega_y_bottom, params.mega_y_top) as f32 + 0.5;
        let center = Vec3::new(cx, cy, cz);

        for _ in 0..n {
            let dx = rng.range_f(-64.0, 64.0);
            let dz = rng.range_f(-64.0, 64.0);
            let dy = rng.range_f(-18.0, 18.0);

            let rx = rng.range_f(params.mega_room_radius_xz_min, params.mega_room_radius_xz_max);
            let rz = rng.range_f(params.mega_room_radius_xz_min, params.mega_room_radius_xz_max);
            let ry = rng.range_f(params.mega_room_radius_y_min,  params.mega_room_radius_y_max);

            rooms.push(CavernRoom { center: center + Vec3::new(dx, dy, dz), rx, ry, rz });
        }

        out.push(CavernCluster { rooms, connector_r: params.mega_connector_radius, noisy: true });
    }

    out
}

/* =========================
   Geometry helpers
   ========================= */

#[inline]
fn lerp(a: f32, b: f32, t: f32) -> f32 { a + (b - a) * t }

/// Append voxels Ellipsoids. Clamped mit Y_CLEARANCE.
fn append_ellipsoid_into_chunk(
    out: &mut Vec<(u16,u16,u16)>,
    center: Vec3,
    rx: f32, ry: f32, rz: f32,
    wx0: i32, wz0: i32,
    chunk_size: IVec2,
    y_min: i32, y_max: i32,
) {
    let cx = center.x.floor() as i32;
    let cy = center.y.floor() as i32;
    let cz = center.z.floor() as i32;

    let x0 = (cx - rx as i32).max(wx0);
    let x1 = (cx + rx as i32).min(wx0 + chunk_size.x - 1);
    let z0 = (cz - rz as i32).max(wz0);
    let z1 = (cz + rz as i32).min(wz0 + chunk_size.y - 1);

    let y0 = (cy - ry as i32).max(y_min + Y_CLEARANCE);
    let y1 = (cy + ry as i32).min(y_max - Y_CLEARANCE);
    if y1 < y0 { return; }

    let inv_rx2 = 1.0 / (rx * rx).max(1e-6);
    let inv_ry2 = 1.0 / (ry * ry).max(1e-6);
    let inv_rz2 = 1.0 / (rz * rz).max(1e-6);

    for wx in x0..=x1 {
        let dx2 = (wx as f32 - center.x).powi(2) * inv_rx2;
        for wz in z0..=z1 {
            let dz2 = (wz as f32 - center.z).powi(2) * inv_rz2;
            let base = dx2 + dz2;
            if base > 1.0 { continue; }
            for wy in y0..=y1 {
                let dy2 = (wy as f32 - center.y).powi(2) * inv_ry2;
                if base + dy2 <= 1.0 {
                    out.push(((wx - wx0) as u16, (wy - y_min) as u16, (wz - wz0) as u16));
                }
            }
        }
    }
}

/// Noisy, domain-warped Ellipsoid. Y_CLEARANCE.
fn append_noisy_ellipsoid_into_chunk(
    out: &mut Vec<(u16,u16,u16)>,
    center: Vec3,
    rx: f32, ry: f32, rz: f32,
    wx0: i32, wz0: i32,
    chunk_size: IVec2,
    y_min: i32, y_max: i32,
    seed: i32,
    edge_amp: f32,
    edge_freq: f32,
    warp_amp: f32,
    warp_freq: f32,
) {
    let mut edge = FastNoiseLite::with_seed(seed.wrapping_add(0x44_44));
    edge.set_noise_type(Some(NoiseType::OpenSimplex2));
    edge.set_frequency(Some(edge_freq));

    let mut warp = FastNoiseLite::with_seed(seed.wrapping_add(0x77_77));
    warp.set_noise_type(Some(NoiseType::OpenSimplex2));
    warp.set_frequency(Some(warp_freq));

    let cx = center.x.floor() as i32;
    let cy = center.y.floor() as i32;
    let cz = center.z.floor() as i32;

    let x0 = (cx - rx as i32).max(wx0);
    let x1 = (cx + rx as i32).min(wx0 + chunk_size.x - 1);
    let z0 = (cz - rz as i32).max(wz0);
    let z1 = (cz + rz as i32).min(wz0 + chunk_size.y - 1);

    let y0 = (cy - ry as i32).max(y_min + Y_CLEARANCE);
    let y1 = (cy + ry as i32).min(y_max - Y_CLEARANCE);
    if y1 < y0 { return; }

    let inv_rx2 = 1.0 / (rx * rx).max(1e-6);
    let inv_ry2 = 1.0 / (ry * ry).max(1e-6);
    let inv_rz2 = 1.0 / (rz * rz).max(1e-6);

    for wx in x0..=x1 {
        for wz in z0..=z1 {
            for wy in y0..=y1 {
                let wxf = wx as f32; let wyf = wy as f32; let wzf = wz as f32;

                let ox = warp.get_noise_3d(wxf, wyf, wzf) * warp_amp;
                let oy = warp.get_noise_3d(wzf + 31.7, wxf - 12.3, wyf + 7.9) * (warp_amp * 0.5);
                let oz = warp.get_noise_3d(wyf - 19.1, wzf + 8.2, wxf - 4.6) * warp_amp;

                let px = (wxf + ox) - center.x;
                let py = (wyf + oy) - center.y;
                let pz = (wzf + oz) - center.z;

                let base = (px*px)*inv_rx2 + (py*py)*inv_ry2 + (pz*pz)*inv_rz2;

                let n = edge.get_noise_3d(wxf, wyf, wzf); // [-1,1]
                let thresh = 1.0 + n * edge_amp;

                if base <= thresh {
                    out.push(((wx - wx0) as u16, (wy - y_min) as u16, (wz - wz0) as u16));
                }
            }
        }
    }
}

/* =========================
   Public: per-chunk edits
   ========================= */

pub fn worm_edits_for_chunk(
    params: &CaveParams,
    chunk_coord: IVec2,
    chunk_size: IVec2, // (CX, CZ)
    y_min: i32,
    y_max: i32,
) -> Vec<(u16, u16, u16)> {
    let reg = chunk_to_region(chunk_coord, params.region_chunks);

    let mut worms: Vec<Worm> = Vec::new();
    let mut caverns: Vec<CavernCluster> = Vec::new();
    for dz in -1..=1 {
        for dx in -1..=1 {
            let r = IVec2::new(reg.x + dx, reg.y + dz);
            worms.extend(worms_for_region(params, r, chunk_size));
            caverns.extend(caverns_for_region(params, r, chunk_size));
            caverns.extend(mega_caverns_for_region(params, r, chunk_size));
        }
    }

    // Chunk world bounds
    let wx0 = chunk_coord.x * chunk_size.x;
    let wz0 = chunk_coord.y * chunk_size.y;
    let wx1 = wx0 + chunk_size.x - 1;
    let wz1 = wz0 + chunk_size.y - 1;

    // Smooth curvature and local widen
    let mut warp = FastNoiseLite::with_seed(params.seed.wrapping_add(0x5A5A));
    warp.set_noise_type(Some(NoiseType::OpenSimplex2));
    warp.set_frequency(Some(0.015));

    let mut widen = FastNoiseLite::with_seed(params.seed.wrapping_add(0xBEEF));
    widen.set_noise_type(Some(NoiseType::OpenSimplex2));
    widen.set_frequency(Some(0.035));

    let mut edits: Vec<(u16, u16, u16)> = Vec::new();

    /* --------- helpers --------- */

    // Keep pitch between gentle down and gentle up
    let clamp_pitch = |dir: Vec3| -> Vec3 {
        let mut v = dir; v.y = v.y.clamp(-0.18, 0.18); v.normalize()
    };

    // Entrance spur pitch: allow a stronger upward tendency
    let clamp_pitch_up = |dir: Vec3| -> Vec3 {
        let mut v = dir; v.y = v.y.clamp(-0.05, 0.45); v.normalize()
    };

    // Carve a short upward spur starting near the top of a cave window.
    // It gradually narrows while climbing a bit above params.y_top (up to entrance_extra_top).
    let carve_entrance_spur = |out: &mut Vec<(u16,u16,u16)>, start: Vec3, base_dir: Vec3, base_r: f32| {
        // Early out if already above allowed entrance ceiling
        let _y_ceiling = (params.y_top + params.entrance_extra_top) as f32;

        let mut p_prev = start;
        let mut d = clamp_pitch_up(Vec3::new(base_dir.x, (base_dir.y + 0.35).clamp(0.1, 0.45), base_dir.z));

        // Shorter steps for more control when going up
        let step_len = (base_r * 0.55).max(0.5);

        for s in 0..params.entrance_len_steps.max(0) {
            // Small curvature with upward bias
            let n1 = warp.get_noise_3d(p_prev.x, p_prev.y, p_prev.z);
            let n2 = warp.get_noise_3d(p_prev.z * 0.7, p_prev.x * 0.7, p_prev.y * 0.7);
            let yaw_delta   = n1 * 0.06;
            let pitch_delta = 0.06 + n2.max(0.0) * 0.10; // enforce upward component

            let cy = yaw_delta.cos(); let sy = yaw_delta.sin();
            let cp = pitch_delta.cos(); let sp = pitch_delta.sin();

            let dx =  cy * d.x + sy * d.z;
            let dz = -sy * d.x + cy * d.z;
            let dy = d.y * cp + sp * dx.hypot(dz).min(1.0);

            d = clamp_pitch_up(Vec3::new(dx, dy, dz).normalize());

            let p_next = p_prev + d * step_len;

            // Abort if we went too high or out of global Y bounds
            if p_next.y as i32 >= params.y_top + params.entrance_extra_top { break; }
            if (p_next.y as i32) < y_min || (p_next.y as i32) > y_max { break; }

            // Culling by XY to avoid doing work for far-away chunks
            let r_max = base_r * params.entrance_radius_scale;
            let max_r_i = r_max.ceil() as i32 + 1;
            let min_x = (p_prev.x.min(p_next.x) as i32) - max_r_i;
            let max_x = (p_prev.x.max(p_next.x) as i32) + max_r_i;
            let min_z = (p_prev.z.min(p_next.z) as i32) - max_r_i;
            let max_z = (p_prev.z.max(p_next.z) as i32) + max_r_i;
            if max_x < wx0 || min_x > wx1 || max_z < wz0 || min_z > wz1 {
                p_prev = p_next; continue;
            }

            // Taper radius: start from a scaled base down to a minimum
            let t = (s as f32 / params.entrance_len_steps.max(1) as f32).clamp(0.0, 1.0);
            let rh0 = (base_r * params.entrance_radius_scale).max(params.entrance_min_radius);
            let rh  = lerp(rh0, params.entrance_min_radius, t);
            let rv  = (rh * 0.85).max(1.6);

            // Sample step density similar to main tunnel
            let seg = p_next - p_prev;
            let seg_len = seg.length().max(1e-4);
            let samples = (seg_len / (rh * 0.6).max(0.5)).ceil() as i32;
            for k in 0..=samples {
                let tk = k as f32 / samples.max(1) as f32;
                let q = p_prev.lerp(p_next, tk);
                append_ellipsoid_into_chunk(out, q, rh, rv, rh, wx0, wz0, chunk_size, y_min, y_max);
            }

            p_prev = p_next;
        }
    };

    /* --------- Tunnels --------- */

    for w in worms {
        let mut p_prev = w.start;
        let mut d = clamp_pitch(w.dir);

        for step in 0..w.steps {
            // Curvature
            let n1 = warp.get_noise_3d(p_prev.x, p_prev.y, p_prev.z);
            let n2 = warp.get_noise_3d(p_prev.z * 0.7, p_prev.x * 0.7, p_prev.y * 0.7);
            let yaw_delta   = n1 * 0.09;
            let pitch_delta = n2 * 0.06;

            let cy = yaw_delta.cos(); let sy = yaw_delta.sin();
            let cp = pitch_delta.cos(); let sp = pitch_delta.sin();

            let dx =  cy * d.x + sy * d.z;
            let dz = -sy * d.x + cy * d.z;
            let dy = d.y * cp + sp * dx.hypot(dz).min(1.0);
            d = clamp_pitch(Vec3::new(dx, dy, dz).normalize());

            let p_next = p_prev + d * w.step_len;

            // Keep the main tunnel within the base window; still advance simulation
            if (p_next.y as i32) < params.y_bottom || (p_next.y as i32) > params.y_top {
                p_prev = p_next;
                continue;
            }

            // Segment XY culling to the current chunk AABB (with a small radius margin)
            let max_r = (w.base_r + w.var_r).ceil() as i32 + 1;
            let min_x = (p_prev.x.min(p_next.x) as i32) - max_r;
            let max_x = (p_prev.x.max(p_next.x) as i32) + max_r;
            let min_z = (p_prev.z.min(p_next.z) as i32) - max_r;
            let max_z = (p_prev.z.max(p_next.z) as i32) + max_r;
            if max_x < wx0 || min_x > wx1 || max_z < wz0 || min_z > wz1 {
                p_prev = p_next;
                continue;
            }

            // Local widening (radius noise)
            let widen_f = 0.5 * (widen.get_noise_3d(p_next.x, p_next.y, p_next.z) + 1.0);
            let r_h = w.base_r + widen_f * w.var_r;
            let r_v = (r_h * 0.85).max(1.8);

            // Carve the main segment as a string of ellipsoids
            let seg = p_next - p_prev;
            let seg_len = seg.length().max(1e-4);
            let samples = (seg_len / (r_h * 0.6).max(0.6)).ceil() as i32;
            for s in 0..=samples {
                let t = s as f32 / samples.max(1) as f32;
                let q = p_prev.lerp(p_next, t);
                append_ellipsoid_into_chunk(&mut edits, q, r_h, r_v, r_h, wx0, wz0, chunk_size, y_min, y_max);
            }

            // Occasionally place a small room along tunnels
            if params.room_event_chance > 0.0 && (step % 28 == 0) {
                let trigger = 0.5 * (warp.get_noise_3d(p_next.x * 0.2, p_next.y * 0.2, p_next.z * 0.2) + 1.0);
                if trigger > (1.0 - params.room_event_chance) {
                    let t = 0.5 * (widen.get_noise_3d(p_next.y, p_next.z, p_next.x) + 1.0);
                    let rr = lerp(params.room_radius_min, params.room_radius_max, t);
                    append_ellipsoid_into_chunk(&mut edits, p_next, rr, rr * 0.85, rr, wx0, wz0, chunk_size, y_min, y_max);
                }
            }

            /* --------- NEW: entrance spur trigger near top window ---------
             * When the next point is inside a small vertical band below y_top,
             * and chance hits, carve an upward spur that may climb slightly
             * above y_top (up to entrance_extra_top).
             */
            if (p_next.y as i32) >= params.y_top - params.entrance_trigger_band as i32
                && (p_next.y as i32) <= params.y_top
            {
                // Deterministic-ish chance via noise at this location
                let chance_v = 0.5 * (widen.get_noise_3d(p_next.x * 0.19, p_next.y * 0.19, p_next.z * 0.19) + 1.0);
                if chance_v > (1.0 - params.entrance_chance).clamp(0.0, 1.0) && d.y >= -0.02 {
                    carve_entrance_spur(&mut edits, p_next, d, r_h);
                }
            }

            p_prev = p_next;
        }
    }

    /* --------- Caverns (rooms + corridors) --------- */

    let append_corridor = |ed: &mut Vec<(u16, u16, u16)>, a: &CavernRoom, b: &CavernRoom, noisy: bool, seed: i32, r: f32| {
        let pa = a.center; let pb = b.center;
        let min_x = pa.x.min(pb.x) as i32 - (r as i32) - 1;
        let max_x = pa.x.max(pb.x) as i32 + (r as i32) + 1;
        let min_z = pa.z.min(pb.z) as i32 - (r as i32) - 1;
        let max_z = pa.z.max(pb.z) as i32 + (r as i32) + 1;
        if max_x < wx0 || min_x > wx1 || max_z < wz0 || min_z > wz1 { return; }

        let seg = pb - pa;
        let len = seg.length().max(1e-4);
        let samples = (len / (r * 0.7).max(0.8)).ceil() as i32;
        for s in 0..=samples {
            let t = s as f32 / samples.max(1) as f32;
            let q = pa.lerp(pb, t);
            if noisy {
                let edge_amp = 0.18;
                let edge_freq = 0.022;
                let warp_amp  = (r * 0.6).clamp(2.5, 9.0);
                let warp_freq = 0.008;
                append_noisy_ellipsoid_into_chunk(ed, q, r, r * 0.85, r, wx0, wz0, chunk_size, y_min, y_max,
                                                  seed, edge_amp, edge_freq, warp_amp, warp_freq);
            } else {
                append_ellipsoid_into_chunk(ed, q, r, r * 0.85, r, wx0, wz0, chunk_size, y_min, y_max);
            }
        }
    };

    for cluster in caverns {
        // Rooms
        for room in &cluster.rooms {
            let rx = room.rx; let ry = room.ry; let rz = room.rz;
            let min_x = (room.center.x - rx) as i32;
            let max_x = (room.center.x + rx) as i32;
            let min_z = (room.center.z - rz) as i32;
            let max_z = (room.center.z + rz) as i32;
            if max_x < wx0 || min_x > wx1 || max_z < wz0 || min_z > wz1 { continue; }

            if cluster.noisy {
                let r_max = rx.max(rz);
                let (edge_amp, edge_freq, warp_amp, warp_freq) = if r_max >= 40.0 {
                    ((0.15 + 0.12 * (r_max / 80.0)).clamp(0.15, 0.32), 0.018,
                     (r_max * 0.10).clamp(4.0, 14.0), 0.006)
                } else {
                    ((0.08 + 0.05 * (r_max / 30.0)).clamp(0.08, 0.18), 0.028,
                     (r_max * 0.06).clamp(2.0, 8.0), 0.010)
                };
                append_noisy_ellipsoid_into_chunk(&mut edits, room.center, rx, ry, rz, wx0, wz0, chunk_size, y_min, y_max,
                                                  params.seed, edge_amp, edge_freq, warp_amp, warp_freq);
            } else {
                append_ellipsoid_into_chunk(&mut edits, room.center, rx, ry, rz, wx0, wz0, chunk_size, y_min, y_max);
            }
        }

        // Connect rooms by shortest links
        if cluster.rooms.len() >= 2 {
            let mut used = vec![false; cluster.rooms.len()];
            let mut idx = 0usize;
            used[idx] = true;
            for _ in 1..cluster.rooms.len() {
                let p = cluster.rooms[idx].center;
                let mut best: Option<(usize, f32)> = None;
                for (j, r) in cluster.rooms.iter().enumerate() {
                    if used[j] { continue; }
                    let d2 = (r.center - p).length_squared();
                    if best.map_or(true, |(_, bd)| d2 < bd) { best = Some((j, d2)); }
                }
                if let Some((j, _)) = best {
                    append_corridor(&mut edits, &cluster.rooms[idx], &cluster.rooms[j], cluster.noisy, params.seed, cluster.connector_r);
                    used[j] = true;
                    idx = j;
                }
            }
        }
    }

    edits
}
