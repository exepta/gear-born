use crate::world::biome::{Biome, BiomeRegistry, BiomeSize};
use bevy::prelude::*;
use std::collections::{HashMap, VecDeque};

pub const DIR4: [IVec2; 4] = [
    IVec2::new( 1,  0),
    IVec2::new(-1,  0),
    IVec2::new( 0,  1),
    IVec2::new( 0, -1),
];

/// Compact region identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegionId(pub u64);

#[derive(Debug, Clone)]
pub struct RegionInfo {
    pub id: RegionId,
    pub biome_name: String,
    pub size_current: u32,
    pub size_target: u32,
    pub size_tag: BiomeSize,
}

#[derive(Debug, Clone)]
pub struct BiomeRegion {
    pub id: RegionId,
    pub origin: IVec2,       // seed/origin chunk
    pub biome_name: String,  // registry key
    pub size_target: u32,    // desired area in chunks
    pub size_current: u32,   // assigned chunks count
    frontier: VecDeque<IVec2>,
}

impl BiomeRegion {
    fn new(origin: IVec2, biome_name: String, size_target: u32, seed: i32) -> Self {
        let mut frontier = VecDeque::new();
        frontier.push_back(origin);
        BiomeRegion {
            id: RegionId(hash64(origin.x, origin.y, seed)),
            origin,
            biome_name,
            size_target,
            size_current: 0,
            frontier,
        }
    }

    /// Attempt to grow this region by assigning up to `budget` new chunks,
    /// expanding in 4-neighborhood, skipping already-assigned chunks.
    pub fn grow(&mut self, mut budget: u32, chunk_to_region: &mut HashMap<IVec2, RegionId>) {
        while budget > 0 && self.size_current < self.size_target {
            let Some(at) = self.frontier.pop_front() else { break; };

            // Skip if already assigned to any region.
            if chunk_to_region.contains_key(&at) { continue; }

            // Assign this chunk to the region.
            chunk_to_region.insert(at, self.id);
            self.size_current += 1;
            budget -= 1;

            // Push neighbors for BFS-like expansion.
            for d in DIR4 {
                let nei = IVec2::new(at.x + d.x, at.y + d.y);
                self.frontier.push_back(nei);
            }
        }
    }

    #[inline]
    pub fn is_complete(&self) -> bool { self.size_current >= self.size_target }

    #[inline]
    pub fn biome(&self) -> &str { &self.biome_name }
}

/// Runtime allocator storing chunk→region mapping and region states.
/// This grows regions lazily as new chunks are requested.
#[derive(Resource, Default)]
pub struct BiomeRegionAllocator {
    seed: i32,
    chunk_to_region: HashMap<IVec2, RegionId>,
    regions: HashMap<RegionId, BiomeRegion>,
}

impl BiomeRegionAllocator {
    /// Construct with a fixed seed to keep world layout deterministic.
    pub fn with_seed(seed: i32) -> Self {
        Self {
            seed,
            ..Default::default()
        }
    }

    /// If `chunk` is already assigned, returns its biome name.
    /// Otherwise, it will try to attach `chunk` to a nearby growing region;
    /// if none fits (or all are full), it starts a new region at `chunk`.
    ///
    /// Returns the biome name from the registry if available.
    pub fn biome_for_chunk(&mut self, chunk: IVec2, reg: &BiomeRegistry) -> Option<String> {
        // Fast path: already assigned
        if let Some(rid) = self.chunk_to_region.get(&chunk).copied() {
            return self.regions.get(&rid).map(|r| r.biome_name.clone());
        }

        // Try to join a neighbor region if any not complete
        if let Some((rid, _joinable)) = self
            .neighbor_regions(chunk)
            .into_iter()
            .find_map(|rid| {
                let joinable = self.regions.get(&rid).map(|r| !r.is_complete()).unwrap_or(false);
                if joinable { Some((rid, true)) } else { None }
            })
        {
            // Move region out to avoid aliasing borrows on `self`.
            if let Some(mut region) = self.regions.remove(&rid) {
                region.frontier.push_front(chunk); // prioritize the current request
                let biome_name = region.biome_name.clone();
                region.grow(1, &mut self.chunk_to_region); // include `chunk`
                self.regions.insert(rid, region);
                return Some(biome_name);
            }
        }

        // No neighbor to join → starts a new region at `chunk`
        let size_tag = choose_size_from01(f_rand01(chunk.x, chunk.y, self.seed, 0x5123_512E));
        let (min_c, max_c) = size_chunk_limits(size_tag);

        // Deterministic target size within [min,max]
        let t = f_rand01(chunk.x, chunk.y, self.seed, 0xA11C_4EEA);
        let size_target = min_c + ((t * ((max_c - min_c + 1) as f32)) as u32).min(max_c - min_c);

        // Pick a biome supporting this size (weighted by rarity)
        let biome_name = pick_biome_for_size(chunk.x, chunk.y, self.seed, reg, size_tag)?;

        let mut region = BiomeRegion::new(chunk, biome_name.clone(), size_target, self.seed);

        // Grow with a modest budget now; a region can continue growing when neighbors are requested.
        region.grow(8, &mut self.chunk_to_region);

        let rid = region.id;
        self.regions.insert(rid, region);
        Some(biome_name)
    }

    /// Try to expand the region of `chunk` (if any) a bit more; useful as a maintenance step.
    pub fn tick_growth_around(&mut self, chunk: IVec2) {
        if let Some(rid) = self.chunk_to_region.get(&chunk).copied() {
            if let Some(mut region) = self.regions.remove(&rid) {
                region.grow(16, &mut self.chunk_to_region);
                self.regions.insert(rid, region);
            }
        }
    }

    /// Returns the region id (if any) for a chunk.
    pub fn region_id_of(&self, chunk: IVec2) -> Option<RegionId> {
        self.chunk_to_region.get(&chunk).copied()
    }

    /// Returns the current size (assigned chunks) of a region, if present.
    pub fn region_size(&self, rid: RegionId) -> Option<u32> {
        self.regions.get(&rid).map(|r| r.size_current)
    }

    /// Convenience: fetch the biome name for a chunk without creating new regions.
    pub fn biome_of_assigned(&self, chunk: IVec2) -> Option<&str> {
        self.chunk_to_region
            .get(&chunk)
            .and_then(|rid| self.regions.get(rid).map(|r| r.biome_name.as_str()))
    }

    /// Neighbor region ids around a chunk (deduplicated).
    fn neighbor_regions(&self, chunk: IVec2) -> Vec<RegionId> {
        let mut out = Vec::new();
        for d in DIR4 {
            if let Some(rid) = self.chunk_to_region.get(&IVec2::new(chunk.x + d.x, chunk.y + d.y)) {
                if !out.contains(rid) { out.push(*rid); }
            }
        }
        out
    }

    pub fn region_info_for_chunk(&self, chunk: IVec2) -> Option<RegionInfo> {
        let rid = *self.chunk_to_region.get(&chunk)?;
        let r   = self.regions.get(&rid)?;
        Some(RegionInfo {
            id: r.id,
            biome_name: r.biome_name.clone(),
            size_current: r.size_current,
            size_target: r.size_target,
            size_tag: infer_size_tag_from_target(r.size_target),
        })
    }
}

pub fn pick_biome_for_size(
    x: i32,
    z: i32,
    seed: i32,
    reg: &BiomeRegistry,
    size: BiomeSize,
) -> Option<String> {
    // Collect candidates
    let mut candidates: Vec<(&Biome, f32)> = reg
        .iter()
        .filter_map(|(_name, b)| {
            if b.sizes.iter().any(|s| *s == size) {
                let w = if b.rarity > 0.0 { b.rarity } else { 0.0001 };
                Some((b, w))
            } else {
                None
            }
        })
        .collect();

    if candidates.is_empty() {
        candidates = reg.iter().map(|(_n, b)| (b, b.rarity.max(0.0001))).collect();
        if candidates.is_empty() {
            return None;
        }
    }

    let sum_w: f32 = candidates.iter().map(|(_, w)| *w).sum();
    let mut t = f_rand01(x, z, seed, 0xB10B_E77A) * sum_w;

    // Fallback in case of rare floating-point rounding
    let fallback = candidates.last().unwrap().0.name.clone();

    for (b, w) in candidates {
        if t <= w {
            return Some(b.name.clone());
        }
        t -= w;
    }
    Some(fallback)
}
#[inline]
pub fn size_chunk_limits(size: BiomeSize) -> (u32, u32) {
    match size {
        BiomeSize::Small     => (20, 26),
        BiomeSize::Medium    => (48, 56),
        BiomeSize::Large     => (86, 120),
        BiomeSize::VeryLarge => (144, 200),
        BiomeSize::Gigantic  => (240, 400),
        BiomeSize::Unknown   => (32, 32),
    }
}

#[inline]
fn hash64(x: i32, z: i32, seed: i32) -> u64 {
    // Keep bit patterns stable for negative numbers by casting via u32.
    let xu = x as u32 as u64;
    let zu = z as u32 as u64;
    let su = seed as u32 as u64;

    let mut v = xu.wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ zu.wrapping_mul(0xC2B2_AE3D_27D4_EB4F)
        ^ su.rotate_left(13);
    v ^= v >> 30;
    v = v.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    v ^= v >> 27;
    v = v.wrapping_mul(0x94D0_49BB_1331_11EB);
    v ^= v >> 31;
    v
}

#[inline]
pub fn choose_size_from01(v: f32) -> BiomeSize {
    if v < 0.40 { BiomeSize::Small }
    else if v < 0.70 { BiomeSize::Medium }
    else if v < 0.90 { BiomeSize::Large }
    else if v < 0.98 { BiomeSize::VeryLarge }
    else { BiomeSize::Gigantic }
}

#[inline]
pub fn f_rand01(x: i32, z: i32, seed: i32, salt: u64) -> f32 {
    let h = hash64(x, z, seed) ^ salt;
    // Map high 24 bits to [0,1)
    let m = ((h >> 40) & 0xFF_FFFF) as u32;
    (m as f32) / 16_777_216.0
}

#[inline]
fn infer_size_tag_from_target(size_target: u32) -> BiomeSize {
    // We infer the original tag by matching inclusive ranges.
    for &tag in &[BiomeSize::Small, BiomeSize::Medium, BiomeSize::Large, BiomeSize::VeryLarge, BiomeSize::Gigantic] {
        let (min_c, max_c) = size_chunk_limits(tag);
        if size_target >= min_c && size_target <= max_c {
            return tag;
        }
    }
    BiomeSize::Unknown
}