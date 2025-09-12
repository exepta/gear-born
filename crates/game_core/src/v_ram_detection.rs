//! Cross-platform V-RAM detection helpers.
//!
//! This module exposes vendor/OS specific ways to query *actual* V-RAM usage,
//! with graceful fallbacks when a backend is not available.
//!
//! - NVIDIA (Win/Linux): NVML → **per-process** bytes (preferred when available).
//! - Windows (AMD & NVIDIA): DXGI → **adapter-wide** "CurrentUsage" bytes.
//! - macOS: Metal → **device-wide** current allocated size.
//!
//! Use `detect_vram_best_effort()` to try the backends in a sensible order
//! and get a `VideoRamInfo { byte, source, scope }` back.
//!
//! ### Example
//! ```ignore
//! if let Some(info) = v_ram_detector::detect_vram_best_effort() {
//!     println!("V-RAM: {} bytes ({} / {})", info.bytes, info.source, info.scope);
//! } else {
//!     println!("No V-RAM backend available – consider using an estimate.");
//! }
//! ```

/// Information about a V-RAM reading.
#[derive(Debug, Clone, Copy)]
pub struct VideoRamInfo {
    /// Bytes reported by the backend.
    pub bytes: u64,
    /// Backend name, e.g. "NVML", "DXGI", "Metal".
    pub source: &'static str,
    /// Scope of the reading, e.g. "per-process", "adapter-wide", "device-wide".
    pub scope: &'static str,
}

/// Try platform/vendor-specific backends in a sensible order and return the first hit.
///
/// Order of preference:
/// 1. NVML (NVIDIA, per-process)
/// 2. DXGI (Windows adapter-wide; works for AMD & NVIDIA)
/// 3. Metal (macOS device-wide)
pub fn detect_v_ram_best_effort() -> Option<VideoRamInfo> {
    // 1) NVIDIA per-process via NVML
    if let Some(bytes) = query_vram_bytes_nvml_this_process() {
        return Some(VideoRamInfo { bytes, source: "NVML", scope: "per-process" });
    }

    #[cfg(target_os = "linux")]
    if let Some(bytes) = query_vram_bytes_linux_amdgpu_per_process(std::process::id()) {
        return Some(VideoRamInfo { bytes, source: "amdgpu-debugfs", scope: "per-process" });
    }

    // 2) Linux AMD
    #[cfg(target_os = "linux")]
    if let Some(bytes) = query_vram_bytes_linux_drm_amdgpu() {
        return Some(VideoRamInfo { bytes, source: "Linux DRM", scope: "device-wide" });
    }

    // 2) Windows adapter-wide via DXGI (covers AMD & NVIDIA)
    if let Some(bytes) = query_vram_bytes_dxgi_adapter_current_usage() {
        return Some(VideoRamInfo { bytes, source: "DXGI", scope: "adapter-wide" });
    }

    // 3) macOS device-wide via Metal
    if let Some(bytes) = query_vram_bytes_metal_device_allocated() {
        return Some(VideoRamInfo { bytes, source: "Metal", scope: "device-wide" });
    }

    None
}

/* ======================== NVIDIA: NVML (per-process) ======================== */

/// Query V-RAM bytes for the current process via NVIDIA NVML (if available).
///
/// Requires feature `vram_nvml`. Returns `Some(bytes)` on success.
#[cfg(feature = "vram_nvml")]
pub fn query_vram_bytes_nvml_this_process() -> Option<u64> {
    query_vram_bytes_nvml_for_pid(std::process::id())
}

/// Stub when `vram_nvml` feature is disabled.
#[cfg(not(feature = "vram_nvml"))]
pub fn query_vram_bytes_nvml_this_process() -> Option<u64> { None }

/// Query VRAM bytes for a given PID via NVML (NVIDIA).
///
/// Requires feature `vram_nvml`. Returns `Some(bytes)` on success.
/// Falls back to scanning all devices and returning the **max** per-device value
/// for that PID (typical single-GPU setups).
#[cfg(feature = "vram_nvml")]
pub fn query_vram_bytes_nvml_for_pid(pid: u32) -> Option<u64> {
    use nvml_wrapper::Nvml;

    let nvml = Nvml::init().ok()?;
    let count = nvml.device_count().ok()?;
    let mut best: Option<u64> = None;

    for i in 0..count {
        let device = nvml.device_by_index(i).ok()?;

        // Prefer graphics; fall back to compute.
        let found = device
            .running_graphics_processes()
            .ok()
            .and_then(|list| find_bytes_for_pid(list, pid))
            .or_else(|| {
                device
                    .running_compute_processes()
                    .ok()
                    .and_then(|list| find_bytes_for_pid(list, pid))
            });

        if let Some(bytes) = found {
            best = Some(best.map_or(bytes, |b| b.max(bytes)));
        }
    }

    best
}

#[cfg(feature = "vram_nvml")]
fn find_bytes_for_pid(list: Vec<nvml_wrapper::struct_wrappers::device::ProcessInfo>, pid: u32) -> Option<u64> {
    use nvml_wrapper::enums::device::UsedGpuMemory;

    for p in list {
        if (p.pid) == pid {
            if let UsedGpuMemory::Used(bytes) = p.used_gpu_memory {
                return Some(bytes);
            }
        }
    }
    None
}

#[cfg(target_os = "linux")]
pub fn query_vram_bytes_linux_drm_amdgpu() -> Option<u64> {
    use std::fs;
    use std::path::{Path, PathBuf};

    fn read_u64_any(path: &Path) -> Option<u64> {
        let s = fs::read_to_string(path).ok()?;
        let t = s.trim();
        if let Some(hex) = t.strip_prefix("0x").or_else(|| t.strip_prefix("0X")) {
            u64::from_str_radix(hex, 16).ok()
        } else {
            t.parse::<u64>().ok()
        }
    }

    fn is_amdgpu(dev_dir: &Path) -> bool {
        if let Ok(link) = fs::read_link(dev_dir.join("driver")) {
            if link.file_name().map(|n| n == "amdgpu").unwrap_or(false) {
                return true;
            }
        }
        // Fallback über Vendor-ID (0x1002)
        read_u64_any(&dev_dir.join("vendor")).map_or(false, |v| v == 0x1002)
    }

    let mut best: Option<u64> = None;
    let drm_path = Path::new("/sys/class/drm");
    let entries = fs::read_dir(drm_path).ok()?;

    for e in entries.flatten() {
        let name = e.file_name();
        let name = name.to_string_lossy();
        if !name.starts_with("card") || !name[4..].chars().all(|c| c.is_ascii_digit()) {
            continue;
        }
        let dev_dir: PathBuf = e.path().join("device");
        if !is_amdgpu(&dev_dir) {
            continue;
        }

        let used = read_u64_any(&dev_dir.join("mem_info_vram_used"))
            .or_else(|| read_u64_any(&dev_dir.join("mem_info_vis_vram_used")));

        if let Some(bytes) = used {
            best = Some(best.map_or(bytes, |b| b.max(bytes)));
        }
    }

    best
}

#[cfg(target_os = "linux")]
pub fn query_vram_bytes_linux_amdgpu_per_process(pid: u32) -> Option<u64> {
    use std::fs;
    use std::path::Path;

    let root = Path::new("/sys/kernel/debug/dri");
    let entries = fs::read_dir(root).ok()?;

    for e in entries.flatten() {
        let p = e.path();
        if !p.is_dir() { continue; }

        let vm_info = p.join("amdgpu_vm_info");
        if vm_info.exists() {
            if let Some(b) = parse_amdgpu_vm_info_for_pid(&vm_info, pid) {
                return Some(b);
            }
        }

        let gem_info = p.join("amdgpu_gem_info");
        if gem_info.exists() {
            if let Some(b) = parse_amdgpu_gem_info_for_pid(&gem_info, pid) {
                return Some(b);
            }
        }
    }

    None
}

#[cfg(target_os = "linux")]
fn parse_amdgpu_vm_info_for_pid(path: &std::path::Path, pid: u32) -> Option<u64> {
    use std::fs;
    let s = fs::read_to_string(path).ok()?;
    for block in s.split("\n\n") {
        if !block.to_ascii_lowercase().contains(&format!("pid {}", pid)) {
            continue;
        }
        if let Some(bytes) = extract_vram_bytes_from_text(block) {
            return Some(bytes);
        }
    }
    None
}

#[cfg(target_os = "linux")]
fn parse_amdgpu_gem_info_for_pid(path: &std::path::Path, pid: u32) -> Option<u64> {
    use std::fs;
    let s = fs::read_to_string(path).ok()?;
    for line in s.lines() {
        if !line.to_ascii_lowercase().contains(&format!("pid {}", pid)) {
            continue;
        }
        if let Some(bytes) = extract_vram_bytes_from_text(line) {
            return Some(bytes);
        }
    }
    None
}

#[cfg(target_os = "linux")]
fn extract_vram_bytes_from_text(txt: &str) -> Option<u64> {
    let lower = txt.to_ascii_lowercase();
    let mut it = lower.split_whitespace().peekable();

    while let Some(tok) = it.next() {
        if tok.contains("vram") {
            for _ in 0..3 {
                if let Some(next) = it.peek().cloned() {
                    if let Some(bytes) = parse_number_with_unit(next) {
                        return Some(bytes);
                    }
                    if let Some(bytes) = parse_embedded_number_with_unit(next) {
                        return Some(bytes);
                    }
                    it.next();
                }
            }
        }
    }
    None
}

#[cfg(target_os = "linux")]
fn parse_number_with_unit(token: &str) -> Option<u64> {
    // "1234", "1234kb", "1234kib", "1234MB", "1234MiB"
    let t = token.trim_matches(|c: char| c == ':' || c == '=');
    parse_embedded_number_with_unit(t)
}

#[cfg(target_os = "linux")]
fn parse_embedded_number_with_unit(t: &str) -> Option<u64> {
    let mut digits = String::new();
    let mut unit = String::new();
    for ch in t.chars() {
        if ch.is_ascii_digit() { digits.push(ch); }
        else { unit.push(ch); }
    }
    if digits.is_empty() { return None; }
    let val: u64 = digits.parse().ok()?;
    let u = unit.to_ascii_lowercase();

    let mul: u64 = if u.is_empty() || u == "b" { 1 }
    else if u == "k" || u == "kb" || u == "kib" { 1024 }
    else if u == "m" || u == "mb" || u == "mib" { 1024*1024 }
    else if u == "g" || u == "gb" || u == "gib" { 1024*1024*1024 }
    else { 1 };
    Some(val.saturating_mul(mul))
}


#[cfg(not(target_os = "linux"))]
pub fn query_vram_bytes_linux_drm_amdgpu() -> Option<u64> { None }

/// Stub when `vram_nvml` feature is disabled.
#[cfg(not(feature = "vram_nvml"))]
pub fn query_vram_bytes_nvml_for_pid(_pid: u32) -> Option<u64> { None }

/* ========================= Windows: DXGI (adapter) ========================= */

/// Query adapter-local VRAM "CurrentUsage" via DXGI (Windows).
///
/// - Works for **AMD & NVIDIA** (and others) on Windows.
/// - Scope is **adapter-wide** (not per-process).
/// - Requires feature `vram_dxgi` and the `windows` crate with DXGI features.
///
/// Returns `Some(bytes)` on success. Chooses the first enumerated adapter.
#[cfg(all(windows, feature = "vram_dxgi"))]
pub fn query_vram_bytes_dxgi_adapter_current_usage() -> Option<u64> {
    use windows::core::Interface;
    use windows::Win32::Graphics::Dxgi::{
        CreateDXGIFactory2, IDXGIAdapter3, IDXGIFactory4,
        DXGI_CREATE_FACTORY_FLAGS, DXGI_MEMORY_SEGMENT_GROUP_LOCAL, DXGI_QUERY_VIDEO_MEMORY_INFO,
    };

    unsafe {
        let factory: IDXGIFactory4 =
            CreateDXGIFactory2::<IDXGIFactory4>(DXGI_CREATE_FACTORY_FLAGS(0)).ok()?;

        let mut index: u32 = 0;
        loop {
            let adapter = match factory.EnumAdapters1(index) {
                Ok(a) => a,
                Err(_) => break,
            };
            index += 1;

            if let Ok(adapter3) = adapter.cast::<IDXGIAdapter3>() {
                let mut info = DXGI_QUERY_VIDEO_MEMORY_INFO::default();
                if adapter3
                    .QueryVideoMemoryInfo(0, DXGI_MEMORY_SEGMENT_GROUP_LOCAL, &mut info)
                    .is_ok()
                {
                    return Some(info.CurrentUsage as u64);
                }
            }
        }
    }

    None
}

/// Stub when not on Windows or feature disabled.
#[cfg(not(all(windows, feature = "vram_dxgi")))]
pub fn query_vram_bytes_dxgi_adapter_current_usage() -> Option<u64> { None }

/* ============================ macOS: Metal (GPU) =========================== */

/// Query device-wide allocated bytes from Metal (macOS).
///
/// - Scope is **device-wide** (not per-process).
/// - Requires feature `vram_metal`.
#[cfg(all(target_os = "macos", feature = "vram_metal"))]
pub fn query_vram_bytes_metal_device_allocated() -> Option<u64> {
    let device = metal::Device::system_default()?;
    Some(device.current_allocated_size())
}

/// Stub when not on macOS or feature disabled.
#[cfg(not(all(target_os = "macos", feature = "vram_metal")))]
pub fn query_vram_bytes_metal_device_allocated() -> Option<u64> { None }

/* ========================== Utility / Presentation ========================= */

/// Human-readable formatter for bytes (MiB/GiB).
pub fn fmt_bytes(bytes: u64) -> String {
    const MIB: f64 = 1024.0 * 1024.0;
    const GIB: f64 = 1024.0 * 1024.0 * 1024.0;
    let b = bytes as f64;
    if b >= GIB {
        format!("{:.1} GB", b / GIB)
    } else {
        format!("{:.0} MB", b / MIB)
    }
}
