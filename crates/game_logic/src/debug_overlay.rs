use bevy::diagnostic::{DiagnosticsStore, EntityCountDiagnosticsPlugin, FrameTimeDiagnosticsPlugin};
use bevy::prelude::*;
use bevy::render::renderer::RenderAdapterInfo;
use bevy::render::view::RenderLayers;
use fastnoise_lite::{FastNoiseLite, NoiseType};
use game_core::configuration::{GameConfig, WorldGenConfig};
use game_core::debug::*;
use game_core::key_converter::convert;
use game_core::player::selection::SelectionState;
use game_core::player::{GameMode, GameModeState};
use game_core::states::{AppState, InGameStates};
use game_core::world::biome::{BiomeAsset, BiomeRegistry, BiomeSize, BIOME_MOIST_FREQ, BIOME_TEMP_FREQ};
use game_core::world::block::{block_name_from_registry, get_block_world, BlockRegistry, MiningState, VOXEL_SIZE};
use game_core::world::chunk::ChunkMap;
use game_core::world::chunk_dim::*;
use game_core::{BlockCatalogPreviewCam, BuildInfo};
use std::ops::Neg;
use sysinfo::{CpuRefreshKind, MemoryRefreshKind, Pid, ProcessesToUpdate, RefreshKind, System};

#[derive(Resource, Default)]
struct DebugSnapshot {
    // Numbers
    fps: f32,
    cpu_percent: f32,
    app_mem_bytes: u64,

    // Cam / World
    pos_bs: Vec3,
    chunk_cc: IVec2,
    facing_text: &'static str,
    yaw_deg: f32,
    biome_name: String,

    // Selection / Mining
    block_line: String,
    hit_str: String,

    // Build / Config
    app_name: &'static str,
    app_ver: &'static str,
    bevy_ver: &'static str,
    backend_name: String,
    backend_str: &'static str,
    chunk_range: i32,
    mode_text: &'static str,

    // Hotkeys (for Ui)
    key_debug: String,
    key_grid: String,
}

pub struct DebugOverlayPlugin;

impl Plugin for DebugOverlayPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_plugins((FrameTimeDiagnosticsPlugin::default(), EntityCountDiagnosticsPlugin))
            .init_resource::<DebugOverlayState>()
            .init_resource::<DebugGridState>()
            .init_resource::<SysStats>()
            .init_resource::<DebugSnapshot>()
            .init_gizmo_group::<ChunkGridGizmos>()
            .add_systems(
                OnEnter(AppState::InGame(InGameStates::Game)),
                (setup_sys_info, setup_chunk_grid_gizmos),
            )

            .add_systems(
                Update,
                (
                    toggle_overlay,
                    toggle_grid,
                    poll_sys_info,
                    ensure_overlay_exists,
                    draw_chunk_grid,
                )
                    .run_if(in_state(AppState::InGame(InGameStates::Game))),
            )

            .add_systems(
                Update,
                (
                    (snap_perf, snap_camera_and_world, snap_biome, snap_selection, snap_build_and_mode).chain(),
                    render_debug_text,
                )
                    .run_if(in_state(AppState::InGame(InGameStates::Game)))
                    .run_if(overlay_visible),
            );
    }
}

fn snap_perf(diag: Res<DiagnosticsStore>, stats: Res<SysStats>, mut snap: ResMut<DebugSnapshot>) {
    snap.fps = diag.get(&FrameTimeDiagnosticsPlugin::FPS)
        .and_then(|d| d.smoothed()).unwrap_or(0.0) as f32;
    snap.cpu_percent = stats.cpu_percent;
    snap.app_mem_bytes = stats.app_mem_bytes;
}

fn snap_camera_and_world(
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    game_cfg: Res<GameConfig>,
    mut snap: ResMut<DebugSnapshot>,
) {
    let pos = q_cam.single().map(|t| t.translation()).unwrap_or(Vec3::ZERO) / VOXEL_SIZE;
    let (cc, _) = world_to_chunk_xz(pos.x.floor() as i32, pos.z.floor() as i32);

    let (facing_text, yaw_deg) = q_cam.single()
        .ok()
        .map(|gt| facing_dir_from_cam(gt))
        .unwrap_or(("—", 0.0));

    snap.pos_bs = pos;
    snap.chunk_cc = IVec2::new(cc.x, cc.y);
    snap.facing_text = facing_text;
    snap.yaw_deg = yaw_deg;
    snap.chunk_range = game_cfg.graphics.chunk_range;
    snap.key_debug = game_cfg.input.debug_overlay.clone();
    snap.key_grid  = game_cfg.input.chunk_grid.clone();
}

fn snap_biome(
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    gen_cfg: Res<WorldGenConfig>,
    assets: Res<Assets<BiomeAsset>>,
    registry: Res<BiomeRegistry>,
    mut snap: ResMut<DebugSnapshot>,
) {
    let Ok(tf) = q_cam.single() else { return; };
    let pos_bs = tf.translation() / VOXEL_SIZE;
    let (x, z) = (pos_bs.x, pos_bs.z);

    let mut temp_n = FastNoiseLite::with_seed(gen_cfg.seed ^ 0x0B10_0001);
    temp_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    temp_n.set_frequency(Some(BIOME_TEMP_FREQ));

    let mut moist_n = FastNoiseLite::with_seed(gen_cfg.seed ^ 0x0B10_0002);
    moist_n.set_noise_type(Some(NoiseType::OpenSimplex2));
    moist_n.set_frequency(Some(BIOME_MOIST_FREQ));

    let t = map01(temp_n.get_noise_2d(x, z));
    let m = map01(moist_n.get_noise_2d(x, z));

    let sigma2 = 0.15f32 * 0.15f32;

    let mut best_name: Option<String> = None;
    let mut best_sizes: Option<Vec<BiomeSize>> = None;
    let mut best_w = -1.0f32;

    for (_, handle) in registry.iter() {
        if let Some(b) = assets.get(handle) {
            let dt = t - b.temperature;
            let dm = m - b.moist;
            let d2 = dt * dt + dm * dm;
            let w = (-d2 / (2.0 * sigma2)).exp() * (0.0001 + b.rarity.clamp(0.0, 1.0));
            if w > best_w {
                best_w = w;
                best_name = Some(b.name.clone());
                best_sizes = Some(b.sizes.clone()); // Vec<BiomeSize>
            }
        }
    }

    snap.biome_name = if let (Some(name), Some(sizes)) = (best_name, best_sizes) {
        let label = active_size_label(&name, &sizes, gen_cfg.seed, x, z);
        format!("{name} ({label})")
    } else {
        "—".to_string()
    };
}

fn snap_selection(
    sel: Option<Res<SelectionState>>,
    reg: Res<BlockRegistry>,
    chunk_map: Res<ChunkMap>,
    time: Res<Time>,
    mining: Option<Res<MiningState>>,
    mut snap: ResMut<DebugSnapshot>,
) {
    let (hit_str, block_line) = if let Some(sel) = sel {
        if let Some(h) = sel.hit {
            let id = get_block_world(&chunk_map, h.block_pos);
            let name = if id != 0 { block_name_from_registry(&reg, id) } else { "air".into() };

            let pct_opt = mining.as_ref()
                .and_then(|m| m.target.as_ref())
                .and_then(|t| {
                    if t.loc == h.block_pos && t.id == id && t.duration > 0.0 {
                        let p = ((time.elapsed_secs() - t.started_at) / t.duration)
                            .clamp(0.0, 1.0);
                        Some((p * 100.0).round() as i32)
                    } else { None }
                });

            let block_line = if id != 0 {
                if let Some(pct) = pct_opt {
                    format!("Block: {} progress ({}%)", name, pct)
                } else {
                    format!("Block: {}", name)
                }
            } else { "Block: —".into() };

            (
                format!("{:?} at ({},{},{})", h.face, h.block_pos.x, h.block_pos.y, h.block_pos.z),
                block_line
            )
        } else { ("—".into(), "Block: —".into()) }
    } else { ("—".into(), "Block: —".into()) };

    snap.hit_str = hit_str;
    snap.block_line = block_line;
}

// Build/Backend/Mode
fn snap_build_and_mode(
    build: Option<Res<BuildInfo>>,
    backend: Res<RenderAdapterInfo>,
    game_mode: Res<GameModeState>,
    mut snap: ResMut<DebugSnapshot>,
) {
    let (app_name, app_ver, bevy_ver) = if let Some(b) = build {
        (b.app_name, b.app_version, b.bevy_version)
    } else { ("<app>", "?", "0.16.1") };

    snap.app_name = app_name;
    snap.app_ver = app_ver;
    snap.bevy_ver = bevy_ver;
    snap.backend_name = backend.name.clone();
    snap.backend_str = backend_to_str(backend.backend.to_str());
    snap.mode_text = match game_mode.0 {
        GameMode::Creative => "Creative",
        GameMode::Survival => "Survival",
    };
}

fn setup_chunk_grid_gizmos(mut store: ResMut<GizmoConfigStore>) {
    let cfg = store.config_mut::<ChunkGridGizmos>();
    cfg.0.enabled = true;
    cfg.0.depth_bias = -0.02;
    cfg.0.render_layers = RenderLayers::layer(1);
}


fn setup_sys_info(mut stats: ResMut<SysStats>) {
    let mut s = System::new_with_specifics(
        RefreshKind::default()
            .with_memory(MemoryRefreshKind::default())
            .with_cpu(CpuRefreshKind::default()),
    );
    let pid = Pid::from_u32(std::process::id());
    s.refresh_cpu_all();
    s.refresh_processes(ProcessesToUpdate::Some(&[pid]), true);

    let (app_mem_bytes, app_cpu) = s.process(pid)
        .map(|p| (p.memory(), p.cpu_usage()))
        .unwrap_or((0, 0.0));

    stats.app_mem_bytes = app_mem_bytes;
    stats.app_cpu_percent = app_cpu;
    stats.sys = s;
}

fn render_debug_text(
    state: Res<DebugOverlayState>,
    mut q_text: Query<&mut Text>,
    snap: Res<DebugSnapshot>,
) {
    if !state.show { return; }
    let Some(text_e) = state.text else { return; };

    let mem_str = fmt_mem_from_bytes(snap.app_mem_bytes);
    let txt = format!(
        "{app} {app_ver}  (Bevy {bevy_ver})\n\
         FPS: {:>5.1}\n\
         Graphic: {}\n\
         CPU: {:>4.1}%  RAM(proc): {}  Backend: {}\n\
         Location: ({:.2}, {:.2}, {:.2})\n\
         Facing: {} ({:.1}°)\n\
         Chunk: ({}, {})  (size: {}x{}, range: {})\n\
         Biome: {}\n\
         {}\n\
         {}\n\
         Game Mode: {}\n\
         {}: Toggle Debug Overlay   {}: Toggle Chunk Grid",
        snap.fps,
        snap.backend_name,
        snap.cpu_percent,
        mem_str,
        snap.backend_str,
        snap.pos_bs.x, snap.pos_bs.y, snap.pos_bs.z,
        snap.facing_text, snap.yaw_deg,
        snap.chunk_cc.x, snap.chunk_cc.y, CX, CZ, snap.chunk_range,
        snap.biome_name,
        snap.block_line,
        snap.hit_str,
        snap.mode_text,
        snap.key_debug,
        snap.key_grid,
        app = snap.app_name,
        app_ver = snap.app_ver,
        bevy_ver = snap.bevy_ver,
    );

    if let Ok(mut t) = q_text.get_mut(text_e) {
        *t = Text::new(txt);
    }
}

/* ---------- Toggles ---------- */

fn toggle_overlay(
    keys: Res<ButtonInput<KeyCode>>,
    mut state: ResMut<DebugOverlayState>,
    game_config: Res<GameConfig>
) {
    let key = convert(game_config.input.debug_overlay.as_str())
        .expect("Invalid key for debugger overlay");

    if keys.just_pressed(key) {
        state.show = !state.show;
    }
}

fn toggle_grid(
    keys: Res<ButtonInput<KeyCode>>,
    mut state: ResMut<DebugGridState>,
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    game_config: Res<GameConfig>
) {
    let key = convert(game_config.input.chunk_grid.as_str())
        .expect("Invalid key for debugger overlay");

    if keys.just_pressed(key) {
        state.show = !state.show;
        if state.show {
            if let Ok(tf) = q_cam.single() {
                state.plane_y = tf.translation().y.floor() + 0.02;
            }
        }
    }
}

fn ensure_overlay_exists(
    mut state: ResMut<DebugOverlayState>,
    mut commands: Commands,
    mut q_node: Query<&mut Visibility>,
) {
    if let (Some(root), Some(_)) = (state.root, state.text) {
        if let Ok(mut vis) = q_node.get_mut(root) {
            *vis = if state.show { Visibility::Visible } else { Visibility::Hidden };
        }
        return;
    }

    let root = commands
        .spawn((
            Node {
                position_type: PositionType::Absolute,
                top: Val::Px(10.0),
                left: Val::Px(10.0),
                padding: UiRect::all(Val::Px(8.0)),
                ..default()
            },
            BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.28)),
            BorderRadius::all(Val::Px(6.0)),
            ZIndex(1000),
            Visibility::Hidden,
            Name::new("DebugOverlayRoot"),
        ))
        .id();

    let text = commands
        .spawn((
            Text::new(""),
            TextFont { font_size: 16.0, ..default() },
            TextColor(Color::WHITE),
            Name::new("DebugText"),
        ))
        .id();

    commands.entity(root).add_child(text);

    if state.show {
        commands.entity(root).insert(Visibility::Visible);
    }
    state.root = Some(root);
    state.text = Some(text);
}

fn poll_sys_info(time: Res<Time>, mut stats: ResMut<SysStats>) {
    if stats.timer.tick(time.delta()).just_finished() {
        let pid = Pid::from_u32(std::process::id());

        stats.sys.refresh_cpu_all();
        stats.sys.refresh_processes(ProcessesToUpdate::Some(&[pid]), false);

        let cpu_percent = stats.sys.global_cpu_usage();
        let (app_mem_bytes, app_cpu_percent) = match stats.sys.process(pid) {
            Some(p) => (p.memory(), p.cpu_usage()),
            None => (0, 0.0),
        };

        stats.cpu_percent = cpu_percent;
        stats.app_mem_bytes = app_mem_bytes;
        stats.app_cpu_percent = app_cpu_percent;
    }
}

/* ---------- Chunk-Grid via Gizmos ---------- */

fn draw_chunk_grid(
    mut gizmos: Gizmos<ChunkGridGizmos>,
    grid: Res<DebugGridState>,
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    cfg: Res<GameConfig>,
) {
    if !grid.show { return; }
    let Ok(cam_tf) = q_cam.single() else { return; };

    let s = VOXEL_SIZE;
    let w = CX as f32 * s;
    let d = CZ as f32 * s;

    let cam_pos = cam_tf.translation();
    let cam_block_x = (cam_pos.x / s).floor() as i32;
    let cam_block_z = (cam_pos.z / s).floor() as i32;
    let (center_c, _) = world_to_chunk_xz(cam_block_x, cam_block_z);

    let radius   = cfg.graphics.chunk_range;

    let eps      = 0.02;
    let y_bottom = Y_MIN as f32 * s + eps;
    let y_top    = Y_MAX as f32 * s - eps;
    let height   = y_top - y_bottom;

    let col_edge   = Color::srgb_u8(255, 235, 59);
    let col_corner = Color::srgb_u8(255, 165, 0);

    const LEVEL_STEP: i32 = SEC_H as i32;

    for cz in (center_c.y - radius)..=(center_c.y + radius) {
        for cx in (center_c.x - radius)..=(center_c.x + radius) {
            let base_b = Vec3::new(
                (cx * CX as i32) as f32 * s,
                y_bottom,
                (cz * CZ as i32) as f32 * s,
            );

            let p0b = base_b;
            let p1b = base_b + Vec3::X * w;
            let p2b = base_b + Vec3::X * w + Vec3::Z * d;
            let p3b = base_b + Vec3::Z * d;

            let p0t = p0b + Vec3::Y * height;
            let p1t = p1b + Vec3::Y * height;
            let p2t = p2b + Vec3::Y * height;
            let p3t = p3b + Vec3::Y * height;

            gizmos.line(p0b, p0t, col_corner);
            gizmos.line(p1b, p1t, col_corner);
            gizmos.line(p2b, p2t, col_corner);
            gizmos.line(p3b, p3t, col_corner);

            let mut y_i = Y_MIN;
            while y_i <= Y_MAX {
                let y = (y_i as f32) * s + eps;

                let base = Vec3::new(
                    (cx * CX as i32) as f32 * s,
                    y,
                    (cz * CZ as i32) as f32 * s,
                );

                let q0 = base;
                let q1 = base + Vec3::X * w;
                let q2 = base + Vec3::X * w + Vec3::Z * d;
                let q3 = base + Vec3::Z * d;

                gizmos.line(q0, q1, col_edge);
                gizmos.line(q1, q2, col_edge);
                gizmos.line(q2, q3, col_edge);
                gizmos.line(q3, q0, col_edge);

                y_i += LEVEL_STEP;
            }
        }
    }
}

fn fmt_mem_from_bytes(bytes: u64) -> String {
    const BYTES_PER_MIB: f64 = 1024.0 * 1024.0;
    const BYTES_PER_GIB: f64 = 1024.0 * 1024.0 * 1024.0;
    let b = bytes as f64;
    if b >= BYTES_PER_GIB {
        format!("{:.1} GB", b / BYTES_PER_GIB)
    } else {
        format!("{:.0} MB", b / BYTES_PER_MIB)
    }
}

fn backend_to_str(b: &str) -> &'static str {
    match b {
        "vulkan" => "Vulkan",
        "gl" => "OpenGL",
        "metal" => "Metal",
        "dx12" | "DX12" => "DirectX12",
        "dx11" | "DX11" => "DirectX11",
        _ => "Unknown",
    }
}

const DIR8: [&str; 8] = [
    "North","North East","East","South East","South","South West","West","North West"
];

fn facing_dir_from_cam(cam_gt: &GlobalTransform) -> (&'static str, f32) {
    let (_, rot, _) = cam_gt.to_scale_rotation_translation();
    let v = (rot * -Vec3::Z).xz();

    if v.length_squared() < f32::EPSILON { return ("North", 0.0); }

    let deg_cw  = v.y.neg().atan2(v.x).to_degrees().rem_euclid(360.0);
    let deg_ccw = (360.0 - deg_cw).rem_euclid(360.0);
    let sector  = ((deg_ccw + 22.5) / 45.0).floor() as usize % 8;
    (DIR8[sector], deg_cw)
}

fn overlay_visible(state: Option<Res<DebugOverlayState>>) -> bool {
    state.map_or(false, |s| s.show)
}

fn map01(x: f32) -> f32 { x * 0.5 + 0.5 }
fn smoothstep(e0: f32, e1: f32, x: f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}
fn size_to_str(s: &BiomeSize) -> &'static str {
    match s {
        BiomeSize::Small    => "Small",
        BiomeSize::Medium   => "Medium",
        BiomeSize::Large    => "Large",
        BiomeSize::Gigantic => "Gigantic",
    }
}
fn size_len_chunks(s: &BiomeSize) -> f32 {
    match s {
        BiomeSize::Small    => ((2.0   + 8.0  ) * 0.5f32).sqrt(),
        BiomeSize::Medium   => ((18.0  + 22.0 ) * 0.5f32).sqrt(),
        BiomeSize::Large    => ((54.0  + 60.0 ) * 0.5f32).sqrt(),
        BiomeSize::Gigantic => ((120.0 + 128.0) * 0.5f32).sqrt(),
    }
}
fn size_freq(s: &BiomeSize) -> f32 {
    1.0 / (size_len_chunks(s) * (CX as f32))
}
fn size_salt(s: &BiomeSize) -> i32 {
    match s {
        BiomeSize::Small    => 0x10000,
        BiomeSize::Medium   => 0x20000,
        BiomeSize::Large    => 0x30000,
        BiomeSize::Gigantic => 0x40000,
    }
}
fn hash32_name(name: &str) -> i32 {
    const OFF: u32 = 0x811C9DC5;
    const PRM: u32 = 16777619;
    let mut h = OFF;
    for &b in name.as_bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(PRM);
    }
    h as i32
}
fn active_size_label(name: &str, sizes: &[BiomeSize], seed: i32, x: f32, z: f32) -> String {
    let list: Vec<BiomeSize> = if sizes.is_empty() { vec![BiomeSize::Medium] } else { sizes.to_vec() };
    let salt_name = hash32_name(name);
    let mut best: (&BiomeSize, f32) = (&list[0], -1.0);

    for s in &list {
        let mut n = FastNoiseLite::with_seed(seed ^ 0x512E_0000 ^ salt_name ^ size_salt(s));
        n.set_noise_type(Some(NoiseType::OpenSimplex2));
        n.set_frequency(Some(size_freq(s).max(0.0003)));
        let g = 0.1 + 0.9 * smoothstep(0.25, 0.75, map01(n.get_noise_2d(x, z)));
        if g > best.1 { best = (s, g); }
    }
    size_to_str(best.0).to_string()
}