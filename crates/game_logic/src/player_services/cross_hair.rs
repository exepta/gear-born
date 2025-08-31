use bevy::prelude::*;
use bevy::render::view::{NoFrustumCulling, RenderLayers};
use game_core::configuration::CrosshairConfig;
use game_core::states::{AppState, InGameStates};
use std::f32::consts::TAU;

#[derive(Component)]
struct Crosshair;

pub struct CrosshairPlugin;
impl Plugin for CrosshairPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(
                OnEnter(AppState::InGame(InGameStates::Game)),
                setup_crosshair,
            )
            .add_systems(
                Update,
                toggle_crosshair_visibility.run_if(in_state(
                    AppState::InGame(InGameStates::Game)
                )),
            );
    }
}

/// Spawns (if necessary) a dedicated 2D overlay camera and a ring-shaped crosshair mesh
/// rendered on an isolated render layer.
///
/// Behavior:
/// - Ensures a single `Camera` exists for overlays; if none is found, spawns one
///   with `order = 1`, `clear_color = None`, and `RenderLayers::layer(3)`.
/// - Builds a torus-like (ring) 2D mesh using [`build_ring_mesh`] with the
///   configured outer radius, inner radius (`radius - thickness`, clamped at 0),
///   and segment count.
/// - Spawns the crosshair entity using `ColorMaterial` on the same render layer (3),
///   with `NoFrustumCulling` to keep it always visible.
///
/// Expects a user-defined:
/// - `CrosshairConfig` resource providing: `radius`, `thickness`, `segments`, `color`,
///   and `visible_when_unlocked`.
/// - `Crosshair` marker component.
fn setup_crosshair(
    mut commands: Commands,
    cfg: Res<CrosshairConfig>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut mats: ResMut<Assets<ColorMaterial>>,
    q_cam2d: Query<Entity, With<Camera2d>>,
) {
    if q_cam2d.single().is_err() {
        commands.spawn((
            Camera2d,
            Camera {
                order: 10,
                clear_color: ClearColorConfig::None,
                ..default()
            },
            RenderLayers::layer(3),
            Name::new("OverlayCamera2D"),
        ));
    }

    let inner = (cfg.radius - cfg.thickness).max(0.0);
    let ring_mesh = build_ring_mesh(cfg.radius, inner, cfg.segments);

    let mesh_h = meshes.add(ring_mesh);
    let mat_h = mats.add(ColorMaterial {
        color: cfg.color,
        ..default()
    });

    commands.spawn((
        Mesh2d(mesh_h),
        MeshMaterial2d(mat_h),
        NoFrustumCulling,
        Transform::from_xyz(0.0, 0.0, 0.0),
        RenderLayers::layer(3),
        Crosshair,
        Name::new("CrosshairRing"),
    ));
}

/// Toggles the `Visibility` of the crosshair based on the cursor grab state.
///
/// Visible when:
/// - The primary window has `CursorGrabMode::Locked` (typical FPS mode), **or**
/// - `cfg.visible_when_unlocked` is `true`.
///
/// If no crosshair or window exists yet, the system exits early.
fn toggle_crosshair_visibility(
    mut q_cross: Query<&mut Visibility, With<Crosshair>>,
    windows: Query<&Window, With<bevy::window::PrimaryWindow>>,
    cfg: Res<CrosshairConfig>,
) {
    let Ok(mut vis) = q_cross.single_mut() else { return; };
    let Ok(win) = windows.single() else { return; };
    let locked = win.cursor_options.grab_mode == bevy::window::CursorGrabMode::Locked;

    *vis = if locked || cfg.visible_when_unlocked {
        Visibility::Visible
    } else {
        Visibility::Hidden
    };
}

/// Builds a 2D ring mesh (triangle list) lying on the XY-plane with outward-facing normals.
///
/// - `outer_r`: outer radius of the ring (must be ≥ 0).
/// - `inner_r`: inner radius of the ring (will be treated as-is; pass 0 for a filled disk).
/// - `segments`: number of radial segments (clamped to at least 8).
///
/// The mesh layout:
/// - Two concentric vertex loops (outer and inner), each with `segments + 1` vertices
///   to close the loop.
/// - Indices form quads between successive pairs, split into two triangles.
/// - Normals point toward +Z; UVs are stubbed to (0.5, 0.5) for all vert (not used by `ColorMaterial`).
fn build_ring_mesh(outer_r: f32, inner_r: f32, segments: usize) -> Mesh {
    let segments = segments.max(8);
    let mut positions: Vec<[f32; 3]> = Vec::with_capacity((segments + 1) * 2);
    let mut indices: Vec<u32> = Vec::with_capacity(segments * 6);

    for i in 0..=segments {
        let t = (i as f32 / segments as f32) * TAU;
        let (s, c) = t.sin_cos();
        positions.push([c * outer_r, s * outer_r, 0.0]);
        positions.push([c * inner_r, s * inner_r, 0.0]);

        if i < segments {
            let base = (i * 2) as u32;
            indices.extend_from_slice(&[
                base, base + 1, base + 2,
                base + 1, base + 3, base + 2,
            ]);
        }
    }

    let normals = vec![[0.0, 0.0, 1.0]; positions.len()];
    let uvs = vec![[0.5, 0.5]; positions.len()];

    let mut mesh = Mesh::new(
        bevy::render::mesh::PrimitiveTopology::TriangleList,
        bevy::asset::RenderAssetUsages::RENDER_WORLD,
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(bevy::render::mesh::Indices::U32(indices));
    mesh
}