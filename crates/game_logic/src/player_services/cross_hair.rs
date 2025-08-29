use bevy::prelude::*;
use bevy::render::view::NoFrustumCulling;
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
                order: 1,
                ..default()
            },
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
        Crosshair,
        Name::new("CrosshairRing"),
    ));
}

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