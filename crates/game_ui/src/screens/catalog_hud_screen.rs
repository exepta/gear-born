use bevy::prelude::*;
use bevy::render::camera::{ImageRenderTarget, RenderTarget};
use bevy::render::mesh::{Indices, PrimitiveTopology};
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages};
use bevy::render::view::RenderLayers;
use bevy::ui::FocusPolicy;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::*;
use game_core::BlockCatalogPreviewCam;

/* ---------------- Plugin ---------------- */

pub struct BlockCatalogUiPlugin;

impl Plugin for BlockCatalogUiPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<BlockCatalogUiState>()
            .add_systems(
                OnEnter(AppState::InGame(InGameStates::Game)),
                spawn_block_catalog_ui.run_if(resource_exists::<BlockRegistry>),
            )
            .add_systems(
                Update,
                (toggle_block_catalog_ui, handle_block_pick_interactions, spin_previews)
                    .run_if(in_state(AppState::InGame(InGameStates::Game)))
                    .run_if(resource_exists::<BlockCatalogUiState>)
                    .run_if(resource_exists::<BlockRegistry>),
            );
    }
}

/* ---------------- State / Components ---------------- */

#[derive(Resource, Default)]
struct BlockCatalogUiState {
    open: bool,
    root: Option<Entity>,
}

#[derive(Component)]
struct BlockCatalogRoot;

#[derive(Component)]
struct BlockItemButton {
    block_id: BlockId,
}

#[derive(Component)]
struct PreviewAnchor; // rotates, centered at origin

#[derive(Component)]
struct PreviewMesh;

#[derive(Component)]
struct Spin {
    speed: f32,
}

/* ---------------- Constants ---------------- */

const PANEL_WIDTH_PX: f32 = 550.0;
const PREVIEW_SIZE_PX: u32 = 160;
const PREVIEW_LAYER_BASE: usize = 3;
const PREVIEW_CAM_FOV_DEG: f32 = 32.0;

/* ---------------- Systems ---------------- */

fn toggle_block_catalog_ui(
    keys: Res<ButtonInput<KeyCode>>,
    mut ui: ResMut<BlockCatalogUiState>,
    mut q_vis: Query<&mut Visibility, With<BlockCatalogRoot>>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
) {
    if !keys.just_pressed(KeyCode::Tab) { return; }

    ui.open = !ui.open;

    if let Some(root) = ui.root {
        if let Ok(mut v) = q_vis.get_mut(root) {
            *v = if ui.open { Visibility::Visible } else { Visibility::Hidden };
        }
    }

    if let Ok(mut win) = windows.single_mut() {
        if ui.open {
            win.cursor_options.grab_mode = CursorGrabMode::None;
            win.cursor_options.visible = true;
        }
    }
}

fn handle_block_pick_interactions(
    mut q_btn: Query<(&Interaction, &BlockItemButton, &mut BackgroundColor), (Changed<Interaction>, With<Button>)>,
    mut selected: ResMut<SelectedBlock>,
    reg: Res<BlockRegistry>,
) {
    for (interaction, item, mut bg) in &mut q_btn {
        match *interaction {
            Interaction::Pressed => {
                selected.id = item.block_id;
                selected.name = reg.name(item.block_id).to_string();
                *bg = BackgroundColor(Color::srgb_u8(66, 165, 245));
            }
            Interaction::Hovered => *bg = BackgroundColor(Color::srgb_u8(120,120,120)),
            Interaction::None    => *bg = BackgroundColor(Color::srgb_u8(80,80,80)),
        }
    }
}

fn spin_previews(time: Res<Time>, mut q: Query<(&mut Transform, &Spin), With<PreviewAnchor>>) {
    for (mut tf, spin) in &mut q {
        tf.rotate_y(spin.speed * time.delta_secs());
    }
}

/* ---------------- Spawn UI + Previews ---------------- */

fn spawn_block_catalog_ui(
    mut commands: Commands,
    mut ui: ResMut<BlockCatalogUiState>,
    reg: Res<BlockRegistry>,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    let root = commands
        .spawn((
            BlockCatalogRoot,
            Node {
                position_type: PositionType::Absolute,
                right: Val::Px(0.0),
                top: Val::Px(0.0),
                width: Val::Px(PANEL_WIDTH_PX),
                height: Val::Percent(100.0),
                padding: UiRect::axes(Val::Px(10.0), Val::Px(10.0)),
                flex_direction: FlexDirection::Column,
                display: Display::Flex,
                ..default()
            },
            BackgroundColor(Color::srgba(0.06, 0.06, 0.07, 0.92)),
            Visibility::Hidden,
            Name::new("BlockCatalog:Root"),
        ))
        .id();

    // Header
    let header = commands
        .spawn((
            Node {
                height: Val::Px(40.0),
                justify_content: JustifyContent::SpaceBetween,
                align_items: AlignItems::Center,
                ..default()
            },
            Visibility::default(),
            Name::new("BlockCatalog:Header"),
        ))
        .with_children(|c| {
            c.spawn((
                Text::new("Blocks"),
                TextFont { font_size: 20.0, ..default() },
                TextColor(Color::WHITE),
                Visibility::default(),
            ));
            c.spawn((
                Text::new("(TAB to toggle)"),
                TextFont { font_size: 14.0, ..default() },
                TextColor(Color::srgba(1.0,1.0,1.0,0.6)),
                Visibility::default(),
            ));
        })
        .id();

    // Grid
    let grid = commands
        .spawn((
            Node {
                flex_grow: 1.0,
                flex_wrap: FlexWrap::Wrap,
                align_content: AlignContent::FlexStart,
                align_items: AlignItems::FlexStart,
                column_gap: Val::Px(10.0),
                row_gap: Val::Px(10.0),
                overflow: Overflow::clip_y(),
                ..default()
            },
            Visibility::default(),
            Name::new("BlockCatalog:Grid"),
        ))
        .id();

    commands.entity(root).add_child(header);
    commands.entity(root).add_child(grid);

    // Shared light (layer matches previews)
    commands.spawn((
        DirectionalLight { illuminance: 1000.0, ..default() },
        Transform::from_xyz(7.0, 10.0, 6.0).looking_at(Vec3::ZERO, Vec3::Y),
        GlobalTransform::default(),
        Visibility::default(),
        RenderLayers::layer(PREVIEW_LAYER_BASE),
        Name::new("Preview:Light"),
    ));

    // Items
    for (block_id_idx, def) in reg.defs.iter().enumerate() {
        let block_id = block_id_idx as BlockId;
        if block_id == 0 { continue; } // skip air

        let rt = make_render_texture(&mut images, PREVIEW_SIZE_PX, PREVIEW_SIZE_PX);
        let rt_for_ui = rt.clone();

        commands.spawn((
            BlockCatalogPreviewCam,
            Camera3d::default(),
            Camera {
                clear_color: ClearColorConfig::Custom(Color::srgba(0.20, 0.22, 0.26, 1.0)),
                target: RenderTarget::Image(ImageRenderTarget::from(rt)),
                ..default()
            },
            Projection::Perspective(PerspectiveProjection {
                fov: PREVIEW_CAM_FOV_DEG.to_radians(),
                near: 0.01,
                far: 50.0,
                ..default()
            }),
            Transform::from_xyz(2.2, 1.8, 2.2).looking_at(Vec3::ZERO, Vec3::Y),
            GlobalTransform::default(),
            Visibility::default(),
            RenderLayers::layer(PREVIEW_LAYER_BASE),
            Name::new(format!("Preview:Cam({})", def.name)),
        ));

        // Rotating anchor at origin; mesh is child translated by -0.5
        let anchor = commands
            .spawn((
                PreviewAnchor,
                Spin { speed: 0.8 },
                Transform::IDENTITY,
                GlobalTransform::default(),
                Visibility::default(),
                RenderLayers::layer(PREVIEW_LAYER_BASE),
                Name::new(format!("Preview:Anchor({})", def.name)),
            ))
            .id();

        let mesh = make_block_mesh(&reg, block_id, 1.0);
        commands.entity(anchor).with_children(|c| {
            c.spawn((
                PreviewMesh,
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(def.material.clone()),
                Transform::from_translation(Vec3::splat(-0.5))
                    .with_rotation(Quat::from_euler(EulerRot::XYZ, -0.35, 0.0, 0.0))
                    .with_scale(Vec3::splat(0.95)),
                GlobalTransform::default(),
                Visibility::default(),
                RenderLayers::layer(PREVIEW_LAYER_BASE),
                Name::new(format!("Preview:Mesh({})", def.name)),
            ));
        });

        // UI Button (children forward pointer with FocusPolicy::Pass)
        let button = commands
            .spawn((
                Button,
                BlockItemButton { block_id },
                Node {
                    width: Val::Px(PREVIEW_SIZE_PX as f32),
                    height: Val::Px(PREVIEW_SIZE_PX as f32 + 28.0),
                    flex_direction: FlexDirection::Column,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::FlexStart,
                    padding: UiRect::all(Val::Px(6.0)),
                    border: UiRect::all(Val::Px(1.0)),
                    ..default()
                },
                BorderColor(Color::srgba(1.0, 1.0, 1.0, 0.08)),
                BackgroundColor(Color::srgb_u8(80, 80, 80)),
                Visibility::default(),
                Name::new(format!("BlockItem:{}", def.name)),
            ))
            .with_children(|c| {
                c.spawn((
                    Node {
                        width: Val::Px(PREVIEW_SIZE_PX as f32),
                        height: Val::Px(PREVIEW_SIZE_PX as f32),
                        ..default()
                    },
                    ImageNode::new(rt_for_ui),
                    FocusPolicy::Pass,
                    Visibility::default(),
                    Name::new("BlockItem:Preview"),
                ));
                c.spawn((
                    Text::new(def.name.clone()),
                    TextFont { font_size: 13.0, ..default() },
                    TextColor(Color::srgba(1.0, 1.0, 1.0, 0.85)),
                    FocusPolicy::Pass,
                    Visibility::default(),
                    Name::new("BlockItem:Label"),
                ));
            })
            .id();

        commands.entity(grid).add_child(button);
    }

    ui.root = Some(root);
}

/* ---------------- Helpers ---------------- */

fn make_render_texture(images: &mut Assets<Image>, w: u32, h: u32) -> Handle<Image> {
    let mut image = Image::new_fill(
        Extent3d { width: w, height: h, depth_or_array_layers: 1 },
        TextureDimension::D2,
        &[0, 0, 0, 0],
        TextureFormat::bevy_default(),
        RenderAssetUsages::RENDER_WORLD | RenderAssetUsages::MAIN_WORLD,
    );
    image.texture_descriptor.usage =
        TextureUsages::RENDER_ATTACHMENT
            | TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_SRC
            | TextureUsages::COPY_DST;
    images.add(image)
}

fn make_block_mesh(reg: &BlockRegistry, id: BlockId, size: f32) -> Mesh {
    let f = FaceUvRectsLocal {
        top:    reg.uv(id, Face::Top),
        bottom: reg.uv(id, Face::Bottom),
        north:  reg.uv(id, Face::North),
        east:   reg.uv(id, Face::East),
        south:  reg.uv(id, Face::South),
        west:   reg.uv(id, Face::West),
    };
    cube_mesh_with_face_uvs_preview(&f, size)
}

#[derive(Clone)]
struct FaceUvRectsLocal {
    top: UvRect, bottom: UvRect, north: UvRect, east: UvRect, south: UvRect, west: UvRect
}

fn cube_mesh_with_face_uvs_preview(f: &FaceUvRectsLocal, size: f32) -> Mesh {
    #[inline]
    fn quad_uv(uv: &UvRect, flip_v: bool) -> [[f32;2];4] {
        if !flip_v {
            [[uv.u0,uv.v0],[uv.u1,uv.v0],[uv.u1,uv.v1],[uv.u0,uv.v1]]
        } else {
            [[uv.u0,uv.v1],[uv.u1,uv.v1],[uv.u1,uv.v0],[uv.u0,uv.v0]]
        }
    }
    let s = size;

    let mut pos = Vec::with_capacity(24);
    let mut nrm = Vec::with_capacity(24);
    let mut uvs = Vec::with_capacity(24);
    let mut idx = Vec::with_capacity(36);

    let mut push = |quad: [[f32;3];4], normal:[f32;3], uv:&UvRect, flip_v: bool| {
        let base = pos.len() as u32;
        pos.extend_from_slice(&quad);
        nrm.extend_from_slice(&[normal;4]);
        uvs.extend_from_slice(&quad_uv(uv, flip_v));
        idx.extend_from_slice(&[base, base+1, base+2, base, base+2, base+3]);
    };

    push([[s,0.0,s],[s,0.0,0.0],[s,s,0.0],[s,s,s]], [ 1.0,0.0, 0.0], &f.east,   true);  // +X
    push([[0.0,0.0,0.0],[0.0,0.0,s],[0.0,s,s],[0.0,s,0.0]], [-1.0,0.0, 0.0], &f.west,   true);  // -X
    push([[0.0,s,s],[s,s,s],[s,s,0.0],[0.0,s,0.0]],        [ 0.0,1.0, 0.0], &f.top,    false); // +Y
    push([[0.0,0.0,0.0],[s,0.0,0.0],[s,0.0,s],[0.0,0.0,s]], [ 0.0,-1.0,0.0], &f.bottom, false); // -Y
    push([[0.0,0.0,s],[s,0.0,s],[s,s,s],[0.0,s,s]],        [ 0.0,0.0, 1.0], &f.south,  true);  // +Z
    push([[s,0.0,0.0],[0.0,0.0,0.0],[0.0,s,0.0],[s,s,0.0]], [ 0.0,0.0,-1.0], &f.north,  true);  // -Z

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   nrm);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0,     uvs);
    mesh.insert_indices(Indices::U32(idx));
    mesh
}
