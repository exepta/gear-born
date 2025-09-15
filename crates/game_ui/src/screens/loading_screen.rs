use bevy::prelude::*;
use game_core::configuration::GameConfig;
use game_core::load_state::{LoadingPhase, LoadingProgress, LoadingTarget, PhaseDetail};
use game_core::states::{AppState, LoadingStates};
use game_core::world::chunk::{CaveTracker, ChunkMap, LoadCenter};
use game_core::world::fluid::FluidMap;
use game_core::UI_ACCENT_COLOR;

#[derive(Component)]
struct LoadingScreenRoot;

#[derive(Component)]
struct LoadingTextTag;

#[derive(Component)]
struct ProgressBarRoot;

#[derive(Component)]
struct ProgressCell(pub usize);

pub struct LoadingScreen;

impl Plugin for LoadingScreen {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<LoadingProgress>()
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::BaseGen)),
                (enter_loading, spawn_loading_ui)
            )
            .add_systems(
                Update,
                (update_loading_progress, update_loading_text, update_progress_bar)
                    .run_if(
                        in_state(AppState::Loading(LoadingStates::BaseGen))
                            .or(in_state(AppState::Loading(LoadingStates::WaterGen)))
                            .or(in_state(AppState::Loading(LoadingStates::CaveGen)))
                    )
            )
            .add_systems(
                OnExit(AppState::Loading(LoadingStates::CaveGen)),
                (despawn_loading_ui, clear_loading_resources)
            );
    }
}

fn enter_loading(mut commands: Commands, game_config: Res<GameConfig>) {
    let initial_radius = game_config.graphics.chunk_range.min(3);
    commands.insert_resource(LoadCenter { world_xz: IVec2::new(0, 0) });
    commands.insert_resource(LoadingTarget {
        center: IVec2::new(0, 0),
        radius: initial_radius,
    });
    commands.insert_resource(LoadingProgress {
        phase: LoadingPhase::BaseGen,
        ..Default::default()
    });
}

fn spawn_loading_ui(mut commands: Commands) {
    commands.spawn((
        Camera2d,
        Camera {
            clear_color: ClearColorConfig::Custom(Color::BLACK),
            ..default()
        },
        LoadingScreenRoot,
        Name::new("LoadingCamera"),
    ));

    commands
        .spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                ..default()
            },
            Visibility::default(),
            BackgroundColor(Color::NONE),
            LoadingScreenRoot,
            Name::new("LoadingRoot"),
        ))
        .with_children(|p| {
            p.spawn((
                Node {
                    flex_direction: FlexDirection::Column,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    row_gap: Val::Px(12.0),
                    ..default()
                },
                BackgroundColor(Color::NONE),
                Name::new("LoadingVBox"),
            ))
                .with_children(|col| {
                    // Text
                    col.spawn((
                        Text(String::from("Loading (0%)")),
                        TextColor(Color::WHITE),
                        TextFont { font_size: 32.0, ..default() },
                        LoadingTextTag,
                        Name::new("LoadingText"),
                    ));

                    col.spawn((
                        Node {
                            width: Val::Px(400.0),
                            height: Val::Px(42.0),
                            padding: UiRect::all(Val::Px(8.0)),
                            align_items: AlignItems::Center,
                            justify_content: JustifyContent::Center,
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.18, 0.18, 0.20)),
                        BorderRadius::all(Val::Px(8.0)),
                        ProgressBarRoot,
                        Name::new("LoadingBar"),
                    ))
                        .with_children(|bar| {
                            bar.spawn((
                                Node {
                                    flex_direction: FlexDirection::Row,
                                    align_items: AlignItems::Center,
                                    justify_content: JustifyContent::Center,
                                    ..default()
                                },
                                BackgroundColor(Color::NONE),
                                Name::new("CellsRow"),
                            ))
                                .with_children(|row| {
                                    for i in 0..10 {
                                        row.spawn((
                                            Node {
                                                width: Val::Px(32.0),
                                                height: Val::Px(32.0),
                                                margin: UiRect {
                                                    left: Val::Px(3.0),
                                                    right: Val::Px(3.0),
                                                    ..default()
                                                },
                                                ..default()
                                            },
                                            BackgroundColor(Color::NONE),
                                            ProgressCell(i),
                                            Name::new(format!("Cell{}", i)),
                                        ));
                                    }
                                });
                        });
                });
        });
}

fn despawn_loading_ui(mut commands: Commands, q: Query<Entity, With<LoadingScreenRoot>>) {
    for e in q.iter() { commands.entity(e).despawn(); }
}

fn update_progress_bar(
    lp: Option<Res<LoadingProgress>>,
    mut q_cells: Query<(&ProgressCell, &mut BackgroundColor)>,
) {
    let Some(lp) = lp else { return; };
    let filled = ((lp.overall_pct * 10.0).floor() as i32).clamp(0, 10);

    for (cell, mut bg) in q_cells.iter_mut() {
        if (cell.0 as i32) < filled {
            *bg = BackgroundColor(UI_ACCENT_COLOR);
        } else {
            *bg = BackgroundColor(Color::NONE);
        }
    }
}

fn update_loading_text(
    lp: Option<Res<LoadingProgress>>,
    mut q_txt: Query<&mut Text, With<LoadingTextTag>>,
) {
    let Ok(mut text) = q_txt.single_mut() else { return; };
    let Some(lp) = lp else { return; };

    let pct = (lp.overall_pct * 100.0).round() as i32;
    text.0 = format!("Loading ({}%)", pct);
}

fn update_loading_progress(
    mut lp: ResMut<LoadingProgress>,
    app_state: Res<State<AppState>>,
    target: Option<Res<LoadingTarget>>,
    chunk_map: Res<ChunkMap>,
    water: Option<Res<FluidMap>>,
    // ✨ Neu: Cave-Progress
    cave_tracker: Option<Res<CaveTracker>>,
) {
    let Some(target) = target else { return; };
    let center = target.center;
    let radius = target.radius;

    let total_chunks = square_count(radius);

    // --- BASE (Terrain) nur Gen ---
    let mut gen_done_base = 0usize;
    for (&c, _) in chunk_map.chunks.iter() {
        if in_area(center, radius, c) { gen_done_base += 1; }
    }
    let base_pct = if total_chunks > 0 {
        gen_done_base as f32 / total_chunks as f32
    } else { 0.0 };

    // --- WATER nur Gen ---
    let mut gen_done_water = 0usize;
    if let Some(w) = &water {
        for (&c, _) in w.0.iter() {
            if in_area(center, radius, c) { gen_done_water += 1; }
        }
    }
    let water_pct = if total_chunks > 0 {
        gen_done_water as f32 / total_chunks as f32
    } else { 0.0 };

    // --- CAVES nur Gen (done-Set aus CaveTracker) ---
    let mut gen_done_cave = 0usize;
    if let Some(ct) = &cave_tracker {
        for &c in ct.done.iter() {
            if in_area(center, radius, c) { gen_done_cave += 1; }
        }
    }
    let cave_pct = if total_chunks > 0 {
        gen_done_cave as f32 / total_chunks as f32
    } else { 0.0 };

    // Phase
    let phase = match app_state.get() {
        AppState::Loading(LoadingStates::BaseGen)  => LoadingPhase::BaseGen,
        AppState::Loading(LoadingStates::WaterGen) => LoadingPhase::WaterGen,
        AppState::Loading(LoadingStates::CaveGen)  => LoadingPhase::CaveGen,
        _ => LoadingPhase::Done,
    };

    // Overall (1/3 je Phase)
    const W_BASE: f32 = 1.0 / 3.0;
    const W_WATER: f32 = 1.0 / 3.0;
    const W_CAVE: f32 = 1.0 / 3.0;

    let overall = match phase {
        LoadingPhase::BaseGen  => clamp01(base_pct * W_BASE),
        LoadingPhase::WaterGen => clamp01(W_BASE + water_pct * W_WATER),
        LoadingPhase::CaveGen  => clamp01(W_BASE + W_WATER + cave_pct * W_CAVE),
        LoadingPhase::Done     => 1.0,
    };

    lp.phase = phase;
    lp.base = PhaseDetail {
        gen_done: gen_done_base, gen_total: total_chunks,
        mesh_done: 0, mesh_total: 0,
        pct: base_pct,
    };
    lp.water = PhaseDetail {
        gen_done: gen_done_water, gen_total: total_chunks,
        mesh_done: 0, mesh_total: 0,
        pct: water_pct,
    };
    
    lp.cave = PhaseDetail {
        gen_done: gen_done_cave, gen_total: total_chunks,
        mesh_done: 0, mesh_total: 0,
        pct: cave_pct,
    };
    lp.overall_pct = overall;
}

fn clear_loading_resources(mut commands: Commands) {
    commands.remove_resource::<LoadingTarget>();
    commands.remove_resource::<LoadingProgress>();
}

#[inline]
fn clamp01(x: f32) -> f32 { x.max(0.0).min(1.0) }

#[inline]
fn in_area(center: IVec2, radius: i32, c: IVec2) -> bool {
    (c.x - center.x).abs() <= radius && (c.y - center.y).abs() <= radius
}

#[inline]
fn square_count(radius: i32) -> usize {
    let d = (radius as i64) * 2 + 1;
    (d * d) as usize
}
