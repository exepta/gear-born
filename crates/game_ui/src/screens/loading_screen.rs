use bevy::prelude::*;
use bevy_extended_ui::widgets::ProgressBar;
use game_core::configuration::GameConfig;
use game_core::load_state::{LoadingPhase, LoadingProgress, LoadingTarget, PhaseDetail};
use game_core::states::{AppState, LoadingStates};
use game_core::world::chunk::{CaveTracker, ChunkMap, LoadCenter};
use game_core::world::fluid::FluidMap;

pub struct LoadingScreen;

impl Plugin for LoadingScreen {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<LoadingProgress>()
            .add_systems(
                OnEnter(AppState::Loading(LoadingStates::BaseGen)),
                enter_loading
            )
            .add_systems(
                Update,
                (update_loading_progress, sync_loading_progressbar)
                    .run_if(
                        in_state(AppState::Loading(LoadingStates::BaseGen))
                            .or(in_state(AppState::Loading(LoadingStates::WaterGen)))
                            .or(in_state(AppState::Loading(LoadingStates::CaveGen)))
                    )
            )
            .add_systems(
                OnExit(AppState::Loading(LoadingStates::CaveGen)),
                clear_loading_resources
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

fn sync_loading_progressbar(
    lp: Option<Res<LoadingProgress>>,
    mut q_bar: Query<&mut ProgressBar>,
) {
    let Some(lp) = lp else {
        return;
    };

    let value = (lp.overall_pct * 100.0).clamp(0.0, 100.0);
    for mut bar in q_bar.iter_mut() {
        bar.min = 0.0;
        bar.max = 100.0;
        bar.value = value;
    }
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
