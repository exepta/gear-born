use bevy::prelude::*;

#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum LoadingPhase { #[default] BaseGen, WaterGen, Done }

#[derive(Clone, Copy, Default, Debug)]
pub struct PhaseDetail {
    pub gen_done:  usize,
    pub gen_total: usize,
    pub mesh_done: usize,
    pub mesh_total:usize,
    pub pct: f32,
}

#[derive(Resource, Clone, Default, Debug)]
pub struct LoadingProgress {
    pub phase: LoadingPhase,
    pub base:  PhaseDetail,
    pub water: PhaseDetail,
    pub overall_pct: f32,
}

#[derive(Resource, Clone, Copy, Debug)]
pub struct LoadingTarget {
    pub center: IVec2,
    pub radius: i32,
}