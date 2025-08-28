use std::collections::HashMap;
use std::path::PathBuf;
use bevy::prelude::*;
use bevy::tasks::Task;

#[derive(Resource, Clone)]
pub struct WorldSave {
    pub root: PathBuf,
    pub chunk_dir: PathBuf,
    pub version: u32
}

#[derive(Resource, Default)]
pub struct PendingSave(pub HashMap<IVec2, Task<(IVec2, bool)>>);

#[derive(Resource, Default)]
pub struct SaveQueue {
    pub due: HashMap<IVec2, f32>,
}

#[derive(Resource)]
pub struct AutoSaveCfg {
    pub debounce_sec: f32,
    pub budget_per_tick: usize,
}

impl Default for AutoSaveCfg {
    fn default() -> Self {
        Self { debounce_sec: 0.5, budget_per_tick: 2 }
    }
}
