use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use rand::{rngs::SmallRng, SeedableRng};

use crate::{
    ant::TeamIndex,
    instruction::{Program, ProgramParseError},
    map::Map,
};

#[cfg(feature = "display")]
#[derive(bevy::prelude::Resource)]
pub struct Game {
    teams: Vec<Team>,
    map: Map,

    rng: SmallRng,
}

#[cfg(not(feature = "display"))]
pub struct Game {
    teams: Vec<Team>,
    map: Map,

    rng: SmallRng,
}
impl Game {
    pub const DEFAULT_TURNS: usize = 100_000;

    pub fn new(teams: Vec<Team>, map: Map, seed: Option<u64>) -> Self {
        Self {
            teams,
            map,
            rng: if let Some(seed) = seed {
                SmallRng::seed_from_u64(seed)
            } else {
                SmallRng::from_entropy()
            },
        }
    }

    pub fn teams(&self) -> &[Team] {
        &self.teams
    }

    pub fn map(&self) -> &Map {
        &self.map
    }

    pub fn seed(&self) -> u64 {
        todo!()
    }

    pub fn update(&mut self) {
        self.map.update(&self.teams, &mut self.rng);
    }

    pub fn play(&mut self, turns: Option<usize>) {
        for _ in 0..turns.unwrap_or(Self::DEFAULT_TURNS) {
            self.update();
        }
    }
}
impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.map)
    }
}

#[derive(Clone)]
pub struct Team {
    pub name: String,
    pub token: char,
    pub index: TeamIndex,
    pub program: Program,

    pub score: Arc<Mutex<u32>>,
}
impl Team {
    pub fn new(name: String, token: char, index: TeamIndex, program: Program) -> Self {
        Self {
            name,
            token,
            index,
            program,

            score: Arc::new(Mutex::new(0)),
        }
    }

    pub fn new_from_file(
        path: PathBuf,
        token: char,
        index: TeamIndex,
    ) -> Result<Self, ProgramParseError> {
        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
        let program = Program::from_file(path)?;
        Ok(Self::new(name, token, index, program))
    }
}
