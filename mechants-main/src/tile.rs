use std::sync::{Arc, Mutex};

use colored::{Color, Colorize};

use crate::{
    ant::{Ant, TeamIndex},
    game::Team,
    map::Coordinates,
};

/// Describes the different types of tiles that compose a map.
#[derive(Clone)]
pub enum Tile {
    Rock,
    Filled {
        pheromones: [u8; 2],
        food: u32,
    },
    Empty {
        food: u32,
        surface_hole_above: bool,
        surface_hole_below: bool,
        pheromones: [u8; 2],
        ant: Option<Ant>,
    },
    Home {
        team: TeamIndex,
        food: u32,
        surface_hole_above: bool,
        surface_hole_below: bool,
        pheromones: [u8; 2],
        ant: Option<Ant>,

        score_ref: Arc<Mutex<u32>>,
    },
}
impl Tile {
    pub fn empty(surface_hole_above: bool, surface_hole_below: bool, food: u32) -> Self {
        Self::Empty {
            food,
            surface_hole_above,
            surface_hole_below,
            pheromones: [0; 2],
            ant: None,
        }
    }
    pub fn home(position: Coordinates, team: &Team) -> Self {
        Self::Home {
            team: team.index,
            food: 0,
            surface_hole_above: false,
            surface_hole_below: false,
            pheromones: [0; 2],
            ant: Some(Ant::new(position, team.index)),

            score_ref: team.score.clone(),
        }
    }
    pub fn filled() -> Self {
        Self::Filled {
            pheromones: [0; 2],
            food: 0,
        }
    }

    pub fn is_home(&self) -> bool {
        matches!(self, Self::Home { .. })
    }
    pub fn is_home_of_team(&self, team: TeamIndex) -> bool {
        match self {
            Self::Home { team: t, .. } => *t == team,
            _ => false,
        }
    }
    pub fn is_home_of_enemy(&self, team: TeamIndex) -> bool {
        match self {
            Self::Home { team: t, .. } => *t != team,
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(
            self,
            Self::Empty { ant: None, .. } | Self::Home { ant: None, .. }
        )
    }

    pub fn take_ant(&mut self) -> Option<Ant> {
        match self {
            Self::Empty { ant, .. } | Self::Home { ant, .. } => ant.take(),
            _ => None,
        }
    }
    pub fn place_ant(&mut self, new_ant: Ant) {
        match self {
            Self::Empty { ant, .. } | Self::Home { ant, .. } => *ant = Some(new_ant),
            _ => panic!("cannot place ant on non-walkable tile"),
        }
    }
    pub fn get_ant(&self) -> Option<&Ant> {
        match self {
            Self::Empty { ant, .. } | Self::Home { ant, .. } => ant.as_ref(),
            _ => None,
        }
    }
    pub fn get_ant_mut(&mut self) -> Option<&mut Ant> {
        match self {
            Self::Empty { ant: Some(a), .. } | Self::Home { ant: Some(a), .. } => Some(a),
            _ => None,
        }
    }

    pub fn has_food(&self) -> bool {
        match self {
            Self::Empty { food, .. } | Self::Home { food, .. } => *food != 0,
            _ => false,
        }
    }
    pub fn take_food(&mut self, amount: u32) -> bool {
        match self {
            Self::Empty { food, .. } | Self::Home { food, .. } if *food < amount => false,
            Self::Empty { food, .. } => {
                *food -= amount;
                true
            }
            Self::Home {
                food, score_ref, ..
            } => {
                *food -= amount;
                *score_ref.lock().unwrap() -= amount;
                true
            }
            _ => false,
        }
    }
    pub fn place_food(&mut self, amount: u32) {
        match self {
            Self::Empty { food, .. } => *food += amount,
            Self::Home {
                food, score_ref, ..
            } => {
                *food += amount;
                *score_ref.lock().unwrap() += amount;
            }
            _ => panic!("cannot place food on a non-walkable tile"),
        }
    }

    pub fn any_enemy_pheromones(&self, team: TeamIndex) -> bool {
        match self {
            Self::Empty { pheromones, .. }
            | Self::Home { pheromones, .. }
            | Self::Filled { pheromones, .. } => pheromones
                .iter()
                .enumerate()
                .any(|(t, p)| t != team as usize && *p != 0),
            _ => false,
        }
    }
    pub fn marked_pheromone(&self, team: TeamIndex, index: u8) -> bool {
        match self {
            Self::Empty { pheromones, .. }
            | Self::Home { pheromones, .. }
            | Self::Filled { pheromones, .. } => (pheromones[team as usize] & (1 << index)) != 0,
            _ => false,
        }
    }
    pub fn mark_pheromone(&mut self, team: TeamIndex, index: u8) {
        match self {
            Self::Empty { pheromones, .. } | Self::Home { pheromones, .. } => {
                pheromones[team as usize] |= 1 << index
            }
            _ => panic!("cannot mark a non-walkable tile"),
        }
    }
    pub fn unmark_pheromone(&mut self, team: TeamIndex, index: u8) {
        match self {
            Self::Empty { pheromones, .. } | Self::Home { pheromones, .. } => {
                pheromones[team as usize] &= !(1 << index)
            }
            _ => panic!("cannot mark a non-walkable tile"),
        }
    }

    pub fn dig(&mut self) -> bool {
        match self {
            Self::Filled { pheromones, food } => {
                *self = Self::Empty {
                    food: std::mem::take(food),
                    ant: None,
                    pheromones: std::mem::take(pheromones),
                    surface_hole_below: false,
                    surface_hole_above: false,
                };
                true
            }
            _ => false,
        }
    }
    pub fn fill(&mut self) -> bool {
        match self {
            Self::Empty {
                pheromones,
                food,
                ant: None,
                ..
            } => {
                *self = Self::Filled {
                    pheromones: std::mem::take(pheromones),
                    food: std::mem::take(food),
                };
                true
            }
            _ => false,
        }
    }

    pub fn dig_up(&mut self) {
        match self {
            Self::Empty {
                surface_hole_above, ..
            }
            | Self::Home {
                surface_hole_above, ..
            } => *surface_hole_above = true,
            _ => (),
        }
    }
    pub fn dig_down(&mut self) {
        match self {
            Self::Empty {
                surface_hole_below, ..
            }
            | Self::Home {
                surface_hole_below, ..
            } => *surface_hole_below = true,
            _ => (),
        }
    }

    pub fn fill_up(&mut self) {
        match self {
            Self::Empty {
                surface_hole_above, ..
            }
            | Self::Home {
                surface_hole_above, ..
            } => *surface_hole_above = false,
            _ => (),
        }
    }
    pub fn fill_down(&mut self) {
        match self {
            Self::Empty {
                surface_hole_below, ..
            }
            | Self::Home {
                surface_hole_below, ..
            } => *surface_hole_below = false,
            _ => (),
        }
    }

    pub fn has_hole_up(&self) -> bool {
        match self {
            Self::Empty {
                surface_hole_above, ..
            }
            | Self::Home {
                surface_hole_above, ..
            } => *surface_hole_above,
            _ => false,
        }
    }
    pub fn has_hole_down(&self) -> bool {
        match self {
            Self::Empty {
                surface_hole_below, ..
            }
            | Self::Home {
                surface_hole_below, ..
            } => *surface_hole_below,
            _ => false,
        }
    }

    pub fn from_str(token: &str, position: Coordinates, teams: &[Team]) -> Option<Self> {
        if let Some(team) = teams.iter().find(|t| t.token.to_string() == token) {
            Some(Self::home(position, team))
        } else {
            Some(match token {
                "#" => Self::Rock,
                "." => Self::empty(false, false, 0),
                "x" => Self::filled(),
                "o" => Self::empty(false, true, 0),
                n => Self::empty(false, false, n.parse().ok()?),
            })
        }
    }
}
impl std::fmt::Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const TEAM_COLORS: [Color; 5] = [
            Color::BrightGreen,
            Color::Red,
            Color::Green,
            Color::Yellow,
            Color::Cyan,
        ];

        match self {
            Self::Empty { ant: Some(a), .. } | Self::Home { ant: Some(a), .. } => {
                let team = a.team;
                write!(f, "{}", "●".color(TEAM_COLORS[team as usize]))
            }
            Self::Home {
                team,
                food,
                surface_hole_above,
                surface_hole_below,
                ..
            } => {
                write!(
                    f,
                    "{}",
                    if *food != 0 {
                        food.min(&9).to_string()
                    } else if *surface_hole_below && *surface_hole_above {
                        String::from("↕")
                    } else if *surface_hole_below {
                        String::from("↓")
                    } else if *surface_hole_above {
                        String::from("↑")
                    } else {
                        String::from("⬡")
                    }
                    .color(TEAM_COLORS[*team as usize])
                    .bold()
                )
            }
            Self::Empty {
                food,
                surface_hole_below,
                surface_hole_above,
                ..
            } => {
                write!(
                    f,
                    "{}",
                    if *food != 0 {
                        food.min(&9).to_string().bright_yellow()
                    } else if *surface_hole_below && *surface_hole_above {
                        String::from("↕").bright_white()
                    } else if *surface_hole_below {
                        String::from("↓").bright_white()
                    } else if *surface_hole_above {
                        String::from("↑").bright_white()
                    } else {
                        String::from("⬡").bright_black()
                    }
                    .bold()
                )
            }
            Self::Filled { .. } => {
                write!(f, "{}", "⬢".bright_black().bold())
            }
            Self::Rock => write!(f, " "),
        }
    }
}
