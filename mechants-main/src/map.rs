use rand::Rng;

use crate::ant::{Ant, Status, Storage, TeamIndex};
use crate::game::Team;
use crate::instruction::{Condition, Instruction, RelativeDirection};
use crate::tile::Tile;
use std::io::BufRead;

/// The direction an entity is oriented towards.
#[derive(Eq, PartialEq, Debug, Default, Copy, Clone)]
pub enum Direction {
    #[default]
    East,
    NorthEast,
    SouthEast,
    SouthWest,
    West,
    NorthWest,
    Above,
    Below,
}
impl Direction {
    pub fn left(&self) -> Self {
        match self {
            Self::East => Self::NorthEast,
            Self::NorthEast => Self::NorthWest,
            Self::NorthWest => Self::West,
            Self::West => Self::SouthWest,
            Self::SouthWest => Self::SouthEast,
            Self::SouthEast => Self::East,
            _ => panic!("cannot call left on above or below"),
        }
    }

    pub fn right(&self) -> Self {
        match self {
            Self::East => Self::SouthEast,
            Self::SouthEast => Self::SouthWest,
            Self::SouthWest => Self::West,
            Self::West => Self::NorthWest,
            Self::NorthWest => Self::NorthEast,
            Self::NorthEast => Self::East,
            _ => panic!("cannot call right on above or below"),
        }
    }
}

/// A position stored in the axial system.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Default)]
pub struct Coordinates {
    pub column: usize,
    pub row: usize,
    pub level: usize,
}
impl Coordinates {
    pub fn neighbor(
        &self,
        width: usize,
        height: usize,
        depth: usize,
        direction: Direction,
    ) -> Option<Self> {
        let row_parity = self.row & 1;
        match direction {
            Direction::East if self.column < width - 1 => Some(Self {
                column: self.column + 1,
                ..*self
            }),
            Direction::West if self.column >= 1 => Some(Self {
                column: self.column - 1,
                ..*self
            }),
            Direction::NorthEast if self.column < width - 1 + row_parity && self.row >= 1 => {
                Some(Self {
                    column: self.column + 1 - row_parity,
                    row: self.row - 1,
                    ..*self
                })
            }
            Direction::NorthWest if self.column > row_parity && self.row >= 1 => Some(Self {
                column: self.column - 1 + row_parity,
                row: self.row - 1,
                ..*self
            }),
            Direction::SouthEast
                if self.column < width - 1 + row_parity && self.row < height - 1 =>
            {
                Some(Self {
                    column: self.column + 1 - row_parity,
                    row: self.row + 1,
                    ..*self
                })
            }
            Direction::SouthWest if self.column > row_parity && self.row < height - 1 => {
                Some(Self {
                    column: self.column - 1 + row_parity,
                    row: self.row + 1,
                    ..*self
                })
            }
            Direction::Above if self.level >= 1 => Some(Self {
                level: self.level - 1,
                ..*self
            }),
            Direction::Below if self.level < depth - 1 => Some(Self {
                level: self.level + 1,
                ..*self
            }),
            _ => None,
        }
    }
}

pub struct Map {
    pub width: usize,
    pub height: usize,
    pub depth: usize,

    pub ants_coordinates: Vec<Coordinates>,
    pub tiles: Vec<Tile>,
}
impl Map {
    pub fn from_source<B: BufRead>(source: B, teams: &[Team]) -> Self {
        let mut lines = source.lines().map_while(Result::ok);

        // Read header
        let Some(header) = lines.next() else { todo!() };
        let mut header_tokens = header.split_whitespace();
        let width: usize = header_tokens.next().unwrap().parse().unwrap();
        let height: usize = header_tokens.next().unwrap().parse().unwrap();
        let depth: usize = header_tokens.next().unwrap().parse().unwrap();

        // Then read each character, with each corresponding to a tile
        let mut tiles = Vec::new();
        let mut ants_coordinates = Vec::new();
        for (i, c) in lines
            .flat_map(|s| s.chars().collect::<Vec<_>>())
            .filter(|c| !c.is_whitespace())
            .enumerate()
        {
            let column = i % width;
            let row = (i / width) % height;
            let level = i / (width * height);
            let coordinates = Coordinates { column, row, level };

            if let Some(tile) = Tile::from_str(&c.to_string(), coordinates, teams) {
                if tile.is_home() {
                    ants_coordinates.push(coordinates)
                }
                tiles.push(tile);
            }
        }

        // We need to link holes from the surface to a corresponding hole in the
        // level below.
        for level in 0..depth {
            for row in 0..height {
                for column in 0..width {
                    if tiles[column + row * width + level * row * width].has_hole_down() {
                        if let Some(tile) =
                            tiles.get_mut(column + row * width + (level + 1) * width * height)
                        {
                            tile.dig();
                            tile.dig_up();
                        } else {
                            tiles[column + row * width + level * row * width].fill_down()
                        }
                    }
                }
            }
        }

        Self {
            width,
            height,
            depth,

            ants_coordinates,
            tiles,
        }
    }

    pub fn get_tile(&self, coordinates: &Coordinates) -> Option<&Tile> {
        self.tiles.get(
            coordinates.column
                + coordinates.row * self.width
                + coordinates.level * self.width * self.height,
        )
    }
    pub fn neighbor(&self, coordinates: Coordinates, direction: Direction) -> Option<&Tile> {
        coordinates
            .neighbor(self.width, self.height, self.depth, direction)
            .and_then(|c| self.get_tile(&c))
    }
    pub fn get_tile_mut(&mut self, coordinates: &Coordinates) -> Option<&mut Tile> {
        self.tiles.get_mut(
            coordinates.column
                + coordinates.row * self.width
                + coordinates.level * self.width * self.height,
        )
    }
    pub fn neighbor_mut(
        &mut self,
        coordinates: Coordinates,
        direction: Direction,
    ) -> Option<&mut Tile> {
        coordinates
            .neighbor(self.width, self.height, self.depth, direction)
            .and_then(|c| self.get_tile_mut(&c))
    }

    pub fn get_ant(&self, coordinates: &Coordinates) -> Option<&Ant> {
        self.get_tile(coordinates).and_then(|t| t.get_ant())
    }
    pub fn neighbor_ant(&self, coordinates: Coordinates, direction: Direction) -> Option<&Ant> {
        self.neighbor(coordinates, direction)
            .and_then(|t| t.get_ant())
    }
    pub fn get_ant_mut(&mut self, coordinates: &Coordinates) -> Option<&mut Ant> {
        self.get_tile_mut(coordinates).and_then(|t| t.get_ant_mut())
    }
    pub fn neighbor_ant_mut(
        &mut self,
        coordinates: Coordinates,
        direction: Direction,
    ) -> Option<&mut Ant> {
        self.neighbor_mut(coordinates, direction)
            .and_then(|t| t.get_ant_mut())
    }

    pub fn move_ant(
        &mut self,
        ant_index: usize,
        coordinates: Coordinates,
        direction: Direction,
    ) -> bool {
        if !self
            .neighbor(coordinates, direction)
            .map_or(false, |t| t.is_empty())
        {
            return false;
        }
        let mut ant = if let Some(ant) = self.get_tile_mut(&coordinates).and_then(|t| t.take_ant())
        {
            ant
        } else {
            return false;
        };

        ant.position = coordinates
            .neighbor(self.width, self.height, self.depth, direction)
            .unwrap();
        self.get_tile_mut(&ant.position).unwrap().place_ant(ant);
        self.ants_coordinates[ant_index] = ant.position;
        true
    }

    pub fn sense(
        &self,
        coordinates: Coordinates,
        direction: Direction,
        sense_direction: RelativeDirection,
        condition: Condition,
        team: TeamIndex,
    ) -> bool {
        let tile = if let Some(t) = match sense_direction {
            RelativeDirection::Here => self.get_tile(&coordinates),
            RelativeDirection::Ahead => self.neighbor(coordinates, direction),
            RelativeDirection::Left => self.neighbor(coordinates, direction.left()),
            RelativeDirection::Right => self.neighbor(coordinates, direction.right()),
            RelativeDirection::Above => self.neighbor(coordinates, Direction::Above),
            RelativeDirection::Below => self.neighbor(coordinates, Direction::Below),
        } {
            t
        } else {
            return matches!(condition, Condition::Rock);
        };

        match condition {
            Condition::Friend => tile.get_ant().map_or(false, |a| a.team == team),
            Condition::Enemy => tile.get_ant().map_or(false, |a| a.team != team),
            Condition::Grabbed => tile.get_ant().map_or(false, |a| a.stunned()),
            Condition::FriendWithFood => tile
                .get_ant()
                .map_or(false, |a| a.team == team && a.storage == Storage::Food),
            Condition::EnemyWithFood => tile
                .get_ant()
                .map_or(false, |a| a.team != team && a.storage == Storage::Food),
            Condition::Food => tile.has_food(),
            Condition::Empty => tile.is_empty(),
            Condition::Underground => coordinates.level != 0,
            Condition::Surface => coordinates.level == 0,
            Condition::HoleUp => tile.has_hole_up(),
            Condition::HoleDown => tile.has_hole_down(),
            Condition::Marker(i) => tile.marked_pheromone(team, i),
            Condition::EnemyMarkers => tile.any_enemy_pheromones(team),
            Condition::Home => tile.is_home_of_team(team),
            Condition::EnemyHome => tile.is_home_of_enemy(team),
            Condition::Rock => matches!(tile, Tile::Rock),
        }
    }

    pub fn update<R: Rng>(&mut self, teams: &[Team], rng: &mut R) {
        let mut ant_index = 0;
        while ant_index < self.ants_coordinates.len() {
            let ant_coordinates = self.ants_coordinates[ant_index];
            // Update the status of the ant and continue if it is not ready to act
            if !self
                .get_ant_mut(&ant_coordinates)
                .map_or(false, |a| a.update_and_check_readiness())
            {
                continue;
            }

            // Execute the current instruction if the ant is ready
            let ant = *self.get_ant(&ant_coordinates).unwrap();
            let instruction = teams[ant.team as usize].program[ant.program_counter];

            let jump = match instruction {
                Instruction::Sense {
                    direction,
                    condition,
                    on_success,
                    on_failure,
                } => {
                    if self.sense(ant.position, ant.direction, direction, condition, ant.team) {
                        Some(on_success)
                    } else {
                        Some(on_failure)
                    }
                }
                Instruction::MarkPheromone(i) => {
                    if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                        t.mark_pheromone(ant.team, i)
                    }
                    None
                }
                Instruction::UnmarkPheromone(i) => {
                    if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                        t.unmark_pheromone(ant.team, i)
                    }
                    None
                }
                Instruction::Pick(on_error) => {
                    if ant.storage != Storage::Empty
                        || !self
                            .get_tile_mut(&ant_coordinates)
                            .map_or(false, |t| t.take_food(1))
                    {
                        Some(on_error)
                    } else {
                        self.get_ant_mut(&ant_coordinates).unwrap().storage = Storage::Food;
                        None
                    }
                }
                Instruction::Drop => {
                    if ant.storage == Storage::Food {
                        self.get_ant_mut(&ant_coordinates).unwrap().storage = Storage::Empty;
                        if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                            t.place_food(1)
                        }
                    }
                    None
                }
                Instruction::TurnLeft => {
                    let direction = ant.direction.left();
                    self.get_ant_mut(&ant_coordinates).unwrap().direction = direction;
                    None
                }
                Instruction::TurnRight => {
                    let direction = ant.direction.right();
                    self.get_ant_mut(&ant_coordinates).unwrap().direction = direction;
                    None
                }
                Instruction::MoveForward(on_error) => {
                    if self.move_ant(ant_index, ant_coordinates, ant.direction) {
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::MoveUp(on_error) => {
                    if self
                        .get_tile(&ant_coordinates)
                        .map_or(false, |t| t.has_hole_up())
                        && self.move_ant(ant_index, ant_coordinates, Direction::Above)
                    {
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::MoveDown(on_error) => {
                    if self
                        .get_tile(&ant_coordinates)
                        .map_or(false, |t| t.has_hole_down())
                        && self.move_ant(ant_index, ant_coordinates, Direction::Below)
                    {
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::Dig(on_error) if ant.position.level == 0 => Some(on_error),
                Instruction::Dig(on_error) => {
                    if let Some(ahead) = self.neighbor_mut(ant_coordinates, ant.direction) {
                        if ahead.dig() {
                            None
                        } else {
                            Some(on_error)
                        }
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::Fill(on_error) if ant.position.level == 0 => Some(on_error),
                Instruction::Fill(on_error) => {
                    let fill_success = self
                        .neighbor_mut(ant_coordinates, ant.direction)
                        .map_or(false, |t| t.fill());
                    if fill_success {
                        if let Some(t) = self.neighbor_mut(ant_coordinates, Direction::Above) {
                            t.fill_down()
                        }
                        if let Some(t) = self.neighbor_mut(ant_coordinates, Direction::Below) {
                            t.fill_up()
                        }
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::DigUp(on_error) => {
                    if let Some(above) = self.neighbor_mut(ant_coordinates, Direction::Above) {
                        above.dig();
                        above.dig_down();
                        if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                            t.dig_up()
                        }
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::DigDown(on_error) => {
                    if let Some(under) = self.neighbor_mut(ant_coordinates, Direction::Below) {
                        under.dig();
                        under.dig_up();
                        if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                            t.dig_down()
                        }
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::FillUp => {
                    if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                        t.fill_up()
                    }
                    if let Some(t) = self.neighbor_mut(ant_coordinates, Direction::Above) {
                        t.fill_down()
                    }
                    None
                }
                Instruction::FillDown => {
                    if let Some(t) = self.get_tile_mut(&ant_coordinates) {
                        t.fill_down()
                    }
                    if let Some(t) = self.neighbor_mut(ant_coordinates, Direction::Below) {
                        t.fill_up()
                    }
                    None
                }
                Instruction::Grab(on_error) => {
                    let grab_success = self
                        .neighbor_ant_mut(ant_coordinates, ant.direction)
                        .map_or(false, |a| a.stun());
                    if grab_success {
                        self.get_ant_mut(&ant_coordinates)
                            .unwrap()
                            .stunning(instruction.cooldown());
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::Attack(on_error) => {
                    let killed = if let Some(attacked) =
                        self.neighbor_ant_mut(ant_coordinates, ant.direction)
                    {
                        attacked.stunned()
                    } else {
                        false
                    };

                    if killed {
                        let tile = self.neighbor_mut(ant_coordinates, ant.direction).unwrap();
                        tile.take_ant();
                        tile.place_food(3);
                        None
                    } else {
                        Some(on_error)
                    }
                }
                Instruction::Random {
                    faces,
                    on_zero,
                    on_other,
                } => Some(if rng.gen_bool(1.0 / faces as f64) {
                    on_zero
                } else {
                    on_other
                }),
                Instruction::Goto(label) => Some(label),
            };

            let ant_coordinates = self.ants_coordinates[ant_index];
            let ant = self.get_ant_mut(&ant_coordinates).unwrap();
            ant.program_counter = jump.unwrap_or(ant.program_counter + 1);
            if ant.status == Status::Ready {
                ant.status = Status::Cooldown(instruction.cooldown());
            }

            ant_index += 1;
        }
    }
}
impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let separator = "-".repeat(self.width * 2);
        for level in 0..self.depth {
            if level != 0 {
                writeln!(f, "{separator}")?;
            }
            for row in 0..self.height {
                if row % 2 != 0 {
                    write!(f, " ")?;
                }
                for column in 0..self.width {
                    write!(
                        f,
                        "{} ",
                        self.get_tile(&Coordinates { column, row, level })
                            .map(|t| t.to_string())
                            .unwrap_or(String::from(" "))
                    )?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
