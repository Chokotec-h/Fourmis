use crate::instruction::Address;
use crate::map::{Coordinates, Direction};

pub type TeamIndex = u8;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Ant {
    pub program_counter: Address,
    pub team: TeamIndex,
    pub position: Coordinates,
    pub direction: Direction,

    pub status: Status,
    pub storage: Storage,
}
impl Ant {
    pub fn new(position: Coordinates, team: TeamIndex) -> Self {
        Self {
            program_counter: 0,
            team,
            position,
            direction: Direction::East,
            status: Status::Ready,
            storage: Storage::Empty,
        }
    }

    pub fn update_and_check_readiness(&mut self) -> bool {
        match &mut self.status {
            Status::Stunning(turns, cooldown_after) => {
                if let Some(t) = turns.checked_sub(1) {
                    *turns = t
                } else {
                    self.status = Status::Cooldown(*cooldown_after)
                }
                false
            }
            Status::Stunned(turns) | Status::Cooldown(turns) => {
                if let Some(t) = turns.checked_sub(1) {
                    *turns = t
                } else {
                    self.status = Status::Ready
                }
                false
            }
            Status::Ready => true,
        }
    }

    pub fn stunned(&self) -> bool {
        matches!(self.status, Status::Stunned(_) | Status::Stunning(_, _))
    }
    pub fn stun(&mut self) -> bool {
        if self.stunned() {
            false
        } else {
            self.status = Status::Stunned(40);
            true
        }
    }
    pub fn stunning(&mut self, cooldown_after: u8) {
        self.status = Status::Stunning(20, cooldown_after);
    }
}

/// The current status of an ant.
#[derive(Eq, PartialEq, Debug, Default, Copy, Clone)]
pub enum Status {
    #[default]
    Ready,
    Stunned(u8),
    Stunning(u8, u8),
    Cooldown(u8),
}

/// Current item carried by an ant.
///
/// # Implementation notes
/// Note that this is equivalent to a boolean for now, but kept as an enum in
/// case future version want to add features.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Default, Copy, Clone)]
pub enum Storage {
    #[default]
    Empty,
    Food,
}
