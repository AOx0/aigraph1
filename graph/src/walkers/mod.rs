pub use beam::*;
pub use best::*;
pub use bidirectional::*;
pub use breadth::*;
pub use depth::*;
pub use hill::*;

pub use super::*;

pub mod beam;
pub mod breadth;
pub mod depth;

pub mod best;
pub mod bidirectional;
pub mod hill;
mod sim_annealing;

#[derive(Debug)]
pub enum WalkerState<Ix = DefaultIx> {
    Done,
    Found(Step<f32, Ix>),
    NotFound(Rc<Step<f32, Ix>>),
    Cutoff,
}

impl<Ix> WalkerState<Ix> {
    /// Peek into the step value if available
    pub fn step_peek(&self) -> Option<&Step<f32, Ix>> {
        match self {
            WalkerState::Done | WalkerState::Cutoff => None,
            WalkerState::Found(ref state) => Some(state),
            WalkerState::NotFound(state) => Some(state.as_ref()),
        }
    }
}

pub trait Walker<Ix = DefaultIx> {
    fn step(&mut self) -> WalkerState<Ix>;
}
