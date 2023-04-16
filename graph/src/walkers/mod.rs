pub mod beam;
pub mod breadth;
pub mod depth;
pub mod dijkstra;
pub use super::*;
pub mod best;

pub use beam::Beam;
pub use best::*;
pub use best::*;
pub use breadth::BreadthFirst;
pub use depth::DepthFirst;
pub use dijkstra::Dijkstra;

#[derive(Debug)]
pub enum WalkerState<S, Ix = DefaultIx> {
    Done,
    Found(Step<S, Ix>),
    NotFound(Rc<Step<S, Ix>>),
    Cutoff,
}

impl<S, Ix> WalkerState<S, Ix> {
    /// Peek into the step value if available
    pub fn step_peek<'a>(&'a self) -> Option<&'a Step<S, Ix>> {
        match self {
            WalkerState::Done | WalkerState::Cutoff => None,
            WalkerState::Found(ref state) => Some(state),
            WalkerState::NotFound(state) => Some(state.as_ref()),
        }
    }
}

pub trait Walker<S, Ix = DefaultIx> {
    fn step(&mut self) -> WalkerState<S, Ix>;
}
