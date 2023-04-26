pub use beam::*;
pub use best::*;
pub use bidirectional::*;
pub use breadth::*;
pub use depth::*;
pub use hill::*;
pub use sim_annealing::*;

use rand::seq::SliceRandom;

pub use super::*;

/// The Beam strategy implementation
pub mod beam;

/// The BFS stategy implementation
pub mod breadth;

/// The DFS strategy implementation. The route is random
pub mod depth;

/// The best first strategy implementation. Various strategies derive from the best
/// first strategy For example Dijkstra, A*, etc.
pub mod best;

/// The hill ans stochastic hill implementation
pub mod hill;

/// Bidirectional search using any of the methods above
pub mod bidirectional;

/// A simulation of the traveler problem
pub mod sim_annealing;

/// State interface for Walker machines.
///
/// Each machine performs a *step* that must return an insight of its current
/// state.
///
/// The way state machines inform about their state is with this enum.
/// There are four possible scenarios:
/// - `WalkerState::Found`: The machine has found a solution to the problem
/// - `WalkerState::Done`: There are no more nodes to explore
/// - `WalkerState::NotFound`: This step neither finished nor found the goal but it found an edge anyway
/// - `WalkerState::Cutoff`: The step did not found anything but has not finished yet.
#[derive(Debug)]
pub enum WalkerState<Ix = DefaultIx> {
    /// There are no more nodes to explore
    Done,
    /// A solution was found. Returns the step chain of the solution
    Found(Step<f32, Ix>),
    /// Found a node, it is not the solution. A step chain up to this point is returned.
    NotFound(Rc<Step<f32, Ix>>),
    /// Did not found anything. The search is not over, tough.
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

/// The Walker trait. All solution-searching strategies must implement this trait.
///
/// The *walkers* are state machines that implement this trait that provides a `step` method
/// to move the walker to the next node in the graph according to the actual implementation of the trait
/// following each strategy algorithm.
///
/// The trait dictates that all of the types that want to traverse a graph must return
/// the result of performing a single step.
pub trait Walker<Ix = DefaultIx> {
    fn step(&mut self) -> WalkerState<Ix>;
}
