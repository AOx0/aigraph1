pub mod breadth;
pub mod depth;
pub mod dijkstra;
pub use super::*;

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

pub trait Walker<S, Ix = DefaultIx> {
    fn step(&mut self) -> WalkerState<S, Ix>;
}
