use super::*;

struct SimAnnealing<'a, I, N, E, Ty, Ix> {
    graph: &'a Graph<I, N, E, Ty, Ix>,
    goal: Option<NodeIndex<Ix>>,
    temperature: f64,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> SimAnnealing<'a, I, N, E, Ty, Ix> {
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        temperature: f64,
    ) -> Self {
        Self {
            graph,
            goal: journey.1,
            temperature,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<(), Ix>
    for SimAnnealing<'a, I, N, E, Ty, Ix>
{
    fn step(&mut self) -> WalkerState<(), Ix> {
        WalkerState::Done
    }
}
