use super::*;

pub struct Hill<'a, I, N, E, Ty, Ix, F> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    direction: Direction,
    neighbors: Vec<NodeIndex<Ix>>,
    border: VecDeque<Step<(), Ix>>,
    compare: F,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Hill<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>, &NodeIndex<Ix>) -> Ordering,
{
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        compare: F,
        direction: Direction,
    ) -> Self {
        Self {
            graph,
            goal: journey.1,
            neighbors: Vec::with_capacity(graph.edge_count()),
            border: {
                let mut border = VecDeque::with_capacity(graph.node_count());
                border.push_front(Step {
                    caller: None,
                    idx: journey.0,
                    rel: None,
                    state: (),
                });
                border
            },
            compare,
            direction,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Walker<(), Ix> for Hill<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>, &NodeIndex<Ix>) -> Ordering,
{
    fn step(&mut self) -> WalkerState<(), Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            if self
                .graph
                .inner
                .neighbors_directed(parent.idx, self.direction)
                .count()
                != 0
            {
                self.neighbors.clear();
                self.neighbors.extend(
                    self.graph
                        .inner
                        .neighbors_directed(parent.idx, self.direction),
                );

                self.neighbors.sort_by(&mut self.compare);

                self.neighbors.iter().copied().rev().for_each(|child_idx| {
                    self.border.push_front(Step {
                        caller: Some(parent.clone()),
                        idx: child_idx,
                        rel: Some(self.graph.edge_between(parent.idx, child_idx)),
                        state: (),
                    });
                });
            }

            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}

pub struct StochasticHill<'a, I, N, E, Ty, Ix, F> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    direction: Direction,
    neighbors: Vec<NodeIndex<Ix>>,
    border: VecDeque<Step<(), Ix>>,
    compare: F,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> StochasticHill<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>) -> f64,
{
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        compare: F,
        direction: Direction,
    ) -> Self {
        Self {
            graph,
            goal: journey.1,
            neighbors: Vec::with_capacity(graph.edge_count()),
            border: {
                let mut border = VecDeque::with_capacity(graph.node_count());
                border.push_front(Step {
                    caller: None,
                    idx: journey.0,
                    rel: None,
                    state: (),
                });
                border
            },
            compare,
            direction,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Walker<(), Ix>
    for StochasticHill<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>) -> f64,
{
    fn step(&mut self) -> WalkerState<(), Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            if self
                .graph
                .inner
                .neighbors_directed(parent.idx, self.direction)
                .count()
                != 0
            {
                self.neighbors.clear();
                self.neighbors.extend(
                    self.graph
                        .inner
                        .neighbors_directed(parent.idx, self.direction),
                );

                self.neighbors.sort_by(|a, b| {
                    (self.compare)(a)
                        .partial_cmp(&(self.compare)(b))
                        .unwrap_or(Ordering::Equal)
                });

                self.neighbors.iter().copied().rev().for_each(|child_idx| {
                    self.border.push_front(Step {
                        caller: Some(parent.clone()),
                        idx: child_idx,
                        rel: Some(self.graph.edge_between(parent.idx, child_idx)),
                        state: (),
                    });
                });
            }

            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}
