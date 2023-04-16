use super::*;

pub struct Beam<'a, I, N, E, Ty, Ix, F> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<(), Ix>>,
    successors: usize,
    neighbors: Vec<NodeIndex<Ix>>,
    visited: FixedBitSet,
    compare: F,
    pub direction: Direction,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Beam<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>, &NodeIndex<Ix>) -> Ordering,
{
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        successors: usize,
        compare: F,
        direction: Direction,
    ) -> Self {
        Self {
            graph,
            goal: journey.1,
            successors,
            visited: graph.visit_map(),
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

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Walker<(), Ix> for Beam<'a, I, N, E, Ty, Ix, F>
where
    F: FnMut(&NodeIndex<Ix>, &NodeIndex<Ix>) -> Ordering,
{
    fn step(&mut self) -> WalkerState<(), Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);

            // We don't want to keep visiting the same node over and over again, specially if
            // we have an animation delay.
            if !self.visited.is_visited(&parent.idx) {
                self.visited.visit(parent.idx);
            } else {
                return WalkerState::NotFound(parent);
            }

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

                self.neighbors
                    .iter()
                    .copied()
                    .enumerate()
                    .take_while(|(i, _)| i < &self.successors)
                    .for_each(|(_, child_idx)| {
                        self.border.push_back(Step {
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
