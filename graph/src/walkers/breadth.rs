use super::*;

#[derive(Clone)]
pub struct BreadthFirst<'a, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<f32, Ix>>,
    visited: FixedBitSet,
    pub direction: Direction,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> BreadthFirst<'a, I, N, E, Ty, Ix> {
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        direction: Direction,
    ) -> Self {
        Self {
            goal: journey.1,
            graph,
            border: {
                let mut border = VecDeque::with_capacity(graph.node_count());
                border.push_front(Step {
                    caller: None,
                    idx: journey.0,
                    rel: None,
                    state: 0.,
                });
                border
            },
            visited: graph.visit_map(),
            direction,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<Ix> for BreadthFirst<'a, I, N, E, Ty, Ix> {
    fn step(&mut self) -> WalkerState<Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            self.graph
                .inner
                .neighbors_directed(parent.idx, self.direction)
                .for_each(|child_idx| {
                    (!self.visited.is_visited(&child_idx)).then(|| {
                        self.visited.visit(child_idx);
                        self.border.push_back(Step {
                            caller: Some(parent.clone()),
                            idx: child_idx,
                            rel: Some(self.graph.edge_between(parent.idx, child_idx)),
                            state: 0.,
                        });
                    });
                });
            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}
