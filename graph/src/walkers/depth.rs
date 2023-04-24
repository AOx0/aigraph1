use super::*;

#[derive(Clone)]
pub struct DepthFirst<'a, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<f32, Ix>>,
    visited: FixedBitSet,
    limit: Option<f32>,
    cutoff: bool,
    level: f32,
    neighbors: Vec<NodeIndex<Ix>>,
    pub direction: Direction,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> DepthFirst<'a, I, N, E, Ty, Ix> {
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        limit: Option<f32>,
        direction: Direction,
    ) -> Self {
        Self {
            graph,
            goal: journey.1,
            limit,
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
            neighbors: Vec::with_capacity(graph.node_count()),
            cutoff: false,
            visited: graph.visit_map(),
            level: 0.,
            direction,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<Ix> for DepthFirst<'a, I, N, E, Ty, Ix> {
    fn step(&mut self) -> WalkerState<Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self
                .limit
                .map(|limit| parent.state == limit)
                .unwrap_or(false)
            {
                if self
                    .graph
                    .inner
                    .neighbors_directed(parent.idx, self.direction)
                    .count()
                    != 0
                {
                    self.cutoff = true;
                }
                return WalkerState::NotFound(Rc::new(parent));
            }
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            self.level = parent.state + 1.;

            if self.visited.is_visited(&parent.idx) {
                return WalkerState::NotFound(parent);
            } else {
                self.visited.visit(parent.idx);
            }

            self.neighbors.clear();
            self.neighbors.extend(
                self.graph
                    .inner
                    .neighbors_directed(parent.idx, self.direction),
            );
            rand::shuffle(&mut self.neighbors);

            for child in self.neighbors.iter().copied() {
                self.border.push_front(Step {
                    caller: Some(parent.clone()),
                    idx: child,
                    rel: Some(self.graph.edge_between(parent.idx, child)),
                    state: self.level,
                })
            }
            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}
