use super::*;

#[derive(Clone)]
pub struct DepthFirst<'a, D, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<D, Ix>>,
    limit: Option<D>,
    cutoff: bool,
    level: D,
    neighbors: Vec<NodeIndex<Ix>>,
    pub direction: Direction,
}

impl<'a, D: Zero, I, N, E, Ty: EdgeType, Ix: IndexType> DepthFirst<'a, D, I, N, E, Ty, Ix> {
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        limit: Option<D>,
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
                    state: Zero::zero(),
                });
                border
            },
            neighbors: Vec::with_capacity(graph.node_count()),
            cutoff: false,
            level: Zero::zero(),
            direction,
        }
    }
}

impl<'a, D, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<D, Ix>
    for DepthFirst<'a, D, I, N, E, Ty, Ix>
where
    D: Measure + Copy + One + Zero,
{
    fn step(&mut self) -> WalkerState<D, Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self
                .limit
                .and_then(|limit| Some(parent.state == limit))
                .unwrap_or(false)
            {
                if self
                    .graph
                    .inner
                    .neighbors_directed(parent.idx.into(), self.direction)
                    .count()
                    != 0
                {
                    self.cutoff = true;
                }
                return WalkerState::NotFound(Rc::new(parent));
            }
            if self
                .goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return WalkerState::Found(parent.clone());
            }

            let parent = Rc::new(parent);
            self.level = parent.state + One::one();

            self.neighbors.clear();
            self.neighbors.extend(
                self.graph
                    .inner
                    .neighbors_directed(parent.idx.into(), self.direction),
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
