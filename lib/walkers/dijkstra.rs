use super::*;

#[derive(Clone)]
pub struct Dijkstra<'a, F, K, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<K, Ix>>,
    pub direction: Direction,
    edge_cost: F,
}


impl<'a, F: FnMut(&E) -> K, K: Zero, I, N, E, Ty: EdgeType, Ix: IndexType>
    Dijkstra<'a, F, K, I, N, E, Ty, Ix>
{
    #[allow(dead_code)]
    pub fn new(
            graph: &'a Graph<I, N, E, Ty, Ix>,
            start: NodeIndex<Ix>,
            goal: Option<NodeIndex<Ix>>,
            direction: Direction,
            edge_cost: F,
            ) -> Self {
        Self {
            goal,
            graph,
            border: {
                let mut border = VecDeque::with_capacity(graph.node_count());
                border.push_front(Step {
                    caller: None,
                    idx: start,
                    rel: None,
                    state: Zero::zero(),
                });
                border
            },
            direction,
            edge_cost,
        }
    }
}

impl<'a, F, K, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<K, Ix>
    for Dijkstra<'a, F, K, I, N, E, Ty, Ix>
where
    K: Measure + Copy + Ord,
    F: FnMut(&E) -> K,
    EdgeIndex: From<EdgeIndex<Ix>>,
{
    fn step(&mut self) -> WalkerState<K, Ix> {
        if let Some(parent) = {
            let i = self
                .border
                .iter()
                .enumerate()
                .min_by(|(_, s1), (_, s2)| s1.state.cmp(&s2.state))
                .map(|(x, _)| x);
            i.map(|i| self.border.remove(i).unwrap())
        } {
            let parent = Rc::new(parent);
            for child_idx in self
                .graph
                .inner
                .neighbors_directed(parent.idx.into(), self.direction)
            {
                let es_goal = self
                    .goal
                    .and_then(|goal| Some(goal == child_idx))
                    .unwrap_or(false);
                let tiene_hijos = self
                    .graph
                    .inner
                    .neighbors_directed(child_idx, self.direction)
                    .count()
                                  != 0;
                if es_goal || tiene_hijos {
                    let edge = self
                        .graph
                        .inner
                        .find_edge_undirected(parent.idx, child_idx)
                        .unwrap();
                    let step = Step {
                        caller: Some(parent.clone()),
                        idx: child_idx.into(),
                        rel: Some(edge.0.into()),
                        state: parent.state + (self.edge_cost)(&self.graph.inner[edge.0]),
                    };

                    if es_goal {
                        return WalkerState::Found(step);
                    }
                    self.border.push_front(step)
                }
            }
            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}
