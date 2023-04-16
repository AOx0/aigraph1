use super::*;

pub struct BestFirst<'a, I, N, E, Ty, Ix, K, F> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<K, Ix>>,
    pub direction: Direction,
    pub h: F,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, F> BestFirst<'a, I, N, E, Ty, Ix, K, F>
where
    K: Default + Measure + Copy + PartialEq + PartialOrd,
    F: Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K,
{
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        h: F,
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
                    state: K::default(),
                });
                border
            },
            direction,
            h,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, F> Walker<K, Ix>
    for BestFirst<'a, I, N, E, Ty, Ix, K, F>
where
    K: Default + Measure + Copy + PartialEq + PartialOrd,
    F: Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K,
{
    fn step(&mut self) -> WalkerState<K, Ix> {
        if let Some(parent) = {
            let i = self
                .border
                .iter()
                .enumerate()
                .min_by(|(_, s1), (_, s2)| s1.state.partial_cmp(&s2.state).unwrap())
                .map(|(x, _)| x);
            i.map(|i| self.border.remove(i).unwrap())
        } {
            if self.goal.map(|goal| goal == parent.idx).unwrap_or(false) {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            for child_idx in self
                .graph
                .inner
                .neighbors_directed(parent.idx.into(), Direction::Outgoing)
            {
                let es_goal = self.goal.map(|goal| goal == child_idx).unwrap_or(false);
                let has_kids = self
                    .graph
                    .inner
                    .neighbors_directed(child_idx, Direction::Outgoing)
                    .count()
                    != 0;
                if es_goal || has_kids {
                    let edge = self.graph.inner.find_edge(parent.idx, child_idx).unwrap();
                    let step = Step {
                        caller: Some(parent.clone()),
                        idx: child_idx.into(),
                        rel: Some(edge.into()),
                        state: (self.h)(child_idx, edge, parent.state, parent.idx),
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
