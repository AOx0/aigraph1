use super::*;

/// Best first implementation.
///
/// This is a key function that is later used to implement variants, this is possible due to the
/// pattern all variants repeat, where only the heuristics function changes but te procedure is the same.
///
/// The following searching methods are implemented using this function:
///     - Dijkstra
///     - A*
///     - A* Weighted
///     - Greedy best first
///
/// The function takes a function `h` of type `F` for computing the heuristic value for each node.
/// This function `F` has access to the node index on the graph, the edge index of the connection
/// to its parent, the current state of the search instance and the index of its parent.
/// With all these values, any heuristic can be calculated, I presume.
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

pub mod dijkstra {
    use super::*;

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, G>(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        edge_cost: G,
        direction: Direction,
    ) -> BestFirst<
        'a,
        I,
        N,
        E,
        Ty,
        Ix,
        K,
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K + 'a,
    >
    where
        K: Measure + Copy + Eq + Default + Ord + PartialOrd,
        G: Fn(&E) -> K + 'a,
    {
        BestFirst::new(
            graph,
            journey,
            move |_: NodeIndex<Ix>, edge: EdgeIndex<Ix>, past: K, _: NodeIndex<Ix>| {
                past + edge_cost(&graph.inner[edge])
            },
            direction,
        )
    }
}

pub mod a_star {
    use super::*;

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, G, F>(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        h: G,
        w: F,
        direction: Direction,
    ) -> BestFirst<
        'a,
        I,
        N,
        E,
        Ty,
        Ix,
        K,
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K + 'a,
    >
    where
        G: Fn(&NodeIndex<Ix>) -> K + 'a,
        F: Fn(&E) -> K + 'a,
        K: Measure + Copy + Default + PartialOrd + Mul<Output = K> + 'a,
    {
        BestFirst::new(
            graph,
            (journey.0, journey.1),
            move |node, edge, _, _| h(&node) + w(&graph.inner[edge]),
            direction,
        )
    }
}

pub mod greedy {
    use super::*;

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, F>(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        h: F,
        direction: Direction,
    ) -> BestFirst<
        'a,
        I,
        N,
        E,
        Ty,
        Ix,
        K,
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K + 'a,
    >
    where
        K: Measure + Copy + Default + PartialOrd + 'a,
        F: Fn(&NodeIndex<Ix>) -> K + 'a,
    {
        BestFirst::new(
            graph,
            (journey.0, journey.1),
            move |node, _, _, _| h(&node),
            direction,
        )
    }
}

pub mod weighted_a_star {
    use super::*;

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, K, G, F>(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        h: G,
        w: F,
        k: K,
        direction: Direction,
    ) -> BestFirst<
        'a,
        I,
        N,
        E,
        Ty,
        Ix,
        K,
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, K, NodeIndex<Ix>) -> K + 'a,
    >
    where
        G: Fn(&NodeIndex<Ix>) -> K + 'a,
        F: Fn(&E) -> K + 'a,
        K: Measure + Copy + Default + PartialOrd + Mul<Output = K> + 'a,
    {
        BestFirst::new(
            graph,
            (journey.0, journey.1),
            move |node, edge, _, _| h(&node) + k * w(&graph.inner[edge]),
            direction,
        )
    }
}
