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
pub struct BestFirst<'a, I, N, E, Ty, Ix, F> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<f32, Ix>>,
    pub direction: Direction,
    pub h: F,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> BestFirst<'a, I, N, E, Ty, Ix, F>
where
    F: Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32,
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
                    state: 0.,
                });
                border
            },
            direction,
            h,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F> Walker<Ix> for BestFirst<'a, I, N, E, Ty, Ix, F>
where
    F: Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32,
{
    fn step(&mut self) -> WalkerState<Ix> {
        if let Some(parent) = {
            // Pop the best node from the border
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
                .neighbors_directed(parent.idx, self.direction)
            {
                let es_goal = self.goal.map(|goal| goal == child_idx).unwrap_or(false);
                let has_kids = self
                    .graph
                    .inner
                    .neighbors_directed(child_idx, self.direction)
                    .count()
                    != 0;
                if es_goal || has_kids {
                    let edge = self.graph.edge_between_unchecked(parent.idx, child_idx);
                    let step = Step {
                        caller: Some(parent.clone()),
                        idx: child_idx,
                        rel: Some(edge),
                        state: (self.h)(child_idx, edge, parent.state, parent.idx),
                    };

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

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, G>(
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
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32 + 'a,
    >
    where
        G: Fn(&E) -> f32 + 'a,
    {
        BestFirst::new(
            graph,
            journey,
            move |_: NodeIndex<Ix>, edge: EdgeIndex<Ix>, past: f32, _: NodeIndex<Ix>| {
                past + edge_cost(&graph.inner[edge])
            },
            direction,
        )
    }
}

pub mod a_star {
    use super::*;

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, G, F>(
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
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32 + 'a,
    >
    where
        G: Fn(&NodeIndex<Ix>) -> f32 + 'a,
        F: Fn(&E) -> f32 + 'a,
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

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, F>(
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
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32 + 'a,
    >
    where
        F: Fn(&NodeIndex<Ix>) -> f32 + 'a,
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

    pub fn new<'a, I, N, E, Ty: EdgeType, Ix: IndexType, G, F>(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        h: G,
        w: F,
        k: f32,
        direction: Direction,
    ) -> BestFirst<
        'a,
        I,
        N,
        E,
        Ty,
        Ix,
        impl Fn(NodeIndex<Ix>, EdgeIndex<Ix>, f32, NodeIndex<Ix>) -> f32 + 'a,
    >
    where
        G: Fn(&NodeIndex<Ix>) -> f32 + 'a,
        F: Fn(&E) -> f32 + 'a,
    {
        BestFirst::new(
            graph,
            (journey.0, journey.1),
            move |node, edge, _, _| h(&node) + k * w(&graph.inner[edge]),
            direction,
        )
    }
}
