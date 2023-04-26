use super::*;

/// A single step in the graph
///
/// Steps are the way we compute and represent the graph traversal with the
/// various algorithms.
///
/// Steps can store a type `S` which can be used to hold any information like
/// total weight or any other primitive or structure.
///
/// The `Step` type is a singly linked list, where each step has a reference to its parent.
/// It is a singly linked list because we only need to go backwards to the root node to get the path.
#[derive(Debug, Default)]
pub struct Step<S = f32, Ix = DefaultIx> {
    /// The parent State that invoked this instance.
    /// If the option is None then it means we arrived to the root state.
    pub caller: Option<Rc<Step<S, Ix>>>,
    /// The current node index the step is at within the graph.
    pub idx: NodeIndex<Ix>,
    /// The index of the edge that binds caller -> self
    pub rel: Option<EdgeIndex<Ix>>,
    /// State of the walking progress
    pub state: S,
}

impl<S, Ix: IndexType> Step<S, Ix> {
    /// Returns the nodes that were traversed to reach this step.
    ///
    /// It starts from the target node and goes backwards to the root node.
    /// To get the nodes in the correct order, you need to reverse the result with `rev()`.
    ///
    /// This is an O(n) operation.
    pub fn collect_nodes(&self) -> Vec<NodeIndex<Ix>> {
        self.iter().map(|step| step.idx).collect()
    }

    /// Returns the edges that were traversed to reach this step.
    ///
    /// It starts from the target node and goes backwards to the root node.
    /// To get the edges in the correct order, you need to reverse the result with `rev()`.
    ///
    /// This is an O(n) operation.
    pub fn collect_edges(&self) -> Vec<EdgeIndex<Ix>> {
        self.iter().filter_map(|step| step.rel).collect()
    }

    /// Returns the size of the chain.
    ///
    /// This is an O(n) operation.
    pub fn chain_size(&self) -> usize {
        let mut size = 0;
        self.iter().for_each(|_| size += 1);
        size
    }

    pub fn from_slice<I, N, E, Ty: EdgeType, F>(
        indices: &[NodeIndex<Ix>],
        graph: &Graph<I, N, E, Ty, Ix>,
        state: F,
    ) -> Step<S, Ix>
    where
        F: Fn(NodeIndex<Ix>, EdgeIndex<Ix>, NodeIndex<Ix>, &S) -> S,
        S: Default,
    {
        let mut step: Step<S, Ix> = Step {
            caller: None,
            idx: indices[0],
            rel: None,
            state: Default::default(),
        };
        for idx in indices.into_iter().skip(1).copied() {
            let old_step = Rc::new(step);
            step = Step {
                caller: Some(old_step.clone()),
                idx,
                rel: graph.edge_between(old_step.idx, idx),
                state: state(
                    old_step.idx,
                    graph.edge_between_unchecked(old_step.idx, idx),
                    idx,
                    &old_step.state,
                ),
            };
        }
        step
    }

    /// Returns an iterator visiting all the steps in the chain
    pub fn iter(&self) -> Iter<S, Ix> {
        Iter::new(self)
    }
}

impl<S: Clone, Ix: Clone> Clone for Step<S, Ix> {
    /// Clone the step chain.
    ///
    /// The new chain is a deep copy of the original chain.
    fn clone(&self) -> Self {
        Self {
            caller: self.caller.as_ref().map(|step| Rc::new(Self::clone(step))),
            idx: self.idx.clone(),
            rel: self.rel.clone(),
            state: self.state.clone(),
        }
    }
}

/// An iterator over the borrowed steps of a step chain
#[derive(Debug)]
pub struct Iter<'a, S, Ix> {
    current: Option<&'a Step<S, Ix>>,
}

impl<'a, S, Ix> Iter<'a, S, Ix> {
    pub fn new(step: &'a Step<S, Ix>) -> Self {
        Self {
            current: Some(step),
        }
    }
}

impl<'a, S, Ix> Iterator for Iter<'a, S, Ix> {
    type Item = &'a Step<S, Ix>;
    fn next(&mut self) -> Option<Self::Item> {
        let head = self.current;
        self.current = head.and_then(|head| head.caller.as_ref().map(|step| step.as_ref()));
        head
    }
}
