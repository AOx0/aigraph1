#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + count!($($xs)*));
}

#[macro_export]
macro_rules! graph {
    (
        with_node: $node:expr,
        with_edges: $type:tt,
        nodes: [$($ident:expr),*],
        connections: [ $($from:expr => {$(($cost:expr) $to:expr),+}),* ]
    ) => {
        graph!{
            nodes: [$($ident => $node),*],
            connections: [ $($from => {$(($type, $cost) {$to}),+}),* ]
        }
    };

    (
        nodes: [$($ident:expr => $node:expr),*],
        connections: [ $($from:expr => {$(($type:tt, $cost:expr) {$($to:expr),+}),+}),* ]
    ) => {{

        let mut g = Graph::with_capacity(count!($($node)*), count!($($({$from;$($to)+})+)*) );
        $(g.register($ident, $node).unwrap());*;
        $(
            $($(g.$type($from, $to,$cost).unwrap());+);+
        );*;
        g
    }};
}

use fixedbitset::FixedBitSet;
use num::{One, Zero};
pub use petgraph;
pub use petgraph::Direction;
use petgraph::{
    algo::Measure,
    graph::{EdgeIndex, NodeIndex},
    stable_graph::{DefaultIx, IndexType},
    visit::{VisitMap, Visitable},
    Directed, EdgeType, Graph as PGraph,
};

use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
    rc::Rc,
};

#[derive(Clone, Debug)]
pub struct Step<S, Ix> {
    pub caller: Option<Rc<Step<S, Ix>>>,
    pub idx: NodeIndex<Ix>,
    pub rel: Option<EdgeIndex>,
    pub state: S,
}

#[derive(Debug)]
pub struct StepUnit<Ix> {
    pub caller: Option<Rc<StepUnit<Ix>>>,
    pub idx: NodeIndex<Ix>,
    pub rel: Option<EdgeIndex>,
}

impl<Ix: IndexType> StepUnit<Ix> {
    pub fn from_step<S>(step: Rc<Step<S, Ix>>) -> Rc<Self> {
        Rc::new(Self {
            caller: step
                .caller
                .as_ref()
                .and_then(|step| Some(Self::from_step(step.clone()))),
            idx: step.idx,
            rel: step.rel,
        })
    }
    pub fn make_void<S>(step: Rc<Step<S, Ix>>) -> Step<(), Ix> {
        Step {
            caller: step
                .caller
                .as_ref()
                .and_then(|step| Some(Rc::new(Self::make_void(step.clone())))),
            idx: step.idx,
            rel: step.rel,
            state: (),
        }
    }
    pub fn into_step(step: Rc<StepUnit<Ix>>) -> Step<(), Ix> {
        Step {
            caller: step
                .caller
                .as_ref()
                .and_then(|step| Some(Rc::new(Self::into_step(step.clone())))),
            idx: step.idx,
            rel: step.rel,
            state: (),
        }
    }
}

#[derive(Debug)]
pub struct Steps<S, Ix> {
    start: Option<Rc<Step<S, Ix>>>,
}

impl<S: Debug, Ix: Debug> IntoIterator for Step<S, Ix> {
    type IntoIter = Steps<S, Ix>;
    type Item = Rc<Step<S, Ix>>;
    fn into_iter(self) -> Self::IntoIter {
        Steps {
            start: Some(Rc::new(self)),
        }
    }
}

impl<S: Debug, Ix: Debug> Iterator for Steps<S, Ix> {
    type Item = Rc<Step<S, Ix>>;
    fn next(&mut self) -> Option<Self::Item> {
        let head = self.start.clone();
        self.start = head.as_ref().and_then(|head| head.caller.clone());
        head.and_then(|head| Some(head))
    }
}

impl<S, Ix> Steps<S, Ix> {
    pub fn from_step(step: Rc<Step<S, Ix>>) -> Self {
        Self { start: Some(step) }
    }
}

pub enum WalkerState<S, Ix> {
    Done,
    Found(Step<S, Ix>),
    NotFound(Rc<Step<S, Ix>>),
    Cutoff,
}

pub trait Walker<S, Ix> {
    fn step(&mut self) -> WalkerState<S, Ix>;
}

pub struct BreadthFirst<'a, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<(), Ix>>,
    visited: FixedBitSet,
    direction: Direction,
}

pub struct Dijkstra<'a, F, K, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<K, Ix>>,
    direction: Direction,
    edge_cost: F,
}

pub struct DepthFirst<'a, D, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<D, Ix>>,
    limit: Option<D>,
    cutoff: bool,
    level: D,
    direction: Direction,
}

impl<'a, D: Zero, I, N, E, Ty: EdgeType, Ix: IndexType> DepthFirst<'a, D, I, N, E, Ty, Ix> {
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
        limit: Option<D>,
        direction: Direction,
    ) -> Self {
        Self {
            graph,
            goal,
            limit,
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
            for child in self
                .graph
                .inner
                .neighbors_directed(parent.idx.into(), self.direction)
            {
                self.border.push_front(Step {
                    caller: Some(parent.clone()),
                    idx: child,
                    rel: None,
                    state: self.level,
                })
            }
            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
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

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> BreadthFirst<'a, I, N, E, Ty, Ix> {
    #[allow(dead_code)]
    pub fn new(
        graph: &'a Graph<I, N, E, Ty, Ix>,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
        direction: Direction,
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
                    state: (),
                });
                border
            },
            visited: graph.visit_map(),
            direction,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<(), Ix>
    for BreadthFirst<'a, I, N, E, Ty, Ix>
{
    fn step(&mut self) -> WalkerState<(), Ix> {
        if let Some(parent) = self.border.pop_front() {
            if self
                .goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return WalkerState::Found(parent);
            }

            let parent = Rc::new(parent);
            self.graph
                .inner
                .neighbors_directed(parent.idx.into(), self.direction)
                .for_each(|child_idx| {
                    (!self.visited.is_visited(&child_idx)).then(|| {
                        self.visited.visit(child_idx);
                        self.border.push_back(Step {
                            caller: Some(parent.clone()),
                            idx: child_idx.into(),
                            rel: None,
                            state: (),
                        });
                    });
                });
            WalkerState::NotFound(parent)
        } else {
            WalkerState::Done
        }
    }
}

/// The library's principal Graph structure.
///
/// The struct is an abstract layer built on top of the
/// [`petgraph::Graph<N, E, Ty, Ix>`](../petgraph/graph/struct.Graph.html)
/// implementation to support named nodes using `I` identifiers
///
/// - `I` is the type used for identifying the nodes, because of its purpose only values that implement
/// [`Copy`](https://doc.rust-lang.org/1.66.1/core/marker/trait.Copy.html) are allowed like `&'static str`
/// or {`u8`, `i8`, ...}. If the identifier is a number it is better
/// to just use [`petgraph::Graph`] since its default
/// behaviour is to work identifying nodes with numbers, these numbers are named indexes and don't add any overhead
/// like this more high-level API which uses a `HashMap`.
/// - `N` is the type used to store values within the graph's nodes
/// - `E` is the type used to store values within the graph's edges
/// - `Ty` is the Graph connection type. [`petgraph::Directed`](../petgraph/enum.Directed.html) by default
/// - `Ix` is the number type value used as indexer for Edges and Nodes.
pub struct Graph<I, N, E, Ty = Directed, Ix = DefaultIx> {
    /// The inner [`petgraph::Graph<N, E, Ty, Ix>`](../petgraph/graph/struct.Graph.html)
    inner: PGraph<N, E, Ty, Ix>,
    /// The map of the `I` node-name to the [`NodeIndex<Ix>`](../petgraph/graph/struct.NodeIndex.html)
    nodes: HashMap<I, NodeIndex<Ix>>,
}

impl<I, N, E, Ty, Ix> Graph<I, N, E, Ty, Ix>
where
    Ty: EdgeType,
    Ix: IndexType,
{
    /// The Graph constructor
    pub fn new() -> Self {
        Self {
            inner: PGraph::<N, E, Ty, Ix>::default(),
            nodes: HashMap::new(),
        }
    }

    /// Construct a new Graph with a fixed initial size.
    /// Since we use a macro to construct the graph we do call this constructor
    /// to save a couple calls to the allocator
    pub fn with_capacity(nodes: usize, edges: usize) -> Self {
        Self {
            inner: PGraph::with_capacity(nodes, edges),
            nodes: HashMap::with_capacity(nodes),
        }
    }

    pub fn node_count(&self) -> usize {
        self.inner.node_count()
    }

    pub fn edge_count(&self) -> usize {
        self.inner.edge_count()
    }

    pub fn visit_map(&self) -> FixedBitSet {
        self.inner.visit_map()
    }
}

impl<I, N, E, Ty: EdgeType, Ix: IndexType> Graph<I, N, E, Ty, Ix>
where
    I: Copy + Hash + Eq,
    NodeIndex: From<NodeIndex<Ix>>,
    EdgeIndex: From<EdgeIndex<Ix>>,
{
    /// Get the high-level node name from the low-level node index. E.g. NodeIndex(0) -> "Arad"
    pub fn index_name<'a>(&'a self, value: NodeIndex<Ix>) -> Option<I> {
        self.nodes
            .iter()
            .find_map(|(key, val)| if val == &value { Some(*key) } else { None })
    }

    /// Get the low-level node index from the high-level node name. E.g. "Arad" -> NodeIndex(0)
    pub fn name_index(&self, ident: I) -> Option<NodeIndex<Ix>> {
        self.nodes.get(&ident).copied()
    }

    /// Connect to nodes by their high-level names. E.g. "Arad" -> "Neamt"
    ///
    /// The method calls the necessary low-level methods to connect both node indexes
    /// within the inner graph.
    ///
    /// Adds values to the inner graph's `Vec<Edge<E, Ix>>` to represent neighbors between nodes
    /// and the binded `E` value
    pub fn next(&mut self, from: I, to: I, edge: E) -> Result<(), ()>
    where
        I: Debug,
    {
        match (self.name_index(from), self.name_index(to)) {
            (Some(fidx), Some(tidx)) => {
                self.inner.add_edge(fidx.into(), tidx.into(), edge);
                Ok(())
            }
            (None, None) => panic!("Los nodos {:?} y {:?} no existen", from, to),
            (None, Some(_)) => panic!("El nodo {:?} no existe", from),
            (Some(_), None) => panic!("El nodo {:?} no existe", to),
        }
    }

    /// Register a node with a given name and stored `N` value
    ///
    /// The method calls the necessary low-level methods to connect both node indexes
    /// within the inner graph.
    ///
    /// Adds values to the inner graph's `Vec<Node<N, Ix>>` to represent neighbors between nodes
    /// and the binded `N` value
    pub fn register(&mut self, ident: I, node: N) -> Result<(), ()>
    where
        I: Debug,
    {
        if self.nodes.contains_key(&ident) {
            panic!("El nodo {:?} ya existe", ident);
        } else {
            let ix = self.inner.add_node(node);
            self.nodes.insert(ident, ix);
            Ok(())
        }
    }

    pub fn iterative_depth_first<D>(
        &self,
        start: I,
        goal: Option<I>,
        limit: Option<D>,
    ) -> Result<Step<D, Ix>, ()>
    where
        D: Measure + Copy + One + Zero,
    {
        match goal {
            Some(goal) => {
                match (self.name_index(start), self.name_index(goal)) {
                    (Some(fidx), Some(tidx)) => {
                        let mut cur_limit: D = One::one();
                        loop {
                            if limit
                                .and_then(|limit| Some(limit == cur_limit))
                                .unwrap_or(false)
                            {
                                return Err(());
                            }
                            match self.depth_first_impl::<D>(fidx, Some(tidx), Some(cur_limit)) {
                            Ok(res) => {
                                return Ok(res);
                            }
                            Err(err) => match err {
                                WalkerState::Done => {
                                    return Err(());
                                }
                                WalkerState::Cutoff => {
                                    cur_limit = cur_limit + One::one();
                                    continue;
                                },
                                _ => unreachable!("Only WalkerState::Done and WalkerState::Cutoff are returned")
                            },
                        }
                        }
                    }
                    (None, None) => Err(()),
                    (None, Some(_)) => Err(()),
                    (Some(_), None) => Err(()),
                }
            }
            _ => match self.name_index(start) {
                Some(fidx) => self.depth_first_impl(fidx, None, limit).map_err(|_| ()),
                _ => Err(()),
            },
        }
    }

    pub fn depth_first<D>(
        &self,
        start: I,
        goal: Option<I>,
        limit: Option<D>,
    ) -> Result<Step<D, Ix>, ()>
    where
        D: Measure + Copy + One + Zero,
    {
        match goal {
            Some(goal) => match (self.name_index(start), self.name_index(goal)) {
                (Some(fidx), Some(tidx)) => self
                    .depth_first_impl::<D>(fidx, Some(tidx), limit)
                    .map_err(|_| ()),
                (None, None) => Err(()),
                (None, Some(_)) => Err(()),
                (Some(_), None) => Err(()),
            },
            _ => match self.name_index(start) {
                Some(fidx) => self.depth_first_impl(fidx, None, limit).map_err(|_| ()),
                _ => Err(()),
            },
        }
    }

    pub fn depth_first_impl<D>(
        &self,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
        limit: Option<D>,
    ) -> Result<Step<D, Ix>, WalkerState<D, Ix>>
    where
        D: Measure + Copy + One + Zero,
    {
        let mut border = VecDeque::with_capacity(self.node_count());
        border.push_front(Step {
            caller: None,
            idx: start,
            rel: None,
            state: Zero::zero(),
        });
        let mut cutoff = false;
        let mut nivel;

        while let Some(parent) = border.pop_front() {
            if limit
                .and_then(|limit| Some(parent.state == limit))
                .unwrap_or(false)
            {
                if self
                    .inner
                    .neighbors_directed(parent.idx.into(), Direction::Outgoing)
                    .count()
                    != 0
                {
                    cutoff = true;
                }
                continue;
            }
            if goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return Ok(parent.clone());
            }

            nivel = parent.state + One::one();
            for child in self
                .inner
                .neighbors_directed(parent.idx.into(), petgraph::Direction::Outgoing)
            {
                border.push_front(Step {
                    caller: Some(Rc::new(parent.clone())),
                    idx: child,
                    rel: None,
                    state: nivel,
                })
            }
        }

        if cutoff {
            Err(WalkerState::Cutoff)
        } else {
            Err(WalkerState::Done)
        }
    }

    pub fn breadth_first(&self, start: I, goal: Option<I>) -> Result<Steps<(), Ix>, ()> {
        match goal {
            Some(goal) => match (self.name_index(start), self.name_index(goal)) {
                (Some(fidx), Some(tidx)) => self.breadth_first_impl(fidx, Some(tidx)),
                (None, None) => Err(()),
                (None, Some(_)) => Err(()),
                (Some(_), None) => Err(()),
            },
            _ => match self.name_index(start) {
                Some(fidx) => self.breadth_first_impl(fidx, None),
                _ => Err(()),
            },
        }
    }

    pub fn dijkstra<K, F>(
        &self,
        start: I,
        goal: Option<I>,
        edge_cost: F,
    ) -> Result<Steps<K, Ix>, ()>
    where
        K: Measure + Copy + Eq + Default + Ord + PartialOrd,
        F: FnMut(&E) -> K,
    {
        match goal {
            Some(goal) => match (self.name_index(start), self.name_index(goal)) {
                (Some(fidx), Some(tidx)) => self.dijkstra_impl(fidx, Some(tidx), edge_cost),
                (None, None) => Err(()),
                (None, Some(_)) => Err(()),
                (Some(_), None) => Err(()),
            },
            _ => match self.name_index(start) {
                Some(fidx) => self.dijkstra_impl(fidx, None, edge_cost),
                _ => Err(()),
            },
        }
    }

    pub fn dijkstra_impl<'a, K, F>(
        &self,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
        mut edge_cost: F,
    ) -> Result<Steps<K, Ix>, ()>
    where
        K: Measure + Copy + Eq + Default + Ord + PartialOrd,
        F: FnMut(&E) -> K,
    {
        let mut border = VecDeque::with_capacity(self.inner.node_count());
        border.push_front(Step {
            caller: None,
            idx: start,
            rel: None,
            state: K::default(),
        });

        while let Some(parent) = {
            let i = border
                .iter()
                .enumerate()
                .min_by(|(_, s1), (_, s2)| s1.state.cmp(&s2.state))
                .map(|(x, _)| x);
            i.map(|i| border.remove(i).unwrap())
        } {
            let parent = Rc::new(parent);
            for child_idx in self
                .inner
                .neighbors_directed(parent.idx.into(), petgraph::Direction::Outgoing)
            {
                let es_goal = goal
                    .and_then(|goal| Some(goal == child_idx))
                    .unwrap_or(false);
                let tiene_hijos = self
                    .inner
                    .neighbors_directed(child_idx, petgraph::Direction::Outgoing)
                    .count()
                    != 0;
                if es_goal || tiene_hijos {
                    let edge = self.inner.find_edge(parent.idx, child_idx).unwrap();
                    let step = Step {
                        caller: Some(parent.clone()),
                        idx: child_idx.into(),
                        rel: Some(edge.into()),
                        state: parent.state + edge_cost(&self.inner[edge]),
                    };

                    if es_goal {
                        return Ok(step.into_iter());
                    }
                    border.push_front(step)
                }
            }
        }
        Err(())
    }

    pub fn breadth_first_impl(
        &self,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
    ) -> Result<Steps<(), Ix>, ()> {
        let mut border = VecDeque::with_capacity(self.inner.node_count());
        let mut visited = self.inner.visit_map();
        border.push_front(Step {
            caller: None,
            idx: start,
            rel: None,
            state: (),
        });

        while let Some(parent) = border.pop_front() {
            if goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return Ok(parent.into_iter());
            }

            if self
                .inner
                .neighbors_directed(parent.idx.into(), petgraph::Direction::Outgoing)
                .count()
                != 0
            {
                let parent = Rc::new(parent);
                self.inner
                    .neighbors_directed(parent.idx.into(), petgraph::Direction::Outgoing)
                    .for_each(|child_idx| {
                        (!visited.is_visited(&child_idx)).then(|| {
                            visited.visit(child_idx);
                            border.push_back(Step {
                                caller: Some(parent.clone()),
                                idx: child_idx.into(),
                                rel: None,
                                state: (),
                            });
                        });
                    });
            }
        }
        Err(())
    }

    pub fn bidirectional<S: Debug, D: Debug>(
        &self,
        mut algo1: impl Walker<S, Ix>,
        mut algo2: impl Walker<D, Ix>,
    ) -> Result<Step<(), Ix>, ()> {
        let mut res1;
        let mut res2;
        let mut visited1 = self.visit_map();
        let mut visited2 = self.visit_map();

        let mut steps1: HashMap<_, Rc<StepUnit<_>>> = HashMap::new();
        let mut steps2: HashMap<_, Rc<StepUnit<_>>> = HashMap::new();

        let mut last_step_1 = None;
        let mut last_step_2 = None;
        // let mut i = 0;
        let matching_on = loop {
            // i += 1;
            // println!("{i}");
            res1 = algo1.step();
            res2 = algo2.step();

            if let WalkerState::NotFound(ref node) = res1 {
                visited1.visit(node.idx);
                steps1.insert(node.idx, StepUnit::from_step(node.clone()));
                last_step_1 = Some(StepUnit::from_step(node.clone()));
                if visited2.is_visited(&node.idx) {
                    break 1;
                }
            }

            if let WalkerState::NotFound(ref node) = res2 {
                visited2.visit(node.idx);
                steps2.insert(node.idx, StepUnit::from_step(node.clone()));
                last_step_2 = Some(StepUnit::from_step(node.clone()));
                if visited1.is_visited(&node.idx) {
                    break 2;
                }
            }

            if matches!(&res1, WalkerState::Done) && matches!(&res2, WalkerState::Done) {
                return Err(());
            }

            if let WalkerState::Found(node) = res1 {
                return Ok(StepUnit::make_void(Rc::new(node)));
            }
            if let WalkerState::Found(node) = res2 {
                return Ok(StepUnit::make_void(Rc::new(node)));
            }
        };

        // println!("Break on {}", matching_on);
        if let (Some(mut last1), Some(mut last2)) = (last_step_1, last_step_2) {
            let mut trace1 = VecDeque::new();
            let mut trace2 = VecDeque::new();

            let res1_p = (&mut last1, &mut steps1);
            let res2_p = (&mut last2, &mut steps2);
            if matching_on == 1 {
                *res2_p.0 = res2_p.1.get(&res1_p.0.idx).unwrap().clone();
            } else {
                *res1_p.0 = res1_p.1.get(&res2_p.0.idx).unwrap().clone();
            }

            // println!("***1");
            let mut last1 = Some(last1);
            while let Some(i) = last1 {
                trace1.push_back(i.clone());
                // println!("{:?}", self.index_name(i.idx));
                last1 = i.caller.clone();
            }
            // println!("***2");
            let mut last2 = Some(last2);
            while let Some(i) = last2 {
                trace2.push_back(i.clone());
                // println!("{:?}", self.index_name(i.idx));
                last2 = i.caller.clone();
            }

            // println!("***3");
            for i in trace1.range(1..) {
                trace2.push_front(i.clone());
            }

            let first = trace2.pop_front().unwrap();
            let mut result = Step {
                caller: None,
                idx: first.idx,
                rel: None,
                state: (),
            };

            while let Some(i) = trace2.pop_front() {
                result = Step {
                    caller: Some(Rc::new(result.clone())),
                    idx: i.idx,
                    state: (),
                    rel: i.rel,
                }
            }
            Ok(result)
        } else {
            unreachable!("This point should always have valid last steps")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn grap() -> Graph<&'static str, (), u16> {
        let graph: Graph<&'static str, (), u16> = graph! {
            with_node: (),
            with_edges: next,
            nodes: [
                "Arad",        "Zerind",      "Oradea",     "Sibiu",
                "Fagaras",     "Timisoara",   "Lugoj",      "Mehadia",
                "Drobeta",     "Craiova",     "Pitesti",    "Rimnieu Vilcea",
                "Bucharest",   "Giurgiu",     "Urziceni",   "Hirsova",
                "Eforie",      "Vasiui",      "Iasi",       "Neamt"
            ],
            connections: [
                "Arad" => {(140) "Sibiu", (75) "Zerind", (118) "Timisoara"},
                "Zerind" => {(71) "Oradea"},
                "Oradea" => {(151) "Sibiu"},
                "Sibiu" => {(99) "Fagaras", (80) "Rimnieu Vilcea"},
                "Timisoara" => {(111) "Lugoj"},
                "Lugoj" => {(70) "Mehadia"},
                "Mehadia" => {(75) "Drobeta"},
                "Drobeta" => {(120) "Craiova"},
                "Craiova" => {(138) "Pitesti"},
                "Pitesti" => {(101) "Bucharest"},
                "Rimnieu Vilcea" => {(97) "Pitesti", (146) "Craiova"},
                "Fagaras" => {(211) "Bucharest"},
                "Bucharest" => {(90) "Giurgiu", (85) "Urziceni"},
                "Urziceni" => {(98) "Hirsova", (142) "Vasiui"},
                "Vasiui" => {(92) "Iasi"},
                "Iasi" => {(87) "Neamt"},
                "Hirsova" => {(86) "Eforie"}
            ]
        };
        graph
    }
    #[test]
    fn test_depth() {
        let graph = final_grap();
        let a = graph
            .depth_first::<u32>("Cancun", Some("Cabo San Lucas"), None)
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth() {
        let graph = final_grap();
        let a = graph
            .breadth_first("Cancun", Some("Cabo San Lucas"))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth_walker() {
        let graph = final_grap();
        let mut a = BreadthFirst::new(
            &graph,
            graph.name_index("Cabo San Lucas").unwrap(),
            Some(graph.name_index("Cancun").unwrap()),
            Direction::Incoming,
        );

        let a = {
            loop {
                match a.step() {
                    WalkerState::Found(result) => {
                        break Some(result.into_iter());
                    }
                    WalkerState::Done => {
                        break None;
                    }
                    _ => {}
                }
            }
        }
        .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra() {
        let graph = final_grap();
        let a = graph
            .dijkstra("Cancun", Some("Cabo San Lucas"), |state| *state)
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra_walker() {
        let graph = final_grap();
        let mut a = Dijkstra::new(
            &graph,
            graph.name_index("Cabo San Lucas").unwrap(),
            Some(graph.name_index("Cancun").unwrap()),
            Direction::Incoming,
            |state| *state,
        );

        let a = {
            // let mut i = 0;
            loop {
                // i += 1;
                // println!("{i}");
                match a.step() {
                    WalkerState::Found(result) => {
                        break Some(result.into_iter());
                    }
                    WalkerState::Done => {
                        break None;
                    }
                    _ => {}
                }
            }
        }
        .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_bidirectional() {
        let graph = final_grap();

        let a = BreadthFirst::new(
            &graph,
            graph.name_index("Cancun").unwrap(),
            Some(graph.name_index("Cabo San Lucas").unwrap()),
            Direction::Outgoing,
        );
        let b = DepthFirst::new(
            &graph,
            graph.name_index("Cancun").unwrap(),
            Some(graph.name_index("Cabo San Lucas").unwrap()),
            None::<usize>,
            Direction::Incoming,
        );

        let res = graph.bidirectional(a, b).unwrap();
        for node in res {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    fn final_grap() -> Graph<&'static str, (), u16> {
        let graph: Graph<&'static str, (), u16> = graph! {
            with_node: (),
            with_edges: next,
            nodes: [
                "Acapulco",        "Villa Hermosa",      "Guanajuato",     "Cancun",
                "Chilpancingo",     "Aguaprieta",   "Alvarado",      "Valladolid",
                "Acayucan",     "Santa Ana",     "Oaxaca",    "Chetumal",
                "Tehuantepec",   "Aguascalientes",     "Atlacomulco",   "Campeche",
                "Tuxtla",      "Guadalajara",      "Queretaro",       "Felipe Carrillo Puerto",
                "Merida", "Chihuahua", "Janos", "Juarez", "Ojinaga", "Iguala", "Ciudad Altamirano",
                "Cuernavaca", "Toluca de Lerdo", "Zihuatanejo", "Ciudad del Carmen", "Ciudad Obregon",
                "Guaymas", "Ciudad Victoria", "Matamoros", "Soto la Marina", "Tampico", "Colima",
                "Morelia", "Playa Azul", "Cordoba", "Veracruz", "Culiacan", "Hidalgo del Parral",
                "Topolobampo", "Durango", "Mazatlan", "Torreon", "Ensenada", "San Quintin" , "Francisco Escarcega",
                "Manzanillo", "Salamanca", "Hermosillo", "San Luis Potosi", "Izucar de Matamoros", "La Paz",
                "Cabo San Lucas", "Reynosa", "Mexicalli", "San Felipe", "Tijuana", "Ciudad de Mexico", "Pachuca de Soto",
                "Puebla", "Tlaxcala", "Monclova", "Piedras Negras", "Monterrey", "Nuevo Laredo" , "Puerto Angel",
                "Tehuacan", "Tuxpan de Rodriguez Cano", "Pinotepa Nacional", "Zacatecas", "Santa Rosalia", "Santo Domingo", "Tepic", "Ciudad Juarez"

            ],
            connections: [
                "Cancun" => {(90) "Valladolid", (100) "Felipe Carrillo Puerto"},
                "Valladolid" => {(90) "Felipe Carrillo Puerto"},
                "Felipe Carrillo Puerto" => {(60) "Campeche"},
                "Campeche" => {(90) "Merida", (100) "Chetumal", (90) "Ciudad del Carmen"},
                "Chetumal" => {(111) "Francisco Escarcega"},
                "Ciudad del Carmen" => {(90) "Villa Hermosa", (90) "Tuxtla"},
                "Villa Hermosa" => {(90) "Acayucan"},
                "Tuxtla" => {(90) "Acayucan"},
                "Acayucan" => {(80) "Tehuantepec", (110) "Alvarado"},
                "Alvarado" => {(100) "Oaxaca"},
                "Oaxaca" => {(80) "Tehuacan", (90) "Puerto Angel", (90) "Izucar de Matamoros"},
                "Puerto Angel" => {(100) "Pinotepa Nacional" },
                "Izucar de Matamoros" => {(90) "Puebla", (100) "Cuernavaca"},
                "Pinotepa Nacional" => {(100) "Acapulco"},
                "Cuernavaca" => {(100) "Ciudad de Mexico", (100) "Ciudad Altamirano"},
                "Puebla" => {(90) "Ciudad de Mexico", (80) "Cordoba"},
                "Acapulco" => {(140) "Chilpancingo"},
                "Ciudad de Mexico" => {(100) "Tlaxcala", (110) "Toluca de Lerdo", (90) "Queretaro", (100) "Pachuca de Soto"},
                "Ciudad Altamirano" => {(90) "Zihuatanejo"},
                "Cordoba" => {(90) "Veracruz"},
                "Chilpancingo" => {(90) "Iguala"},
                "Toluca de Lerdo" => {(100) "Ciudad Altamirano"},
                "Queretaro" => {(90) "Atlacomulco", (90) "Salamanca", (90) "San Luis Potosi"},
                "Pachuca de Soto" => {(110) "Tuxpan de Rodriguez Cano"},
                "Zihuatanejo" => {(90) "Playa Azul"},
                "Iguala" => {(100) "Cuernavaca", (110) "Ciudad Altamirano"},
                "Salamanca" => {(90) "Guanajuato", (90) "Guadalajara"},
                "San Luis Potosi" => {(90) "Zacatecas", (70) "Durango", (100) "Aguascalientes" },
                "Tuxpan de Rodriguez Cano" => {(100) "Tampico"},
                "Playa Azul" => {(100) "Morelia", (100) "Colima", (100) "Manzanillo"},
                "Guanajuato" => {(80) "Aguascalientes"},
                "Guadalajara" => {(110) "Tepic"},
                "Aguascalientes" =>{(70) "Guadalajara"},
                "Durango" => {(90) "Hidalgo del Parral", (90) "Mazatlan"},
                "Tampico" => {(80) "Ciudad Victoria"},
                "Morelia" => {(90) "Salamanca"},
                "Manzanillo" => {(50) "Colima", (80) "Guadalajara"},
                "Colima" => {(90) "Morelia", (50) "Guadalajara"},
                "Tepic" =>{(50) "Mazatlan"},
                "Hidalgo del Parral" => {(130) "Chihuahua", (110) "Topolobampo", (80) "Culiacan"},
                "Mazatlan" => {(90) "Culiacan"},
                "Ciudad Victoria" => {(80) "Soto la Marina", (80) "Matamoros", (80) "Monterrey", (80) "Durango"},
                "Chihuahua" => {(90) "Ciudad Juarez", (90) "Janos"},
                "Topolobampo" => {(90) "Ciudad Obregon"},
                "Culiacan" => {(110) "Topolobampo"},
                "Matamoros" => {(90) "Reynosa"},
                "Monterrey" => {(110) "Nuevo Laredo",(70) "Monclova"},
                "Janos" => {(110) "Aguaprieta"},
                "Ciudad Obregon" => {(80) "Guaymas"},
                "Reynosa" => {(100) "Nuevo Laredo"},
                "Nuevo Laredo" => {(100) "Piedras Negras"},
                "Monclova" => {(100) "Torreon", (90) "Ojinaga"},
                "Aguaprieta" => {(90) "Santa Ana"},
                "Guaymas" => {(90) "Hermosillo"},
                "Piedras Negras" => {(90) "Monclova"},
                "Torreon" => {(90) "Durango"},
                "Ojinaga" => {(90) "Chihuahua"},
                "Santa Ana" => {(159) "Mexicalli"},
                "Hermosillo" => {(100) "Santa Ana"},
                "Mexicalli" => {(50) "Tijuana", (70) "San Felipe"},
                "Tijuana" => {(30) "Ensenada"},
                "San Felipe" => {(50) "Ensenada"},
                "Ensenada" => {(90) "San Quintin"},
                "San Quintin" => {(140) "Santa Rosalia"},
                "Santa Rosalia" => {(100) "Santo Domingo"},
                "Santo Domingo" => {(100) "La Paz"},
                "La Paz" => {(40) "Cabo San Lucas"}
            ]
        };
        graph
    }
}
