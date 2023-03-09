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
use unicase::Ascii;

use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
    rc::Rc,
};

pub mod walkers;
use walkers::{Walker, WalkerState};

/// Counts the number of nodes and edges of the graph
///
/// This is a recursive macro that calls itself *n* times where *n*
/// is the depth the token tree has, hence the number of items it's been
/// applied to.
#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + count!($($xs)*));
}

/// Declarative way to create Graph instances with ease.
///
/// The macro has two main fields:
/// - `nodes`: A list of node identifiers (names)
/// - `connections`: A list of type `identifier => connections`
///
/// There is the possibility to create overloading equivalent macros to initialize
/// sets of fields to the same value. E.g. the `with_edges` field makes all edges to be of
/// a specific type.
///
/// Example:
/// ```rust
/// use graph::{Graph, graph, count};
/// let graph: Graph<&str, (), u8> = graph! {
///     with_node: (),
///     with_edges: next,
///     nodes: [ "A", "B", "C", "D", "E" ],
///     connections: [
///         "A" => { (7) "C", (10) "B" },
///         "B" => { (4) "C", (11) "D" },
///         "D" => { (5) "E" }
///     ]
/// };
/// ```
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
        with_edges: $type:tt,
        nodes: [$($ident:expr => $node:expr),*],
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

/// A single step in the graph
///
/// Steps are the way we compute and represent the graph traversal with the
/// various algorithms.
///
/// Steps can store a type `S` which can be used to hold any information like
/// total weight or any other primitive or structure.
#[derive(Clone, Debug)]
pub struct Step<S, Ix> {
    /// The parent State that invoked this instance.
    /// If the option is None then it means we arrived to the root state.
    pub caller: Option<Rc<Step<S, Ix>>>,
    /// The current node index the step is at within the graph.
    pub idx: NodeIndex<Ix>,
    /// The index of the edge that binds caller -> self
    pub rel: Option<EdgeIndex>,
    /// State of the walking progress
    pub state: S,
}

/// A stateless step.
///
/// The structure comes in handy when working with two `Step` intances bound to
/// diferent data types for storing state e.g. `Step<S, Ix>` and `Step<Y, Ix>`
///
/// We then construct a copy of the call-chain removing the state from each step,
/// the final result is two `StepUnit<Ix>` that we can work with.
pub type StepUnit<Ix> = Step<(), Ix>;

impl<Ix: IndexType> StepUnit<Ix> {
    pub fn from_step<S>(step: Rc<Step<S, Ix>>) -> Rc<Self> {
        Rc::new(Self {
            caller: step
                .caller
                .as_ref()
                .and_then(|step| Some(Self::from_step(step.clone()))),
            idx: step.idx,
            rel: step.rel,
            state: (),
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

/// A Step iterator
#[derive(Debug)]
pub struct Steps<S, Ix = DefaultIx> {
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
    pub nodes: HashMap<Ascii<I>, NodeIndex<Ix>>,
}

pub trait Coords {
    fn get_x(&self) -> f64;
    fn get_y(&self) -> f64;
    fn get_z(&self) -> Option<f64>;
}

impl<I, N: Coords, E, Ty: EdgeType, Ix: IndexType> Graph<I, N, E, Ty, Ix> {
    pub fn get_distances(&self, to: NodeIndex<Ix>) -> HashMap<NodeIndex<Ix>, f64> {
        let w_original = self.inner.node_weight(to).unwrap();
        let (x1, y1) = (<N as Coords>::get_x(w_original), <N as Coords>::get_y(w_original));
        self.inner.node_indices().into_iter().map(|idx| {
            let node = self.inner.node_weight(idx).unwrap();
            let distance = (
                    (<N as Coords>::get_x(&node) - x1).powi(2)
                    + (<N as Coords>::get_y(&node) - y1).powi(2)
            ).sqrt();
            (idx, distance)
        }).collect::<HashMap<_, _>>()
    }
}

impl<I, N, E, Ty: EdgeType, Ix: IndexType> Graph<I, N, E, Ty, Ix> {
    /// Create a new empty instance of [`graph::Graph`](./struct.Graph.html)
    pub fn new() -> Self {
        Self {
            inner: PGraph::<N, E, Ty, Ix>::default(),
            nodes: HashMap::new(),
        }
    }

    /// Create a new [`graph::Graph`](./struct.Graph.html) with a fixed initial size.
    /// Since we use the macro [`graph::count`](./macro.count.html) to construct the
    /// graph we do call this constructor to save a couple calls to the allocator
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
    Ascii<I>: Copy + Hash + Eq,
    NodeIndex: From<NodeIndex<Ix>>,
    EdgeIndex: From<EdgeIndex<Ix>>,
{
    /// Get the high-level node name from the low-level node index. E.g. NodeIndex(0) -> "Arad"
    pub fn index_name<'a>(&'a self, value: NodeIndex<Ix>) -> Option<I> {
        self.nodes.iter().find_map(|(key, val)| {
            if val == &value {
                Some(key.into_inner())
            } else {
                None
            }
        })
    }

    /// Get the low-level node index from the high-level node name. E.g. "Arad" -> NodeIndex(0)
    pub fn name_index(&self, ident: I) -> Option<NodeIndex<Ix>> {
        self.nodes.get(&Ascii::new(ident)).copied()
    }

    /// Connect two nodes by their high-level names. E.g. "Arad" -> "Neamt"
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
        let ascii = Ascii::new(ident);
        if self.nodes.contains_key(&ascii) {
            panic!("El nodo {:?} ya existe", ident);
        } else {
            let ix = self.inner.add_node(node);
            self.nodes.insert(ascii, ix);
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
        D: Measure + Copy + One + Zero + Debug,
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
            if goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return Ok(parent.into_iter());
            }

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

    pub fn greedy_best_first_impl<K, F> (
            &self,
        start: NodeIndex<Ix>,
        goal: Option<NodeIndex<Ix>>,
        edge_cost: F,
    ) -> Result<Steps<K, Ix>, ()>
        where
        F: Fn(&NodeIndex<Ix>) -> K,
            K: Measure + Copy + Default + PartialOrd
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
                .min_by(|(_, s1), (_, s2)| s1.state.partial_cmp(&s2.state).unwrap())
                .map(|(x, _)| x);
            i.map(|i| border.remove(i).unwrap())
        } {
            if goal
                .and_then(|goal| Some(goal == parent.idx))
                .unwrap_or(false)
            {
                return Ok(parent.into_iter());
            }

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
                    .count() != 0;
                if es_goal || tiene_hijos {
                    let edge = self.inner.find_edge(parent.idx, child_idx).unwrap();
                    let step = Step {
                        caller: Some(parent.clone()),
                        idx: child_idx.into(),
                        rel: Some(edge.into()),
                        state: edge_cost(&child_idx),
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

impl Coords for (f64, f64) {
    fn get_x(&self) -> f64 {
        self.0
    }
    fn get_y(&self) -> f64 {
        self.1
    }
    fn get_z(&self) -> Option<f64> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::walkers::*;

    fn test_graph() -> Graph<&'static str, (f64, f64), u16> {
        graph! {
            with_edges: next,
            nodes: [
                "Arad" => (-8., 3.),
                "Zerind" => (-7.5,4.8),
                "Oradea" => (-6.5, 6.2),
                "Sibiu" => (-3.5, 1.8),
                "Fagaras" => (0.3, 1.5),
                "Timisoara" => (-8., 0.),
                "Lugoj" => (-5.2, -1.2),
                "Mehadia"=> (-5.1, -3.),
                "Drobeta"=> (-5.4, -4.4),
                "Craiova" =>(-2., -5.),
                "Pitesti"=>(1., 2.),
                "Rimnieu Vilcea"=> (-2.5, 0.),
                "Bucharest"=> (4., -3.),
                "Giurgiu" =>(3., -5.5),
                "Urziceni" => (6., -2.3),
                "Hirsova"=> (9., -2.2),
                "Eforie"=> (10., -4.4),
                "Vasiui"=> (8.2, 1.3),
                "Iasi" => (6.9, 3.9) ,
                "Neamt" => (4.1, 5.)
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
        }
    }

    #[test]
    fn test_depth() {
        let graph = test_graph();
        let a = graph
            .depth_first::<u32>("Arad", Some("Neamt"), None)
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth() {
        let graph = test_graph();
        let a = graph
            .breadth_first("Arad", Some("Neamt"))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth_walker() {
        let graph = test_graph();
        let mut a = BreadthFirst::new(
                &graph,
            graph.name_index("Neamt").unwrap(),
            Some(graph.name_index("Arad").unwrap()),
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
    fn test_distances() {
        let graph = test_graph();
        for (k, v) in graph.get_distances(graph.name_index("Bucharest").unwrap()).into_iter() {
            println!("{}: {}", graph.index_name(k).unwrap(), v);
        }
    }

    #[test]
    fn test_dijkstra() {
        let graph = test_graph();
        let a = graph
            .dijkstra("Arad", Some("Neamt"), |state| *state)
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_greedy_best_first() {
        let graph = test_graph();
        let start = graph.name_index("Arad").unwrap();
        let goal = graph.name_index("Bucharest").unwrap();
        let graph = test_graph();
        let distances = graph.get_distances(goal);
        let a = graph
            .greedy_best_first_impl(start, Some(goal), |index| *distances.get(index).unwrap() )
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra_walker() {
        let graph = test_graph();
        let mut a = Dijkstra::new(
                &graph,
            graph.name_index("Neamt").unwrap(),
            Some(graph.name_index("Arad").unwrap()),
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
        let graph = test_graph();

        let a = BreadthFirst::new(
                &graph,
            graph.name_index("Arad").unwrap(),
            Some(graph.name_index("Neamt").unwrap()),
            Direction::Outgoing,
        );
        let b = DepthFirst::new(
                &graph,
            graph.name_index("Arad").unwrap(),
            Some(graph.name_index("Neamt").unwrap()),
            None::<usize>,
            Direction::Incoming,
        );

        let res = graph.bidirectional(a, b).unwrap();
        for node in res {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }
}
