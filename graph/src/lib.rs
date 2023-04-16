use anyhow::{anyhow, Context, Result};
use fixedbitset::FixedBitSet;
use glam::f32::Vec2;
use num::{One, Zero};
pub use petgraph;
pub use petgraph::stable_graph::{
    DefaultIx, EdgeIndex, IndexType, NodeIndex, StableGraph as PGraph,
};
pub use petgraph::Direction;
use petgraph::{
    algo::Measure,
    visit::{VisitMap, Visitable},
};
pub use petgraph::{Directed, EdgeType};
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::{Debug, Display},
    hash::Hash,
    ops::Mul,
    rc::Rc,
};
use unicase::Ascii;

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
#[derive(Debug)]
pub struct Step<S, Ix> {
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

/// A stateless step.
///
/// The structure comes in handy when working with two `Step` instances bound to
/// different data types for storing state e.g. `Step<S, Ix>` and `Step<Y, Ix>`
///
/// We then construct a copy of the call-chain removing the state from each step,
/// the final result is two `StepUnit<Ix>` that we can work with.
pub type StepUnit<Ix> = Step<(), Ix>;

impl<Ix: IndexType> StepUnit<Ix> {
    /// TODO: Document
    /// Creates a new `StepUnit` from a `Step` instance.
    ///
    /// The function will recursively create a new `StepUnit` from the `caller`
    /// field of the `Step` instance.
    ///
    /// The function will also remove the state from the `Step` instance.
    pub fn from_step<S>(step: Rc<Step<S, Ix>>) -> Rc<Self> {
        Rc::new(Self {
            caller: step
                .caller
                .as_ref()
                .map(|step| Self::from_step(step.clone())),
            idx: step.idx,
            rel: step.rel,
            state: (),
        })
    }

    /// TODO: Document
    /// Transform a `Step` into a `Step` instance with no datatype coupled to
    /// the state type, instead the state is set to `()`.
    pub fn make_void<S>(step: Rc<Step<S, Ix>>) -> Step<(), Ix> {
        Step {
            caller: step
                .caller
                .as_ref()
                .map(|step| Rc::new(Self::make_void(step.clone()))),
            idx: step.idx,
            rel: step.rel,
            state: (),
        }
    }

    /// TODO: Document
    /// Transform a `StepUnit` into a `Step` instance with no coupling to the
    /// state type, instead the state is set to `()`.
    ///
    /// The function will recursively create a new `Step` from the `caller`
    /// field of the `StepUnit` instance.
    pub fn into_step(step: Rc<StepUnit<Ix>>) -> Step<(), Ix> {
        Step {
            caller: step
                .caller
                .as_ref()
                .map(|step| Rc::new(Self::into_step(step.clone()))),
            idx: step.idx,
            rel: step.rel,
            state: (),
        }
    }

    pub fn collect_nodes<S>(step: Rc<Step<S, Ix>>) -> VecDeque<NodeIndex<Ix>> {
        let mut nodes = VecDeque::new();
        let step = Self::make_void(step);

        for node in step.into_iter() {
            nodes.push_front(node.idx);
        }
        nodes
    }

    pub fn collect_edges<S>(step: Rc<Step<S, Ix>>) -> VecDeque<EdgeIndex<Ix>> {
        let mut nodes = VecDeque::new();
        let step = Self::make_void(step);

        for node in step.into_iter() {
            if let Some(rel) = node.rel {
                nodes.push_front(rel);
            }
        }
        nodes
    }
}

impl<S: Clone, Ix: Clone> Clone for Step<S, Ix> {
    fn clone(&self) -> Self {
        Self {
            caller: self.caller.as_ref().map(|step| Rc::new(Self::clone(step))),
            idx: self.idx.clone(),
            rel: self.rel.clone(),
            state: self.state.clone(),
        }
    }
}

/// A Step iterator
#[derive(Debug)]
pub struct Steps<S, Ix = DefaultIx> {
    start: Option<Rc<Step<S, Ix>>>,
}

impl<S: Debug, Ix: Debug> IntoIterator for Step<S, Ix> {
    type Item = Rc<Step<S, Ix>>;
    type IntoIter = Steps<S, Ix>;
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
        head
    }
}

impl<S, Ix> Steps<S, Ix> {
    pub fn from_step(step: Rc<Step<S, Ix>>) -> Self {
        Self { start: Some(step) }
    }
}

impl<S: Clone, Ix: Clone> Clone for Steps<S, Ix> {
    fn clone(&self) -> Self {
        Self {
            start: self.start.clone(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Node {
    pub name: String,
    pub location: Vec2,
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
    pub inner: PGraph<N, E, Ty, Ix>,
    pub repr: PGraph<Node, (), Ty, Ix>,
    /// The map of the `I` node-name to the [`NodeIndex<Ix>`](../petgraph/graph/struct.NodeIndex.html)
    pub nodes: HashMap<Ascii<I>, NodeIndex<Ix>>,
    repr_nodes: HashMap<Ascii<I>, NodeIndex<Ix>>,
}

pub trait Coords {
    fn get_x(&self) -> f32;
    fn get_y(&self) -> f32;
}

/// Implementation block for distance methods for Graphs where its type N has the Coords trait.
/// The trait Coords is necessary to extract the node coordinates from any given generic N type.
impl<I, N, E, Ty: EdgeType, Ix: IndexType> Graph<I, N, E, Ty, Ix>
where
    N: Coords,
{
    /// Haversine with a constant r of 6317
    pub fn get_haversine_6371(&self, to: NodeIndex<Ix>) -> HashMap<NodeIndex<Ix>, f32> {
        self.get_haversine(to, 6371.)
    }

    /// Haversine distance with a given radius `r`
    /// Formula from https://en.wikipedia.org/wiki/Haversine_formula#Formulation
    pub fn get_haversine(&self, to: NodeIndex<Ix>, r: f32) -> HashMap<NodeIndex<Ix>, f32> {
        use std::f32::consts::PI;

        let w_original = self.inner.node_weight(to).unwrap();
        let (x1, y1) = (
            w_original.get_x() * PI / 180.,
            w_original.get_y() * PI / 180.,
        );
        self.inner
            .node_indices()
            .into_iter()
            .map(|idx| {
                let node = self.inner.node_weight(idx).unwrap();
                let (x2, y2) = (&node.get_x() * PI / 180., &node.get_y() * PI / 180.);
                let distance = 2. * r * {
                    {
                        ((x2 - x1) / 2.).sin().powi(2)
                            + x1.cos() * x2.cos() * ((y2 - y1) / 2.).sin().powi(2)
                    }
                    .sqrt()
                }
                .asin();
                (idx, distance)
            })
            .collect::<HashMap<_, _>>()
    }
}

impl<I, N, E, Ty: EdgeType, Ix: IndexType> Graph<I, N, E, Ty, Ix> {
    /// Create a new empty instance of [`graph::Graph`](./struct.Graph.html)
    pub fn new() -> Self {
        Self {
            inner: PGraph::<N, E, Ty, Ix>::default(),
            nodes: HashMap::new(),
            repr_nodes: HashMap::new(),
            repr: PGraph::<Node, (), Ty, Ix>::default(),
        }
    }

    /// Create a new [`graph::Graph`](./struct.Graph.html) with a fixed initial size.
    /// Since we use the macro [`graph::count`](./macro.count.html) to construct the
    /// graph we do call this constructor to save a couple calls to the allocator
    pub fn with_capacity(nodes: usize, edges: usize) -> Self {
        Self {
            inner: PGraph::with_capacity(nodes, edges),
            nodes: HashMap::with_capacity(nodes),
            repr_nodes: HashMap::with_capacity(nodes),
            repr: PGraph::with_capacity(nodes, edges),
        }
    }

    pub fn edge_between(&self, source: NodeIndex<Ix>, target: NodeIndex<Ix>) -> EdgeIndex<Ix> {
        use petgraph::visit::EdgeRef;
        self.inner
            .edges_connecting(source, target)
            .next()
            .unwrap()
            .id()
            .into()
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
    I: Copy + Hash + Eq + Debug + Display,
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
                self.repr.add_edge(
                    *self.repr_nodes.get(&Ascii::new(from)).unwrap(),
                    *self.repr_nodes.get(&Ascii::new(to)).unwrap(),
                    (),
                );
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
    /// and the bound `N` value
    pub fn register(&mut self, ident: I, node: N) -> Result<(), ()>
    where
        I: Debug,
    {
        let ascii = Ascii::new(ident);

        // If the node doesn't exist, add it to the graph
        if let std::collections::hash_map::Entry::Vacant(e) = self.nodes.entry(ascii) {
            let ix = self.inner.add_node(node);
            let ix_repr = self.repr.add_node(Node {
                name: ident.to_string(),
                location: Vec2::ZERO,
            });
            e.insert(ix);
            self.repr_nodes.insert(ascii, ix_repr);
            Ok(())
        } else {
            panic!("El nodo {:?} ya existe", ident);
        }
    }

    /// Normalize coordinates of the repr graph so that they are centered
    pub fn done_register(&mut self)
    where
        N: Coords,
    {
        // Collect x, y from the inner graph
        let avg_lon = self
            .inner
            .node_weights()
            .map(|node| node.get_x())
            .sum::<f32>()
            / self.inner.node_count() as f32;
        let avg_lat = self
            .inner
            .node_weights()
            .map(|node| node.get_y())
            .sum::<f32>()
            / self.inner.node_count() as f32;

        // Shift all coordinates to the center
        std::iter::zip(self.inner.node_weights(), self.repr.node_weights_mut()).for_each(
            |(inner, repr)| {
                repr.location = (inner.get_x() - avg_lon, inner.get_y() - avg_lat).into();
            },
        );
    }

    pub fn iterative_depth_first<D>(
        &self,
        start: I,
        goal: Option<I>,
        limit: Option<D>,
    ) -> Result<Step<D, Ix>>
    where
        D: Measure + Copy + One + Zero,
    {
        let mut cur_limit: D = One::one();
        loop {
            if limit
                .and_then(|limit| Some(limit == cur_limit))
                .unwrap_or(false)
            {
                todo!("Return a cutoff error");
            }
            let machine = walkers::DepthFirst::new(
                self,
                self.journey(start, goal)?,
                limit,
                Direction::Outgoing,
            );
            match self.perform_search::<D>(machine) {
                Ok(res) => {
                    return Ok(res);
                }
                Err(err) => match err {
                    WalkerState::Done => {
                        return Err(anyhow!("No path found"));
                    }
                    WalkerState::Cutoff => {
                        cur_limit = cur_limit + One::one();
                        continue;
                    }
                    _ => {
                        unreachable!("Only WalkerState::Done and WalkerState::Cutoff are returned")
                    }
                },
            }
        }
    }

    pub fn journey(
        &self,
        start: I,
        goal: Option<I>,
    ) -> Result<(NodeIndex<Ix>, Option<NodeIndex<Ix>>)> {
        let start = self
            .name_index(start)
            .context(format!("Starting node {} does not exist", start))?;
        let end = goal
            .map(|ident| {
                self.name_index(ident)
                    .context(format!("Target node {} does not exist", ident))
            })
            .transpose()?;
        Ok((start, end))
    }

    pub fn perform_search<D>(
        &self,
        mut machine: impl Walker<D, Ix>,
    ) -> Result<Step<D, Ix>, WalkerState<D, Ix>> {
        let mut res = machine.step();

        while !matches!(res, WalkerState::Done) {
            if let WalkerState::Found(step) = res {
                return Ok(step);
            }
            res = machine.step();
        }
        Err(res)
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
        let matching_on = loop {
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

            let mut last1 = Some(last1);
            while let Some(i) = last1 {
                trace1.push_back(i.clone());
                last1 = i.caller.clone();
            }

            let mut last2 = Some(last2);
            while let Some(i) = last2 {
                trace2.push_back(i.clone());
                last2 = i.caller.clone();
            }

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

/// Implement the Coords trait for all tuples (f32, f32).
///
/// When a Graph has the generic N type bound to (f32, f32) we can call
/// the various distance methods on the nodes.
impl Coords for (f32, f32) {
    fn get_x(&self) -> f32 {
        self.0
    }
    fn get_y(&self) -> f32 {
        self.1
    }
}

impl Coords for () {
    fn get_x(&self) -> f32 {
        0.
    }
    fn get_y(&self) -> f32 {
        0.
    }
}

#[allow(dead_code)]
pub fn test_graph() -> Graph<&'static str, (f32, f32), u16> {
    let mut g = graph! {
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
    };
    g.done_register();
    g
}

pub fn test_graph2() -> Graph<&'static str, (f32, f32), u16> {
    let mut graph: Graph<&'static str, (f32, f32), u16> = graph! {
        with_edges: next,
        nodes: [
            "Acapulco" => ( -99.82365329900568, 16.85310859874989 ),
            "Chilpancingo" => ( -99.50063219718855, 17.5515346019228 ),
            "Acayucan" => ( -94.91473748198624, 17.94923695984836 ),
            "Tehuantepec" => ( -95.24232999999997, 16.3226994 ),
            "Tuxtla" => ( -93.10312145056437, 16.75157557931809 ),
            "Villa Hermosa" => ( -92.94752610256997, 17.98944564153284 ),
            "Agua Prieta" => ( -109.5489603638477, 31.32777318316638 ),
            "Santa Ana" => ( -111.1196215, 30.53983329999999 ),
            "Aguascalientes" => ( -102.2915677, 21.88525620000001 ),
            "Guadalajara" => ( -103.3496092, 20.65969879999995 ),
            "Guanajuato" => ( -101.2573586, 21.0190145 ),
            "Alvarado" => ( -95.75894900000004, 18.7696195 ),
            "Oaxaca" => ( -96.72658889999992, 17.0731842 ),
            "Atlacomulco" => ( -99.87668250000002, 19.7975581 ),
            "Queretaro" => ( -100.3898881, 20.5887932 ),
            "Cancun" => ( -86.85153608730901, 21.16190750852745 ),
            "Valladolid" => ( -88.20224879999994, 20.68964 ),
            "Chetumal" => ( -88.29614599999994, 18.5001889 ),
            "Campeche" => ( -90.5349087, 19.8301251 ),
            "Felipe Carrillo Puerto" => ( -88.04409570000003, 19.58033420000001 ),
            "Merida" => ( -89.59258569999994, 20.9673702 ),
            "Chihuahua" => ( -106.0691004, 28.63299570000001 ),
            "Janos" => ( -108.1924158886408, 30.88893268807164 ),
            "Juarez" => ( -106.4245478, 31.6903638 ),
            "Ojinaga" => ( -104.4082915783784, 29.54588456755576 ),
            "Iguala" => ( -99.53973439999994, 18.3448477 ),
            "Ciudad Altamirano" => ( -100.6686260911074, 18.35781544847858 ),
            "Cuernavaca" => ( -99.2215659, 18.9242095 ),
            "Toluca de Lerdo" => ( -99.65566529999998, 19.2826098 ),
            "Zihuatanejo" => ( -101.5516954999999, 17.64166930000001 ),
            "Ciudad del Carmen" => ( -91.8074586, 18.6504879 ),
            "Ciudad Obregon" => ( -109.9303660117927, 27.4827723941238 ),
            "Guaymas" => ( -110.9089378, 27.91786510000001 ),
            "Ciudad Victoria" => ( -99.14111539999999, 23.73691640000001 ),
            "Matamoros" => ( -97.50273759999996, 25.86902940000001 ),
            "Soto la Marina" => ( -98.20762819999996, 23.76801930000001 ),
            "Tampico" => ( -97.86109899999995, 22.2331041 ),
            "Colima" => ( -103.7240866970209, 19.24523428494255 ),
            "Morelia" => ( -101.1949825, 19.70595040000001 ),
            "Playa Azul" => ( -102.350469, 17.98202150000001 ),
            "Cordoba" => ( -96.9237751, 18.8838909 ),
            "Veracruz" => ( -96.13422410000001, 19.173773 ),
            "Culiacan" => ( -107.3940117, 24.80906490000001 ),
            "Hidalgo del Parral" => ( -105.6666166, 26.9317835 ),
            "Topolobampo" => ( -109.0503685, 25.60069250000001 ),
            "Durango" => ( -104.6531759, 24.0277202 ),
            "Mazatlan" => ( -106.4111425, 23.2494148 ),
            "Torreon" => ( -103.4067860999999, 25.5428443 ),
            "Ensenada" => ( -116.5963708985866, 31.86674248460107 ),
            "San Quintin" => ( -115.9379302, 30.5608767 ),
            "Francisco Escarcega" => ( -90.73902439049336, 18.61018341806402 ),
            "Manzanillo" => ( -104.3384616, 19.1138094 ),
            "Salamanca" => ( -101.1957172, 20.5739314 ),
            "Tepic" => ( -104.8946664723632, 21.50414299987411 ),
            "Hermosillo" => ( -110.9559192, 29.0729673 ),
            "San Luis Potosi" => ( -100.9855409, 22.15646989999995 ),
            "Izucar de Matamoros" => ( -98.46778509999996, 18.5991249 ),
            "La Paz" => ( -110.3127531, 24.1426408 ),
            "Cabo San Lucas" => ( -109.9167371, 22.8905327 ),
            "Reynosa" => ( -98.29789509999996, 26.05084060000001 ),
            "Mexicalli" => ( -115.4522620271724, 32.62453873266227 ),
            "San Felipe" => ( -114.8407776, 31.02507090000001 ),
            "Tijuana" => ( -117.0382434591223, 32.51494624262627 ),
            "Ciudad de Mexico" => ( -99.13320799999998, 19.4326077 ),
            "Pachuca de Soto" => ( -98.75913109999998, 20.10106080000001 ),
            "Puebla" => ( -98.20627269999999, 19.0414398 ),
            "Tlaxcala" => ( -98.23757882356725, 19.3181620778507 ),
            "Monclova" => ( -101.4215189298722, 26.9080248820723 ),
            "Piedras Negras" => ( -100.5408622, 28.6916182 ),
            "Monterrey" => ( -100.3161126, 25.68661420000001 ),
            "Nuevo Laredo" => ( -99.54957116219069, 27.47791757328244 ),
            "Puerto Angel" => ( -96.49131370809657, 15.66800763147724 ),
            "Tehuacan" => ( -97.4003716535704, 18.46649712573129 ),
            "Tuxpan de Rodriguez Cano" => ( -97.40633509999998, 20.95611490000001 ),
            "Pinotepa Nacional" => ( -98.05368699999998, 16.3411824 ),
            "Zacatecas" => ( -102.5832525341495, 22.77092401669794 ),
            "Santa Rosalia" => ( -112.2701464859334, 27.33619390443964 ),
            "Santo Domingo" => ( -111.9888369288694, 25.34873125527853 )
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
            "Chihuahua" => {(90) "Juarez", (90) "Janos"},
            "Topolobampo" => {(90) "Ciudad Obregon"},
            "Culiacan" => {(110) "Topolobampo"},
            "Matamoros" => {(90) "Reynosa"},
            "Monterrey" => {(110) "Nuevo Laredo",(70) "Monclova"},
            "Janos" => {(110) "Agua Prieta"},
            "Ciudad Obregon" => {(80) "Guaymas"},
            "Reynosa" => {(100) "Nuevo Laredo"},
            "Nuevo Laredo" => {(100) "Piedras Negras"},
            "Monclova" => {(100) "Torreon", (90) "Ojinaga"},
            "Agua Prieta" => {(90) "Santa Ana"},
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
    graph.done_register();
    graph
}
#[cfg(test)]
mod tests {
    use super::walkers::*;

    #[test]
    fn test_depth() {
        let graph = test_graph();
        let a = graph
            .perform_search::<u32>(walkers::DepthFirst::new(
                &graph,
                graph.journey("Arad", Some("Neamt")).unwrap(),
                None,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth() {
        let graph = test_graph();
        let a = graph
            .perform_search::<()>(walkers::BreadthFirst::new(
                &graph,
                graph.journey("Arad", Some("Neamt")).unwrap(),
                Direction::Outgoing,
            ))
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
            graph.journey("Neamt", Some("Arad")).unwrap(),
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
        for (k, v) in graph
            .get_haversine_6371(graph.name_index("Bucharest").unwrap())
            .into_iter()
        {
            println!("{}: {}", graph.index_name(k).unwrap(), v);
        }
    }

    #[test]
    fn test_dijkstra() {
        let graph = test_graph();
        let a = graph
            .perform_search(dijkstra::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |edge| *edge,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_greedy_best_first() {
        let graph = test_graph();
        let distances = graph.get_haversine_6371(graph.name_index("Bucharest").unwrap());
        let a = graph
            .perform_search(greedy::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |index| *distances.get(index).unwrap(),
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_beam_impl() {
        let graph = test_graph();
        let journey = graph.journey("Arad", Some("Bucharest")).unwrap();
        let distances = graph.get_haversine_6371(journey.1.unwrap());
        let a = graph
            .perform_search(walkers::Beam::new(
                &graph,
                journey,
                10,
                |i1, i2| {
                    (distances.get(i1).unwrap())
                        .partial_cmp(distances.get(i2).unwrap())
                        .unwrap()
                },
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra_walker() {
        let graph = test_graph();
        let mut a = dijkstra::new(
            &graph,
            graph.journey("Arad", Some("Neamt")).unwrap(),
            |state| *state,
            Direction::Incoming,
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
            graph.journey("Arad", Some("Neamt")).unwrap(),
            Direction::Outgoing,
        );
        let b = DepthFirst::new(
            &graph,
            graph.journey("Arad", Some("Neamt")).unwrap(),
            None::<usize>,
            Direction::Incoming,
        );

        let res = graph.bidirectional(a, b).unwrap();
        for node in res {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_a_star_impl() {
        let graph = test_graph();
        let distances = graph.get_haversine_6371(graph.name_index("Bucharest").unwrap());
        let a = graph
            .perform_search(a_star::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |index| *distances.get(index).unwrap(),
                |state| *state as f32,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_a_star_weight_impl() {
        let graph = test_graph();
        let distances = graph.get_haversine_6371(graph.name_index("Bucharest").unwrap());
        let a = graph
            .perform_search(weighted_a_star::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |index| *distances.get(index).unwrap(),
                |state| *state as f32,
                1.5,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }
}
