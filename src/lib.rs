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
    fmt::{Debug, Display},
    hash::Hash,
    ops::Add,
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

pub enum WalkerState<S, Ix> {
    Done,
    Found(Step<S, Ix>),
    NotFound(Rc<Step<S, Ix>>),
    Cutoff,
}

trait Walker<S, Ix> {
    fn step(&mut self) -> WalkerState<S, Ix>;
}

struct BreadthFirst<'a, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<(), Ix>>,
    visited: FixedBitSet,
    direction: Direction,
}

struct Dijkstra<'a, F, K, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<K, Ix>>,
    direction: Direction,
    edge_cost: F,
}

pub struct Depth<'a, D, I, N, E, Ty, Ix> {
    goal: Option<NodeIndex<Ix>>,
    graph: &'a Graph<I, N, E, Ty, Ix>,
    border: VecDeque<Step<D, Ix>>,
    limit: Option<D>,
    cutoff: bool,
    level: D,
    direction: Direction,
}

impl<'a, D, I, N, E, Ty: EdgeType, Ix: IndexType> Depth<'a, D, I, N, E, Ty, Ix>
where
    D: Copy + Eq + Default + Ord + PartialOrd + One + Add<Output = D> + Zero,
{
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

impl<'a, D, I, N, E, Ty: EdgeType, Ix: IndexType> Walker<D, Ix> for Depth<'a, D, I, N, E, Ty, Ix>
where
    D: Copy + Eq + Default + Ord + PartialOrd + One + Add<Output = D> + Zero,
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
                return WalkerState::Cutoff;
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
                    caller: parent.caller.clone(),
                    idx: child,
                    rel: None,
                    state: self.level,
                })
            }
            WalkerState::NotFound(parent)
        } else {
            if self.cutoff {
                WalkerState::Cutoff
            } else {
                WalkerState::Done
            }
        }
    }
}

impl<'a, F, K: Default, I, N, E, Ty: EdgeType, Ix: IndexType> Dijkstra<'a, F, K, I, N, E, Ty, Ix>
where
    K: Measure + Copy + Eq + Default + Ord + PartialOrd,
    F: FnMut(&E) -> K,
{
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
                    state: K::default(),
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
    K: Measure + Copy + Eq + Default + Ord + PartialOrd,
    F: FnMut(&E) -> K,
    EdgeIndex: From<EdgeIndex<Ix>>,
    EdgeIndex<Ix>: From<EdgeIndex>,
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

impl<I, N, E, Ty, Ix> Graph<I, N, E, Ty, Ix>
where
    N: Eq,
    Ty: EdgeType,
    Ix: IndexType,
    I: Copy + Hash + Eq + Debug + Display,
    NodeIndex: From<NodeIndex<Ix>>,
    NodeIndex<Ix>: From<NodeIndex>,
    EdgeIndex: From<EdgeIndex<Ix>>,
    EdgeIndex<Ix>: From<EdgeIndex>,
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
    pub fn next(&mut self, from: I, to: I, edge: E) -> Result<(), ()> {
        match (self.name_index(from), self.name_index(to)) {
            (Some(fidx), Some(tidx)) => {
                self.inner.add_edge(fidx.into(), tidx.into(), edge);
                Ok(())
            }
            (None, None) => Err(()),
            (None, Some(_)) => Err(()),
            (Some(_), None) => Err(()),
        }
    }

    /// Register a node with a given name and stored `N` value
    ///
    /// The method calls the necessary low-level methods to connect both node indexes
    /// within the inner graph.
    ///
    /// Adds values to the inner graph's `Vec<Node<N, Ix>>` to represent neighbors between nodes
    /// and the binded `N` value
    pub fn register(&mut self, ident: I, node: N) -> Result<(), ()> {
        if self.nodes.contains_key(&ident) {
            Err(())
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
        D: Copy + Eq + Default + Ord + PartialOrd + One + Add<Output = D> + Zero,
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
        D: Copy + Eq + Default + Ord + PartialOrd + One + Add<Output = D> + Zero,
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
        D: Copy + Eq + Default + Ord + PartialOrd + One + Add<Output = D> + Zero,
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
                    caller: parent.caller.clone(),
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
        let graph = grap();
        let a = graph
            .depth_first::<u32>("Arad", Some("Fagaras"), Some(3))
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth() {
        let graph = grap();
        let a = graph.breadth_first("Arad", Some("Neamt")).unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth_walker() {
        let graph = grap();
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
    fn test_dijkstra() {
        let graph = grap();
        let a = graph
            .dijkstra("Arad", Some("Neamt"), |state| *state)
            .unwrap();

        for node in a {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra_walker() {
        let graph = grap();
        let mut a = Dijkstra::new(
            &graph,
            graph.name_index("Neamt").unwrap(),
            Some(graph.name_index("Arad").unwrap()),
            Direction::Incoming,
            |state| *state,
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
}
