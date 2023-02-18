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

pub use petgraph;
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

impl<S, Ix> IntoIterator for Step<S, Ix> {
    type IntoIter = Steps<S, Ix>;
    type Item = Step<S, Ix>;
    fn into_iter(self) -> Self::IntoIter {
        Steps {
            start: Some(Rc::new(self)),
        }
    }
}

impl<S, Ix> Iterator for Steps<S, Ix> {
    type Item = Step<S, Ix>;
    fn next(&mut self) -> Option<Self::Item> {
        let head = self.start.clone();
        self.start = head.as_ref().and_then(|head| head.caller.clone());
        head.and_then(|head| Rc::try_unwrap(head).ok().and_then(|inner| Some(inner)))
    }
}

/// The library's principal Graph structure.
///
/// The struct is an abstract layer built on top of the [`petgraph::Graph<N, E, Ty, Ix>`](../petgraph/graph/struct.Graph.html)
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
    /// The map of the `I` node-name to the [`NodeIndex<Ix>`](../petgraph/graph/struct.NodeIndex.html)
    pub nodes: HashMap<I, NodeIndex<Ix>>,
}

impl<I: Copy + Eq + Hash + Debug + Display, N: Eq, E, Ty: EdgeType, Ix: IndexType>
    Graph<I, N, E, Ty, Ix>
where
    NodeIndex: From<NodeIndex<Ix>>,
    NodeIndex<Ix>: From<NodeIndex>,
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

    pub fn breadth_first(&self, inicio: I, meta: Option<I>) -> Result<Steps<(), Ix>, ()> {
        match meta {
            Some(meta) => match (self.name_index(inicio), self.name_index(meta)) {
                (Some(fidx), Some(tidx)) => self.breadth_first_impl(fidx, Some(tidx)),
                (None, None) => Err(()),
                (None, Some(_)) => Err(()),
                (Some(_), None) => Err(()),
            },
            _ => match self.name_index(inicio) {
                Some(fidx) => self.breadth_first_impl(fidx, None),
                _ => Err(()),
            },
        }
    }

    pub fn dijkstra<K, F>(
        &self,
        inicio: I,
        meta: Option<I>,
        edge_cost: F,
    ) -> Result<Steps<K, Ix>, ()>
    where
        K: Measure + Copy + Eq + Default + Ord + PartialOrd,
        F: FnMut(&E) -> K,
    {
        match meta {
            Some(meta) => match (self.name_index(inicio), self.name_index(meta)) {
                (Some(fidx), Some(tidx)) => self.dijkstra_impl(fidx, Some(tidx), edge_cost),
                (None, None) => Err(()),
                (None, Some(_)) => Err(()),
                (Some(_), None) => Err(()),
            },
            _ => match self.name_index(inicio) {
                Some(fidx) => self.dijkstra_impl(fidx, None, edge_cost),
                _ => Err(()),
            },
        }
    }

    pub fn dijkstra_impl<'a, K, F>(
        &self,
        inicio: NodeIndex<Ix>,
        meta: Option<NodeIndex<Ix>>,
        mut edge_cost: F,
    ) -> Result<Steps<K, Ix>, ()>
    where
        K: Measure + Copy + Eq + Default + Ord + PartialOrd,
        F: FnMut(&E) -> K,
    {
        let mut frontera = VecDeque::with_capacity(self.inner.node_count());
        frontera.push_front(Step {
            caller: None,
            idx: inicio,
            rel: None,
            state: K::default(),
        });

        while let Some(parent) = {
            let i = frontera
                .iter()
                .enumerate()
                .min_by(|(_, s1), (_, s2)| s1.state.cmp(&s2.state))
                .map(|(x, _)| x);
            i.map(|i| frontera.remove(i).unwrap())
        } {
            for child_idx in self
                .inner
                .neighbors_directed(parent.idx.into(), petgraph::Direction::Outgoing)
            {
                let es_meta = meta
                    .and_then(|meta| Some(meta == child_idx))
                    .unwrap_or(false);
                let tiene_hijos = self
                    .inner
                    .neighbors_directed(child_idx, petgraph::Direction::Outgoing)
                    .count()
                    != 0;
                if es_meta || tiene_hijos {
                    let step = Step {
                        caller: Some(Rc::new(Step {
                            caller: parent.caller.clone(),
                            idx: parent.idx.into(),
                            rel: parent.rel,
                            state: parent.state,
                        })),
                        idx: child_idx.into(),
                        rel: None,
                        state: parent.state
                            + edge_cost(
                                &self.inner[self.inner.find_edge(parent.idx, child_idx).unwrap()],
                            ),
                    };

                    if es_meta {
                        return Ok(step.into_iter());
                    }
                    frontera.push_front(step)
                }
            }
        }
        Err(())
    }

    pub fn breadth_first_impl(
        &self,
        inicio: NodeIndex<Ix>,
        meta: Option<NodeIndex<Ix>>,
    ) -> Result<Steps<(), Ix>, ()> {
        let mut frontera = VecDeque::with_capacity(self.inner.node_count());
        let mut visitados = self.inner.visit_map();
        frontera.push_front(Step {
            caller: None,
            idx: inicio,
            rel: None,
            state: (),
        });

        while let Some(Step {
            caller,
            idx,
            rel,
            state,
        }) = frontera.pop_front()
        {
            if meta.and_then(|meta| Some(meta == idx)).unwrap_or(false) {
                return Ok(Step {
                    caller,
                    idx: idx.into(),
                    rel,
                    state: (),
                }
                .into_iter());
            }
            self.inner
                .neighbors_directed(idx.into(), petgraph::Direction::Outgoing)
                .for_each(|child_idx| {
                    (!visitados.is_visited(&child_idx)).then(|| {
                        visitados.visit(child_idx);
                        frontera.push_back(Step {
                            caller: Some(Rc::new(Step {
                                caller: caller.clone(),
                                idx: idx.into(),
                                rel,
                                state,
                            })),
                            idx: child_idx.into(),
                            rel: None,
                            state: (),
                        });
                    });
                });
        }
        Err(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn grap() -> Graph<&'static str, (), u32> {
        let graph: Graph<&'static str, (), u32> = graph! {
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
    fn test_breadth_first() {
        let graph = grap();
        let a = graph.breadth_first("Arad", Some("Neamt"));

        assert!(a.is_ok());

        if let Ok(mut res) = a {
            while let Some(node) = res.next() {
                println!("{:#?}", graph.index_name(node.idx).unwrap());
            }
        }
    }

    #[test]
    fn test_dijkstra() {
        let graph = grap();
        let a = graph.dijkstra("Arad", Some("Neamt"), |state| *state);

        assert!(a.is_ok());

        if let Ok(mut res) = a {
            while let Some(node) = res.next() {
                println!("{:#?}", graph.index_name(node.idx).unwrap());
            }
        }
    }
}
