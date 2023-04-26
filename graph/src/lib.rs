//! Universidad Panamericana, Mexico City. Facultad de Ingeniería.
//!
//! Daniel Alejandro Osornio López (0244685@up.edu.mx),
//! Daniel Hernandez Toledo (0243179@up.edu.mx)
//!
//! A graph is a collection of nodes and edges.
//! This is a wrapper around the [`petgraph`](../petgraph/index.html) crate.
//! It provides a set of methods to manipulate the graph.
//! It also provides a set of macros to create graphs with ease.
//! The graph is generic over the following types:
//! - `I`: The type of the node identifier
//! - `N`: The type of the node data
//! - `E`: The type of the edge data
//! - `Ty`: The type of the edge
//! - `Ix`: The type of the index
//!
//! [`petgraph`](../petgraph/index.html) uses an adjacency list to represent the graph.
//!
//! The traversal algorithms are implemented in the [`walkers`](./walkers/index.html) module.
//! The *walkers* are state machines that implement the [`Walker<T>`](./walkers/trait.Walker.html) trait
//! that provides the [`step`](./walkers/trait.Walker.html#tymethod.step) method to move the walker to the next node
//! in the graph according to the algorithm.
//!
//! All solutions are represented as a singly linked list of [`Step<T, Ix>`](./step/struct.Step.html) instances.
//!
//! The [`rrand`](./rrand/index.html) module provides a set of random graph generators.
//!

#![allow(redundant_semicolons)]

use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use anyhow::{Context, Result};
use fixedbitset::FixedBitSet;
use glam::f32::Vec2;
pub use petgraph;
use petgraph::stable_graph::DefaultIx;
pub use petgraph::stable_graph::{EdgeIndex, IndexType, NodeIndex, StableGraph as PGraph};
use petgraph::visit::{VisitMap, Visitable};
pub use petgraph::Direction;
pub use petgraph::{Directed, EdgeType};
use unicase::Ascii;

pub use step::*;
use walkers::{Walker, WalkerState};

/// Random number generator based on the [`getrandom`](../getrandom/index.html) crate.
/// You may want to take a look at:
/// - The random number generator (rng) instantiator [`rrand::get_rng`](./fn.get_rng.html)
pub mod rrand;

/// Implementation of the [`Step`](./step/struct.Step.html) struct for traversal representation.
pub mod step;

/// Implementations of the [`Walker<T>`](./walkers/trait.Walker.html) trait
/// for various traversal algorithms.
///
/// You may want to take a look at:
/// - The [`Walker<Ix>`](./trait.Walker.html) trait
/// - The [`WalkerState`](./enum.WalkerState.html) enumerator
/// - Each of the strategies implementation their corresponding modules.
pub mod walkers;

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
/// let graph: Graph<&str, (), u8, graph::Directed, u32> = graph! {
///     with_node: (),
///     with_edges: unchecked_next,
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
        let mut g = Graph::<_, _, _, _, _>::with_capacity(count!($($node)*), count!($($({$from;$($to)+})+)*) );
        $(g.unchecked_register($ident, $node));*;
        $(
            $($(g.$type($from, $to,$cost));+);+
        );*;
        g
    }};
}

/// A node in the graph with a name and a location.
///
/// The name is used to identify the node and the location is used
/// to calculate the distance between nodes and to draw the graph in a 2D space.
///
/// This struct is used by the [`Graph`](./struct.Graph.html) struct to store the nodes
/// and their names within the repr field.
#[derive(Clone, PartialEq)]
pub struct Node {
    /// The name of the node
    pub name: String,
    /// The location of the node in a 2D space
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
pub struct Graph<I, N, E, Ty, Ix = DefaultIx> {
    /// The inner [`petgraph::Graph<N, E, Ty, Ix>`](../petgraph/graph/struct.Graph.html)
    pub inner: PGraph<N, E, Ty, Ix>,
    pub repr: PGraph<Node, (), Ty, Ix>,
    /// The map of the `I` node-name to the [`NodeIndex<Ix>`](../petgraph/graph/struct.NodeIndex.html)
    pub nodes: HashMap<Ascii<I>, NodeIndex<Ix>>,
    pub repr_nodes: HashMap<Ascii<I>, NodeIndex<Ix>>,
}

/// A trait to extract the coordinates of a node.
pub trait Coords {
    /// Get the x coordinate of the node
    fn get_x(&self) -> f32;
    /// Get the y coordinate of the node
    fn get_y(&self) -> f32;
}

/// Implementation block for distance methods for Graphs where its type N has the Coords trait.
/// The trait Coords is necessary to extract the node coordinates from any given generic N type.
impl<I, N, E, Ty, Ix> Graph<I, N, E, Ty, Ix>
where
    N: Coords,
    Ty: EdgeType,
    Ix: IndexType,
{
    /// Get the distance to all nodes from a given node `idx` with the Haversine formula using the Earth radius as 6371 km
    pub fn get_haversine_table_6371(&self, to: NodeIndex<Ix>) -> HashMap<NodeIndex<Ix>, f32> {
        self.get_haversine_table(to, 6371.)
    }

    /// Get the distance between two nodes with the Haversine formula using the Earth radius as 6371 km
    pub fn get_haversine_6371(&self, from: NodeIndex<Ix>, to: NodeIndex<Ix>) -> f32 {
        self.get_haversine(from, to, 6371.)
    }

    /// Haversine distance between two nodes using a given radius `r` as the Earth radius
    ///
    /// Formula from <https://en.wikipedia.org/wiki/Haversine_formula#Formulation>
    pub fn get_haversine(&self, from: NodeIndex<Ix>, to: NodeIndex<Ix>, r: f32) -> f32 {
        use std::f32::consts::PI;

        // Get the coordinates of the origin node
        let w_original = self.inner.node_weight(from).unwrap();
        let (x1, y1) = (
            w_original.get_x() * PI / 180.,
            w_original.get_y() * PI / 180.,
        );

        // Get the coordinates of the destination node
        let node = self.inner.node_weight(to).unwrap();
        let (x2, y2) = (node.get_x() * PI / 180., node.get_y() * PI / 180.);

        // Calculate the distance between the two nodes

        2. * r * {
            {
                ((x2 - x1) / 2.).sin().powi(2)
                    + x1.cos() * x2.cos() * ((y2 - y1) / 2.).sin().powi(2)
            }
            .sqrt()
        }
        .asin()
    }

    /// Get the distance to all nodes in the graph using the Haversine formula.
    ///
    /// The returned HashMap has the node index as key and the distance as value.
    pub fn get_haversine_table(&self, to: NodeIndex<Ix>, r: f32) -> HashMap<NodeIndex<Ix>, f32> {
        self.inner
            .node_indices()
            .map(|idx| (idx, self.get_haversine(idx, to, r)))
            .collect::<HashMap<_, _>>()
    }
}

impl<I, N, E, Ty, Ix> Default for Graph<I, N, E, Ty, Ix>
where
    Ix: IndexType,
    Ty: EdgeType,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<I, N, E, Ty, Ix> Graph<I, N, E, Ty, Ix>
where
    Ty: EdgeType,
    Ix: IndexType,
{
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
            inner: PGraph::<N, E, Ty, Ix>::with_capacity(nodes, edges),
            nodes: HashMap::with_capacity(nodes),
            repr_nodes: HashMap::with_capacity(nodes),
            repr: PGraph::<Node, (), Ty, Ix>::with_capacity(nodes, edges),
        }
    }

    /// Get the [`EdgeIndex<Ix>`](../petgraph/graph/struct.EdgeIndex.html) of the edge between two nodes in any direction.
    ///
    /// Panics if there is no edge between the nodes.
    pub fn edge_between_unchecked(
        &self,
        source: NodeIndex<Ix>,
        target: NodeIndex<Ix>,
    ) -> EdgeIndex<Ix> {
        self.edge_between(source, target).unwrap_or_else(|| {
            panic!(
                "No edge between {:?} and {:?} in graph",
                source.index(),
                target.index()
            )
        })
    }

    /// Gets the edge between two nodes in any direction.
    ///
    /// If there is more than one edge only the first one is taken.
    /// Return None if there is no edge connecting two nodes.
    pub fn edge_between(
        &self,
        source: NodeIndex<Ix>,
        target: NodeIndex<Ix>,
    ) -> Option<EdgeIndex<Ix>> {
        use petgraph::visit::EdgeRef;
        let a = self.inner.edges_connecting(source, target).next();
        let b = self.inner.edges_connecting(target, source).next();
        a.map(|a| a.id()).or_else(|| b.map(|b| b.id()))
    }

    /// Returns the number of nodes in the graph.
    pub fn node_count(&self) -> usize {
        self.inner.node_count()
    }

    /// Returns the number of edges in the graph.
    pub fn edge_count(&self) -> usize {
        self.inner.edge_count()
    }

    /// Create a bit set for registering which nodes have been visited.
    pub fn visit_map(&self) -> FixedBitSet {
        self.inner.visit_map()
    }
}

impl<I, N, E, Ty, Ix> Graph<I, N, E, Ty, Ix>
where
    I: Copy + Hash + Eq + Debug + Display,
    Ty: EdgeType,
    Ix: IndexType,
    Ascii<I>: Copy + Hash + Eq,
    NodeIndex: From<NodeIndex<Ix>>,
    EdgeIndex: From<EdgeIndex<Ix>>,
{
    /// Get the high-level node name from the low-level node index. E.g. NodeIndex(0) -> "Arad"
    pub fn index_name(&self, value: NodeIndex<Ix>) -> Option<I> {
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
    /// and the bound `E` value.
    pub fn next(&mut self, from: I, to: I, edge: E) -> Result<(), String>
    where
        I: Debug,
    {
        match (self.name_index(from), self.name_index(to)) {
            (Some(fidx), Some(tidx)) => {
                self.inner.add_edge(fidx, tidx, edge);
                self.repr.add_edge(
                    *self.repr_nodes.get(&Ascii::new(from)).unwrap(),
                    *self.repr_nodes.get(&Ascii::new(to)).unwrap(),
                    (),
                );
                Ok(())
            }
            (None, None) => Err(format!("Los nodos {:?} y {:?} no existen", from, to)),
            (None, Some(_)) => Err(format!("El nodo {:?} no existe", from)),
            (Some(_), None) => Err(format!("El nodo {:?} no existe", to)),
        }
    }

    /// Connect two nodes by their high-level names. E.g. "Arad" -> "Neamt"
    ///
    /// Panics if any of the nodes doesn't exist
    pub fn unchecked_next(&mut self, from: I, to: I, edge: E)
    where
        I: Debug,
    {
        if let Err(err) = self.next(from, to, edge) {
            panic!("{}", err);
        }
    }

    /// Register a node with a given name and stored `N` value
    ///
    /// The method calls the necessary low-level methods to connect both node indexes
    /// within the inner graph.
    ///
    /// Adds values to the inner graph's `Vec<Node<N, Ix>>` to represent neighbors between nodes
    /// and the bound `N` value
    pub fn register(&mut self, ident: I, node: N) -> Result<(), String>
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
            Err(format!("El nodo {:?} ya existe", ident))
        }
    }

    /// Register a node with a given name and stored `N` value
    ///
    /// Panics if the node already exists
    pub fn unchecked_register(&mut self, ident: I, node: N)
    where
        I: Debug,
    {
        if let Err(err) = self.register(ident, node) {
            panic!("{}", err);
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

    /// Translates start and end node names to node indexes.
    ///
    /// Returns an error if any of the two node identifiers is missing in the graph. (does not exist)
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

    /// Pergorm any shearch stratey easily
    ///
    /// This is just a loop wrapped on any search strategy, that is any type that
    /// implements the [`Walker<T>`](./walkers/trait.Walker.html) trait.
    ///
    /// The loop breaks either when a solution is found or when there are no more edges to
    /// explore.
    pub fn perform_search(
        &self,
        mut machine: impl Walker<Ix>,
    ) -> Result<Step<f32, Ix>, WalkerState<Ix>> {
        let mut res = machine.step();

        while !matches!(res, WalkerState::Done) {
            if let WalkerState::Found(step) = res {
                return Ok(step);
            }
            res = machine.step();
        }
        Err(res)
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

/// Implement the Coords trait for the unit type.
///
/// Since there's no data 0. is returned
impl Coords for () {
    fn get_x(&self) -> f32 {
        0.
    }
    fn get_y(&self) -> f32 {
        0.
    }
}

#[allow(dead_code)]
pub fn unit_graph() -> Graph<&'static str, (f32, f32), f32, Directed> {
    let mut graph = graph! {
        with_edges: unchecked_next,
        nodes: [
            "Acapulco" => ( -99.823_654, 16.853_11 ),
            "Chilpancingo" => ( -99.500_63, 17.551_535 ),
            "Acayucan" => ( -94.914_734, 17.949_238 ),
            "Tehuantepec" => ( -95.242_33, 16.322_699 ),
            "Tuxtla" => ( -93.103_12, 16.751_575 ),
            "Villa Hermosa" => ( -92.947_525, 17.989_445 ),
            "Agua Prieta" => ( -109.548_96, 31.327_774 ),
            "Santa Ana" => ( -111.119_62, 30.539_833 ),
            "Aguascalientes" => ( -102.291_565, 21.885_256 ),
            "Guadalajara" => ( -103.349_61, 20.659_698 ),
            "Guanajuato" => ( -101.257_36, 21.019_014 ),
            "Alvarado" => ( -95.758_95, 18.769_619 ),
            "Oaxaca" => ( -96.726_585, 17.073_185 ),
            "Atlacomulco" => ( -99.876_686, 19.797_558 ),
            "Queretaro" => ( -100.389_885, 20.588_793 ),
            "Cancun" => ( -86.851_54, 21.161_907 ),
            "Valladolid" => ( -88.202_25, 20.68964 ),
            "Chetumal" => ( -88.296_14, 18.500_189 ),
            "Campeche" => ( -90.534_91, 19.830_126 ),
            "Felipe Carrillo Puerto" => ( -88.044_1, 19.580_334 ),
            "Merida" => ( -89.592_58, 20.967_371 ),
            "Chihuahua" => ( -106.069_1, 28.632_996 ),
            "Janos" => ( -108.192_41, 30.888_933 ),
            "Juarez" => ( -106.424_545, 31.690_363 ),
            "Ojinaga" => ( -104.408_295, 29.545_885 ),
            "Iguala" => ( -99.539_734, 18.344_849 ),
            "Ciudad Altamirano" => ( -100.668_625, 18.357_815 ),
            "Cuernavaca" => ( -99.221_565, 18.924_21 ),
            "Toluca de Lerdo" => ( -99.655_66, 19.282_61 ),
            "Zihuatanejo" => ( -101.551_7, 17.641_67 ),
            "Ciudad del Carmen" => ( -91.807_46, 18.650_488 ),
            "Ciudad Obregon" => ( -109.930_37, 27.482_773 ),
            "Guaymas" => ( -110.908_936, 27.917_866 ),
            "Ciudad Victoria" => ( -99.141_11, 23.736_916 ),
            "Matamoros" => ( -97.502_74, 25.869_03 ),
            "Soto la Marina" => ( -98.207_63, 23.768_019 ),
            "Tampico" => ( -97.861_1, 22.233_105 ),
            "Colima" => ( -103.724_08, 19.245_234 ),
            "Morelia" => ( -101.194_984, 19.705_95 ),
            "Playa Azul" => ( -102.350_47, 17.982_021 ),
            "Cordoba" => ( -96.923_775, 18.883_89 ),
            "Veracruz" => ( -96.134_224, 19.173773 ),
            "Culiacan" => ( -107.394_01, 24.809_065 ),
            "Hidalgo del Parral" => ( -105.666_62, 26.931_784 ),
            "Topolobampo" => ( -109.050_37, 25.600_693 ),
            "Durango" => ( -104.653_175, 24.027_72 ),
            "Mazatlan" => ( -106.411_14, 23.249_414 ),
            "Torreon" => ( -103.406_784, 25.542_845 ),
            "Ensenada" => ( -116.596_375, 31.866_743 ),
            "San Quintin" => ( -115.937_93, 30.560_877 ),
            "Francisco Escarcega" => ( -90.739_02, 18.610_184 ),
            "Manzanillo" => ( -104.338_46, 19.113_81 ),
            "Salamanca" => ( -101.195_72, 20.573_93 ),
            "Tepic" => ( -104.894_67, 21.504_143 ),
            "Hermosillo" => ( -110.955_92, 29.072_968 ),
            "San Luis Potosi" => ( -100.985_54, 22.156_47 ),
            "Izucar de Matamoros" => ( -98.467_79, 18.599_125 ),
            "La Paz" => ( -110.312_75, 24.142_641 ),
            "Cabo San Lucas" => ( -109.916_74, 22.890_533 ),
            "Reynosa" => ( -98.297_9, 26.050_84 ),
            "Mexicalli" => ( -115.452_26, 32.624_54 ),
            "San Felipe" => ( -114.840_775, 31.025_07 ),
            "Tijuana" => ( -117.038_246, 32.514_946 ),
            "Ciudad de Mexico" => ( -99.133_21, 19.432_608 ),
            "Pachuca de Soto" => ( -98.759_13, 20.101_06 ),
            "Puebla" => ( -98.206_276, 19.041_44 ),
            "Tlaxcala" => ( -98.237_58, 19.318_163 ),
            "Monclova" => ( -101.421_52, 26.908_026 ),
            "Piedras Negras" => ( -100.540_86, 28.691_618 ),
            "Monterrey" => ( -100.316_12, 25.686_615 ),
            "Nuevo Laredo" => ( -99.549_57, 27.477_917 ),
            "Puerto Angel" => ( -96.491_31, 15.668_008 ),
            "Tehuacan" => ( -97.400_375, 18.466_497 ),
            "Tuxpan de Rodriguez Cano" => ( -97.406_334, 20.956_116 ),
            "Pinotepa Nacional" => ( -98.053_69, 16.341_183 ),
            "Zacatecas" => ( -102.583_25, 22.770_924 ),
            "Santa Rosalia" => ( -112.270_15, 27.336_193 ),
            "Santo Domingo" => ( -111.988_84, 25.348_732 )
        ],
        connections: []
    };
    graph.done_register();
    graph
}

#[allow(dead_code)]
pub fn full_connected_graph() -> Graph<&'static str, (f32, f32), f32, Directed> {
    let mut graph = unit_graph();
    for index in graph.inner.node_indices().collect::<Vec<_>>() {
        for j in graph.inner.node_indices().collect::<Vec<_>>() {
            let distance = graph.get_haversine_6371(index, j);

            graph
                .next(
                    graph.index_name(index).unwrap(),
                    graph.index_name(j).unwrap(),
                    distance,
                )
                .unwrap();
        }
    }
    graph.done_register();
    graph
}

#[allow(dead_code)]
pub fn test_graph() -> Graph<&'static str, (f32, f32), f32, Directed> {
    let mut g = graph! {
        with_edges: unchecked_next,
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
            "Arad" => {(140.) "Sibiu", (75.) "Zerind", (118.) "Timisoara"},
            "Zerind" => {(71.) "Oradea"},
            "Oradea" => {(151.) "Sibiu"},
            "Sibiu" => {(99.) "Fagaras", (80.) "Rimnieu Vilcea"},
            "Timisoara" => {(111.) "Lugoj"},
            "Lugoj" => {(70.) "Mehadia"},
            "Mehadia" => {(75.) "Drobeta"},
            "Drobeta" => {(120.) "Craiova"},
            "Craiova" => {(138.) "Pitesti"},
            "Pitesti" => {(101.) "Bucharest"},
            "Rimnieu Vilcea" => {(97.) "Pitesti", (146.) "Craiova"},
            "Fagaras" => {(211.) "Bucharest"},
            "Bucharest" => {(90.) "Giurgiu", (85.) "Urziceni"},
            "Urziceni" => {(98.) "Hirsova", (142.) "Vasiui"},
            "Vasiui" => {(92.) "Iasi"},
            "Iasi" => {(87.) "Neamt"},
            "Hirsova" => {(86.) "Eforie"}
        ]
    };
    g.done_register();
    g
}

pub fn mexico_graph() -> Graph<&'static str, (f32, f32), f32, Directed> {
    let mut graph = graph! {
        with_edges: unchecked_next,
        nodes: [
            "Acapulco" => ( -99.823_654, 16.853_11 ),
            "Chilpancingo" => ( -99.500_63, 17.551_535 ),
            "Acayucan" => ( -94.914_734, 17.949_238 ),
            "Tehuantepec" => ( -95.242_33, 16.322_699 ),
            "Tuxtla" => ( -93.103_12, 16.751_575 ),
            "Villa Hermosa" => ( -92.947_525, 17.989_445 ),
            "Agua Prieta" => ( -109.548_96, 31.327_774 ),
            "Santa Ana" => ( -111.119_62, 30.539_833 ),
            "Aguascalientes" => ( -102.291_565, 21.885_256 ),
            "Guadalajara" => ( -103.349_61, 20.659_698 ),
            "Guanajuato" => ( -101.257_36, 21.019_014 ),
            "Alvarado" => ( -95.758_95, 18.769_619 ),
            "Oaxaca" => ( -96.726_585, 17.073_185 ),
            "Atlacomulco" => ( -99.876_686, 19.797_558 ),
            "Queretaro" => ( -100.389_885, 20.588_793 ),
            "Cancun" => ( -86.851_54, 21.161_907 ),
            "Valladolid" => ( -88.202_25, 20.68964 ),
            "Chetumal" => ( -88.296_14, 18.500_189 ),
            "Campeche" => ( -90.534_91, 19.830_126 ),
            "Felipe Carrillo Puerto" => ( -88.044_1, 19.580_334 ),
            "Merida" => ( -89.592_58, 20.967_371 ),
            "Chihuahua" => ( -106.069_1, 28.632_996 ),
            "Janos" => ( -108.192_41, 30.888_933 ),
            "Juarez" => ( -106.424_545, 31.690_363 ),
            "Ojinaga" => ( -104.408_295, 29.545_885 ),
            "Iguala" => ( -99.539_734, 18.344_849 ),
            "Ciudad Altamirano" => ( -100.668_625, 18.357_815 ),
            "Cuernavaca" => ( -99.221_565, 18.924_21 ),
            "Toluca de Lerdo" => ( -99.655_66, 19.282_61 ),
            "Zihuatanejo" => ( -101.551_7, 17.641_67 ),
            "Ciudad del Carmen" => ( -91.807_46, 18.650_488 ),
            "Ciudad Obregon" => ( -109.930_37, 27.482_773 ),
            "Guaymas" => ( -110.908_936, 27.917_866 ),
            "Ciudad Victoria" => ( -99.141_11, 23.736_916 ),
            "Matamoros" => ( -97.502_74, 25.869_03 ),
            "Soto la Marina" => ( -98.207_63, 23.768_019 ),
            "Tampico" => ( -97.861_1, 22.233_105 ),
            "Colima" => ( -103.724_08, 19.245_234 ),
            "Morelia" => ( -101.194_984, 19.705_95 ),
            "Playa Azul" => ( -102.350_47, 17.982_021 ),
            "Cordoba" => ( -96.923_775, 18.883_89 ),
            "Veracruz" => ( -96.134_224, 19.173773 ),
            "Culiacan" => ( -107.394_01, 24.809_065 ),
            "Hidalgo del Parral" => ( -105.666_62, 26.931_784 ),
            "Topolobampo" => ( -109.050_37, 25.600_693 ),
            "Durango" => ( -104.653_175, 24.027_72 ),
            "Mazatlan" => ( -106.411_14, 23.249_414 ),
            "Torreon" => ( -103.406_784, 25.542_845 ),
            "Ensenada" => ( -116.596_375, 31.866_743 ),
            "San Quintin" => ( -115.937_93, 30.560_877 ),
            "Francisco Escarcega" => ( -90.739_02, 18.610_184 ),
            "Manzanillo" => ( -104.338_46, 19.113_81 ),
            "Salamanca" => ( -101.195_72, 20.573_93 ),
            "Tepic" => ( -104.894_67, 21.504_143 ),
            "Hermosillo" => ( -110.955_92, 29.072_968 ),
            "San Luis Potosi" => ( -100.985_54, 22.156_47 ),
            "Izucar de Matamoros" => ( -98.467_79, 18.599_125 ),
            "La Paz" => ( -110.312_75, 24.142_641 ),
            "Cabo San Lucas" => ( -109.916_74, 22.890_533 ),
            "Reynosa" => ( -98.297_9, 26.050_84 ),
            "Mexicalli" => ( -115.452_26, 32.624_54 ),
            "San Felipe" => ( -114.840_775, 31.025_07 ),
            "Tijuana" => ( -117.038_246, 32.514_946 ),
            "Ciudad de Mexico" => ( -99.133_21, 19.432_608 ),
            "Pachuca de Soto" => ( -98.759_13, 20.101_06 ),
            "Puebla" => ( -98.206_276, 19.041_44 ),
            "Tlaxcala" => ( -98.237_58, 19.318_163 ),
            "Monclova" => ( -101.421_52, 26.908_026 ),
            "Piedras Negras" => ( -100.540_86, 28.691_618 ),
            "Monterrey" => ( -100.316_12, 25.686_615 ),
            "Nuevo Laredo" => ( -99.549_57, 27.477_917 ),
            "Puerto Angel" => ( -96.491_31, 15.668_008 ),
            "Tehuacan" => ( -97.400_375, 18.466_497 ),
            "Tuxpan de Rodriguez Cano" => ( -97.406_334, 20.956_116 ),
            "Pinotepa Nacional" => ( -98.053_69, 16.341_183 ),
            "Zacatecas" => ( -102.583_25, 22.770_924 ),
            "Santa Rosalia" => ( -112.270_15, 27.336_193 ),
            "Santo Domingo" => ( -111.988_84, 25.348_732 )
        ],
        connections: [
            "Cancun" => {(90.) "Valladolid", (100.) "Felipe Carrillo Puerto"},
            "Valladolid" => {(90.) "Felipe Carrillo Puerto"},
            "Felipe Carrillo Puerto" => {(60.) "Campeche"},
            "Campeche" => {(90.) "Merida", (100.) "Chetumal", (90.) "Ciudad del Carmen"},
            "Chetumal" => {(111.) "Francisco Escarcega"},
            "Ciudad del Carmen" => {(90.) "Villa Hermosa", (90.) "Tuxtla"},
            "Villa Hermosa" => {(90.) "Acayucan"},
            "Tuxtla" => {(90.) "Acayucan"},
            "Acayucan" => {(80.) "Tehuantepec", (110.) "Alvarado"},
            "Alvarado" => {(100.) "Oaxaca"},
            "Oaxaca" => {(80.) "Tehuacan", (90.) "Puerto Angel", (90.) "Izucar de Matamoros"},
            "Puerto Angel" => {(100.) "Pinotepa Nacional" },
            "Izucar de Matamoros" => {(90.) "Puebla", (100.) "Cuernavaca"},
            "Pinotepa Nacional" => {(100.) "Acapulco"},
            "Cuernavaca" => {(100.) "Ciudad de Mexico", (100.) "Ciudad Altamirano"},
            "Puebla" => {(90.) "Ciudad de Mexico", (80.) "Cordoba"},
            "Acapulco" => {(140.) "Chilpancingo"},
            "Ciudad de Mexico" => {(100.) "Tlaxcala", (110.) "Toluca de Lerdo", (90.) "Queretaro", (100.) "Pachuca de Soto"},
            "Ciudad Altamirano" => {(90.) "Zihuatanejo"},
            "Cordoba" => {(90.) "Veracruz"},
            "Chilpancingo" => {(90.) "Iguala"},
            "Toluca de Lerdo" => {(100.) "Ciudad Altamirano"},
            "Queretaro" => {(90.) "Atlacomulco", (90.) "Salamanca", (90.) "San Luis Potosi"},
            "Pachuca de Soto" => {(110.) "Tuxpan de Rodriguez Cano"},
            "Zihuatanejo" => {(90.) "Playa Azul"},
            "Iguala" => {(100.) "Cuernavaca", (110.) "Ciudad Altamirano"},
            "Salamanca" => {(90.) "Guanajuato", (90.) "Guadalajara"},
            "San Luis Potosi" => {(90.) "Zacatecas", (70.) "Durango", (100.) "Aguascalientes" },
            "Tuxpan de Rodriguez Cano" => {(100.) "Tampico"},
            "Playa Azul" => {(100.) "Morelia", (100.) "Colima", (100.) "Manzanillo"},
            "Guanajuato" => {(80.) "Aguascalientes"},
            "Guadalajara" => {(110.) "Tepic"},
            "Aguascalientes" =>{(70.) "Guadalajara"},
            "Durango" => {(90.) "Hidalgo del Parral", (90.) "Mazatlan"},
            "Tampico" => {(80.) "Ciudad Victoria"},
            "Morelia" => {(90.) "Salamanca"},
            "Manzanillo" => {(50.) "Colima", (80.) "Guadalajara"},
            "Colima" => {(90.) "Morelia", (50.) "Guadalajara"},
            "Tepic" =>{(50.) "Mazatlan"},
            "Hidalgo del Parral" => {(130.) "Chihuahua", (110.) "Topolobampo", (80.) "Culiacan"},
            "Mazatlan" => {(90.) "Culiacan"},
            "Ciudad Victoria" => {(80.) "Soto la Marina", (80.) "Matamoros", (80.) "Monterrey", (80.) "Durango"},
            "Chihuahua" => {(90.) "Juarez", (90.) "Janos"},
            "Topolobampo" => {(90.) "Ciudad Obregon"},
            "Culiacan" => {(110.) "Topolobampo"},
            "Matamoros" => {(90.) "Reynosa"},
            "Monterrey" => {(110.) "Nuevo Laredo",(70.) "Monclova"},
            "Janos" => {(110.) "Agua Prieta"},
            "Ciudad Obregon" => {(80.) "Guaymas"},
            "Reynosa" => {(100.) "Nuevo Laredo"},
            "Nuevo Laredo" => {(100.) "Piedras Negras"},
            "Monclova" => {(100.) "Torreon", (90.) "Ojinaga"},
            "Agua Prieta" => {(90.) "Santa Ana"},
            "Guaymas" => {(90.) "Hermosillo"},
            "Piedras Negras" => {(90.) "Monclova"},
            "Torreon" => {(90.) "Durango"},
            "Ojinaga" => {(90.) "Chihuahua"},
            "Santa Ana" => {(159.) "Mexicalli"},
            "Hermosillo" => {(100.) "Santa Ana"},
            "Mexicalli" => {(50.) "Tijuana", (70.) "San Felipe"},
            "Tijuana" => {(30.) "Ensenada"},
            "San Felipe" => {(50.) "Ensenada"},
            "Ensenada" => {(90.) "San Quintin"},
            "San Quintin" => {(140.) "Santa Rosalia"},
            "Santa Rosalia" => {(100.) "Santo Domingo"},
            "Santo Domingo" => {(100.) "La Paz"},
            "La Paz" => {(40.) "Cabo San Lucas"}
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
            .perform_search(walkers::DepthFirst::new(
                &graph,
                graph.journey("Arad", Some("Neamt")).unwrap(),
                None,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_breadth() {
        let graph = test_graph();
        let a = graph
            .perform_search(BreadthFirst::new(
                &graph,
                graph.journey("Arad", Some("Neamt")).unwrap(),
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a.iter() {
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
                        break Some(result);
                    }
                    WalkerState::Done => {
                        break None;
                    }
                    _ => {}
                }
            }
        }
        .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_distances() {
        let graph = test_graph();
        for (k, v) in graph
            .get_haversine_table_6371(graph.name_index("Bucharest").unwrap())
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
                |edge| *edge as f32,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_greedy_best_first() {
        let graph = test_graph();
        let distances = graph.get_haversine_table_6371(graph.name_index("Bucharest").unwrap());
        let a = graph
            .perform_search(greedy::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |index| *distances.get(index).unwrap(),
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_beam_impl() {
        let graph = test_graph();
        let journey = graph.journey("Arad", Some("Bucharest")).unwrap();
        let distances = graph.get_haversine_table_6371(journey.1.unwrap());
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

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_dijkstra_walker() {
        let graph = test_graph();
        let mut a = dijkstra::new(
            &graph,
            graph.journey("Neamt", Some("Arad")).unwrap(),
            |state| *state as f32,
            Direction::Incoming,
        );

        let a = {
            loop {
                match a.step() {
                    WalkerState::Found(result) => {
                        break Some(result);
                    }
                    WalkerState::Done => {
                        break None;
                    }
                    _ => {}
                }
            }
        }
        .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_a_star_impl() {
        let graph = test_graph();
        let distances = graph.get_haversine_table_6371(graph.name_index("Bucharest").unwrap());
        let a = graph
            .perform_search(a_star::new(
                &graph,
                graph.journey("Arad", Some("Bucharest")).unwrap(),
                |index| *distances.get(index).unwrap(),
                |state| *state as f32,
                Direction::Outgoing,
            ))
            .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_a_star_weight_impl() {
        let graph = test_graph();
        let distances = graph.get_haversine_table_6371(graph.name_index("Bucharest").unwrap());
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

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_simann() {
        let graph = full_connected_graph();
        let a = graph
            .perform_search(SimAnnealing::new(
                &graph,
                graph.journey("Cancun", Some("Cabo san lucas")).unwrap(),
                50.,
            ))
            .unwrap();

        for node in a.iter() {
            println!("{:#?}", graph.index_name(node.idx).unwrap());
        }
    }

    #[test]
    fn test_bidirectional_dijkstra() {
        let graph = test_graph();
        let journey = graph.journey("Arad", Some("Bucharest")).unwrap();
        let journey_rev = graph.journey("Bucharest", Some("Arad")).unwrap();
        let res = graph
            .perform_search(Bidirectional::new(
                &graph,
                dijkstra::new(&graph, journey, |edge| *edge as f32, Direction::Outgoing),
                dijkstra::new(
                    &graph,
                    journey_rev,
                    |edge| *edge as f32,
                    Direction::Incoming,
                ),
            ))
            .unwrap();

        let dijkstra_res = graph
            .perform_search(dijkstra::new(
                &graph,
                journey,
                |edge| *edge as f32,
                Direction::Outgoing,
            ))
            .unwrap();

        assert_eq!(res.collect_nodes(), dijkstra_res.collect_nodes());
    }

    #[test]
    fn test_bidirectional_dijkstra2() {
        let graph = test_graph();
        let journey = graph.journey("Arad", Some("Neamt")).unwrap();
        let journey_rev = graph.journey("Neamt", Some("Arad")).unwrap();
        let res = graph
            .perform_search(Bidirectional::new(
                &graph,
                dijkstra::new(&graph, journey, |edge| *edge as f32, Direction::Outgoing),
                dijkstra::new(
                    &graph,
                    journey_rev,
                    |edge| *edge as f32,
                    Direction::Incoming,
                ),
            ))
            .unwrap();

        let dijkstra_res = graph
            .perform_search(dijkstra::new(
                &graph,
                journey,
                |edge| *edge as f32,
                Direction::Outgoing,
            ))
            .unwrap();

        assert_eq!(res.collect_nodes(), dijkstra_res.collect_nodes());
    }
}
