mod graph {
    pub use petgraph;
    use petgraph::{
        algo::dijkstra,
        graph::{EdgeIndex, NodeIndex},
        stable_graph::IndexType,
        visit::{VisitMap, Visitable},
        *,
    };
    use std::{collections::VecDeque, rc::Rc};

    pub struct MyGraph<I, N, E> {
        inner: Graph<N, E>,
        names: Vec<I>,
    }

    impl<I: Copy + Eq, N, E> MyGraph<I, N, E> {
        pub fn new() -> Self {
            Self {
                inner: Graph::new(),
                names: Vec::new(),
            }
        }
        pub fn add_node(&mut self, ident: I, node: N) {
            if !self.names.contains(&ident) {
                self.names.push(ident);
                self.inner.add_node(node);
            }
        }
    }
}

use graph::MyGraph;

fn main() {
    let graph: MyGraph<&'static str, (), u32> = MyGraph::new();
}

#[derive(Clone, Debug)]
struct Step<Ix> {
    caller: Option<Rc<Step<Ix>>>,
    idx: NodeIndex<Ix>,
    rel: Option<EdgeIndex>,
}

#[derive(Debug)]
struct Steps<Ix> {
    start: Option<Rc<Step<Ix>>>,
}

impl<Ix> Iterator for Steps<Ix> {
    type Item = Step<Ix>;
    fn next(&mut self) -> Option<Self::Item> {
        let head = self.start.clone();
        self.start = head.as_ref().and_then(|head| head.caller.clone());
        head.and_then(|head| Rc::try_unwrap(head).ok().and_then(|inner| Some(inner)))
    }
}

impl<Ix> Into<Vec<Step<Ix>>> for Steps<Ix> {
    fn into(self) -> Vec<Step<Ix>> {
        let mut vec = vec![];
        for a in self {
            vec.push(a);
        }
        vec
    }
}
impl<Ix> IntoIterator for Step<Ix> {
    type IntoIter = Steps<Ix>;
    type Item = Step<Ix>;
    fn into_iter(self) -> Self::IntoIter {
        Steps {
            start: Some(Rc::new(self)),
        }
    }
}

fn breadth_first<'a, Ix: IndexType>(
    graph: &'a Graph<&'static str, usize>,
    inicio: NodeIndex<Ix>,
    meta: NodeIndex<Ix>,
) -> Result<Steps<Ix>, String>
where
    NodeIndex: From<NodeIndex<Ix>>,
    NodeIndex<Ix>: From<NodeIndex>,
{
    let mut frontera = VecDeque::with_capacity(graph.node_count());
    let mut visitados = graph.visit_map();
    frontera.push_front(Step {
        caller: None,
        idx: inicio,
        rel: None,
    });

    while let Some(Step { caller, idx, rel }) = frontera.pop_front() {
        if graph.node_weight(idx.into()).unwrap() == graph.node_weight(meta.into()).unwrap() {
            return Ok(Step {
                caller,
                idx: idx.into(),
                rel,
            }
            .into_iter());
        } else {
            graph
                .neighbors_directed(idx.into(), Direction::Outgoing)
                .into_iter()
                .for_each(|child_idx| {
                    (!visitados.is_visited(&child_idx)).then(|| {
                        visitados.visit(child_idx);
                        frontera.push_back(Step {
                            caller: Some(Rc::new(Step {
                                caller: caller.clone(),
                                idx: idx.into(),
                                rel,
                            })),
                            idx: child_idx.into(),
                            rel: None,
                        });
                    });
                });
        }
    }
    Err(format!("No hay ruta"))
}

// fn main() {
//     let mut graph: Graph<&str, usize> = Graph::new();
//     let a = graph.add_node("A");
//     let b = graph.add_node("B");
//     let c = graph.add_node("C");
//     let d = graph.add_node("D");
//     let e = graph.add_node("E");
//     let f = graph.add_node("F");
//     graph.extend_with_edges(&[(a, b, 10), (b, d, 14), (c, d, 12), (d, e, 10), (f, a, 42)]);

//     let a = breadth_first(&graph, a, e).unwrap();

//     for i in a {
//         println!("{:?}", graph[i.idx])
//     }
// }
