use super::*;

pub struct SimAnnealing<'a, Ix> {
    graph: &'a Graph<&'static str, (f32, f32), f32, Directed, Ix>,
    state: Rc<Step<f32, Ix>>,
    temperature: f32,
}

impl<'a, Ix: IndexType> SimAnnealing<'a, Ix> {
    fn perturb_state(&mut self) -> Rc<Step<f32, Ix>> {
        let mut rng = rrand::get_rng();
        use rand::Rng;
        let mut indices = self.state.collect_nodes();
        let len = indices.len() - 1;
        let idx1 = rng.gen_range(1..(len - 1));
        let idx2 = rng.gen_range(1..(len - 1));

        indices.swap(idx1, idx2);

        // Re-create the step chain
        Rc::new(Step::from_slice(
            &indices,
            &self.graph,
            |par, _, curr, step| {
                self.graph
                    .inner
                    .edge_weight(self.graph.edge_between_unchecked(par, curr))
                    .unwrap()
                    + *step
            },
        ))
    }

    pub fn new(
        graph: &'a Graph<&'static str, (f32, f32), f32, Directed, Ix>,
        journey: (NodeIndex<Ix>, Option<NodeIndex<Ix>>),
        temperature: f32,
    ) -> Self {
        let mut graph_nodes = graph.inner.node_indices().collect::<Vec<_>>();
        let mut rng = rrand::get_rng();
        while let Ok(start) = {
            graph_nodes.sort();
            graph_nodes.binary_search(&journey.0)
        } {
            graph_nodes.remove(start);
        }
        graph_nodes.shuffle(&mut rng);

        let mut result_nodes = Vec::with_capacity(graph_nodes.len() + 1);
        result_nodes.push(journey.0);
        result_nodes.extend(graph_nodes);
        result_nodes.push(journey.0);
        let step = Rc::new(Step::from_slice(
            &result_nodes,
            &graph,
            |par, _, curr, step| {
                graph
                    .inner
                    .edge_weight(graph.edge_between_unchecked(par, curr))
                    .unwrap()
                    + *step
            },
        ));
        Self {
            graph,
            temperature,
            state: step,
        }
    }
}

impl<'a, Ix: IndexType> Walker<Ix> for SimAnnealing<'a, Ix> {
    fn step(&mut self) -> WalkerState<Ix> {
        self.temperature -= 100. * 0.1 / self.temperature;
        if self.temperature <= 0. {
            WalkerState::Done
        } else {
            let new = self.perturb_state();

            if new.state < self.state.state {
                self.state = new;
                WalkerState::NotFound(self.state.clone())
            } else {
                // To try and force recalculations we give non-direct benefiting changes a chance
                use rand::Rng;
                let mut rng = rrand::get_rng();
                let p = rng.gen::<f32>().ln();
                if p > 0.8 {
                    self.state = new;
                    WalkerState::NotFound(self.state.clone())
                } else {
                    WalkerState::Cutoff
                }
            }
        }
    }
}
