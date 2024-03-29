use super::*;

/// The Bidirectional strategy. Performs the seach operation with two (may be different) state machines
///
/// Rules:
/// Each machine needs to be totally opposite to the other one. Thus:
/// - The target node of the a machine *m1* needs to be the starting node of the machine *m2*
/// - The starting node of the a walker *m1* needs to be the target node of a machine *m2*
/// - The direction of the walker *m1* needs to be `petgraph::Outgoing` and the *m2* must be `petgraph::Incoming`
pub struct Bidirectional<'a, I, N, E, Ty, Ix, M1, M2> {
    graph: &'a Graph<I, N, E, Ty, Ix>,
    machine_a: M1,
    machine_b: M2,
    visited_a: FixedBitSet,
    visited_b: FixedBitSet,
    story_a: HashMap<NodeIndex<Ix>, Rc<Step<f32, Ix>>>,
    story_b: HashMap<NodeIndex<Ix>, Rc<Step<f32, Ix>>>,
    turn_of_a: bool,
    machine_a_finished: bool,
    machine_b_finished: bool,
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, M1, M2> Bidirectional<'a, I, N, E, Ty, Ix, M1, M2>
where
    M1: Walker<Ix>,
    M2: Walker<Ix>,
{
    #[allow(dead_code)]
    pub fn new(graph: &'a Graph<I, N, E, Ty, Ix>, machine_a: M1, machine_b: M2) -> Self {
        Self {
            graph,
            machine_a,
            machine_b,
            turn_of_a: true,
            visited_a: FixedBitSet::with_capacity(graph.node_count()),
            visited_b: FixedBitSet::with_capacity(graph.node_count()),
            story_a: HashMap::with_capacity(graph.node_count()),
            story_b: HashMap::with_capacity(graph.node_count()),
            machine_a_finished: false,
            machine_b_finished: false,
        }
    }
}

impl<'a, I, N, E, Ty: EdgeType, Ix: IndexType, M1, M2> Walker<Ix>
    for Bidirectional<'a, I, N, E, Ty, Ix, M1, M2>
where
    M1: Walker<Ix>,
    M2: Walker<Ix>,
{
    fn step(&mut self) -> WalkerState<Ix> {
        let next_step = match self.turn_of_a {
            true => match self.machine_a.step() {
                WalkerState::Found(step) => {
                    return WalkerState::Found(step);
                }
                WalkerState::NotFound(step) => {
                    self.visited_a.visit(step.idx);
                    self.story_a.insert(step.idx, Rc::clone(&step));
                    WalkerState::NotFound(step)
                }
                WalkerState::Cutoff => WalkerState::Cutoff,
                WalkerState::Done => {
                    self.machine_a_finished = true;
                    if self.machine_b_finished {
                        WalkerState::Done
                    } else {
                        WalkerState::Cutoff
                    }
                }
            },
            false => match self.machine_b.step() {
                WalkerState::Found(step) => {
                    return WalkerState::Found(step);
                }
                WalkerState::NotFound(step) => {
                    self.visited_b.visit(step.idx);
                    self.story_b.insert(step.idx, Rc::clone(&step));
                    WalkerState::NotFound(step)
                }
                WalkerState::Cutoff => WalkerState::Cutoff,
                WalkerState::Done => {
                    self.machine_b_finished = true;
                    if self.machine_a_finished {
                        WalkerState::Done
                    } else {
                        WalkerState::Cutoff
                    }
                }
            },
        };

        let res = if let WalkerState::NotFound(step) = next_step {
            // Check if the current node has been visited by the other machine
            let intersection =
                self.visited_b.is_visited(&step.idx) && self.visited_a.is_visited(&step.idx);

            if !intersection {
                // If it is not an intersection, just return the step
                self.turn_of_a = !self.turn_of_a;
                return WalkerState::NotFound(step);
            }

            // If it is indeed an intersection, create a vector of NodeIndex<Ix>
            let mut path = VecDeque::with_capacity(self.graph.node_count());

            // Add all the steps from the current machine to the vector
            for step in step.iter() {
                path.push_back(step.idx);
            }

            // Add all steps from the other machine from the story where the intersection was found
            if self.turn_of_a {
                for step in self.story_b.get(&step.idx).unwrap().iter().skip(1) {
                    path.push_front(step.idx);
                }
            } else {
                for step in self.story_a.get(&step.idx).unwrap().iter().skip(1) {
                    path.push_front(step.idx);
                }
            }

            let mut step: Step<_, _> = Step {
                idx: if self.turn_of_a {
                    path.pop_back()
                } else {
                    path.pop_front()
                }
                .unwrap(),
                state: 0.,
                ..Default::default()
            };
            while let Some(next) = if self.turn_of_a {
                path.pop_back()
            } else {
                path.pop_front()
            } {
                let idx = step.idx;
                step = Step {
                    idx: next,
                    caller: Some(Rc::new(step)),
                    rel: self.graph.edge_between(idx, next),
                    state: 0.,
                };
            }
            return WalkerState::Found(step);
        } else {
            next_step
        };

        if self.machine_a_finished && self.machine_b_finished {
            WalkerState::Done
        } else {
            self.turn_of_a = !self.turn_of_a;
            res
        }
    }
}
