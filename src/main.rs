#![allow(non_snake_case)]

mod svg;

use graph::walkers::*;
use leptos::*;
use svg::SvgPlot;

fn main() {
    mount_to_body(|cx| view! { cx,  <App /> })
}

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    let (bench_mode, set_bench_mode) = create_signal(cx, false);
    let graph: &'static _ = Box::leak(Box::new(test_graph2()));
    let img = SvgPlot::new(graph.repr.clone(), None).print_to_string();

    let (time, set_time) = create_signal(cx, 10);
    let (start, set_start) = create_signal(cx, "Cancun".to_string());
    let (end, set_end) = create_signal(cx, "Cabo San Lucas".to_string());
    let (method, set_method) = create_signal(cx, "Bidirectional".to_string());
    let elem_ref = create_node_ref(cx);
    let graph_start_button = create_node_ref(cx);

    let (valid_starting_node, set_valid_start) = create_signal(cx, true);
    let (valid_ending_node, set_valid_end) = create_signal(cx, true);

    let update_valid_hints = move || {
        request_animation_frame(move || {
            let start_button = document()
                .get_element_by_id("graph-start")
                .unwrap()
                .class_list();

            // Update start button class list based on validity of starting and ending nodes
            start_button.remove_3(
                "dark:bg-[#0d1117]",
                "cursor-not-allowed",
                "dark:bg-[#444444]",
            );

            if valid_ending_node.get() && valid_starting_node.get() {
                start_button.add_1("dark:bg-[#0d1117]");
            } else {
                start_button.add_2("cursor-not-allowed", "dark:bg-[#444444]");
            }

            // Update start input class list based on whether the start node exists in the graph
            let start_input = document()
                .get_element_by_id("start-field")
                .unwrap()
                .class_list();
            let start_exists = graph.name_index(&start.get()).is_some();
            start_input.toggle_with_force("dark:border-red-500", !start_exists);
            start_input.toggle_with_force("dark:border-[#0d1117]", start_exists);

            // Update end input class list based on whether the end node exists in the graph
            let end_input = document()
                .get_element_by_id("end-field")
                .unwrap()
                .class_list();
            let end_exists = graph.name_index(&end.get()).is_some();
            end_input.toggle_with_force("dark:border-red-500", !end_exists);
            end_input.toggle_with_force("dark:border-[#0d1117]", end_exists);
        })
    };

    create_effect(cx, move |_| {
        if elem_ref.get().is_some() {
            request_animation_frame(move || {
                let svg_container = document().get_element_by_id("svg-container").unwrap();
                let child = svg_container.children().get_with_index(0).unwrap();
                child.set_attribute("height", "100%").is_err().then(|| {
                    log!("Failed to set height of child 0 of svg-container");
                });
                child.set_attribute("width", "100%").is_err().then(|| {
                    log!("Failed to set width of child 0 of svg-container");
                });
            });
        }
    });

    let search = move |_| {
        if !(valid_ending_node.get() && valid_starting_node.get()) {
            return;
        }

        spawn_local(async move {
            restart_colors();
            let journey = graph.journey(&start.get(), Some(&end.get())).unwrap();
            let distances = graph.get_haversine_table_6371(journey.1.unwrap());

            match method.get().as_str() {
                "A*" => {
                    visual_search(
                        time,
                        a_star::new(
                            graph,
                            journey,
                            |index| *distances.get(index).unwrap(),
                            |state| *state as f32,
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "Weighted A*" => {
                    visual_search(
                        time,
                        weighted_a_star::new(
                            graph,
                            journey,
                            |index| *distances.get(index).unwrap(),
                            |state| *state as f32,
                            1.5,
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "Dijkstra" => {
                    visual_search(
                        time,
                        dijkstra::new(&graph, journey, |edge| *edge, Direction::Outgoing),
                    )
                    .await;
                }
                "Greedy" => {
                    visual_search(
                        time,
                        greedy::new(
                            &graph,
                            journey,
                            |index| *distances.get(index).unwrap(),
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "Beam" => {
                    visual_search(
                        time,
                        Beam::new(
                            &graph,
                            journey,
                            2,
                            |i1, i2| {
                                (distances.get(i1).unwrap())
                                    .partial_cmp(distances.get(i2).unwrap())
                                    .unwrap()
                            },
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "Hill" => {
                    visual_search(
                        time,
                        Hill::new(
                            &graph,
                            journey,
                            |i1, i2| {
                                (distances.get(i1).unwrap())
                                    .partial_cmp(distances.get(i2).unwrap())
                                    .unwrap()
                            },
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "Stochastic Hill" => {
                    visual_search(
                        time,
                        StochasticHill::new(
                            &graph,
                            journey,
                            |i1| *distances.get(i1).unwrap() as f64,
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
                "BFS" => {
                    visual_search(
                        time,
                        BreadthFirst::new(&graph, journey, Direction::Outgoing),
                    )
                    .await;
                }
                "DFS" => {
                    visual_search(
                        time,
                        DepthFirst::new(&graph, journey, None::<usize>, Direction::Outgoing),
                    )
                    .await;
                }
                "Bidirectional" => {
                    let journey_rev = graph.journey(&end.get(), Some(&start.get())).unwrap();
                    visual_search(
                        time,
                        Bidirectional::new(
                            &graph,
                            dijkstra::new(&graph, journey, |edge| *edge, Direction::Outgoing),
                            dijkstra::new(&graph, journey_rev, |edge| *edge, Direction::Incoming),
                        ),
                    )
                    .await;
                }
                _ => {
                    visual_search(
                        time,
                        greedy::new(
                            graph,
                            journey,
                            |index| *distances.get(index).unwrap(),
                            Direction::Outgoing,
                        ),
                    )
                    .await;
                }
            }
        })
    };

    // let restart = move |_| restart_colors();

    let set_start = move |e: web_sys::Event| {
        set_start.set(event_target_value(&e));
        set_valid_start.set(graph.name_index(&start.get()).is_some());
        update_valid_hints();
    };
    let set_end = move |e: web_sys::Event| {
        set_end.set(event_target_value(&e));
        set_valid_end.set(graph.name_index(&end.get()).is_some());
        update_valid_hints();
    };
    let set_method = move |e: web_sys::Event| set_method.set(event_target_value(&e));
    let set_time =
        move |e: web_sys::Event| set_time.set(event_target_value(&e).parse().unwrap_or(10));

    let update_hidden_elements = |bench_mode: bool| {
        toggle_items(bench_mode);
    };

    let toggle_bench_mode = move |_| {
        let new_value = !bench_mode.get();
        set_bench_mode.set(new_value);
        update_hidden_elements(new_value);
    };

    let run_benches = move |_| {
        if !(valid_ending_node.get() && valid_starting_node.get()) {
            return;
        }
        restart_colors();
        let journey = graph.journey(&start.get(), Some(&end.get())).unwrap();
        let distances = graph.get_haversine_table_6371(journey.1.unwrap());

        // Clear tbody
        let tbody = document().get_element_by_id("bench-results").unwrap();
        tbody.set_inner_html("");
        let mut results = Vec::new();

        results.push((
            timed_search(
                a_star::new(
                    graph,
                    journey,
                    |index| *distances.get(index).unwrap(),
                    |state| *state as f32,
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "A*",
        ));

        results.push((
            timed_search(
                weighted_a_star::new(
                    graph,
                    journey,
                    |index| *distances.get(index).unwrap(),
                    |state| *state as f32,
                    1.5,
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "Weighted A*",
        ));

        results.push((
            timed_search(
                dijkstra::new(graph, journey, |edge| *edge, Direction::Outgoing),
                &graph,
            ),
            "Dijkstra",
        ));

        results.push((
            timed_search(
                greedy::new(
                    graph,
                    journey,
                    |index| *distances.get(index).unwrap(),
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "Greedy",
        ));

        results.push((
            timed_search(
                Beam::new(
                    graph,
                    journey,
                    2,
                    |i1, i2| {
                        (distances.get(i1).unwrap())
                            .partial_cmp(distances.get(i2).unwrap())
                            .unwrap()
                    },
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "Beam",
        ));

        results.push((
            timed_search(
                Hill::new(
                    graph,
                    journey,
                    |i1, i2| {
                        (distances.get(i1).unwrap())
                            .partial_cmp(distances.get(i2).unwrap())
                            .unwrap()
                    },
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "Hill",
        ));

        results.push((
            timed_search(
                StochasticHill::new(
                    graph,
                    journey,
                    |i1| *distances.get(i1).unwrap() as f64,
                    Direction::Outgoing,
                ),
                &graph,
            ),
            "Stochastic Hill",
        ));

        results.push((
            timed_search(
                BreadthFirst::new(graph, journey, Direction::Outgoing),
                &graph,
            ),
            "Breadth First",
        ));

        results.push((
            timed_search(
                DepthFirst::new(graph, journey, None::<usize>, Direction::Outgoing),
                &graph,
            ),
            "Depth First",
        ));

        results.push((
            timed_search(
                Bidirectional::new(
                    graph,
                    dijkstra::new(graph, journey, |edge| *edge, Direction::Outgoing),
                    dijkstra::new(
                        graph,
                        graph.journey(&end.get(), Some(&start.get())).unwrap(),
                        |edge| *edge,
                        Direction::Incoming,
                    ),
                ),
                &graph,
            ),
            "Bidirectional",
        ));

        for (result, name) in results.into_iter() {
            let tr = document().create_element("tr").unwrap();
            tr.set_inner_html(&gen_row(name, result));
            tbody.append_child(&tr).unwrap();
        }
    };

    spawn_local(async move {
        run_benches(web_sys::MouseEvent::new("click").unwrap());
    });

    view! {
        cx,
        <div id="graph-container" class="flex flex-col md:flex-row items-center h-full w-full">
            <div class="flex flex-col justify-between space-y-5 bg-gray-900 w-full h-auto md:h-full w-full md:w-1/3 p-5">
                <div>
                    <p id="graph-title" class="text-xl md:text-2xl" >"aigraph1/graph"</p>
                    <p id="bench-title" class="hidden text-xl md:text-2xl" >"aigraph1/bench"</p>
                    <a href="./graph/" class="text-sm" target="_blank">"docs/"</a>
                </div>
                <div class="h-full w-full flex flex-col items-center justify-center space-y-5">
                    <div>
                        <p class="text-sm font-bold pb-1" >"Starting node"</p>
                        <input id="start-field" class="dark:bg-[#0d1117] focus:border-transparent focus:ring-0 border-2 dark:border-[#0d1117] rounded p-2" placeholder="Start" prop:value={start.get()} on:input=set_start />
                    </div>
                    <div>
                        <p class="text-sm font-bold pb-1" >"Target node"</p>
                        <input id="end-field" class="dark:bg-[#0d1117] focus:border-transparent focus:ring-0 border-2 dark:border-[#0d1117] rounded p-2" placeholder="End" prop:value={end.get()} on:input=set_end />
                    </div>
                    <div id="graph-time">
                        <p class="text-sm font-bold pb-1" >"Delay (ms)"</p>
                        <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Time" prop:value={time.get()} on:input=set_time />
                    </div>
                    <div id="graph-selector" class="overflow-y-scroll">
                        <select class="dark:bg-[#0d1117] rounded p-2" prop:value={method.get()} on:input=set_method>
                            <option value="Bidirectional">"Bidirectional"</option>
                            <option value="Dijkstra">"Dijkstra"</option>
                            <option value="A*">"A*"</option>
                            <option value="Weighted A*">"Weighted A*"</option>
                            <option value="Beam">"Beam"</option>
                            <option value="BFS">"BFS"</option>
                            <option value="DFS">"DFS"</option>
                            <option value="Greedy">"Greedy"</option>
                            <option value="Hill">"Hill"</option>
                            <option value="Stochastic Hill">"Stochastic Hill"</option>
                            <option value="Simulated Annealing">"Simulated Annealing"</option>
                        </select>
                    </div>
                    <div class="flex space-x-5">
                        <button id="bench-start" class="hidden dark:bg-[#0d1117] rounded p-2" on:click=run_benches>"Re-run benches"</button>
                        <button _ref=graph_start_button id="graph-start" class="dark:bg-[#0d1117] rounded p-2" on:click=search>"Start search"</button>
                    </div>
                    <button id="graph-toggle" class="dark:bg-[#0d1117] rounded p-2" on:click=toggle_bench_mode>"Change to bench mode"</button>
                    <button id="bench-toggle" class="hidden dark:bg-[#0d1117] rounded p-2" on:click=toggle_bench_mode>"Change to visual mode"</button>
                </div>
                <div/>
            </div>
            <div _ref=elem_ref id="svg-container" inner_html={&img} class="c-block justify-items-center flex w-full md:w-2/3 h-full"/>
            <div id="bench-container" class="hidden items-center justify-center flex w-full md:w-2/3 h-full">
                // Minimal tailwindcss table with method name, time, iterations and cost
                <table class="table-auto">
                    <thead>
                        <tr>
                            <th class="px-4 py-2">"Method"</th>
                            <th class="px-4 py-2">"Time (ms)"</th>
                            <th class="px-4 py-2">"Iterations"</th>
                            <th class="px-4 py-2">"Size"</th>
                            <th class="px-4 py-2">"Edge Cost"</th>
                            <th class="px-4 py-2">"Distance Cost"</th>
                        </tr>
                    </thead>
                    <tbody id="bench-results" />
                </table>
            </div>
        </div>
    }
}

fn toggle_items(bench_mode: bool) {
    request_animation_frame(move || {
        let bench_related = &[
            "bench-title",
            "bench-start",
            "bench-container",
            "bench-toggle",
        ];
        let graph_related = &[
            "graph-title",
            "graph-start",
            "svg-container",
            "graph-toggle",
            "graph-selector",
            "graph-time",
        ];

        for (related, add) in [
            (bench_related.as_ref(), !bench_mode),
            (graph_related.as_ref(), bench_mode),
        ] {
            for i in related {
                let elem = document().get_element_by_id(i).unwrap();
                if elem.class_list().toggle_with_force("hidden", add).is_err() {
                    log!(
                        "Failed to {} hidden class from {}",
                        if add { "add" } else { "remove" },
                        i
                    );
                }
            }
        }
    });
}

fn restart_colors() {
    let polyline_list = document().get_elements_by_tag_name("polyline");
    for child in 0..polyline_list.length() {
        set_stroke(child, "#FF0000");
    }
}

fn timed_search<S>(
    mut machine: impl Walker<S>,
    graph: &Graph<&'static str, (f32, f32), u16, Directed>,
) -> (f64, usize, usize, u64, f64) {
    let mut iter = 0;
    let start = window()
        .performance()
        .map(|p| p.now())
        .unwrap_or(js_sys::Date::now());

    loop {
        iter += 1;

        let res: WalkerState<_, _> = machine.step();

        match res {
            WalkerState::Found(step) => {
                let edges = step
                    .collect_edges()
                    .into_iter()
                    .map(|edge| graph.inner.edge_weight(edge).unwrap())
                    .fold(0u64, |acc, x| acc + *x as u64);

                let distance = step
                    .collect_nodes()
                    .windows(2)
                    .map(|window| {
                        let (a, b) = (window[0], window[1]);
                        graph.get_haversine_6371(a, b)
                    })
                    .fold(0f64, |acc, x| acc + x as f64);

                return (
                    window()
                        .performance()
                        .map(|p| p.now())
                        .unwrap_or(js_sys::Date::now())
                        - start,
                    iter,
                    step.chain_size(),
                    edges,
                    distance,
                );
            }
            WalkerState::Done => {
                return (
                    window()
                        .performance()
                        .map(|p| p.now())
                        .unwrap_or(js_sys::Date::now())
                        - start,
                    iter,
                    0,
                    0,
                    0.,
                );
            }
            _ => {}
        }
    }
}

async fn visual_search<S>(time: ReadSignal<u64>, mut machine: impl Walker<S>) {
    loop {
        use async_std::task::sleep;

        let res: WalkerState<_, _> = machine.step();
        let time = time.get();

        match res {
            WalkerState::NotFound(ref step) => {
                let edges = step.collect_edges();

                if time != 0 {
                    sleep(std::time::Duration::from_millis(time)).await;
                }
                edges
                    .iter()
                    .for_each(|edge| set_stroke(edge.index() as u32, "#FFFFFF"));

                if time != 0 {
                    sleep(std::time::Duration::from_millis(50)).await;
                }
                edges
                    .into_iter()
                    .for_each(|edge| set_stroke(edge.index() as u32, "#555555"));
            }
            WalkerState::Found(step) => {
                if time != 0 {
                    sleep(std::time::Duration::from_millis(time)).await;
                }
                step.collect_edges()
                    .into_iter()
                    .for_each(|edge| set_stroke(edge.index() as u32, "#FFFFFF"));
                break;
            }
            WalkerState::Done => {
                break;
            }
            _ => {}
        }
    }
}

fn set_stroke(rel: u32, stroke: &'static str) {
    request_animation_frame(move || {
        let polyline_list = document().get_elements_by_tag_name("polyline");
        let child = polyline_list.get_with_index(rel).unwrap();
        child.set_attribute("stroke", stroke).is_err().then(|| {
            log!("Failed to set stroke to edge {}", rel);
        });
    });
}

fn gen_row(name: &str, result: (f64, usize, usize, u64, f64)) -> String {
    format!(
        "<tr>
    <td class=\"border-t border-b px-4 py-2\">{}</td>
    <td class=\"border-t border-b px-4 py-2\">{:.2}</td>
    <td class=\"border-t border-b px-4 py-2\">{}</td>
    <td class=\"border-t border-b px-4 py-2\">{}</td>
    <td class=\"border-t border-b px-4 py-2\">{}</td>
    <td class=\"border-t border-b px-4 py-2\">{:.2}</td>
    </tr>",
        name, result.0, result.1, result.2, result.3, result.4
    )
}
