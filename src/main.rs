#![allow(non_snake_case)]

use anyhow::{anyhow, Context, Result};

use leptos::*;

use graph::walkers::*;
use svg::SvgPlot;

mod svg;

use std::panic;

fn main() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    mount_to_body(|cx| view! { cx,  <App /> })
}

const METHODS: [&str; 11] = [
    "Bidirectional",
    "Dijkstra",
    "A*",
    "Weighted A*",
    "Beam",
    "BFS",
    "DFS",
    "Greedy",
    "Hill",
    "Stochastic Hill",
    "Simulated Annealing",
];

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    let (bench_mode, set_bench_mode) = create_signal(cx, false);
    let graph: &'static _ = Box::leak(Box::new(mexico_graph()));
    let graph_connected: &'static _ = Box::leak(Box::new(full_connected_graph()));
    let img: &'static str = Box::leak(Box::new(
        SvgPlot::new(graph.repr.clone(), None).print_to_string(),
    ));
    let (temperature, set_temperature) = create_signal(cx, 550.);

    let (time, set_time) = create_signal(cx, 10);
    let (start, set_start) = create_signal(cx, "Cancun".to_string());
    let (end, set_end) = create_signal(cx, "Cabo San Lucas".to_string());
    let (method, set_method) = create_signal(cx, "Bidirectional".to_string());
    let elem_ref = create_node_ref(cx);
    let graph_start_button = create_node_ref(cx);
    let (blocked_button, set_blocked) = create_signal(cx, false);

    let (valid_starting_node, set_valid_start) = create_signal(cx, true);
    let (valid_ending_node, set_valid_end) = create_signal(cx, true);

    let update_valid_hints = move || {
        request_animation_frame(move || {
            if let Err(err) = update_hints(
                graph,
                &start,
                &end,
                valid_starting_node,
                valid_ending_node,
                blocked_button.get(),
            ) {
                log!("Error updating hints: {}", err);
            }
        })
    };

    let remove_size_restriction = move || {
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
    };

    create_effect(cx, move |_| remove_size_restriction());

    let search = move |_| {
        if !(valid_ending_node.get() && valid_starting_node.get()) {
            return;
        }

        if blocked_button.get() {
            return;
        }
        set_blocked.set(true);
        block_button(true).unwrap_or_else(|err| log!("Error blocking button: {:?}", err));

        spawn_local(async move {
            if method.get() == "Simulated Annealing" {
                let mut machine = SimAnnealing::new(
                    graph_connected,
                    graph.journey(&start.get(), None).unwrap(),
                    temperature.get(),
                );

                loop {
                    let step = machine.step();
                    if let WalkerState::Cutoff = step {
                        continue;
                    } else if let WalkerState::Done = step {
                        restart_colors();
                        break;
                    } else {
                        async_std::task::sleep(std::time::Duration::from_millis(10)).await;
                        let mut repr = unit_graph();
                        for index in step
                            .step_peek()
                            .as_ref()
                            .unwrap()
                            .collect_nodes()
                            .windows(2)
                        {
                            repr.next(
                                repr.index_name(index[0]).unwrap(),
                                repr.index_name(index[1]).unwrap(),
                                repr.get_haversine_6371(index[0], index[1]),
                            )
                            .unwrap();
                        }
                        let svg_container = document().get_element_by_id("svg-container").unwrap();
                        svg_container
                            .set_inner_html(&SvgPlot::new_white(repr.repr).print_to_string());
                        let state_indicator =
                            document().get_element_by_id("state-indicator").unwrap();
                        state_indicator.set_inner_html(&format!(
                            "{}",
                            step.step_peek().as_ref().unwrap().state
                        ));

                        remove_size_restriction();
                    }
                }
            } else {
                let svg_container = document().get_element_by_id("svg-container").unwrap();
                svg_container.set_inner_html(img);
                remove_size_restriction();
                visual_search(
                    time,
                    create_machine(graph, &start, &end, method.get().as_str()),
                )
                .await;
            }
            set_blocked.set(false);
            block_button(false).unwrap_or_else(|err| log!("Error blocking button: {:?}", err));
        })
    };

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

    let set_temperature = move |e: web_sys::Event| {
        set_temperature.set(event_target_value(&e).parse().unwrap_or(550.));
    };

    let set_method = move |e: web_sys::Event| {
        if blocked_button.get() {
            return;
        }

        let temperature_input = document().get_element_by_id("sim-ann").unwrap();
        temperature_input
            .class_list()
            .toggle_with_force("hidden", event_target_value(&e) != "Simulated Annealing")
            .unwrap();
        let temperature_input = document().get_element_by_id("sa-distance").unwrap();
        temperature_input
            .class_list()
            .toggle_with_force("hidden", event_target_value(&e) != "Simulated Annealing")
            .unwrap();
        let temperature_input = document().get_element_by_id("end-div").unwrap();
        temperature_input
            .class_list()
            .toggle_with_force("hidden", event_target_value(&e) == "Simulated Annealing")
            .unwrap();
        let temperature_input = document().get_element_by_id("graph-toggle").unwrap();
        temperature_input
            .class_list()
            .toggle_with_force("hidden", event_target_value(&e) == "Simulated Annealing")
            .unwrap();
        let temperature_input = document().get_element_by_id("graph-time").unwrap();
        temperature_input
            .class_list()
            .toggle_with_force("hidden", event_target_value(&e) == "Simulated Annealing")
            .unwrap();

        let svg_container = document().get_element_by_id("svg-container").unwrap();
        if event_target_value(&e) == "Simulated Annealing" {
            svg_container.set_inner_html("");
        } else {
            svg_container.set_inner_html(img);
            remove_size_restriction();
        }

        set_method.set(event_target_value(&e));
    };

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

        // Clear tbody
        let tbody = document().get_element_by_id("bench-results").unwrap();
        tbody.set_inner_html("");

        for method in METHODS.iter().copied() {
            if method == "Simulated Annealing" {
                continue;
            }
            let tr = document().create_element("tr").unwrap();
            tr.set_inner_html(&gen_row(
                method,
                timed_search(create_machine(graph, &start, &end, method), graph),
            ));
            tbody.append_child(&tr).unwrap();
        }
    };

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
                    <div id="end-div">
                        <p class="text-sm font-bold pb-1" >"Target node"</p>
                        <input id="end-field" class="dark:bg-[#0d1117] focus:border-transparent focus:ring-0 border-2 dark:border-[#0d1117] rounded p-2" placeholder="End" prop:value={end.get()} on:input=set_end />
                    </div>
                    <div id="graph-time">
                        <p class="text-sm font-bold pb-1" >"Delay (ms)"</p>
                        <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Time" prop:value={time.get()} on:input=set_time />
                    </div>
                    <div id="sim-ann" class="hidden">
                        <p class="text-sm font-bold pb-1" >"Temperature"</p>
                        <input class="dark:bg-[#0d1117] focus:border-transparent focus:ring-0 border-2 dark:border-[#0d1117] rounded p-2" placeholder="Temperature" prop:value={temperature.get()} on:input=set_temperature />
                    </div>
                    <div id="graph-selector" class="overflow-y-scroll">
                        <select class="dark:bg-[#0d1117] rounded p-2" prop:value={method.get()} on:input=set_method>
                            <For
                                 each=|| METHODS
                                 key=|i| *i
                                 view=|cx, i| view! { cx, <option value={i}>{i}</option> }
                            />
                        </select>
                    </div>
                    <div id="sa-distance" class="hidden flex space-x-2">
                        <p>"Distance:"</p>
                        <p id="state-indicator">"0"</p>
                    </div>
                    <div class="flex">
                        <button id="bench-start" class="hidden dark:bg-[#0d1117] rounded p-2" on:click=run_benches>"Re-run benches"</button>
                        <button _ref=graph_start_button id="graph-start" class="dark:bg-[#0d1117] rounded p-2" on:click=search>"Start search"</button>
                    </div>
                    <button id="graph-toggle" class="dark:bg-[#0d1117] rounded p-2" on:click=toggle_bench_mode>"Change to bench mode"</button>
                    <button id="bench-toggle" class="hidden dark:bg-[#0d1117] rounded p-2" on:click=toggle_bench_mode>"Change to visual mode"</button>
                </div>
                <div/>
            </div>
            <div _ref=elem_ref id="svg-container" inner_html={img} class="c-block justify-items-center flex w-full md:w-2/3 h-full"/>
            <div id="bench-container" class="hidden items-center justify-center flex w-full md:w-2/3 h-0 sm:h-full">
                // Minimal tailwindcss table with method name, time, iterations and cost
                <div class="hidden sm:block">
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
            <div id="bench-warning" class="block hidden sm:hidden items-center justify-center flex w-full px-4 md:w-2/3 h-full sm:h-0">
                <p>"Benchmark results only available on larger screens"</p>
            </div>
        </div>
    }
}

fn create_machine<'a>(
    graph: &'a Graph<&str, (f32, f32), f32, Directed>,
    start: &ReadSignal<String>,
    end: &ReadSignal<String>,
    method: &str,
) -> Box<dyn Walker + 'a> {
    let journey: (NodeIndex<u32>, Option<NodeIndex<u32>>) = graph
        .journey(start.get().as_str(), Some(end.get().as_str()))
        .unwrap();

    let distances = graph.get_haversine_table_6371(journey.1.unwrap());

    match method {
        "A*" => Box::new(a_star::new(
            graph,
            journey,
            move |index| *distances.get(index).unwrap(),
            |state| *state as f32,
            Direction::Outgoing,
        )),
        "Weighted A*" => Box::new(weighted_a_star::new(
            graph,
            journey,
            move |index| *distances.get(index).unwrap(),
            |state| *state as f32,
            1.5,
            Direction::Outgoing,
        )),
        "Dijkstra" => Box::new(dijkstra::new(
            graph,
            journey,
            |edge| *edge as f32,
            Direction::Outgoing,
        )),
        "Greedy" => Box::new(greedy::new(
            graph,
            journey,
            move |index| *distances.get(index).unwrap(),
            Direction::Outgoing,
        )),
        "Beam" => Box::new(Beam::new(
            graph,
            journey,
            2,
            move |i1, i2| {
                (distances.get(i1).unwrap())
                    .partial_cmp(distances.get(i2).unwrap())
                    .unwrap()
            },
            Direction::Outgoing,
        )),
        "Hill" => Box::new(Hill::new(
            graph,
            journey,
            move |i1, i2| {
                (distances.get(i1).unwrap())
                    .partial_cmp(distances.get(i2).unwrap())
                    .unwrap()
            },
            Direction::Outgoing,
        )),
        "Stochastic Hill" => Box::new(StochasticHill::new(
            graph,
            journey,
            move |i1| *distances.get(i1).unwrap(),
            Direction::Outgoing,
        )),
        "BFS" => Box::new(BreadthFirst::new(graph, journey, Direction::Outgoing)),
        "DFS" => Box::new(DepthFirst::new(graph, journey, None, Direction::Outgoing)),
        "Bidirectional" => {
            let journey_rev = graph.journey(&end.get(), Some(&start.get())).unwrap();
            Box::new(Bidirectional::new(
                graph,
                dijkstra::new(graph, journey, |edge| *edge as f32, Direction::Outgoing),
                dijkstra::new(graph, journey_rev, |edge| *edge as f32, Direction::Incoming),
            ))
        }
        _ => panic!("Invalid method {}", method),
    }
}

fn block_button(block: bool) -> Result<()> {
    let start_button = document()
        .get_element_by_id("graph-start")
        .context("Failed to get graph-start")?
        .class_list();

    // Update start button class list based on validity of starting and ending nodes
    start_button
        .remove_3(
            "dark:bg-[#0d1117]",
            "cursor-not-allowed",
            "dark:bg-[#444444]",
        )
        .map_err(|_| anyhow!("Failed to remove 3 classes"))?;

    if !block {
        start_button
            .add_1("dark:bg-[#0d1117]")
            .map_err(|_| anyhow!("Failed to add 1 class"))?;
    } else {
        start_button
            .add_2("cursor-not-allowed", "dark:bg-[#444444]")
            .map_err(|_| anyhow!("Failed to add 2 classes"))?;
    }

    Ok(())
}

fn update_hints(
    graph: &Graph<&str, (f32, f32), f32, Directed>,
    start: &ReadSignal<String>,
    end: &ReadSignal<String>,
    valid_starting_node: ReadSignal<bool>,
    valid_ending_node: ReadSignal<bool>,
    blocked_button: bool,
) -> Result<()> {
    block_button(!(valid_ending_node.get() && valid_starting_node.get()) || blocked_button)
        .unwrap_or_else(|err| {
            log!("Error while blocking button with error: {:?}", err);
        });

    // Update start input class list based on whether the start node exists in the graph
    let start_input = document()
        .get_element_by_id("start-field")
        .context("Failed to get start-field")?
        .class_list();
    let start_exists = graph.name_index(&start.get()).is_some();
    start_input
        .toggle_with_force("dark:border-red-500", !start_exists)
        .map_err(|_| anyhow!("Failed to toggle red class"))?;
    start_input
        .toggle_with_force("dark:border-[#0d1117]", start_exists)
        .map_err(|_| anyhow!("Failed to toggle normal class"))?;

    // Update end input class list based on whether the end node exists in the graph
    let end_input = document()
        .get_element_by_id("end-field")
        .context("Failed to get end-field")?
        .class_list();
    let end_exists = graph.name_index(&end.get()).is_some();
    end_input
        .toggle_with_force("dark:border-red-500", !end_exists)
        .map_err(|_| anyhow!("Failed to toggle red class"))?;
    end_input
        .toggle_with_force("dark:border-[#0d1117]", end_exists)
        .map_err(|_| anyhow!("Failed to toggle normal class"))?;
    Ok(())
}

fn toggle_items(bench_mode: bool) {
    request_animation_frame(move || {
        let bench_related = &[
            "bench-title",
            "bench-start",
            "bench-container",
            "bench-warning",
            "bench-toggle",
        ];
        let graph_related = &[
            "graph-title",
            "state-indicator",
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

fn timed_search(
    mut machine: Box<dyn Walker>,
    graph: &Graph<&'static str, (f32, f32), f32, Directed>,
) -> (f32, usize, usize, u64, f32) {
    let mut iter = 0;
    let start = window()
        .performance()
        .map(|p| p.now())
        .unwrap_or(js_sys::Date::now());

    loop {
        iter += 1;

        let res: WalkerState<_> = machine.step();

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
                    .fold(0f32, |acc, x| acc + x);

                return (
                    (window()
                        .performance()
                        .map(|p| p.now())
                        .unwrap_or(js_sys::Date::now())
                        - start) as f32,
                    iter,
                    step.chain_size(),
                    edges,
                    distance,
                );
            }
            WalkerState::Done => {
                return (
                    (window()
                        .performance()
                        .map(|p| p.now())
                        .unwrap_or(js_sys::Date::now())
                        - start) as f32,
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

async fn visual_search(time: ReadSignal<u64>, mut machine: Box<dyn Walker>) {
    loop {
        use async_std::task::sleep;

        let res: WalkerState<_> = machine.step();
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

fn gen_row(name: &str, result: (f32, usize, usize, u64, f32)) -> String {
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
