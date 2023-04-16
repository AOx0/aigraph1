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
    let graph: &'static _ = Box::leak(Box::new(test_graph2()));
    let img = SvgPlot::new(graph.repr.clone(), None).print_to_string();

    let (time, set_time) = create_signal(cx, 100);
    let (start, set_start) = create_signal(cx, "Cancun".to_string());
    let (end, set_end) = create_signal(cx, "Cabo San Lucas".to_string());
    let (method, set_method) = create_signal(cx, "A*".to_string());
    let elem_ref = create_node_ref(cx);

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
        spawn_local(async move {
            restart_colors();
            let journey = graph.journey(&start.get(), Some(&end.get())).unwrap();
            let distances = graph.get_haversine_6371(journey.1.unwrap());

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
                _ => {
                    let machine = greedy::new(
                        graph,
                        journey,
                        |index| *distances.get(index).unwrap(),
                        Direction::Outgoing,
                    );
                    visual_search(time, machine).await;
                }
            }
        })
    };

    let restart = move |_| restart_colors();

    let set_start = move |e: web_sys::Event| set_start.set(event_target_value(&e));
    let set_end = move |e: web_sys::Event| set_end.set(event_target_value(&e));
    let set_method = move |e: web_sys::Event| set_method.set(event_target_value(&e));
    let set_time =
        move |e: web_sys::Event| set_time.set(event_target_value(&e).parse().unwrap_or(100));

    view! {
        cx,
        <div class="flex flex-col md:flex-row items-center h-full w-full">
            <div class="flex flex-col space-y-5 bg-gray-900 w-full h-auto md:h-full w-full md:w-1/3 p-5">
                <p class="text-xl md:text-2xl" >"aigraph1/"</p>
                <div class="h-full w-full flex flex-col items-center justify-center space-y-5">
                    <div>
                        <p class="text-sm font-bold pb-1" >"Starting node"</p>
                        <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Start" prop:value={start.get()} on:input=set_start />
                    </div>
                    <div>
                        <p class="text-sm font-bold pb-1" >"Target node"</p>
                        <input class="dark:bg-[#0d1117] rounded p-2" placeholder="End" prop:value={end.get()} on:input=set_end />
                    </div>
                    <div>
                        <p class="text-sm font-bold pb-1" >"Delay (ms)"</p>
                        <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Time" prop:value={time.get()} on:input=set_time />
                    </div>
                    <div class="overflow-y-scroll">
                        <select class="dark:bg-[#0d1117] rounded p-2" prop:value={method.get()} on:input=set_method>
                            <option value="A*">"A*"</option>
                            <option value="Weighted A*">"Weighted A*"</option>
                            <option value="Beam">"Beam"</option>
                            <option value="BFS">"BFS"</option>
                            <option value="DFS">"DFS"</option>
                            <option value="Dijkstra">"Dijkstra"</option>
                            <option value="Greedy">"Greedy"</option>
                        </select>
                    </div>
                    <div class="flex space-x-5">
                        <button class="dark:bg-[#0d1117] rounded p-2" on:click=search>"Start search"</button>
                        <button class="dark:bg-[#0d1117] rounded p-2" on:click=restart>"Reset colors"</button>
                    </div>
                </div>
            </div>
            <div _ref=elem_ref id="svg-container" inner_html=img class="c-block justify-items-center flex w-full md:w-2/3 h-full"/>
        </div>
    }
}

fn restart_colors() {
    let polyline_list = document().get_elements_by_tag_name("polyline");
    for child in 0..polyline_list.length() {
        set_stroke(child, "#FF0000");
    }
}

async fn visual_search<S>(time: ReadSignal<u64>, mut machine: impl Walker<S>) {
    loop {
        use async_std::task::sleep;

        let res: WalkerState<_> = machine.step();
        let time = time.get();

        match res {
            WalkerState::NotFound(ref step) => {
                let edges = StepUnit::collect_edges(step.clone());

                if time != 0 {
                    sleep(std::time::Duration::from_millis(time)).await;
                }
                edges
                    .iter()
                    .for_each(|edge| set_stroke(edge.index() as u32, "#FFFFFF"));

                if time != 0 {
                    sleep(std::time::Duration::from_millis(time / 2)).await;
                }
                edges
                    .into_iter()
                    .for_each(|edge| set_stroke(edge.index() as u32, "#555555"));
            }
            WalkerState::Found(step) => {
                if time != 0 {
                    sleep(std::time::Duration::from_millis(time)).await;
                }
                StepUnit::collect_edges(std::rc::Rc::new(step))
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
