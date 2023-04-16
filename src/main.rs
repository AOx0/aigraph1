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
            let journey = graph.journey(&start.get(), Some(&end.get())).unwrap();
            let distances = graph.get_haversine_6371(journey.1.unwrap());
            let mut machine = greedy::new(
                graph,
                journey,
                |index| *distances.get(index).unwrap(),
                Direction::Outgoing,
            );
            visual_search(time, &mut machine).await;
        })
    };

    let restart = move |_| {
        let polyline_list = document().get_elements_by_tag_name("polyline");
        for child in 0..polyline_list.length() {
            set_stroke(child, "#FF0000");
        }
    };

    let set_start = move |e: web_sys::Event| set_start.set(event_target_value(&e));
    let set_end = move |e: web_sys::Event| set_end.set(event_target_value(&e));
    let set_method = move |e: web_sys::Event| set_method.set(event_target_value(&e));
    let set_time =
        move |e: web_sys::Event| set_time.set(event_target_value(&e).parse().unwrap_or(100));

    view! {
        cx,
        <div class="flex items-center h-full w-full">
            <div class="flex flex-col space-y-5 bg-gray-900 w-full h-auto md:h-full w-1/3 p-5">
                <p class="text-xl md:text-2xl" >"aigraph1/"</p>
                <div class="h-full w-full flex flex-col items-center justify-center space-y-5">
                    <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Start" prop:value={start.get()} on:input=set_start />
                    <input class="dark:bg-[#0d1117] rounded p-2" placeholder="End" prop:value={end.get()} on:input=set_end />
                    <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Time" prop:value={time.get()} on:input=set_time />
                    <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Method" prop:value={method.get()} on:input=set_method />
                    <button class="dark:bg-[#0d1117] rounded p-2" on:click=search>"Start search"</button>
                    <button class="dark:bg-[#0d1117] rounded p-2" on:click=restart>"Reset colors"</button>
                </div>
            </div>
            <div _ref=elem_ref id="svg-container" inner_html=img class="c-block justify-items-center flex w-2/3 h-full"/>
        </div>
    }
}

async fn visual_search<S>(time: ReadSignal<u64>, machine: &mut impl Walker<S>) {
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
