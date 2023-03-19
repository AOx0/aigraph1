#![allow(non_snake_case)]
use std::collections::VecDeque;

// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use graph::*;

fn main() {
    // launch the web app
    dioxus_web::launch(App);
}

static SCRIPT: &'static str = "\
const myScrollableDiv = document.getElementById('my-scrollable-div');\
myScrollableDiv.scrollTop = myScrollableDiv.scrollHeight;";


#[inline_props]
fn Message<'a>(cx: Scope, i: usize, value: &'a str, ) -> Element {
    cx.render(
        if i % 2 == 0 {
            rsx! {
                div {
                    "x-init": "{SCRIPT}", 
                    class: "mt-auto bg-blue-500 dark:bg-blue-800 rounded-lg py-2 px-4 max-w-xs",
                    p { class: "text-sm text-white whitespace-pre-wrap",
                        "{value}"
                    }
                }
            }
        } else {
            rsx! {
                div {
                    "x-init": "{SCRIPT}", 
                    class: "mt-auto bg-gray-200 dark:bg-gray-800 rounded-lg py-2 px-4 max-w-xs",
                    p { class: "text-sm text-black dark:text-white whitespace-pre-wrap",
                        "{value}"
                    }
                }
            }
        }
    )

}

fn App(cx: Scope) -> Element {
    let mut count = use_state(cx, || 0);
    let mut text = use_state(cx, || String::with_capacity(350));
    let mut messages = use_state(cx, || VecDeque::from_iter([
        r#"The available operations are:
    1-Breadth first search
    2-Dijkstra search
    3-Depth first search (optional limit)
    4-Iterative depth first search (optional limit)
    5-Bidirectional
    6-Exit"#.to_owned()
    ].into_iter()));
    let output = use_state(cx, String::new);    


    cx.render(rsx! {
        div { 
            id: "my-scrollable-div", class: "flex-1 overflow-y-auto",
            /* Chat messages */
            div { 
                class: "flex flex-col items-end justify-end space-y-2 py-4 px-8",
                for (i, value) in messages.get().iter().enumerate() { 
                    rsx! {
                        Message { i: i, value: value }
                    }
                }
            }
        }
        /* Input field */
        div { 
            class: "bg-white dark:bg-gray-700",
            div { class: "max-w-screen-lg mx-auto py-4 px-4 flex items-center",
                input { class: "text-black dark:text-white bg-white dark:bg-gray-900 flex-1 rounded-lg py-2 px-4 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500",
                    id: "userInput",
                    name: "userInput",
                    r#type: "text",
                    value: "{text}",
                    placeholder: "Type something...",
                    oninput: move |evt: FormEvent| {
                        text.set(evt.value.to_owned())
                    }
                }
                button { class: "dark:bg-gray-900 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded ml-4",
                    onclick: move |evt: Event<MouseData>| {
                        if !text.get().trim().is_empty() {
                            messages.with_mut(|v| { v.push_back(text.get().clone()) } );
                            text.with_mut(|v| v.clear());
                        }
                    },
                    "Send"
                }
            }
        }   
    })
}


