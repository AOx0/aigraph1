#![allow(non_snake_case)]
use std::collections::VecDeque;

// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use graph::*;

fn main() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Info));
    // launch the web app
    dioxus_web::launch(App);
}


#[inline_props]
fn Message<'a>(cx: Scope, i: usize, value: &'a str, ) -> Element {
    let SCRIPT = "\
const myScrollableDiv = document.getElementById('my-scrollable-div');\
myScrollableDiv.scrollTop = myScrollableDiv.scrollHeight;";

    cx.render(
        if i % 2 == 0 {
            rsx! {
                div {
                    "x-init": "{SCRIPT}", 
                    class: "mt-auto bg-blue-500 dark:bg-blue-800 rounded-lg py-2 px-4 w-full",
                    p { class: "text-sm text-white whitespace-pre-wrap",
                        "{value}"
                    }
                }
            }
        } else {
            rsx! {
                div {
                    "x-init": "{SCRIPT}", 
                    class: "mt-auto bg-gray-200 dark:bg-gray-800 rounded-lg py-2 px-4 w-full",
                    p { class: "text-sm text-black dark:text-white whitespace-pre-wrap",
                        "{value}"
                    }
                }
            }
        }
    )

}


#[inline_props]
fn MoonIcon(cx: Scope) -> Element {
    cx.render(rsx! {
        svg {
            height: "1em",
            view_box: "0 0 50 50",
            width: "1em",
            xmlns: "http://www.w3.org/2000/svg",
            path {
                d: "M 43.81 29.354 C 43.688 28.958 43.413 28.626 43.046 28.432 C 42.679 28.238 42.251 28.198 41.854 28.321 C 36.161 29.886 30.067 28.272 25.894 24.096 C 21.722 19.92 20.113 13.824 21.683 8.133 C 21.848 7.582 21.697 6.985 21.29 6.578 C 20.884 6.172 20.287 6.022 19.736 6.187 C 10.659 8.728 4.691 17.389 5.55 26.776 C 6.408 36.163 13.847 43.598 23.235 44.451 C 32.622 45.304 41.28 39.332 43.816 30.253 C 43.902 29.96 43.9 29.647 43.81 29.354 Z",
                fill: "currentColor",
            }
        }
    })
}

#[inline_props]
fn SunIcon(cx: Scope) -> Element {
    cx.render(rsx! {
        svg {
            width: "1em",
            view_box: "0 0 24 24",
            height: "1em",
            fill: "none",
            xmlns: "http://www.w3.org/2000/svg",
            circle {
                r: "5.75375",
                fill: "currentColor",
                cx: "11.9998",
                cy: "11.9998",
            }
            g {
                circle {
                    transform: "rotate(-60 3.08982 6.85502)",
                    fill: "currentColor",
                    cx: "3.08982",
                    cy: "6.85502",
                    r: "1.71143",
                }
                circle {
                    r: "1.71143",
                    cx: "3.0903",
                    cy: "17.1436",
                    transform: "rotate(-120 3.0903 17.1436)",
                    fill: "currentColor",
                }
                circle {
                    r: "1.71143",
                    cx: "12",
                    cy: "22.2881",
                    fill: "currentColor",
                }
                circle {
                    transform: "rotate(-60 20.9101 17.1436)",
                    cy: "17.1436",
                    cx: "20.9101",
                    r: "1.71143",
                    fill: "currentColor",
                }
                circle {
                    cy: "6.8555",
                    r: "1.71143",
                    fill: "currentColor",
                    cx: "20.9101",
                    transform: "rotate(-120 20.9101 6.8555)",
                }
                circle {
                    fill: "currentColor",
                    cy: "1.71143",
                    r: "1.71143",
                    cx: "12",
                }
            }
        }
    })
}

fn App(cx: Scope) -> Element {
    let text = use_state(cx, || String::with_capacity(350));
    let output = use_state(cx, String::new);    
    let messages = use_state(cx, || VecDeque::from_iter([
        r#"The available operations are:
    1-Breadth first search
    2-Dijkstra search
    3-Depth first search (optional limit)
    4-Iterative depth first search (optional limit)
    5-Bidirectional
    6-Exit"#.to_owned()
    ].into_iter()));

    let SCRIPT = r#"
const html = document.getElementsByTagName('html')[0];
if(html.classList.contains('dark')) {
    document.getElementById('tcolor').content = 'black'
    html.classList.remove('dark');
    localStorage.theme = 'light'
} else {
    document.getElementById('tcolor').content = 'rgb(31 41 55 / var(--tw-bg-opacity))'
    html.classList.add('dark');
    localStorage.theme = 'dark'
}            
    "#;

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
            class: "bg-gray-200 dark:bg-gray-800",
            div { class: "max-w-screen-lg mx-auto p-4 flex items-center space-x-5",
                button {
                    class: "bg-blue-500 dark:bg-gray-900 rounded py-2 px-4",
                    "onclick": "{SCRIPT}",
                    div {
                        class: "flex items-center space-x-3 text-white",
                        p { "Theme" }
                        div {
                            div { class: "dark:hidden block",
                                SunIcon {}
                            }
                            div { class: "hidden dark:block",
                                MoonIcon {}
                            }
                        }
                    }
                }
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
                button { class: "dark:bg-gray-900 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded",
                    onclick: move |_| {
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


