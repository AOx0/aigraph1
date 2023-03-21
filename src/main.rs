#![allow(non_snake_case)]

use dioxus::prelude::*;
use graph::*;

fn main() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Info));
    dioxus_web::launch(App);
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
            width: "1em", view_box: "0 0 24 24",
            height: "1em", fill: "none",
            xmlns: "http://www.w3.org/2000/svg",
            circle { r: "5.75375", fill: "currentColor", cx: "11.9998", cy: "11.9998" }
            g {
                circle { transform: "rotate(-60 3.08982 6.85502)", fill: "currentColor", cx: "3.08982", cy: "6.85502", r: "1.71143" }
                circle { r: "1.71143", cx: "3.0903", cy: "17.1436", transform: "rotate(-120 3.0903 17.1436)", fill: "currentColor" }
                circle { r: "1.71143", cx: "12", cy: "22.2881", fill: "currentColor" }
                circle { transform: "rotate(-60 20.9101 17.1436)", cy: "17.1436", cx: "20.9101", r: "1.71143", fill: "currentColor" }
                circle { cy: "6.8555", r: "1.71143", fill: "currentColor", cx: "20.9101", transform: "rotate(-120 20.9101 6.8555)" }
                circle { fill: "currentColor", cy: "1.71143", r: "1.71143", cx: "12" }
            }
        }
    })
}

#[inline_props]
fn Input<'a>(cx: Scope, placeholder: &'a str, value: UseState<String>) -> Element {
    cx.render(rsx!{
        div {
            class: "w-full flex",
            input { class: "flex-1 grow mx-4 !outline-none focus:outline-0 rounded-md p-2 bg-[#f6f8fa] dark:bg-[#010409] border dark:border-[#282d34]",
                spellcheck:"false",
                placeholder: "{placeholder}",
                value: "{value}",
                oninput: move |evt| {
                    value.set(evt.value.clone());
                }
            }
        }
    })
}

#[inline_props]
fn TextArea<'a>(cx: Scope, placeholder: &'a str, value: UseState<String>) -> Element {
    cx.render(rsx!{
        div {
            class: "w-full h-full flex",
            textarea { 
                style: "font-size: 12px;",
                spellcheck:"false",
                class: "flex-1 grow !outline-none font-mono resize-none focus:outline-0 ml-4 md:ml-0 mb-4 mr-4 rounded-md p-2 bg-[#f6f8fa] dark:bg-[#010409] border dark:border-[#282d34]",
                placeholder: "{placeholder}",
                value: "{value}",
                oninput: move |evt| {
                    value.set(evt.value.clone());
                }
            }
        }
    })
}

fn App(cx: Scope) -> Element {
    let SCRIPT = 
r#"const html = document.getElementsByTagName('html')[0];
if(html.classList.contains('dark')) {
    document.getElementById('tcolor').content = 'black'
    html.classList.remove('dark');
    localStorage.theme = 'light'
} else {
    document.getElementById('tcolor').content = 'rgb(31 41 55 / var(--tw-bg-opacity))'
    html.classList.add('dark');
    localStorage.theme = 'dark'
}"#;

    let graph: Option<Graph<&str, (f64, f64), u64>> = None;
    let graph_description = use_state(cx, || {
        r##"{ "nodes": [
    { "name":"Arad",
      "goestTo": [
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 }
      ]
    },
    { "name":"Arad",
      "goestTo": [
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 },
          { "name": "Sibiu", "weight": 5 }
      ]
    }
]}"##
            .to_owned()
    });
    let entrada = use_state(cx, || String::new());
    let salida = use_state(cx, || String::new());
    let metodo = use_state(cx, || String::new());

    cx.render(rsx! {
        div {
            class: "py-5 flex justify-between mx-5 items-center",
            h1 {
                class: "text-lg md:text-2xl font-bold",
                "aigraph1/"
            }
            button {
                class: "bg-[#f6f8fa] dark:bg-gray-900 rounded py-2 px-4",
                "onclick": "{SCRIPT}",
                div {
                    class: "flex items-center space-x-3 text-black dark:text-white",
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
        }
        div {
            class: "flex flex-col md:flex-row sm:flex-col w-full h-full space-y-5 sm:space-y-5 md:space-y-0 placeholder:text-[#c9d1d9] text-[#24292f] dark:text-[#c9d1d9]",
            div {
                class: "flex flex-col text-sm items-center justify-start w-full md:w-1/3 sm:w-full space-y-5",
                Input { placeholder: "Starting edge...", value: entrada.clone() }
                Input { placeholder: "Ending edge...", value: salida.clone() }
                Input { placeholder: "Search method...", value: metodo.clone() }
            }
            div {
                class: "w-full h-full md:w-2/3 sm:w-full",
                TextArea { placeholder: "Graph description...", value: graph_description.clone() }
            }
        }
    })
}
