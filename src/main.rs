#![allow(non_snake_case)]

use graph::petgraph::visit::{EdgeRef, IntoEdgeReferences};
use graph::{
    test_graph, test_graph2,
    walkers::{Walker, WalkerState},
    DefaultIx, Directed, EdgeType, IndexType, Node, PGraph,
};
use leptos::*;

fn main() {
    mount_to_body(|cx| view! { cx,  <SimpleCounter /> })
}

use graph::walkers::DepthFirst;
use graph::Direction::Outgoing;
use plotters::prelude::*;

// Note: the following replaces the same-named struct & method from fdg_img
//  but allows a custom backend

/// Parameters for drawing the SVG image.
pub struct Settings {
    /// Size of the backend
    pub size: (u32, u32),
    /// The radius of the nodes
    pub node_size: u32,
    /// RGBA color of the nodes
    pub node_color: RGBAColor,
    /// Width of the edge lines
    pub edge_size: u32,
    /// RGBA color of the edge lines
    pub edge_color: RGBAColor,
    /// RGBA background color
    pub background_color: RGBAColor,
    /// If true, the simulation will be printed on each
    pub print_progress: bool,
    /// If supplied, the names of nodes will be written
    pub text_style: Option<TextStyle<'static>>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            size: (480, 480),
            node_size: 1,
            node_color: RGBAColor(0, 0, 0, 1.0),
            edge_size: 2,
            edge_color: RGBAColor(255, 0, 0, 1.0),
            background_color: RGBAColor(255, 255, 255, 0.0),
            print_progress: false,
            text_style: None,
        }
    }
}

pub struct SvgPlot {
    pub plot_type: PGraph<Node, (), Directed, DefaultIx>,
    pub settings: Settings,
}

impl SvgPlot {
    pub fn new(
        plot_type: PGraph<Node, (), Directed, DefaultIx>,
        settings: Option<Settings>,
    ) -> Self {
        let settings = settings.unwrap_or_default();
        let mut s = Self {
            settings,
            plot_type,
        };
        s.translate_coords();
        s
    }

    pub fn print_to_string(&self) -> String {
        let mut svg_str = String::new();
        let backend = SVGBackend::with_string(&mut svg_str, self.settings.size);

        // Get svg backend and associate it with svg_str
        self.draw_on_backend(backend)
            .expect("draw_on_backend error");

        svg_str
    }

    fn translate_coords(&mut self) {
        let graph = &mut self.plot_type;
        // set up the simulation and settings
        let settings = &self.settings;

        // get the size of the graph (avg of width and height to try to account for oddly shaped graphs)
        let (top, bottom, left, right): (f32, f32, f32, f32) =
            graph.node_weights().fold((0., 0., 0., 0.), |bounds, node| {
                (
                    bounds.0.max(node.location.y),
                    bounds.1.min(node.location.y),
                    bounds.2.min(node.location.x),
                    bounds.3.max(node.location.x),
                )
            });

        let size = (settings.size.0 as f32, settings.size.1 as f32);
        let diff_x = match right - left {
            x if x > 0.1 => x,
            _ => size.0,
        };
        let diff_y = match top - bottom {
            y if y > 0.1 => y,
            _ => size.1,
        };
        let (avg_x, avg_y) = (0.5 * (left + right), 0.5 * (bottom + top));

        let image_scale = 1.1;
        let (scale_x, scale_y) = (
            (size.0 as f32) / (diff_x * image_scale),
            (size.1 as f32) / (diff_y * image_scale),
        );

        // translate all the points over into the image coordinate space
        for node in graph.node_weights_mut() {
            node.location.x = 0.5 * size.0 + scale_x * (node.location.x - avg_x);
            node.location.y = 0.5 * size.1 + scale_y * (node.location.y - avg_y);
        }
    }

    /// Based on
    ///     - https://github.com/grantshandy/fdg/blob/main/fdg-img/src/lib.rs
    ///     - https://github.com/wolfjagger/trope-correlate/blob/master/trope-web/src/plot/fdg_img_custom.rs
    /// Generate an image from a graph and a force.
    fn draw_on_backend<Backend>(
        &self,
        backend: Backend,
    ) -> Result<(), DrawingAreaErrorKind<Backend::ErrorType>>
    where
        Backend: DrawingBackend,
    {
        let drawing_area = backend.into_drawing_area();
        let settings = &self.settings;
        let graph = &self.plot_type;

        // fill in the background
        drawing_area.fill(&settings.background_color).unwrap();

        // draw all the edges
        for edge in graph.edge_references() {
            let source = &graph[edge.source()].location;
            let target = &graph[edge.target()].location;

            drawing_area.draw(&PathElement::new(
                [
                    (source.x as i32, source.y as i32),
                    (target.x as i32, target.y as i32),
                ],
                ShapeStyle {
                    color: settings.edge_color,
                    filled: true,
                    stroke_width: settings.edge_size,
                },
            ))?;
        }

        // draw all the nodes
        for node in graph.node_weights() {
            drawing_area.draw(&Circle::new(
                (node.location.x as i32, node.location.y as i32),
                settings.node_size,
                ShapeStyle {
                    color: settings.node_color,
                    filled: true,
                    stroke_width: 1,
                },
            ))?;
        }

        // draw the text by nodes
        if let Some(ref text_style) = settings.text_style {
            for node in graph.node_weights() {
                let pos = (
                    node.location.x as i32 + (text_style.font.get_size() / 2.0) as i32,
                    node.location.y as i32,
                );
                drawing_area.draw_text(node.name.as_str(), &text_style, pos)?;
            }
        }

        // Present changes to the backend
        drawing_area.present()?;

        Ok(())
    }
}

#[component]
pub fn SimpleCounter(cx: Scope) -> impl IntoView {
    let elem_ref = create_node_ref(cx);

    create_effect(cx, move |_| {
        if let Some(elem) = elem_ref.get() {
            request_animation_frame(move || {
                let svg_container = document().get_element_by_id("svg-container").unwrap();
                let child = svg_container.children().get_with_index(0).unwrap();
                child.set_attribute("height", "100%");
                child.set_attribute("width", "100%");
            });
        }
    });

    let search = move |_| {
        let graph = test_graph2();
        spawn_local(async move {
            let mut machine = DepthFirst::new(
                &graph,
                graph.journey("Cancun", Some("Cabo San Lucas")).unwrap(),
                None,
                Outgoing,
            );

            let mut res: WalkerState<usize> = machine.step();

            while !matches!(res, WalkerState::Done | WalkerState::Cutoff) {
                if let Some(step) = res.step_peek() {
                    async_std::task::sleep(std::time::Duration::from_secs_f32(0.2)).await;
                    let polyline_list = document().get_elements_by_tag_name("polyline");
                    let child = polyline_list
                        .get_with_index(step.rel.unwrap_or_default().index() as u32)
                        .unwrap();
                    child.set_attribute("stroke", "#FFFFFF");
                }

                if let WalkerState::Found(_) = res {
                    break;
                }

                res = machine.step();
            }
        })
    };

    let restart = move |_| {
        let polyline_list = document().get_elements_by_tag_name("polyline");
        for child in 0..polyline_list.length() {
            let child = polyline_list.get_with_index(child as u32).unwrap();
            child.set_attribute("stroke", "#FF0000");
        }
    };

    let img = SvgPlot::new(test_graph2().repr, None).print_to_string();
    // create user interfaces with the declarative `view!` macro
    view! {
        cx,
        <div class="flex items-center h-full w-full">
            <div class="flex flex-col space-y-5 bg-gray-900 w-full h-auto md:h-full w-1/3 p-5">
                <p class="text-xl md:text-2xl" >"aigraph1/"</p>
                <div class="h-full w-full flex flex-col items-center justify-center space-y-5">
                    <input class="dark:bg-[#0d1117] rounded p-2" placeholder="Method"/>
                    <button class="dark:bg-[#0d1117] rounded p-2" on:click=search>"Start search"</button>
                    <button class="dark:bg-[#0d1117] rounded p-2" on:click=restart>"Reset colors"</button>
                </div>
            </div>
            <div _ref=elem_ref id="svg-container" inner_html=img class="c-block justify-items-center flex w-2/3 h-full"/>
        </div>
    }
}
