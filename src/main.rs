#![allow(non_snake_case)]

use graph::petgraph::visit::{EdgeRef, IntoEdgeReferences};
use graph::{test_graph, test_graph2, DefaultIx, Directed, EdgeType, IndexType, Node, PGraph};
use leptos::*;

fn main() {
    mount_to_body(|cx| view! { cx,  <SimpleCounter initial_value=3 /> })
}

use plotters::{coord::Shift, prelude::*};

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

/// Based on
///     - https://github.com/grantshandy/fdg/blob/main/fdg-img/src/lib.rs
///     - https://github.com/wolfjagger/trope-correlate/blob/master/trope-web/src/plot/fdg_img_custom.rs
/// Generate an image from a graph and a force.
pub fn gen_image<E, Ty: EdgeType, Ix: IndexType, Backend: DrawingBackend>(
    mut graph: PGraph<Node, E, Ty, Ix>,
    drawing_area: &DrawingArea<Backend, Shift>,
    settings: Option<Settings>,
) -> Result<(), DrawingAreaErrorKind<Backend::ErrorType>> {
    // set up the simulation and settings
    let settings = settings.unwrap_or_default();

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
    if let Some(text_style) = settings.text_style {
        for node in graph.node_weights() {
            let pos = (
                node.location.x as i32 + (text_style.font.get_size() / 2.0) as i32,
                node.location.y as i32,
            );
            drawing_area.draw_text(node.name.as_str(), &text_style, pos)?;
        }
    }

    Ok(())
}

pub struct SvgPlot {
    pub size: (u32, u32),
    pub plot_type: PGraph<Node, (), Directed, DefaultIx>,
}

impl SvgPlot {
    pub fn print_to_string(&self) -> String {
        let mut svg_str = String::new();
        let backend = SVGBackend::with_string(&mut svg_str, self.size);

        // Get svg backend and associate it with svg_str
        self.draw_on_backend(backend)
            .expect("draw_on_backend error");

        svg_str
    }

    /// Draw power function on the given backend
    fn draw_on_backend<Backend>(
        &self,
        backend: Backend,
    ) -> Result<(), DrawingAreaErrorKind<Backend::ErrorType>>
    where
        Backend: plotters::prelude::DrawingBackend,
    {
        let size = backend.get_size();
        let root = backend.into_drawing_area();
        // let text_style = Some(("sans-serif", 20.).into_text_style(&root));
        root.fill(&WHITE)?;

        // generate svg text for your graph
        let settings = Some(Settings {
            // text_style,
            size,
            ..Settings::default()
        });
        gen_image(self.plot_type.clone(), &root, settings).unwrap();

        // Present changes to the backend
        root.present()?;

        Ok(())
    }
}

#[component]
pub fn SimpleCounter(cx: Scope, initial_value: i32) -> impl IntoView {
    let elem_ref = create_node_ref(cx);

    create_effect(cx, move |_| {
        if let Some(elem) = elem_ref.get() {
            request_animation_frame(move || {
                let doc = document().get_element_by_id("svg-container").unwrap();
                let child = doc.children().get_with_index(0).unwrap();
                child.set_attribute("height", "100%");
                child.set_attribute("width", "100%");
                // child.set_attribute("preserveAspectRatio", "none");
                let rect = child.children().get_with_index(0).unwrap();
                rect.set_attribute("fill", "none");
            });
        }
    });

    let img = SvgPlot {
        size: (500, 500),
        plot_type: test_graph2().repr,
    }
    .print_to_string();
    // create user interfaces with the declarative `view!` macro
    view! {
        cx,
        <div class="flex space-y-5 items-center h-full w-full">
            <div _ref=elem_ref id="svg-container" inner_html=img class="c-block justify-items-center flex w-full h-full"/>
        </div>
    }
}
