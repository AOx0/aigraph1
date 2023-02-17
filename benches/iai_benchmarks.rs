use graph::*;
use iai::{black_box, Iai};

fn test_graph() -> Graph<&'static str, (), usize> {
    black_box(graph! {
        with_node: (),
        with_edges: next,
        nodes: [
            "Arad",        "Zerind",      "Oradea",     "Sibiu",
            "Fagaras",     "Timisoara",   "Lugoj",      "Mehadia",
            "Drobeta",     "Craiova",     "Pitesti",    "Rimnieu Vilcea",
            "Bucharest",   "Giurgiu",     "Urziceni",   "Hirsova",
            "Eforie",      "Vasiui",      "Iasi",       "Neamt"
        ],
        connections: [
            "Arad" => {(140) "Sibiu", (75) "Zerind", (118) "Timisoara"},
            "Zerind" => {(71) "Oradea"},
            "Oradea" => {(151) "Sibiu"},
            "Sibiu" => {(99) "Fagaras", (80) "Rimnieu Vilcea"},
            "Timisoara" => {(111) "Lugoj"},
            "Lugoj" => {(70) "Mehadia"},
            "Mehadia" => {(75) "Drobeta"},
            "Drobeta" => {(120) "Craiova"},
            "Craiova" => {(138) "Pitesti"},
            "Pitesti" => {(101) "Bucharest"},
            "Rimnieu Vilcea" => {(97) "Pitesti", (146) "Craiova"},
            "Fagaras" => {(211) "Bucharest"},
            "Bucharest" => {(90) "Giurgiu", (85) "Urziceni"},
            "Urziceni" => {(98) "Hirsova", (142) "Vasiui"},
            "Vasiui" => {(92) "Iasi"},
            "Iasi" => {(87) "Neamt"},
            "Hirsova" => {(86) "Eforie"}
        ]
    })
}

// fn dijkstra_test1(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         dijkstra(black_box(&graph), black_box("Arad"), black_box("Neamt")).unwrap();
//     });
// }

fn breadth_test1(iai: &mut Iai) {
    let graph = black_box(test_graph());
    iai.run(|| {
        graph
            .breadth_first(black_box("Arad"), black_box(Some("Neamt")))
            .unwrap();
    });
}

// fn depth_test1(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         depth_first(black_box(&graph), black_box("Arad"), black_box("Neamt")).unwrap();
//     });
// }

// fn dijkstra_test2(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         dijkstra(black_box(&graph), black_box("Arad"), black_box("")).ok();
//     });
// }

fn breadth_test2(iai: &mut Iai) {
    let graph = black_box(test_graph());
    iai.run(|| {
        graph.breadth_first(black_box("Arad"), None).ok();
    });
}

fn breadth_test3(iai: &mut Iai) {
    let graph = black_box(test_graph());
    iai.run(|| {
        graph
            .breadth_first_impl(
                black_box(graph.name_index("Arad").unwrap()),
                Some(10000.into()),
            )
            .ok();
    });
}

fn breadth_test4(iai: &mut Iai) {
    let graph = black_box(test_graph());
    iai.run(|| {
        graph
            .breadth_first_impl(
                black_box(graph.name_index("Arad").unwrap()),
                black_box(Some(graph.name_index("Neamt").unwrap())),
            )
            .ok();
    });
}

// fn depth_test2(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         depth_first(black_box(&graph), black_box("Arad"), black_box("")).ok();
//     });
// }
// fn backtracking1(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         backtracking(
//             black_box(&graph),
//             black_box("Arad"),
//             black_box("Neamt"),
//             black_box(None),
//         )
//         .unwrap();
//     });
// }
// fn backtracking2(iai: &mut Iai) {
//     let graph = black_box(test_graph());
//     iai.run(|| {
//         backtracking(
//             black_box(&graph),
//             black_box("Arad"),
//             black_box(""),
//             black_box(None),
//         )
//         .ok();
//     });
// }

fn construct_graph(iai: &mut Iai) {
    iai.run(|| {
        let _graph: Graph<&str, (), i32> = graph! {
            with_node: (),
            with_edges: next,
            nodes: [
                black_box("Arad"),        black_box("Zerind"),      black_box("Oradea"),     black_box("Sibiu"),
                black_box("Fagaras"),     black_box("Timisoara"),   black_box("Lugoj"),      black_box("Mehadia"),
                black_box("Drobeta"),     black_box("Craiova"),     black_box("Pitesti"),    black_box("Rimnieu Vilcea"),
                black_box("Bucharest"),   black_box("Giurgiu"),     black_box("Urziceni"),   black_box("Hirsova"),
                black_box("Eforie"),      black_box("Vasiui"),      black_box("Iasi"),       black_box("Neamt")
            ],
            connections: [
                black_box("Arad") => {(black_box(140)) black_box("Sibiu"), (black_box(75)) black_box("Zerind"), (black_box(118)) black_box("Timisoara")},
                black_box("Zerind") => {(black_box(71)) black_box("Oradea")},
                black_box("Oradea") => {(black_box(151)) black_box("Sibiu")},
                black_box("Sibiu") => {(black_box(99)) black_box("Fagaras"), (black_box(80)) black_box("Rimnieu Vilcea")},
                black_box("Timisoara") => {(black_box(111)) black_box("Lugoj")},
                black_box("Lugoj") => {(black_box(70)) black_box("Mehadia")},
                black_box("Mehadia") => {(black_box(75)) black_box("Drobeta")},
                black_box("Drobeta") => {(black_box(120)) black_box("Craiova")},
                black_box("Craiova") => {(black_box(138)) black_box("Pitesti")},
                black_box("Pitesti") => {(black_box(101)) black_box("Bucharest")},
                black_box("Rimnieu Vilcea") => {(black_box(97)) black_box("Pitesti"), (black_box(146)) black_box("Craiova")},
                black_box("Fagaras") => {(black_box(211)) black_box("Bucharest")},
                black_box("Bucharest") => {(black_box(90)) black_box("Giurgiu"), (black_box(85)) black_box("Urziceni")},
                black_box("Urziceni") => {(black_box(98)) black_box("Hirsova"), (black_box(142)) black_box("Vasiui")},
                black_box("Vasiui") => {(black_box(92)) black_box("Iasi")},
                black_box("Iasi") => {(black_box(87)) black_box("Neamt")},
                black_box("Hirsova") => {(black_box(86)) black_box("Eforie")}
            ]
        };
    });
}

iai::main!(
    // dijkstra_test1,
    // dijkstra_test2,
    breadth_test1,
    breadth_test2,
    breadth_test3,
    breadth_test4,
    // depth_test1,
    // depth_test2,
    // backtracking1,
    // backtracking2,
    construct_graph
);