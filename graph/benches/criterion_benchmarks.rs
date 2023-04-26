#![allow(dead_code)]

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use graph::walkers::*;

fn breadth_test1(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("breadth_full", |a| {
        a.iter(|| {
            graph
                .perform_search(BreadthFirst::new(
                    &graph,
                    graph.journey("Cancun", None).unwrap(),
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn breadth_test2(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("breadth_arad_neamt", |a| {
        a.iter(|| {
            graph
                .perform_search(BreadthFirst::new(
                    &graph,
                    graph.journey("Cancun", Some("Cabo San Lucas")).unwrap(),
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn breadth_test3(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    let mut group = c.benchmark_group("Evolution breadth_first");
    for (depth, city) in [
        (26, "Cabo San Lucas"),
        (25, "La Paz"),
        (24, "Santo Domingo"),
        (23, "Santa Rosalia"),
        (22, "San Quintin"),
        (21, "Ensenada"),
        (20, "San Felipe"),
        (19, "Mexicalli"),
        (18, "Santa Ana"),
        (17, "Agua Prieta"),
        (16, "Janos"),
        (15, "Chihuahua"),
        (14, "Hidalgo del Parral"),
        (13, "Durango"),
        (12, "San Luis Potosi"),
        (11, "Queretaro"),
        (10, "Ciudad de Mexico"),
        (9, "Cuernavaca"),
        (8, "Izucar de Matamoros"),
        (7, "Oaxaca"),
        (6, "Alvarado"),
        (5, "Acayucan"),
        (4, "Tuxtla"),
        (3, "Ciudad del Carmen"),
        (2, "Campeche"),
        (1, "Felipe Carrillo Puerto"),
        (0, "Cancun"),
    ] {
        group.throughput(criterion::Throughput::Elements(depth));
        group.bench_with_input(BenchmarkId::from_parameter(depth), city, |b, name| {
            b.iter(|| {
                graph
                    .perform_search(BreadthFirst::new(
                        &graph,
                        graph.journey("Cancun", Some(name)).unwrap(),
                        Direction::Outgoing,
                    ))
                    .unwrap()
            })
        });
    }
}
fn depht_test3(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    let mut group = c.benchmark_group("Evolution depth_first");
    for (depth, city) in [
        (26, "Cabo San Lucas"),
        (25, "La Paz"),
        (24, "Santo Domingo"),
        (23, "Santa Rosalia"),
        (22, "San Quintin"),
        (21, "Ensenada"),
        (20, "San Felipe"),
        (19, "Mexicalli"),
        (18, "Santa Ana"),
        (17, "Agua Prieta"),
        (16, "Janos"),
        (15, "Chihuahua"),
        (14, "Hidalgo del Parral"),
        (13, "Durango"),
        (12, "San Luis Potosi"),
        (11, "Queretaro"),
        (10, "Ciudad de Mexico"),
        (9, "Cuernavaca"),
        (8, "Izucar de Matamoros"),
        (7, "Oaxaca"),
        (6, "Alvarado"),
        (5, "Acayucan"),
        (4, "Tuxtla"),
        (3, "Ciudad del Carmen"),
        (2, "Campeche"),
        (1, "Felipe Carrillo Puerto"),
        (0, "Cancun"),
    ] {
        group.throughput(criterion::Throughput::Elements(depth));
        group.bench_with_input(BenchmarkId::from_parameter(depth), city, |b, name| {
            b.iter(|| {
                graph
                    .perform_search(DepthFirst::new(
                        &graph,
                        graph.journey("Cancun", Some(name)).unwrap(),
                        None::<u8>,
                        Direction::Outgoing,
                    ))
                    .unwrap()
            })
        });
    }
}
fn dijkstra_test3(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    let mut group = c.benchmark_group("Evolution dijkstra");
    for (depth, city) in [
        (27, "Cabo San Lucas"),
        (26, "La Paz"),
        (25, "Santo Domingo"),
        (24, "Santa Rosalia"),
        (23, "San Quintin"),
        (22, "Ensenada"),
        (21, "Tijuana"),
        (20, "Mexicalli"),
        (19, "Santa Ana"),
        (18, "Agua Prieta"),
        (17, "Janos"),
        (16, "Chihuahua"),
        (15, "Hidalgo del Parral"),
        (14, "Durango"),
        (13, "San Luis Potosi"),
        (12, "Queretaro"),
        (11, "Ciudad de Mexico"),
        (10, "Puebla"),
        (9, "Izucar de Matamoros"),
        (8, "Oaxaca"),
        (7, "Alvarado"),
        (6, "Acayucan"),
        (5, "Tuxtla"),
        (4, "Ciudad del Carmen"),
        (3, "Campeche"),
        (2, "Felipe Carrillo Puerto"),
        (1, "Cancun"),
    ] {
        group.throughput(criterion::Throughput::Elements(depth));
        group.bench_with_input(BenchmarkId::from_parameter(depth), city, |b, name| {
            b.iter(|| {
                graph
                    .perform_search(dijkstra::new(
                        &graph,
                        black_box(graph.journey("Cancun", Some(name)).unwrap()),
                        |e| *e,
                        Direction::Outgoing,
                    ))
                    .unwrap()
            })
        });
    }
}

fn bidirectional_test3(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    let mut group = c.benchmark_group("Evolution bidirectional");
    for (depth, city) in [
        // expect nodes
        (27, "Cabo San Lucas"),
        (26, "La Paz"),
        (25, "Santo Domingo"),
        (24, "Santa Rosalia"),
        (23, "San Quintin"),
        (22, "Ensenada"),
        (21, "Tijuana"),
        (20, "Mexicalli"),
        (19, "Santa Ana"),
        (18, "Agua Prieta"),
        (17, "Janos"),
        (16, "Chihuahua"),
        (15, "Hidalgo del Parral"),
        (14, "Durango"),
        (13, "San Luis Potosi"),
        (12, "Queretaro"),
        (11, "Ciudad de Mexico"),
        (10, "Puebla"),
        (9, "Izucar de Matamoros"),
        (8, "Oaxaca"),
        (7, "Alvarado"),
        (6, "Acayucan"),
        (5, "Tuxtla"),
        (4, "Ciudad del Carmen"),
        (3, "Campeche"),
        (2, "Felipe Carrillo Puerto"),
        (1, "Cancun"),
    ] {
        group.throughput(criterion::Throughput::Elements(depth));
        group.bench_with_input(BenchmarkId::from_parameter(depth), city, |b, name| {
            b.iter(|| {
                let a = dijkstra::new(
                    &graph,
                    graph.journey("Cancun", Some(name)).unwrap(),
                    |e| *e,
                    Direction::Outgoing,
                );
                let b = dijkstra::new(
                    &graph,
                    graph.journey("Cancun", Some(name)).unwrap(),
                    |e| *e,
                    Direction::Incoming,
                );
                graph.bidirectional(a, b).unwrap();
            })
        });
    }
}

fn depth_test1(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("depth_full", |a| {
        a.iter(|| {
            graph
                .perform_search(DepthFirst::new(
                    &graph,
                    graph.journey("Cancun", None).unwrap(),
                    None::<u8>,
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn depth_test2(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("depth_arad_neamt", |a| {
        a.iter(|| {
            graph
                .perform_search(DepthFirst::new(
                    &graph,
                    graph.journey("Cancun", Some("Cabo San Lucas")).unwrap(),
                    None::<u8>,
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn depth_test3(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    let mut group = c.benchmark_group("Evolution breadth_first");
    for (depth, city) in [
        (40, "Cabo San Lucas"),
        (39, "La Paz"),
        (38, "Santo Domingo"),
        (37, "Santa Rosalia"),
        (36, "San Quintin"),
        (35, "Ensenada"),
        (34, "Tijuana"),
        (33, "Mexicalli"),
        (32, "Santa Ana"),
        (31, "Hermosillo"),
        (30, "Guaymas"),
        (29, "Ciudad Obregon"),
        (28, "Topolobampo"),
        (27, "Culiacan"),
        (26, "Mazatlan"),
        (25, "Tepic"),
        (24, "Guadalajara"),
        (23, "Aguascalientes"),
        (22, "Guanajuato"),
        (21, "Salamanca"),
        (20, "Morelia"),
        (19, "Playa Azul"),
        (18, "Zihuatanejo"),
        (17, "Ciudad Altamirano"),
        (16, "Toluca de Lerdo"),
        (15, "Ciudad de Mexico"),
        (14, "Cuernavaca"),
        (13, "Iguala"),
        (12, "Chilpancingo"),
        (11, "Acapulco"),
        (10, "Pinotepa Nacional"),
        (9, "Puerto Angel"),
        (8, "Oaxaca"),
        (7, "Alvarado"),
        (6, "Acayucan"),
        (5, "Villa Hermosa"),
        (4, "Ciudad del Carmen"),
        (3, "Campeche"),
        (2, "Felipe Carrillo Puerto"),
        (1, "Valladolid"),
    ] {
        group.throughput(criterion::Throughput::Elements(depth));
        group.bench_with_input(BenchmarkId::from_parameter(depth), city, |b, name| {
            b.iter(|| {
                graph
                    .perform_search(DepthFirst::new(
                        &graph,
                        graph.journey("Cancun", Some(name)).unwrap(),
                        None::<u8>,
                        Direction::Outgoing,
                    ))
                    .unwrap()
            })
        });
    }
}

fn dijkstra_test1(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("dijkstra_full", |a| {
        a.iter(|| {
            graph
                .perform_search(dijkstra::new(
                    &graph,
                    black_box(graph.journey("Cancun", None).unwrap()),
                    |e| *e,
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn dijkstra_test2(c: &mut Criterion) {
    let graph = black_box(mexico_graph());
    c.bench_function("dijkstra_arad_neamt", |a| {
        a.iter(|| {
            graph
                .perform_search(dijkstra::new(
                    &graph,
                    black_box(graph.journey("Cancun", Some("Cabo San Lucas")).unwrap()),
                    |e| *e,
                    Direction::Outgoing,
                ))
                .ok()
        })
    });
}

fn construct_graph(c: &mut Criterion) {
    c.bench_function("graph_construct", |a| {
        a.iter(|| {
            mexico_graph();
        })
    });
}

criterion_group!(
    benches,
    breadth_test1,
    breadth_test2,
    breadth_test3,
    depth_test1,
    depth_test2,
    depth_test3,
    dijkstra_test1,
    dijkstra_test2,
    dijkstra_test3,
    bidirectional_test3,
    construct_graph
);

criterion_main!(benches);
