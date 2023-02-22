use criterion::{black_box, criterion_group, criterion_main, Criterion};
use graph::*;

fn test_graph() -> Graph<&'static str, (), u16> {
    let graph: Graph<&'static str, (), u16> = graph! {
        with_node: (),
        with_edges: next,
        nodes: [
            "Acapulco",        "Villa Hermosa",      "Guanajuato",     "Cancun",
            "Chilpancingo",     "Aguaprieta",   "Alvarado",      "Valladolid",
            "Acayucan",     "Santa Ana",     "Oaxaca",    "Chetumal",
            "Tehuantepec",   "Aguascalientes",     "Atlacomulco",   "Campeche",
            "Tuxtla",      "Guadalajara",      "Queretaro",       "Felipe Carrillo Puerto",
            "Merida", "Chihuahua", "Janos", "Juarez", "Ojinaga", "Iguala", "Ciudad Altamirano",
            "Cuernavaca", "Toluca de Lerdo", "Zihuatanejo", "Ciudad del Carmen", "Ciudad Obregon",
            "Guaymas", "Ciudad Victoria", "Matamoros", "Soto la Marina", "Tampico", "Colima",
            "Morelia", "Playa Azul", "Cordoba", "Veracruz", "Culiacan", "Hidalgo del Parral",
            "Topolobampo", "Durango", "Mazatlan", "Torreon", "Ensenada", "San Quintin" , "Francisco Escarcega",
            "Manzanillo", "Salamanca", "Hermosillo", "San Luis Potosi", "Izucar de Matamoros", "La Paz",
            "Cabo San Lucas", "Reynosa", "Mexicalli", "San Felipe", "Tijuana", "Ciudad de Mexico", "Pachuca de Soto",
            "Puebla", "Tlaxcala", "Monclova", "Piedras Negras", "Monterrey", "Nuevo Laredo" , "Puerto Angel",
            "Tehuacan", "Tuxpan de Rodriguez Cano", "Pinotepa Nacional", "Zacatecas", "Santa Rosalia", "Santo Domingo", "Tepic", "Ciudad Juarez"

        ],
        connections: [
            "Cancun" => {(90) "Valladolid", (100) "Felipe Carrillo Puerto"},
            "Valladolid" => {(90) "Felipe Carrillo Puerto"},
            "Felipe Carrillo Puerto" => {(60) "Campeche"},
            "Campeche" => {(90) "Merida", (100) "Chetumal", (90) "Ciudad del Carmen"},
            "Chetumal" => {(111) "Francisco Escarcega"},
            "Ciudad del Carmen" => {(90) "Villa Hermosa", (90) "Tuxtla"},
            "Villa Hermosa" => {(90) "Acayucan"},
            "Tuxtla" => {(90) "Acayucan"},
            "Acayucan" => {(80) "Tehuantepec", (110) "Alvarado"},
            "Alvarado" => {(100) "Oaxaca"},
            "Oaxaca" => {(80) "Tehuacan", (90) "Puerto Angel", (90) "Izucar de Matamoros"},
            "Puerto Angel" => {(100) "Pinotepa Nacional" },
            "Izucar de Matamoros" => {(90) "Puebla", (100) "Cuernavaca"},
            "Pinotepa Nacional" => {(100) "Acapulco"},
            "Cuernavaca" => {(100) "Ciudad de Mexico", (100) "Ciudad Altamirano"},
            "Puebla" => {(90) "Ciudad de Mexico", (80) "Cordoba"},
            "Acapulco" => {(140) "Chilpancingo"},
            "Ciudad de Mexico" => {(100) "Tlaxcala", (110) "Toluca de Lerdo", (90) "Queretaro", (100) "Pachuca de Soto"},
            "Ciudad Altamirano" => {(90) "Zihuatanejo"},
            "Cordoba" => {(90) "Veracruz"},
            "Chilpancingo" => {(90) "Iguala"},
            "Toluca de Lerdo" => {(100) "Ciudad Altamirano"},
            "Queretaro" => {(90) "Atlacomulco", (90) "Salamanca", (90) "San Luis Potosi"},
            "Pachuca de Soto" => {(110) "Tuxpan de Rodriguez Cano"},
            "Zihuatanejo" => {(90) "Playa Azul"},
            "Iguala" => {(100) "Cuernavaca", (110) "Ciudad Altamirano"},
            "Salamanca" => {(90) "Guanajuato", (90) "Guadalajara"},
            "San Luis Potosi" => {(90) "Zacatecas", (70) "Durango", (100) "Aguascalientes" },
            "Tuxpan de Rodriguez Cano" => {(100) "Tampico"},
            "Playa Azul" => {(100) "Morelia", (100) "Colima", (100) "Manzanillo"},
            "Guanajuato" => {(80) "Aguascalientes"},
            "Guadalajara" => {(110) "Tepic"},
            "Aguascalientes" =>{(70) "Guadalajara"},
            "Durango" => {(90) "Hidalgo del Parral", (90) "Mazatlan"},
            "Tampico" => {(80) "Ciudad Victoria"},
            "Morelia" => {(90) "Salamanca"},
            "Manzanillo" => {(50) "Colima", (80) "Guadalajara"},
            "Colima" => {(90) "Morelia", (50) "Guadalajara"},
            "Tepic" =>{(50) "Mazatlan"},
            "Hidalgo del Parral" => {(130) "Chihuahua", (110) "Topolobampo", (80) "Culiacan"},
            "Mazatlan" => {(90) "Culiacan"},
            "Ciudad Victoria" => {(80) "Soto la Marina", (80) "Matamoros", (80) "Monterrey", (80) "Durango"},
            "Chihuahua" => {(90) "Ciudad Juarez", (90) "Janos"},
            "Topolobampo" => {(90) "Ciudad Obregon"},
            "Culiacan" => {(110) "Topolobampo"},
            "Matamoros" => {(90) "Reynosa"},
            "Monterrey" => {(110) "Nuevo Laredo",(70) "Monclova"},
            "Janos" => {(110) "Aguaprieta"},
            "Ciudad Obregon" => {(80) "Guaymas"},
            "Reynosa" => {(100) "Nuevo Laredo"},
            "Nuevo Laredo" => {(100) "Piedras Negras"},
            "Monclova" => {(100) "Torreon", (90) "Ojinaga"},
            "Aguaprieta" => {(90) "Santa Ana"},
            "Guaymas" => {(90) "Hermosillo"},
            "Piedras Negras" => {(90) "Monclova"},
            "Torreon" => {(90) "Durango"},
            "Ojinaga" => {(90) "Chihuahua"},
            "Santa Ana" => {(159) "Mexicalli"},
            "Hermosillo" => {(100) "Santa Ana"},
            "Mexicalli" => {(50) "Tijuana", (70) "San Felipe"},
            "Tijuana" => {(30) "Ensenada"},
            "San Felipe" => {(50) "Ensenada"},
            "Ensenada" => {(90) "San Quintin"},
            "San Quintin" => {(140) "Santa Rosalia"},
            "Santa Rosalia" => {(100) "Santo Domingo"},
            "Santo Domingo" => {(100) "La Paz"},
            "La Paz" => {(40) "Cabo San Lucas"}
        ]
    };
    graph
}

fn breadth_test1(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("breadth_full", |a| {
        a.iter(|| {
            graph
                .breadth_first_impl(
                    black_box(graph.name_index("Cancun").unwrap()),
                    Some(10000.into()),
                )
                .ok()
        })
    });
}

fn breadth_test2(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("breadth_arad_neamt", |a| {
        a.iter(|| {
            graph
                .breadth_first_impl(
                    black_box(graph.name_index("Cancun").unwrap()),
                    black_box(Some(graph.name_index("Cabo San Lucas").unwrap())),
                )
                .ok()
        })
    });
}

fn depth_test1(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("depth_full", |a| {
        a.iter(|| {
            graph
                .depth_first_impl::<u32>(
                    black_box(graph.name_index("Cancun").unwrap()),
                    Some(10000.into()),
                    None,
                )
                .ok()
        })
    });
}

fn depth_test2(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("depth_arad_neamt", |a| {
        a.iter(|| {
            graph
                .depth_first_impl::<u32>(
                    black_box(graph.name_index("Cancun").unwrap()),
                    black_box(Some(graph.name_index("Cabo San Lucas").unwrap())),
                    None,
                )
                .ok()
        })
    });
}

fn dijkstra_test1(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("dijkstra_full", |a| {
        a.iter(|| {
            graph
                .dijkstra_impl(
                    black_box(graph.name_index("Cancun").unwrap()),
                    Some(10000.into()),
                    |e| *e,
                )
                .ok()
        })
    });
}

fn dijkstra_test2(c: &mut Criterion) {
    let graph = black_box(test_graph());
    c.bench_function("dijkstra_arad_neamt", |a| {
        a.iter(|| {
            graph
                .dijkstra_impl(
                    black_box(graph.name_index("Cancun").unwrap()),
                    black_box(Some(graph.name_index("Cabo San Lucas").unwrap())),
                    |e| *e,
                )
                .ok()
        })
    });
}

fn construct_graph(c: &mut Criterion) {
    c.bench_function("graph_construct", |a| {
        a.iter(|| {
            test_graph();
        })
    });
}

criterion_group!(
    benches,
    breadth_test1,
    breadth_test2,
    depth_test1,
    depth_test2,
    dijkstra_test1,
    dijkstra_test2,
    construct_graph
);

criterion_main!(benches);
