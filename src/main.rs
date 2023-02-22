use graph::*;
use text_io::try_read;

macro_rules! printf {
    ( $($t:tt)* ) => {
        {
            use std::io::Write;
            let mut h = std::io::stdout();
            write!(h, $($t)* ).unwrap();
            h.flush().unwrap();
        }
    }
}

fn main() {
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

    println!("Welcome! To cancel on any point type 'end' or 'e'");
    println!("Available cities:");
    graph
        .nodes
        .keys()
        .enumerate()
        .for_each(|(i, city)| print!("{city:>25}{}", if i % 5 == 0 { '\n' } else { ' ' }));
    println!();

    let with_costs = 'a: {
        for _ in 0..3 {
            printf!("Do you want a graph with weights? [Y/N]: ");
            let answer: Result<String, _> = try_read!("{}");

            if answer.is_ok() {
                let answer = answer.unwrap().to_lowercase();
                let answer = answer.trim();
                if ["end", "e"].contains(&answer) {
                    return;
                } else if ["yes", "y"].contains(&answer) {
                    break 'a true;
                } else if ["no", "n"].contains(&answer) {
                    break 'a false;
                }
            }

            println!("Error: Please write a valid input: y, yes, n, no");
            continue;
        }
        println!("Too many tries. Ending the application");
        return;
    };

    let city_start = 'a: {
        for _ in 0..3 {
            printf!("Select a Starting city: ");
            let answer: Result<String, _> = try_read!("{}\n");

            if answer.is_ok() {
                let answer = answer.unwrap().to_lowercase();
                let answer = answer.trim().to_owned();
                if ["end", "e"].contains(&answer.as_str()) {
                    return;
                }
                if graph.name_index(&answer).is_some() {
                    break 'a answer;
                }
            }
            println!("Error: Please write a valid city name");
            continue;
        }
        println!("Too many tries. Ending the application");
        return;
    };

    let city_end = 'a: {
        for _ in 0..3 {
            printf!("Select a Goal city: ");
            let answer: Result<String, _> = try_read!("{}\n");

            if answer.is_ok() {
                let answer = answer.unwrap().to_lowercase();
                let answer = answer.trim().to_owned();
                if ["end", "e"].contains(&answer.as_str()) {
                    return;
                }
                if graph.name_index(&answer).is_some() {
                    break 'a answer;
                }
            }
            println!("Error: Please write a valid city name");
            continue;
        }
        println!("Too many tries. Ending the application");
        return;
    };

    if !with_costs {
            let result = graph.breadth_first(&city_start.as_str(), Some(&city_end.as_str()));
            if let Ok(step) = result {
                println!("Since there's no weights fallbacking to breadth_first. The shortest route is:");
                for city in step {
                    println!("    {}", graph.index_name(city.idx).unwrap());
                }
            } else {
                println!("There's no route from {} to {}", city_start, city_end);
            }
    return;
    }

    println!(
        r#"
The available operations are:
    1-Breadth first search
    2-Dijkstra search
    3-Depth first search (optional limit)
    4-Iterative depth first search (optional limit)
    5-Exit"#
    );
    
    let option = 'a: {
        for _ in 0..3 {
            printf!("Select an option [1-5]: ");
            let answer: Result<u8, _> = try_read!("{}\n");

            if answer.is_ok() {
                let answer = answer.unwrap();
                if answer > 6 || answer < 1 {
                    println!("Error: Please write a number in range 1 to 5");
                    if answer == 5 {
                        return;
                    }
                } else {
                    break 'a answer;
                }
            } else {
                println!("Error: Please write a valid number");
            }
            continue;
        }
        println!("Too many tries. Ending the application");
        return;
    };

    match option {
        1 => {
            let result = graph.breadth_first(&city_start.as_str(), Some(&city_end.as_str()));
            if let Ok(step) = result {
                println!("The shortest route is:");
                for city in step {
                    println!("{}", graph.index_name(city.idx).unwrap());
                }
            } else {
                println!("There's no route from {} to {}", city_start, city_end);
            }
        },
        2 => {
            let result = graph.dijkstra(&city_start.as_str(), Some(&city_end.as_str()), |s| *s);
            if let Ok(step) = result {
                let mut i = 0;
                for city in step {
                    if i == 0 {
                        println!("With a cost of {:?}, the cheapest route is:", city.state);
                        i = 1;
                    }
                    println!("    {}", graph.index_name(city.idx).unwrap());
                }
            } else {
                println!("There's no route from {} to {}", city_start, city_end);
            }
            
        }
        3 => {
            let want_limit = 'a: {
                for _ in 0..3 {
                    printf!("Do you want a limit? [Y/N]: ");
                    let answer: Result<String, _> = try_read!("{}");

                    if answer.is_ok() {
                        let answer = answer.unwrap().to_lowercase();
                        let answer = answer.trim();
                        if ["end", "e"].contains(&answer) {
                            return;
                        } else if ["yes", "y"].contains(&answer) {
                            break 'a true;
                        } else if ["no", "n"].contains(&answer) {
                            break 'a false;
                        }
                    }

                    println!("Error: Please write a valid input: y, yes, n, no");
                    continue;
                }
                println!("Too many tries. Not using a limit");
                false
            };

            let limit: Option<u8> = want_limit.then(|| 'a: {
                for _ in 0..3 {
                    printf!("Input a limit [1-255] or 0 to use no limit: ");
                    let answer: Result<u8, _> = try_read!("{}\n");

                    if answer.is_ok() {
                        let answer = answer.unwrap();
                        if answer <= 0 {
                            println!("Error: Please write a number bigger than 0");
                            if answer == 0 {
                                break 'a None;
                            }
                        } else {
                            break 'a Some(answer);
                        }
                    } else {
                        println!("Error: Please write a valid limit, up to 255");
                    }
                    continue;
                }
                println!("Too many tries. Not using a limit");
                None
            }).flatten();

            let result = graph.depth_first(&city_start.as_str(), Some(&city_end.as_str()), limit);
            if let Ok(step) = result {
                println!("The algorithm found the route:");
                for city in step {
                    println!("    {}", graph.index_name(city.idx).unwrap());
                }
            } else {
                if let Some(limit) = limit {
                    println!("There's no route from {} to {} with a limit of {}", city_start, city_end, limit);
                } else {
                    println!("There's no route from {} to {} with no limit", city_start, city_end);
                }
            }
        
        }
        4 => {
    
            let want_limit = 'a: {
                for _ in 0..3 {
                    printf!("Do you want a limit? [Y/N]: ");
                    let answer: Result<String, _> = try_read!("{}");

                    if answer.is_ok() {
                        let answer = answer.unwrap().to_lowercase();
                        let answer = answer.trim();
                        if ["end", "e"].contains(&answer) {
                            return;
                        } else if ["yes", "y"].contains(&answer) {
                            break 'a true;
                        } else if ["no", "n"].contains(&answer) {
                            break 'a false;
                        }
                    }

                    println!("Error: Please write a valid input: y, yes, n, no");
                    continue;
                }
                println!("Too many tries. Not using a limit");
                false
            };

            let limit: Option<u8> = want_limit.then(|| 'a: {
                for _ in 0..3 {
                    printf!("Input a limit [1-255] or 0 to use no limit: ");
                    let answer: Result<u8, _> = try_read!("{}\n");

                    if answer.is_ok() {
                        let answer = answer.unwrap();
                        if answer <= 0 {
                            println!("Error: Please write a number bigger than 0");
                            if answer == 0 {
                                break 'a None;
                            }
                        } else {
                            break 'a Some(answer);
                        }
                    } else {
                        println!("Error: Please write a valid limit, up to 255");
                    }
                    continue;
                }
                println!("Too many tries. Not using a limit");
                None
            }).flatten();

            let result = graph.iterative_depth_first(&city_start.as_str(), Some(&city_end.as_str()), limit);
            if let Ok(step) = result {
                println!("The algorithm found the route:");
                for city in step {
                    println!("    {}", graph.index_name(city.idx).unwrap());
                }
            } else {
                if let Some(limit) = limit {
                    println!("There's no route from {} to {} with a limit of {}", city_start, city_end, limit);
                } else {
                    println!("There's no route from {} to {} with no limit", city_start, city_end);
                }
            }
        },
        _ => {}
    }
}
