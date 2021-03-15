use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use argparse;
use futures::task::SpawnExt;

use rjt::bc::refer::JavaReference;
use rjt::jarparse::{JarContents, MissingReferenceSummary};
use std::hash::Hash;

struct Args {
    command: String,
    paths: Vec<String>,
    parallelism: usize,
    cache: bool,
    references: bool,
    full: bool,
}

fn main() {
    let mut args = Args {
        command: String::new(),
        paths: vec![],
        parallelism: 8,
        cache: false,
        references: false,
        full: false,
    };
    {
        let mut parser = argparse::ArgumentParser::new();
        parser.set_description("Analyze jar files.");

        parser
            .refer(&mut args.command)
            .add_argument("command", argparse::Store, "Command to run. [dump, check, compare-missing]")
            .required();

        parser.refer(&mut args.parallelism).add_option(
            &["-p", "--parallelism"],
            argparse::Store,
            "Number of threads to use in parallel for intensive operations.",
        );

        parser.refer(&mut args.cache).add_option(
            &["--cache"],
            argparse::StoreTrue,
            "Enable cache. Note that while this seems like a good idea, in practice many \
            operations may actually be faster without the extra serialization/deserialization \
            overhead of using the cache.",
        );

        parser.refer(&mut args.full).add_option(
            &["--full"],
            argparse::StoreTrue,
            "Show the 'full' output rather than an abbreviated summary.",
        );

        parser.refer(&mut args.references).add_option(
            &["-r", "--references"],
            argparse::StoreTrue,
            "Parse references in addition to the public api.",
        );

        parser.refer(&mut args.paths).add_argument(
            "paths",
            argparse::Collect,
            "File paths to perform operations on.",
        );

        parser.parse_args_or_exit();
    }

    match &args.command as &str {
        "dump" => {
            load_and_dump_jar_contents(&args);
        }
        "check" => {
            check_references(&args);
        }
        "compare-missing" => {
            missing_reference_delta(&args);
        }
        s => {
            println!("Unrecognized command '{}'. Run with --help for usage.", s);
            std::process::exit(1);
        }
    }
}

fn check_references(args: &Args) {
    if args.paths.len() != 1 {
        println!("Please provided exactly one jar path.");
        std::process::exit(1);
    }

    let jar = JarContents::load_jars(
        args.paths[0].split(":").collect(), args.parallelism, true);

    if let Err(e) = jar {
        println!("Error loading jar: {:#?}", e);
        std::process::exit(1);
    }
    let jar = Arc::new(jar.unwrap());
    let missing_references = jar.find_missing_references(true);
    display_missing_references(&missing_references, args.full);
}

fn missing_reference_delta(args: &Args) {
    if args.paths.len() != 2 {
        println!("Please provide exactly two jar paths.");
        std::process::exit(1);
    }

    let threadpool = futures::executor::ThreadPoolBuilder::new()
        .create()
        .expect("Unable to construct threadpool.");

    let parallelism = args.parallelism;

    let path1 = String::from(&args.paths[0]);
    let jar1 = threadpool.spawn_with_handle(async move {
        let path1 = &path1;
        JarContents::load_jars(path1.split(":").collect(), parallelism, true)
    });

    let path2 = String::from(&args.paths[1]);
    let jar2 = threadpool.spawn_with_handle(async move {
        let path2 = &path2;
        JarContents::load_jars(path2.split(":").collect(), parallelism, true)
    });

    let jar1 = jar1.expect("Unable to spawn jar1 process.");
    let jar2 = jar2.expect("Unable to spawn jar2 process.");

    let jar1 = futures::executor::block_on(jar1);
    if let Err(e) = jar1 {
        println!("Error loading jar: {:#?}", e);
        std::process::exit(1);
    }
    let jar1 = jar1.unwrap();

    let jar2 = futures::executor::block_on(jar2);
    if let Err(e) = jar2 {
        println!("Error loading jar: {:#?}", e);
        std::process::exit(1);
    }
    let jar2 = jar2.unwrap();

    let jar1 = Arc::new(jar1);
    let jar2 = Arc::new(jar2);

    println!("Finding missing reference in jar1.");
    let jar1_missing = jar1.find_missing_references(true);

    println!("Finding missing reference in jar2.");
    let jar2_missing = jar2.find_missing_references(true);

    println!("Calculating diff ...");
    let missing_diff = jar1_missing.difference(&jar2_missing);

    println!("Invalid references in (2) that were not in (1):");
    display_missing_references(&missing_diff, args.full);
}

fn display_missing_references(missing_references: &MissingReferenceSummary, full: bool) {
    println!("\nMissing classes:");
    display_map_of_sets(
        &missing_references.classes,
        full,
        "reference",
        "references",
    );

    println!("\nMissing methods:");
    display_map_of_sets(
        &missing_references.methods,
        full,
        "reference",
        "references",
    );

    println!("\nMissing fields:");
    display_map_of_sets(
        &missing_references.fields,
        full,
        "reference",
        "references",
    );

    println!("\nPackages with missing classes:");
    for (package_name, _) in sorted_map_entries(&missing_references.packages) {
        println!("{}", package_name);
    }

    if !missing_references.dynamic.is_empty() {
        if missing_references.dynamic.len() == 1 {
            println!(
                "\nAdditionally, there was 1 potential invalid reflective reference."
            );
        } else {
            println!(
                "\nAdditionally, there were {} potential invalid reflective references.",
                missing_references.dynamic.len()
            );
        }
        println!("These might be false positives.");
        display_map_of_sets(
            &missing_references.dynamic,
            full,
            "reference",
            "references",
        );
    }
}

fn invert_map_of_sets<T, V>(input_map: &HashMap<T, HashSet<V>>) -> HashMap<V, HashSet<T>>
    where T: Clone + Eq + Hash, V: Clone + Eq + Hash {
    let mut map: HashMap<V, HashSet<T>> = HashMap::new();
    for (key, items) in input_map.iter() {
        for value in items {
            if let Some(set) = map.get_mut(value) {
                set.insert(key.clone());
            } else {
                let mut set = HashSet::new();
                set.insert(key.clone());
                map.insert(value.clone(), set);
            }
        }
    }
    map
}

fn display_map_of_sets(
    map: &HashMap<String, HashSet<String>>,
    full: bool,
    item_name_singular: &str,
    item_name_plural: &str) {
    let map = if full {
        invert_map_of_sets(map)
    } else {
        map.clone()
    };
    for (key_name, values) in sorted_map_entries(&map) {
        print!("{}", key_name);
        if full {
            println!();
            let mut sorted_values: Vec<_> = values.iter().collect();
            sorted_values.sort();
            for r in sorted_values {
                println!("  -> {}", r);
            }
            continue;
        }
        println!("  ({})", named_count(values.len(), item_name_singular, item_name_plural));
    }
}

fn named_count(count: usize, singular: &str, plural: &str) -> String {
    format!("{} {}", count, if count == 1 { singular } else { plural })
}

fn sorted_map_entries<'a, K, V>(map: &'a HashMap<K, V>) -> Vec<(&'a K, &'a V)>
    where K: std::cmp::Eq,
          K: std::hash::Hash,
          K: std::cmp::Ord,
          K: Clone {
    let mut keys: Vec<&'a K> = map.keys().collect();
    keys.sort();
    let mut result: Vec<(&'a K, &'a V)> = vec![];
    for key in keys {
        result.push((key, map.get(key).unwrap()));
    }
    result
}

fn load_and_dump_jar_contents(args: &Args) {
    if args.paths.len() != 1 {
        println!("Please provided exactly one jar path.");
        std::process::exit(1);
    }

    let jar = JarContents::load_jars(
        args.paths[0].split(":").collect(), args.parallelism, args.references);
    match jar {
        Err(e) => {
            println!("Error loading jar: {}", e.to_string());
        }
        Ok(contents) => {
            let contents = contents.into_combined();
            let string_constants = &contents.string_constants;

            println!("\n=== Public API ===");
            for class in &contents.api.classes {
                println!("Class: {}", class.class_name(string_constants));
                for method in &class.methods {
                    println!("  +{}", method.signature(string_constants))
                }
            }

            println!("\n=== Service Interfaces ===");
            for interface in &contents.api.service_interfaces.interfaces() {
                println!("{}", interface);
                for implementation in &contents.api.service_interfaces.implementations(interface) {
                    println!("  - {}", implementation);
                }
            }

            println!("\n=== References ===");
            let mut references = contents.references.clone();
            references.sort();
            for reference in &references {
                println!("{}", reference.to_string(string_constants));
            }

            println!("\n=== Main Class ===");
            println!("{}", contents.manifest.get("Main-Class").unwrap_or(""));
        }
    };
}
