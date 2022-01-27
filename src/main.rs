use std::{env,process};
mod types;

fn main() {
    let mut pretty = false;
    let mut filename = None;

    let arguments: Vec<String> = env::args().collect();
    for (_, argument) in arguments.iter().enumerate() {
        if argument == "--help" {
            println!("usage: {} [-p] input", arguments[0]);
            process::exit(1);
        }
        if argument == "-p" {
            pretty = true;
            continue;
        }
        filename = Some(argument);
    }

    let filename = match filename {
        None => {
            eprintln!("Filename not given");
            process::exit(1);
        },
        Some(value) => value,
    };

    println!("{} invoked with file: {}, pretty: {}", arguments[0], filename, pretty);
}
