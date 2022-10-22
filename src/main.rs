use crate::lex::lex;
use crate::parse::parse;
use std::{env, error::Error, process};

mod ast;
mod lex;
mod parse;
mod types;

fn main() -> Result<(), Box<dyn Error>> {
    let mut pretty = false;
    let mut filename = None;

    let arguments: Vec<String> = env::args().collect();
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 {
            continue;
        }

        if argument == "--help" {
            println!("usage: {} [-p] input", arguments[0]);
            process::exit(0);
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
        }
        Some(value) => value,
    };

    println!("{} invoked with file: {}, pretty: {}", arguments[0], filename, pretty);

    let contents = std::fs::read_to_string(filename).expect("cannot read from file");

    let tokens = lex(&contents)?;
    println!("{:?}", tokens);

    let parsed = parse(&tokens);
    println!("{:?}", parsed);

    Ok(())
}
