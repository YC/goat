use crate::lex::lex;
use crate::parse::parse;
use crate::semantic::semantic_analysis;
use std::{env, error::Error, process};

mod ast;
mod lex;
mod parse;
mod semantic;
mod tokens;

fn main() -> Result<(), Box<dyn Error>> {
    let mut pretty = false;
    let mut filename = None;

    let arguments: Vec<String> = env::args().collect();
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 {
            continue;
        }

        if argument == "--help" || argument == "-h" {
            println!("usage: {} [-p] source.gt", arguments[0]);
            process::exit(0);
        }
        if argument == "-p" || argument == "--pretty-print" {
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

    eprintln!(
        "{} invoked with source file: {}, pretty: {}",
        arguments[0], filename, pretty
    );

    let contents = std::fs::read_to_string(filename).expect("cannot read from file");

    let tokens = match lex(&contents) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            process::exit(2);
        }
    };
    if pretty {
        eprintln!("{:?}", tokens);
    }

    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            process::exit(3);
        }
    };
    if pretty {
        eprintln!("{:?}", ast);
        println!("{}", ast);
    }

    match semantic_analysis(ast) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Semantic analysis error: {}", e);
            process::exit(4);
        }
    }

    Ok(())
}
