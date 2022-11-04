use crate::lex::lex;
use crate::parse::parse;
use crate::semantic::semantic_analysis;
use std::{env, fs, process};

mod ast;
mod lex;
mod parse;
mod semantic;
mod tokens;

#[allow(clippy::print_stderr, clippy::print_stdout, clippy::use_debug)]
fn main() {
    let mut pretty = false;
    let mut verbose = false;
    let mut filename = None;

    let arguments: Vec<String> = env::args().collect();
    let command = arguments.first().expect("cannot get command name");
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 {
            continue;
        }

        if argument == "--help" || argument == "-h" {
            println!("usage: {} [-p] source.gt", command);
            process::exit(0);
        }
        if argument == "-p" || argument == "--pretty-print" {
            pretty = true;
            continue;
        }
        if argument == "-v" || argument == "--verbose" {
            verbose = true;
            continue;
        }

        filename = Some(argument);
    }

    let Some(filename) = filename else {
        eprintln!("Filename not given");
        process::exit(1);
    };

    if verbose {
        eprintln!("{} invoked with source file: {}, pretty: {}", command, filename, pretty);
    }

    let contents = fs::read_to_string(filename).expect("cannot read from file");

    let tokens = match lex(&contents) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            process::exit(2);
        }
    };
    if verbose {
        eprintln!("{:?}", tokens);
    }

    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            process::exit(3);
        }
    };
    if verbose {
        eprintln!("{:?}", ast);
    }
    if pretty {
        println!("{}", ast);
    }

    match semantic_analysis(&ast) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Semantic analysis error: {}", e);
            process::exit(4);
        }
    }
}
