use std::{env, process};

use crate::lex::{construct_regex, regexes_to_nfa};
mod lex;
mod types;

fn main() {
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

    let r = construct_regex();
    let _ = regexes_to_nfa(r);
}
