#![warn(clippy::pedantic, clippy::cargo)]
#![allow(clippy::too_many_lines)]

extern crate core;
use std::{
    env, fs,
    process::{self, Command, ExitCode},
};

extern crate goat_core;
use goat_core::{codegen_llvm, lex, parse, semantic};

#[allow(clippy::print_stderr, clippy::print_stdout, clippy::use_debug)]
fn main() -> process::ExitCode {
    let mut pretty = false;
    let mut verbose = false;
    let mut bounds_check = true;
    let mut filename = None;
    let mut out = false;
    let mut outfile = None;

    let arguments: Vec<String> = env::args().collect();
    let command = arguments.first().expect("command name");
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 {
            continue;
        }

        if argument == "--help" || argument == "-h" {
            println!("usage: {command} [-p] [-o <outfile>] source.gt");
            println!("  -p: Pretty prints Goat code");
            println!("  -o <outfile>: Use llvm to create executable");
            println!("  -v: Verbose printing of internals");
            return ExitCode::from(0);
        }
        if argument == "--disable-bounds-check" {
            bounds_check = false;
            continue;
        }
        if argument == "-p" || argument == "--pretty-print" {
            pretty = true;
            continue;
        }
        if argument == "-v" || argument == "--verbose" {
            verbose = true;
            continue;
        }
        if argument == "-o" {
            out = true;
            continue;
        }
        if out {
            outfile = Some(argument);
            out = false;
            continue;
        }

        filename = Some(argument);
    }

    let Some(filename) = filename else {
        eprintln!("Filename not given");
        return ExitCode::from(1);
    };

    if verbose {
        eprintln!("{command} invoked with source file: {filename}, pretty: {pretty}");
    }

    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Failed to read file: {e}");
            return ExitCode::from(1);
        }
    };

    let tokens = match lex::lex(&contents) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {e}");
            return ExitCode::from(2);
        }
    };
    if verbose {
        eprintln!("{tokens:?}");
    }

    let ast = match parse::parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parser error: {e}");
            return ExitCode::from(3);
        }
    };
    if verbose {
        eprintln!("{ast:?}");
    }
    if pretty {
        println!("{ast}");
    }

    let symbol_table = match semantic::analyse(&ast) {
        Ok(table) => table,
        Err(e) => {
            eprintln!("Semantic analysis error: {e}");
            return ExitCode::from(4);
        }
    };

    let output = codegen_llvm::generate_code(&ast, &symbol_table, bounds_check);
    if verbose {
        eprintln!("{output}");
    }

    if let Some(outfile) = outfile {
        let ll_path = filename.clone() + ".ll";
        if let Err(e) = fs::write(&ll_path, output) {
            eprintln!("Cannot save {ll_path}: {e}");
            return ExitCode::from(1);
        }

        let command = Command::new("clang")
            .arg("-O3")
            .arg("-o")
            .arg(outfile)
            .arg(&ll_path)
            .output();

        let command_output = match command {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Failed to execute clang: {e}");
                return ExitCode::from(5);
            }
        };

        if !command_output.status.success() {
            println!("{}", String::from_utf8_lossy(&command_output.stdout));
            println!("{}", String::from_utf8_lossy(&command_output.stderr));
            return ExitCode::from(5);
        }
    }

    ExitCode::from(0)
}
