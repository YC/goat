#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(
    clippy::integer_arithmetic,
    clippy::arithmetic_side_effects,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    clippy::implicit_return,
    clippy::try_err,
    clippy::expect_used,
    clippy::missing_docs_in_private_items,
    clippy::wildcard_enum_match_arm,
    clippy::std_instead_of_core,
    clippy::pattern_type_mismatch,
    clippy::too_many_lines,
    clippy::panic,
    clippy::string_add
)]

use std::{
    env, fs,
    io::{self, Write},
    process::{self, Command, ExitCode, Stdio},
};

mod ast;
mod codegen_llvm;
mod lex;
mod parse;
mod semantic;
mod tokens;

#[allow(clippy::print_stderr, clippy::print_stdout, clippy::use_debug)]
fn main() -> process::ExitCode {
    let mut pretty = false;
    let mut verbose = false;
    let mut filename = None;
    let mut out = false;
    let mut outfile = None;

    let arguments: Vec<String> = env::args().collect();
    let command = arguments.first().expect("cannot get command name");
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 {
            continue;
        }

        if argument == "--help" || argument == "-h" {
            println!("usage: {} [-p] [-o <outfile>] source.gt", command);
            println!("  -p: Pretty prints Goat code");
            println!("  -o <outfile>: Use llvm to create executable");
            println!("  -v: Verbose printing of internals");
            return ExitCode::from(0);
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
        eprintln!("{} invoked with source file: {}, pretty: {}", command, filename, pretty);
    }

    let contents = fs::read_to_string(filename).expect("cannot read from file");

    let tokens = match lex::lex(&contents) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            return ExitCode::from(2);
        }
    };
    if verbose {
        eprintln!("{:?}", tokens);
    }

    let ast = match parse::parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            return ExitCode::from(3);
        }
    };
    if verbose {
        eprintln!("{:?}", ast);
    }
    if pretty {
        println!("{}", ast);
    }

    let symbol_table = match semantic::analyse(&ast) {
        Ok(table) => table,
        Err(e) => {
            eprintln!("Semantic analysis error: {}", e);
            return ExitCode::from(4);
        }
    };

    let output = codegen_llvm::generate_code(&ast, &symbol_table);
    if verbose {
        eprintln!("{}", output);
    }

    if let Some(outfile) = outfile {
        let ll_path = filename.clone() + ".ll";
        if let Err(e) = fs::write(&ll_path, output) {
            eprintln!("Cannot save {}: {}", ll_path, e);
            return ExitCode::from(1);
        }

        let command_output = Command::new("clang")
            .arg("-O3")
            .arg("-o")
            .arg(outfile)
            .arg(&ll_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to execute llvm");
        if !command_output.status.success() {
            if let Err(e) = io::stdout().write_all(&command_output.stdout) {
                eprintln!("Failed to write LLVM stdout: {}", e);
            }
            if let Err(e) = io::stderr().write_all(&command_output.stdout) {
                eprintln!("Failed to write LLVM stderr: {}", e);
            }
            return ExitCode::from(5);
        }
    }

    ExitCode::from(0)
}
