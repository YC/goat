#![warn(clippy::pedantic, clippy::cargo)]
#![allow(clippy::too_many_lines, clippy::missing_errors_doc, clippy::must_use_candidate)]

extern crate core;

mod ast;
pub mod codegen_llvm;
pub mod lex;
pub mod parse;
pub mod semantic;
mod tokens;
