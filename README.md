# Goat
A compiler for Goat, a procedual language from the assignments of
COMP90045 - Programming Language Implementation,
[Semester 1 2019](https://handbook.unimelb.edu.au/2019/subjects/comp90045/print).

This is my implementation in Rust, to relearn some compiler concepts.

## Implementation
- [x] Lexer
  - [x] Regular Expression to NFA
  - [ ] NFA to DFA (via subset construction)
- [x] Parser
  - [x] Recursive Descent
- [x] Semantic Analysis
- [x] Code generation
  - [x] LLVM IR
  - [ ] Oz

## Usage
Install `clang` (optional, to compile executable).

Compile the compiler:
```sh
$ cargo build --release
```

Execute:
```
$ ./goat [-p] source.gt -o <executable-name>
```
- If `-p` is specified, pretty printing of the source will be performed per stage 1 specification.
- If `-v` is specified, information determined by the lexer/parser/codegen will be printed.
- If `-o <executable-name>`, `goat` will use `clang` to compile `<source>.gt` into `<executable-name>`.

Note that this implementation currently does not support compilation to Oz,
the target language from stage 3 of the specification.

## Tests
1. Build the compiler.
2. Run the test script.
```sh
$ ./tests/test.sh target/release/goat
```
