# Goat
A compiler for Goat, a procedual language from the assignments of
COMP90045 - Programming Language Implementation,
[Semester 1 2019](https://handbook.unimelb.edu.au/2019/subjects/comp90045/print).

This is my implementation in Rust, to relearn some compiler concepts.

## Implementation
- [x] Lexer
  - [x] Regular Expression to NFA
  - [x] NFA to DFA (via subset construction)
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

## Performance
NFA vs DFA for Lexer
```
Benchmark 1: ./test.sh ../target/release/goat-dfa
  Time (mean ± σ):      4.698 s ±  0.030 s    [User: 2.368 s, System: 2.401 s]
  Range (min … max):    4.657 s …  4.753 s    10 runs

Benchmark 2: ./test.sh ../target/release/goat-nfa
  Time (mean ± σ):      5.538 s ±  0.111 s    [User: 3.147 s, System: 2.437 s]
  Range (min … max):    5.414 s …  5.775 s    10 runs

Summary
  './test.sh ../target/release/goat-dfa' ran
    1.18 ± 0.02 times faster than './test.sh ../target/release/goat-nfa'
```
