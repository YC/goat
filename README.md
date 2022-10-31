# Goat
A compiler for Goat, a procedual language from the assignments of [COMP90045 - Programming Language Implementation, Semester 1 2019](https://handbook.unimelb.edu.au/2019/subjects/comp90045/print).

This is my implementation in Rust, to relearn some compiler concepts.

## Implementation
- [x] Lexer
  - [x] Regex to NFA
  - [ ] NFA to DFA (via subset construction)
- [x] Parser
  - [x] Recursive Descent
- [ ] Semantic Analysis
- [ ] Code generation

## Usage
Compile the compiler:
```sh
rustc src/main.rs -o goat
```

Execute:
```
./goat [-p] source.gt
```
If `-p` is specified, pretty printing of the source will be performed per stage 1 specification.\
Otherwise, `goat` will compile `<source>.gt` into `<source>.o`.