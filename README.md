# Noam: Automata Formal Verification Language

Noam is a domain-specific language for the formal verification of automata; it is also a tribute to Noam Chomsky's foundational work in formal language theory and computational linguistics. Currently, the project works on finite automata verification, but it will extend to other kinds of automata over time.

## Project Overview

Noam is an educational project studying the junction between the design of programming languages, formal verification, and compiler construction. Being implemented in Common Lisp, it uses the symbolic computation and meta-programming facilities of the language to reach a sound basis for the automata verification.

## Current Status of the Implementation

Still in the early stages, the only major work has been the basic compilation chain as below:

```
Noam Source Code --> LISP AST --> LLVM IR --> Assembly --> Object Code
```

It is being written in Common Lisp using the cl-lex lexical analyzer and cl-yacc parser. These tools have been chosen because of their immediate compatibility with Lisp's high-powered macro system and the symbolic computation; it is therefore very natural to compile Noam source code directly into a Lisp-based abstract syntax tree.

## Technical Implementation

The compiler implementation will follow a traditional structure in many ways but leverages unique strengths of Lisp:

- **Lexical Analysis**: cl-lex used, for tokenizing Noam source code
- **Parsing**: cl-yacc used, this tool provides LALR(1) parsing capability and easily integrates into the Common Lisp environment
- **AST Generation**: Parser outputs a lisp-based abstract syntax tree, leveraging the natural tree-like structure of lisp in describing the semantics of a program

## Development Status

This is an actively developed project, focusing on the core compilation pipeline. The implementation strategy and technical approaches taken may change while this project evolves.

### Current
- cl-lex, cl-yacc front end
- AST creation and manipulation in Lisp
- Researching how to emit LLVM IR

### Future
- Smoothen out the compilation pipeline
- If current approach proves impractical, research alternative implementation strategies
- Expansion to support additional types of automata
