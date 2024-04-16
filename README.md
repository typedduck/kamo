# Kamo

Kamo (カモ) is japanese for duck.

Kamo is a library to assist in the creation of an interpreter or compiler and
its associated runtime.

The library is designed to be modular and generic. It is not tied to any
specific language or runtime. It can be used to implement any simple language or
runtime. It provides a parser combinator library, automatic memory management
with a garbage collector, value representation, evaluation, type system, and a
read-eval-print-loop.

The underlying technology is based on the Lisp family of languages. The runtime
values and abstract syntax tree (AST) are conceptually based on S-expressions.
The evaluation is based on the Lisp evaluation model with primitive operations and
special forms.

The library is written in Rust and is designed to be safe and fast.

The documentation is available at [docs.rs](https://docs.rs/kamo).

## Status

The project is a work in progress. The API is not stable and may change at any
time.

![Build Status](https://img.shields.io/github/actions/workflow/status/typedduck/kamo/rust.yml)

**Kamo**

[![Crates.io](https://img.shields.io/crates/v/kamo)](https://crates.io/crates/kamo)
[![Crates.io](https://img.shields.io/crates/d/kamo)](https://crates.io/crates/kamo)

**Kamo Macros**

[![Crates.io](https://img.shields.io/crates/v/kamo-macros)](https://crates.io/crates/kamo-macros)
[![Crates.io](https://img.shields.io/crates/d/kamo-macros)](https://crates.io/crates/kamo-macros)

## Feature List

- [x] Module `kamo::parser` for parsing UTF-8 text. A parser combinator
    library for parsing UTF-8 text in a safe and mostly zero-copy way.
- [x] Module `kamo::mem` for automatic memory management. Values are allocated
    in a mutator which holds an arena allocator for each type. Memory collection
    is done by a mark and sweep garbage collector.
- [x] Module `kamo::value` for values. Values can either hold immediate values
    or pointers to values allocated in the mutator.
- [ ] Module `kamo::eval` for evaluation. The evaluator processes an AST, which
    is an symbolic expression tree, and evaluates it to an intermediate
    representation. The intermediate representation can then be interpreted or
    compiled to a target representation. The interpreter is generic and can be
    used to interpret any intermediate representation.
- [ ] Module `kamo::types` for types. The type system is used to infer the
    types of the intermediate representation and the AST.
- [ ] Module `kamo::repl` for a read-eval-print-loop. The REPL is used to
    interactively evaluate expressions and is generic and can be used to
    evaluate any intermediate representation.
- [ ] Module `kamo::lang::scheme` for the Scheme language. The Scheme language
    is implemented as a library on top of the `kamo` modules. It implements a
    subset of the R7RS standard.

## License

This project is licensed under the MIT License or the Apache License 2.0, at
your option. For details, see the `LICENSE-MIT` and `LICENSE-APACHE` files for
more information.

## Support

If you like this project and want to support it, you can do so by:

- Giving it a star on GitHub.
- Sharing it with your friends.
- Contributing to the project by opening an issue or a pull request.
- Donating to the project by using the following links:
  - Bitcoin (Taproot): `bc1pqdck3v3r7sa4mgl0dztfzufa4xg66g8cpcgwvjax9rtx6mlxafdqcgw3g2`
  - Bitcoin (Segwit): `bc1qet2ypmsxtx6mc03329ft5a736fy906flm4c42a9d3e7mvu872tcs8myzs6`
  - [Patreon](https://www.patreon.com/typedduck)

Patreon supporters will be listed in the `SUPPORTERS.md` file.
