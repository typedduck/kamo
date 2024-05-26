# Change Log

## [0.9.4](https://github.com/typedduck/kamo/tree/kamo-v0.9.4) - 2024-05-26

* FIX: Type parser for textual representation now parses union types. This was
  missing in the previous implementation.
* Refactor the type parser for textual representation.
* Renamed `types::parser::binary::parse_filled_or_option` to
  `parse_filled_option` for consistency with other parsers.

## [0.9.3](https://github.com/typedduck/kamo/tree/kamo-v0.9.3) - 2024-04-28

* Enable feature doc_cfg for docs.rs in `Cargo.toml` in crate `kamo`.

## [0.9.2](https://github.com/typedduck/kamo/tree/kamo-v0.9.2) - 2024-04-28

* Add package metadata for `docs.rs` in `Cargo.toml` in crate `kamo`.
* Add `cfg_attr` to `lib.rs` in crate `kamo`.

## [0.9.1](https://github.com/typedduck/kamo/tree/kamo-v0.9.1) - 2024-04-27

* Change categories and keywords in `Cargo.toml` in crate `kamo`.

## [0.9.0](https://github.com/typedduck/kamo/tree/kamo-v0.9.0) - 2024-04-27

* Add module `types` and `env`. The module `types` contains the type system and
  type checker for a language. The module `env` contains the environment for an
  interpreter.

## [0.8.1](https://github.com/typedduck/kamo/tree/kamo-v0.8.1) - 2024-04-13

* Merge kamo-macros into the main crate. There will be no separate repository for
  the macros. The repository `kamo-macros` will be archived.

## [0.8.0](https://github.com/typedduck/kamo/tree/v0.8.0) - 2024-04-11

* Refactor after activating pedantic linting. Must changes are in the
  documentation and adding `#[must_use]`.

## [0.7.0](https://github.com/typedduck/kamo/tree/v0.7.0) - 2024-04-02

* FIX: Proper tracking of positions when parsing lists with position map.
* Added failure-error to the combination parser library. This allows to
  propagate errors in the parser combinators. Parsers using the `failure`
  flag are `any` and `opt`.
* New parsers `cut` and `map_err`.
* Span now supports generic parameters to `Span::new` and `Span::new_at`.

## [0.6.0](https://github.com/typedduck/kamo/tree/v0.6.0) - 2024-03-26

* FIX: Checking equality of floating-point values when they are NaN.
* FIX: Tracing of allocated objects in the garbage collector.
* CHG: The methods `Value::new_list*` and `Value::new_dotlist*` return the empty
  list (`nil`-value) if the input iterator is empty. The methods
  `Value::new_dotlist*` panic if the input iterator is empty and the alternate
  list end is not `nil`.
* Added type `value::PositionMap` to store source code positions of nodes
  emitted by a parser.
* Added method `tag()` to `value::ValueId` to get the tag of a value.
* Added semantic error and chaining methods to the combination parser library.
* Moved the type `Position` to the crate root.
* Runtime parser for s-expressions in module `form::sexpr`.

## [0.5.0](https://github.com/typedduck/kamo/tree/v0.5.0) - 2024-01-18

* Added feature `macros` to enable the `sexpr!`, `sexpr_file!` and
  `sexpr_script!` macros. The macros are not enabled by default.

* Handle special cases for floating-point values in `SimplePrinterVisitor`.

## [0.4.0](https://github.com/typedduck/kamo/tree/v0.4.0) - 2023-12-27

* Changed the implementation of byte vector from `Vec<u8>` to
  `SmallVec<[u8; 16]>`. The type alias `ByteVector` was added.

## [0.3.0](https://github.com/typedduck/kamo/tree/v0.3.0) - 2023-12-24

* Add memory management and runtime values modules

## [0.2.0](https://github.com/typedduck/kamo/tree/v0.2.0) - 2023-12-09

* Add map2 combinator to parser module

## [0.1.1](https://github.com/typedduck/kamo/tree/v0.1.1) - 2023-11-27

* Update repository URL and crate name in Cargo.toml

## [0.1.0](https://github.com/typedduck/kamo/tree/v0.1.0) - 2023-11-27

* Initial upload
