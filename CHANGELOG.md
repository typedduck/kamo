# Change Log

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
