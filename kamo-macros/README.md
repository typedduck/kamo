# Kamo-macros

![Build Status](https://img.shields.io/github/actions/workflow/status/typedduck/kamo-macros/rust.yml)
[![Crates.io](https://img.shields.io/crates/v/kamo-macros)](https://crates.io/crates/kamo-macros)
[![Crates.io](https://img.shields.io/crates/d/kamo-macros)](https://crates.io/crates/kamo-macros)

Kamo (カモ) is japanese for duck.

This crate provides procedural macros for the
[Kamo](https://crates.io/crates/kamo) crate. The macros provided are:

- `sexpr!(<mutator>, <expression>)` - A macro for parsing a single s-expression
  from a string. It returns a `kamo::value::Value`.

- `sexpr_file!(<mutator>, <filename>)` - A macro for parsing multiple
  s-expressions from a file. It returns an array of `kamo::value::Value`s. The
  array may be empty.

- `sexpr_script!(<mutator>, <filename>)` - A macro for parsing multiple
  s-expressions from a string. It returns an array of `kamo::value::Value`s. The
  array may be empty.

The macros all take an optional `MutatorRef` identifier. This is used to allocate
values on the heap. If the expression does not contain any values that need to
be allocated on the heap, then the `Mutator` identifier can be omitted.

The syntax for the macros is as defined by the Scheme standard R7RS for the
`read` procedure. The syntactic definition is the `<datum>` in section
["7.1.2 External representations"](https://standards.scheme.org/official/r7rs.pdf)
of the standard.

The syntax deviations from the standard are:

- The extactness (`#e` and `#i`) of numbers is not supported. Floating-point
  numbers are always inexact and integers are always exact.

- Numbers may only be signed 64-bit integers or IEEE 754 double precision
  floating-point numbers. The standard allows for arbitrary precision integers,
  rationals and complex numbers.

- Labels are not supported.

- The `#;` comment syntax is only supported in the macros which parse multiple
  s-expressions. The `#;` comment syntax may not be nested.

- Character literals defined by a hex escape sequence may have 1 to 6 digits.
  The standard excepts 1 or more digits. The code must be a valid Unicode code
  point.

The parser is implemented with the `pest` crate. The grammar is defined in
`src/sexpr/sexpr.pest`. This is necessary because the combination parser library
defined in `kamo::parser` cannot be used here. It would be cyclic dependency.
There will be an implementation of a parser for s-expressions in the
`kamo::form` module in the future. It will be based on the `kamo::parser` crate
and will be used by the scheme interpreter.

## Examples

```rust
use kamo::{mem::Mutator, sexpr, value::{print, Value}};
 
let m = Mutator::new_ref();
let value = sexpr!(m, "(1 2 3)");
 
assert_eq!(print(value).to_string(), "(1 2 3)");
```
 
```rust
use kamo::{sexpr_file, value::{print, Value}};
 
let m = Mutator::new_ref();
let values = sexpr_file!("tests/sexpr/values.scm");
 
assert_eq!(values.len(), 3);
assert_eq!(print(values[0].clone()).to_string(), "()");
assert_eq!(print(values[1].clone()).to_string(), "100");
assert_eq!(print(values[2].clone()).to_string(), "#t");

let values: &[Value] = &sexpr_file!("tests/sexpr/empty.scm");
assert_eq!(values.len(), 0);
```
 
```rust
use kamo::{mem::Mutator, sexpr_script, value::{print, Value}};
 
let m = Mutator::new_ref();
let values = sexpr_script!(m, "(define a 1)\n(define b 2)\n(+ a b)");
 
assert_eq!(values.len(), 3);
assert_eq!(print(values[0].clone()).to_string(), "(define a 1)");
assert_eq!(print(values[1].clone()).to_string(), "(define b 2)");
assert_eq!(print(values[2].clone()).to_string(), "(+ a b)");

let values: &[Value] = &sexpr_script!("");
 
assert_eq!(values.len(), 0);
```