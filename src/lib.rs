//! A library to assist in the creation of an interpreter or compiler and its
//! associated runtime.
//! 
//! This is the release of the first module of the Drake project. This crate
//! contains the parser library. It is a general purpose parser library that
//! can be used to parse any language. It is designed to be used in conjunction
//! with the future parts of this project, but it can also be used on its own.
//! 
//! The future parts of this project will extend this crate with a library for
//! automatic memory management, compile time evaluation and transformation into
//! an intermediate representation, an implemenation of an interpreter for the
//! intermediate representation, and a reference implementation of Scheme.The
//! implementation will be based on the R7RS standard and will implement a
//! subset of the standard.
//! 
//! The design goal of this project is to create a framework that can be used to
//! create a language interpreter or compiler. AST nodes will be created by the
//! parser and then passed to the evaluator. The AST is represented as symbolic
//! expressions (s-expressions) and is designed to be language agnostic. The
//! evaluator will then evaluate the AST into a combination of s-expressions and
//! intermediate representation. This is the compile time representation of the
//! program. The intermediate representation can then be either interpreted or
//! compiled into a target representation.
//! 
//! # Example
//! 
//! Example of a parser that parses a Scheme-like byte-vector and returns it as
//! `Vec<u8>`.
//! 
//! The grammar is defined as follows:
//! 
//! ```text
//! ByteVector = "#u8(" Byte* ')'
//! Byte       = <any exact integer between 0 and 255>
//! ```
//! 
//! The parser is defined as follows:
//! 
//! ```rust
//! use drake::parser::{code, prelude::*, Input, ParseError, ParseResult, Span};
//! 
//! fn main() {
//!     assert_eq!(
//!         parse_bytevec(Input::new("#u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)")),
//!         Ok((
//!             vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
//!             Input::new("")
//!         ))
//!     );
//! }
//! 
//! fn parse_bytevec(input: Input<'_>) -> ParseResult<'_, Vec<u8>> {
//!     context_as(
//!         delimited(
//!             pair(tag("#u8("), ascii::whitespace0),
//!             list0(parse_bytevec_value, ascii::whitespace1),
//!             context_as(
//!                 preceded(ascii::whitespace0, char(')')),
//!                 code::ERR_CUSTOM + 2,
//!                 "expecting a closing parenthesis: )",
//!             ),
//!         ),
//!         code::ERR_CUSTOM + 1,
//!         "expecting a byte vector: #u8(<byte>*)",
//!     )(input)
//! }
//! 
//! fn parse_bytevec_value(input: Input<'_>) -> ParseResult<'_, u8> {
//!     let result = preceded(
//!         ascii::whitespace0,
//!         any((
//!             parse_binary_natural,
//!             parse_octal_natural,
//!             parse_decimal_natural,
//!             parse_hexadecimal_natural,
//!             parse_natural,
//!         )),
//!     )(input);
//! 
//!     match result {
//!         Ok((value, cursor)) => {
//!             if value > u8::MAX as u64 {
//!                 Err(ParseError::new(
//!                     Span::new(input.position(), cursor.position()),
//!                     code::ERR_CUSTOM + 3,
//!                     "expecting a byte value: 0..255",
//!                 ))
//!             } else {
//!                 Ok((value as u8, cursor))
//!             }
//!         }
//!         Err(err) => Err(err),
//!     }
//! }
//! 
//! #[inline]
//! fn parse_binary_natural(input: Input<'_>) -> ParseResult<u64> {
//!     preceded(tag("#b"), literal::natural(literal::Radix::Binary))(input)
//! }
//! 
//! #[inline]
//! fn parse_octal_natural(input: Input<'_>) -> ParseResult<u64> {
//!     preceded(tag("#o"), literal::natural(literal::Radix::Octal))(input)
//! }
//! 
//! #[inline]
//! fn parse_decimal_natural(input: Input<'_>) -> ParseResult<u64> {
//!     preceded(tag("#d"), parse_natural)(input)
//! }
//! 
//! #[inline]
//! fn parse_hexadecimal_natural(input: Input<'_>) -> ParseResult<u64> {
//!     preceded(tag("#x"), literal::natural(literal::Radix::Hexadecimal))(input)
//! }
//! 
//! #[inline]
//! fn parse_natural(input: Input<'_>) -> ParseResult<u64> {
//!     literal::natural(literal::Radix::Decimal)(input)
//! }
//! ```
//! 
//! # Feature List
//! 
//! - [x] Module `drake::parser` for parsing UTF-8 text. A parser combinator
//! library for parsing UTF-8 text in a safe and mostly zero-copy way.
//! - [ ] Module `drake::mem` for automatic memory management. Values are allocated
//! in a mutator which holds an arena allocator for each type. Memory collection
//! is done by a mark and sweep garbage collector.
//! - [ ] Module `drake::value` for values. Values can either hold immediate values
//! or pointers to values allocated in the mutator.
//! - [ ] Module `drake::eval` for evaluation. The evaluator processes an AST, which
//! is an symbolic expression tree, and evaluates it to an intermediate
//! representation. The intermediate representation can then be interpreted or
//! compiled to a target representation. The interpreter is generic and can be
//! used to interpret any intermediate representation.
//! - [ ] Module `drake::types` for types. The type system is used to infer the
//! types of the intermediate representation and the AST.
//! - [ ] Module `drake::repl` for a read-eval-print-loop. The REPL is used to
//! interactively evaluate expressions and is generic and can be used to
//! evaluate any intermediate representation.
//! - [ ] Module `drake::lang::scheme` for the Scheme language. The Scheme language
//! is implemented as a library on top of the `drake` modules. It implements the
//! a subset of the R7RS standard.


pub mod parser;
