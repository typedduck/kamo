
//! # The parser module provides a parser combinator library.
//!
//! This module implements a parser combinator library. The library is focused
//! on parsing UTF-8 text in a safe and mostly zero-copy way. It is designed to
//! be used to implement parsers for programming languages. It is not designed
//! to be used to parse binary data.
//! 
//! Therefore the parsers are only generic over the output type. The input and
//! errors are defined by concrete types. The input is a wrapper around a string
//! slice and keeps track of the current character and position while advancing.
//! The error handling is based on [`Code`]s and [`Cause`]s. The error codes are
//! defined in the [`code`] module and the causes are defined in the [`Cause`]
//! module.
//!
//! This aproach is more like implementing a parser by hand in a traditional
//! programming language. The difference to the traditional way is that the
//! parser is implemented as a function that takes an [`Input`] and returns a
//! [`ParseResult`]. The result is either an `Ok` containing the output of the
//! parser and the remaining input, or an `Err` containing the error that
//! occurred. Combining these parsers lets you build a parser from simple to
//! complex.
//!
//! One design goal of this library is to make it easy to write parsers that
//! keep track of the position of the input and the cause of the error. The
//! position is tracked automatically by its offset in bytes from the begining
//! of the input and the line and column number in UTF-8 characters. The cause
//! of the error is tracked by a stack of causes in the error. The stack is used
//! to keep track of multiple causes of one error. The cause is added to the
//! stack when a parser fails and holds the position, error code and a message.
//! The error code is used to identify the cause of the error. Typically parsers
//! wich take other parsers as input will add a cause to the stack when the
//! input parser fails.
//!
//! ## Input handling for the parser.
//!
//! The input is a string slice that keeps track of the current character and
//! position. The input is advanced by calling `advance` or one of the
//! `advance_*` functions. The current character can be accessed with `current`
//! and the current position with `position`. It keeps track of newlines and the
//! current position is updated accordingly. Input reads are UTF-8 safe and the
//! current character is always a valid UTF-8 character. Lines and columns are
//! counted in UTF-8 characters and not in bytes. The offset is always in bytes
//! and counts the number of bytes read from the start of the input so far.
//!
//! The underlying string slice can be accessed with `as_str`. The length of the
//! remaining input in bytes can be accessed with `len` and if the input is
//! empty with `is_empty`. If the end of input is reached `current` returns
//! `None` and `is_eof` returns `true`.
//!
//! When advancing the slice is updated and the current character and position
//! are updated. The current character is always at offset 0 in the slice.
//! Therefore the offset returned by `position` is always the number of bytes
//! read from the begining up to the current character.
//!
//! ### Example
//!
//! ```rust
//! # use kamo::parser::{Input, Position};
//! let mut input = Input::new("abc");
//!
//! assert_eq!(input.current(), Some('a'));
//! assert_eq!(input.position(), Position::new(0, 1, 1));
//! assert_eq!(input.advance(), Some('b'));
//! assert_eq!(input.current(), Some('b'));
//! assert_eq!(input.advance(), Some('c'));
//! assert_eq!(input.current(), Some('c'));
//! assert_eq!(input.position(), Position::new(2, 1, 3));
//! assert_eq!(input.advance(), None);
//! assert_eq!(input.current(), None);
//! ```
//!
//! ## Parser combinators.
//!
//! The parser combinators are used to create parsers that can be used to parse
//! input defined by the [`Input`] type. The parser combinators are defined as
//! functions that take a parser and return a new parser or return a
//! parameterized parser.
//!
//! The parser combinators can be divided into several categories:
//!
//! - **basic combinators**. These are used to combine parsers that return
//!   different types of output. They include combinators that return a constant
//!   value, an optional value, mapped values, and verified values etc.
//! - **branching combinators**. These are used to combine parsers that return
//!   the same type of output and returns the output of the first parser that
//!   succeeds. Currently there is one branching combinator: [`any()`].
//! - **character combinators**. These are the most commonly used combinators.
//!   There are specialized combinators for [ASCII characters](ascii), 
//!   [Unicode code points](unicode), and combinators that match strings and
//!   characters. An additional combinator is available if the
//!   [`regex` feature](match_re) is enabled.
//! - **error combinators**. These are used to add context to errors. They are  
//!   accessible through one of the [`context()`], [`context_as()`], or
//!   [`context_and()`] functions.
//! - **literal combinators**. These are basic parsers that match literals and
//!   can be used to build more complex parsers when implementing a parser for a
//!   programming language. These are defined in the [`literal`] module.
//! - **sequence combinators**. These are used to combine parsers that are
//!   applied in sequence or can fold a parsed sequence. The most commonly used
//!   combinators are [`pair()`], [`tuple()`], [`list0()`], [`list1()`], [`many0()`], and
//!   [`many1()`]. The [`fold_many0()`], [`fold_many1()`], [`fold_list0()`], and
//!   [`fold_list1()`] combinators can be used to fold the output of the parsers.
//! 
//!
//! ## The [`Parser`] trait.
//!
//! The [`Parser`] trait is used to define parsers that can be used with the
//! parser combinators. The trait is implemented for functions that take an
//! [`Input`] and return a [`ParseResult`]. The trait is also implemented for
//! boxed parsers.
//! 
//! The two implementations of the [`Parser`] trait makes it possible to use
//! both functions and boxed parsers with the parser combinators. A simple
//! parser can be implemented as a function and a more complex parser can be
//! implemented as a struct that implements the [`Parser`] trait.
//! 
//! ### Example
//! 
//! ```rust
//! # use kamo::parser::{prelude::*, Input};
//! /// Parses a string of zero or more digits.
//! fn digits(input: Input) -> ParseResult<&str> {
//!     let mut input = input;
//!     let output = input.advance_while(|c| c.is_ascii_digit());
//! 
//!     Ok((output, input))
//! }
//! 
//! assert_eq!(digits.parse("123".into()), Ok(("123", "".into())));
//! assert_eq!(digits.parse("abc".into()), Ok(("", "abc".into())));
//! ```
//!
//! ## The [`ParseResult`] type.
//!
//! The result of a call to a parser. The result is either an `Ok` containing
//! the output of the parser and the remaining input, or an `Err` containing the
//! error that occurred.
//! 
//! The result is defined as a type alias for [`std::result::Result`] with the
//! error type set to [`ParseError`]. This and the constraint on the input type
//! simplifies the implementation of the parser combinators. But also
//! constraints the parsers to only be used with UTF-8 text. This is sufficient
//! for most use cases and especially for programming languages.
//!
//! ## The [`ParseError`] type.
//!
//! The error type returned by the parser. The error contains the position of
//! the error and a code of error that occurred. The error code is defined in
//! the [`code`] module.
//!
//! The error also contains a stack of [`Cause`]s. The stack is used to
//! keep track of the cause of the error. The cause is added to the stack when a
//! parser fails and keeps track of the position of the error and the error
//! code.
//!
//! ## How to write a parser.
//!
//! Writing a parser is a simple and straight forward process. The parser is
//! implemented as a function that takes an [`Input`] and returns a
//! [`ParseResult`]. Or it can be implemented as a struct that implements the
//! [`Parser`] trait. The parser is then combined with other parsers using the
//! parser combinators.
//!
//! A parser can also be a function that returns a parser. This is useful when
//! the parser needs to be parameterized with one or more values.
//!
//! Here is an example of a parser that parses a string of digits and returns
//! the number.
//!
//! ### Example
//!
//! ```rust
//! # use kamo::parser::{
//! #     prelude::*, CharacterError, code, Input, Position
//! # };
//! /// Parses a string of digits.
//! fn digits(input: Input) -> ParseResult<&str> {
//!     take_while1(|c| c.is_ascii_digit()).parse(input)
//! }
//!
//! /// Parses a string of digits and returns the number.
//! fn number(input: Input) -> ParseResult<u32> {
//!     digits
//!         .parse(input)
//!         .map(|(s, input)| (s.parse::<u32>().unwrap(), input))
//! }
//!
//! assert_eq!(number.parse("123".into()), Ok((123, "".into())));
//! assert_eq!(
//!     number.parse("abc".into()),
//!     Err(ParseError::new(
//!         Position::new(0, 1, 1),
//!         code::ERR_TAKE_WHILE_1,
//!         CharacterError::TakeWhile1
//!     ))
//! );
//! ```
//!
//! A common practice is to write parsers-factories that return a parser. This
//! is useful when the parser needs to be parameterized with a value. Here is an
//! example of a parser-factory that takes a tag and returns a parser that
//! matches the tag.
//!
//! ```rust
//! # use kamo::parser::{
//! #    prelude::*, CharacterError, code, Input, Position
//! # };
//! /// Parses a tag.
//! fn tag<'a>(tag: &'static str) -> impl Fn(Input<'a>) -> 
//!     ParseResult<'a, &'static str> {
//! 
//!     move |input| {
//!         let mut input = input;
//!         let mut tag = tag;
//!
//!         while let Some(ch) = tag.chars().next() {
//!             if input.current() != Some(ch) {
//!                 return Err(ParseError::new(
//!                     input.position(),
//!                     code::ERR_TAG,
//!                     CharacterError::Tag(tag),
//!                 ));
//!             }
//!
//!             input.advance();
//!             tag = &tag[ch.len_utf8()..];
//!         }
//!
//!         Ok((tag, input))
//!     }
//! }
//!
//! assert_eq!(tag("abc").parse("abc".into()), Ok(("", "".into())));
//! assert_eq!(
//!     tag("abc")("def".into()),
//!     Err(ParseError::new(
//!         Position::new(0, 1, 1),
//!         code::ERR_TAG,
//!         CharacterError::Tag("abc")
//!     ))
//! );
//! ```

pub mod literal;
pub mod predicate;

mod input;
pub use input::Input;

mod position;
pub use position::Position;

mod span;
pub use span::Span;

mod branch;
pub use branch::*;

mod character;
pub use character::*;

mod combinator;
pub use combinator::*;

mod error;
pub use error::*;

mod sequence;
pub use sequence::*;

use std::result::Result as StdResult;

/// The result of a call to a parser. The result is either an `Ok` containing
/// the output of the parser and the remaining input, or an `Err` containing the
/// error that occurred.
/// 
/// The result is only generic over the output type. This makes this library to
/// be used for different kind of outputs and not only for ASTs. The output type
/// can be a string slice, a string, a number, a boolean, a tuple, a struct or
/// an enum. There are no restrictions on the output type.
/// 
/// The result is defined as a type alias for [`std::result::Result`] with the
/// error type set to [`ParseError`]. This and the constraint on the input type
/// simplifies the implementation of the parser combinators. But also
/// constraints the parsers to only be used with UTF-8 text. This is sufficient
/// for most use cases and especially for programming languages.
pub type ParseResult<'a, O> = StdResult<(O, Input<'a>), ParseError>;

/// A prelude of commonly used parsers.
///
/// This module contains a prelude of commonly used parsers. It is intended to be
/// used with the `use` keyword to import the parsers into the current scope.
///
/// ```ignore
/// use kamo::parser::prelude::*;
/// ```
pub mod prelude {
    #[cfg(feature = "regex")]
    pub use super::character::match_re;

    pub use super::{
        branch::any,
        character::{
            any_char, ascii, char, is_not, none_of, one_of, satisfy, tag, take, take_while0,
            take_while1, take_while_m_n, unicode,
        },
        combinator::{eof, map, opt, recognize, value, verify},
        error::{context, context_as, context_and},
        literal,
        sequence::{
            delimited, fold_list0, fold_list1, fold_list_m_n, fold_many0, fold_many1,
            fold_many_m_n, list0, list1, list_m_n, many0, many0_count, many1, many1_count,
            many_m_n, many_m_n_count, pair, preceded, terminated, tuple,
        },
        ParseError, ParseResult, Parser,
    };
}

/// The target passed to the logger.
pub const TARGET: &str = "kamo::parser";

/// A trait defining a parser.
///
/// A parser is a function that takes an input and returns a result. The result
/// is either an `Ok` containing the output of the parser and the remaining
/// input, or an `Err` containing the error that occurred.
///
/// There are two blanket implementations of `Parser`:
///
/// - a function that takes an `Input` and returns a `ParseResult`
/// - a boxed parser
pub trait Parser<'a, 'b, O>
where
    O: 'b,
{
    /// Parses the input and returns the result.
    fn parse(&mut self, input: Input<'a>) -> ParseResult<'a, O>;
}

/// Blanket implementation of `Parser` for functions.
impl<'a, 'b, O, F> Parser<'a, 'b, O> for F
where
    O: 'b,
    F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    #[inline]
    fn parse(&mut self, input: Input<'a>) -> ParseResult<'a, O> {
        self(input)
    }
}

/// Blanket implementation of `Parser` for boxed parsers.
impl<'a, 'b, O> Parser<'a, 'b, O> for Box<dyn Parser<'a, 'b, O>>
where
    O: 'b,
{
    #[inline]
    fn parse(&mut self, input: Input<'a>) -> ParseResult<'a, O> {
        self.as_mut().parse(input)
    }
}
