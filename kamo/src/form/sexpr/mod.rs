//! S-expression parser.
//!
//! The syntax the parser processes is as defined by the Scheme standard R7RS
//! for the `read` procedure. The syntactic definition is the `<datum>` in
//! section [`7.1.2 External representations`](https://standards.scheme.org/official/r7rs.pdf)
//! of the standard.
//!  
//! The syntax deviations from the standard are:
//!  
//! - The extactness (`#e` and `#i`) of numbers is not supported. Floating-point
//!   numbers are always inexact and integers are always exact.
//!  
//! - Numbers may only be signed 64-bit integers or IEEE 754 double precision
//!   floating-point numbers. The standard allows for arbitrary precision
//!   integers, rationals and complex numbers.
//!  
//! - Labels are not supported.
//!  
//! - The `#;` comment syntax is only supported in the methods which parse
//!   multiple s-expressions. The `#;` comment syntax may not be nested.
//!  
//! - Character literals defined by a hex escape sequence may have 1 to 6
//!   digits. The standard excepts 1 or more digits. The code must be a valid
//!   Unicode code point.
//!
//! The parser is implemented with the combination parser library
//! [`kamo::parser`](crate::parser). The parser is implemented by the struct
//! [`Sexpr`].

use std::{cell::RefCell, fs::read_to_string, path::Path};

use crate::{
    mem::MutatorRef,
    parser::{Code, Input, ParseError, ParseResult, Parser},
    value::{PositionMap, Value},
    Position,
};

/// A parser for S-expressions.
///
/// The parser is implemented with the combination parser library
/// [`kamo::parser`](crate::parser). It is the runtime equivalent of the
/// proc-macro implemented by the
/// [kamo-macros](https://crates.io/crates/kamo-macros) crate.
///
/// The parser implements the trait [`Parser`] for the type [`Value`]. It parses
/// a single datum optionally delimited by intertoken whitespace and followed by
/// an end of file.
///
/// The parser requires a mutatator to allocate new values. All parse-methods
/// which return a parser-closure can potentially allocate new values. The
/// static methods only return intermediate values.
///
/// The generic parameter `const ECO: Code` is the error code offset for the
/// parser. The error codes are defined in the module
/// [`sexpr::error::code`](crate::form::sexpr::error::code). The `ECO` parameter
/// is added to the error codes to create a unique error code domain for the
/// parser when used in combination with other parsers. Error codes of this
/// parser all start with [`ERR_CUSTOM`](crate::parser::code::ERR_CUSTOM). The
/// last error code not included in the domain is
/// [`ERR_SEXPR_END`](crate::form::sexpr::error::code::ERR_SEXPR_END).
///
/// Optionally there can be a [`PositionMap`] chained to the parser. The
/// positions are used to annotate the values of list nodes with their source
/// code positions. For every pair the position of the car-value is added to the
/// position map. It is possible to chain a pre-existing position map to the
/// parser or to create a new position map. The position map can be taken from
/// the parser after parsing. The position map can be used by stages of the
/// compiler to annotate errors with source code positions.
///
/// # Example
///
/// ```rust
/// use kamo::form::sexpr::Sexpr;
/// use kamo::mem::Mutator;
/// use kamo::parser::Parser;
/// use kamo::value::Value;
/// use kamo_macros::sexpr;
///
/// let mut m = Mutator::new_ref();
/// let mut p = Sexpr::<0>::new(m.to_owned());
/// let result = p.parse("(+ 1 2)".into());
///
/// assert!(result.is_ok());
///
/// let (value, _) = result.unwrap();
/// assert_eq!(value, sexpr!(m, "(+ 1 2)"));
/// ```
///
/// # Grammar
///
/// The parser implements the following grammar for S-expressions:
///
/// ```text
/// datum                  = boolean | character | number | string | bytevec
///                        | vector | symbol | list | quote | quasiquote
///                        | unquote | unquote_splicing
/// boolean                = "#t" | "#f" | "#true" | "#false"
/// character              = "#\\" <any character>
///                        | "#\\" character_name
///                        | "#\\x" character_code
/// character_name         = "alarm" | "backspace" | "delete | "escape"
///                        | "newline" | "null" | "return" | "space"
///                        | "tab"
/// character_code         = [0-9A-Fa-f]{1, 6}
/// number                 = number_binary | number_octal | number_hexdec
///                        | number_decimal | decimal | infnan
/// number_binary          = "#b" ((sign [0-1]+) | infnan)
/// number_octal           = "#o" ((sign [0-7]+) | infnan)
/// number_hexdec          = "#x" ((sign [0-9a-fA-F]+) | infnan)
/// number_decimal         = "#d" (decimal | infnan)
/// decimal                = sign (([0-9]+ (decimal_fraction | decimal_suffix)?)
///                                | decimal_fraction)
/// decimal_fraction       = "." [0-9]+ decimal_suffix?
/// decimal_suffix         = [eE] sign [0-9]+
/// sign                   = ("+" | "-")?
/// infnan                 = "+inf.0" | "-inf.0" | "+nan.0" | "-nan.0"
/// string                 = "\"" string-text "\""
/// string-text            = (string-escape | string-char)*
/// string-escape          = "\\" ( "a" | "b" | "t" | "n" | "r" | "\\" | "\""
///                               | "x" hex-code ";" | ilws )
/// string-char            = <any> - ["\\" | "\""]
/// symbol                 = (symbol_initial symbol_subsequent*)
///                        | ([+\-]
///                              ( (symbol_sign_subsequent symbol_subsequent*)
///                              | ([.] symbol_dot_subsequent symbol_subsequent*))
///                        | ([.] symbol_dot_subsequent symbol_subsequent*))
///                        | ("|" symbol_text "|")
/// symbol_text            = (symbol_escape | symbol_char)*
/// symbol_escape          = "\" ( "a" | "b" | "t" | "n" | "r" | "\\" | "|"
///                              | "x" hex-code ";")
/// symbol_char            = <any> - ["\\" | "|"]
/// symbol_initial         = [a-zA-Z] | [!$%&*/:<=>?^_~]
/// symbol_subsequent      = symbol_initial | [0-9] | [+\-.@]
/// symbol_sign_subsequent = symbol_initial | [+\-@]
/// symbol_dot_subsequent  = symbol_sign_subsequent | "."
/// list                   = "(" intertoken
///                              ((datum intertoken)+ ("." intertoken datum)?)?
///                          intertoken ")"
/// hex-code               = [0-9a-fA-F]{1,6}
/// ilws                   = [ \t]* ("\n" | "\r\n") [ \t]*
/// bytevec                = "#u8(" intertoken (byte intertoken)* ")"
/// byte                   = <any extact number between 0 and 255>
/// vector                 = "#(" intertoken (datum intertoken)* ")"
/// quote                  = "'" intertoken datum
/// quasiquote             = "`" intertoken datum
/// unquote                = "," intertoken datum
/// unquote_splicing       = ",@" intertoken datum
/// intertoken             = (whitespace | comment)*
/// whitespace             = <Unicode whitespace>+
/// comment                = comment-line | comment-nested
/// comment-line           = ";" <any>* ("\n" | eof)
/// comment-nested         = "#|" comment-text comment-cont* "|#"
/// comment-text           = <any except "#|" or "|#">*
/// comment-cont           = comment-nested comment-text
/// ```
#[derive(Clone, Debug)]
pub struct Sexpr<'a, const ECO: Code> {
    m: MutatorRef<'a>,
    pmap: Option<RefCell<PositionMap>>,
}

impl<'a, const ECO: Code> Sexpr<'a, ECO> {
    /// Creates a new parser for S-expressions.
    ///
    /// Optionally, a position map can be chained to the parser. The positions
    /// are used to annotate the values of list nodes with their source code
    /// positions. For every pair the position of the car-value is added to the
    /// position map.
    #[must_use]
    pub const fn new(m: MutatorRef<'a>) -> Self {
        Self { m, pmap: None }
    }

    /// Chains an existing [`PositionMap`] to the parser. The parser takes the
    /// ownership of the position map.
    #[must_use]
    pub fn with_pmap(mut self, pos: PositionMap) -> Self {
        self.pmap = Some(RefCell::new(pos));
        self
    }

    /// Chains an empty [`PositionMap`] to the parser.
    #[must_use]
    pub fn tracked(mut self) -> Self {
        self.pmap = Some(RefCell::new(PositionMap::new()));
        self
    }

    /// Takes the [`PositionMap`] if it is present.
    pub fn take_positions(&mut self) -> Option<PositionMap> {
        self.pmap.take().map(RefCell::into_inner)
    }

    /// Parses zero or more expressions delimited by intertoken whitespace and
    /// followed by an end of file. An expression may be shadowed.
    ///
    /// # Errors
    ///
    /// If the input is not a valid script or if the script is not terminated
    /// with an end of file, an error is returned.
    ///
    /// # Grammar
    ///
    /// ```text
    /// script = intertoken ((datum | shadowed) intertoken)* eof
    /// ```
    pub fn parse_script<'b>(&mut self, input: Input<'b>) -> ParseResult<'b, Box<[Value<'a>]>> {
        use crate::parser::prelude::*;

        let (script, cursor) = delimited(
            Self::intertoken,
            fold_many0(
                terminated(self.datum_or_shadowed(), Self::intertoken),
                Vec::new,
                |mut acc, _, value| {
                    if let Datum::Exposed(value) = value {
                        acc.push(value);
                    }
                    acc
                },
            ),
            eof,
        )(input)?;

        Ok((script.into_boxed_slice(), cursor))
    }

    /// Reads the content of a file and parses it as a script by calling
    /// [`parse_script()`](Self::parse_script).
    ///
    /// # Errors
    ///
    /// If the file cannot be read, the input is not a valid script or the
    /// script is not terminated with an end of file, an error is returned.
    pub fn parse_file<P: AsRef<Path>>(&mut self, path: P) -> ParseResult<Box<[Value<'a>]>> {
        use error::code::ERR_FILE;

        let input = &read_to_string(path.as_ref()).map_err(|err| {
            ParseError::new(
                Position::new(0, 1, 1),
                ERR_FILE + ECO,
                SexprError::FileError(path.as_ref()),
            )
            .and_source(err)
        })?;
        let (exprs, cursor) = self.parse_script(input.into())?;

        Ok((exprs, Input::eof_from(cursor)))
    }
}

impl<'a, 'b, const ECO: Code> Parser<'b, 'a, Value<'a>> for Sexpr<'a, ECO> {
    /// Parses a single datum as defined in [`datum()`](Self::datum) delimited
    /// by [`intertoken()`](Self::intertoken) and followed by an end of file.
    ///
    /// # Grammar
    ///
    /// ```text
    /// sexpr = intertoken datum intertoken eof
    /// ```
    fn parse(&mut self, input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        use crate::parser::prelude::*;

        terminated(
            delimited(Self::intertoken, self.datum(), Self::intertoken),
            eof,
        )(input)
    }
}

mod abbreviation;
mod boolean;
mod bytevec;
mod character;
mod datum;
mod error;
mod list;
mod number;
mod string;
mod symbol;
mod vector;
mod whitespace;

pub use datum::Datum;
#[allow(clippy::module_name_repetitions)]
pub use error::{code, SexprError};

#[cfg(test)]
mod tests {
    use kamo_macros::{sexpr, sexpr_file};

    use crate::{mem::Mutator, Position};

    use super::*;

    #[test]
    fn parse_success() {
        let m = Mutator::new_ref();
        let mut p = Sexpr::<0>::new(m.clone());

        let result = p.parse("; Adding two numbers\n(+ 1 2) ".into());
        assert_eq!(result, Ok((sexpr!(m, "(+ 1 2)"), Input::new(""))));
    }

    #[test]
    fn parse_files_success() {
        let m = Mutator::new_ref();
        let mut p = Sexpr::<0>::new(m.clone());

        parse_file_chapter_01(m.clone(), &mut p);
        parse_file_chapter_02(m.clone(), &mut p);
        parse_file_chapter_03(m.clone(), &mut p);
        parse_file_chapter_04(m.clone(), &mut p);
        parse_file_chapter_05(m.clone(), &mut p);
        parse_file_chapter_06(m.clone(), &mut p);
        parse_file_chapter_07(m.clone(), &mut p);
        parse_file_chapter_08(m.clone(), &mut p);
        parse_file_chapter_09(m.clone(), &mut p);
        parse_file_chapter_10(m.clone(), &mut p);
        parse_file_the_little_schemer(&mut p);
        // dbg!(m.borrow().stats());
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_01(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-01.scm");
        let result = p.parse_file("tests/sexpr/chapter-01.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(4864, 184, 66));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_02(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-02.scm");
        let result = p.parse_file("tests/sexpr/chapter-02.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(1373, 48, 49));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_03(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-03.scm");
        let result = p.parse_file("tests/sexpr/chapter-03.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(3638, 102, 56));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_04(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-04.scm");
        let result = p.parse_file("tests/sexpr/chapter-04.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(3861, 161, 23));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_05(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-05.scm");
        let result = p.parse_file("tests/sexpr/chapter-05.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(5786, 152, 61));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_06(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-06.scm");
        let result = p.parse_file("tests/sexpr/chapter-06.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(1640, 65, 92));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_07(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-07.scm");
        let result = p.parse_file("tests/sexpr/chapter-07.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(2589, 86, 63));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_08(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-08.scm");
        let result = p.parse_file("tests/sexpr/chapter-08.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(2342, 76, 44));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_09(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-09.scm");
        let result = p.parse_file("tests/sexpr/chapter-09.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(1011, 44, 25));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn parse_file_chapter_10(m: MutatorRef<'_>, p: &mut Sexpr<0>) {
        let expected = &sexpr_file!(m, "tests/sexpr/chapter-10.scm");
        let result = p.parse_file("tests/sexpr/chapter-10.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(2796, 88, 67));
        assert_eq!(exprs.len(), expected.len());
        for (i, (actual, expected)) in exprs.iter().zip(expected.iter()).enumerate() {
            assert_eq!(actual, expected, "Expression {} does not match", i + 1);
        }
    }

    fn parse_file_the_little_schemer(p: &mut Sexpr<0>) {
        // This file seems too big to compare. The rust-analyzer crashes when
        // trying to analyze the resulting expression. The proc-macro seems to
        // generate a too large expression.
        let result = p.parse_file("tests/sexpr/the-little-schemer.scm");
        assert!(result.is_ok());
        let (exprs, rest) = result.unwrap();
        assert_eq!(rest.position(), Position::new(36810, 1245, 4));
        assert_eq!(exprs.len(), 147);
    }
}
