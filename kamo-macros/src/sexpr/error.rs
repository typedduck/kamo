#![allow(clippy::std_instead_of_alloc)]
use std::{error::Error as StdError, fmt};

use pest::{error::Error as PestError, Span};

use super::{emitter::Number, parser::Rule};

#[allow(clippy::error_impl_error)]
#[derive(Clone, PartialEq)]
pub enum Error<'a> {
    Parser(String),
    EmptySExpr,
    MutatorRequired(Span<'a>),
    InvalidCodePoint(Span<'a>, u32),
    InvalidDecimal(Span<'a>, String),
    InvalidBinary(Span<'a>, String),
    InvalidOctal(Span<'a>, String),
    InvalidHexadecimal(Span<'a>, String),
    ExpectedAbbrev(Span<'a>),
    ExpectedBoolean(Span<'a>),
    ExpectedByte(Span<'a>, Number),
    ExpectedByteVector(Span<'a>),
    ExpectedCharacter(Span<'a>),
    ExpectedDecimal(Span<'a>),
    ExpectedEndOfExpression(Span<'a>),
    ExpectedEndOfInput,
    ExpectedEndOfList(Span<'a>),
    ExpectedEscape(Span<'a>),
    ExpectedInfnan(Span<'a>),
    ExpectedList(Span<'a>),
    ExpectedNumber(Span<'a>),
    ExpectedString(Span<'a>),
    ExpectedSymbol(Span<'a>),
    ExpectedVector(Span<'a>),
}

impl<'a> StdError for Error<'a> {}

impl<'a> fmt::Display for Error<'a> {
    #[allow(clippy::too_many_lines, clippy::use_debug, clippy::ref_patterns)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Parser(ref e) => write!(f, "{e}"),
            Self::EmptySExpr => write!(f, "empty s-expression"),
            Self::MutatorRequired(span) => {
                write!(f, "{:?} mutator required", span.start_pos().line_col())
            }
            Self::InvalidCodePoint(span, code) => {
                write!(
                    f,
                    "{:?} invalid code point: 0x{:x}",
                    span.start_pos().line_col(),
                    code
                )
            }
            Self::InvalidDecimal(span, ref float) => {
                write!(
                    f,
                    "{:?} invalid decimal: {}",
                    span.start_pos().line_col(),
                    float
                )
            }
            Self::InvalidBinary(span, ref float) => {
                write!(
                    f,
                    "{:?} invalid binary: {}",
                    span.start_pos().line_col(),
                    float
                )
            }
            Self::InvalidOctal(span, ref float) => {
                write!(
                    f,
                    "{:?} invalid octal: {}",
                    span.start_pos().line_col(),
                    float
                )
            }
            Self::InvalidHexadecimal(span, ref float) => {
                write!(
                    f,
                    "{:?} invalid hexadecimal: {}",
                    span.start_pos().line_col(),
                    float
                )
            }
            Self::ExpectedAbbrev(span) => {
                write!(
                    f,
                    "{:?} expected abbreviated list",
                    span.start_pos().line_col()
                )
            }
            Self::ExpectedBoolean(span) => {
                write!(f, "{:?} expected boolean", span.start_pos().line_col())
            }
            Self::ExpectedByte(span, ref num) => {
                write!(
                    f,
                    "{:?} expected integer in the range 0..=255, found {}",
                    span.start_pos().line_col(),
                    num
                )
            }
            Self::ExpectedByteVector(span) => {
                write!(f, "{:?} expected bytevector", span.start_pos().line_col())
            }
            Self::ExpectedCharacter(span) => {
                write!(f, "{:?} expected character", span.start_pos().line_col())
            }
            Self::ExpectedDecimal(span) => {
                write!(f, "{:?} expected decimal", span.start_pos().line_col())
            }
            Self::ExpectedEndOfExpression(span) => {
                write!(
                    f,
                    "{:?} expected end of expression",
                    span.start_pos().line_col()
                )
            }
            Self::ExpectedEndOfInput => write!(f, "expected end of input"),
            Self::ExpectedEndOfList(span) => {
                write!(
                    f,
                    "{:?} expected end of list: dotted pair ends the list",
                    span.start_pos().line_col()
                )
            }
            Self::ExpectedEscape(span) => {
                write!(f, "{:?} expected escape", span.start_pos().line_col())
            }
            Self::ExpectedInfnan(span) => {
                write!(
                    f,
                    "{:?} expected infinity or NaN",
                    span.start_pos().line_col()
                )
            }
            Self::ExpectedList(span) => {
                write!(f, "{:?} expected list", span.start_pos().line_col())
            }
            Self::ExpectedNumber(span) => {
                write!(f, "{:?} expected number", span.start_pos().line_col())
            }
            Self::ExpectedString(span) => {
                write!(f, "{:?} expected string", span.start_pos().line_col())
            }
            Self::ExpectedSymbol(span) => {
                write!(f, "{:?} expected symbol", span.start_pos().line_col())
            }
            Self::ExpectedVector(span) => {
                write!(f, "{:?} expected vector", span.start_pos().line_col())
            }
        }
    }
}

impl<'a> fmt::Debug for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> From<PestError<Rule>> for Error<'a> {
    fn from(e: PestError<Rule>) -> Self {
        Self::Parser(e.to_string())
    }
}
