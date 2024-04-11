use crate::parser::{
    character::CharacterError, code, prelude::ascii, Input, ParseError, ParseResult,
};

/// This helper enum represents the radix of a number literal.
///
/// The radix is the base of the number literal. For example, the number literal
/// `0b1010` has a radix of `2`. The number literal `0x1F` has a radix of `16`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Radix {
    /// The binary radix with the digit set `[0-1]`.
    Binary,
    /// The octal radix with the digit set `[0-7]`.
    Octal,
    /// The decimal radix with the digit set `[0-9]`.
    Decimal,
    /// The hexadecimal radix with the digit set `[0-9A-Fa-f]`.
    Hexadecimal,
}

impl Radix {
    /// Returns the base of the radix as a `u32`.
    #[must_use]
    pub const fn base(&self) -> u32 {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Decimal => 10,
            Self::Hexadecimal => 16,
        }
    }

    /// Returns a function that parses zero or more digits of the radix. Returns
    /// the digits as a `&str`.
    pub fn digit0(&self) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> {
        match self {
            Self::Binary => ascii::bin_digit0,
            Self::Octal => ascii::oct_digit0,
            Self::Decimal => ascii::digit0,
            Self::Hexadecimal => ascii::hex_digit0,
        }
    }

    /// Returns a function that parses one or more digits of the radix. Returns
    /// the digits as a `&str`.
    pub fn digit1(&self) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> {
        match self {
            Self::Binary => ascii::bin_digit1,
            Self::Octal => ascii::oct_digit1,
            Self::Decimal => ascii::digit1,
            Self::Hexadecimal => ascii::hex_digit1,
        }
    }

    /// Returns a function that parses exactly one digit of the radix and returns
    /// the digit as a `u32`.
    pub fn one_digit(&self) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, u32> {
        let (radix, code, error) = match self {
            Self::Binary => (2, code::ERR_BIN_DIGIT, CharacterError::BinDigit),
            Self::Octal => (8, code::ERR_OCT_DIGIT, CharacterError::OctDigit),
            Self::Decimal => (10, code::ERR_DIGIT, CharacterError::Digit),
            Self::Hexadecimal => (16, code::ERR_HEX_DIGIT, CharacterError::HexDigit),
        };

        move |input| {
            if let Some(ch) = input.current() {
                if let Some(digit) = ch.to_digit(radix) {
                    let mut cursor = input;

                    cursor.advance();
                    return Ok((digit, cursor));
                }
                return Err(ParseError::new(input, code, error.to_string()));
            }
            Err(ParseError::eof(input).and(input, code, error.to_string()))
        }
    }
}
