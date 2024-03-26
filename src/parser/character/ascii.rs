//! # ASCII character parsers
//!
//! This module contains parsers that match a single or multiple ASCII
//! characters. They return a slice of the input string. If you need to match
//! Unicode characters, use the [`unicode`](crate::parser) module instead.
//!
//! ## Example
//!
//! ```rust
//! # use kamo::{
//! #     Position,
//! #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
//! # };
//! let (output, input) = digit1(Input::from("0123456789"))
//!     .expect("valid output");
//!
//! assert_eq!(output, "0123456789");
//! assert_eq!(input, Input::from(""));
//!
//! let error = digit1(Input::from("a0123456789")).expect_err("error output");
//!
//! assert_eq!(error, ParseError::new(
//!     Position::new(0, 1, 1),
//!     code::ERR_DIGIT,
//!     CharacterError::Digit
//! ));
//!
//! let error = digit1(Input::from("")).expect_err("error output");
//!
//! assert!(error.is_eof());
//! assert_eq!(error, ParseError::new(
//!     Position::new(0, 1, 1),
//!     code::ERR_DIGIT,
//!     CharacterError::Digit
//! ));
//! ```

use crate::parser::{code, predicate, Input, ParseError, ParseResult};

use super::{char, CharacterError};

/// Matches zero or more alphabetical characters: `[a-zA-Z]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = alpha0(Input::from("abc123")).expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from("123"));
///
/// let (output, input) = alpha0(Input::from("123abc")).expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("123abc"));
/// ```
pub fn alpha0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_ascii_alpha);

    Ok((output, cursor))
}

/// Matches at least one alphabetical character: `[a-zA-Z]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = alpha1(Input::from("abc123")).expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from("123"));
///
/// let error = alpha1(Input::from("123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHA,
///     CharacterError::Alpha
/// ));
///
/// let error = alpha1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHA,
///     CharacterError::Alpha
/// ));
/// ```
pub fn alpha1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_ascii_alpha(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_ALPHA,
                CharacterError::Alpha,
            ));
        }
        alpha0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_ALPHA, CharacterError::Alpha))
    }
}

/// Matches zero or more alphanumeric characters: `[a-zA-Z0-9]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = alphanum0(Input::from("abc123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc123");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = alphanum0(Input::from("123abc"))
///     .expect("valid output");
///
/// assert_eq!(output, "123abc");
/// assert_eq!(input, Input::from(""));
/// ```
pub fn alphanum0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_ascii_alphanum);

    Ok((output, cursor))
}

/// Matches at least one alphanumeric character: `[a-zA-Z0-9]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = alphanum1(Input::from("abc123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc123");
/// assert_eq!(input, Input::from(""));
///
/// let error = alphanum1(Input::from("_123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHANUM,
///     CharacterError::AlphaNum
/// ));
///
/// let error = alphanum1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHANUM,
///     CharacterError::AlphaNum
/// ));
/// ```
pub fn alphanum1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_ascii_alphanum(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_ALPHANUM,
                CharacterError::AlphaNum,
            ));
        }
        alphanum0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_ALPHANUM, CharacterError::AlphaNum))
    }
}

/// Matches zero or more decimal digits: `[0-9]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = digit0(Input::from("0123456789"))
///     .expect("valid output");
///
/// assert_eq!(output, "0123456789");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = digit0(Input::from("a0123456789"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("a0123456789"));
/// ```
pub fn digit0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_digit);

    Ok((output, cursor))
}

/// Matches at least one decimal digit: `[0-9]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = digit1(Input::from("0123456789"))
///     .expect("valid output");
///
/// assert_eq!(output, "0123456789");
/// assert_eq!(input, Input::from(""));
///
/// let error = digit1(Input::from("a0123456789")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_DIGIT,
///     CharacterError::Digit
/// ));
///
/// let error = digit1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_DIGIT,
///     CharacterError::Digit
/// ));
/// ```
pub fn digit1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_digit(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_DIGIT,
                CharacterError::Digit,
            ));
        }
        digit0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_DIGIT, CharacterError::Digit))
    }
}

/// Matches zero or more binary digits: `[0-1]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = bin_digit0(Input::from("0101"))
///     .expect("valid output");
///
/// assert_eq!(output, "0101");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = bin_digit0(Input::from("20101"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("20101"));
/// ```
pub fn bin_digit0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_bin_digit);

    Ok((output, cursor))
}

/// Matches at least one binary digit: `[0-1]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = bin_digit1(Input::from("0101"))
///     .expect("valid output");
///
/// assert_eq!(output, "0101");
/// assert_eq!(input, Input::from(""));
///
/// let error = bin_digit1(Input::from("20101")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_BIN_DIGIT,
///     CharacterError::BinDigit
/// ));
///
/// let error = bin_digit1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_BIN_DIGIT,
///     CharacterError::BinDigit
/// ));
/// ```
pub fn bin_digit1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_bin_digit(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_BIN_DIGIT,
                CharacterError::BinDigit,
            ));
        }
        bin_digit0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_BIN_DIGIT, CharacterError::BinDigit))
    }
}

/// Matches zero or more octal digits: `[0-7]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = oct_digit0(Input::from("01234567"))
///     .expect("valid output");
///
/// assert_eq!(output, "01234567");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = oct_digit0(Input::from("801234567"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("801234567"));
/// ```
pub fn oct_digit0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_oct_digit);

    Ok((output, cursor))
}

/// Matches at least one octal digit: `[0-7]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = oct_digit1(Input::from("01234567"))
///     .expect("valid output");
///
/// assert_eq!(output, "01234567");
/// assert_eq!(input, Input::from(""));
///
/// let error = oct_digit1(Input::from("801234567")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_OCT_DIGIT,
///     CharacterError::OctDigit
/// ));
///
/// let error = oct_digit1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_OCT_DIGIT,
///     CharacterError::OctDigit
/// ));
/// ```
pub fn oct_digit1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_oct_digit(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_OCT_DIGIT,
                CharacterError::OctDigit,
            ));
        }
        oct_digit0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_OCT_DIGIT, CharacterError::OctDigit))
    }
}

/// Matches zero or more hexadecimal digits: `[0-9A-Fa-f]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = hex_digit0(Input::from("0123456789abcdef"))
///     .expect("valid output");
///
/// assert_eq!(output, "0123456789abcdef");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = hex_digit0(Input::from("g0123456789abcdef"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("g0123456789abcdef"));
/// ```
pub fn hex_digit0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_hex_digit);

    Ok((output, cursor))
}

/// Matches at least one hexadecimal digit: `[0-9A-Fa-f]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = hex_digit1(Input::from("0123456789abcdef"))
///     .expect("valid output");
///
/// assert_eq!(output, "0123456789abcdef");
/// assert_eq!(input, Input::from(""));
///
/// let error = hex_digit1(Input::from("g0123456789abcdef"))
///     .expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_HEX_DIGIT,
///     CharacterError::HexDigit
/// ));
///
/// let error = hex_digit1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_HEX_DIGIT,
///     CharacterError::HexDigit
/// ));
/// ```
pub fn hex_digit1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_hex_digit(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_HEX_DIGIT,
                CharacterError::HexDigit,
            ));
        }
        hex_digit0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_HEX_DIGIT, CharacterError::HexDigit))
    }
}

/// Matches zero or more whitespaces: `[ \t\n\r]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = whitespace0(Input::from(" \t\n\r"))
///     .expect("valid output");
///
/// assert_eq!(output, " \t\n\r");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = whitespace0(Input::from("a \t\n\r"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("a \t\n\r"));
/// ```
pub fn whitespace0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_ascii_whitespace);

    Ok((output, cursor))
}

/// Matches at least one whitespace: `[ \t\n\r]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = whitespace1(Input::from(" \t\n\r"))
///     .expect("valid output");
///
/// assert_eq!(output, " \t\n\r");
/// assert_eq!(input, Input::from(""));
///
/// let error = whitespace1(Input::from("a \t\n\r")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_WHITESPACE,
///     CharacterError::Whitespace
/// ));
///
/// let error = whitespace1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_WHITESPACE,
///     CharacterError::Whitespace
/// ));
/// ```
pub fn whitespace1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_ascii_whitespace(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_WHITESPACE,
                CharacterError::Whitespace,
            ));
        }
        whitespace0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_WHITESPACE, CharacterError::Whitespace))
    }
}

/// Matches zero or more spaces: `[ \t]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = space0(Input::from(" \t")).expect("valid output");
///
/// assert_eq!(output, " \t");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = space0(Input::from("a \t")).expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("a \t"));
/// ```
pub fn space0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_ascii_space);

    Ok((output, cursor))
}

/// Matches at least one space: `[ \t]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = space1(Input::from(" \t")).expect("valid output");
///
/// assert_eq!(output, " \t");
/// assert_eq!(input, Input::from(""));
///
/// let error = space1(Input::from("a \t")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_SPACE,
///     CharacterError::Space
/// ));
///
/// let error = space1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_SPACE,
///     CharacterError::Space
/// ));
/// ```
pub fn space1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_ascii_space(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_SPACE,
                CharacterError::Space,
            ));
        }
        space0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_SPACE, CharacterError::Space))
    }
}

/// Matches zero or more ASCII graphic characters: `[!-~]`, `U+0021..U+007E`.
///
/// Note that this does not include the space `U+0020` character.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::ascii::*, Input};
/// let (output, input) = graphic0(Input::from("abc")).expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = graphic0(Input::from(" abc")).expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from(" abc"));
/// ```
pub fn graphic0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(|ch| ch.is_ascii_graphic());

    Ok((output, cursor))
}

/// Matches at least one ASCII graphic character: `[!-~]`, `U+0021..U+007E`.
///
/// Note that this does not include the space `U+0020` character.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = graphic1(Input::from("abc")).expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from(""));
///
/// let error = graphic1(Input::from(" abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_GRAPHIC,
///     CharacterError::Graphic
/// ));
///
/// let error = graphic1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_GRAPHIC,
///     CharacterError::Graphic
/// ));
/// ```
pub fn graphic1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_ascii_printable(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_GRAPHIC,
                CharacterError::Graphic,
            ));
        }
        graphic0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_GRAPHIC, CharacterError::Graphic))
    }
}

/// Matches one or more line endings: `[\r]?[\n]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = line_ending(Input::from("\r\n"))
///     .expect("valid output");
///
/// assert_eq!(output, "\r\n");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = line_ending(Input::from("\n"))
///     .expect("valid output");
///
/// assert_eq!(output, "\n");
/// assert_eq!(input, Input::from(""));
///
/// let error = line_ending(Input::from("\r")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LINE_ENDING,
///     CharacterError::LineEnding
/// ));
///
/// let error = line_ending(Input::from("\r\r")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LINE_ENDING,
///     CharacterError::LineEnding
/// ));
///
/// let error = line_ending(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///    Position::new(0, 1, 1),
///    code::ERR_LINE_ENDING,
///    CharacterError::LineEnding
/// ));
/// ```
pub fn line_ending(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;

    match cursor.current() {
        Some('\r') => {
            if let Some('\n') = cursor.advance() {
                cursor.advance();
                Ok(("\r\n", cursor))
            } else {
                Err(ParseError::new(
                    input,
                    code::ERR_LINE_ENDING,
                    CharacterError::LineEnding,
                ))
            }
        }
        Some('\n') => {
            cursor.advance();
            Ok(("\n", cursor))
        }
        Some(_) => Err(ParseError::new(
            input,
            code::ERR_LINE_ENDING,
            CharacterError::LineEnding,
        )),
        None => Err(ParseError::eof(input).and(
            input,
            code::ERR_LINE_ENDING,
            CharacterError::LineEnding,
        )),
    }
}

/// Matches a carriage return followed by a line feed: `[\r][\n]`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{
/// #     Position,
/// #     parser::{prelude::{*, ascii::*}, CharacterError, code, Input}
/// # };
/// let (output, input) = crlf(Input::from("\r\n")).expect("valid output");
///
/// assert_eq!(output, "\r\n");
/// assert_eq!(input, Input::from(""));
///
/// let error = crlf(Input::from("\n")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LINE_ENDING,
///     CharacterError::Crlf
/// ));
///
/// let error = crlf(Input::from("\r")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LINE_ENDING,
///     CharacterError::Crlf,
/// ));
/// ```
pub fn crlf(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;

    match (cursor.current(), cursor.advance()) {
        (Some('\r'), Some('\n')) => {
            cursor.advance();
            Ok(("\r\n", cursor))
        }
        (Some('\r'), None) | (None, _) => {
            Err(ParseError::eof(input).and(input, code::ERR_LINE_ENDING, CharacterError::Crlf))
        }
        (Some(_), _) => Err(ParseError::new(
            input,
            code::ERR_LINE_ENDING,
            CharacterError::Crlf,
        )),
    }
}

/// Matches a newline character: `[\n]`.
///
/// It is an alias for [`char('\n')`](super::char).
#[inline]
pub fn newline(input: Input<'_>) -> ParseResult<char> {
    char('\n')(input)
}

/// Matches a tab character: `[\t]`.
///
/// It is an alias for [`char('\t')`](super::char).
#[inline]
pub fn tab(input: Input<'_>) -> ParseResult<char> {
    char('\t')(input)
}

#[cfg(test)]
mod tests {
    use crate::Position;

    use super::*;

    #[test]
    fn alpha_chars() {
        let (output, input) = alpha0(Input::from("abc123")).expect("valid output");

        assert_eq!(output, "abc");
        assert_eq!(input, Input::from("123"));

        let (output, input) = alpha0(Input::from("123abc")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("123abc"));

        let (output, input) = alpha1(Input::from("abc123")).expect("valid output");

        assert_eq!(output, "abc");
        assert_eq!(input, Input::from("123"));

        let error = alpha1(Input::from("123abc")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_ALPHA,
                CharacterError::Alpha
            )
        );

        let error = alpha1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_ALPHA,
                CharacterError::Alpha
            )
        );
    }

    #[test]
    fn alphanum_chars() {
        let (output, input) = alphanum0(Input::from("abc123")).expect("valid output");

        assert_eq!(output, "abc123");
        assert_eq!(input, Input::from(""));

        let (output, input) = alphanum0(Input::from("123abc")).expect("valid output");

        assert_eq!(output, "123abc");
        assert_eq!(input, Input::from(""));

        let (output, input) = alphanum1(Input::from("abc123")).expect("valid output");

        assert_eq!(output, "abc123");
        assert_eq!(input, Input::from(""));

        let error = alphanum1(Input::from("_123abc")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_ALPHANUM,
                CharacterError::AlphaNum
            )
        );

        let error = alphanum1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_ALPHANUM,
                CharacterError::AlphaNum
            )
        );
    }

    #[test]
    fn bin_digits() {
        let (output, input) = bin_digit0(Input::from("0101")).expect("valid output");

        assert_eq!(output, "0101");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 1, 5));

        let (output, input) = bin_digit0(Input::from("20101")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("20101"));
        assert_eq!(input.current(), Some('2'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = bin_digit0(Input::from("a0101")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("a0101"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = bin_digit1(Input::from("0101")).expect("valid output");

        assert_eq!(output, "0101");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 1, 5));

        let error = bin_digit1(Input::from("20101")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_BIN_DIGIT,
                CharacterError::BinDigit
            )
        );

        let error = bin_digit1(Input::from("a0101")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_BIN_DIGIT,
                CharacterError::BinDigit
            )
        );

        let error = bin_digit1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_BIN_DIGIT,
                CharacterError::BinDigit
            )
        );
    }

    #[test]
    fn oct_digits() {
        let (output, input) = oct_digit0(Input::from("01234567")).expect("valid output");

        assert_eq!(output, "01234567");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(8, 1, 9));

        let (output, input) = oct_digit0(Input::from("801234567")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("801234567"));
        assert_eq!(input.current(), Some('8'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = oct_digit0(Input::from("a01234567")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("a01234567"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = oct_digit1(Input::from("01234567")).expect("valid output");

        assert_eq!(output, "01234567");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(8, 1, 9));

        let error = oct_digit1(Input::from("801234567")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_OCT_DIGIT,
                CharacterError::OctDigit
            )
        );

        let error = oct_digit1(Input::from("a01234567")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_OCT_DIGIT,
                CharacterError::OctDigit
            )
        );

        let error = oct_digit1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_OCT_DIGIT,
                CharacterError::OctDigit
            )
        );
    }

    #[test]
    fn digits() {
        let (output, input) = digit0(Input::from("0123456789")).expect("valid output");

        assert_eq!(output, "0123456789");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(10, 1, 11));

        let (output, input) = digit0(Input::from("a0123456789")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("a0123456789"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = digit1(Input::from("0123456789")).expect("valid output");

        assert_eq!(output, "0123456789");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(10, 1, 11));

        let error = digit1(Input::from("a0123456789")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_DIGIT,
                CharacterError::Digit
            )
        );

        let error = digit1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_DIGIT,
                CharacterError::Digit
            )
        );
    }

    #[test]
    fn hex_digits() {
        let (output, input) = hex_digit0(Input::from("0123456789abcdef")).expect("valid output");

        assert_eq!(output, "0123456789abcdef");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(16, 1, 17));

        let (output, input) = hex_digit0(Input::from("g0123456789abcdef")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("g0123456789abcdef"));
        assert_eq!(input.current(), Some('g'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = hex_digit1(Input::from("0123456789abcdef")).expect("valid output");

        assert_eq!(output, "0123456789abcdef");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(16, 1, 17));

        let error = hex_digit1(Input::from("g0123456789abcdef")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_HEX_DIGIT,
                CharacterError::HexDigit
            )
        );

        let error = hex_digit1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_HEX_DIGIT,
                CharacterError::HexDigit
            )
        );
    }

    #[test]
    fn whitespaces() {
        let (output, input) = whitespace0(Input::from(" \t\n\r")).expect("valid output");

        assert_eq!(output, " \t\n\r");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 2, 1));

        let (output, input) = whitespace0(Input::from("a \t\n\r")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("a \t\n\r"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = whitespace1(Input::from(" \t\n\r")).expect("valid output");

        assert_eq!(output, " \t\n\r");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 2, 1));

        let error = whitespace1(Input::from("a \t\n\r")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_WHITESPACE,
                CharacterError::Whitespace
            )
        );

        let error = whitespace1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_WHITESPACE,
                CharacterError::Whitespace
            )
        );
    }

    #[test]
    fn spaces() {
        let (output, input) = space0(Input::from(" \t")).expect("valid output");

        assert_eq!(output, " \t");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(2, 1, 3));

        let (output, input) = space0(Input::from("a \t")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from("a \t"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = space1(Input::from(" \t")).expect("valid output");

        assert_eq!(output, " \t");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(2, 1, 3));

        let error = space1(Input::from("a \t")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_SPACE,
                CharacterError::Space
            )
        );

        let error = space1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_SPACE,
                CharacterError::Space
            )
        );
    }

    #[test]
    fn graphics() {
        let (output, input) = graphic0(Input::from("abc")).expect("valid output");

        assert_eq!(output, "abc");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(3, 1, 4));

        let (output, input) = graphic0(Input::from(" abc")).expect("valid output");

        assert_eq!(output, "");
        assert_eq!(input, Input::from(" abc"));
        assert_eq!(input.current(), Some(' '));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = graphic1(Input::from("abc")).expect("valid output");

        assert_eq!(output, "abc");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(3, 1, 4));

        let error = graphic1(Input::from(" abc")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_GRAPHIC,
                CharacterError::Graphic
            )
        );

        let error = graphic1(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_GRAPHIC,
                CharacterError::Graphic
            )
        );
    }

    #[test]
    fn line_endings() {
        let (output, input) = line_ending(Input::from("\r\n")).expect("valid output");

        assert_eq!(output, "\r\n");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(2, 2, 1));

        let (output, input) = line_ending(Input::from("\n")).expect("valid output");

        assert_eq!(output, "\n");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(1, 2, 1));

        let error = line_ending(Input::from("\r")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::LineEnding
            )
        );

        let error = line_ending(Input::from("\r\r")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::LineEnding
            )
        );

        let error = line_ending(Input::from("")).expect_err("error output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::LineEnding
            )
        );
    }

    #[test]
    fn crlfs() {
        let (output, input) = crlf(Input::from("\r\n")).expect("valid output");

        assert_eq!(output, "\r\n");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(2, 2, 1));

        let error = crlf(Input::from("\n")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::Crlf
            )
        );

        let error = crlf(Input::from("\ra")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::Crlf
            )
        );

        let error = crlf(Input::from("\r")).expect_err("error output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LINE_ENDING,
                CharacterError::Crlf
            )
        );
    }
}
