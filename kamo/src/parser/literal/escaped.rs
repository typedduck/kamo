use crate::parser::{code, literal::Radix, prelude::*, Input, ParseError, ParseResult, Span};

use super::LiteralError;

/// Parses an escaped character literal.
///
/// This is a character literal that starts with a backslash `\` and is followed
/// by an escape code. The escape code can be an ASCII escape code, a Unicode
/// escape code, or a single escape code.
///
/// # Errors
///
/// An error will occur if the character is not a valid escape code. The error
/// code is [`ERR_ESCAPE`](code::ERR_ESCAPE) and the error variant is
/// [`LiteralError::Escape`].
///
/// # Grammar:
///
/// ```text
/// EscapedChar       = '\\' EscapedPart
/// EscapedPart       = AsciiEscapedCode
///                   | UnicodeEscapedCode
///                   | SingleEscapedPart
/// AsciiEscapeCode   = 'x' [0-7] [0-9A-Fa-f]
/// UnicodeEscapeCode = "u{" [0-9A-Fa-f]{1,6} '}'
/// SingleEscapeCode  = ['"\\abednrt0]
/// ```
#[allow(clippy::module_name_repetitions)]
pub fn escaped_char(input: Input<'_>) -> ParseResult<'_, char> {
    preceded(char('\\'), escaped_part)(input).map_err(|mut err| {
        err.push(input, code::ERR_ESCAPE, LiteralError::Escape);
        err
    })
}

/// Parses the quoted character literal part after the escape character.
///
/// This can be an ASCII escape code, a Unicode escape code, or a single escape
/// code.
///
/// # Errors
///
/// An error will occur if the character is not a valid escape code. The error
/// code is [`ERR_ESCAPE_PART`](code::ERR_ESCAPE_PART) and the error variant is
/// [`LiteralError::Escape`].
///
/// # Grammar:
///
/// ```text
/// EscapedPart = AsciiEscapeCode
///             | UnicodeEscapeCode
///             | SingleEscapeCode
/// ```
#[allow(clippy::module_name_repetitions)]
pub fn escaped_part(input: Input<'_>) -> ParseResult<'_, char> {
    any((ascii_escape_code, unicode_escape_code, single_escape_code))(input).map_err(|mut err| {
        err.push(input, code::ERR_ESCAPE_PART, LiteralError::Escape);
        err
    })
}

/// Parses a single escaped character part.
///
/// # Errors
///
/// An error will occur if the character is not a valid single escape code.
/// The error code is [`ERR_SINGLE_ESCAPE_PART`](code::ERR_SINGLE_ESCAPE_PART)
/// and the error variant is [`LiteralError::SingleEscapeCode`].
///
/// # Grammar:
///
/// ```text
/// SingleEscapeCode = ['"\\abednrt0]
/// ```
pub fn single_escape_code(input: Input<'_>) -> ParseResult<'_, char> {
    any((
        value('\'', char('\'')),
        value('"', char('"')),
        value('\\', char('\\')),
        value('\x07', char('a')),
        value('\x08', char('b')),
        value('\x1b', char('e')),
        value('\x7f', char('d')),
        value('\n', char('n')),
        value('\r', char('r')),
        value('\t', char('t')),
        value('\0', char('0')),
    ))(input)
    .map_err(|mut err| {
        err.push(
            input,
            code::ERR_SINGLE_ESCAPE_PART,
            LiteralError::SingleEscapeCode,
        );
        err
    })
}

/// Parses an ASCII escaped character code.
///
/// The code must be within the range of `0x00` to `0x7F`.
///
/// # Errors
///
/// An error will occur if the code is not within the valid range.
/// The error code is [`ERR_ASCII_ESCAPE_CODE`](code::ERR_ASCII_ESCAPE_CODE) and
/// the error variant is [`LiteralError::AsciiEscapeCode`].
///
/// # Panics
///
/// This function will panic if the code point is not a valid ASCII character.
/// This should never happen as the code point is guaranteed to be within the
/// valid range.
///
/// # Grammar:
///
/// ```text
/// AsciiEscapeCode = 'x' [0-7] [0-9A-Fa-f]
/// ```
pub fn ascii_escape_code(input: Input<'_>) -> ParseResult<'_, char> {
    let ((hi, lo), cursor) = preceded(
        char('x'),
        tuple((Radix::Octal.one_digit(), Radix::Hexadecimal.one_digit())),
    )(input)
    .map_err(|mut err| {
        err.push(
            input,
            code::ERR_ASCII_ESCAPE_CODE,
            LiteralError::AsciiEscapeCode,
        );
        err
    })?;
    let code = (hi << 4) + lo;

    Ok((char::from_u32(code).unwrap(), cursor))
}

/// Parses a Unicode escaped character.
///
/// The code point must be within the range of `0x0` to `0x10FFFF`.
///
/// # Errors
///
/// An error will occur if the code point is not within the valid range.
/// The error code is [`ERR_UNICODE_CHAR`](code::ERR_UNICODE_CHAR) and the error
/// variant is [`LiteralError::UnicodeEscapeCode`].
///
/// # Grammar:
///
/// ```text
/// UnicodeEscapeCode = "u{" [0-9A-Fa-f]{1,6} '}'
/// ```
pub fn unicode_escape_code(input: Input<'_>) -> ParseResult<'_, char> {
    let (code, cursor) = delimited(
        tag("u{"),
        fold_many_m_n(
            1,
            6,
            Radix::Hexadecimal.one_digit(),
            || 0u32,
            |acc, _, digit| (acc << 4) + digit,
        ),
        char('}'),
    )(input)
    .map_err(|mut err| {
        err.push(
            input,
            code::ERR_UNICODE_ESCAPE_CODE,
            LiteralError::UnicodeEscapeCode,
        );
        err
    })?;

    char::from_u32(code).map_or_else(
        || {
            Err(ParseError::new(
                Span::new(input.position(), cursor.position()),
                code::ERR_UNICODE_CHAR,
                LiteralError::UnicodeChar(code),
            )
            .and_semantic())
        },
        |ch| Ok((ch, cursor)),
    )
}
