use crate::parser::{code, literal::Radix, prelude::*, Input, ParseError, ParseResult, Span};

use super::LiteralError;

/// Parses an escaped character literal.
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
pub fn escaped_char(input: Input<'_>) -> ParseResult<'_, char> {
    preceded(char('\\'), escaped_part)(input).map_err(|mut err| {
        err.push(input, code::ERR_ESCAPE, LiteralError::Escape);
        err
    })
}

/// Parses the quoted character literal part after the escape character.
///
/// # Grammar:
///
/// ```text
/// EscapedPart = AsciiEscapeCode
///             | UnicodeEscapeCode
///             | SingleEscapeCode
/// ```
pub fn escaped_part(input: Input<'_>) -> ParseResult<'_, char> {
    any((ascii_escape_code, unicode_escape_code, single_escape_code))(input).map_err(|mut err| {
        err.push(input, code::ERR_ESCAPE_PART, LiteralError::Escape);
        err
    })
}

/// Parses a single escaped character part.
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
    let ch = char::from_u32((hi << 4) + lo).unwrap();

    Ok((ch, cursor))
}

/// Parses a Unicode escaped character.
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

    match char::from_u32(code) {
        Some(ch) => Ok((ch, cursor)),
        None => Err(ParseError::new(
            Span::new(input.position(), cursor.position()),
            code::ERR_UNICODE_CHAR,
            LiteralError::UnicodeChar(code),
        )
        .and_semantic()),
    }
}
