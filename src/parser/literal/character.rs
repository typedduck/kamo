use crate::parser::{code, predicate, prelude::*, Input, ParseResult};

use super::{escaped_char, LiteralError};

/// Parses a quoted character literal. The syntax is mostly what you would
/// expect from a character literal in most programming languages.
///
/// # Grammar:
///
/// ```text
/// QuotedChar        = "'" (EscapedChar | CharQuotable) "'"
/// EscapedChar       = '\' EscapedPart
/// EscapedPart       = AsciiEscapedCode
///                   | UnicodeEscapedCode
///                   | SingleEscapedPart
/// AsciiEscapeCode   = 'x' [0-7] [0-9A-Fa-f]
/// UnicodeEscapeCode = 'u{' [0-9A-Fa-f]{1,6} '}'
/// SingleEscapeCode  = ['"\\abednrt0]
/// CharQuotable      = <is_char_quotable>
/// ```
pub fn quoted_char(input: Input<'_>) -> ParseResult<'_, char> {
    delimited(
        char('\''),
        any((escaped_char, satisfy(predicate::is_char_quotable))),
        char('\''),
    )(input)
    .map_err(|mut err| {
        err.push(err.span(), code::ERR_QUOTED_CHAR, LiteralError::QuotedChar);
        err
    })
}

#[cfg(test)]
mod tests {
    use crate::{parser::{ParseError, Span}, Position};

    use super::*;

    #[test]
    fn quoted_char_success() {
        let (output, input) = quoted_char(Input::new("'a'")).expect("valid output");

        assert_eq!(output, 'a');
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(3, 1, 4));
    }

    #[test]
    fn quoted_char_failure() {
        let error = quoted_char(Input::new("'a")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(2, 1, 3)).and(
                Span::new(Position::new(0, 1, 1), Position::new(2, 1, 3)),
                code::ERR_QUOTED_CHAR,
                LiteralError::QuotedChar
            )
        );
    }
}
