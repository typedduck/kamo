use crate::parser::{code, Input, ParseError, ParseResult};

use super::CombinatorError;

/// Asserts that the input is at the end of the input.
///
/// # Errors
///
/// If the input is not at the end of the input, it returns a [`ParseError`]
/// with the code [`ERR_NOT_EOF`](code::ERR_NOT_EOF) and the error
/// [`CombinatorError::Eof`].
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CombinatorError, code, Input}};
/// let mut parser = eof;
///
/// assert_eq!(parser.parse("".into()), Ok(("", Input::from(""))));
/// assert_eq!(parser.parse("a".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_NOT_EOF,
///     CombinatorError::Eof
/// )));
/// ```
pub fn eof(input: Input<'_>) -> ParseResult<'_, &str> {
    if input.is_eof() {
        Ok(("", input))
    } else {
        Err(ParseError::new(
            input,
            code::ERR_NOT_EOF,
            CombinatorError::Eof,
        ))
    }
}
