use crate::parser::{code, Input, ParseError, ParseResult};

use super::CombinatorError;

/// Asserts that the input is at the end of the input.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CombinatorError, code, Input, Position
/// # };
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
