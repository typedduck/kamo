use crate::parser::{code, Input, ParseResult, Parser, Span};

use super::SequenceError;

/// Creates a parser that parses `prefix` followed by `element`. The parser will
/// return the result of `element`.
///
/// # Examples
///
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = preceded(char('('), tag("let"));
///
/// assert_eq!(parser(Input::new("(let")), Ok(("let", Input::new(""))));
/// assert_eq!(parser(Input::new(")let")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_PRECEDED,
///     SequenceError::Preceded
/// )));
/// assert_eq!(parser(Input::new("(abc")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_TAG,
///     CharacterError::Tag("let")
/// )));
/// assert_eq!(parser(Input::new("(")), Err(ParseError::eof(
///     Position::new(1, 1, 2))));
/// ```
pub fn preceded<'a, 'b, F1, F2, O1, O2>(
    mut prefix: F1,
    mut element: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    move |input| {
        let (_, cursor) = prefix.parse(input).map_err(|mut err| {
            err.push(
                Span::new(input.position(), err.span().end()),
                code::ERR_PRECEDED,
                SequenceError::Preceded,
            );
            err
        })?;
        element.parse(cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{character::CharacterError, prelude::*, ParseError, Position};

    #[test]
    fn preceded_success() {
        let (output, input) =
            preceded(char('('), tag("let"))(Input::new("(let")).expect("valid output");

        assert_eq!(output, "let");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 1, 5));
    }

    #[test]
    fn preceded_failure() {
        let error =
            preceded(char('('), tag("let"))(Input::new(")let")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_PRECEDED,
                SequenceError::Preceded
            )
        );

        let error =
            preceded(char('('), tag("let"))(Input::new("(abc")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_TAG,
                CharacterError::Tag("let")
            )
        );

        let error = preceded(char('('), tag("let"))(Input::new("(")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(1, 1, 2)));
    }
}
