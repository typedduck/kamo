use crate::parser::{code, Input, Parser, ParseResult, Span};

use super::SequenceError;

/// Creates a parser that parses `element` followed by `postfix`. The parser
/// will return the result of `element`.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = terminated(tag("let"), char(')'));
/// 
/// assert_eq!(parser(Input::new("let)")), Ok(("let", Input::new(""))));
/// assert_eq!(parser(Input::new("abc)")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAG,
///     CharacterError::Tag("let")
/// )));
/// assert_eq!(parser(Input::new("let(")), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_TERMINATED,
///     SequenceError::Terminated
/// )));
/// assert_eq!(parser(Input::new("let")), Err(ParseError::eof(
///     Position::new(3, 1, 4))));
/// ```
pub fn terminated<'a, 'b, F1, F2, O1, O2>(
    mut element: F1,
    mut postfix: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O1>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1> + 'b,
    F2: Parser<'a, 'b, O2> + 'b,
{
    move |input| {
        let (output, cursor) = element.parse(input)?;
        let (_, cursor) = postfix.parse(cursor).map_err(|mut err| {
            err.push(
                Span::new(cursor.position(), err.span().end()),
                code::ERR_TERMINATED,
                SequenceError::Terminated,
            );
            err
        })?;

        Ok((output, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{character::CharacterError, prelude::*, ParseError, Position};

    #[test]
    fn terminated_success() {
        let (output, input) =
            terminated(tag("let"), char(')'))(Input::new("let)")).expect("valid output");

        assert_eq!(output, "let");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(4, 1, 5));
    }

    #[test]
    fn terminated_failure() {
        let error =
            terminated(tag("let"), char(')'))(Input::new("abc)")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_TAG,
                CharacterError::Tag("let")
            )
        );

        let error =
            terminated(tag("let"), char(')'))(Input::new("let(")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_TERMINATED,
                SequenceError::Terminated
            )
        );

        let error =
            terminated(tag("let"), char(')'))(Input::new("let")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(3, 1, 4)));
    }
}
