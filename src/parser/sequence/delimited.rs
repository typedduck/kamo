use crate::parser::{code, Input, ParseResult, Parser, Span};

use super::SequenceError;

/// Creates a parser that parses `prefix` followed by `element` followed by
/// `postfix`. The parser will return the result of `element`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, SequenceError, code, Input, Span}};
/// let mut parser = delimited(char('('), tag("let"), char(')'));
///
/// assert_eq!(parser.parse("(let)".into()), Ok(("let", Input::from(""))));
/// assert_eq!(parser.parse("let)".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_PRECEDED,
///     SequenceError::Preceded
/// )));
/// assert_eq!(parser.parse("(abc".into()), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_TAG,
///     CharacterError::Tag("let")
/// )));
/// assert_eq!(parser.parse("(let ".into()), Err(ParseError::new(
///     Span::new(Position::new(0, 1, 1), Position::new(4, 1, 5)),
///     code::ERR_TERMINATED,
///     SequenceError::Terminated
/// )));
/// 
/// let error = parser.parse("(let".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Span::new(Position::new(0, 1, 1), Position::new(4, 1, 5)),
///     code::ERR_TERMINATED,
///     SequenceError::Terminated,
/// ));
/// ```
pub fn delimited<'a, 'b, F1, F2, F3, O1, O2, O3>(
    mut prefix: F1,
    mut element: F2,
    mut postfix: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    O3: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
    F3: Parser<'a, 'b, O3>,
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
        let (output, cursor) = element.parse(cursor)?;
        let (_, cursor) = postfix.parse(cursor).map_err(|mut err| {
            err.push(
                Span::new(input.position(), cursor.position()), //err.span().end()),
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

    use crate::{
        parser::{character::CharacterError, prelude::*, ParseError},
        Position,
    };

    #[test]
    fn delimited_success() {
        let (output, input) =
            delimited(char('('), tag("let"), char(')'))(Input::new("(let)")).expect("valid output");

        assert_eq!(output, "let");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(5, 1, 6));
    }

    #[test]
    fn delimited_failure() {
        let error = delimited(char('('), tag("let"), char(')'))(Input::new(")let"))
            .expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_PRECEDED,
                SequenceError::Preceded
            )
        );

        let error = delimited(char('('), tag("let"), char(')'))(Input::new("(abc"))
            .expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_TAG,
                CharacterError::Tag("let")
            )
        );

        let error = delimited(char('('), tag("let"), char(')'))(Input::new("(let("))
            .expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(4, 1, 5)),
                code::ERR_TERMINATED,
                SequenceError::Terminated
            )
        );

        let error = delimited(char('('), tag("let"), char(')'))(Input::new("(let"))
            .expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(4, 1, 5)).and(
                Span::new(Position::new(0, 1, 1), Position::new(4, 1, 5)),
                code::ERR_TERMINATED,
                SequenceError::Terminated
            )
        );
    }
}
