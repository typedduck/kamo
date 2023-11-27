use crate::parser::{code, Input, Parser, ParseResult};

use super::SequenceError;

/// Creates a parser that parses two values in sequence. The parser will return
/// a tuple of the two values.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = pair(char('a'), char('b'));
/// 
/// assert_eq!(parser(Input::new("ab")), Ok((('a', 'b'), Input::new(""))));
/// assert_eq!(parser(Input::new("ba")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_PAIR_FIRST,
///     SequenceError::PairFirst
/// )));
/// assert_eq!(parser(Input::new("aa")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_PAIR_SECOND,
///     SequenceError::PairSecond
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_EOF,
///     CharacterError::Char('a')
/// )));
/// ```
pub fn pair<'a, 'b, F, G, O1, O2>(
    mut first: F,
    mut second: G,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, (O1, O2)>
where
    O1: 'b,
    O2: 'b,
    F: Parser<'a, 'b, O1>,
    G: Parser<'a, 'b, O2>,
{
    move |input| {
        let (first, cursor) = first.parse(input).map_err(|mut err| {
            err.push(input, code::ERR_PAIR_FIRST, SequenceError::PairFirst);
            err
        })?;
        let (second, cursor) = second.parse(cursor).map_err(|mut err| {
            err.push(cursor, code::ERR_PAIR_SECOND, SequenceError::PairSecond);
            err
        })?;

        Ok(((first, second), cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{prelude::*, ParseError, Position, CharacterError};

    #[test]
    fn pair_success() {
        let mut parser = pair(char('a'), char('b'));

        assert_eq!(parser(Input::new("ab")), Ok((('a', 'b'), Input::new(""))));
        assert_eq!(parser(Input::new("aba")), Ok((('a', 'b'), Input::new("a"))));
    }

    #[test]
    fn pair_failure() {
        let mut parser = pair(char('a'), char('b'));

        assert_eq!(parser(Input::new("ba")), Err(ParseError::new(
            Position::new(0, 1, 1),
            code::ERR_PAIR_FIRST,
            SequenceError::PairFirst
        )));
        assert_eq!(parser(Input::new("aa")), Err(ParseError::new(
            Position::new(1, 1, 2),
            code::ERR_PAIR_SECOND,
            SequenceError::PairSecond
        )));
        assert_eq!(parser(Input::new("")), Err(ParseError::new(
            Position::new(0, 1, 1),
            code::ERR_EOF,
            CharacterError::Char('a')
        )));
    }
}