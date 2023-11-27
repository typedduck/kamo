use crate::parser::{code, Input, ParseResult, Parser};

use super::SequenceError;

/// Counts the number of times a parser succeeds.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = many0_count(char('a'));
/// 
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((1, Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Ok((0, Input::new("b"))));
/// assert_eq!(parser(Input::new("")), Ok((0, Input::new(""))));
/// ```
pub fn many0_count<'a, 'b, F, O>(mut element: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| {
        let mut acc = 0;
        let mut cursor = input;

        while let Ok((_, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc += 1;
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Counts the number of times a parser succeeds. The parser must succeed at
/// least once.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = many1_count(char('a'));
/// 
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((1, Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_1_COUNT,
///     SequenceError::Many1
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_EOF,
///     SequenceError::Many1
/// )));
pub fn many1_count<'a, 'b, F, O>(mut element: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| {
        let (mut acc, mut cursor) = match element.parse(input) {
            Ok((_, next)) => Ok((1, next)),
            Err(mut err) => {
                err.push(input, code::ERR_MANY_1_COUNT, SequenceError::Many1);
                Err(err)
            }
        }?;

        infinite_loop_check!(input, cursor);
        while let Ok((_, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc += 1;
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Counts the number of times a parser succeeds. The parser must succeed at
/// least `m` times and at most `n` times.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
///
/// # Panics
///
/// Panics if `m` is greater than `n`.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = many_m_n_count(2, 3, char('a'));
/// 
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_EOF,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("b")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_EOF,
///     SequenceError::ManyM(2)
/// )));
/// ```
pub fn many_m_n_count<'a, 'b, F, O>(
    m: usize,
    n: usize,
    mut element: F,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        if n - m == 0 && n == 0 {
            return Ok((0, input));
        }

        // If m is 0, then many are optional
        let (mut acc, mut cursor) = if m == 0 {
            if let Ok((_, cursor)) = element.parse(input) {
                (1, cursor)
            } else {
                return Ok((0, input));
            }
        } else {
            let (_, cursor) = element.parse(input).map_err(|mut err| {
                err.push(input, code::ERR_MANY_M, SequenceError::ManyM(m));
                err
            })?;
            (1, cursor)
        };

        infinite_loop_check!(input, cursor);
        for i in 1..n {
            match element.parse(cursor) {
                Ok((_, next)) => {
                    infinite_loop_check!(cursor, next);
                    acc += 1;
                    cursor = next;
                }
                Err(mut err) => {
                    if i < m {
                        err.push(cursor, code::ERR_MANY_M, SequenceError::ManyM(m));
                        return Err(err);
                    }
                    return Ok((acc, cursor));
                }
            }
        }
        Ok((acc, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{prelude::*, ParseError, Position};

    #[test]
    fn many0_count_success() {
        let (value, input) = many0_count(char('a'))(Input::new("")).expect("valid input");

        assert_eq!(value, 0);
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert_eq!(input.current(), None);

        let (value, input) = many0_count(char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = many0_count(char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = many0_count(char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) = many0_count(char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many0_count_infinite_loop() {
        let error = many0_count(opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = many0_count(opt(char('a')))(Input::new("a")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );
    }

    #[test]
    fn many1_count_success() {
        let (value, input) = many1_count(char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = many1_count(char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = many1_count(char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) = many1_count(char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many1_count_infinte_loop() {
        let error = many1_count(opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = many1_count(opt(char('a')))(Input::new("a")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );
    }

    #[test]
    fn many1_count_failure() {
        let error = many1_count(char('a'))(Input::new("öbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_1_COUNT,
                SequenceError::Many1
            )
        );
    }

    #[test]
    fn many_m_n_count_success() {
        let (value, input) =
            many_m_n_count(2, 3, char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) =
            many_m_n_count(2, 3, char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) =
            many_m_n_count(2, 3, char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) =
            many_m_n_count(2, 3, char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, 3);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many_m_n_count_infinite_loop() {
        let error =
            many_m_n_count(2, 3, opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error =
            many_m_n_count(2, 3, opt(char('a')))(Input::new("a")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );
    }

    #[test]
    fn many_m_n_count_failure() {
        let error = many_m_n_count(2, 3, char('a'))(Input::new("öbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        let error =
            many_m_n_count(2, 3, char('a'))(Input::new("aöbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        let error = many_m_n_count(2, 3, char('a'))(Input::new("a")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_EOF,
                SequenceError::ManyM(2)
            )
        );
    }
}
