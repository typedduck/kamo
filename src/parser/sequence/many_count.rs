use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::{fold_many_n, SequenceError};

/// Counts the number of times a parser succeeds. The parser must succeed zero
/// or at most `n` times.
///
/// Calls [`fold_many_n()`].
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many_n_count(3, char('a'));
///
/// assert_eq!(parser(Input::new("aaaa")), Ok((3, Input::new("a"))));
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((1, Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Ok((0, Input::new("b"))));
/// assert_eq!(parser(Input::new("")), Ok((0, Input::new(""))));
/// ```
#[inline]
pub fn many_n_count<'a, 'b, F, O>(
    n: usize,
    item: F,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    fold_many_n(n, item, || 0, |acc, _, _| acc + 1)
}

/// Counts the number of times a parser succeeds.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many0_count(char('a'));
///
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((1, Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Ok((0, Input::new("b"))));
/// assert_eq!(parser(Input::new("")), Ok((0, Input::new(""))));
/// ```
#[inline]
pub fn many0_count<'a, 'b, F, O>(item: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    many_n_count(usize::MAX, item)
}

/// Counts the number of times a parser succeeds. The parser must succeed at
/// least once.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
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
///     code::ERR_MANY_1_COUNT,
///     SequenceError::Many1
/// )));
pub fn many1_count<'a, 'b, F, O>(item: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    let mut count = many_n_count(usize::MAX, item);

    move |input| {
        let (count, cursor) = count(input)?;

        if count == 0 {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_1_COUNT,
                SequenceError::Many1,
            ));
        }
        Ok((count, cursor))
    }
}

/// Counts the number of times a parser succeeds. The parser must succeed at
/// least `m` times and at most `n` times.
///
/// Calls [`fold_many_n()`].
///
/// # Panics
///
/// Panics if `m` is greater than `n`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many_m_n_count(2, 3, char('a'));
///
/// assert_eq!(parser(Input::new("aaa")), Ok((3, Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((2, Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_MANY_M_COUNT,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("b")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M_COUNT,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M_COUNT,
///     SequenceError::ManyM(2)
/// )));
/// ```
pub fn many_m_n_count<'a, 'b, F, O>(
    m: usize,
    n: usize,
    item: F,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, usize>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    assert!(m <= n, "m must be less than or equal to n");

    let mut count = many_n_count(n, item);

    move |input| {
        let (count, cursor) = count(input)?;

        if count < m {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_M_COUNT,
                SequenceError::ManyM(m),
            ));
        }
        Ok((count, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        parser::{prelude::*, ParseError},
        Position,
    };

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

        let error = many1_count(char('a'))(Input::new("")).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
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
                code::ERR_MANY_M_COUNT,
                SequenceError::ManyM(2)
            )
        );

        let error =
            many_m_n_count(2, 3, char('a'))(Input::new("aöbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_MANY_M_COUNT,
                SequenceError::ManyM(2)
            )
        );

        let error = many_m_n_count(2, 3, char('a'))(Input::new("a")).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(1, 1, 2)).and(
                Position::new(1, 1, 2),
                code::ERR_MANY_M_COUNT,
                SequenceError::ManyM(2)
            )
        );
    }
}
