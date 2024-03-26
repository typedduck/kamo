use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::{fold_many_n, SequenceError};

/// Parse zero or up to `n` occurrences of the given parser and returns a
/// vector of the results.
///
/// Calls [`fold_many_n()`].
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many_n(3, char('a'));
///
/// assert_eq!(parser(Input::new("aaaa")),
///    Ok((vec!['a', 'a', 'a'], Input::new("a"))));
/// assert_eq!(parser(Input::new("aaa")),
///    Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")),
///    Ok((vec!['a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("a")),
///    Ok((vec!['a'], Input::new(""))));
/// assert_eq!(parser(Input::new("")),
///    Ok((vec![], Input::new(""))));
/// ```
#[inline]
pub fn many_n<'a, 'b, F, O>(n: usize, item: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    fold_many_n(n, item, Vec::new, |mut acc, _, item| {
        acc.push(item);
        acc
    })
}

/// Parses zero or more occurrences of the given parser and returns a vector of
/// the results.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many0(char('a'));
///
/// assert_eq!(parser(Input::new("aaa")),
///     Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((vec!['a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((vec!['a'], Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Ok((vec![], Input::new("b"))));
/// assert_eq!(parser(Input::new("")), Ok((vec![], Input::new(""))));
/// ```
#[inline]
pub fn many0<'a, 'b, F, O>(item: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    many_n(usize::MAX, item)
}

/// Parses one or more occurrences of the given parser and returns a vector of
/// the results.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = many1(char('a'));
///
/// assert_eq!(parser(Input::new("aaa")),
///     Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((vec!['a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((vec!['a'], Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_1,
///     SequenceError::Many1
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_1,
///     SequenceError::Many1
/// )));
/// ```
pub fn many1<'a, 'b, F, O>(item: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    let mut items = many_n(usize::MAX, item);

    move |input| {
        let (items, cursor) = items.parse(input)?;

        if items.is_empty() {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_1,
                SequenceError::Many1,
            ));
        }
        Ok((items, cursor))
    }
}

/// Parses between `m` and `n` occurrences of the given parser and returns a
/// vector of the results.
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
/// let mut parser = many_m_n(2, 3, char('a'));
///
/// assert_eq!(parser(Input::new("aaa")),
///     Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((vec!['a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("b")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser(Input::new("")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// ```
pub fn many_m_n<'a, 'b, F, O>(
    m: usize,
    n: usize,
    item: F,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    assert!(m <= n, "m must be less than or equal to n");

    let mut items = many_n(n, item);

    move |input| {
        let (items, cursor) = items.parse(input)?;

        if items.len() < m {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_M,
                SequenceError::ManyM(m),
            ));
        }
        Ok((items, cursor))
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
    fn many0_success() {
        let (value, input) = many0(char('a'))(Input::new("")).expect("valid input");

        assert_eq!(value, vec![]);
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert_eq!(input.current(), None);

        let (value, input) = many0(char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = many0(char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = many0(char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) = many0(char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many0_infinite_loop() {
        let error = many0(opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = many0(opt(char('a')))(Input::new("a")).expect_err("invalid input");

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
    fn many1_success() {
        let (value, input) = many1(char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = many1(char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = many1(char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) = many1(char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many1_infinte_loop() {
        let error = many1(opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = many1(opt(char('a')))(Input::new("a")).expect_err("invalid input");

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
    fn many1_failure() {
        let error = many1(char('a'))(Input::new("öbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_1,
                SequenceError::Many1
            )
        );

        let error = many1(char('a'))(Input::new("")).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_MANY_1,
                SequenceError::Many1
            )
        );
    }

    #[test]
    fn many_m_n_success() {
        let (value, input) = many_m_n(2, 3, char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = many_m_n(2, 3, char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = many_m_n(2, 3, char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) = many_m_n(2, 3, char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many_m_n_infinite_loop() {
        let error = many_m_n(2, 3, opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = many_m_n(2, 3, opt(char('a')))(Input::new("a")).expect_err("invalid input");

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
    fn many_m_n_failure() {
        let error = many_m_n(2, 3, char('a'))(Input::new("öbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        let error = many_m_n(2, 3, char('a'))(Input::new("aöbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        let error = many_m_n(2, 3, char('a'))(Input::new("a")).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(1, 1, 2)).and(
                Position::new(1, 1, 2),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );
    }
}
