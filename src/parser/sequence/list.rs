use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::{fold_list_n, SequenceError};

/// Parse a list of zero or up to `n` elements separated by the given separator
/// and return a vector of the results.
///
/// Calls [`fold_list_n()`].
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = list_n(2, char('a'), char(','));
///
/// assert_eq!(parser.parse(Input::new("abc")),
///     Ok((vec!['a'], Input::from("bc"))));
/// assert_eq!(parser.parse(Input::new("a,a")),
///     Ok((vec!['a', 'a'], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("a,a,a")),
///     Ok((vec!['a', 'a'], Input::from(",a"))));
/// assert_eq!(parser.parse(Input::new("bac")),
///     Ok((vec![], Input::from("bac"))));
/// assert_eq!(parser.parse(Input::new("")), Ok((vec![], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("a,b")), Err(ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// assert_eq!(parser.parse(Input::new("a,")),Err(ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
#[inline]
pub fn list_n<'a, 'b, F1, F2, O1, O2>(
    n: usize,
    item: F1,
    separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    fold_list_n(n, item, separator, Vec::new, |mut acc, _, item| {
        acc.push(item);
        acc
    })
}

/// Parse a list of zero or more elements separated by the given separator and
/// returns a vector of the results.
///
/// Calls [`fold_list_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = list0(char('a'), char(','));
///
/// assert_eq!(parser.parse(Input::new("abc")),
///     Ok((vec!['a'], Input::from("bc"))));
/// assert_eq!(parser.parse(Input::new("a,a")),
///     Ok((vec!['a', 'a'], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("bac")),
///     Ok((vec![], Input::from("bac"))));
/// assert_eq!(parser.parse(Input::new("")), Ok((vec![], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("a,b")), Err(ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// assert_eq!(parser.parse(Input::new("a,")),Err(ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
#[inline]
pub fn list0<'a, 'b, F1, F2, O1, O2>(
    item: F1,
    separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    list_n(usize::MAX, item, separator)
}

/// Parse a list of one or more elements separated by the given separator and
/// returns a vector of the results.
///
/// Calls [`fold_list_n()`], `n` is set to `usize::MAX`.
/// 
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = list1(char('a'), char(','));
///
/// assert_eq!(parser.parse(Input::new("abc")),
///     Ok((vec!['a'], Input::from("bc"))));
/// assert_eq!(parser.parse(Input::new("a,a")),
///     Ok((vec!['a', 'a'], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("bac")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
///     SequenceError::ListStart
/// )));
/// assert_eq!(parser.parse(Input::new("a,b")), Err(ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
///
/// let error = parser.parse("a,".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext,
/// ));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
///     SequenceError::ListStart,
/// ));
/// ```
pub fn list1<'a, 'b, F1, F2, O1, O2>(
    item: F1,
    separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    let mut items = list_n(usize::MAX, item, separator);

    move |input| {
        let (items, cursor) = items.parse(input)?;

        if items.is_empty() {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_LIST_START,
                SequenceError::ListStart,
            ));
        }
        Ok((items, cursor))
    }
}

/// Parse a list of at least `m` elements and at most `n` elements separated by
/// the given separator and returns a vector of the results.
///
/// Calls [`fold_list_n()`].
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
/// let mut parser = list_m_n(1, 2, tag("let"), char(','));
///
/// assert_eq!(parser.parse(Input::new("let,let")),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("let,let,let")),
///     Ok((vec!["let", "let"], Input::from(",let"))));
/// assert_eq!(parser.parse("abc,let".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_M,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListM(1)
/// )));
///
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_M,
///     SequenceError::ListM(1),
/// ));
/// ```
pub fn list_m_n<'a, 'b, F1, F2, O1, O2>(
    m: usize,
    n: usize,
    item: F1,
    separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    assert!(m <= n, "m must be less than or equal to n");

    let mut items = list_n(n, item, separator);

    move |input| {
        let (items, cursor) = items.parse(input)?;

        if items.len() < m {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_LIST_M,
                SequenceError::ListM(m),
            ));
        }
        Ok((items, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{parser::prelude::*, Position};

    #[test]
    fn list0_success() {
        let (output, input) = list0(char('a'), char(','))(Input::new("abc")).expect("valid output");

        assert_eq!(output, vec!['a']);
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let (output, input) = list0(char('a'), char(','))(Input::new("a,a")).expect("valid output");

        assert_eq!(output, vec!['a', 'a']);
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(3, 1, 4));

        let (output, input) = list0(char('a'), char(','))(Input::new("bac")).expect("valid output");

        assert_eq!(output, vec![]);
        assert_eq!(input, Input::from("bac"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (output, input) = list0(char('a'), char(','))(Input::new("")).expect("valid output");

        assert_eq!(output, vec![]);
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(0, 1, 1));
    }

    #[test]
    fn list0_infinite_loop() {
        let error =
            list0(char('a'), opt(char(',')))(Input::new("a,aa")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = list0(char('a'), opt(char(',')))(Input::new("a")).expect_err("invalid output");

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
    fn list0_failure() {
        let error = list0(char('a'), char(','))(Input::new("a,b")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error = list0(char('a'), char(','))(Input::new("a,")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(2, 1, 3)).and(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );
    }

    #[test]
    fn list1_success() {
        let (output, input) = list1(char('a'), char(','))(Input::new("abc")).expect("valid output");

        assert_eq!(output, vec!['a']);
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let (output, input) = list1(char('a'), char(','))(Input::new("a,a")).expect("valid output");

        assert_eq!(output, vec!['a', 'a']);
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(3, 1, 4));
    }

    #[test]
    fn list1_infinite_loop() {
        let error =
            list1(char('a'), opt(char(',')))(Input::new("aa,a")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error =
            list1(char('a'), opt(char(',')))(Input::new("a,aa")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = list1(char('a'), opt(char(',')))(Input::new("a")).expect_err("invalid output");

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
    fn list1_failure() {
        let error = list1(char('a'), char(','))(Input::new("bac")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LIST_START,
                SequenceError::ListStart
            )
        );

        let error = list1(char('a'), char(','))(Input::new("a,b")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error = list1(char('a'), char(','))(Input::new("a,")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(2, 1, 3)).and(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error = list1(char('a'), char(','))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_LIST_START,
                SequenceError::ListStart
            )
        );
    }

    #[test]
    fn list_m_n_success() {
        let (output, input) =
            list_m_n(0, 1, char('a'), char(','))(Input::new("abc")).expect("valid output");

        assert_eq!(output, vec!['a']);
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let (output, input) =
            list_m_n(0, 1, char('a'), char(','))(Input::new("a,a")).expect("valid output");

        assert_eq!(output, vec!['a']);
        assert_eq!(input, Input::from(",a"));
        assert_eq!(input.current(), Some(','));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let (output, input) =
            list_m_n(0, 1, char('a'), char(','))(Input::new("bac")).expect("valid output");

        assert_eq!(output, vec![]);
        assert_eq!(input, Input::from("bac"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(0, 1, 1));
    }

    #[test]
    fn list_m_n_infinite_loop() {
        let error = list_m_n(1, 3, char('a'), opt(char(',')))(Input::new("aa,a"))
            .expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error = list_m_n(1, 3, char('a'), opt(char(',')))(Input::new("a,aa"))
            .expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error =
            list_m_n(1, 3, char('a'), opt(char(',')))(Input::new("a")).expect_err("invalid output");

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
    fn list_m_n_failure() {
        let error =
            list_m_n(1, 2, char('a'), char(','))(Input::new("bac")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LIST_M,
                SequenceError::ListM(1)
            )
        );

        let error =
            list_m_n(1, 2, char('a'), char(','))(Input::new("a,b")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error =
            list_m_n(1, 2, char('a'), char(','))(Input::new("a,")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(2, 1, 3)).and(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error =
            list_m_n(1, 2, char('a'), char(','))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_LIST_M,
                SequenceError::ListM(1)
            )
        );
    }
}
