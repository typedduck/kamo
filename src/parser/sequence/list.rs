use crate::parser::{code, Input, ParseResult, Parser};

use super::SequenceError;

macro_rules! list_loop {
    ($cursor:ident, $list:expr, $element:ident, $separator:ident) => {{
        let mut cursor = $cursor;
        let mut list = $list;

        while let Ok((_, next)) = $separator.parse(cursor) {
            infinite_loop_check!(cursor, next);
            cursor = next;
            match $element.parse(cursor) {
                Ok((item, next)) => {
                    list.push(item);
                    cursor = next;
                }
                Err(mut err) => {
                    err.push(next, code::ERR_LIST_NEXT, SequenceError::ListNext);
                    return Err(err);
                }
            }
        }
        Ok((list, cursor))
    }};
}

/// Parse a list of zero or more elements separated by the given separator.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
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
/// assert_eq!(parser.parse(Input::new("a,")),
///     Err(ParseError::eof(Position::new(2, 1, 3))));
/// ```
pub fn list0<'a, 'b, F1, F2, O1, O2>(
    mut element: F1,
    mut separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    move |input| {
        if let Ok((item, cursor)) = element.parse(input) {
            infinite_loop_check!(input, cursor);
            list_loop!(cursor, vec![item], element, separator)
        } else {
            Ok((vec![], input))
        }
    }
}

/// Parse a list of one or more elements separated by the given separator.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
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
/// assert_eq!(parser.parse(Input::new("a,")),
///     Err(ParseError::eof(Position::new(2, 1, 3))));
/// assert_eq!(parser.parse(Input::new("")), Err(ParseError::eof(
///     Position::new(0, 1, 1))));
/// ```
pub fn list1<'a, 'b, F1, F2, O1, O2>(
    mut element: F1,
    mut separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    move |input| {
        let (item, cursor) = element.parse(input).map_err(|mut err| {
            err.push(input, code::ERR_LIST_START, SequenceError::ListStart);
            err
        })?;

        infinite_loop_check!(input, cursor);
        list_loop!(cursor, vec![item], element, separator)
    }
}

/// Parse a list of at least `m` elements and at most `n` elements separated by
/// the given separator.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
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
/// let mut parser = list_m_n(1, 2, tag("let"), char(','));
///
/// assert_eq!(parser.parse(Input::new("let,let")),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse(Input::new("let,let,let")),
///     Ok((vec!["let", "let"], Input::from(",let"))));
/// assert_eq!(parser.parse("abc,let".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::eof(
///     Position::new(4, 1, 5))));
/// assert_eq!(parser.parse("".into()), Err(ParseError::eof(
///     Position::new(0, 1, 1))));
/// ```
pub fn list_m_n<'a, 'b, F1, F2, O1, O2>(
    m: usize,
    n: usize,
    mut element: F1,
    mut separator: F2,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O1>>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        if n - m == 0 && n == 0 {
            return Ok((vec![], input));
        }

        // If m is 0, then the list is optional
        let (mut list, mut cursor) = if m == 0 {
            if let Ok((item, cursor)) = element.parse(input) {
                (vec![item], cursor)
            } else {
                return Ok((vec![], input));
            }
        } else {
            let (item, cursor) = element.parse(input).map_err(|mut err| {
                err.push(input, code::ERR_LIST_START, SequenceError::ListStart);
                err
            })?;
            (vec![item], cursor)
        };

        infinite_loop_check!(input, cursor);
        for i in 1..n {
            match separator.parse(cursor) {
                Ok((_, next)) => {
                    infinite_loop_check!(cursor, next);
                    cursor = next;
                    match element.parse(cursor) {
                        Ok((item, next)) => {
                            list.push(item);
                            cursor = next;
                        }
                        Err(mut err) => {
                            err.push(cursor, code::ERR_LIST_NEXT, SequenceError::ListNext);
                            return Err(err);
                        }
                    }
                }
                Err(mut err) => {
                    if i < m {
                        err.push(cursor, code::ERR_LIST_M, SequenceError::ListM(m));
                        return Err(err);
                    }
                    return Ok((list, cursor));
                }
            }
        }
        Ok((list, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{prelude::*, Position};

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

        assert_eq!(error, ParseError::eof(Position::new(2, 1, 3)));
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

        assert_eq!(error, ParseError::eof(Position::new(2, 1, 3)));

        let error = list1(char('a'), char(','))(Input::new("")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(0, 1, 1)));
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

        assert_eq!(error, ParseError::eof(Position::new(2, 1, 3)));

        let error = list1(char('a'), char(','))(Input::new("")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(0, 1, 1)));
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

        let error =
            list_m_n(0, 2, char('a'), char(','))(Input::new("a,b")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(2, 1, 3),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );

        let error =
            list_m_n(0, 2, char('a'), char(','))(Input::new("a,")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(2, 1, 3)));

        let (output, input) =
            list_m_n(0, 1, char('a'), char(','))(Input::new("")).expect("valid output");

        assert_eq!(output, vec![]);
        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
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
                code::ERR_LIST_START,
                SequenceError::ListStart
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

        assert_eq!(error, ParseError::eof(Position::new(2, 1, 3)));

        let error =
            list_m_n(1, 2, char('a'), char(','))(Input::new("")).expect_err("invalid output");

        assert_eq!(error, ParseError::eof(Position::new(0, 1, 1)));
    }
}
