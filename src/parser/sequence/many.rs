use crate::parser::{code, Input, Parser, ParseResult};

use super::SequenceError;

/// Parses zero or more occurrences of the given parser and returns a vector of
/// the results.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = many0(char('a'));
/// 
/// assert_eq!(parser(Input::new("aaa")),
///     Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((vec!['a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("a")), Ok((vec!['a'], Input::new(""))));
/// assert_eq!(parser(Input::new("b")), Ok((vec![], Input::new("b"))));
/// assert_eq!(parser(Input::new("")), Ok((vec![], Input::new(""))));
/// ```
pub fn many0<'a, 'b, F, O>(mut element: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| {
        let mut acc = Vec::new();
        let mut cursor = input;

        while let Ok((item, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc.push(item);
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Parses one or more occurrences of the given parser and returns a vector of
/// the results.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
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
///     code::ERR_EOF,
///     SequenceError::Many1
/// )));
/// ```
pub fn many1<'a, 'b, F, O>(mut element: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| {
        let (mut acc, mut cursor) = match element.parse(input) {
            Ok((item, next)) => Ok((vec![item], next)),
            Err(mut err) => {
                err.push(input, code::ERR_MANY_1, SequenceError::Many1);
                Err(err)
            }
        }?;

        infinite_loop_check!(input, cursor);
        while let Ok((item, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc.push(item);
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Parses between `m` and `n` occurrences of the given parser and returns a
/// vector of the results.
/// 
/// # Panics
/// 
/// Panics if `m` is greater than `n`.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = many_m_n(2, 3, char('a'));
/// 
/// assert_eq!(parser(Input::new("aaa")),
///     Ok((vec!['a', 'a', 'a'], Input::new(""))));
/// assert_eq!(parser(Input::new("aa")), Ok((vec!['a', 'a'], Input::new(""))));
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
pub fn many_m_n<'a, 'b, F, O>(
    m: usize,
    n: usize,
    mut element: F,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        if n - m == 0 && n == 0 {
            return Ok((vec![], input));
        }

        // If m is 0, then many are optional
        let (mut acc, mut cursor) = if m == 0 {
            if let Ok((item, cursor)) = element.parse(input) {
                (vec![item], cursor)
            } else {
                return Ok((vec![], input));
            }
        } else {
            let (item, cursor) = element.parse(input).map_err(|mut err| {
                err.push(input, code::ERR_MANY_M, SequenceError::ManyM(m));
                err
            })?;
            (vec![item], cursor)
        };

        infinite_loop_check!(input, cursor);
        for i in 1..n {
            match element.parse(cursor) {
                Ok((item, next)) => {
                    infinite_loop_check!(cursor, next);
                    acc.push(item);
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
    }

    #[test]
    fn many_m_n_success() {
        let (value, input) =
            many_m_n(2, 3, char('a'))(Input::new("aaa")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) =
            many_m_n(2, 3, char('a'))(Input::new("aaaö")).expect("valid input");

        assert_eq!(value, vec!['a', 'a', 'a']);
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) =
            many_m_n(2, 3, char('ä'))(Input::new("äääo")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('o'));

        let (value, input) =
            many_m_n(2, 3, char('ä'))(Input::new("äääö")).expect("valid input");

        assert_eq!(value, vec!['ä', 'ä', 'ä']);
        assert_eq!(input.position(), Position::new(6, 1, 4));
        assert_eq!(input.current(), Some('ö'));
    }

    #[test]
    fn many_m_n_infinite_loop() {
        let error =
            many_m_n(2, 3, opt(char('a')))(Input::new("ab")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_INFINITE,
                SequenceError::Infinite
            )
        );

        let error =
            many_m_n(2, 3, opt(char('a')))(Input::new("a")).expect_err("invalid input");

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

        let error =
            many_m_n(2, 3, char('a'))(Input::new("aöbbb")).expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        let error = many_m_n(2, 3, char('a'))(Input::new("a")).expect_err("invalid input");

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
