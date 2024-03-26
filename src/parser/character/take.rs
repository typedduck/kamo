use crate::parser::{code, Input, ParseError, ParseResult, Span};

use super::CharacterError;

/// Take `n` characters (unicode code points) from the input and return them as
/// a slice.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = take(3);
///
/// assert_eq!(parser.parse("abc".into()), Ok(("abc", "".into())));
/// assert_eq!(parser.parse("abcd".into()), Ok(("abc", "d".into())));
/// 
/// let error = parser.parse("ab".into()).expect_err("error output");
/// 
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_TAKE,
///     CharacterError::Take(3)
/// ));
/// ```
pub fn take(n: usize) -> impl for<'a> Fn(Input<'a>) -> ParseResult<&'a str> + Copy {
    move |input| {
        let mut cursor = input;

        (0..n).try_for_each(|_| match cursor.current() {
            Some(_) => {
                cursor.advance();
                Ok(())
            }
            None => {
                Err(ParseError::eof(input).and(cursor, code::ERR_TAKE, CharacterError::Take(n)))
            }
        })?;
        let output = &input.as_str()[..(cursor.position().offset() - input.position().offset())];

        Ok((output, cursor))
    }
}

/// Take characters from the input until the predicate returns `false` or end of
/// file is reached.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = take_while0(|c| c.is_ascii_digit());
///
/// assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
/// assert_eq!(parser.parse("abc".into()), Ok(("", "abc".into())));
/// assert_eq!(parser.parse("".into()), Ok(("", "".into())));
/// ```
pub fn take_while0<P>(cond: P) -> impl for<'a> Fn(Input<'a>) -> ParseResult<&'a str> + Copy
where
    P: Fn(char) -> bool + Copy,
{
    move |input| {
        let mut cursor = input;
        let output = cursor.advance_while(cond);

        Ok((output, cursor))
    }
}

/// Take at least one character matching the predicate from the input and
/// continue until the predicate returns `false` or end of file is reached.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = take_while1(|c| c.is_ascii_digit());
///
/// assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAKE_WHILE_1,
///     CharacterError::TakeWhile1
/// )));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
/// 
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAKE_WHILE_1,
///     CharacterError::TakeWhile1
/// ));
/// ```
pub fn take_while1<P>(cond: P) -> impl for<'a> Fn(Input<'a>) -> ParseResult<&'a str>
where
    P: Fn(char) -> bool + Copy,
{
    move |input| {
        let mut cursor = input;

        match cursor.advance_if(cond) {
            Some(true) => {
                cursor.advance_while(cond);
                let output =
                    &input.as_str()[..(cursor.position().offset() - input.position().offset())];

                Ok((output, cursor))
            }
            Some(false) => Err(ParseError::new(
                Span::new(input.position(), cursor.position()),
                code::ERR_TAKE_WHILE_1,
                CharacterError::TakeWhile1,
            )),
            None => Err(ParseError::eof(input).and(
                cursor,
                code::ERR_TAKE_WHILE_1,
                CharacterError::TakeWhile1,
            )),
        }
    }
}

/// Take at least `m` characters matching the predicate from the input and
/// continue until up to `n` or the predicate returns `false` or end of file is
/// reached.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// # Panics
///
/// Panics if `m` is greater than `n`.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, Span, code}};
/// let mut parser = take_while_m_n(2, 4, |c| c.is_ascii_digit());
///
/// assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
/// assert_eq!(parser.parse("1abc".into()), Err(ParseError::new(
///     Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
///     code::ERR_TAKE_WHILE_M,
///     CharacterError::TakeWhileM(2)
/// )));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAKE_WHILE_M,
///     CharacterError::TakeWhileM(2)
/// )));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
/// 
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAKE_WHILE_M,
///     CharacterError::TakeWhileM(2)
/// ));
/// ```
pub fn take_while_m_n<P>(
    m: usize,
    n: usize,
    cond: P,
) -> impl for<'a> Fn(Input<'a>) -> ParseResult<&'a str>
where
    P: Fn(char) -> bool + Copy,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        let mut cursor = input;

        (0..m).try_for_each(|_| match cursor.advance_if(cond) {
            Some(true) => Ok(()),
            Some(false) => Err(ParseError::new(
                Span::new(input.position(), cursor.position()),
                code::ERR_TAKE_WHILE_M,
                CharacterError::TakeWhileM(m),
            )),
            None => Err(ParseError::eof(input).and(
                cursor.position(),
                code::ERR_TAKE_WHILE_M,
                CharacterError::TakeWhileM(m),
            )),
        })?;
        let _ = (m..n).try_for_each(|_| match cursor.advance_if(cond) {
            Some(true) => Ok(()),
            _ => Err(()),
        });
        let output = &input.as_str()[..(cursor.position().offset() - input.position().offset())];

        Ok((output, cursor))
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, Position};

    use super::*;

    #[test]
    fn take_success() {
        let mut parser = take(3);

        assert_eq!(parser.parse("abc".into()), Ok(("abc", "".into())));
        assert_eq!(parser.parse("abcd".into()), Ok(("abc", "d".into())));
    }

    #[test]
    fn take_failure() {
        let mut parser = take(3);

        assert_eq!(
            parser.parse("ab".into()),
            Err(ParseError::new(
                Position::new(2, 1, 3),
                code::ERR_TAKE,
                CharacterError::Take(3)
            ))
        );

        let error = parser.parse("".into()).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_TAKE,
                CharacterError::Take(3)
            )
        );
    }

    #[test]
    fn take_while0_success() {
        let mut parser = take_while0(|c| c.is_ascii_digit());

        assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
        assert_eq!(parser.parse("abc".into()), Ok(("", "abc".into())));
        assert_eq!(parser.parse("".into()), Ok(("", "".into())));
    }

    #[test]
    fn take_while1_success() {
        let mut parser = take_while1(|c| c.is_ascii_digit());

        assert_eq!(parser.parse("123".into()), Ok(("123", "".into())));
        assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
    }

    #[test]
    fn take_while1_failure() {
        let mut parser = take_while1(|c| c.is_ascii_digit());

        assert_eq!(
            parser.parse("abc".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_TAKE_WHILE_1,
                CharacterError::TakeWhile1
            ))
        );

        let error = parser.parse("".into()).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_TAKE_WHILE_1,
                CharacterError::TakeWhile1
            )
        );
    }

    #[test]
    fn take_while_m_n_success() {
        let mut parser = take_while_m_n(2, 4, |c| c.is_ascii_digit());

        assert_eq!(parser.parse("12345abc".into()), Ok(("1234", "5abc".into())));
        assert_eq!(parser.parse("1234abc".into()), Ok(("1234", "abc".into())));
        assert_eq!(parser.parse("123abc".into()), Ok(("123", "abc".into())));
        assert_eq!(parser.parse("12abc".into()), Ok(("12", "abc".into())));

        let mut parser = take_while_m_n(2, 2, |c| c.is_ascii_digit());

        assert_eq!(parser.parse("123abc".into()), Ok(("12", "3abc".into())));
        assert_eq!(parser.parse("12abc".into()), Ok(("12", "abc".into())));

        let mut parser = take_while_m_n(0, 2, |c| c.is_ascii_digit());

        assert_eq!(parser.parse("123abc".into()), Ok(("12", "3abc".into())));
        assert_eq!(parser.parse("12abc".into()), Ok(("12", "abc".into())));
        assert_eq!(parser.parse("1abc".into()), Ok(("1", "abc".into())));
        assert_eq!(parser.parse("abc".into()), Ok(("", "abc".into())));
    }

    #[test]
    fn take_while_m_n_failure() {
        let mut parser = take_while_m_n(2, 4, |c| c.is_ascii_digit());

        assert_eq!(
            parser.parse("1abc".into()),
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
                code::ERR_TAKE_WHILE_M,
                CharacterError::TakeWhileM(2)
            ))
        );
        assert_eq!(
            parser.parse("abc".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_TAKE_WHILE_M,
                CharacterError::TakeWhileM(2)
            ))
        );

        let error = parser.parse("".into()).expect_err("invalid input");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_TAKE_WHILE_M,
                CharacterError::TakeWhileM(2)
            )
        );
    }
}
