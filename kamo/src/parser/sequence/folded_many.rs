use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::SequenceError;

/// Fold zero to `n` elements into a single value.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
///
/// The iteration stops when the parser for the element fails or `n` elements
/// have been parsed. If the parser for the element fails, the parser returns
/// the current value and the input before the element was parsed. An exception
/// is made if the error is semantic, then the parser returns the error.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = fold_many_n(usize::MAX, tag("let"),
///     || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
/// assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
/// assert_eq!(parser.parse("abc".into()), Ok((0, Input::from("abc"))));
/// assert_eq!(parser.parse("".into()), Ok((0, Input::from(""))));
/// ```
pub fn fold_many_n<'a, 'b, F1, F2, F3, O1, O2>(
    n: usize,
    item: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: FnMut(O2, Input<'a>, O1) -> O2,
{
    let mut value = counting_fold_many_n(n, item, init, fold);

    move |input| value(input).map(|((_, value), input)| (value, input))
}

/// Fold zero or more elements into a single value.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = fold_many0(tag("let"), || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
/// assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
/// assert_eq!(parser.parse("abc".into()), Ok((0, Input::from("abc"))));
/// assert_eq!(parser.parse("".into()), Ok((0, Input::from(""))));
/// ```
#[inline]
pub fn fold_many0<'a, 'b, F1, F2, F3, O1, O2>(
    element: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: FnMut(O2, Input<'a>, O1) -> O2,
{
    fold_many_n(usize::MAX, element, init, fold)
}

/// Fold one or more elements into a single value.
///
/// Calls [`fold_many_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = fold_many1(tag("let"), || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
/// assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_1,
///     SequenceError::Many1
/// )));
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_1,
///     SequenceError::Many1
/// )));
/// ```
pub fn fold_many1<'a, 'b, F1, F2, F3, O1, O2>(
    item: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: FnMut(O2, Input<'a>, O1) -> O2,
{
    let mut value = counting_fold_many_n(usize::MAX, item, init, fold);

    move |input| {
        let ((count, value), cursor) = value.parse(input)?;

        if count == 0 {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_1,
                SequenceError::Many1,
            ));
        }
        Ok((value, cursor))
    }
}

/// Fold at least `m` and at most `n` elements into a single value.
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
/// let mut parser = fold_many_m_n(2, 3, tag("let"), || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("letletletlet".into()),
///     Ok((3, Input::from("let"))));
/// assert_eq!(parser.parse("let".into()), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser.parse("letabc".into()), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_MANY_M,
///     SequenceError::ManyM(2)
/// )));
/// ```
pub fn fold_many_m_n<'a, 'b, F1, F2, F3, O1, O2>(
    m: usize,
    n: usize,
    item: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: FnMut(O2, Input<'a>, O1) -> O2 + 'a,
{
    assert!(m <= n, "m must be less than or equal to n");

    let mut value = counting_fold_many_n(n, item, init, fold);

    move |input| {
        let ((count, value), cursor) = value.parse(input)?;

        if count < m {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_MANY_M,
                SequenceError::ManyM(m),
            ));
        }
        Ok((value, cursor))
    }
}

fn counting_fold_many_n<'a, 'b, F1, F2, F3, O1, O2>(
    n: usize,
    item: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, (usize, O2)>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: FnMut(O2, Input<'a>, O1) -> O2,
{
    let mut item = item;
    let mut fold = fold;

    move |input| {
        let mut acc = init();
        let mut cursor = input;

        for m in 0..n {
            match item.parse(cursor) {
                Ok((item, next)) => {
                    infinite_loop_check!(cursor, next);
                    acc = fold(acc, cursor, item);
                    cursor = next;
                }
                Err(err) => {
                    if err.is_semantic() {
                        return Err(err);
                    }
                    return Ok(((m, acc), cursor));
                }
            }
        }
        Ok(((n, acc), cursor))
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::prelude::*, Position};

    use super::*;

    #[test]
    fn fold_many0_success() {
        let mut parser = fold_many0(tag("let"), || 0, |acc, _, _| acc + 1);

        assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
        assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
        assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
        assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
        assert_eq!(parser.parse("".into()), Ok((0, Input::from(""))));
    }

    #[test]
    fn fold_many0_infinite_loop() {
        let mut parser = fold_many0(opt(tag("let")), || 0, |acc, _, _| acc + 1);

        assert_eq!(
            parser.parse("letabc".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );

        assert_eq!(
            parser.parse("let".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );
    }

    #[test]
    fn fold_many1_success() {
        let mut parser = fold_many1(tag("let"), || 0, |acc, _, _| acc + 1);

        assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
        assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
        assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
        assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
    }

    #[test]
    fn fold_many1_infinite_loop() {
        let mut parser = fold_many1(opt(tag("let")), || 0, |acc, _, _| acc + 1);

        assert_eq!(
            parser.parse("letabc".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );

        assert_eq!(
            parser.parse("let".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );
    }

    #[test]
    fn fold_many1_failure() {
        let mut parser = fold_many1(tag("let"), || 0, |acc, _, _| acc + 1);

        assert_eq!(
            parser.parse("abc".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_1,
                SequenceError::Many1
            ))
        );

        let error = parser.parse("".into()).expect_err("invalid output");

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
    fn fold_many_m_n_success() {
        let mut parser = fold_many_m_n(2, 3, tag("let"), || 0, |acc, _, _| acc + 1);

        assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
        assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
        assert_eq!(
            parser.parse("letletletlet".into()),
            Ok((3, Input::from("let")))
        );
    }

    #[test]
    fn fold_many_m_n_infinite_loop() {
        let mut parser = fold_many_m_n(2, 3, opt(tag("let")), || 0, |acc, _, _| acc + 1);

        assert_eq!(
            parser.parse("letabc".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );

        assert_eq!(
            parser.parse("let".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_INFINITE,
                SequenceError::Infinite
            ))
        );
    }

    #[test]
    fn fold_many_m_n_failure() {
        let mut parser = fold_many_m_n(2, 3, tag("let"), || 0, |acc, _, _| acc + 1);

        let error = parser.parse("let".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(3, 1, 4)).and(
                Position::new(3, 1, 4),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );

        assert_eq!(
            parser.parse("letabc".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            ))
        );
        assert_eq!(
            parser.parse("abc".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            ))
        );

        let error = parser.parse("".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_MANY_M,
                SequenceError::ManyM(2)
            )
        );
    }
}
