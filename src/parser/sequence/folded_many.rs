use crate::parser::{code, Input, ParseResult, Parser};

use super::SequenceError;

/// Fold zero or more elements into a single value.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
///
/// The fold function is called with the initial value, the input, and the
/// element. The `input` parameter is the input before the element is parsed.
/// The result of the fold function is used as the new initial value
/// for the next iteration.
///
/// # Examples
///
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
/// let mut parser = fold_many0(tag("let"), || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((1, Input::from(""))));
/// assert_eq!(parser.parse("letabc".into()), Ok((1, Input::from("abc"))));
/// assert_eq!(parser.parse("abc".into()), Ok((0, Input::from("abc"))));
/// assert_eq!(parser.parse("".into()), Ok((0, Input::from(""))));
/// ```
pub fn fold_many0<'a, 'b, F1, F2, F3, O1, O2>(
    mut element: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: Fn(O2, Input<'a>, O1) -> O2,
{
    move |input| {
        let mut acc = init();
        let mut cursor = input;

        while let Ok((item, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc = fold(acc, cursor, item);
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Fold one or more elements into a single value.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
///
/// The fold function is called with the initial value, the input, and the
/// element. The `input` parameter is the input before the element is parsed.
/// The result of the fold function is used as the new initial value
/// for the next iteration.
///
/// # Examples
///
/// ```rust
/// # use drake::parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Position, Span
/// # };
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
///     code::ERR_EOF,
///     SequenceError::Many1
/// )));
/// ```
pub fn fold_many1<'a, 'b, F1, F2, F3, O1, O2>(
    mut element: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: Fn(O2, Input<'a>, O1) -> O2,
{
    move |input| {
        let (mut acc, mut cursor) = match element.parse(input) {
            Ok((item, next)) => Ok((fold(init(), input, item), next)),
            Err(mut err) => {
                err.push(input, code::ERR_MANY_1, SequenceError::Many1);
                Err(err)
            }
        }?;

        infinite_loop_check!(input, cursor);
        while let Ok((item, next)) = element.parse(cursor) {
            infinite_loop_check!(cursor, next);
            acc = fold(acc, cursor, item);
            cursor = next;
        }
        Ok((acc, cursor))
    }
}

/// Fold at least `m` and at most `n` elements into a single value.
///
/// When the parser for the element excepts an empty input and no progress is
/// made, the parser returns an infinte loop error. Sequence parsers must make
/// progress on every successful iteration.
///
/// The fold function is called with the initial value, the input, and the
/// element. The `input` parameter is the input before the element is parsed.
/// The result of the fold function is used as the new initial value
/// for the next iteration.
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
/// let mut parser = fold_many_m_n(2, 3, tag("let"), || 0, |acc, _, _| acc + 1);
///
/// assert_eq!(parser.parse("letletlet".into()), Ok((3, Input::from(""))));
/// assert_eq!(parser.parse("letlet".into()), Ok((2, Input::from(""))));
/// assert_eq!(parser.parse("letletletlet".into()),
///     Ok((3, Input::from("let"))));
/// assert_eq!(parser.parse("let".into()), Err(ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_EOF,
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
///     code::ERR_EOF,
///     SequenceError::ManyM(2)
/// )));
/// ```
pub fn fold_many_m_n<'a, 'b, F1, F2, F3, O1, O2>(
    m: usize,
    n: usize,
    mut element: F1,
    init: F2,
    fold: F3,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b,
    O2: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Fn() -> O2,
    F3: Fn(O2, Input<'a>, O1) -> O2,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        if n - m == 0 && n == 0 {
            return Ok((init(), input));
        }

        // If m is 0, then many are optional
        let (mut acc, mut cursor) = if m == 0 {
            if let Ok((item, cursor)) = element.parse(input) {
                (fold(init(), input, item), cursor)
            } else {
                return Ok((init(), input));
            }
        } else {
            let (item, cursor) = element.parse(input).map_err(|mut err| {
                err.push(input, code::ERR_MANY_M, SequenceError::ManyM(m));
                err
            })?;
            (fold(init(), input, item), cursor)
        };

        infinite_loop_check!(input, cursor);

        for i in 1..n {
            match element.parse(cursor) {
                Ok((item, next)) => {
                    infinite_loop_check!(cursor, next);
                    acc = fold(acc, cursor, item);
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
    use crate::parser::{prelude::*, Position};

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
        assert_eq!(
            parser.parse("".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_EOF,
                SequenceError::Many1
            ))
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

        assert_eq!(
            parser.parse("let".into()),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                code::ERR_EOF,
                SequenceError::ManyM(2)
            ))
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
        assert_eq!(
            parser.parse("".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_EOF,
                SequenceError::ManyM(2)
            ))
        );
    }
}
