use crate::parser::{code, Input, ParseResult, Parser};

use super::SequenceError;

macro_rules! list_fold {
    ($cursor:ident, $acc:expr, $element:ident, $separator:ident, $fold:ident) => {{
        let mut cursor = $cursor;
        let mut acc = $acc;

        while let Ok((_, next)) = $separator.parse(cursor) {
            infinite_loop_check!(cursor, next);
            cursor = next;
            match $element.parse(cursor) {
                Ok((item, next)) => {
                    acc = $fold(acc, cursor, item);
                    cursor = next;
                }
                Err(mut err) => {
                    err.push(next, code::ERR_LIST_NEXT, SequenceError::ListNext);
                    return Err(err);
                }
            }
        }
        Ok((acc, cursor))
    }};
}

/// Fold a list of zero or more elements separated by the given separator
/// parser.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
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
/// let mut parser = fold_list0(tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Ok((Vec::new(), Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((vec!["let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let".into()),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("abc,let".into()),
///     Ok((Vec::new(), Input::from("abc,let"))));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     CharacterError::Tag("let")
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_EOF,
///     SequenceError::ListNext
/// )));
/// ```
pub fn fold_list0<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    mut element: F1,
    mut separator: F2,
    init: F3,
    fold: F4,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O3>
where
    O1: 'b,
    O2: 'b,
    O3: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
    F3: Fn() -> O3,
    F4: Fn(O3, Input<'a>, O1) -> O3,
{
    move |input| {
        if let Ok((item, cursor)) = element.parse(input) {
            infinite_loop_check!(input, cursor);
            list_fold!(cursor, fold(init(), input, item), element, separator, fold)
        } else {
            Ok((init(), input))
        }
    }
}

/// Fold a list of one or more elements separated by the given separator
/// parser.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
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
/// let mut parser = fold_list1(tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_EOF,
///     SequenceError::ListStart
/// )));
/// assert_eq!(parser.parse("let".into()), Ok((vec!["let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let".into()),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("abc,let".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
///     SequenceError::ListStart
/// )));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_EOF,
///     SequenceError::ListNext
/// )));
/// ```
pub fn fold_list1<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    mut element: F1,
    mut separator: F2,
    init: F3,
    fold: F4,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O3>
where
    O1: 'b,
    O2: 'b,
    O3: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
    F3: Fn() -> O3,
    F4: Fn(O3, Input<'a>, O1) -> O3,
{
    move |input| {
        let (item, cursor) = element.parse(input).map_err(|mut err| {
            err.push(input, code::ERR_LIST_START, SequenceError::ListStart);
            err
        })?;

        infinite_loop_check!(input, cursor);
        list_fold!(cursor, fold(init(), input, item), element, separator, fold)
    }
}

/// Fold a list of at least `m` elements and at most `n` elements separated by
/// the given separator parser. Returns an empty list if `n - m == 0 && n == 0`.
///
/// When the parser for the separator or the element excepts an empty input and
/// no progress is made, the parser returns an infinte loop error. Sequence
/// parsers must make progress on every successful iteration.
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
/// let mut parser = fold_list_m_n(1, 2, tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_EOF,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let".into()), Ok((vec!["let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let".into()),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let,let".into()),
///     Ok((vec!["let", "let"], Input::from(",let"))));
/// assert_eq!(parser.parse("abc,let".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
///     SequenceError::ListStart
/// )));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_EOF,
///     SequenceError::ListNext
/// )));
/// ```
pub fn fold_list_m_n<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    m: usize,
    n: usize,
    mut element: F1,
    mut separator: F2,
    init: F3,
    fold: F4,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, O3>
where
    O1: 'b,
    O2: 'b,
    O3: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
    F3: Fn() -> O3,
    F4: Fn(O3, Input<'a>, O1) -> O3,
{
    assert!(m <= n, "m must be less than or equal to n");

    move |input| {
        if n - m == 0 && n == 0 {
            return Ok((init(), input));
        }

        // If m is 0, then the list is optional
        let (mut acc, mut cursor) = if m == 0 {
            if let Ok((item, cursor)) = element.parse(input) {
                (fold(init(), input, item), cursor)
            } else {
                return Ok((init(), input));
            }
        } else {
            let (item, cursor) = element.parse(input).map_err(|mut err| {
                err.push(input, code::ERR_LIST_START, SequenceError::ListStart);
                err
            })?;
            (fold(init(), input, item), cursor)
        };

        infinite_loop_check!(input, cursor);

        for i in 1..n {
            match separator.parse(cursor) {
                Ok((_, next)) => {
                    infinite_loop_check!(cursor, next);
                    cursor = next;
                    match element.parse(cursor) {
                        Ok((item, next)) => {
                            acc = fold(acc, cursor, item);
                            cursor = next;
                        }
                        Err(mut err) => {
                            err.push(next, code::ERR_LIST_NEXT, SequenceError::ListNext);
                            return Err(err);
                        }
                    }
                }
                Err(mut err) => {
                    if i < m {
                        err.push(cursor, code::ERR_LIST_M, SequenceError::ListM(m));
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
    fn fold_list0_success() {
        let mut parser = fold_list0(tag("let"), char(','), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(parser.parse("".into()), Ok((Vec::new(), Input::from(""))));
        assert_eq!(
            parser.parse("let".into()),
            Ok((vec!["let"], Input::from("")))
        );
        assert_eq!(
            parser.parse("let,let".into()),
            Ok((vec!["let", "let"], Input::from("")))
        );
        assert_eq!(
            parser.parse("abc,let".into()),
            Ok((Vec::new(), Input::from("abc,let")))
        );
    }

    #[test]
    fn fold_list0_infinite_loop() {
        let mut parser = fold_list0(tag("let"), opt(char(',')), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(
            parser.parse("let,letlet".into()),
            Err(ParseError::new(
                Position::new(7, 1, 8),
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
    fn fold_list0_failure() {
        let mut parser = fold_list0(tag("let"), char(','), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(
            parser.parse("let,abc".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            ))
        );
        assert_eq!(
            parser.parse("let,".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_EOF,
                SequenceError::ListNext
            ))
        );
    }

    #[test]
    fn fold_list1_success() {
        let mut parser = fold_list1(tag("let"), char(','), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(
            parser.parse("let".into()),
            Ok((vec!["let"], Input::from("")))
        );
        assert_eq!(
            parser.parse("let,let".into()),
            Ok((vec!["let", "let"], Input::from("")))
        );
    }

    #[test]
    fn fold_list1_infinite_loop() {
        let mut parser = fold_list1(tag("let"), opt(char(',')), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(
            parser.parse("let,letlet".into()),
            Err(ParseError::new(
                Position::new(7, 1, 8),
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
    fn fold_list1_failure() {
        let mut parser = fold_list1(tag("let"), char(','), Vec::new, |mut acc, _, item| {
            acc.push(item);
            acc
        });

        assert_eq!(
            parser.parse("".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_EOF,
                SequenceError::ListStart
            ))
        );
        assert_eq!(
            parser.parse("abc,let".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LIST_START,
                SequenceError::ListStart
            ))
        );
        assert_eq!(
            parser.parse("let,abc".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            ))
        );
        assert_eq!(
            parser.parse("let,".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_EOF,
                SequenceError::ListNext
            ))
        );
    }

    #[test]
    fn fold_list_m_n_success() {
        let mut parser =
            fold_list_m_n(1, 2, tag("let"), char(','), Vec::new, |mut acc, _, item| {
                acc.push(item);
                acc
            });

        assert_eq!(
            parser.parse("let".into()),
            Ok((vec!["let"], Input::from("")))
        );
        assert_eq!(
            parser.parse("let,let".into()),
            Ok((vec!["let", "let"], Input::from("")))
        );
        assert_eq!(
            parser.parse("let,let,let".into()),
            Ok((vec!["let", "let"], Input::from(",let")))
        );
    }

    #[test]
    fn fold_list_m_n_infinite_loop() {
        let mut parser = fold_list_m_n(
            2,
            3,
            tag("let"),
            opt(char(',')),
            Vec::new,
            |mut acc, _, item| {
                acc.push(item);
                acc
            },
        );

        assert_eq!(
            parser.parse("let,letlet".into()),
            Err(ParseError::new(
                Position::new(7, 1, 8),
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
    fn fold_list_m_n_failure() {
        let mut parser =
            fold_list_m_n(1, 2, tag("let"), char(','), Vec::new, |mut acc, _, item| {
                acc.push(item);
                acc
            });

        assert_eq!(
            parser.parse("".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_EOF,
                SequenceError::ListM(1)
            ))
        );
        assert_eq!(
            parser.parse("abc,let".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LIST_START,
                SequenceError::ListStart
            ))
        );
        assert_eq!(
            parser.parse("let,abc".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            ))
        );
        assert_eq!(
            parser.parse("let,".into()),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                code::ERR_EOF,
                SequenceError::ListNext
            ))
        );
    }
}
