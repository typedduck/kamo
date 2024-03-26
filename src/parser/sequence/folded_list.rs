use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::SequenceError;

/// Fold a list of zero or at most `n` elements separated by the given separator
/// parser.
///
/// When the parser for the element or separator excepts an empty input and no
/// progress is made, the parser returns an infinte loop error. List parsers
/// must make progress on every successful iteration.
///
/// The iteration stops when the parser for the separatot fails or `n` elements
/// have been parsed. If the parser for the separator fails, the parser returns
/// the current value and the input before the separator was parsed. When
/// parsing an element fails and the error is semantic, the parser does not
/// append an [`ERR_LIST_NEXT`](code::ERR_LIST_NEXT) error to the error stack.
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = fold_list_n(3, tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Ok((Vec::new(), Input::from(""))));
/// assert_eq!(parser.parse("let".into()), Ok((vec!["let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let".into()),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let,let".into()),
///     Ok((vec!["let", "let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let,let,let".into()),
///     Ok((vec!["let", "let", "let"], Input::from(",let"))));
/// assert_eq!(parser.parse("abc,let".into()),
///     Ok((Vec::new(), Input::from("abc,let"))));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     CharacterError::Tag("let")
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
#[inline]
pub fn fold_list_n<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    n: usize,
    item: F1,
    separator: F2,
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
    F4: FnMut(O3, Input<'a>, O1) -> O3,
{
    let mut value = counting_fold_list_n(n, item, separator, init, fold);

    move |input| value(input).map(|((_, acc), input)| (acc, input))
}

/// Fold a list of zero or more elements separated by the given separator
/// parser.
///
/// Calls [`fold_list_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
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
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
#[inline]
pub fn fold_list0<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    item: F1,
    separator: F2,
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
    F4: FnMut(O3, Input<'a>, O1) -> O3,
{
    fold_list_n(usize::MAX, item, separator, init, fold)
}

/// Fold a list of one or more elements separated by the given separator
/// parser.
///
/// Calls [`fold_list_n()`], `n` is set to `usize::MAX`.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = fold_list1(tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_START,
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
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
pub fn fold_list1<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    item: F1,
    separator: F2,
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
    F4: FnMut(O3, Input<'a>, O1) -> O3,
{
    let mut value = counting_fold_list_n(usize::MAX, item, separator, init, fold);

    move |input| {
        let ((count, value), cursor) = value.parse(input)?;

        if count == 0 {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_LIST_START,
                SequenceError::ListStart,
            ));
        }
        Ok((value, cursor))
    }
}

/// Fold a list of at least `m` elements and at most `n` elements separated by
/// the given separator parser.
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
/// let mut parser = fold_list_m_n(1, 2, tag("let"), char(','),
///     Vec::new,
///     |mut acc, _, item| { acc.push(item); acc }
/// );
///
/// assert_eq!(parser.parse("".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_M,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let".into()), Ok((vec!["let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let".into()),
///     Ok((vec!["let", "let"], Input::from(""))));
/// assert_eq!(parser.parse("let,let,let".into()),
///     Ok((vec!["let", "let"], Input::from(",let"))));
/// assert_eq!(parser.parse("abc,let".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LIST_M,
///     SequenceError::ListM(1)
/// )));
/// assert_eq!(parser.parse("let,abc".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// assert_eq!(parser.parse("let,".into()), Err(ParseError::new(
///     Position::new(4, 1, 5),
///     code::ERR_LIST_NEXT,
///     SequenceError::ListNext
/// )));
/// ```
pub fn fold_list_m_n<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    m: usize,
    n: usize,
    item: F1,
    separator: F2,
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
    F4: FnMut(O3, Input<'a>, O1) -> O3,
{
    assert!(m <= n, "m must be less than or equal to n");

    let mut value = counting_fold_list_n(n, item, separator, init, fold);

    move |input| {
        let ((count, value), cursor) = value.parse(input)?;

        if count < m {
            return Err(ParseError::new_at(
                cursor,
                code::ERR_LIST_M,
                SequenceError::ListM(m),
            ));
        }
        Ok((value, cursor))
    }
}

fn counting_fold_list_n<'a, 'b, F1, F2, F3, F4, O1, O2, O3>(
    n: usize,
    item: F1,
    separator: F2,
    init: F3,
    fold: F4,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, (usize, O3)>
where
    O1: 'b,
    O2: 'b,
    O3: 'b,
    F1: Parser<'a, 'b, O1>,
    F2: Parser<'a, 'b, O2>,
    F3: Fn() -> O3,
    F4: FnMut(O3, Input<'a>, O1) -> O3,
{
    let mut item = item;
    let mut fold = fold;
    let mut separator = separator;

    move |input| {
        if let Ok((value, cursor)) = item.parse(input) {
            let mut count = 1;
            let mut cursor = cursor;
            let mut acc = fold(init(), input, value);

            infinite_loop_check!(input, cursor);
            for _ in 1..n {
                match separator.parse(cursor) {
                    Ok((_, next)) => {
                        infinite_loop_check!(cursor, next);
                        cursor = next;
                        match item.parse(cursor) {
                            Ok((value, next)) => {
                                infinite_loop_check!(cursor, next);
                                acc = fold(acc, cursor, value);
                                cursor = next;
                                count += 1;
                            }
                            Err(mut err) => {
                                if !err.is_semantic() {
                                    err.push(next, code::ERR_LIST_NEXT, SequenceError::ListNext);
                                    return Err(err);
                                }
                                return Err(err);
                            }
                        }
                    }
                    Err(_) => {
                        return Ok(((count, acc), cursor));
                    }
                }
            }
            Ok(((count, acc), cursor))
        } else {
            Ok(((0, init()), input))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::prelude::*, Position};

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

        let error = parser.parse("let,".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(4, 1, 5)).and(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
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

        let error = parser.parse("".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_LIST_START,
                SequenceError::ListStart
            )
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

        let error = parser.parse("let,".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(4, 1, 5)).and(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
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

        let error = parser.parse("".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_LIST_M,
                SequenceError::ListM(1)
            )
        );

        assert_eq!(
            parser.parse("abc,let".into()),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_LIST_M,
                SequenceError::ListM(1)
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

        let error = parser.parse("let,".into()).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(4, 1, 5)).and(
                Position::new(4, 1, 5),
                code::ERR_LIST_NEXT,
                SequenceError::ListNext
            )
        );
    }
}
