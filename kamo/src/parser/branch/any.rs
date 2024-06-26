use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::BranchError;

/// The `Any` trait is for parsing a choice of multiple parsers.
///
/// The `Any` trait is implemented by tuples of parsers. It is used to parse a
/// choice of multiple parsers. It is used to implement the [`any()`] parser
/// combinator. It is implemented for tuples of parsers up to a length of 16.
/// The parsers are tried in order. If a parser fails, the next parser is tried.
///
/// If all parsers fail, an error is returned indicating that no parser matched.
/// If one of the parsers hits the end of the input, and end of file cause is
/// appended to the error.
///
/// If a parser fails with a semantic error, the error of that element is
/// returned.
///
/// The output-type of all parsers must be the same.
pub trait Any<'a, 'b, O>
where
    O: 'b,
{
    /// The `parse_any` method is used to parse a choice of multiple parsers
    /// and returns the result of the first parser which succeeds.
    ///
    /// # Errors
    ///
    /// If all parsers fail, an error is returned indicating that no parser
    /// matched. If one of the parsers hits the end of the input, and end of
    /// file cause is appended to the error.
    ///
    /// If a parser fails or flags a semantic error, the error of that element
    /// is returned.
    fn parse_any(&mut self, input: Input<'a>) -> ParseResult<'a, O>;
}

impl<'a, 'b> Any<'a, 'b, ()> for () {
    /// The `parse_any` method of the `Any` trait is implemented for the
    /// empty tuple.
    fn parse_any(&mut self, input: Input<'a>) -> ParseResult<'a, ()> {
        Ok(((), input))
    }
}

macro_rules! any_branch {
    ($self:ident, $input:ident, $state:ident, $idx:tt) => {
        match ($self.$idx).parse($input) {
            Ok(out) => {
                return Ok(out);
            }
            Err(err) => {
                if err.is_semantic() || err.is_failure() {
                    return Err(err);
                }
                err.is_eof()
            }
        }
    };
}

macro_rules! any_impl {
    ($($idx:tt: $F:tt);+ => $O:tt) => {
        impl<'a, 'b, $O, $($F),+> Any<'a, 'b, $O> for ($($F),+,)
        where
            $O: 'b,
            $($F: Parser<'a, 'b, $O> + 'b),+,
        {
            fn parse_any(&mut self, input: Input<'a>) -> ParseResult<'a, $O> {
                let mut hit_eof = false;
                $(
                    let branch_hit_eof = any_branch!(self, input, state, $idx);
                    hit_eof = hit_eof || branch_hit_eof;
                )+


                if hit_eof || input.is_eof() {
                    return Err(ParseError::eof(input)
                        .and(input, code::ERR_ANY, BranchError::NoMatch))
                }
                Err(ParseError::new(input, code::ERR_ANY, BranchError::NoMatch))
            }
        }
    };
}

any_impl!(0: F1 => O);
any_impl!(0: F1; 1: F2 => O);
any_impl!(0: F1; 1: F2; 2: F3 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8;
    8: F9 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11; 11: F12 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11; 11: F12; 12: F13 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11; 11: F12; 12: F13; 13: F14 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11; 11: F12; 12: F13; 13: F14; 14: F15 => O);
any_impl!(0: F1; 1: F2; 2: F3; 3: F4; 4: F5; 5: F6; 6: F7; 7: F8; 8: F9;
    9: F10; 10: F11; 11: F12; 12: F13; 13: F14; 14: F15; 15: F16 => O);

/// The `any` parser combinator is used to parse a choice of multiple parsers.
///
/// It is implemented using the `Any` trait. It is implemented for tuples of
/// parsers up to a length of 16. The parsers are tried in order. If a parser
/// fails, the next parser is tried.
///
/// If all parsers fail, an error is returned indicating that no parser matched.
/// If one of the parsers hits the end of the input, and end of file cause is
/// appended to the error.
///
/// If a parser fails with a semantic error or a failure, the error of that
/// element is returned.
///
/// The output-type of all parsers must be the same.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, BranchError, code, Input}};
/// let mut parser = any((char('a'), char('b')));
///
/// assert_eq!(parser.parse("abc".into()), Ok(('a', Input::from("bc"))));
/// assert_eq!(parser.parse("bac".into()), Ok(('b', Input::from("ac"))));
/// assert_eq!(parser.parse("cab".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ANY,
///     BranchError::NoMatch
/// )));
/// ```
pub fn any<'a, 'b, O, L>(mut any: L) -> impl FnMut(Input<'a>) -> ParseResult<'a, O>
where
    O: 'b,
    L: Any<'a, 'b, O>,
{
    move |input| any.parse_any(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{parser::prelude::*, Position};

    #[test]
    fn any_0() {
        let ((), input) = any(())(Input::new("abc")).expect("valid output");

        assert_eq!(input, Input::from("abc"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let ((), input) = any(())(Input::new("")).expect("valid output");

        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(0, 1, 1));
    }

    #[test]
    fn any_1() {
        let (output, input) = any((char('a'),))(Input::new("abc")).expect("valid output");

        assert_eq!(output, 'a');
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let error = any((char('a'),))(Input::new("bac")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(Position::new(0, 1, 1), code::ERR_ANY, BranchError::NoMatch)
        );

        let error = any((char('a'),))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_ANY,
                BranchError::NoMatch
            )
        );
    }

    #[test]
    fn any_2() {
        let (output, input) = any((char('a'), char('b')))(Input::new("abc")).expect("valid output");

        assert_eq!(output, 'a');
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let (output, input) = any((char('a'), char('b')))(Input::new("bac")).expect("valid output");

        assert_eq!(output, 'b');
        assert_eq!(input, Input::from("ac"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let error = any((char('a'), char('b')))(Input::new("cab")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(Position::new(0, 1, 1), code::ERR_ANY, BranchError::NoMatch)
        );

        let error = any((char('a'), char('b')))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_ANY,
                BranchError::NoMatch
            )
        );
    }
}
