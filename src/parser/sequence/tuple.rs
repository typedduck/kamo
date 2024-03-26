use crate::parser::{code, Input, ParseResult, Parser};

use super::SequenceError;

/// The `Tuple` trait is for parsing a sequence of parsers.
///
/// The `Tuple` trait is implemented by tuples of parsers. It is used to parse
/// a sequence of multiple parsers and is used to implement the [`tuple()`]
/// parser combinator. It is implemented for tuples of parsers up to a length of
/// 16. Every parser can have a different output type.
pub trait Tuple<'a, 'b, O>
where
    O: 'b,
{
    /// The `parse_tuple` method is used to parse a sequence of multiple parsers
    /// and returns a tuple of the results of the parsers.
    fn parse_tuple(&mut self, input: Input<'a>) -> ParseResult<'a, O>;

    /// The `tuple_len` method returns the number of parsers in the tuple.
    fn tuple_len(&self) -> usize;
}

/// The `Tuple` trait is implemented for the empty tuple.
impl<'a, 'b> Tuple<'a, 'b, ()> for () {
    fn parse_tuple(&mut self, input: Input<'a>) -> ParseResult<'a, ()> {
        Ok(((), input))
    }

    fn tuple_len(&self) -> usize {
        0
    }
}

macro_rules! tuple_first {
    ($self:ident, $input:ident) => {
        ($self.0).parse($input).map_err(|mut err| {
            err.push(
                $input,
                code::ERR_TUPLE,
                SequenceError::Tuple(0, $self.tuple_len()),
            );
            err
        })?
    };
}

/// The `Tuple` trait is implemented for tuples of parsers up to a length of 16.
impl<'a, 'b, O, F> Tuple<'a, 'b, (O,)> for (F,)
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    fn parse_tuple(&mut self, input: Input<'a>) -> ParseResult<'a, (O,)> {
        let (out, cursor) = tuple_first!(self, input);

        Ok(((out,), cursor))
    }

    fn tuple_len(&self) -> usize {
        1
    }
}

macro_rules! tuple_next {
    ($self:ident, $input:ident, $idx:tt) => {
        match ($self.$idx).parse($input) {
            Ok(out) => out,
            Err(mut err) => {
                err.push(
                    $input,
                    code::ERR_TUPLE,
                    SequenceError::Tuple($idx, $self.tuple_len()),
                );
                return Err(err);
            }
        }
    };
}

macro_rules! tuple_count {
    ($($idx:tt),+) => {
        1 $(+ tuple_count!(@count $idx))*
    };
    (@count $idx:tt) => {
        1
    };
}

macro_rules! tuple_impl {
    ($($idx:tt: $out:ident, $O:tt, $F:tt);+) => {
        impl<'a, 'b, O1, $($O),+, F1, $($F),+> Tuple<'a, 'b, (O1, $($O),+)> for (F1, $($F),+)
        where
            O1: 'b,
            $($O: 'b),+,
            F1: Parser<'a, 'b, O1>,
            $($F: Parser<'a, 'b, $O>),+,
        {
            fn parse_tuple(&mut self, input: Input<'a>) -> ParseResult<'a, (O1, $($O),+)> {
                let (out1, cursor) = tuple_first!(self, input);

                $(
                    let ($out, cursor) = tuple_next!(self, cursor, $idx);
                )+

                Ok(((out1, $($out),+), cursor))
            }

            fn tuple_len(&self) -> usize {
                tuple_count!($($idx),+)
            }
        }
    };
}

tuple_impl!(1: out2, O2, F2);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11;
    11: out12, O12, F12);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11;
    11: out12, O12, F12; 12: out13, O13, F13);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11;
    11: out12, O12, F12; 12: out13, O13, F13; 13: out14, O14, F14);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11;
    11: out12, O12, F12; 12: out13, O13, F13; 13: out14, O14, F14; 14: out15, O15, F15);
tuple_impl!(1: out2, O2, F2; 2: out3, O3, F3; 3: out4, O4, F4; 4: out5, O5, F5; 5: out6, O6, F6;
    6: out7, O7, F7; 7: out8, O8, F8; 8: out9, O9, F9; 9: out10, O10, F10; 10: out11, O11, F11;
    11: out12, O12, F12; 12: out13, O13, F13; 13: out14, O14, F14; 14: out15, O15, F15;
    15: out16, O16, F16);

/// The `tuple` parser combinator parses a sequence of multiple parsers and
/// returns a tuple of the results of the parsers.
///
/// It is implemented using the [`Tuple`] trait. It is implemented for tuples of
/// parsers up to a length of 16. Every parser can have a different output type.
/// If the tuple is empty, the parser will return `()`. The parser fails if any
/// of the parsers fail.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{
/// #     prelude::*, CharacterError, SequenceError, code, Input, Span
/// # }};
/// let mut parser = tuple((char('a'), char('b')));
///
/// assert_eq!(parser(Input::new("ab")), Ok((('a', 'b'), Input::new(""))));
/// assert_eq!(parser(Input::new("ba")), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TUPLE,
///     SequenceError::Tuple(0, 2)
/// )));
/// assert_eq!(parser(Input::new("aa")), Err(ParseError::new(
///     Position::new(1, 1, 2),
///     code::ERR_TUPLE,
///     SequenceError::Tuple(1, 2)
/// )));
/// 
/// let error = parser(Input::new("a")).expect_err("error output");
/// 
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///    Position::new(1, 1, 2),
///    code::ERR_TUPLE,
///    SequenceError::Tuple(2, 2)
/// ));
/// 
/// let error = parser(Input::new("")).expect_err("error output");
/// 
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TUPLE,
///     SequenceError::Tuple(1, 2)
/// ));
/// ```
pub fn tuple<'a, 'b, O, T>(mut tuple: T) -> impl FnMut(Input<'a>) -> ParseResult<'a, O>
where
    O: 'b,
    T: Tuple<'a, 'b, O>,
{
    move |input| tuple.parse_tuple(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        parser::{prelude::*, ParseError},
        Position,
    };

    #[test]
    fn tuple_0() {
        let (_, input) = tuple(())(Input::new("abc")).expect("valid output");

        assert_eq!(input, Input::from("abc"));
        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));

        let (_, input) = tuple(())(Input::new("")).expect("valid output");

        assert_eq!(input, Input::from(""));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(0, 1, 1));
    }

    #[test]
    fn tuple_1() {
        let (_, input) = tuple((char('a'),))(Input::new("abc")).expect("valid output");

        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));

        let error = tuple((char('a'),))(Input::new("bac")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_TUPLE,
                SequenceError::Tuple(0, 1)
            )
        );

        let error = tuple((char('a'),))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_TUPLE,
                SequenceError::Tuple(0, 1)
            )
        );
    }

    #[test]
    fn tuple_2() {
        let (_, input) = tuple((char('a'), char('b')))(Input::new("abc")).expect("valid output");

        assert_eq!(input, Input::from("c"));
        assert_eq!(input.current(), Some('c'));
        assert_eq!(input.position(), Position::new(2, 1, 3));

        let error = tuple((char('a'), char('b')))(Input::new("bac")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_TUPLE,
                SequenceError::Tuple(0, 2)
            )
        );

        let error = tuple((char('a'), char('b')))(Input::new("acb")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_TUPLE,
                SequenceError::Tuple(1, 2)
            )
        );

        let error = tuple((char('a'), char('b')))(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_TUPLE,
                SequenceError::Tuple(1, 2)
            )
        );

        let error = tuple((char('a'), char('b')))(Input::new("a")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(1, 1, 2)).and(
                Position::new(1, 1, 2),
                code::ERR_TUPLE,
                SequenceError::Tuple(1, 2)
            )
        );
    }
}
