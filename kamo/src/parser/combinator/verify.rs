use std::borrow::Borrow;

use crate::parser::{code, Input, ParseError, ParseResult, Parser};

use super::CombinatorError;

/// Verifies the output of a parser. If verification fails, the returned error
/// is a semantic error.
///
/// # Examples
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, code, CombinatorError, CharacterError, Input}};
/// let digit = literal::Radix::Decimal.one_digit();
/// let mut parser = verify(digit, |d| *d == 0);
///
/// assert_eq!(parser.parse("0".into()), Ok((0, Input::from(""))));
/// assert_eq!(parser.parse("1".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_VERIFY,
///     CombinatorError::Verify
/// )));
/// assert_eq!(parser.parse("a".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_DIGIT,
///     CharacterError::Digit
/// )));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_DIGIT,
///     CharacterError::Digit,
/// ));
/// ```
pub fn verify<'a, 'b, O1, O2, F, G>(mut f: F, g: G) -> impl FnMut(Input<'a>) -> ParseResult<'a, O1>
where
    O1: 'b + Borrow<O2>,
    O2: 'b + ?Sized,
    F: Parser<'a, 'b, O1>,
    G: Fn(&O2) -> bool,
{
    move |input: Input<'a>| {
        let (value, cursor) = f.parse(input)?;

        if !g(value.borrow()) {
            return Err(
                ParseError::new(input, code::ERR_VERIFY, CombinatorError::Verify).and_semantic(),
            );
        }
        Ok((value, cursor))
    }
}
