use std::fmt;

use crate::parser::{Input, Parser, ParseResult};

/// Maps the output of a parser to another value.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, code, CharacterError, Input}};
/// let mut parser = map(char('a'), |c| c as u8);
/// 
/// assert_eq!(parser.parse("abc".into()), Ok((97, Input::from("bc"))));
/// assert_eq!(parser.parse("bc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('a')
/// )));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('a'),
/// ));
/// ```
pub fn map<'a, 'b, O1, O2, F, G>(mut f: F, g: G) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b + fmt::Debug,
    O2: 'b,
    F: Parser<'a, 'b, O1>,
    G: Fn(O1) -> O2,
{
    move |input| {
        let (value, cursor) = f.parse(input)?;

        Ok((g(value), cursor))
    }
}

/// Maps the output of a parser to another value. The mapping function takes the
/// output of the parser and the input.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, code, CharacterError, Input}};
/// let mut parser = map2(char('a'), |c, input| (c as u8, input));
/// 
/// assert_eq!(parser.parse("abc".into()),
///     Ok(((97, Input::from("abc")), Input::from("bc"))));
/// assert_eq!(parser.parse("bc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('a')
/// )));
/// 
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('a'),
/// ));
/// ```
pub fn map2<'a, 'b, O1, O2, F, G>(mut f: F, g: G) -> impl FnMut(Input<'a>) -> ParseResult<'a, O2>
where
    O1: 'b + fmt::Debug,
    O2: 'b,
    F: Parser<'a, 'b, O1>,
    G: FnMut(O1, Input<'a>) -> O2,
{
    let mut g = g;
    
    move |input| {
        let (value, cursor) = f.parse(input)?;

        Ok((g(value, input), cursor))
    }
}

