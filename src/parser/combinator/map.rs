use std::fmt;

use crate::parser::{Input, Parser, ParseResult};

/// Maps the output of a parser to another value.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, code, CharacterError, Input, Position
/// # };
/// let mut parser = map(char('a'), |c| c as u8);
/// 
/// assert_eq!(parser.parse("abc".into()), Ok((97, Input::from("bc"))));
/// assert_eq!(parser.parse("bc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('a')
/// )));
/// assert_eq!(parser.parse("".into()),
///     Err(ParseError::eof(Position::new(0, 1, 1))));
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
