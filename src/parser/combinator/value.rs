use std::fmt;

use crate::parser::{Input, Parser, ParseResult};

/// Coerces the output of a parser to another value.
/// 
/// # Examples
/// 
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, code, CharacterError, Input}};
/// let mut parser = value('a', char('b'));
/// 
/// assert_eq!(parser.parse("bac".into()), Ok(('a', Input::from("ac"))));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_CHAR,
///     CharacterError::Char('b')
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
pub fn value<'a, 'b, O1, O2, F>(value: O1, mut f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, O1>
where
    O1: 'b + fmt::Debug + Clone,
    O2: 'b,
    F: Parser<'a, 'b, O2>,
{
    move |input: Input<'a>| {
        let (_, cursor) = f.parse(input)?;

        Ok((value.to_owned(), cursor))
    }
}
