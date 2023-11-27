use crate::parser::{code, Input, Parser, ParseResult};

use super::CombinatorError;

/// Executes a parser and returns the input that was parsed as a string slice of
/// the original input.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{prelude::*, CombinatorError, code, Input, Position};
/// let mut parser = recognize(delimited(char('('), tag("foo"), char(')')));
/// 
/// assert_eq!(parser.parse("(foo)bar".into()),
///     Ok(("(foo)", Input::from("bar"))));
/// assert_eq!(parser.parse("bar".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_RECOGNIZE,
///     CombinatorError::Recognize
/// )));
/// ```
pub fn recognize<'a, 'b, O, F>(mut f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, &'a str>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| {
        let (_, cursor) = f.parse(input).map_err(|mut err| {
            err.push(input, code::ERR_RECOGNIZE, CombinatorError::Recognize);
            err
        })?;
        let output = &input.as_str()[..(cursor.position().offset() - input.position().offset())];

        Ok((output, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{prelude::*, ParseError, Position};

    #[test]
    fn recognize_success() {
        let (value, input) =
            recognize(delimited(char('('), tag("foo"), char(')')))(Input::new("(foo)bar"))
                .expect("valid input");

        assert_eq!(value, "(foo)");
        assert_eq!(input.position(), Position::new(5, 1, 6));
        assert_eq!(input.current(), Some('b'));

        let (value, input) =
            recognize(delimited(char('('), tag("föö"), char(')')))(Input::new("(föö)bar"))
                .expect("valid input");

        assert_eq!(value, "(föö)");
        assert_eq!(input.position(), Position::new(7, 1, 6));
        assert_eq!(input.current(), Some('b'));
    }

    #[test]
    fn recognize_failure() {
        let error = recognize(delimited(char('('), tag("foo"), char(')')))(Input::new("bar"))
            .expect_err("invalid input");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_RECOGNIZE,
                CombinatorError::Recognize
            )
        );
    }
}
