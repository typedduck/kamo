use crate::parser::{Input, Parser, ParseResult};

/// Makes a parser optional.
/// 
/// # Examples
/// 
/// ```rust
/// # use drake::parser::{prelude::*, Input};
/// let mut parser = opt(char('a'));
/// 
/// assert_eq!(parser.parse("abc".into()), Ok((Some('a'), Input::from("bc"))));
/// assert_eq!(parser.parse("bc".into()), Ok((None, Input::from("bc"))));
/// assert_eq!(parser.parse("".into()), Ok((None, Input::from(""))));
/// ```
pub fn opt<'a, 'b, O, F>(mut f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Option<O>>
where
    O: 'b,
    F: Parser<'a, 'b, O>,
{
    move |input| match f.parse(input) {
        Ok((value, input)) => Ok((Some(value), input)),
        Err(_) => Ok((None, input)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{prelude::*, Position};

    #[test]
    fn opt_none() {
        let (output, input) = opt(tag("def"))(Input::new("abc")).expect("valid output");

        assert_eq!(output, None);
        assert_eq!(input, Input::from("abc"));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert_eq!(input.current(), Some('a'));

        let (output, input) = opt(tag("def"))(Input::new("")).expect("valid output");

        assert_eq!(output, None);
        assert_eq!(input, Input::from(""));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert_eq!(input.current(), None);
    }

    #[test]
    fn opt_some() {
        let (output, input) = opt(tag("def"))(Input::new("defabc")).expect("valid output");

        assert_eq!(output, Some("def"));
        assert_eq!(input, Input::from("abc"));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('a'));
    }
}
