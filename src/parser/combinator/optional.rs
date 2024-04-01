use crate::parser::{Input, ParseResult, Parser};

/// Makes a parser optional.
/// 
/// If the parser succeeds, the output is wrapped in `Some`. If the parser fails
/// with a semantic or failure error, the error is returned. Otherwise, the
/// output is `None`.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{prelude::*, Input};
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
        Err(err) => {
            if err.is_semantic() || err.is_failure() {
                Err(err)
            } else {
                Ok((None, input))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{parser::prelude::*, Position};

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
