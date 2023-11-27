use regex::Regex;

use crate::parser::{code, Input, ParseError, ParseResult};

use super::CharacterError;

/// Matches a sequence that matches the given regular expression.
///
/// # Examples
///
/// ```rust
/// # use kamo::parser::{
/// #     prelude::*, CharacterError, code, Input, Position
/// # };
/// # use regex::Regex;
/// let regex = Regex::new(r"^[a-z]+").unwrap();
/// let (output, input) = match_re(&regex)(Input::new("hello world")).expect("valid output");
///
/// assert_eq!(output, "hello");
/// assert_eq!(input, Input::from(" world"));
/// assert_eq!(input.position(), Position::new(5, 1, 6));
/// assert_eq!(input.current(), Some(' '));
///
/// let regex = Regex::new(r"^#t(rue)?").unwrap();
/// let error = match_re(&regex)(Input::new("#f")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_REGEX,
///     CharacterError::Regex(regex.to_string())
/// ));
/// ```
pub fn match_re(regex: &Regex) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> + '_ {
    move |input| match regex.find(input.as_str()) {
        Some(m) => {
            if m.is_empty() {
                return Ok(("", input));
            }

            let mut cursor = input;
            let matched = &input.as_str()[..m.end()];

            for _ in matched.chars() {
                cursor.advance();
            }
            Ok((matched, cursor))
        }
        None => Err(ParseError::new(
            input,
            code::ERR_REGEX,
            CharacterError::Regex(regex.to_string()),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Position;

    use super::*;

    #[test]
    fn match_re_success() {
        let regex = Regex::new(r"^[a-z]+").unwrap();
        let (value, input) = match_re(&regex)(Input::new("hello world")).expect("valid output");

        assert_eq!(value, "hello");
        assert_eq!(input, Input::from(" world"));
        assert_eq!(input.position(), Position::new(5, 1, 6));
        assert_eq!(input.current(), Some(' '));

        let regex = Regex::new(r"^#t(rue)?").unwrap();
        let (value, input) = match_re(&regex)(Input::new("#t")).expect("valid output");

        assert_eq!(value, "#t");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.position(), Position::new(2, 1, 3));
        assert_eq!(input.current(), None);

        let (value, input) = match_re(&regex)(Input::new("#true")).expect("valid output");

        assert_eq!(value, "#true");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.position(), Position::new(5, 1, 6));
        assert_eq!(input.current(), None);
    }

    #[test]
    fn match_re_failure() {
        let regex = Regex::new(r"^[a-z]+").unwrap();
        let error = match_re(&regex)(Input::new("123 hello world"));

        assert_eq!(
            error,
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_REGEX,
                CharacterError::Regex(regex.to_string())
            ))
        );

        let regex = Regex::new(r"^#t(rue)?$").unwrap();
        let error = match_re(&regex)(Input::new("#tr"));

        assert_eq!(
            error,
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_REGEX,
                CharacterError::Regex(regex.to_string())
            ))
        );
    }
}
