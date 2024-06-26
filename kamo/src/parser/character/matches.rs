use crate::parser::{code, Input, ParseError, ParseResult, Span};

use super::CharacterError;

/// Matches a character that satisfies the predicate.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = satisfy(|ch| ch.is_ascii_alphabetic());
///
/// assert_eq!(parser.parse("abc".into()), Ok(('a', "bc".into())));
/// assert_eq!(parser.parse("123".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_SATISFY,
///     CharacterError::Satisfy
/// )));
/// ```
pub fn satisfy<F>(predicate: F) -> impl Fn(Input) -> ParseResult<char> + Copy
where
    F: Fn(char) -> bool + Copy,
{
    move |input| {
        input.current().map_or_else(
            || Err(ParseError::eof(input).and(input, code::ERR_SATISFY, CharacterError::Satisfy)),
            |ch| {
                if predicate(ch) {
                    let mut cursor = input;

                    cursor.advance();
                    Ok((ch, cursor))
                } else {
                    Err(ParseError::new(
                        input,
                        code::ERR_SATISFY,
                        CharacterError::Satisfy,
                    ))
                }
            },
        )
    }
}

/// Matches any character.
///
/// # Errors
///
/// If the input is empty, it returns a [`ParseError`] with the code
/// [`ERR_ANY_CHAR`](code::ERR_ANY_CHAR) and the error variant
/// [`CharacterError::AnyChar`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = any_char;
///
/// assert_eq!(parser.parse("abc".into()), Ok(('a', "bc".into())));
///
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ANY_CHAR,
///     CharacterError::AnyChar,
/// ));
/// ```
pub fn any_char(input: Input<'_>) -> ParseResult<char> {
    input.current().map_or_else(
        || Err(ParseError::eof(input).and(input, code::ERR_ANY_CHAR, CharacterError::AnyChar)),
        |ch| {
            let mut cursor = input;

            cursor.advance();
            Ok((ch, cursor))
        },
    )
}

/// Matches the given character.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = char('a');
///
/// assert_eq!(parser.parse("abc".into()), Ok(('a', "bc".into())));
/// assert_eq!(parser.parse("bcd".into()), Err(ParseError::new(
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
pub const fn char(value: char) -> impl Fn(Input) -> ParseResult<char> + Copy {
    move |mut input| match input.advance_if(|ch| ch == value) {
        Some(true) => Ok((value, input)),
        Some(false) => Err(ParseError::new(
            input,
            code::ERR_CHAR,
            CharacterError::Char(value),
        )),
        None => Err(ParseError::eof(input).and(input, code::ERR_CHAR, CharacterError::Char(value))),
    }
}

/// Matches the given string.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = tag("abc");
///
/// assert_eq!(parser.parse("abc".into()), Ok(("abc", "".into())));
/// assert_eq!(parser.parse("abcd".into()), Ok(("abc", "d".into())));
/// assert_eq!(parser.parse("dabc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_TAG,
///     CharacterError::Tag("abc")
/// )));
///
/// let error = parser.parse("ab".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(2, 1, 3),
///     code::ERR_TAG,
///     CharacterError::Tag("abc"),
/// ));
/// ```
pub fn tag(value: &'static str) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> {
    move |input| {
        let mut cursor = input;

        for ch in value.chars() {
            if let Some(current) = cursor.current() {
                if ch == current {
                    cursor.advance();
                } else {
                    return Err(ParseError::new(
                        Span::new(input.position(), cursor.position()),
                        code::ERR_TAG,
                        CharacterError::Tag(value),
                    ));
                }
            } else {
                return Err(ParseError::eof(input).and(
                    cursor,
                    code::ERR_TAG,
                    CharacterError::Tag(value),
                ));
            }
        }
        Ok((
            &input.as_str()[..(cursor.position().offset() - input.position().offset())],
            cursor,
        ))
    }
}

/// Matches any character except the ones specified.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = none_of("abc");
///
/// assert_eq!(parser.parse("def".into()), Ok(('d', "ef".into())));
/// assert_eq!(parser.parse("abc".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_NONE_OF,
///     CharacterError::NoneOf("abc")
/// )));
///
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_NONE_OF,
///     CharacterError::NoneOf("abc"),
/// ));
/// ```
pub fn none_of(values: &'static str) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, char> {
    move |input| {
        input.current().map_or_else(
            || {
                Err(ParseError::eof(input).and(
                    input,
                    code::ERR_NONE_OF,
                    CharacterError::NoneOf(values),
                ))
            },
            |ch| {
                if values.chars().any(|value| value == ch) {
                    Err(ParseError::new(
                        input,
                        code::ERR_NONE_OF,
                        CharacterError::NoneOf(values),
                    ))
                } else {
                    let mut cursor = input;

                    cursor.advance();
                    Ok((ch, cursor))
                }
            },
        )
    }
}

/// Matches any character of the ones specified.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = one_of("abc");
///
/// assert_eq!(parser.parse("abc".into()), Ok(('a', "bc".into())));
/// assert_eq!(parser.parse("def".into()), Err(ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ONE_OF,
///     CharacterError::OneOf("abc")
/// )));
///
/// let error = parser.parse("".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ONE_OF,
///     CharacterError::OneOf("abc"),
/// ));
/// ```
pub fn one_of(values: &'static str) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, char> {
    move |input| {
        input.current().map_or_else(
            || {
                Err(ParseError::eof(input).and(
                    input.position(),
                    code::ERR_ONE_OF,
                    CharacterError::OneOf(values),
                ))
            },
            |ch| {
                if values.chars().any(|value| value == ch) {
                    let mut cursor = input;

                    cursor.advance();
                    Ok((ch, cursor))
                } else {
                    Err(ParseError::new(
                        input.position(),
                        code::ERR_ONE_OF,
                        CharacterError::OneOf(values),
                    ))
                }
            },
        )
    }
}

/// Matches any character until one of the specified characters is found.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::*, CharacterError, code}};
/// let mut parser = is_not("abc");
///
/// assert_eq!(parser.parse("defa".into()), Ok(("def", "a".into())));
/// assert_eq!(parser.parse("defb".into()), Ok(("def", "b".into())));
/// assert_eq!(parser.parse("defc".into()), Ok(("def", "c".into())));
///
/// let error = parser.parse("def".into()).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(3, 1, 4),
///     code::ERR_IS_NOT,
///     CharacterError::IsNot("abc"),
/// ));
/// ```
pub fn is_not(value: &'static str) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> {
    move |input| {
        let mut cursor = input;

        while let Some(ch) = cursor.current() {
            if value.chars().any(|v| v == ch) {
                break;
            }
            cursor.advance();
        }

        if cursor.is_eof() {
            return Err(ParseError::eof(input).and(
                cursor,
                code::ERR_IS_NOT,
                CharacterError::IsNot(value),
            ));
        }

        let output = &input.as_str()[..(cursor.position().offset() - input.position().offset())];

        Ok((output, cursor))
    }
}

#[cfg(test)]
mod tests {
    use crate::Position;

    use super::*;

    #[test]
    fn anychar_success() {
        let (ch, input) = any_char(Input::new("abc")).expect("valid output");
        assert_eq!(ch, 'a');
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert_eq!(input.current(), Some('b'));
    }

    #[test]
    fn anychar_failure() {
        let error = any_char(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_ANY_CHAR,
                CharacterError::AnyChar
            )
        );
    }

    #[test]
    fn char_success() {
        let (ch, input) = char('a')(Input::new("abc")).expect("valid output");
        assert_eq!(ch, 'a');
        assert_eq!(input, Input::from("bc"));
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert_eq!(input.current(), Some('b'));
    }

    #[test]
    fn char_failure() {
        let error = char('b')(Input::new("abc")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_CHAR,
                CharacterError::Char('b')
            )
        );

        let error = char('a')(Input::new("")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(0, 1, 1)).and(
                Position::new(0, 1, 1),
                code::ERR_CHAR,
                CharacterError::Char('b')
            )
        );
    }

    #[test]
    fn tag_success() {
        let (value, input) = tag("abc")(Input::new("abc")).expect("valid output");

        assert_eq!(value, "abc");
        assert_eq!(input, Input::from(""));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), None);

        let (value, input) = tag("abc")(Input::new("abcd")).expect("valid output");

        assert_eq!(value, "abc");
        assert_eq!(input, Input::from("d"));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('d'));

        let (value, input) = tag("abc")(Input::new("abcö")).expect("valid output");

        assert_eq!(value, "abc");
        assert_eq!(input, Input::from("ö"));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('ö'));

        let (value, input) = tag("abö")(Input::new("aböc")).expect("valid output");

        assert_eq!(value, "abö");
        assert_eq!(input, Input::from("c"));
        assert_eq!(input.position(), Position::new(4, 1, 4));
        assert_eq!(input.current(), Some('c'));
    }

    #[test]
    fn tag_failure() {
        let error = tag("def")(Input::new("deabc")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(2, 1, 3)),
                code::ERR_TAG,
                CharacterError::Tag("def")
            )
        );

        let error = tag("def")(Input::new("de")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(2, 1, 3)).and(
                Position::new(2, 1, 3),
                code::ERR_TAG,
                CharacterError::Tag("def")
            )
        );
    }

    #[test]
    fn none_of_success() {
        let (ch, input) = none_of("abc")(Input::new("def")).expect("valid output");

        assert_eq!(ch, 'd');
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert_eq!(input.current(), Some('e'));
    }

    #[test]
    fn none_of_failure() {
        let error = none_of("abc")(Input::new("abc")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_NONE_OF,
                CharacterError::NoneOf("abc")
            )
        );
    }

    #[test]
    fn one_of_success() {
        let (ch, input) = one_of("abc")(Input::new("abc")).expect("valid output");

        assert_eq!(ch, 'a');
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert_eq!(input.current(), Some('b'));
    }

    #[test]
    fn one_of_failure() {
        let error = one_of("abc")(Input::new("def")).expect_err("invalid output");

        assert_eq!(
            error,
            ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_ONE_OF,
                CharacterError::NoneOf("abc")
            )
        );
    }

    #[test]
    fn is_not_success() {
        let (value, input) = is_not("abc")(Input::new("defa")).expect("valid output");

        assert_eq!(value, "def");
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('a'));

        let (value, input) = is_not("abc")(Input::new("defb")).expect("valid output");

        assert_eq!(value, "def");
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('b'));

        let (value, input) = is_not("abc")(Input::new("defc")).expect("valid output");

        assert_eq!(value, "def");
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert_eq!(input.current(), Some('c'));

        let (value, input) = is_not("abc")(Input::new("a")).expect("valid output");

        assert_eq!(value, "");
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert_eq!(input.current(), Some('a'));
    }

    #[test]
    fn is_not_failure() {
        let error = is_not("abc")(Input::new("def")).expect_err("invalid output");

        assert!(error.is_eof());
        assert_eq!(
            error,
            ParseError::eof(Position::new(3, 1, 4)).and(
                Position::new(3, 1, 4),
                code::ERR_IS_NOT,
                CharacterError::IsNot("abc")
            )
        );
    }
}
