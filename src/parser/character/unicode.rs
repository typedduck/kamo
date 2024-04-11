//! # Unicode character parsers
//!
//! This module contains parsers that match a single or multiple Unicode
//! characters. They return either the matched character or a slice containing
//! the matched characters.
//!
//! ## Example
//!
//! ```rust
//! # use kamo::parser::{prelude::{*, unicode::*}, Input};
//! let (output, input) = alpha0(Input::from("abc京123"))
//!     .expect("valid output");
//!
//! assert_eq!(output, "abc京");
//! assert_eq!(input, Input::from("123"));
//!
//! let (output, input) = alpha0(Input::from("123abc京"))
//!     .expect("valid output");
//!
//! assert_eq!(output, "");
//! assert_eq!(input, Input::from("123abc京"));
//! ```

use crate::parser::{code, predicate, Input, ParseError, ParseResult};

use super::CharacterError;

/// Matches zero or more Unicode alphabetic characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = alpha0(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc京");
/// assert_eq!(input, Input::from("123"));
///
/// let (output, input) = alpha0(Input::from("123abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("123abc京"));
/// ```
pub fn alpha0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_alpha);

    Ok((output, cursor))
}

/// Matches one or more Unicode alphabetic characters.
///
/// # Errors
///
/// If the input is empty or the first character is not an alphabetical
/// character, it returns a [`ParseError`] with the error code
/// [`ERR_ALPHA`](code::ERR_ALPHA) and the error variant
/// [`CharacterError::UnicodeAlpha`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = alpha1(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc京");
/// assert_eq!(input, Input::from("123"));
///
/// let error = alpha1(Input::from("123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHA,
///     CharacterError::Alpha
/// ));
///
/// let error = alpha1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHA,
///     CharacterError::Alpha
/// ));
/// ```
pub fn alpha1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_alpha(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_ALPHA,
                CharacterError::UnicodeAlpha,
            ));
        }
        alpha0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_ALPHA, CharacterError::UnicodeAlpha))
    }
}

/// Matches zero or more Unicode alphanumeric characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = alphanum0(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc京123");
/// assert_eq!(input, Input::from(""));
///
/// let (output, input) = alphanum0(Input::from("123abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "123abc京");
/// assert_eq!(input, Input::from(""));
/// ```
pub fn alphanum0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_alphanum);

    Ok((output, cursor))
}

/// Matches one or more Unicode alphanumeric characters.
///
/// # Errors
///
/// If the input is empty or the first character is not an alphanumeric
/// character, it returns a [`ParseError`] with the error code
/// [`ERR_ALPHANUM`](code::ERR_ALPHANUM) and the error variant
/// [`CharacterError::UnicodeAlphaNum`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = alphanum1(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc京123");
/// assert_eq!(input, Input::from(""));
///
/// let error = alphanum1(Input::from("_123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///    Position::new(0, 1, 1),
///   code::ERR_ALPHANUM,
///   CharacterError::UnicodeAlphaNum
/// ));
///
/// let error = alphanum1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_ALPHANUM,
///     CharacterError::UnicodeAlphaNum
/// ));
/// ```
pub fn alphanum1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_alphanum(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_ALPHANUM,
                CharacterError::UnicodeAlphaNum,
            ));
        }
        alphanum0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_ALPHANUM, CharacterError::UnicodeAlphaNum))
    }
}

/// Matches zero or more Unicode numeric characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = numeric0(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("abc京123"));
///
/// let (output, input) = numeric0(Input::from("123abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "123");
/// assert_eq!(input, Input::from("abc京"));
/// ```
pub fn numeric0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_numeric);

    Ok((output, cursor))
}

/// Matches one or more Unicode numeric characters.
///
/// # Errors
///
/// If the input is empty or the first character is not a numeric character,
/// it returns a [`ParseError`] with the error code
/// [`ERR_NUMERIC`](code::ERR_NUMERIC) and the error variant
/// [`CharacterError::UnicodeNumeric`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = numeric1(Input::from("123abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "123");
/// assert_eq!(input, Input::from("abc京"));
///
/// let error = numeric1(Input::from("abc123京")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_NUMERIC,
///     CharacterError::UnicodeNumeric
/// ));
///
/// let error = numeric1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_NUMERIC,
///     CharacterError::UnicodeNumeric
/// ));
/// ```
pub fn numeric1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_numeric(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_NUMERIC,
                CharacterError::UnicodeNumeric,
            ));
        }
        numeric0(input)
    } else {
        Err(ParseError::eof(input).and(input, code::ERR_NUMERIC, CharacterError::UnicodeNumeric))
    }
}

/// Matches zero or more Unicode lowercase characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = lowercase0(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from("京123"));
///
/// let (output, input) = lowercase0(Input::from("Abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("Abc京"));
/// ```
pub fn lowercase0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_lowercase);

    Ok((output, cursor))
}

/// Matches one or more Unicode lowercase characters.
///
/// # Errors
///
/// If the input is empty or the first character is not a lowercase character,
/// it returns a [`ParseError`] with the error code
/// [`ERR_LOWERCASE`](code::ERR_LOWERCASE) and the error variant
/// [`CharacterError::UnicodeLowercase`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = lowercase1(Input::from("abc京123"))
///  .expect("valid output");
///
/// assert_eq!(output, "abc");
/// assert_eq!(input, Input::from("京123"));
///
/// let error = lowercase1(Input::from("Abc京")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LOWERCASE,
///     CharacterError::UnicodeLowercase
/// ));
///
/// let error = lowercase1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_LOWERCASE,
///     CharacterError::UnicodeLowercase
/// ));
/// ```
pub fn lowercase1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_lowercase(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_LOWERCASE,
                CharacterError::UnicodeLowercase,
            ));
        }
        lowercase0(input)
    } else {
        Err(ParseError::eof(input).and(
            input,
            code::ERR_LOWERCASE,
            CharacterError::UnicodeLowercase,
        ))
    }
}

/// Matches zero or more Unicode uppercase characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = uppercase0(Input::from("ABC京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "ABC");
/// assert_eq!(input, Input::from("京123"));
///
/// let (output, input) = uppercase0(Input::from("abc京"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("abc京"));
/// ```
pub fn uppercase0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_uppercase);

    Ok((output, cursor))
}

/// Matches one or more Unicode uppercase characters.
///
/// # Errors
///
/// If the input is empty or the first character is not an uppercase character,
/// it returns a [`ParseError`] with the error code
/// [`ERR_UPPERCASE`](code::ERR_UPPERCASE) and the error variant
/// [`CharacterError::UnicodeUppercase`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = uppercase1(Input::from("ABC京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "ABC");
/// assert_eq!(input, Input::from("京123"));
///
/// let error = uppercase1(Input::from("abc京")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_UPPERCASE,
///     CharacterError::UnicodeUppercase
/// ));
///
/// let error = uppercase1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_UPPERCASE,
///     CharacterError::UnicodeUppercase
/// ));
/// ```
pub fn uppercase1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_uppercase(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_UPPERCASE,
                CharacterError::UnicodeUppercase,
            ));
        }
        uppercase0(input)
    } else {
        Err(ParseError::eof(input).and(
            input,
            code::ERR_UPPERCASE,
            CharacterError::UnicodeUppercase,
        ))
    }
}

/// Matches zero or more Unicode whitespace characters.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::parser::{prelude::{*, unicode::*}, Input};
/// let (output, input) = whitespace0(Input::from(" \t\n\u{a0}abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, " \t\n\u{a0}");
/// assert_eq!(input, Input::from("abc京123"));
///
/// let (output, input) = whitespace0(Input::from("abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, "");
/// assert_eq!(input, Input::from("abc京123"));
/// ```
pub fn whitespace0(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_whitespace);

    Ok((output, cursor))
}

/// Matches one or more Unicode whitespace characters.
///
/// # Errors
///
/// If the input is empty or the first character is not a whitespace character,
/// it returns a [`ParseError`] with the error code
/// [`ERR_WHITESPACE`](code::ERR_WHITESPACE) and the error variant
/// [`CharacterError::UnicodeWhitespace`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = whitespace1(Input::from(" \t\n\u{a0}abc京123"))
///     .expect("valid output");
///
/// assert_eq!(output, " \t\n\u{a0}");
/// assert_eq!(input, Input::from("abc京123"));
///
/// let error = whitespace1(Input::from("abc京123")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_WHITESPACE,
///     CharacterError::UnicodeWhitespace
/// ));
///
/// let error = whitespace1(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_WHITESPACE,
///     CharacterError::UnicodeWhitespace
/// ));
/// ```
pub fn whitespace1(input: Input<'_>) -> ParseResult<&str> {
    if let Some(ch) = input.current() {
        if !predicate::is_whitespace(ch) {
            return Err(ParseError::new(
                input,
                code::ERR_WHITESPACE,
                CharacterError::UnicodeWhitespace,
            ));
        }
        whitespace0(input)
    } else {
        Err(ParseError::eof(input).and(
            input,
            code::ERR_WHITESPACE,
            CharacterError::UnicodeWhitespace,
        ))
    }
}

/// Matches a Unicode identifier start character.
///
/// Implements the [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/).
/// The standard states that the following characters are valid identifier start
/// characters:
///
/// > Start characters are derived from the Unicode `General_Category` of
/// > uppercase letters, lowercase letters, titlecase letters, modifier letters,
/// > other letters, letter numbers, plus `Other_ID_Start`, minus
/// > `Pattern_Syntax` and `Pattern_White_Space` code points.
///
/// This includes the ASCII characters `A-Z` and `a-z`.
///
/// # Errors
///
/// If the input is empty or the first character is not an identifier start
/// character, it returns a [`ParseError`] with the error code
/// [`ERR_IDENT_START`](code::ERR_IDENT_START) and the error variant
/// [`CharacterError::UnicodeIdentStart`].
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = ident_start(Input::from("京abc123"))
///     .expect("valid output");
///
/// assert_eq!(output, '京');
/// assert_eq!(input, Input::from("abc123"));
///
/// let (output, input) = ident_start(Input::from("abc123"))
///     .expect("valid output");
///
/// assert_eq!(output, 'a');
/// assert_eq!(input, Input::from("bc123"));
///
/// let error = ident_start(Input::from("123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_IDENT_START,
///     CharacterError::UnicodeIdentStart
/// ));
///
/// let error = ident_start(Input::from("")).expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_IDENT_START,
///     CharacterError::UnicodeIdentStart
/// ));
/// ```
pub fn ident_start(input: Input<'_>) -> ParseResult<char> {
    let mut cursor = input;

    cursor.current().map_or_else(
        || {
            Err(ParseError::eof(input).and(
                input,
                code::ERR_IDENT_START,
                CharacterError::UnicodeIdentStart,
            ))
        },
        |ch| {
            if predicate::is_ident_start(ch) {
                cursor.advance();
                Ok((ch, cursor))
            } else {
                Err(ParseError::new(
                    input,
                    code::ERR_IDENT_START,
                    CharacterError::UnicodeIdentStart,
                ))
            }
        },
    )
}

/// Matches a Unicode identifier start character or a character that satisfies
/// the given predicate.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// Implements the [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/).
/// The standard states that the following characters are valid identifier start
/// characters:
///
/// > Start characters are derived from the Unicode `General_Category` of
/// > uppercase letters, lowercase letters, titlecase letters, modifier letters,
/// > other letters, letter numbers, plus `Other_ID_Start`, minus
/// > `Pattern_Syntax` and `Pattern_White_Space` code points.
///
/// This includes the ASCII characters `A-Z` and `a-z`.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
/// let (output, input) = ident_start_or(|ch| ch == '_')
///     (Input::from("_京abc123")).expect("valid output");
///
/// assert_eq!(output, '_');
/// assert_eq!(input, Input::from("京abc123"));
///
/// let (output, input) = ident_start_or(|ch| ch == '_')
///     (Input::from("abc123")).expect("valid output");
///
/// assert_eq!(output, 'a');
/// assert_eq!(input, Input::from("bc123"));
///
/// let error = ident_start_or(|ch| ch == '_')
///     (Input::from("123abc")).expect_err("error output");
///
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_IDENT_START,
///     CharacterError::UnicodeIdentStart
/// ));
///
/// let error = ident_start_or(|ch| ch == '_')(Input::from(""))
///     .expect_err("error output");
///
/// assert!(error.is_eof());
/// assert_eq!(error, ParseError::new(
///     Position::new(0, 1, 1),
///     code::ERR_IDENT_START,
///     CharacterError::UnicodeIdentStart
/// ));
/// ```
pub fn ident_start_or<F>(predicate: F) -> impl Fn(Input) -> ParseResult<char> + Copy
where
    F: Fn(char) -> bool + Copy,
{
    move |input| {
        let mut cursor = input;

        cursor.current().map_or_else(
            || {
                Err(ParseError::eof(input).and(
                    input,
                    code::ERR_IDENT_START,
                    CharacterError::UnicodeIdentStart,
                ))
            },
            |ch| {
                if predicate::is_ident_start(ch) || predicate(ch) {
                    cursor.advance();
                    Ok((ch, cursor))
                } else {
                    Err(ParseError::new(
                        input,
                        code::ERR_IDENT_START,
                        CharacterError::UnicodeIdentStart,
                    ))
                }
            },
        )
    }
}

/// Matches zero or more Unicode identifier continuation characters.
///
/// Implements the [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/).
/// The standard states that the following characters are valid identifier
/// continuation characters:
///
/// > Continue characters include [start characters](ident_start), plus
/// > characters having the Unicode `General_Category` of nonspacing marks,
/// > spacing combining marks, decimal number, connector punctuation, plus
/// > `Other_ID_Continue`, minus `Pattern_Syntax` and `Pattern_White_Space` code
/// > points.
///
/// This includes the ASCII characters `A-Z`, `a-z`, `0-9` and `_`.
///
/// # Errors
///
/// This function does not return any errors.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
///
/// let (output, input) = ident_cont(Input::from("_abc123-"))
///     .expect("valid output");
///
/// assert_eq!(output, "_abc123");
/// assert_eq!(input, Input::from("-"));
///
/// let (output, input) = ident_cont(Input::from("京abc123"))
///     .expect("valid output");
///
/// assert_eq!(output, "京abc123");
/// assert_eq!(input, Input::from(""));
/// ```
pub fn ident_cont(input: Input<'_>) -> ParseResult<&str> {
    let mut cursor = input;
    let output = cursor.advance_while(predicate::is_ident_cont);

    Ok((output, cursor))
}

/// Matches zero or more Unicode identifier continuation character or a
/// character that satisfies the given predicate.
///
/// The predicate is a closure or function that takes a character and returns a
/// boolean.
///
/// Implements the [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/).
/// The standard states that the following characters are valid identifier
/// continuation characters:
///
/// > Continuation characters are derived from the Unicode `General_Category` of
/// > uppercase letters, lowercase letters, titlecase letters, modifier letters,
/// > other letters, letter numbers, plus `Other_ID_Continue`, minus
/// > `Pattern_Syntax` and `Pattern_White_Space` code points.
///
/// This includes the ASCII characters `A-Z`, `a-z` and `0-9`.
///
/// # Example
///
/// ```rust
/// # use kamo::{Position, parser::{prelude::{*, unicode::*}, CharacterError, code, Input}};
///
/// let (output, input) = ident_cont_or(|ch| ch == '$')
///     (Input::from("_abc123$-")).expect("valid output");
///
/// assert_eq!(output, "_abc123$");
/// assert_eq!(input, Input::from("-"));
///
/// let (output, input) = ident_cont_or(|ch| ch == '$')
///     (Input::from("京abc123$")).expect("valid output");
///
/// assert_eq!(output, "京abc123$");
/// assert_eq!(input, Input::from(""));
/// ```
pub fn ident_cont_or<F>(predicate: F) -> impl Fn(Input) -> ParseResult<&str> + Copy
where
    F: Fn(char) -> bool + Copy,
{
    move |input| {
        let mut cursor = input;
        let output = cursor.advance_while(|ch| predicate::is_ident_cont(ch) || predicate(ch));

        Ok((output, cursor))
    }
}
