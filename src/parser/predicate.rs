//! Predefined character predicates.
//!
//! This module contains a number of predefined character predicates that can be
//! used as predicate-functions in some of the parser combinators. In the library
//! predicates are not implemented as closures, but as functions. This allows
//! the predicates to be definded in one place. This ensures that the predicates
//! are not duplicated and that they are consistent throughout the library.

use unicode_ident::{is_xid_continue, is_xid_start};

/// Returns `true` if the character is a valid Unicode letter.
#[must_use]
#[inline]
pub fn is_alpha(c: char) -> bool {
    c.is_alphabetic()
}

/// Returns `true` if the character is a valid Unicode letter or number.
#[must_use]
#[inline]
pub fn is_alphanum(c: char) -> bool {
    c.is_alphanumeric()
}

/// Returns `true` if the character is a valid ASCII letter. `[A-Za-z]`
#[must_use]
#[inline]
pub const fn is_ascii_alpha(c: char) -> bool {
    c.is_ascii_alphabetic()
}

/// Returns `true` if the character is a valid ASCII letter or number.
/// `[0-9A-Fa-f]`
#[must_use]
#[inline]
pub const fn is_ascii_alphanum(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

/// Returns `true` if the character is a printable, non-whitespace ASCII
/// character. `[!-~]`
#[must_use]
#[inline]
pub const fn is_ascii_printable(c: char) -> bool {
    c.is_ascii_graphic()
}

/// Returns `true` if the character is a space or tab. `[ \t]`
#[must_use]
#[inline]
pub const fn is_ascii_space(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

/// Returns `true` if the character is a ASCII whitespace character.
/// `[ \t\n\r\f]`
#[must_use]
#[inline]
pub const fn is_ascii_whitespace(c: char) -> bool {
    c.is_ascii_whitespace()
}

/// Returns `true` if the character is a binary digit. `[0-1]`
#[must_use]
#[inline]
pub const fn is_bin_digit(c: char) -> bool {
    matches!(c, '0' | '1')
}

/// Returns `true` if the character is a printable, non-whitespace ASCII or
/// Unicode character excluding `'` and `\`. It is used to determine if a
/// character can be used in a quoted character literal.
#[must_use]
#[inline]
pub fn is_char_quotable(c: char) -> bool {
    is_printable(c) && !matches!(c, '\'' | '\\')
}

/// Returns `true` if the character is a decimal digit. `[0-9]`
#[must_use]
#[inline]
pub const fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// Returns `true` if the character is a hexadecimal digit. `[0-9A-Fa-f]`
#[must_use]
#[inline]
pub const fn is_hex_digit(c: char) -> bool {
    c.is_ascii_hexdigit()
}

/// Returns `true` if the character is a valid Unicode identifier continuation
/// character.
///
/// Implements the [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/).
/// The standard states that the following characters are valid identifier
/// continuation characters:
///
/// > Continue characters include [start characters](is_ident_start), plus
/// > characters having the Unicode `General_Category` of nonspacing marks,
/// > spacing combining marks, decimal number, connector punctuation, plus
/// > `Other_ID_Continue`, minus `Pattern_Syntax` and `Pattern_White_Space` code
/// > points.
///
/// This includes the ASCII characters `A-Z`, `a-z`, `0-9` and `_`.
#[must_use]
#[inline]
pub fn is_ident_cont(c: char) -> bool {
    is_xid_continue(c)
}

/// Returns `true` if the character is a valid Unicode identifier start
/// character.
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
#[must_use]
#[inline]
pub fn is_ident_start(c: char) -> bool {
    is_xid_start(c)
}

/// Returns `true` if the character is a valid Unicode lowercase letter.
#[must_use]
#[inline]
pub fn is_lowercase(c: char) -> bool {
    c.is_lowercase()
}

/// Returns `true` if the character is a valid Unicode number.
#[must_use]
#[inline]
pub fn is_numeric(c: char) -> bool {
    c.is_numeric()
}

/// Returns `true` if the character is an octal digit. `[0-7]`
#[must_use]
#[inline]
pub const fn is_oct_digit(c: char) -> bool {
    matches!(c, '0'..='7')
}

/// Returns `true` if the character is a printable, non-whitespace ASCII or
/// Unicode character.
#[must_use]
#[inline]
pub fn is_printable(c: char) -> bool {
    c.is_ascii_graphic() && !c.is_control()
}

/// Returns `true` if the character is a printable, non-whitespace ASCII or
/// Unicode character excluding `"` and `\`. It is used to determine if a
/// character can be used in a quoted string literal.
#[must_use]
#[inline]
pub fn is_string_quotable(c: char) -> bool {
    is_printable(c) && !matches!(c, '"' | '\\')
}

/// Returns `true` if the character is a valid Unicode uppercase letter.
#[must_use]
#[inline]
pub fn is_uppercase(c: char) -> bool {
    c.is_uppercase()
}

/// Returns `true` if the character is a valid Unicode whitespace character.
#[must_use]
#[inline]
pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}
