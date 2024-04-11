use std::fmt;

/// Definition of the error messages of the character parsers.
#[allow(clippy::module_name_repetitions)]
pub enum CharacterError {
    /// The parser was unable to match an alphabetic character.
    Alpha,
    /// The parser was unable to match an alphanumeric character.
    AlphaNum,
    /// The parser was unable to match any character.
    AnyChar,
    /// The parser was unable to match the given character.
    Char(char),
    /// The parser was unable to match a binary digit.
    BinDigit,
    /// The parser was unable to match an octal digit.
    OctDigit,
    /// The parser was unable to match a digit.
    Digit,
    /// The parser was unable to match a hexadecimal digit.
    HexDigit,
    /// The parser was unable to match the given tag.
    Tag(&'static str),
    /// The parser was unable to match a whitespace.
    Whitespace,
    /// The parser was unable to match a space.
    Space,
    /// The parser was unable to match a graphic character.
    Graphic,
    /// The parser was unable to match a tabulator.
    Tab,
    /// The parser was unable to take the given number of characters.
    Take(usize),
    /// The parser was unable to take at least the given number of characters.
    TakeWhileM(usize),
    /// The parser was unable to take at least one character.
    TakeWhile1,
    /// The parser was unable to match a newline.
    Newline,
    /// The parser was unable to match a character that is not one of the given values.
    NoneOf(&'static str),
    /// The parser was unable to match a character that is one of the given values.
    OneOf(&'static str),
    /// The parser was unable to match a character that is one of the given values.
    IsNot(&'static str),
    /// The parser was unable to match a character that satisfies the predicate.
    Satisfy,
    /// The parser was unable to match a line ending.
    LineEnding,
    /// The parser was unable to match a line ending.
    Crlf,
    /// The parser was unable to match a alphabetic Unicode character.
    UnicodeAlpha,
    /// The parser was unable to match a alphanumeric Unicode character.
    UnicodeAlphaNum,
    /// The parser was unable to match a numeric Unicode character.
    UnicodeNumeric,
    /// The parser was unable to match a Unicode character that starts an identifier.
    UnicodeIdentStart,
    /// The parser was unable to match a lowercase Unicode character.
    UnicodeLowercase,
    /// The parser was unable to match a uppercase Unicode character.
    UnicodeUppercase,
    /// The parser was unable to match a whitespace Unicode character.
    UnicodeWhitespace,
    #[cfg(feature = "regex")]
    /// The parser was unable to match a sequence that matches the given regular expression.
    Regex(String),
}

impl fmt::Display for CharacterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Alpha => write!(f, "expecting an alphabetic character [a-zA-Z]"),
            Self::AlphaNum => write!(f, "expecting an alphanumeric character [a-zA-Z0-9]"),
            Self::AnyChar => write!(f, "expecting any character"),
            Self::Char(ch) => write!(f, "expecting the character '{ch:#}'"),
            Self::BinDigit => write!(f, "expecting a binary digit [0-1]"),
            Self::OctDigit => write!(f, "expecting an octal digit [0-7]"),
            Self::Digit => write!(f, "expecting a digit [0-9]"),
            Self::HexDigit => write!(f, "expecting a hexadecimal digit [0-9A-Fa-f]"),
            Self::Tag(tag) => write!(f, "expecting the tag \"{tag:#}\""),
            Self::Whitespace => write!(f, "expecting a whitespace [ \\t\\n\\r]"),
            Self::Space => write!(f, "expecting a space [ \\t]"),
            Self::Graphic => write!(f, "expecting a graphic character [!-~]"),
            Self::Tab => write!(f, "expecting a tabulator [\\t]"),
            Self::Take(m) => write!(f, "expecting exactly {m} character(s)"),
            Self::TakeWhileM(m) => {
                write!(f, "expecting at least {m} character(s)")
            }
            Self::TakeWhile1 => write!(f, "expecting at least one character"),
            Self::Newline => write!(f, "expecting a newline [\\n]"),
            Self::NoneOf(values) => {
                write!(f, "expecting a character that is not one of {values:?}")
            }
            Self::OneOf(values) => write!(f, "expecting a character that is one of {values:?}"),
            Self::IsNot(values) => {
                write!(f, "expecting a character that is one of {values:?}")
            }
            Self::Satisfy => write!(f, "expecting a character that satisfies the predicate"),
            Self::LineEnding => write!(f, "expecting a line ending [\\r]?[\\n]"),
            Self::Crlf => write!(f, "expecting a line ending [\\r][\\n]"),
            Self::UnicodeAlpha => write!(f, "expecting an alphabetic Unicode character"),
            Self::UnicodeAlphaNum => write!(f, "expecting an alphanumeric Unicode character"),
            Self::UnicodeNumeric => write!(f, "expecting a numeric Unicode character"),
            Self::UnicodeIdentStart => {
                write!(f, "expecting a Unicode character that starts an identifier")
            }
            Self::UnicodeLowercase => write!(f, "expecting a lowercase Unicode character"),
            Self::UnicodeUppercase => write!(f, "expecting a uppercase Unicode character"),
            Self::UnicodeWhitespace => write!(f, "expecting a whitespace Unicode character"),
            #[cfg(feature = "regex")]
            Self::Regex(re) => write!(
                f,
                "expecting a sequence matching the regular expression: {}",
                re
            ),
        }
    }
}
