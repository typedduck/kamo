use std::fmt;

/// Definition of the error messages of the literal parsers.
pub enum LiteralError {
    /// Expecting a floating-point number.
    Float,
    /// Expecting a floating-point number.
    FloatFormat,
    /// Expecting an exponent.
    ExponentFormat,
    /// Expecting an integer number.
    Integer,
    /// Expecting an integer number.
    IntegerFormat,
    /// Expecting a natural number.
    Natural,
    /// Expecting a natural number.
    NaturalFormat,
    /// Malformed single escape code: ['"\\\\abednrt0]
    SingleEscapeCode,
    /// Malformed ascii escape code: x[0-7][0-9A-Fa-f]
    AsciiEscapeCode,
    /// Malformed unicode escape code: u{{[0-9A-Fa-f]{{1-6}}}}
    UnicodeEscapeCode,
    /// Invalid unicode character
    UnicodeChar(u32),
    /// Escape sequence: ['"\\\\abednrt0] | x[0-7][0-9A-Fa-f] | u{{[0-9A-Fa-f]{{1-6}}}}
    Escape,
    /// Escape sequence part: ['"\\\\abednrt0] | x[0-7][0-9A-Fa-f] | u{{[0-9A-Fa-f]{{1-6}}}}
    EscapePart,
    /// Quoted character: "'" ( <is_char_quotable> | EscapedChar ) "'"
    QuotedChar,
}

impl fmt::Display for LiteralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float => write!(f, "expecting a floating-point number"),
            Self::FloatFormat => write!(f, "expecting a floating-point number"),
            Self::ExponentFormat => write!(f, "expecting an exponent"),
            Self::Integer => write!(f, "expecting an integer number"),
            Self::IntegerFormat => write!(f, "expecting an integer number"),
            Self::Natural => write!(f, "expecting a natural number"),
            Self::NaturalFormat => write!(f, "expecting a natural number"),
            Self::SingleEscapeCode => write!(f, "malformed single escape code: ['\"\\\\abednrt0]"),
            Self::AsciiEscapeCode => write!(f, "malformed ascii escape code: x[0-7][0-9A-Fa-f]"),
            Self::UnicodeEscapeCode => write!(f, "malformed unicode escape code: u{{[0-9A-Fa-f]{{1-6}}}}"),
            Self::UnicodeChar(code) => write!(f, "invalid unicode character: 0x{:x}", code),
            Self::Escape => write!(f, "escape sequence: ['\"\\\\abednrt0] | x[0-7][0-9A-Fa-f] | u{{[0-9A-Fa-f]{{1-6}}}}"),
            Self::EscapePart => write!(f, "escape sequence part: ['\"\\\\abednrt0] | x[0-7][0-9A-Fa-f] | u{{[0-9A-Fa-f]{{1-6}}}}"),
            Self::QuotedChar => write!(f, "quoted character: \"'\" ( <is_char_quotable> | EscapedChar ) \"'\""),
        }
    }
}
