use std::{error::Error, fmt, path::Path};

/// Definition of the error messages for the S-expression parser.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SexprError<'a> {
    /// Expecting a binary integer.
    BinaryLiteral,
    /// Expecting a boolean.
    BooleanLiteral,
    /// Expecting a byte between 0 and 255.
    BytevecByte,
    /// Expecting a closing parenthesis of byte-vector.
    BytevecClosing,
    /// Expecting a byte-vector.
    BytevecLiteral,
    /// Expecting a character code of 1 to 6 hex digits.
    CharacterCode,
    /// Character code is not a valid Unicode code point.
    CharacterCodePoint,
    /// Expecting a character literal.
    CharacterLiteral,
    /// Expecting a comment.
    Comment,
    /// Nested comments must close with `|#`.
    CommentClose,
    /// Expecting a datum.
    Datum,
    /// Expecting one of the following: boolean, number with radix, byte-vector or vector.
    DatumHashtag,
    /// Expecting an unquote or unquote-splicing.
    DatumUnquote,
    /// Expecting a decimal number.
    DecimalLiteral,
    /// Error reading file.
    FileError(&'a Path),
    /// Expecting a fraction part for a floating-point.
    FloatFraction,
    /// Expecting a valid floating-point, either infinities or NaN.
    FloatInfNaN,
    /// Number literal could not be converted to a floating-point.
    FloatLiteral,
    /// Expecting a floating-point suffix.
    FloatSuffix,
    /// Expecting a hexadecimal integer.
    HexdecLiteral,
    /// Literal could not be converted to an integer, value is out of range.
    IntegerLiteral,
    /// Expecting intertoken whitespace or comment.
    InterToken,
    /// Expecting a closing parenthesis of list.
    ListClosing,
    /// Expecting a list.
    ListLiteral,
    /// Expecting a number.
    NumberLiteral,
    /// Expecting an octal integer.
    OctalLiteral,
    /// Expecting a closing quote of a string.
    StringClosing,
    /// Expecting a string escaped code point.
    StringCode,
    /// Expecting a string escape sequence or intraline whitespace.
    StringEscape,
    /// Expecting a string.
    StringLiteral,
    /// Expecting a closing vertical bar of a symbol.
    SymbolClosing,
    /// Expecting a symbol escaped code point.
    SymbolCode,
    /// Expecting a symbol escape sequence.
    SymbolEscape,
    /// Expecting a symbol.
    SymbolLiteral,
    /// Symbol may not consist of a single dot, it must be followed by a subsequent character.
    SymbolSingleDot,
    /// Expecting a closing parenthesis of vector.
    VectorClosing,
    /// Expecting a vector.
    VectorLiteral,
    /// Expecting whitespace.
    Whitespace,
}

impl<'a> Error for SexprError<'a> {}

impl<'a> fmt::Display for SexprError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BinaryLiteral => write!(f, "expecting a binary integer"),
            Self::BooleanLiteral => write!(f,
                "expecting a boolean: #true | #t | #false | #f"),
            Self::BytevecByte => write!(f, "expecting a byte between 0 and 255"),
            Self::BytevecClosing => write!(f, "expecting a closing parenthesis of byte-vector"),
            Self::BytevecLiteral => write!(f, "expecting a byte-vector: #u8(<byte>*)"),
            Self::CharacterCode => write!(f, "expecting a character code of 1 to 6 hex digits"),
            Self::CharacterCodePoint => write!(f,
                "character code is not a valid Unicode code point"),
            Self::CharacterLiteral => write!(f,
                "expecting a character: #\\<name> | #\\x<hex> | #\\<char>"),
            Self::Comment => write!(f, "expecting a comment: ; <any>* \\n | #| <any>* |#"),
            Self::CommentClose => write!(f, "nested comments must close with \"|#\""),
            Self::Datum => write!(f, "expecting a datum"),
            Self::DatumHashtag => write!(f,
                "expecting one of the following: boolean, number with radix, byte-vector or vector"),
            Self::DatumUnquote => write!(f, "expecting an unquote or unquote-splicing"),
            Self::DecimalLiteral => write!(f, "expecting a decimal number"),
            Self::FileError(path) => write!(f, "error reading file: {}", path.display()),
            Self::FloatFraction => write!(f, "expecting a fraction part for a floating-point"),
            Self::FloatInfNaN => write!(f, "expecting a valid floating-point: [+|-]inf.0 | nan.0"),
            Self::FloatLiteral => write!(f,
                "number could not be converted to a floating-point"),
            Self::FloatSuffix => write!(f,
                "expecting a floating-point suffix: [e|E][+|-]?<digit>+"),
            Self::HexdecLiteral => write!(f, "expecting a hexadecimal integer"),
            Self::IntegerLiteral => write!(f,
                "literal could not be converted to an integer, value is out of range"),
            Self::InterToken => write!(f, "expecting intertoken whitespace or comment"),
            Self::ListClosing => write!(f, "expecting a closing parenthesis of list"),
            Self::ListLiteral => write!(f, "expecting a list: (<datum>*) | (<datum>+ . <datum>)"),
            Self::NumberLiteral => write!(f, "expecting a number"),
            Self::OctalLiteral => write!(f, "expecting an octal integer"),
            Self::StringClosing => write!(f, "expecting a closing quote of a string"),
            Self::StringCode => write!(f, "expecting a string escaped code point: \\x<hex>{{1,6}};"),
            Self::StringEscape => write!(f,
                "expecting a string escape sequence or intraline whitespace: \\(a | b | t | n | r | \\ | \" | x<hex>{{1,6}};)"),
            Self::StringLiteral => write!(f, "expecting a string"),
            Self::SymbolClosing => write!(f, "expecting a closing vertical bar of a symbol"),
            Self::SymbolCode => write!(f, "expecting a symbol escaped code point: \\x<hex>{{1,6}};"),
            Self::SymbolEscape => write!(f,
                "expecting a symbol escape sequence: \\(a | b | t | n | r | \"|\" | x<hex>{{1,6}};)"),
            Self::SymbolLiteral => write!(f, "expecting a symbol"),
            Self::SymbolSingleDot => write!(f,
                "symbol may not consist of a single dot, it must be followed by a subsequent character"),
            Self::VectorClosing => write!(f, "expecting a closing parenthesis of vector"),
            Self::VectorLiteral => write!(f, "expecting a vector: #(<datum>*)"),
            Self::Whitespace => write!(f, "expecting whitespace"),
        }
    }
}

impl<'a> fmt::Debug for SexprError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub mod code {
    //! Error codes for S-expression parser.
    //!
    //! Error codes start with [`ERR_CUSTOM`] and end at [`ERR_SEXPR_END`].

    use crate::parser::{code::ERR_CUSTOM, Code};

    // Whitespace and comment errors
    pub const ERR_COMMENT: Code = ERR_CUSTOM;
    pub const ERR_COMMENT_CLOSE: Code = ERR_CUSTOM + 0x0001;
    pub const ERR_WHITESPACE: Code = ERR_CUSTOM + 0x0002;
    pub const ERR_INTER_TOKEN: Code = ERR_CUSTOM + 0x0003;
    // Boolean errors
    pub const ERR_BOOLEAN_LITERAL: Code = ERR_CUSTOM + 0x0004;
    // Character errors
    pub const ERR_CHARACTER_CODE: Code = ERR_CUSTOM + 0x0005;
    pub const ERR_CHARACTER_CODE_POINT: Code = ERR_CUSTOM + 0x0006;
    pub const ERR_CHARACTER_LITERAL: Code = ERR_CUSTOM + 0x0007;
    // Number errors
    pub const ERR_BINARY_LITERAL: Code = ERR_CUSTOM + 0x0008;
    pub const ERR_DECIMAL_LITERAL: Code = ERR_CUSTOM + 0x0009;
    pub const ERR_FLOAT_FRACTION: Code = ERR_CUSTOM + 0x000a;
    pub const ERR_FLOAT_INFNAN: Code = ERR_CUSTOM + 0x000b;
    pub const ERR_FLOAT_LITERAL: Code = ERR_CUSTOM + 0x000c;
    pub const ERR_FLOAT_SUFFIX: Code = ERR_CUSTOM + 0x000d;
    pub const ERR_HEXDEC_LITERAL: Code = ERR_CUSTOM + 0x000e;
    pub const ERR_INTEGER_LITERAL: Code = ERR_CUSTOM + 0x000f;
    pub const ERR_OCTAL_LITERAL: Code = ERR_CUSTOM + 0x0010;
    pub const ERR_NUMBER_LITERAL: Code = ERR_CUSTOM + 0x0011;
    // String errors
    pub const ERR_STRING_CLOSING: Code = ERR_CUSTOM + 0x0012;
    pub const ERR_STRING_CODE: Code = ERR_CUSTOM + 0x0013;
    pub const ERR_STRING_ESCAPE: Code = ERR_CUSTOM + 0x0014;
    pub const ERR_STRING_LITERAL: Code = ERR_CUSTOM + 0x0015;
    // Symbol errors
    pub const ERR_SYMBOL_CLOSING: Code = ERR_CUSTOM + 0x0016;
    pub const ERR_SYMBOL_CODE: Code = ERR_CUSTOM + 0x0017;
    pub const ERR_SYMBOL_ESCAPE: Code = ERR_CUSTOM + 0x0018;
    pub const ERR_SYMBOL_LITERAL: Code = ERR_CUSTOM + 0x0019;
    pub const ERR_SYMBOL_SINGLE_DOT: Code = ERR_CUSTOM + 0x001a;
    // Byte-vector errors
    pub const ERR_BYTEVEC_BYTE: Code = ERR_CUSTOM + 0x001b;
    pub const ERR_BYTEVEC_CLOSING: Code = ERR_CUSTOM + 0x001c;
    pub const ERR_BYTEVEC_LITERAL: Code = ERR_CUSTOM + 0x001d;
    // Vector errors
    pub const ERR_VECTOR_CLOSING: Code = ERR_CUSTOM + 0x001e;
    pub const ERR_VECTOR_LITERAL: Code = ERR_CUSTOM + 0x001f;
    // List errors
    pub const ERR_LIST_CLOSING: Code = ERR_CUSTOM + 0x0020;
    pub const ERR_LIST_LITERAL: Code = ERR_CUSTOM + 0x0021;
    // File error
    pub const ERR_FILE: Code = ERR_CUSTOM + 0x0022;
    // Datum errors
    pub const ERR_DATUM: Code = ERR_CUSTOM + 0x0023;
    pub const ERR_DATUM_HASHTAG: Code = ERR_CUSTOM + 0x0024;
    pub const ERR_DATUM_UNQUOTE: Code = ERR_CUSTOM + 0x0025;
    /// S-expression errors end at `ERR_CUSTOM + 0x0026`.
    pub const ERR_SEXPR_END: Code = ERR_CUSTOM + 0x0026;
}
