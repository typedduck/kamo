//! # Error Types for the Type Parsers

use std::{error::Error, fmt};

use crate::types::{Type, ARRAY_MAX};

/* #region TypeCodeError */

/// An error that occurs during type code parsing.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeCodeError {
    /// Empty type code stream.
    Empty,
    /// Expected the type code of an array type. Takes the offset.
    ExpectedArray(usize),
    /// Expected the type code of an array type but reached the end of the type
    /// code stream. Takes the offset.
    ExpectedArrayEof(usize),
    /// Expected the type code of a lambda type. Takes the offset.
    ExpectedLambda(usize),
    /// Expected the type code of a lambda type but reached the end of the type
    /// code stream. Takes the offset.
    ExpectedLambdaEof(usize),
    /// Expected the type code of a pair type. Takes the offset.
    ExpectedPair(usize),
    /// Expected the type code of a pair type but reached the end of the type
    /// code stream. Takes the offset.
    ExpectedPairEof(usize),
    /// Expected the type code of an option type. Takes the offset.
    ExpectedOption(usize),
    /// Expected the type code of an option type but reached the end of the type
    /// code stream. Takes the offset.
    ExpectedOptionType(usize),
    /// Expected the type code of a specific type. Takes the offset.
    ExpectedSpecificType(usize),
    /// Expected the type code of a specific type but reached the end of the
    /// type code stream. Takes the offset.
    ExpectedSpecificTypeEof(usize),
    /// Expected the type code of a filled type. Takes the offset.
    ExpectedFilledType(usize),
    /// Expected the type code of a filled type but reached the end of the type
    /// code stream. Takes the offset.
    ExpectedFilledTypeEof(usize),
    /// Expected the type code of either a filled type or an option type but
    /// reached the end of the type code stream. Takes the offset.
    ExpectedTypeOrOption(usize),
    /// Expected the type code for the return type. Takes the offset.
    ExpectedReturn(usize),
    /// Expected the type returned by the function but reached the end of the
    /// type code stream. Takes the offset.
    ExpectedReturnType(usize),
    /// Expected the type code of a union type. Takes the offset.
    ExpectedUnion(usize),
    /// Expected the type code of end marker or a specific type. Takes the
    /// offset.
    ExpectedMemberOrEnd(usize),
    /// Reached end of type code stream while parsing fixed byte. Takes the
    /// offset.
    ExpectedFixedByte(usize),
    /// Reached end of type code stream while parsing fixed bytes. Takes the
    /// offset and the expected number of bytes.
    ExpectedFixedBytes(usize, usize),
    /// Expected end of type code stream at the given offset.
    ExpectedEof(usize),
}

impl Error for TypeCodeError {}

impl fmt::Display for TypeCodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "empty type code stream"),
            Self::ExpectedArray(offset) => write!(f, "[{offset}] expected array type"),
            Self::ExpectedArrayEof(offset) => {
                write!(f, "[{offset}] expected array type, got end of stream")
            }
            Self::ExpectedLambda(offset) => write!(f, "[{offset}] expected lambda type"),
            Self::ExpectedLambdaEof(offset) => {
                write!(f, "[{offset}] expected lambda type, got end of stream")
            }
            Self::ExpectedPair(offset) => write!(f, "[{offset}] expected pair type"),
            Self::ExpectedPairEof(offset) => {
                write!(f, "[{offset}] expected pair type, got end of stream")
            }
            Self::ExpectedOption(offset) => write!(f, "[{offset}] expected option type"),
            Self::ExpectedOptionType(offset) => {
                write!(f, "[{offset}] expected option type, got end of stream")
            }
            Self::ExpectedSpecificType(offset) => write!(f, "[{offset}] expected specific type"),
            Self::ExpectedSpecificTypeEof(offset) => {
                write!(f, "[{offset}] expected specific type, got end of stream")
            }
            Self::ExpectedFilledType(offset) => write!(f, "[{offset}] expected filled type"),
            Self::ExpectedFilledTypeEof(offset) => {
                write!(f, "[{offset}] expected filled type, got end of stream")
            }
            Self::ExpectedTypeOrOption(offset) => {
                write!(
                    f,
                    "[{offset}] expected filled type or option type, got end of stream"
                )
            }
            Self::ExpectedReturn(offset) => write!(f, "[{offset}] expected return type"),
            Self::ExpectedReturnType(offset) => {
                write!(f, "[{offset}] expected return type, got end of stream")
            }
            Self::ExpectedUnion(offset) => write!(f, "[{offset}] expected union type"),
            Self::ExpectedMemberOrEnd(offset) => {
                write!(
                    f,
                    "[{offset}] expected a specific type or end marker of union type"
                )
            }
            Self::ExpectedFixedByte(offset) => {
                write!(f, "[{offset}] expected fixed byte, got end of stream")
            }
            Self::ExpectedFixedBytes(offset, len) => write!(
                f,
                "[{offset}] expected {len} fixed bytes, got end of stream"
            ),
            Self::ExpectedEof(offset) => write!(f, "[{offset}] expected end of type code stream"),
        }
    }
}

impl fmt::Debug for TypeCodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/* #endregion */

/* #region TypeParseError */

pub mod code {
    //! # Error Codes for the Type Parser
    use crate::parser::{code::ERR_CONTEXT, Code};

    pub const ERR_TYPE: Code = ERR_CONTEXT + 0x0001;
    pub const ERR_FILLED_TYPE: Code = ERR_CONTEXT + 0x0002;
    pub const ERR_PREDEFINED_TYPE: Code = ERR_CONTEXT + 0x0003;
    pub const ERR_SPECIFIC_TYPE: Code = ERR_CONTEXT + 0x0004;
    pub const ERR_MALFORMED_NAME: Code = ERR_CONTEXT + 0x0005;
    pub const ERR_UNDEFINED_NAME: Code = ERR_CONTEXT + 0x0006;
    pub const ERR_UNBOUND_NAME: Code = ERR_CONTEXT + 0x0007;
    pub const ERR_EXPECTED_TYPE: Code = ERR_CONTEXT + 0x0008;
    pub const ERR_NESTED_OPTION: Code = ERR_CONTEXT + 0x0009;
    pub const ERR_ARRAY_LENGTH: Code = ERR_CONTEXT + 0x000a;
}

/// An error that occurs during type parsing.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, PartialEq, Eq)]
pub enum TypeParseError {
    /// Array length exceeds maximum.
    ArrayLength,
    /// Array length exceeds maximum.
    ArrayLengthCast,
    /// Expected a type.
    ExpectedType(String, Type),
    /// Malformed type name.
    MalformedName,
    /// Nested option types are not supported.
    NestedOption,
    /// Expecting a filled or named type.
    NotAFilledType,
    /// Not a predefined type.
    NotAPredefinedType,
    /// Not a specific type.
    NotASpecificType,
    /// Not a type.
    NotAType,
    /// Unbound type name.
    UnboundName(String),
    /// Undefined type name.
    UndefinedName(String),
}

impl Error for TypeParseError {}

impl fmt::Display for TypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ArrayLength => {
                write!(f, "array length exceeds maximum of {ARRAY_MAX} elements")
            }
            Self::ArrayLengthCast => {
                write!(f, "array length exceeds maximum of {} elements", usize::MAX)
            }
            Self::ExpectedType(name, declared) => {
                write!(f, "expected that {name} is a type, got {declared}")
            }
            Self::MalformedName => write!(f, "malformed type name"),
            Self::NestedOption => write!(f, "nested option types are not supported"),
            Self::NotAFilledType => write!(f, "expecting a filled type"),
            Self::NotAPredefinedType => write!(f, "expecting a predefined type"),
            Self::NotASpecificType => write!(f, "expecting a specific type"),
            Self::NotAType => write!(f, "not a type"),
            Self::UnboundName(name) => write!(f, "unbound type name: {name}"),
            Self::UndefinedName(name) => write!(f, "undefined type name: {name}"),
        }
    }
}

impl fmt::Debug for TypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/* #endregion */
