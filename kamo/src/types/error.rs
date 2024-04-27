#![allow(clippy::module_name_repetitions)]

use std::{error::Error, fmt};

use super::{Type, ARRAY_MAX};

/// An error that occurs during type checking.
#[derive(Clone, PartialEq, Eq)]
pub enum TypeError {
    /// The size of a fixed array exceeds the maximum size. Takes the actual
    /// size.
    ArraySizeTooLarge(usize),
    /// Array element type must either be a filled type or an option type.
    ArrayElemNotFilled,
    /// Nested option types are not supported.
    NestedOption,
    /// Option type must be a filled. It can than be either `nil` or the filled
    /// type.
    OptionNotFilled,
    /// Parameter type must either be a filled type or an option type. Takes the
    /// parameter index starting from 1.
    ParamNotFilled(usize),
    /// Variadic type must either be a filled type or an option type.
    VariadicNotFilled,
    /// Pair car type must either be a filled type or an option type.
    PairCarNotFilled,
    /// Pair cdr type must either be a filled type or an option type.
    PairCdrNotFilled,
    /// Union type must contain only specific types.
    UnionNotSpecific(Type),
    /// Union type must contain at least two members.
    UnionTooFewMembers,
}

impl Error for TypeError {}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ArraySizeTooLarge(size) => {
                write!(f, "array size exceeds maximum of {ARRAY_MAX}: {size}")
            }
            Self::ArrayElemNotFilled => write!(f, "array element type is not filled or an option"),
            Self::NestedOption => write!(f, "nested option types are not supported"),
            Self::OptionNotFilled => write!(f, "option type is not filled"),
            Self::ParamNotFilled(idx) => {
                write!(f, "parameter {idx} type is not filled or an option")
            }
            Self::VariadicNotFilled => write!(f, "variadic type is not filled or an option"),
            Self::PairCarNotFilled => write!(f, "pair car type is not filled or an option"),
            Self::PairCdrNotFilled => write!(f, "pair cdr type is not filled or an option"),
            Self::UnionNotSpecific(ty) => {
                write!(f, "union type must contain only specific types, got: {ty}")
            }
            Self::UnionTooFewMembers => write!(f, "union type must contain at least two members"),
        }
    }
}

impl fmt::Debug for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
