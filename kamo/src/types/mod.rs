//! # Type System
//!
//! This module is available when the `types` feature is enabled.
//!
//! Types are defined by a type code which is a byte array. The type code is
//! used to represent the type of a value. This allows for a simple and
//! efficient way to represent types. The syntax of the type code is
//! straightforward and easy to understand and parse. The type code allows the
//! construction of complex types with multiple levels of nesting by combining
//! simpler types.
//!
//! The supported primitive types are the following:
//!
//! - **Boolean**: A boolean value.
//! - **Character**: A Unicode character.
//! - **Integer**: A signed integer value.
//! - **Float**: A floating-point value.
//! - **Symbol**: An interned string value.
//! - **Type**: A type value.
//! - **Binary**: A byte array value.
//!
//! Besides the primitive types, the type system supports the following
//! compound types:
//!
//! - **Array**: A homogeneous collection of elements.
//! - **Pair**: A pair of two values also known as a cons cell.
//! - **Lambda**: A function type.
//! - **Option**: A type that can be `None` or `Some(T)`.
//! - **Union**: A type that can be one of the given types. There must be at
//! least two types in the union.
//!
//! Special types are also supported:
//!
//! - **Any**: A type that can be any value.
//! - **Void**: A type that can't be any type or value. It is used to represent
//! the absence of a type. It is used in the context of functions that don't
//! return a value or do not take any arguments.
//! - **Nil**: A type that represents the absence of a value. It is used in the
//! context of optional types.
//!
//! The types are grouped into different categories:
//!
//! - **Specific Types**: Types that represent a specific value and can be a
//! member of a union type. These types are the following: *Boolean*,
//! *Character*, *Integer*, *Float*, *Symbol*, *Type*, *Binary*, *Array*, *Pair*
//! and *Lambda*.
//! - **Filled Types**: Types that represent a mor general specific value. These
//! types can be used in an option type. These types are the following: *Any*,
//! *Union* and *Specific Types*.
//!
//! ## Parser
//!
//! The type system includes two parsers: a type code parser and a parser for
//! a textual representation of types. The
//! [type code parser](crate::types::parser::binary) is used to parse
//! type codes and convert them into a type. The parser for the
//! [textual representation](crate::types::parser::text) of types is used to
//! parse a string and convert it into a type. The textual representation of
//! types is a human-readable format that can be used to represent types in a
//! more readable way than the type code.
//!
//! ## Type Checker
//!
//! The [`TypeChecker`] trait is used to implement type checking for an
//! interpreter or compiler. The type checker is used to check if a value has a
//! specific type or if a type is a subtype of another type. The type checker
//! can also be used to infer the type of an expression or a value. The type
//! checker is used to implement type inference and type checking for a
//! programming language.

mod base;
mod checker;
mod error;
mod subtypes;

use smallvec::SmallVec;

pub mod parser;

pub use base::Type;
pub use checker::{Parameters, TypeCheckError, TypeChecker};
pub use error::TypeError;
pub use parser::TypeCodeError;
pub use subtypes::{ArrayType, LambdaType, OptionType, PairType, UnionType};

/// Maximum number of elements in an array.
pub const ARRAY_MAX: usize = 0x000f_ffff_ffff;

/// A type alias for a small vector of type codes. The inline storage is 16
/// bytes which is enough for most types.
type TypeVec = SmallVec<[u8; 16]>;
