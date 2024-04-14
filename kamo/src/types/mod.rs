//! Filled types are the following:
//! - [`Type::any()`](Type::any)
//! - [`Type::typedef()`](Type::typedef)
//! - [`Type::boolean()`](Type::boolean)
//! - [`Type::character()`](Type::character)
//! - [`Type::integer()`](Type::integer)
//! - [`Type::integer()`](Type::integer)
//! - [`Type::float()`](Type::float)
//! - [`Type::symbol()`](Type::symbol)
//! - [`Type::array()`](Type::array)
//! - [`Type::pair()`](Type::pair)
//! - [`Type::lambda()`](Type::lambda)

mod base;
mod checker;
mod error;
mod subtypes;

use smallvec::SmallVec;

pub mod parser;

pub use base::Type;
pub use checker::{TypeCheckError, TypeChecker};
pub use error::TypeError;
pub use parser::TypeCodeError;
pub use subtypes::{ArrayType, LambdaType, OptionType, PairType, UnionType};

/// Maximum number of elements in an array.
pub const ARRAY_MAX: usize = 0x000f_ffff_ffff;

/// A type alias for a small vector of type codes. The inline storage is 16
/// bytes which is enough for most types.
type TypeVec = SmallVec<[u8; 16]>;
