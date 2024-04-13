#![allow(clippy::module_name_repetitions, clippy::pub_use)]

use alloc::fmt;

mod abbrev;
pub use abbrev::emit_abbrev;

mod boolean;
pub use boolean::emit_boolean;

mod bytevector;
pub use bytevector::emit_bytevector;

mod character;
pub use character::emit_character;

mod datum;
pub use datum::emit_datum;

mod decimal;
pub use decimal::emit_decimal;

mod helper;

mod infnan;
pub use infnan::emit_infnan;

mod list;
pub use list::emit_list;

mod number;
pub use number::emit_number;

mod string;
pub use string::emit_string;

mod symbol;
pub use symbol::emit_symbol;

mod vector;
pub use vector::emit_vector;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Number {
    Integer(i64),
    Float(f64),
    Infinty,
    NaN,
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Integer(val) => write!(f, "{val}"),
            Self::Float(val) => write!(f, "{val}"),
            Self::Infinty => write!(f, "infity"),
            Self::NaN => write!(f, "+NaN"),
        }
    }
}
