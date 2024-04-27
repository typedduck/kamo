//! This module contains the parsers for the types in text as well as binary
//! format.

pub mod binary;
pub mod code;
pub mod error;
pub mod text;

pub use error::{TypeCodeError, TypeParseError};
