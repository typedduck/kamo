//! # Binary Type Code and Predefined Types
//!
//! This module defines the binary type code and some predefined types.

use crate::types::Type;

/// The nil type which represents the absence of a value.
pub const NIL: u8 = 0x00;
/// The void type which represents the absence of a value.
pub const VOID: u8 = 0x01;
/// The any type which represents any typed value. This excludes the void type.
pub const ANY: u8 = 0x02;
/// The type type which represents a type value.
pub const TYPE: u8 = 0x03;
/// The bool type which represents a boolean value.
pub const BOOL: u8 = 0x04;
/// The char type which represents a character value.
pub const CHAR: u8 = 0x05;
/// The int type which represents an integer value.
pub const INT: u8 = 0x06;
/// The float type which represents a floating-point value.
pub const FLOAT: u8 = 0x07;
/// The string type which represents a symbol value, also known as an interned
/// string.
pub const SYMBOL: u8 = 0x08;
/// The binary type which represents a byte array value.
pub const BINARY: u8 = 0x09;
/// The array type which represents a homogeneous array value.
pub const ARRAY: u8 = 0x0a;
/// The pair type which represents a cons cell value.
pub const PAIR: u8 = 0x0b;
/// The lambda type which represents a function value.
pub const LAMBDA: u8 = 0x0c;
/// The option type which represents an optional value. The some variant is
/// a filled type and the none variant is a nil type.
pub const OPTION: u8 = 0x0d;
/// The union type which represents a sequence of at least two types, which
/// can be used alternatively (or-ed sequence).
pub const UNION: u8 = 0x0e;

/// Marker for the start of a variadic type.
pub const VARIADIC: u8 = 0x40;
/// Marker for the start of a return type.
pub const RETURN: u8 = 0x41;
/// Marker for the end of a union type.
pub const END: u8 = 0x7f;

/// Marker for the start of a fixed length of an array with a maximum length of
/// 15 elements (4-bit length field)
pub const FIXED0: u8 = 0x80;
pub const FIXED0_MAX: u8 = 0x8f;
/// Marker for the start of a fixed length of an array with a maximum length of
/// 4095 elements (12-bit length field)
pub const FIXED1: u8 = 0x90;
pub const FIXED1_MAX: u8 = 0x9f;
/// Marker for the start of a fixed length of an array with a maximum length of
/// 1048575 elements (20-bit length field)
pub const FIXED2: u8 = 0xa0;
pub const FIXED2_MAX: u8 = 0xaf;
/// Marker for the start of a fixed length of an array with a maximum length of
/// 268435455 elements (28-bit length field)
pub const FIXED3: u8 = 0xb0;
pub const FIXED3_MAX: u8 = 0xbf;
/// Marker for the start of a fixed length of an array with a maximum length of
/// 68719476735 elements (36-bit length field)
pub const FIXED4: u8 = 0xc0;
pub const FIXED4_MAX: u8 = 0xcf;

lazy_static! {
    /// Predifined nil type.
    pub static ref T_NIL: Type = unsafe { Type::new_unchecked([NIL]) };
    /// Predifined void type.
    pub static ref T_VOID: Type = unsafe { Type::new_unchecked([VOID]) };
    /// Predifined any type.
    pub static ref T_ANY: Type = unsafe { Type::new_unchecked([ANY]) };
    /// Predifined type type.
    pub static ref T_TYPE: Type = unsafe { Type::new_unchecked([TYPE]) };
    /// Predifined bool type.
    pub static ref T_BOOL: Type = unsafe { Type::new_unchecked([BOOL]) };
    /// Predifined char type.
    pub static ref T_CHAR: Type = unsafe { Type::new_unchecked([CHAR]) };
    /// Predifined int type.
    pub static ref T_INT: Type = unsafe { Type::new_unchecked([INT]) };
    /// Predifined float type.
    pub static ref T_FLOAT: Type = unsafe { Type::new_unchecked([FLOAT]) };
    /// Predifined string type.
    pub static ref T_STRING: Type = unsafe { Type::new_unchecked([ARRAY, CHAR]) };
    /// Predifined symbol type.
    pub static ref T_SYMBOL: Type = unsafe { Type::new_unchecked([SYMBOL]) };
    /// Predifined binary type.
    pub static ref T_BINARY: Type = unsafe { Type::new_unchecked([BINARY]) };
    /// Predifined vector type an array of any type.
    pub static ref T_VECTOR: Type = unsafe { Type::new_unchecked([ARRAY, ANY]) };
    /// Predifined list type a pair of any type and an optional list type.
    pub static ref T_LIST: Type = unsafe { Type::new_unchecked([PAIR, OPTION, ANY, OPTION, ANY]) };
}
