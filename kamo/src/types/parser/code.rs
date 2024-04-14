use crate::types::Type;

pub const NIL: u8 = 0x00;
pub const VOID: u8 = 0x01;
pub const ANY: u8 = 0x02;
pub const TYPE: u8 = 0x03;
pub const BOOL: u8 = 0x04;
pub const CHAR: u8 = 0x05;
pub const INT: u8 = 0x06;
pub const FLOAT: u8 = 0x07;
pub const SYMBOL: u8 = 0x08;
pub const BINARY: u8 = 0x09;
pub const ARRAY: u8 = 0x0a;
pub const PAIR: u8 = 0x0b;
pub const LAMBDA: u8 = 0x0c;
pub const OPTION: u8 = 0x0d;
pub const UNION: u8 = 0x0e;

pub const VARIADIC: u8 = 0x40;
pub const RETURN: u8 = 0x41;
pub const END: u8 = 0x7f;

pub const FIXED0: u8 = 0x80;
pub const FIXED0_MAX: u8 = 0x8f;
pub const FIXED1: u8 = 0x90;
pub const FIXED1_MAX: u8 = 0x9f;
pub const FIXED2: u8 = 0xa0;
pub const FIXED2_MAX: u8 = 0xaf;
pub const FIXED3: u8 = 0xb0;
pub const FIXED3_MAX: u8 = 0xbf;
pub const FIXED4: u8 = 0xc0;
pub const FIXED4_MAX: u8 = 0xcf;

lazy_static! {
    pub static ref T_NIL: Type = unsafe { Type::new_unchecked([NIL]) };
    pub static ref T_VOID: Type = unsafe { Type::new_unchecked([VOID]) };
    pub static ref T_ANY: Type = unsafe { Type::new_unchecked([ANY]) };
    pub static ref T_TYPE: Type = unsafe { Type::new_unchecked([TYPE]) };
    pub static ref T_BOOL: Type = unsafe { Type::new_unchecked([BOOL]) };
    pub static ref T_CHAR: Type = unsafe { Type::new_unchecked([CHAR]) };
    pub static ref T_INT: Type = unsafe { Type::new_unchecked([INT]) };
    pub static ref T_FLOAT: Type = unsafe { Type::new_unchecked([FLOAT]) };
    pub static ref T_STRING: Type = unsafe { Type::new_unchecked([ARRAY, CHAR]) };
    pub static ref T_SYMBOL: Type = unsafe { Type::new_unchecked([SYMBOL]) };
    pub static ref T_BINARY: Type = unsafe { Type::new_unchecked([BINARY]) };
    pub static ref T_VECTOR: Type = unsafe { Type::new_unchecked([ARRAY, ANY]) };
    pub static ref T_LIST: Type = unsafe { Type::new_unchecked([PAIR, OPTION, ANY, OPTION, ANY]) };
}
