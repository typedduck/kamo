//! Error codes for the parser.

/// Error codes for the parser.
/// 
/// Codes are grouped by their type. The first byte of the code indicates the
/// type of the error. The second byte indicates the category of the error. The
/// codes from `0x0000_0000` to `0x0000_ffff` are reserved for the core library.
/// The codes from `0x8000_0000` to `0xffff_ffff` are reserved for custom
/// errors.
/// 
/// These codes are used to identify the error in the
/// [`ParseError`](crate::parser::ParseError) and to provide a unique method to
/// identify the error. This is a convenient way to handle errors in a
/// programmatic way. Custom parsers can use the custom error codes to identify
/// their errors. But they can also use the core error codes if they want to or
/// omit the error code completely and only use the error message with an error
/// code of [`ERR_CUSTOM`].
/// 
/// An important error code is [`ERR_EOF`]. It is used to indicate an unexpected
/// end of file and can be used to determine if more input is required to parse
/// the input.
pub type Code = u32;

pub const ERR_UNKNOWN: Code = 0x0000_0000;
pub const ERR_EOF: Code = 0x0000_0001;
pub const ERR_CONTEXT: Code = 0x0000_0002;

/// Branch errors start at `0x0000_0100`.
pub const ERR_BRANCH_START: Code = 0x0000_0100;
pub const ERR_ANY: Code = 0x0000_0101;
/// Branch errors end at `0x0000_01ff`.
pub const ERR_BRANCH_END: Code = 0x0000_0200;

/// Characters errors start at `0x0000_0200`.
pub const ERR_CHAR_START: Code = 0x0000_0200;
pub const ERR_CHAR: Code = 0x0000_0201;
pub const ERR_BIN_DIGIT: Code = 0x0000_0202;
pub const ERR_OCT_DIGIT: Code = 0x0000_0203;
pub const ERR_DIGIT: Code = 0x0000_0204;
pub const ERR_HEX_DIGIT: Code = 0x0000_0205;
pub const ERR_TAG: Code = 0x0000_0206;
pub const ERR_WHITESPACE: Code = 0x0000_0207;
pub const ERR_SPACE: Code = 0x0000_0208;
pub const ERR_GRAPHIC: Code = 0x0000_0209;
pub const ERR_TAKE: Code = 0x0000_020a;
pub const ERR_TAKE_WHILE_M: Code = 0x0000_020b;
pub const ERR_TAKE_WHILE_1: Code = 0x0000_020c;
pub const ERR_NONE_OF: Code = 0x0000_020d;
pub const ERR_ONE_OF: Code = 0x0000_020e;
pub const ERR_SATISFY: Code = 0x0000_020f;
pub const ERR_LINE_ENDING: Code = 0x0000_0210;
pub const ERR_ALPHA: Code = 0x0000_0211;
pub const ERR_ALPHANUM: Code = 0x0000_0212;
pub const ERR_NUMERIC: Code = 0x0000_0213;
pub const ERR_LOWERCASE: Code = 0x0000_0214;
pub const ERR_UPPERCASE: Code = 0x0000_0215;
pub const ERR_IDENT_START: Code = 0x0000_0216;
#[cfg(feature = "regex")]
pub const ERR_REGEX: Code = 0x0000_0217;
/// Characters errors end at `0x0000_02ff`.
pub const ERR_CHAR_END: Code = 0x0000_0300;

/// Combinator errors start at `0x0000_0300`.
pub const ERR_COMBINATOR_START: Code = 0x0000_0300;
pub const ERR_NOT_EOF: Code = 0x0000_0301;
pub const ERR_RECOGNIZE: Code = 0x0000_0302;
pub const ERR_VERIFY: Code = 0x0000_0303;
/// Combinator errors end at `0x0000_03ff`.
pub const ERR_COMBINATOR_END: Code = 0x0000_0400;

/// Literal errors start at `0x0000_0400`.
pub const ERR_LITERAL_START: Code = 0x0000_0400;
pub const ERR_FLOAT: Code = 0x000_0401;
pub const ERR_FLOAT_FORMAT: Code = 0x000_0402;
pub const ERR_EXPONENT_FORMAT: Code = 0x000_0403;
pub const ERR_INTEGER: Code = 0x000_0404;
pub const ERR_INTEGER_FORMAT: Code = 0x000_0405;
pub const ERR_NATURAL: Code = 0x000_0406;
pub const ERR_NATURAL_FORMAT: Code = 0x000_0407;
pub const ERR_SINGLE_ESCAPE: Code = 0x000_0408;
pub const ERR_SINGLE_ESCAPE_PART: Code = 0x000_0409;
pub const ERR_ASCII_ESCAPE: Code = 0x000_040a;
pub const ERR_ASCII_ESCAPE_CODE: Code = 0x000_040b;
pub const ERR_UNICODE_ESCAPE: Code = 0x000_040c;
pub const ERR_UNICODE_ESCAPE_CODE: Code = 0x000_040d;
pub const ERR_UNICODE_CHAR: Code = 0x000_040e;
pub const ERR_ESCAPE: Code = 0x000_040f;
pub const ERR_ESCAPE_PART: Code = 0x000_0410;
pub const ERR_QUOTED_CHAR: Code = 0x000_0411;
/// Literal errors end at `0x0000_04ff`.
pub const ERR_LITERAL_END: Code = 0x0000_0500;

/// Sequence errors start at `0x0000_0500`.
pub const ERR_SEQUENCE_START: Code = 0x0000_0500;
pub const ERR_TUPLE: Code = 0x0000_0501;
pub const ERR_LIST_START: Code = 0x0000_0502;
pub const ERR_LIST_M: Code = 0x0000_0503;
pub const ERR_LIST_NEXT: Code = 0x0000_0504;
pub const ERR_TERMINATED: Code = 0x0000_0505;
pub const ERR_PRECEDED: Code = 0x0000_0506;
pub const ERR_INFINITE: Code = 0x0000_0507;
pub const ERR_MANY_1: Code = 0x0000_0508;
pub const ERR_MANY_M: Code = 0x0000_0509;
pub const ERR_MANY_1_COUNT: Code = 0x0000_050a;
pub const ERR_MANY_M_COUNT: Code = 0x0000_050b;
pub const ERR_PAIR_FIRST: Code = 0x0000_050c;
pub const ERR_PAIR_SECOND: Code = 0x0000_050d;
/// Sequence errors end at `0x0000_05ff`.
pub const ERR_SEQUENCE_END: Code = 0x0000_0600;

/// Custom errors start at `0x8000_0000`.
pub const ERR_CUSTOM: Code = 0x8000_0000;
