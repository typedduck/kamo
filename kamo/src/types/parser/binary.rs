use crate::types::Type;

use super::{code, TypeCodeError};

/// Parse a type code stream into a type. A type code stream is a sequence of
/// bytes that represents a type. This function is called by the
/// [`Type::new()`].
///
/// # Errors
///
/// If the type code stream is empty or is malformed, an error is returned.
///
/// # Grammar
///
/// ```text
/// type = (filled | option | code::VOID | code::NIL) eof
/// ```
pub fn parse(src: &[u8]) -> Result<Type, TypeCodeError> {
    let first = src.first().copied();
    let (ty, rest) = match first {
        Some(code::OPTION) => parse_option(src, 0)?,
        Some(code::VOID) => (Type::void(), &src[1..]),
        Some(code::NIL) => (Type::nil(), &src[1..]),
        Some(_) => parse_filled(src, 0)?,
        None => return Err(TypeCodeError::Empty),
    };

    if rest.is_empty() {
        Ok(ty)
    } else {
        Err(TypeCodeError::ExpectedEof(src.len() - rest.len()))
    }
}

/// Parse a type code stream into a specific type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not a specific type, an error is
/// returned.
///
/// # Grammar
///
/// ```text
/// specific = code::TYPE | code::BOOL | code::CHAR | code::INT
///          | code::FLOAT | code::SYMBOL | code::BINARY
///          | array | pair | lambda
/// ```
pub fn parse_specific(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    src.first()
        .copied()
        .ok_or(TypeCodeError::ExpectedSpecificTypeEof(offset))
        .and_then(|code| match code {
            code::TYPE => Ok((Type::typedef(), &src[1..])),
            code::BOOL => Ok((Type::boolean(), &src[1..])),
            code::CHAR => Ok((Type::character(), &src[1..])),
            code::INT => Ok((Type::integer(), &src[1..])),
            code::FLOAT => Ok((Type::float(), &src[1..])),
            code::SYMBOL => Ok((Type::symbol(), &src[1..])),
            code::BINARY => Ok((Type::binary(), &src[1..])),
            code::ARRAY => parse_array(src, offset),
            code::PAIR => parse_pair(src, offset),
            code::LAMBDA => parse_lambda(src, offset),
            _ => Err(TypeCodeError::ExpectedSpecificType(offset)),
        })
}

/// Parse a type code stream into a filled type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not a filled type, an error is
/// returned.
///
/// # Grammar
///
/// ```text
/// filled = code::ANY | union | specific
/// ```
pub fn parse_filled(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    src.first()
        .copied()
        .ok_or(TypeCodeError::ExpectedFilledTypeEof(offset))
        .and_then(|code| match code {
            code::ANY => Ok((Type::any(), &src[1..])),
            code::TYPE => Ok((Type::typedef(), &src[1..])),
            code::BOOL => Ok((Type::boolean(), &src[1..])),
            code::CHAR => Ok((Type::character(), &src[1..])),
            code::INT => Ok((Type::integer(), &src[1..])),
            code::FLOAT => Ok((Type::float(), &src[1..])),
            code::SYMBOL => Ok((Type::symbol(), &src[1..])),
            code::BINARY => Ok((Type::binary(), &src[1..])),
            code::ARRAY => parse_array(src, offset),
            code::PAIR => parse_pair(src, offset),
            code::LAMBDA => parse_lambda(src, offset),
            code::UNION => parse_union(src, offset),
            _ => Err(TypeCodeError::ExpectedFilledType(offset)),
        })
}

/// Parse a type code stream into a filled type or an option type. A type code
/// stream is a sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not a filled or option type, an error
/// is returned.
///
/// # Grammar
///
/// ```text
/// filled-or-option = filled | option
/// ```
pub fn parse_filled_or_option(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let first = src
        .first()
        .copied()
        .ok_or(TypeCodeError::ExpectedTypeOrOption(offset))?;

    match first {
        code::OPTION => parse_option(src, offset),
        _ => parse_filled(src, offset),
    }
}

#[inline]
fn expect_array(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .ok_or(TypeCodeError::ExpectedArrayEof(offset))
        .and_then(|c| match c {
            code::ARRAY => Ok((&src[1..], offset + 1)),
            _ => Err(TypeCodeError::ExpectedArray(offset)),
        })
}

/// Parse a type code stream into an array type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not an array type, an error is
/// returned.
///
/// # Panics
///
/// This function will panic if the array type cannot be created from the
/// emitted type code. This should never happen as the type code is generated
/// from a valid type.
///
/// # Grammar
///
/// ```text
/// array = code::ARRAY filled-or-option fixed
/// ```
pub fn parse_array(input: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let (cursor, offset) = expect_array(input, offset)?;
    let (elem, cursor) = parse_filled_or_option(cursor, offset)?;
    let (size, src) = parse_fixed(cursor, offset + (input.len() - cursor.len()))?;

    Ok((Type::array(elem, size).expect("array type"), src))
}

#[inline]
fn expect_pair(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .ok_or(TypeCodeError::ExpectedPairEof(offset))
        .and_then(|c| match c {
            code::PAIR => Ok((&src[1..], offset + 1)),
            _ => Err(TypeCodeError::ExpectedPair(offset)),
        })
}

/// Parse a type code stream into a pair type. A type code stream is a sequence
/// of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not a pair type, an error is
/// returned.
///
/// # Panics
///
/// This function will panic if the pair type cannot be created from the emitted
/// type code. This should never happen as the type code is generated from a
/// valid type.
///
/// # Grammar
///
/// ```text
/// pair = code::PAIR filled-or-option filled-or-option
/// ```
#[allow(clippy::similar_names)]
pub fn parse_pair(input: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let (cursor, offset) = expect_pair(input, offset)?;
    let (car, cursor) = parse_filled_or_option(cursor, offset)?;
    let (cdr, cursor) = parse_filled_or_option(cursor, offset + (input.len() - cursor.len()))?;

    Ok((Type::pair(car, cdr).expect("pair taype"), cursor))
}

#[inline]
fn expect_lambda(input: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    input
        .first()
        .copied()
        .ok_or(TypeCodeError::ExpectedLambdaEof(offset))
        .and_then(|c| match c {
            code::LAMBDA => Ok((&input[1..], offset + 1)),
            _ => Err(TypeCodeError::ExpectedLambda(offset)),
        })
}

/// Parse a type code stream into a lambda type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is not a lambda type, an error is
/// returned.
///
/// # Panics
///
/// This function will panic if the lambda type cannot be created from the
/// emitted type code. This should never happen as the type code is generated
/// from a valid type.
///
/// # Grammar
///
/// ```text
/// lambda = code::LAMBDA args varg return
/// ```
pub fn parse_lambda(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let (src, offset) = expect_lambda(src, offset)?;
    let (args, cursor) = parse_args(src, offset)?;
    let (varg, cursor) = parse_varg(cursor, offset + (src.len() - cursor.len()))?;
    let (ret, cursor) = parse_return(cursor, offset + (src.len() - cursor.len()))?;

    Ok((Type::lambda(args, varg, ret).expect("lambda type"), cursor))
}

/// Parse a type code stream into a sequence of types. The sequence may be of
/// zero length. A type code stream is a sequence of bytes that represents a
/// type.
///
/// # Errors
///
/// If the type code stream is malformed, an error is returned.
///
/// # Grammar
///
/// ```text
/// args = (filled-or-option)*
/// ```
pub fn parse_args(src: &[u8], offset: usize) -> Result<(Vec<Type>, &[u8]), TypeCodeError> {
    let mut args = Vec::new();
    let mut offset = offset;
    let mut cursor = src;

    while let Ok((arg, rest)) = parse_filled_or_option(cursor, offset) {
        args.push(arg);
        offset += cursor.len() - rest.len();
        cursor = rest;
    }

    Ok((args, cursor))
}

/// Parse a type code stream into an optional variadic type. A type code stream
/// is a sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is malformed, an error is returned.
///
/// # Grammar
///
/// ```text
/// varg = (code::VARIADIC filled-or-option)?
/// ```
pub fn parse_varg(src: &[u8], offset: usize) -> Result<(Option<Type>, &[u8]), TypeCodeError> {
    if src.first().copied() == Some(code::VARIADIC) {
        let offset = offset + 1;
        let (arg, src) = parse_filled_or_option(&src[1..], offset)?;

        Ok((Some(arg), src))
    } else {
        Ok((None, src))
    }
}

#[inline]
fn expect_return(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .filter(|&c| c == code::RETURN)
        .map(|_| (&src[1..], offset + 1))
        .ok_or(TypeCodeError::ExpectedReturn(offset))
}

/// Parse a type code stream into a return type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is malformed, an error is returned.
///
/// # Grammar
///
/// ```text
/// return = code::RETURN (code::VOID | filled-or-option)
/// ```
pub fn parse_return(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let (src, offset) = expect_return(src, offset)?;
    let (ty, rest) = match src.first().copied() {
        Some(code::OPTION) => parse_option(src, offset)?,
        Some(code::VOID) => (Type::void(), &src[1..]),
        Some(_) => parse_filled(src, offset)?,
        None => return Err(TypeCodeError::ExpectedReturnType(offset)),
    };

    Ok((ty, rest))
}

#[inline]
fn expect_option(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .filter(|&c| c == code::OPTION)
        .map(|_| (&src[1..], offset + 1))
        .ok_or(TypeCodeError::ExpectedOption(offset))
}

/// Parse a type code stream into an option type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is malformed, an error is returned.
///
/// # Panics
///
/// This function will panic if the option type cannot be created from the
/// emitted type code. This should never happen as the type code is generated
/// from a valid type.
///
/// # Grammar
///
/// ```text
/// option = code::OPTION filled
/// ```
pub fn parse_option(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let (src, offset) = expect_option(src, offset)?;
    let (ty, rest) = match src.first().copied() {
        Some(code::OPTION) => return Err(TypeCodeError::ExpectedFilledType(offset)),
        Some(_) => parse_filled(src, offset)?,
        None => return Err(TypeCodeError::ExpectedOptionType(offset)),
    };

    Ok((Type::option(ty).expect("option type"), rest))
}

#[inline]
fn expect_union(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .filter(|&c| c == code::UNION)
        .map(|_| (&src[1..], offset + 1))
        .ok_or(TypeCodeError::ExpectedUnion(offset))
}

fn expect_union_end(src: &[u8], offset: usize) -> Result<(&[u8], usize), TypeCodeError> {
    src.first()
        .copied()
        .filter(|&c| c == code::END)
        .map(|_| (&src[1..], offset + 1))
        .ok_or(TypeCodeError::ExpectedMemberOrEnd(offset))
}

/// Parse a type code stream into a union type. A type code stream is a
/// sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is malformed, an error is returned.
///
/// # Panics
///
/// This function will panic if the union type cannot be created from the
/// emitted type code. This should never happen as the type code is generated
/// from a valid type.
///
/// # Grammar
///
/// ```text
/// union = code::UNION specific specific+ code::END
/// ```
pub fn parse_union(src: &[u8], offset: usize) -> Result<(Type, &[u8]), TypeCodeError> {
    let mut members = Vec::new();
    let (cursor, offset) = expect_union(src, offset)?;
    let (ty1, cursor) = parse_specific(cursor, offset)?;
    let offset = src.len() - cursor.len();
    let (ty2, mut cursor) = parse_specific(cursor, offset)?;
    let mut offset = src.len() - cursor.len();

    members.push(ty1);
    members.push(ty2);
    while let Ok((ty, rest)) = parse_specific(cursor, offset) {
        members.push(ty);
        offset += cursor.len() - rest.len();
        cursor = rest;
    }

    let (cursor, _) = expect_union_end(cursor, offset)?;

    Ok((Type::union(members).expect("union type"), cursor))
}

/// Parse a type code stream into an optional fixed length value. A type code
/// stream is a sequence of bytes that represents a type.
///
/// # Errors
///
/// If the type code stream is empty or is malformed, an error is returned.
///
/// # Grammar
///
/// ```text
/// fixed  = (fixed0 | fixed1 | fixed2 | fixed3 | fixed4)?
/// fixed0 = code::FIXED0..=code::FIXED0_MAX
/// fixed1 = code::FIXED1..=code::FIXED1_MAX byte
/// fixed2 = code::FIXED2..=code::FIXED2_MAX byte byte
/// fixed3 = code::FIXED3..=code::FIXED3_MAX byte byte byte
/// fixed4 = code::FIXED4..=code::FIXED4_MAX byte byte byte byte
/// byte   = 0x00..=0xFF
/// ```
pub fn parse_fixed(src: &[u8], offset: usize) -> Result<(Option<usize>, &[u8]), TypeCodeError> {
    let (fixed, len) = match src.first().copied() {
        Some(val @ code::FIXED0..=code::FIXED0_MAX) => ((val - code::FIXED0) as usize, 1),
        Some(val @ code::FIXED1..=code::FIXED1_MAX) => {
            let mut fixed = ((val - code::FIXED1) as usize) << 8;

            if src.len() < 2 {
                return Err(TypeCodeError::ExpectedFixedByte(offset));
            }
            fixed |= src[1] as usize;
            (fixed, 2)
        }
        Some(val @ code::FIXED2..=code::FIXED2_MAX) => {
            let mut fixed = ((val - code::FIXED2) as usize) << 16;

            if src.len() < 3 {
                return Err(TypeCodeError::ExpectedFixedBytes(offset, 2));
            }
            fixed |= (src[1] as usize) << 8;
            fixed |= src[2] as usize;
            (fixed, 3)
        }
        Some(val @ code::FIXED3..=code::FIXED3_MAX) => {
            let mut fixed = ((val - code::FIXED3) as usize) << 24;

            if src.len() < 4 {
                return Err(TypeCodeError::ExpectedFixedBytes(offset, 3));
            }
            fixed |= (src[1] as usize) << 16;
            fixed |= (src[2] as usize) << 8;
            fixed |= src[3] as usize;
            (fixed, 4)
        }
        Some(val @ code::FIXED4..=code::FIXED4_MAX) => {
            let mut fixed = ((val - code::FIXED4) as usize) << 32;

            if src.len() < 5 {
                return Err(TypeCodeError::ExpectedFixedBytes(offset, 4));
            }
            fixed |= (src[1] as usize) << 24;
            fixed |= (src[2] as usize) << 16;
            fixed |= (src[3] as usize) << 8;
            fixed |= src[4] as usize;
            (fixed, 5)
        }
        Some(_) | None => return Ok((None, src)),
    };
    Ok((Some(fixed), &src[len..]))
}
