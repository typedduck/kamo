use crate::parser::{code, prelude::*, Input, ParseError, ParseResult};

use super::{LiteralError, Radix};

/// Recognizes an integer literal with the given radix.
///
/// If `signed` is `true` the literal can have an optional sign `[+\-]`.
///
/// # Grammar:
///
/// The grammar does not include a prefix for the radix. This is because the
/// radix is already known when parsing the integer literal. You must implement
/// the prefix yourself.
///
/// The rules for `<Digit R>` are generic over `R` which expands to the radix
/// of the integer literal. The radix can be one of the following:
///
/// ```text
/// Integer     = Sign? <Digit R>+
/// Sign        = [+\-]
/// <Digit 2>   = [0-1]
/// <Digit 8>   = [0-7]
/// <Digit 10>  = [0-9]
/// <Digit 16>  = [0-9A-Fa-f]
///
/// R = 2 | 8 | 10 | 16
/// ```
pub fn recognize_integer(
    radix: Radix,
    signed: bool,
) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &str> {
    move |input| {
        match signed {
            true => recognize(tuple((opt(any((char('+'), char('-')))), radix.digit1())))(input),
            false => recognize(radix.digit1())(input),
        }
        .map_err(|mut err| {
            err.push(input, code::ERR_INTEGER_FORMAT, LiteralError::IntegerFormat);
            err
        })
    }
}

/// Parses an integer literal with the given radix.
///
/// If `signed` is `true` the literal can have an optional sign `[+\-]`.
///
/// The grammar is the same as the one for `recognize_integer` except that it
/// parses the value instead of recognizing it. Returns the value as a `i64`.
///
/// The grammar does not include a prefix for the radix. This is because the
/// radix is already known when parsing the integer literal. You must implement
/// the prefix yourself.
pub fn integer(radix: Radix, signed: bool) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, i64> {
    move |input| {
        let (output, input) = recognize_integer(radix, signed)(input)?;
        let value = i64::from_str_radix(output, radix.base())
            .map_err(|_| ParseError::new(input, code::ERR_INTEGER, LiteralError::Integer))?;

        Ok((value, input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const INTEGERS: [(&str, Radix); 42] = [
        ("0", Radix::Binary),
        ("0", Radix::Octal),
        ("0", Radix::Decimal),
        ("0", Radix::Hexadecimal),
        ("1", Radix::Binary),
        ("1", Radix::Octal),
        ("1", Radix::Decimal),
        ("1", Radix::Hexadecimal),
        ("0101010", Radix::Binary),
        ("01234567", Radix::Octal),
        ("0123456789", Radix::Decimal),
        ("0123456789", Radix::Hexadecimal),
        ("abcdef", Radix::Hexadecimal),
        ("ABCDEF", Radix::Hexadecimal),
        ("+0", Radix::Binary),
        ("+0", Radix::Octal),
        ("+0", Radix::Decimal),
        ("+0", Radix::Hexadecimal),
        ("+1", Radix::Binary),
        ("+1", Radix::Octal),
        ("+1", Radix::Decimal),
        ("+1", Radix::Hexadecimal),
        ("+0101010", Radix::Binary),
        ("+01234567", Radix::Octal),
        ("+0123456789", Radix::Decimal),
        ("+0123456789", Radix::Hexadecimal),
        ("+abcdef", Radix::Hexadecimal),
        ("+ABCDEF", Radix::Hexadecimal),
        ("-0", Radix::Binary),
        ("-0", Radix::Octal),
        ("-0", Radix::Decimal),
        ("-0", Radix::Hexadecimal),
        ("-1", Radix::Binary),
        ("-1", Radix::Octal),
        ("-1", Radix::Decimal),
        ("-1", Radix::Hexadecimal),
        ("-0101010", Radix::Binary),
        ("-01234567", Radix::Octal),
        ("-0123456789", Radix::Decimal),
        ("-0123456789", Radix::Hexadecimal),
        ("-abcdef", Radix::Hexadecimal),
        ("-ABCDEF", Radix::Hexadecimal),
    ];

    #[test]
    fn recognize_integer_success() {
        for (value, radix) in INTEGERS.iter() {
            let input = Input::new(value);
            let result = recognize_integer(*radix, true)(input);
            assert_eq!(result, Ok((*value, Input::new(""))));
        }
    }

    #[test]
    fn integer_success() {
        for (value, radix) in INTEGERS.iter() {
            let input = Input::new(value);
            let result = integer(*radix, true)(input);
            assert_eq!(
                result,
                Ok((
                    i64::from_str_radix(value, radix.base()).unwrap(),
                    Input::new("")
                ))
            );
        }
    }
}
