use crate::parser::{code, prelude::*, Input, ParseError, ParseResult};

use super::{LiteralError, Radix};

/// Recognizes a natural number literal with an optional sign `'+'` and the
/// given radix.
///
/// # Grammar:
///
/// The grammar does not include a prefix for the radix. This is because the
/// radix is already known when parsing the natural literal. You must implement
/// the prefix yourself.
///
/// The rules for `<Digit R>` are generic over `R` which expands to the radix
/// of the natural literal.
///
/// ```text
/// Natural     = Sign? <Digit R>+
/// Sign        = '+'
/// <Digit 2>   = [0-1]
/// <Digit 8>   = [0-7]
/// <Digit 10>  = [0-9]
/// <Digit 16>  = [0-9A-Fa-f]
///
/// R = 2 | 8 | 10 | 16
/// ```
#[allow(clippy::module_name_repetitions)]
pub fn recognize_natural(radix: Radix) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, &'a str> {
    move |input| {
        recognize(tuple((opt(char('+')), radix.digit1())))(input).map_err(|mut err| {
            err.push(input, code::ERR_NATURAL_FORMAT, LiteralError::NaturalFormat);
            err
        })
    }
}

/// Parses a natural number literal with an optional sign `'+'` and the given
/// radix.
///
/// The grammar is the same as the one for `recognize_natural` except that it
/// parses the value instead of recognizing it. Returns the value as a `u64`.
///
/// The grammar does not include a prefix for the radix. This is because the
/// radix is already known when parsing the natural literal. You must implement
/// the prefix yourself.
pub fn natural(radix: Radix) -> impl for<'a> Fn(Input<'a>) -> ParseResult<'a, u64> {
    move |input| {
        let (output, input) = recognize_natural(radix)(input)?;
        let value = u64::from_str_radix(output, radix.base()).map_err(|_| {
            ParseError::new(input, code::ERR_NATURAL, LiteralError::Natural).and_semantic()
        })?;
        Ok((value, input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NATURALS: [(&str, Radix); 28] = [
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
    ];

    #[test]
    fn recognize_natural_success() {
        for (value, radix) in &NATURALS {
            let input = Input::new(value);
            let result = recognize_natural(*radix)(input);
            assert_eq!(result, Ok((*value, Input::new(""))));
        }
    }

    #[test]
    fn natural_success() {
        for (value, radix) in &NATURALS {
            let input = Input::new(value);
            let result = natural(*radix)(input);
            assert_eq!(
                result,
                Ok((
                    u64::from_str_radix(value, radix.base()).unwrap(),
                    Input::new("")
                ))
            );
        }
    }
}
