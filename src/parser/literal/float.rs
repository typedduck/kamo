use crate::parser::{code, prelude::*, Input, ParseError, ParseResult};

use super::LiteralError;

/// Recognizes a floating-point number exponent.
/// 
/// # Grammar:
/// 
/// ```text
/// Exp = [eE] Sign? Digit+
/// ```
pub fn recognize_exponent(input: Input<'_>) -> ParseResult<'_, &str> {
    recognize(tuple((
        any((char('e'), char('E'))),
        opt(any((char('+'), char('-')))),
        ascii::digit1,
    )))(input)
    .map_err(|mut err| {
        err.push(
            input,
            code::ERR_EXPONENT_FORMAT,
            LiteralError::ExponentFormat,
        );
        err
    })
}

/// Recognizes a floating-point number literal.
/// 
/// If `signed` is `true` the literal can have an optional sign `[+\-]`.
///
/// # Grammar:
/// 
/// ```text
/// Float  = Sign? ( 'inf' | 'infinity' | 'nan' | 'NaN' | Number )
/// Number = Digit+ Exp | ( Digit+ '.' Digit* | Digit* '.' Digit+ ) Exp?
/// Exp    = [eE] Sign? Digit+
/// Sign   = [+\-]
/// Digit  = [0-9]
/// ```
pub fn recognize_float(signed: bool) -> impl FnMut(Input<'_>) -> ParseResult<'_, &str> {
    move |input| {
        let sign = any((char('+'), char('-')));
        let number = any((
            recognize(tuple((ascii::digit1, recognize_exponent))),
            recognize(tuple((
                ascii::digit1,
                char('.'),
                opt(ascii::digit1),
                opt(recognize_exponent),
            ))),
            recognize(tuple((
                opt(ascii::digit1),
                char('.'),
                ascii::digit1,
                opt(recognize_exponent),
            ))),
        ));

        match signed {
            true => recognize(tuple((
                opt(sign),
                any((tag("infinity"), tag("inf"), tag("nan"), tag("NaN"), number)),
            )))(input),
            false => recognize(any((
                tag("infinity"),
                tag("inf"),
                tag("nan"),
                tag("NaN"),
                number,
            )))(input),
        }
        .map_err(|mut err| {
            err.push(input, code::ERR_FLOAT_FORMAT, LiteralError::FloatFormat);
            err
        })
    }
}

/// Parses a floating-point number literal.
/// 
/// If `signed` is `true` the literal can have an optional sign `[+\-]`.
/// 
/// The grammar is the same as [`recognize_float`] expect that it parses the
/// value instead of recognizing it. Returns the value as a `f64`.
pub fn float(signed: bool) -> impl FnMut(Input<'_>) -> ParseResult<'_, f64> {
    move |input| {
        let (output, cursor) = recognize_float(signed)(input)?;
        let value = output
            .parse::<f64>()
            .map_err(|_| ParseError::new(cursor, code::ERR_FLOAT, LiteralError::Float))?;

        Ok((value, cursor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FLOATS: [&str; 47] = [
        "0.0",
        "123.456",
        "123.456e+789",
        "123.456e-789",
        "123.456e789",
        "123.456e789",
        "123.456E+789",
        "123.456E-789",
        "123.456E789",
        "123.456E789",
        ".0",
        ".456",
        ".456e+789",
        ".456e-789",
        ".456e789",
        ".456e789",
        ".456E+789",
        ".456E-789",
        ".456E789",
        ".456E789",
        "0.",
        "123.",
        "123.e+789",
        "123.e-789",
        "123.e789",
        "123.e789",
        "123.E+789",
        "123.E-789",
        "123.E789",
        "123.E789",
        "123e+789",
        "123e-789",
        "123e789",
        "123e789",
        "123E+789",
        "123E-789",
        "123E789",
        "123E789",
        "inf",
        "infinity",
        "NaN",
        "+inf",
        "+infinity",
        "+NaN",
        "-inf",
        "-infinity",
        "-NaN",
    ];

    #[test]
    fn recognize_float_success() {
        for value in FLOATS.iter() {
            let input = Input::new(value);
            let result = recognize_float(true)(input);
            assert_eq!(result, Ok((*value, Input::new(""))));
        }
    }

    #[test]
    fn float_success() {
        for value in FLOATS.iter() {
            let input = Input::new(value);
            let result = float(true)(input);
            let value = value.parse::<f64>().unwrap();

            if value.is_nan() {
                assert!(result.is_ok());
                let (result, input) = result.unwrap();
                assert!(result.is_nan());
                assert_eq!(input, Input::new(""));
            } else {
                assert_eq!(result, Ok((value, Input::new(""))));
            }
        }
    }
}
