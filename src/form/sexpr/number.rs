use std::fmt;

use crate::{
    parser::{predicate, prelude::*, Code, Input, ParseResult, Span},
    value::Value,
};

use super::{
    code::{
        ERR_BINARY_LITERAL, ERR_DECIMAL_LITERAL, ERR_FLOAT_FRACTION, ERR_FLOAT_INFNAN,
        ERR_FLOAT_LITERAL, ERR_FLOAT_SUFFIX, ERR_HEXDEC_LITERAL, ERR_INTEGER_LITERAL,
        ERR_NUMBER_LITERAL, ERR_OCTAL_LITERAL,
    },
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a number literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// number = number_binary | number_octal | number_hexdec | number_decimal
    ///        | decimal | infnan
    /// ```
    pub fn number(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        for (prefix, parser) in &mut [
            (
                tag("#b"),
                &mut Self::number_binary as &mut dyn Parser<'b, 'a, Value<'a>>,
            ),
            (tag("#o"), &mut Self::number_octal),
            (tag("#x"), &mut Self::number_hexdec),
            (tag("#d"), &mut Self::number_decimal),
        ] {
            match prefix(input) {
                Ok(_) => return parser.parse(input),
                Err(err) => {
                    if err.is_eof() {
                        return Err(err);
                    }
                }
            }
        }

        for parser in &mut [
            &mut Self::decimal as &mut dyn Parser<'b, 'a, Value<'a>>,
            &mut Self::infnan,
        ] {
            match parser.parse(input) {
                Ok((value, cursor)) => return Ok((value, cursor)),
                Err(err) => {
                    if err.is_semantic() || err.is_eof() {
                        return Err(err);
                    }
                }
            }
        }

        Err(ParseError::new(
            input,
            ERR_NUMBER_LITERAL + ECO,
            SexprError::NumberLiteral,
        ))
    }

    /// Parses a binary number literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// number_binary = "#b" ((sign [0-1]+) | infnan)
    /// sign          = ("+" | "-")?
    /// ```
    #[inline]
    pub fn number_binary(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        Self::number_radix(input, &RADIX[0])
    }

    /// Parses an octal number literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// number_octal = "#o" ((sign [0-7]+) | infnan)
    /// sign         = ("+" | "-")?
    /// ```
    #[inline]
    pub fn number_octal(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        Self::number_radix(input, &RADIX[1])
    }

    /// Parses a hexadecimal number literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// number_hexdec = "#x" ((sign [0-9a-fA-F]+) | infnan)
    /// sign          = ("+" | "-")?
    /// ```
    #[inline]
    pub fn number_hexdec(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        Self::number_radix(input, &RADIX[2])
    }

    /// Parses a decimal number literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// number_decimal = "#d" (decimal | infnan)
    /// ```
    pub fn number_decimal(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        let (_, cursor) = tag("#d")(input).map_err(|mut err| {
            let span = err.span();

            err.push(span, ERR_DECIMAL_LITERAL + ECO, SexprError::DecimalLiteral);
            err
        })?;

        if let Ok((value, cursor)) = Self::infnan(cursor) {
            Ok((value, cursor))
        } else {
            Self::decimal(cursor)
        }
    }

    /// Parses a decimal literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// decimal          = sign (([0-9]+ (decimal_fraction | decimal_suffix)?)
    ///                          | decimal_fraction)
    /// decimal_fraction = "." [0-9]+ decimal_suffix?
    /// decimal_suffix   = [eE] sign [0-9]+
    /// sign             = ("+" | "-")?
    /// ```
    pub fn decimal(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        let (_, cursor) = Self::sign(input)?;

        if let Ok((_, cursor)) = ascii::digit1(cursor) {
            match cursor.current() {
                Some('.') => {
                    let (_, cursor) = Self::decimal_fraction(cursor)?;
                    let (num, cursor) = Self::convert2float(input, cursor)?;

                    Ok((Value::new_float(num), cursor))
                }
                Some('e') | Some('E') => {
                    let (_, cursor) = Self::decimal_suffix(cursor)?;
                    let (num, cursor) = Self::convert2float(input, cursor)?;

                    Ok((Value::new_float(num), cursor))
                }
                Some(_) | None => {
                    let (num, cursor) = Self::convert2int(input, cursor)?;

                    Ok((Value::new_int(num), cursor))
                }
            }
        } else if let Some('.') = cursor.current() {
            let (_, cursor) = Self::decimal_fraction(cursor)?;
            let (num, cursor) = Self::convert2float(input, cursor)?;

            Ok((Value::new_float(num), cursor))
        } else {
            Err(Self::error(
                input,
                cursor,
                ERR_DECIMAL_LITERAL + ECO,
                SexprError::DecimalLiteral,
            ))
        }
    }

    fn decimal_fraction(input: Input<'b>) -> ParseResult<'b, ()> {
        let (_, cursor) = pair(char('.'), ascii::digit1)(input).map_err(|mut err| {
            let span = err.span();

            err.push(span, ERR_FLOAT_FRACTION + ECO, SexprError::FloatFraction);
            err
        })?;

        match cursor.current() {
            Some('e') | Some('E') => Self::decimal_suffix(cursor),
            _ => Ok(((), cursor)),
        }
    }

    fn decimal_suffix(input: Input<'b>) -> ParseResult<'b, ()> {
        tuple((any((tag("e"), tag("E"))), Self::sign, ascii::digit1))(input)
            .map(|(_, cursor)| ((), cursor))
            .map_err(|mut err| {
                let span = err.span();

                err.push(span, ERR_FLOAT_SUFFIX + ECO, SexprError::FloatSuffix);
                err
            })
    }

    fn sign(input: Input<'b>) -> ParseResult<'b, ()> {
        opt(any((char('+'), char('-'))))(input).map(|(_, cursor)| ((), cursor))
    }

    /// Parses a floating-point infinite or NaN literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// infnan = "+inf.0" | "-inf.0" | "+nan.0" | "-nan.0"
    /// ```
    pub fn infnan(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        any((
            value(Value::new_float(f64::INFINITY), tag("+inf.0")),
            value(Value::new_float(f64::NEG_INFINITY), tag("-inf.0")),
            value(Value::new_float(f64::NAN), tag("+nan.0")),
            value(Value::new_float(f64::NAN), tag("-nan.0")),
        ))(input)
        .map_err(|mut err| {
            let span = err.span();

            err.push(span, ERR_FLOAT_INFNAN + ECO, SexprError::FloatInfNaN);
            err
        })
    }

    fn number_radix(input: Input<'b>, radix: &'static Radix) -> ParseResult<'b, Value<'a>> {
        let (prefix, code, message, base, predicate) = radix;
        let (_, cursor) = tag(prefix)(input).map_err(|mut err| {
            let span = err.span();

            err.push(span, code + ECO, message);
            err
        })?;

        if let Ok((value, cursor)) = Self::infnan(cursor) {
            Ok((value, cursor))
        } else {
            let (digits, cursor) = recognize(pair(Self::sign, take_while1(predicate)))(cursor)
                .map_err(|mut err| {
                    let span = err.span();

                    err.push(span, ERR_INTEGER_LITERAL + ECO, SexprError::IntegerLiteral);
                    err
                })?;
            let value = i64::from_str_radix(digits, *base).map_err(|_| {
                Self::error(
                    input,
                    cursor,
                    ERR_INTEGER_LITERAL + ECO,
                    SexprError::IntegerLiteral,
                )
                .and_semantic()
            })?;

            Ok((Value::new_int(value), cursor))
        }
    }

    fn convert2int(start: Input<'b>, end: Input<'b>) -> ParseResult<'b, i64> {
        let span = Span::new(start.position(), end.position());
        let value = start.as_str()[..span.len()].parse::<i64>().map_err(|_| {
            ParseError::new(span, ERR_INTEGER_LITERAL + ECO, SexprError::IntegerLiteral)
                .and_semantic()
        })?;

        Ok((value, end))
    }

    fn convert2float(start: Input<'b>, end: Input<'b>) -> ParseResult<'b, f64> {
        let span = Span::new(start.position(), end.position());
        let value = start.as_str()[..span.len()].parse::<f64>().map_err(|_| {
            ParseError::new(span, ERR_FLOAT_LITERAL + ECO, SexprError::FloatLiteral).and_semantic()
        })?;

        Ok((value, end))
    }

    fn error<M>(start: Input<'b>, end: Input<'b>, code: Code, message: M) -> ParseError
    where
        M: fmt::Display,
    {
        if start.is_eof() {
            ParseError::eof(start.position()).and(start.position(), code + ECO, message)
        } else {
            ParseError::new(
                Span::new(start.position(), end.position()),
                code + ECO,
                message,
            )
        }
    }
}

type Radix = (
    &'static str,
    Code,
    SexprError<'static>,
    u32,
    fn(char) -> bool,
);

const RADIX: &[Radix] = &[
    (
        "#b",
        ERR_BINARY_LITERAL,
        SexprError::BinaryLiteral,
        2,
        predicate::is_bin_digit,
    ),
    (
        "#o",
        ERR_OCTAL_LITERAL,
        SexprError::OctalLiteral,
        8,
        predicate::is_oct_digit,
    ),
    (
        "#x",
        ERR_HEXDEC_LITERAL,
        SexprError::HexdecLiteral,
        16,
        predicate::is_hex_digit,
    ),
];

#[cfg(test)]
mod tests {
    use crate::Position;

    use super::*;

    const DECIMAL_INTEGERS: &[(&str, i64)] = &[("+42", 42), ("-42", -42), ("42", 42)];
    const DECIMAL_FLOATS: &[(&str, f64)] = &[
        ("42e+10", 42e10),
        ("42e-10", 42e-10),
        ("42e10", 42e10),
        ("42E+10", 42e10),
        ("42E-10", 42e-10),
        ("42E10", 42e10),
        ("+42e+10", 42e10),
        ("+42e-10", 42e-10),
        ("+42e10", 42e10),
        ("+42E+10", 42e10),
        ("+42E-10", 42e-10),
        ("+42E10", 42e10),
        ("-42e+10", -42e10),
        ("-42e-10", -42e-10),
        ("-42e10", -42e10),
        ("-42E+10", -42e10),
        ("-42E-10", -42e-10),
        ("-42E10", -42e10),
        (".42e+10", 0.42e10),
        (".42e-10", 0.42e-10),
        ("10.42e+10", 10.42e10),
        ("10.42e-10", 10.42e-10),
        ("10.42e10", 10.42e10),
        ("10.42E+10", 10.42e10),
        ("10.42E-10", 10.42e-10),
        ("10.42E10", 10.42e10),
        ("10.42e+10", 10.42e10),
        ("+10.42e-10", 10.42e-10),
        ("+10.42e10", 10.42e10),
        ("+10.42E+10", 10.42e10),
        ("+10.42E-10", 10.42e-10),
        ("+10.42E10", 10.42e10),
        ("-10.42e+10", -10.42e10),
        ("-10.42e-10", -10.42e-10),
        ("-10.42e10", -10.42e10),
        ("-10.42E+10", -10.42e10),
        ("-10.42E-10", -10.42e-10),
        ("-10.42E10", -10.42e10),
    ];

    const NUMBER_INTEGERS: &[(&str, i64)] = &[
        ("#b+1010", 10),
        ("#b-1010", -10),
        ("#b1010", 10),
        ("#o+52", 42),
        ("#o-52", -42),
        ("#o52", 42),
        ("#x+2A", 42),
        ("#x-2A", -42),
        ("#x2a", 42),
        ("#d+42", 42),
        ("#d-42", -42),
        ("#d42", 42),
    ];
    const NUMBER_FLOATS: &[(&str, f64)] = &[
        ("#b+inf.0", f64::INFINITY),
        ("#b-inf.0", f64::NEG_INFINITY),
        ("#b+nan.0", f64::NAN),
        ("#b-nan.0", f64::NAN),
        ("#o+inf.0", f64::INFINITY),
        ("#o-inf.0", f64::NEG_INFINITY),
        ("#o+nan.0", f64::NAN),
        ("#o-nan.0", f64::NAN),
        ("#d+inf.0", f64::INFINITY),
        ("#d-inf.0", f64::NEG_INFINITY),
        ("#d+nan.0", f64::NAN),
        ("#d-nan.0", f64::NAN),
        ("#x+inf.0", f64::INFINITY),
        ("#x-inf.0", f64::NEG_INFINITY),
        ("#x+nan.0", f64::NAN),
        ("#x-nan.0", f64::NAN),
        ("+inf.0", f64::INFINITY),
        ("-inf.0", f64::NEG_INFINITY),
        ("+nan.0", f64::NAN),
        ("-nan.0", f64::NAN),
        ("#d42e+10", 42e10),
        ("#d42e-10", 42e-10),
        ("#d42e10", 42e10),
        ("#d42E+10", 42e10),
        ("#d42E-10", 42e-10),
        ("#d42E10", 42e10),
        ("#d+42e+10", 42e10),
        ("#d+42e-10", 42e-10),
        ("#d+42e10", 42e10),
        ("#d+42E+10", 42e10),
        ("#d+42E-10", 42e-10),
        ("#d+42E10", 42e10),
        ("#d-42e+10", -42e10),
        ("#d-42e-10", -42e-10),
        ("#d-42e10", -42e10),
        ("#d-42E+10", -42e10),
        ("#d-42E-10", -42e-10),
        ("#d-42E10", -42e10),
        ("#d.42e+10", 0.42e10),
        ("#d.42e-10", 0.42e-10),
        ("#d10.42e+10", 10.42e10),
        ("#d10.42e-10", 10.42e-10),
        ("#d10.42e10", 10.42e10),
        ("#d10.42E+10", 10.42e10),
        ("#d10.42E-10", 10.42e-10),
        ("#d10.42E10", 10.42e10),
        ("#d10.42e+10", 10.42e10),
        ("#d+10.42e-10", 10.42e-10),
        ("#d+10.42e10", 10.42e10),
        ("#d+10.42E+10", 10.42e10),
        ("#d+10.42E-10", 10.42e-10),
        ("#d+10.42E10", 10.42e10),
        ("#d-10.42e+10", -10.42e10),
        ("#d-10.42e-10", -10.42e-10),
        ("#d-10.42e10", -10.42e10),
        ("#d-10.42E+10", -10.42e10),
        ("#d-10.42E-10", -10.42e-10),
        ("#d-10.42E10", -10.42e10),
    ];

    #[test]
    fn number_success() {
        let parse = Sexpr::<0>::number;

        for (i, (input, expected)) in DECIMAL_INTEGERS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_int(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "decimal integer {} failed",
                i + 1
            );
        }

        for (i, (input, expected)) in DECIMAL_FLOATS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_float(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "decimal float {} failed",
                i + 1
            );
        }

        for (i, (input, expected)) in NUMBER_INTEGERS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_int(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "number integer {} failed",
                i + 1
            );
        }

        for (i, (input, expected)) in NUMBER_FLOATS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_float(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "number float {} failed",
                i + 1
            );
        }
    }

    #[test]
    fn decimal_success() {
        let parse = Sexpr::<0>::decimal;

        for (i, (input, expected)) in DECIMAL_INTEGERS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_int(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "integer {} failed",
                i + 1
            );
        }

        for (i, (input, expected)) in DECIMAL_FLOATS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Value::new_float(*expected);
            let result = parse(input);

            assert_eq!(
                result,
                Ok((expected, Input::new(""))),
                "float {} failed",
                i + 1
            );
        }
    }

    #[test]
    fn decimal_failure() {
        let parse = |input| Sexpr::<0>::decimal(Input::new(input));

        assert_eq!(
            parse(""),
            Err(ParseError::new(
                Position::default(),
                ERR_DECIMAL_LITERAL,
                SexprError::DecimalLiteral
            ))
        );

        assert_eq!(
            parse("hello"),
            Err(ParseError::new(
                Position::default(),
                ERR_DECIMAL_LITERAL,
                SexprError::DecimalLiteral
            ))
        );

        assert_eq!(
            parse("1.e"),
            Err(ParseError::new(
                Position::new(2, 1, 3),
                ERR_FLOAT_FRACTION,
                SexprError::FloatFraction
            ))
        );

        assert_eq!(
            parse("1.0e"),
            Err(ParseError::new(
                Position::new(4, 1, 5),
                ERR_FLOAT_SUFFIX,
                SexprError::FloatSuffix
            ))
        );

        assert_eq!(
            parse("1.0e+"),
            Err(ParseError::new(
                Position::new(5, 1, 6),
                ERR_FLOAT_SUFFIX,
                SexprError::FloatSuffix
            ))
        );

        assert_eq!(
            parse("10000000000000000000000000000000000000000000000000000000000"),
            Err(ParseError::new(
                Span::new(Position::default(), Position::new(59, 1, 60)),
                ERR_INTEGER_LITERAL,
                SexprError::IntegerLiteral
            ))
        );
    }
}
