use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{
    emitter::{emit_decimal, emit_infnan},
    error::Error,
    parser::Rule,
};

use super::Number;

/// Emit a number value.
#[allow(
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::wildcard_enum_match_arm,
    clippy::map_err_ignore
)]
pub fn emit_number<'a>(pair: Pair<'a, Rule>, out: &mut TokenStream) -> Result<Number, Error<'a>> {
    if pair.as_rule() == Rule::number {
        // Descend into the number_*, decimal or infnan rules
        let mut pairs = pair.into_inner();
        let pair = pairs.next().unwrap();

        if pairs.next().is_some() {
            return Err(Error::ExpectedEndOfExpression(pair.as_span()));
        }
        match pair.as_rule() {
            Rule::decimal => return emit_decimal(&pair, out),
            Rule::infnan => return emit_infnan(&pair, out),
            Rule::number_binary
            | Rule::number_decimal
            | Rule::number_hexdec
            | Rule::number_octal => {
                // Descend into the number_*_digits, decimal or infnan rules
                let mut pairs = pair.into_inner();
                let pair = pairs.next().unwrap();

                let value =
                    match pair.as_rule() {
                        Rule::decimal => return emit_decimal(&pair, out),
                        Rule::infnan => return emit_infnan(&pair, out),
                        Rule::number_binary_digits => i64::from_str_radix(pair.as_str(), 2)
                            .map_err(|_| {
                                Error::InvalidBinary(pair.as_span(), pair.as_str().to_owned())
                            })?,
                        Rule::number_octal_digits => i64::from_str_radix(pair.as_str(), 8)
                            .map_err(|_| {
                                Error::InvalidOctal(pair.as_span(), pair.as_str().to_owned())
                            })?,
                        Rule::number_hexdec_digits => i64::from_str_radix(pair.as_str(), 16)
                            .map_err(|_| {
                                Error::InvalidHexadecimal(pair.as_span(), pair.as_str().to_owned())
                            })?,
                        _ => unreachable!(),
                    };

                out.extend(quote! { Value::new_int(#value) });
                if pairs.next().is_some() {
                    return Err(Error::ExpectedEndOfExpression(pair.as_span()));
                }
                Ok(Number::Integer(value))
            }
            _ => unreachable!(),
        }
    } else {
        Err(Error::ExpectedNumber(pair.as_span()))
    }
}

#[allow(clippy::unwrap_used, clippy::panic)]
#[cfg(test)]
mod tests {
    use pest::Parser;
    use quote::quote;

    use crate::sexpr::parser::SExpr;

    use super::*;

    #[test]
    fn emit_number_success() {
        let exprs = [
            ("10", quote! { Value::new_int(10i64) }),
            ("10.0", quote! { Value::new_float(10f64) }),
            ("+10.0", quote! { Value::new_float(10f64) }),
            ("-10.0", quote! { Value::new_float(-10f64) }),
            ("+inf.0", quote! { Value::new_float(f64::INFINITY) }),
            ("-inf.0", quote! { Value::new_float(f64::NEG_INFINITY) }),
            ("+nan.0", quote! { Value::new_float(f64::NAN) }),
            ("-nan.0", quote! { Value::new_float(f64::NAN) }),
            ("#b+0101", quote! { Value::new_int(5i64) }),
            ("#b-0101", quote! { Value::new_int(-5i64) }),
            ("#b0101", quote! { Value::new_int(5i64) }),
            ("#b+inf.0", quote! { Value::new_float(f64::INFINITY) }),
            ("#o+70", quote! { Value::new_int(56i64) }),
            ("#o-70", quote! { Value::new_int(-56i64) }),
            ("#o70", quote! { Value::new_int(56i64) }),
            ("#o+inf.0", quote! { Value::new_float(f64::INFINITY) }),
            ("#d+90", quote! { Value::new_int(90i64) }),
            ("#d-90", quote! { Value::new_int(-90i64) }),
            ("#d90", quote! { Value::new_int(90i64) }),
            ("#d+inf.0", quote! { Value::new_float(f64::INFINITY) }),
            ("#x+f0", quote! { Value::new_int(240i64) }),
            ("#x-F0", quote! { Value::new_int(-240i64) }),
            ("#xf0", quote! { Value::new_int(240i64) }),
            ("#x+inf.0", quote! { Value::new_float(f64::INFINITY) }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::datum, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                emit_number(pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_number_failure() {
        let exprs: [&str; 2] = ["#t", "#f"];

        for (i, input) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::datum, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                assert!(emit_number(pair, &mut out).is_err(), "expr value {i}");
            }
        }
    }
}
