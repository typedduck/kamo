use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

use super::Number;

/// Emit a decimal value.
pub fn emit_decimal<'a>(pair: &Pair<'a, Rule>, out: &mut TokenStream) -> Result<Number, Error<'a>> {
    if pair.as_rule() == Rule::decimal {
        if let Ok(value) = pair.as_str().parse::<i64>() {
            out.extend(quote! { Value::new_int(#value) });
            Ok(Number::Integer(value))
        } else if let Ok(value) = pair.as_str().parse::<f64>() {
            out.extend(quote! { Value::new_float(#value) });
            if value.is_infinite() {
                Ok(Number::Infinty)
            } else if value.is_nan() {
                Ok(Number::NaN)
            } else {
                Ok(Number::Float(value))
            }
        } else {
            Err(Error::InvalidDecimal(
                pair.as_span(),
                pair.as_str().to_owned(),
            ))
        }
    } else {
        Err(Error::ExpectedDecimal(pair.as_span()))
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
    fn emit_decimal_success() {
        let exprs = [
            ("+10", quote! { Value::new_int(10i64) }),
            ("-10", quote! { Value::new_int(-10i64) }),
            ("+10.0", quote! { Value::new_float(10f64) }),
            ("-10.0", quote! { Value::new_float(-10f64) }),
            ("10.42e+10", quote! { Value::new_float(104200000000f64) }),
            ("10.0", quote! { Value::new_float(10f64) }),
            ("10", quote! { Value::new_int(10i64) }),
            ("10e4", quote! { Value::new_float(100000f64) }),
            (".5", quote! { Value::new_float(0.5f64) }),
            (".5E-3", quote! { Value::new_float(0.0005f64) }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::decimal, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                emit_decimal(&pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_decimal_failure() {
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

                assert!(emit_decimal(&pair, &mut out).is_err(), "expr value {i}");
            }
        }
    }
}
