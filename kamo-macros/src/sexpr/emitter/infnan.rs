use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

use super::Number;

/// Emit an infinite or NaN number.
#[allow(clippy::unreachable)]
pub fn emit_infnan<'a>(pair: &Pair<'a, Rule>, out: &mut TokenStream) -> Result<Number, Error<'a>> {
    if pair.as_rule() == Rule::infnan {
        match pair.as_str() {
            "+inf.0" => {
                out.extend(quote! { Value::new_float(f64::INFINITY) });
                Ok(Number::Infinty)
            }
            "-inf.0" => {
                out.extend(quote! { Value::new_float(f64::NEG_INFINITY) });
                Ok(Number::Infinty)
            }
            "+nan.0" | "-nan.0" => {
                out.extend(quote! { Value::new_float(f64::NAN) });
                Ok(Number::NaN)
            }
            _ => unreachable!(),
        }
    } else {
        Err(Error::ExpectedInfnan(pair.as_span()))
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
    fn emit_infnan_success() {
        let exprs = [
            ("+inf.0", quote! { Value::new_float(f64::INFINITY) }),
            ("-inf.0", quote! { Value::new_float(f64::NEG_INFINITY) }),
            ("+nan.0", quote! { Value::new_float(f64::NAN) }),
            ("-nan.0", quote! { Value::new_float(f64::NAN) }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::infnan, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                emit_infnan(&pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_infnan_failure() {
        let exprs: [&str; 2] = ["10", "hello"];

        for (i, input) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::datum, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                assert!(emit_infnan(&pair, &mut out).is_err(), "expr value {i}");
            }
        }
    }
}
