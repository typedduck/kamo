use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

/// Emit a boolean value.
#[allow(clippy::single_call_fn, clippy::unreachable)]
#[inline]
pub fn emit_boolean<'a>(pair: &Pair<'a, Rule>, out: &mut TokenStream) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::boolean {
        match pair.as_str() {
            "#t" | "#true" => out.extend(quote! { Value::new_bool(true) }),
            "#f" | "#false" => out.extend(quote! { Value::new_bool(false) }),
            _ => unreachable!(),
        }
        Ok(())
    } else {
        Err(Error::ExpectedBoolean(pair.as_span()))
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
    fn emit_boolean_success() {
        let exprs = [
            ("#t", quote! { Value::new_bool(true) }),
            ("#f", quote! { Value::new_bool(false) }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::boolean, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                emit_boolean(&pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_boolean_failure() {
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

                assert!(emit_boolean(&pair, &mut out).is_err(), "expr value {i}");
            }
        }
    }
}
