use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{emitter::emit_datum, error::Error, parser::Rule};

/// Emit a vector.
#[allow(clippy::single_call_fn)]
#[inline]
pub fn emit_vector<'a>(
    mutator: &syn::Ident,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::vector {
        let mut pairs = pair.into_inner();

        if let Some(pair) = pairs.next() {
            let mut values = TokenStream::new();

            emit_datum(Some(mutator.to_owned()), pair.clone(), &mut values)?;
            for pair in pairs {
                values.extend(quote! {, });
                emit_datum(Some(mutator.to_owned()), pair.clone(), &mut values)?;
            }
            out.extend(quote! { Value::new_vector(#mutator.clone(), vec![#values]) });
        } else {
            out.extend(quote! { Value::new_vector(#mutator.clone(), vec![]) });
        }
        Ok(())
    } else {
        Err(Error::ExpectedVector(pair.as_span()))
    }
}

#[allow(clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use pest::Parser;
    use proc_macro2::Span;
    use quote::quote;

    use crate::sexpr::parser::SExpr;

    use super::*;

    #[test]
    fn emit_vector_success() {
        let exprs = [
            ("#()", quote! { Value::new_vector(m.clone(), vec![]) }),
            ("#( )", quote! { Value::new_vector(m.clone(), vec![]) }),
            (
                "#(1)",
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                "#( 1 )",
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                "#(1 2)",
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64), Value::new_int(2i64)]) },
            ),
            (
                "#( 1 2 )",
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64), Value::new_int(2i64)]) },
            ),
            (
                "#(1 2 3)",
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64), Value::new_int(2i64), Value::new_int(3i64)]) },
            ),
        ];

        for (input, expected) in exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            emit_vector(&syn::Ident::new("m", Span::call_site()), pair, &mut out).unwrap();
            assert_eq!(out.to_string(), expected.to_string());
        }
    }

    #[test]
    fn emit_vector_failure() {
        let exprs = ["#t", "#f", "#\\a"];
        for input in &exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            assert!(emit_vector(&syn::Ident::new("m", Span::call_site()), pair, &mut out).is_err());
        }
    }
}
