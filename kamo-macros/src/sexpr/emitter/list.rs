use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

use super::emit_datum;

/// Emit a list.
#[allow(clippy::single_call_fn, clippy::unwrap_used)]
#[inline]
pub fn emit_list<'a>(
    mutator: Option<syn::Ident>,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::list {
        let mut pairs = pair.into_inner();

        if let Some(pair) = pairs.next() {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            let mut values = TokenStream::new();
            let mut dot_value = None;
            let mut seen_dot = false;

            emit_datum(Some(mutator.clone()), pair.clone(), &mut values)?;
            for pair in pairs {
                if seen_dot {
                    return Err(Error::ExpectedEndOfList(pair.as_span()));
                } else if pair.as_rule() == Rule::dotted {
                    let pair = pair.into_inner().next().unwrap();
                    let mut value = TokenStream::new();

                    emit_datum(Some(mutator.clone()), pair.clone(), &mut value)?;
                    dot_value = Some(value);
                    seen_dot = true;
                } else {
                    values.extend(quote! {, });
                    emit_datum(Some(mutator.clone()), pair.clone(), &mut values)?;
                }
            }
            if let Some(dot_value) = dot_value {
                out.extend(quote! { Value::new_dotlist(#mutator.clone(), vec![#values], Some(#dot_value)) });
            } else {
                out.extend(quote! { Value::new_list(#mutator.clone(), vec![#values]) });
            }
        } else {
            out.extend(quote! { Value::new_nil() });
        }
        Ok(())
    } else {
        Err(Error::ExpectedList(pair.as_span()))
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
    fn emit_list_success() {
        let exprs = [
            ("()", quote! { Value::new_nil() }),
            ("( )", quote! { Value::new_nil() }),
            (
                "(1)",
                quote! { Value::new_list(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                "( 1 )",
                quote! { Value::new_list(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                "(1 2)",
                quote! { Value::new_list(m.clone(), vec![Value::new_int(1i64), Value::new_int(2i64)]) },
            ),
            (
                "(1 . 2)",
                quote! { Value::new_dotlist(m.clone(),
                vec![Value::new_int(1i64)],
                Some(Value::new_int(2i64))) },
            ),
            (
                "(1 2 . 3)",
                quote! { Value::new_dotlist(m.clone(),
                vec![Value::new_int(1i64), Value::new_int(2i64)],
                Some(Value::new_int(3i64))) },
            ),
            (
                "(1 2 . (3))",
                quote! { Value::new_dotlist(m.clone(),
                vec![Value::new_int(1i64), Value::new_int(2i64)],
                Some(Value::new_list(m.clone(), vec![Value::new_int(3i64)]))) },
            ),
            (
                "((1 2) . (3 4))",
                quote! { Value::new_dotlist(m.clone(),
                vec![Value::new_list(m.clone(),
                vec![Value::new_int(1i64), Value::new_int(2i64)])],
                Some(Value::new_list(m.clone(),
                vec![Value::new_int(3i64), Value::new_int(4i64)]))) },
            ),
        ];

        for (input, expected) in exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            emit_list(
                Some(syn::Ident::new("m", Span::call_site())),
                pair,
                &mut out,
            )
            .unwrap();
            assert_eq!(out.to_string(), expected.to_string());
        }
    }

    #[test]
    fn emit_list_failure() {
        let exprs = ["#t", "#f", "#\\a"];
        for input in &exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            assert!(emit_list(
                Some(syn::Ident::new("m", Span::call_site())),
                pair,
                &mut out
            )
            .is_err());
        }
    }
}
