use pest::iterators::Pair;
use proc_macro2::{Span, TokenStream};
use quote::quote;

use crate::sexpr::{emitter::emit_datum, error::Error, parser::Rule};

/// Emit an abbreviated datum.
#[allow(clippy::single_call_fn, clippy::unwrap_used, clippy::unreachable)]
#[inline]
pub fn emit_abbrev<'a>(
    mutator: &syn::Ident,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::abbrev {
        let mut pairs = pair.into_inner();
        let abbrev = pairs.next().unwrap();
        let mut datum = TokenStream::new();
        let abbrev = match abbrev.as_str() {
            "'" => syn::LitStr::new("quote", Span::call_site()),
            "`" => syn::LitStr::new("quasiquote", Span::call_site()),
            "," => syn::LitStr::new("unquote", Span::call_site()),
            ",@" => syn::LitStr::new("unquote-splicing", Span::call_site()),
            _ => unreachable!("unimplemented abbreviation: {:?}", abbrev.as_str()),
        };

        emit_datum(Some(mutator.to_owned()), pairs.next().unwrap(), &mut datum)?;
        out.extend(quote! {
            Value::new_list(#mutator.clone(),
                vec![Value::new_symbol(#mutator.clone(), #abbrev), #datum])
        });
        Ok(())
    } else {
        Err(Error::ExpectedAbbrev(pair.as_span()))
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
    fn emit_abbrev_success() {
        let exprs = [
            (
                "'()",
                quote! { Value::new_list(m.clone(),
                vec![Value::new_symbol(m.clone(), "quote"), Value::new_nil()]) },
            ),
            (
                "''()",
                quote! { Value::new_list(m.clone(),
                    vec![Value::new_symbol(m.clone(), "quote"),
                    Value::new_list(m.clone(),
                vec![Value::new_symbol(m.clone(), "quote"), Value::new_nil()])]) },
            ),
            (
                ",()",
                quote! { Value::new_list(m.clone(),
                vec![Value::new_symbol(m.clone(), "unquote"), Value::new_nil()]) },
            ),
            (
                "`()",
                quote! { Value::new_list(m.clone(),
                vec![Value::new_symbol(m.clone(), "quasiquote"), Value::new_nil()]) },
            ),
            (
                ",@()",
                quote! { Value::new_list(m.clone(),
                vec![Value::new_symbol(m.clone(), "unquote-splicing"), Value::new_nil()]) },
            ),
        ];

        for (input, expected) in exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            emit_abbrev(&syn::Ident::new("m", Span::call_site()), pair, &mut out).unwrap();
            assert_eq!(out.to_string(), expected.to_string());
        }
    }

    #[test]
    fn emit_abbrev_failure() {
        let exprs = ["#t", "#f", "#\\a"];
        for input in &exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();

            assert!(emit_abbrev(&syn::Ident::new("m", Span::call_site()), pair, &mut out).is_err());
        }
    }
}
