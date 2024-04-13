use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{emitter::helper, error::Error, parser::Rule};

/// Emit a symbol value.
#[allow(
    clippy::single_call_fn,
    clippy::unwrap_used,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm
)]
pub fn emit_symbol<'a>(
    mutator: &syn::Ident,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::symbol {
        let symbol = pair.as_str();

        if symbol == "||" {
            out.extend(quote! { Value::new_symbol(#mutator.clone(), "") });
            return Ok(());
        }

        let pairs = pair.into_inner();

        if pairs.peek().is_some() {
            let mut symbol = String::with_capacity(symbol.len());

            for pair in pairs {
                match pair.as_rule() {
                    Rule::symbol_text => symbol.push_str(pair.as_str()),
                    Rule::symbol_escape => {
                        symbol.push(helper::get_escape(pair)?.unwrap());
                    }
                    _ => unreachable!(),
                }
            }
            out.extend(quote! { Value::new_symbol(#mutator.clone(), #symbol) });
        } else {
            out.extend(quote! { Value::new_symbol(#mutator.clone(), #symbol) });
        }
        Ok(())
    } else {
        Err(Error::ExpectedSymbol(pair.as_span()))
    }
}

#[allow(clippy::unwrap_used, clippy::panic)]
#[cfg(test)]
mod tests {
    use pest::Parser;
    use proc_macro2::Span;
    use quote::quote;

    use crate::sexpr::parser::SExpr;

    use super::*;

    #[test]
    fn emit_symbol_success() {
        let exprs = [
            ("hello", quote! { Value::new_symbol(m.clone(), "hello") }),
            ("...", quote! { Value::new_symbol(m.clone(), "...") }),
            ("+", quote! { Value::new_symbol(m.clone(), "+") }),
            ("+soup+", quote! { Value::new_symbol(m.clone(), "+soup+") }),
            ("<=?", quote! { Value::new_symbol(m.clone(), "<=?") }),
            (
                "->string",
                quote! { Value::new_symbol(m.clone(), "->string") },
            ),
            (
                "a34kTMNs",
                quote! { Value::new_symbol(m.clone(), "a34kTMNs") },
            ),
            ("lambda", quote! { Value::new_symbol(m.clone(), "lambda") }),
            (
                "list->vector",
                quote! { Value::new_symbol(m.clone(), "list->vector") },
            ),
            ("q", quote! { Value::new_symbol(m.clone(), "q") }),
            ("V17a", quote! { Value::new_symbol(m.clone(), "V17a") }),
            (
                "the-word-recursion-has-many-meanings",
                quote! { Value::new_symbol(m.clone(), "the-word-recursion-has-many-meanings") },
            ),
            ("||", quote! { Value::new_symbol(m.clone(), "") }),
            (
                "|two words|",
                quote! { Value::new_symbol(m.clone(), "two words") },
            ),
            (
                r"|two\x20;words|",
                quote! { Value::new_symbol(m.clone(), "two words") },
            ),
            (
                r"|two\twords|",
                quote! { Value::new_symbol(m.clone(), "two\twords") },
            ),
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

                emit_symbol(&syn::Ident::new("m", Span::call_site()), pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_symbol_failure() {
        let exprs = [r#""hello""#, "#t", "'a'", "100"];

        for (i, input) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::datum, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                assert!(
                    emit_symbol(&syn::Ident::new("m", Span::call_site()), pair, &mut out).is_err(),
                    "expr value {i}"
                );
            }
        }
    }
}
