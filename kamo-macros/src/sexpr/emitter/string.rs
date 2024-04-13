use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

use super::helper;

/// Emit a string value.
#[allow(
    clippy::single_call_fn,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm
)]
#[inline]
pub fn emit_string<'a>(
    mutator: &syn::Ident,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::string {
        let string = pair.as_str();

        if string.is_empty() || string == r#""""# {
            out.extend(quote! { Value::new_string(#mutator.clone(), "") });
            return Ok(());
        }

        let pairs = pair.into_inner();

        if pairs.peek().is_some() {
            let mut string = String::with_capacity(string.len());

            for pair in pairs {
                match pair.as_rule() {
                    Rule::string_text => string.push_str(pair.as_str()),
                    Rule::string_escape => {
                        if let Some(c) = helper::get_escape(pair)? {
                            string.push(c);
                        }
                    }
                    _ => unreachable!(),
                }
            }
            out.extend(quote! { Value::new_string(#mutator.clone(), #string) });
        } else {
            out.extend(quote! { Value::new_string(#mutator.clone(), #string) });
        }
        Ok(())
    } else {
        Err(Error::ExpectedString(pair.as_span()))
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
    fn emit_string_success() {
        let exprs = [
            (
                r#""hello""#,
                quote! { Value::new_string(m.clone(), "hello") },
            ),
            (
                r#""\"hello\"""#,
                quote! { Value::new_string(m.clone(), "\"hello\"") },
            ),
            (
                r#""hello\\""#,
                quote! { Value::new_string(m.clone(), "hello\\") },
            ),
            (
                r#""hello\a""#,
                quote! { Value::new_string(m.clone(), "hello\u{7}") },
            ),
            (
                r#""hello\b""#,
                quote! { Value::new_string(m.clone(), "hello\u{8}") },
            ),
            (
                r#""hello\t""#,
                quote! { Value::new_string(m.clone(), "hello\t") },
            ),
            (
                r#""hello\n""#,
                quote! { Value::new_string(m.clone(), "hello\n") },
            ),
            (
                r#""hello\r""#,
                quote! { Value::new_string(m.clone(), "hello\r") },
            ),
            (
                r#""hello \ 
                    world""#,
                quote! { Value::new_string(m.clone(), "hello world") },
            ),
        ];

        for (input, expected) in exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();
            emit_string(&syn::Ident::new("m", Span::call_site()), pair, &mut out).unwrap();
            assert_eq!(out.to_string(), expected.to_string());
        }
    }

    #[test]
    fn emit_string_failure() {
        let exprs = ["10", "hello"];

        for input in &exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();
            assert!(emit_string(&syn::Ident::new("m", Span::call_site()), pair, &mut out).is_err());
        }
    }
}
