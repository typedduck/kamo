use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::sexpr::{error::Error, parser::Rule};

use super::{emit_number, Number};

/// Emits a bytevector.
#[allow(clippy::single_call_fn)]
#[inline]
pub fn emit_bytevector<'a>(
    mutator: &syn::Ident,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::bytevector {
        let pairs = pair.into_inner();

        if pairs.peek().is_some() {
            let mut bytes = Bytes::default();

            for pair in pairs {
                if pair.as_rule() != Rule::number {
                    return Err(Error::ExpectedNumber(pair.as_span()));
                }
                let mut bytes_out = TokenStream::new();
                // Conditions for casting are checked beforehand
                #[allow(clippy::cast_possible_truncation)]
                #[allow(clippy::cast_sign_loss)]
                #[allow(clippy::as_conversions)]
                let byte = match emit_number(pair.clone(), &mut bytes_out) {
                    Ok(Number::Integer(val)) if (0..=255).contains(&val) => val as u8,
                    Ok(num) => return Err(Error::ExpectedByte(pair.as_span(), num)),
                    Err(err) => return Err(err),
                };

                bytes.0.push(byte);
            }
            out.extend(quote! { Value::new_bytevec(#mutator.clone(), #bytes) });
        } else {
            out.extend(quote! { Value::new_bytevec(#mutator.clone(), &[]) });
        }
        Ok(())
    } else {
        Err(Error::ExpectedByteVector(pair.as_span()))
    }
}

#[derive(Default)]
struct Bytes(Vec<u8>);

impl ToTokens for Bytes {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let bytes = self.0.iter().map(|b| quote! { #b });
        tokens.extend(quote! { &[#(#bytes),*] });
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
    fn emit_bytevector_success() {
        let exprs = [
            ("#u8()", quote! { Value::new_bytevec(m.clone(), &[]) }),
            ("#u8( )", quote! { Value::new_bytevec(m.clone(), &[]) }),
            ("#u8(0)", quote! { Value::new_bytevec(m.clone(), &[0u8]) }),
            ("#u8( 0 )", quote! { Value::new_bytevec(m.clone(), &[0u8]) }),
            (
                "#u8(0 1 2 3 4 5 6 7 8 9)",
                quote! { Value::new_bytevec(m.clone(), &[0u8, 1u8, 2u8, 3u8, 4u8, 5u8, 6u8, 7u8, 8u8, 9u8]) },
            ),
        ];

        for (input, expected) in exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();
            emit_bytevector(&syn::Ident::new("m", Span::call_site()), pair, &mut out).unwrap();
            assert_eq!(out.to_string(), expected.to_string());
        }
    }

    #[test]
    fn emit_bytevector_failure() {
        let exprs = ["10", "hello"];

        for input in &exprs {
            let pair = SExpr::parse(Rule::datum, input).unwrap().next().unwrap();
            let mut out = TokenStream::new();
            assert!(
                emit_bytevector(&syn::Ident::new("m", Span::call_site()), pair, &mut out).is_err()
            );
        }
    }
}
