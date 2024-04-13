use pest::iterators::Pair;
use proc_macro2::TokenStream;
use quote::quote;

use crate::sexpr::{error::Error, parser::Rule};

/// Emit a character value.
#[allow(
    clippy::single_call_fn,
    clippy::unwrap_used,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm
)]
#[inline]
pub fn emit_character<'a>(pair: Pair<'a, Rule>, out: &mut TokenStream) -> Result<(), Error<'a>> {
    if pair.as_rule() == Rule::character {
        let mut pairs = pair.into_inner();
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::character_any => {
                let c = pair.as_str().chars().next().unwrap();
                out.extend(quote! { Value::new_char(#c) });
            }
            Rule::character_name => {
                let c = match pair.as_str() {
                    "alarm" => '\x07',
                    "backspace" => '\x08',
                    "delete" => '\x7f',
                    "escape" => '\x1b',
                    "newline" => '\n',
                    "null" => '\0',
                    "return" => '\r',
                    "space" => ' ',
                    "tab" => '\t',
                    _ => unreachable!(),
                };
                out.extend(quote! { Value::new_char(#c) });
            }
            Rule::character_code => {
                let code = pair.as_str();
                let code = u32::from_str_radix(code, 16).unwrap();
                let c = char::from_u32(code)
                    .ok_or_else(|| Error::InvalidCodePoint(pair.as_span(), code))?;
                out.extend(quote! { Value::new_char(#c) });
            }
            _ => unreachable!(),
        }
        if pairs.next().is_some() {
            return Err(Error::ExpectedEndOfExpression(pair.as_span()));
        }
        Ok(())
    } else {
        Err(Error::ExpectedCharacter(pair.as_span()))
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
    fn emit_character_success() {
        let exprs = [
            ("#\\a", quote! { Value::new_char('a') }),
            ("#\\A", quote! { Value::new_char('A') }),
            ("#\\alarm", quote! { Value::new_char('\u{7}') }),
            ("#\\backspace", quote! { Value::new_char('\u{8}') }),
            ("#\\delete", quote! { Value::new_char('\u{7f}') }),
            ("#\\escape", quote! { Value::new_char('\u{1b}') }),
            ("#\\newline", quote! { Value::new_char('\n') }),
            ("#\\null", quote! { Value::new_char('\0') }),
            ("#\\return", quote! { Value::new_char('\r') }),
            ("#\\space", quote! { Value::new_char(' ') }),
            ("#\\tab", quote! { Value::new_char('\t') }),
            ("#\\x41", quote! { Value::new_char('A') }),
            ("#\\x1f4af", quote! { Value::new_char('💯') }),
            ("#\\x7f", quote! { Value::new_char('\u{7f}') }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::character, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                emit_character(pair, &mut out).unwrap();
                assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
            }
        }
    }

    #[test]
    fn emit_character_failure() {
        let exprs = ["10", "hello"];

        for (i, input) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::datum, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };

            for pair in pairs {
                let mut out = TokenStream::new();

                assert!(emit_character(pair, &mut out).is_err(), "expr value {i}");
            }
        }
    }
}
