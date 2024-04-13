use std::fs::read_to_string;

use pest::Parser;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::sexpr::{
    emitter::emit_datum,
    error::Error,
    parser::{Rule, SExpr},
};

/// Expands the `sexpr!` macro.
#[allow(clippy::single_call_fn)]
#[inline]
pub fn sexpr(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let pairs = SExpr::parse(Rule::sexpr, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut pairs = pairs.into_iter();
    let mut out = TokenStream::new();

    // First pair is the datum
    if let Some(pair) = pairs.next() {
        if let Err(err) = emit_datum(sexpr.mutator, pair, &mut out) {
            return syn::Error::new(sexpr.input.span(), err).to_compile_error();
        }
    } else {
        return syn::Error::new(sexpr.input.span(), Error::EmptySExpr).to_compile_error();
    }

    // Second pair is the end of input
    if let Some(pair) = pairs.next() {
        if pair.as_rule() != Rule::EOI {
            return syn::Error::new(sexpr.input.span(), Error::ExpectedEndOfInput)
                .to_compile_error();
        }
    } else {
        return syn::Error::new(sexpr.input.span(), Error::ExpectedEndOfInput).to_compile_error();
    }

    out
}

/// Expands the `sexpr_file!` macro.
#[allow(clippy::single_call_fn, clippy::wildcard_enum_match_arm)]
#[inline]
pub fn sexpr_file(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let input = match read_to_string(input) {
        Ok(input) => input,
        Err(err) => return syn::Error::new(sexpr.input.span(), err).to_compile_error(),
    };
    let pairs = SExpr::parse(Rule::script, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut exprs = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::shadowed => continue,
            Rule::EOI => break,
            _ => {
                let mut datum = TokenStream::new();

                if let Err(err) = emit_datum(sexpr.mutator.clone(), pair, &mut datum) {
                    return syn::Error::new(sexpr.input.span(), err).to_compile_error();
                }
                exprs.push(datum);
            }
        }
    }
    quote! { [#(#exprs),*] }
}

/// Expands the `sexpr_script!` macro.
#[allow(clippy::single_call_fn, clippy::wildcard_enum_match_arm)]
#[inline]
pub fn sexpr_script(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let pairs = SExpr::parse(Rule::script, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut exprs = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::shadowed => continue,
            Rule::EOI => break,
            _ => {
                let mut datum = TokenStream::new();

                if let Err(err) = emit_datum(sexpr.mutator.clone(), pair, &mut datum) {
                    return syn::Error::new(sexpr.input.span(), err).to_compile_error();
                }
                exprs.push(datum);
            }
        }
    }
    quote! { [#(#exprs),*] }
}

struct SExprMacro {
    mutator: Option<syn::Ident>,
    input: syn::LitStr,
}

impl Parse for SExprMacro {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mutator = if input.peek(syn::Ident) && input.peek2(Token![,]) {
            let mutator = Some(input.parse::<syn::Ident>()?);

            input.parse::<Token![,]>()?;
            mutator
        } else {
            None
        };
        let input = input.parse::<syn::LitStr>()?;

        Ok(Self { mutator, input })
    }
}

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn sexpr_success() {
        let sexprs = [
            (
                quote! { m, "#(1)" },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, " #( 1 ) " },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, r#"
                ; line comment
                #( 1 #| value comment |# ) ; inline comment
                #| block comment |#
                ; last line comment"# },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, r#"
                #| ; line comment
                #| nested comment |#
                |#
                #( 1 )
                "# },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().cloned().enumerate() {
            let i = i + 1;
            let output = sexpr(input);

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr #{i} failed"
            );
        }
    }

    #[test]
    fn sexpr_file_success() {
        let sexprs = [
            (quote! { m, "tests/sexpr/empty.scm" }, quote! { [] }),
            (
                quote! { m, "tests/sexpr/values.scm" },
                quote! { [Value::new_nil(), Value::new_int(100i64), Value::new_bool(true)] },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().cloned().enumerate() {
            let i = i + 1;
            let output = sexpr_file(input);

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr_file #{i} failed"
            );
        }
    }

    #[test]
    fn sexpr_script_success() {
        let sexprs = [
            (
                quote! { m, r#"
                ; line comment
                #( 1 #| value comment |# ) ; inline comment
                #| block comment |#
                ; last line comment"# },
                quote! { [Value::new_vector(m.clone(), vec![Value::new_int(1i64)])] },
            ),
            (
                quote! { m, r#"
                #| ; line comment
                #| nested comment |#
                |#
                #( 1 )
                "# },
                quote! { [Value::new_vector(m.clone(), vec![Value::new_int(1i64)])] },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().cloned().enumerate() {
            let i = i + 1;
            let output = sexpr_script(input);

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr_script #{i} failed"
            );
        }
    }
}
