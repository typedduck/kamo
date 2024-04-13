use pest::iterators::Pair;
use proc_macro2::TokenStream;

use crate::sexpr::{error::Error, parser::Rule};

use super::{
    emit_abbrev, emit_boolean, emit_bytevector, emit_character, emit_list, emit_number,
    emit_string, emit_symbol, emit_vector,
};

/// Emit a datum.
#[allow(clippy::unreachable, clippy::wildcard_enum_match_arm)]
pub fn emit_datum<'a>(
    mutator: Option<syn::Ident>,
    pair: Pair<'a, Rule>,
    out: &mut TokenStream,
) -> Result<(), Error<'a>> {
    match pair.as_rule() {
        Rule::abbrev => {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            emit_abbrev(&mutator, pair, out)
        }
        Rule::boolean => emit_boolean(&pair, out),
        Rule::bytevector => {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            emit_bytevector(&mutator, pair, out)
        }
        Rule::character => emit_character(pair, out),
        Rule::list => emit_list(mutator, pair, out),
        Rule::number => emit_number(pair, out).map(|_| ()),
        Rule::string => {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            emit_string(&mutator, pair, out)
        }
        Rule::symbol => {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            emit_symbol(&mutator, pair, out)
        }
        Rule::vector => {
            let mutator = mutator.ok_or(Error::MutatorRequired(pair.as_span()))?;
            emit_vector(&mutator, pair, out)
        }
        _ => unreachable!("unimplemented sexpr: {:?}", pair.as_rule()),
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
    fn emit_sexpr_success() {
        let exprs = [
            ("#t", quote! { Value::new_bool(true) }),
            ("#f", quote! { Value::new_bool(false) }),
        ];

        for (i, (input, expected)) in exprs.into_iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::sexpr, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {i}: {e}"),
            };
            let mut out = TokenStream::new();

            emit_datum(None, pairs.into_iter().next().unwrap(), &mut out).unwrap();
            assert_eq!(out.to_string(), expected.to_string(), "expr value {i}");
        }
    }
}
