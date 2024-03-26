use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{
    code::{ERR_VECTOR_CLOSING, ERR_VECTOR_LITERAL},
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a vector literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// vector = "#(" intertoken (datum intertoken)* ")"
    /// ```
    pub fn vector(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            map(
                delimited(
                    terminated(tag("#("), Self::intertoken),
                    many0(terminated(self.datum(), Self::intertoken)),
                    tag(")"),
                ),
                |items| m.borrow_mut().new_vector(items).into(),
            )(input)
            .map_err(|mut err| {
                let span = err.span();

                match err.code() {
                    code::ERR_PRECEDED => {
                        err.push(span, ERR_VECTOR_LITERAL + ECO, SexprError::VectorClosing)
                    }
                    code::ERR_TERMINATED => {
                        err.push(span, ERR_VECTOR_CLOSING + ECO, SexprError::VectorClosing)
                    }
                    _ => (),
                }
                err
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use kamo_macros::sexpr;

    use crate::mem::Mutator;

    use super::*;

    #[test]
    fn vector_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.to_owned());
        let parse = sexpr.vector();

        assert_eq!(
            parse(Input::new("#(1 2 3)")),
            Ok((sexpr!(m, "#(1 2 3)"), Input::from(""))),
        );
    }
}
