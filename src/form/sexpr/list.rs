use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{
    code::{ERR_LIST_CLOSING, ERR_LIST_LITERAL},
    PositionMap, Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a list literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// list = "(" intertoken
    ///            ((datum intertoken)+ ("." intertoken datum)?)?
    ///        intertoken ")"
    /// ```
    pub fn list(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        move |input| {
            if let Some(pmap) = self.pmap.as_ref() {
                self.tracked_list(input, &mut pmap.borrow_mut())
            } else {
                self.untracked_list(input)
            }
            .map_err(|mut err| {
                let span = err.span();

                match err.code() {
                    code::ERR_PRECEDED => {
                        err.push(span, ERR_LIST_LITERAL + ECO, SexprError::ListLiteral)
                    }
                    code::ERR_TERMINATED => {
                        err.push(span, ERR_LIST_CLOSING + ECO, SexprError::ListClosing)
                    }
                    _ => (),
                }
                err
            })
        }
    }

    fn untracked_list(&self, input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        let m = self.m.to_owned();

        let items = many1(terminated(self.datum(), Self::intertoken));
        let dotted = preceded(terminated(char('.'), Self::intertoken), self.datum());
        let items = pair(items, opt(dotted));
        let items = delimited(terminated(tag("("), Self::intertoken), opt(items), tag(")"))(input)?;

        match items {
            (Some((items, Some(dotted))), cursor) => Ok((
                Value::new_dotlist(m.to_owned(), items, Some(dotted)),
                cursor,
            )),
            (Some((items, None)), cursor) => Ok((Value::new_list(m.to_owned(), items), cursor)),
            (None, cursor) => Ok((Value::new_nil(), cursor)),
        }
    }

    fn tracked_list(&self, input: Input<'b>, pmap: &mut PositionMap) -> ParseResult<'b, Value<'a>> {
        let m = self.m.to_owned();

        let item = map2(self.datum(), |item, input| {
            pmap.insert(item.id(), input.position());
            item
        });
        let items = many1(terminated(item, Self::intertoken));
        let dotted = map2(self.datum(), |item, input| (item, input.position()));
        let dotted = preceded(terminated(char('.'), Self::intertoken), dotted);
        let items = pair(items, opt(dotted));
        let items = delimited(terminated(tag("("), Self::intertoken), opt(items), tag(")"))(input)?;

        match items {
            (Some((items, Some((dotted, pos)))), cursor) => {
                pmap.insert(dotted.id(), pos);
                Ok((
                    Value::new_dotlist(m.to_owned(), items, Some(dotted)),
                    cursor,
                ))
            }
            (Some((items, None)), cursor) => Ok((Value::new_list(m.to_owned(), items), cursor)),
            (None, cursor) => Ok((Value::new_nil(), cursor)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use kamo_macros::sexpr;

    use crate::{mem::Mutator, value::ValueId, Position};

    use super::*;

    #[test]
    fn test_list() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.to_owned());
        let parse = sexpr.list();

        assert_eq!(
            parse(Input::new("(1 2 3)")),
            Ok((sexpr!(m, "(1 2 3)"), Input::from(""))),
        );

        let size = std::mem::size_of::<Option<BTreeMap<ValueId, Position>>>();
        println!("size: {}", size);
        let size = std::mem::size_of::<Sexpr<0>>();
        println!("size: {}", size);
    }
}
