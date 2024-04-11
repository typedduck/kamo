use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{
    code::{ERR_LIST_CLOSING, ERR_LIST_DOTTED, ERR_LIST_DOTTED_PRECEDED, ERR_LIST_LITERAL},
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
            self.pmap
                .as_ref()
                .map_or_else(
                    || self.untracked_list(input),
                    |pmap| self.tracked_list(input, &mut pmap.borrow_mut()),
                )
                .map_err(|mut err| {
                    let span = err.span();

                    match err.code() {
                        code::ERR_PRECEDED => {
                            err.push(span, ERR_LIST_LITERAL + ECO, SexprError::ListLiteral);
                        }
                        code::ERR_TERMINATED => {
                            err.push(span, ERR_LIST_CLOSING + ECO, SexprError::ListClosing);
                        }
                        _ => (),
                    }
                    err
                })
        }
    }

    fn untracked_list(&self, input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        let m = self.m.clone();

        let items = many1(terminated(self.datum(), Self::intertoken));
        let dotted = preceded(terminated(char('.'), Self::intertoken), cut(self.datum()));
        let items = map_err(pair(opt(items), opt(dotted)), |mut err| {
            if err.code() == code::ERR_PAIR_SECOND {
                let span = err.span();
                err.push(span, ERR_LIST_DOTTED + ECO, SexprError::ListDotted);
            }
            err
        });
        let items = delimited(terminated(tag("("), Self::intertoken), items, tag(")"))(input)?;

        match items {
            ((Some(items), Some(dotted)), cursor) => {
                Ok((Value::new_dotlist(m, items, Some(dotted)), cursor))
            }
            ((Some(items), None), cursor) => Ok((Value::new_list(m, items), cursor)),
            ((None, Some(_)), _) => Err(ParseError::new(
                input,
                ERR_LIST_DOTTED_PRECEDED + ECO,
                SexprError::ListDottedPreceded,
            )),
            ((None, None), cursor) => Ok((Value::new_nil(), cursor)),
        }
    }

    fn tracked_list(&self, input: Input<'b>, pmap: &mut PositionMap) -> ParseResult<'b, Value<'a>> {
        let m = self.m.clone();

        let item = map2(self.datum(), |item, input| (item, input.position()));
        let items = many1(terminated(item, Self::intertoken));
        let dotted = preceded(terminated(char('.'), Self::intertoken), cut(self.datum()));
        let items = map_err(pair(opt(items), opt(dotted)), |mut err| {
            if err.code() == code::ERR_PAIR_SECOND {
                let span = err.span();
                err.push(span, ERR_LIST_DOTTED + ECO, SexprError::ListDotted);
            }
            err
        });
        let items = delimited(terminated(tag("("), Self::intertoken), items, tag(")"))(input)?;

        match items {
            ((Some(items), Some(dotted)), cursor) => {
                let mut m = m.borrow_mut();
                let head = m.new_pair(items[0].0.clone(), Value::new_nil());
                let mut tail = head.clone();

                pmap.insert(head.id(), items[0].1);
                for (item, pos) in items.into_iter().skip(1) {
                    let pair = m.new_pair(item, Value::new_nil());
                    pmap.insert(pair.id(), pos);
                    tail.set_cdr(pair.clone().into());
                    tail = pair;
                }
                tail.set_cdr(dotted.clone());
                Ok((head.into(), cursor))
            }
            ((Some(items), None), cursor) => {
                let mut m = m.borrow_mut();
                let head = m.new_pair(items[0].0.clone(), Value::new_nil());
                let mut tail = head.clone();

                pmap.insert(head.id(), items[0].1);
                for (item, pos) in items.into_iter().skip(1) {
                    let pair = m.new_pair(item, Value::new_nil());
                    pmap.insert(pair.id(), pos);
                    tail.set_cdr(pair.clone().into());
                    tail = pair;
                }
                Ok((head.into(), cursor))
            }
            ((None, Some(_)), _) => Err(ParseError::new(
                input,
                ERR_LIST_DOTTED_PRECEDED + ECO,
                SexprError::ListDottedPreceded,
            )),
            ((None, None), cursor) => Ok((Value::new_nil(), cursor)),
        }
    }
}

#[cfg(test)]
mod tests {
    use kamo_macros::sexpr;

    use crate::{mem::Mutator, parser::Span, Position};

    use super::*;

    #[test]
    fn list_untracked_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone());
        let parse = |input| sexpr.list()(Input::new(input));

        assert_eq!(
            parse("(1 2 3)"),
            Ok((sexpr!(m, "(1 2 3)"), Input::from(""))),
        );
        assert_eq!(
            parse("(1 2 . 3)"),
            Ok((sexpr!(m, "(1 2 . 3)"), Input::from(""))),
        );
        assert_eq!(
            parse("(1 . 2)"),
            Ok((sexpr!(m, "(1 . 2)"), Input::from(""))),
        );
        assert_eq!(parse("()"), Ok((sexpr!(m, "()"), Input::from(""))),);
    }

    #[test]
    fn list_tracked_success() {
        let m = Mutator::new_ref();
        let mut sexpr = Sexpr::<0>::new(m.clone()).tracked();

        let list = sexpr.list()("(1 2 3)".into());
        assert_eq!(list, Ok((sexpr!(m, "(1 2 3)"), Input::from(""))),);
        check_positions(
            list.unwrap().0,
            sexpr.take_positions().unwrap(),
            &[
                Position::new(1, 1, 2),
                Position::new(3, 3, 4),
                Position::new(5, 5, 6),
            ],
        );

        let mut sexpr = sexpr.tracked();
        let list = sexpr.list()("(1 2 . 3)".into());
        assert_eq!(list, Ok((sexpr!(m, "(1 2 . 3)"), Input::from(""))),);
        check_positions(
            list.unwrap().0,
            sexpr.take_positions().unwrap(),
            &[Position::new(1, 1, 2), Position::new(3, 3, 4)],
        );

        let mut sexpr = sexpr.tracked();
        let list = sexpr.list()("(1 . 2)".into());
        assert_eq!(list, Ok((sexpr!(m, "(1 . 2)"), Input::from(""))),);
        check_positions(
            list.unwrap().0,
            sexpr.take_positions().unwrap(),
            &[Position::new(1, 1, 2)],
        );

        let mut sexpr = sexpr.tracked();
        let list = sexpr.list()("()".into());
        assert_eq!(list, Ok((sexpr!(m, "()"), Input::from(""))),);
        check_positions(list.unwrap().0, sexpr.take_positions().unwrap(), &[]);
    }

    #[allow(clippy::needless_pass_by_value)]
    fn check_positions(list: Value, pmap: PositionMap, positions: &[Position]) {
        assert_eq!(pmap.len(), positions.len());
        if let Some(pair) = list.as_pair_ptr() {
            assert_eq!(pmap.get(&pair.id()), Some(&positions[0]));
        } else {
            assert_eq!(pmap.len(), 0);
        }
    }

    #[test]
    fn list_untracked_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone());
        let parse = |input| sexpr.list()(Input::new(input));

        list_failure(parse);
    }

    #[test]
    fn list_tracked_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone()).tracked();
        let parse = |input| sexpr.list()(Input::new(input));

        list_failure(parse);
    }

    fn list_failure<'a, F>(parse: F)
    where
        F: Fn(&'a str) -> ParseResult<'a, Value<'a>>,
    {
        let result = parse("");
        assert_eq!(
            result,
            Err(ParseError::new(
                Position::new(0, 1, 1),
                ERR_LIST_LITERAL,
                SexprError::ListLiteral
            )),
        );
        assert!(result.unwrap_err().is_eof());

        let result = parse("(");
        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
                ERR_LIST_CLOSING,
                SexprError::ListClosing
            )),
        );
        assert!(result.unwrap_err().is_eof());

        let result = parse("(1 2 3");
        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(6, 1, 7)),
                ERR_LIST_CLOSING,
                SexprError::ListClosing
            )),
        );
        assert!(result.unwrap_err().is_eof());

        let result = parse("(1 2 3 .");
        assert_eq!(
            result,
            Err(ParseError::new(
                Position::new(7, 1, 8),
                ERR_LIST_DOTTED,
                SexprError::ListDotted
            )),
        );
        assert!(result.unwrap_err().is_eof());

        let result = parse("(. 2)");
        assert_eq!(
            result,
            Err(ParseError::new(
                Position::new(0, 1, 1),
                ERR_LIST_DOTTED_PRECEDED,
                SexprError::ListDottedPreceded
            )),
        );
    }
}
