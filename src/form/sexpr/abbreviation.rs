use crate::{
    parser::{prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::Sexpr;

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a quoted literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// quote = "'" intertoken datum
    /// ```
    pub fn quote(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            map(
                preceded(char('\''), preceded(Self::intertoken, self.datum())),
                |value| {
                    let quote = m.borrow_mut().new_symbol("quote");

                    Value::new_list(m.to_owned(), [quote.into(), value])
                },
            )(input)
        }
    }

    /// Parses a quasi-quoted literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// quasiquote = "`" intertoken datum
    /// ```
    pub fn quasiquote(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            map(
                preceded(char('`'), preceded(Self::intertoken, self.datum())),
                |value| {
                    let quote = m.borrow_mut().new_symbol("quasiquote");

                    Value::new_list(m.to_owned(), [quote.into(), value])
                },
            )(input)
        }
    }

    /// Parses an unquoted literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// unquote = "," intertoken datum
    /// ```
    pub fn unquote(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            map(
                preceded(char(','), preceded(Self::intertoken, self.datum())),
                |value| {
                    let quote = m.borrow_mut().new_symbol("unquote");

                    Value::new_list(m.to_owned(), [quote.into(), value])
                },
            )(input)
        }
    }

    /// Parses an unquoted splicing literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// unquote_splicing = ",@" intertoken datum
    /// ```
    pub fn unquote_splicing(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            map(
                preceded(tag(",@"), preceded(Self::intertoken, self.datum())),
                |value| {
                    let quote = m.borrow_mut().new_symbol("unquote-splicing");

                    Value::new_list(m.to_owned(), [quote.into(), value])
                },
            )(input)
        }
    }
}
