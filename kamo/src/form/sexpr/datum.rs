use crate::{
    parser::{prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{code, Sexpr, SexprError};

/// An enum representing a datum or a shadowed datum.
///
/// This enum is returned by the
/// [`datum_or_shadowed()`](Sexpr::datum_or_shadowed) method.
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Datum<'a> {
    Exposed(Value<'a>),
    Shadowed(Value<'a>),
}

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a datum.
    ///
    /// # Grammar
    ///
    /// ```text
    /// datum = boolean | character | number | string | bytevec | vector
    ///       | symbol | list | quote | quasiquote | unquote | unquote_splicing
    /// ```
    pub fn datum(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        move |input| {
            if input.is_eof() {
                return Err(ParseError::eof(input).and(
                    input,
                    code::ERR_DATUM + ECO,
                    SexprError::Datum,
                ));
            }
            for (prefix, parser) in &mut [
                (
                    tag("("),
                    &mut self.list() as &mut dyn Parser<'b, 'a, Value<'a>>,
                ),
                (tag("#t"), &mut Self::boolean),
                (tag("#f"), &mut Self::boolean),
                (tag("#\\"), &mut Self::character),
                (tag("#b"), &mut Self::number_binary),
                (tag("#o"), &mut Self::number_octal),
                (tag("#d"), &mut Self::number_decimal),
                (tag("#x"), &mut Self::number_hexdec),
                (tag("\""), &mut self.string()),
                (tag("#("), &mut self.vector()),
                (tag("#u8("), &mut self.bytevec()),
                (tag("'"), &mut self.quote()),
                (tag("`"), &mut self.quasiquote()),
                (tag(",@"), &mut self.unquote_splicing()),
                (tag(","), &mut self.unquote()),
            ] {
                match prefix(input) {
                    Ok(_) => return parser.parse(input),
                    Err(err) => {
                        if err.is_eof() {
                            if input.as_str().starts_with('#') {
                                return Err(ParseError::eof(input).and(
                                    input,
                                    code::ERR_DATUM_HASHTAG + ECO,
                                    SexprError::DatumHashtag,
                                ));
                            }
                            if input.as_str().starts_with(',') {
                                return Err(ParseError::eof(input).and(
                                    input,
                                    code::ERR_DATUM_UNQUOTE + ECO,
                                    SexprError::DatumUnquote,
                                ));
                            }
                            return Err(err);
                        }
                    }
                }
            }

            for parser in &mut [
                &mut Self::decimal as &mut dyn Parser<'b, 'a, Value<'a>>,
                &mut Self::infnan,
                &mut self.symbol(),
            ] {
                match parser.parse(input) {
                    Ok((value, cursor)) => return Ok((value, cursor)),
                    Err(err) => {
                        if err.is_semantic() || err.is_eof() {
                            return Err(err);
                        }
                    }
                }
            }

            Err(ParseError::new(
                input,
                code::ERR_DATUM + ECO,
                SexprError::Datum,
            ))
        }
    }

    /// Parses a datum or a shadowed datum.
    ///
    /// # Grammar
    ///
    /// ```text
    /// datum_or_shadowed = shadowed | datum
    /// ```
    pub fn datum_or_shadowed(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Datum<'a>> + '_ {
        move |input| {
            if tag("#;")(input).is_ok() {
                let (value, cursor) = self.shadowed()(input)?;
                Ok((Datum::Shadowed(value), cursor))
            } else {
                let (value, cursor) = self.datum()(input)?;
                Ok((Datum::Exposed(value), cursor))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use kamo_macros::sexpr;

    use crate::{mem::Mutator, parser::Span, Position};

    use super::*;

    #[allow(clippy::cognitive_complexity)]
    #[test]
    fn datum_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone());
        let datum = |input| sexpr.datum()(Input::new(input));

        assert_eq!(datum("#t"), Ok((sexpr!(r#"#t"#), Input::new(""))));
        assert_eq!(datum("#true"), Ok((sexpr!(r#"#t"#), Input::new(""))));
        assert_eq!(datum("#truely"), Ok((sexpr!(r#"#t"#), Input::new("ly"))));
        assert_eq!(datum("#f"), Ok((sexpr!(r#"#f"#), Input::new(""))));
        assert_eq!(datum("#falsly"), Ok((sexpr!(r#"#f"#), Input::new("alsly"))));
        assert_eq!(datum("#false"), Ok((sexpr!(r#"#f"#), Input::new(""))));
        assert_eq!(datum("#\\a"), Ok((sexpr!(r#"#\a"#), Input::new(""))));
        assert_eq!(
            datum("#\\alarm"),
            Ok((sexpr!(r#"#\alarm"#), Input::new("")))
        );
        assert_eq!(datum("#\\x41"), Ok((sexpr!(r#"#\A"#), Input::new(""))));
        assert_eq!(datum("#\\x41ly"), Ok((sexpr!(r#"#\A"#), Input::new("ly"))));
        assert_eq!(datum("#b101"), Ok((sexpr!(r#"#b101"#), Input::new(""))));
        assert_eq!(datum("#o777"), Ok((sexpr!(r#"#o777"#), Input::new(""))));
        assert_eq!(datum("#d123"), Ok((sexpr!(r#"#d123"#), Input::new(""))));
        assert_eq!(datum("#x1f"), Ok((sexpr!(r#"#x1f"#), Input::new(""))));
        assert_eq!(
            datum("\"hello\""),
            Ok((sexpr!(m, r#""hello""#), Input::new("")))
        );
        assert_eq!(
            datum("#(1 2 3)"),
            Ok((sexpr!(m, r"#(1 2 3)"), Input::new("")))
        );
        assert_eq!(
            datum("#u8(1 2 3)"),
            Ok((sexpr!(m, r"#u8(1 2 3)"), Input::new("")))
        );
        assert_eq!(datum("'a"), Ok((sexpr!(m, r#"'a"#), Input::new(""))));
        assert_eq!(datum("`a"), Ok((sexpr!(m, r#"`a"#), Input::new(""))));
        assert_eq!(datum(",@a"), Ok((sexpr!(m, r#",@a"#), Input::new(""))));
        assert_eq!(datum(",a"), Ok((sexpr!(m, r#",a"#), Input::new(""))));
        assert_eq!(datum("123"), Ok((sexpr!(m, r#"123"#), Input::new(""))));
        assert_eq!(datum("123ly"), Ok((sexpr!(m, r#"123"#), Input::new("ly"))));
        assert_eq!(
            datum("123.456"),
            Ok((sexpr!(m, r#"123.456"#), Input::new("")))
        );
        assert_eq!(
            datum("123.456ly"),
            Ok((sexpr!(m, r#"123.456"#), Input::new("ly")))
        );
        assert_eq!(
            datum("123.456e+289"),
            Ok((sexpr!(m, r#"123.456e+289"#), Input::new("")))
        );
        assert_eq!(
            datum("123.456e+289ly"),
            Ok((sexpr!(m, r#"123.456e+289"#), Input::new("ly")))
        );
        assert_eq!(
            datum("123.456e-789"),
            Ok((sexpr!(m, r#"123.456e-789"#), Input::new("")))
        );
        assert_eq!(
            datum("123.456e-789ly"),
            Ok((sexpr!(m, r#"123.456e-789"#), Input::new("ly")))
        );
        assert_eq!(
            datum("123.456e289"),
            Ok((sexpr!(m, r#"123.456e289"#), Input::new("")))
        );
        assert_eq!(
            datum("123.456e289ly"),
            Ok((sexpr!(m, r#"123.456e289"#), Input::new("ly")))
        );
        assert_eq!(
            datum("+inf.0"),
            Ok((sexpr!(m, r#"+inf.0"#), Input::new("")))
        );
        assert_eq!(
            datum("-inf.0"),
            Ok((sexpr!(m, r#"-inf.0"#), Input::new("")))
        );
        assert_eq!(
            datum("+nan.0"),
            Ok((sexpr!(m, r#"+nan.0"#), Input::new("")))
        );
        assert_eq!(
            datum("-nan.0"),
            Ok((sexpr!(m, r#"-nan.0"#), Input::new("")))
        );
        assert_eq!(datum("a"), Ok((sexpr!(m, r#"a"#), Input::new(""))));
        assert_eq!(datum("a123"), Ok((sexpr!(m, r#"a123"#), Input::new(""))));
        assert_eq!(
            datum("(1 2 3)"),
            Ok((sexpr!(m, r#"(1 2 3)"#), Input::new("")))
        );
    }

    #[test]
    fn datum_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone());
        let datum = |input| sexpr.datum()(Input::new(input));

        assert_eq!(
            datum(""),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_DATUM,
                SexprError::Datum
            ))
        );
        assert_eq!(
            datum("#"),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_DATUM_HASHTAG,
                SexprError::DatumHashtag
            ))
        );
        assert_eq!(
            datum(","),
            Err(ParseError::new(
                Position::new(0, 1, 1),
                code::ERR_DATUM_UNQUOTE,
                SexprError::DatumUnquote
            ))
        );
        assert_eq!(
            datum("'"),
            Err(ParseError::new(
                Position::new(1, 1, 2),
                code::ERR_DATUM,
                SexprError::Datum
            ))
        );
        assert_eq!(
            datum("("),
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
                code::ERR_LIST_CLOSING,
                SexprError::ListClosing
            ))
        );
    }
}
