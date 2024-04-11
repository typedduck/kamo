use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult, Span},
    value::Value,
};

use super::{
    code::{ERR_BYTEVEC_BYTE, ERR_BYTEVEC_CLOSING, ERR_BYTEVEC_LITERAL},
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a byte-vector literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// bytevec = "#u8(" intertoken (byte intertoken)* ")"
    /// byte    = <any extact number between 0 and 255>
    /// ```
    pub fn bytevec(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> {
        let m = self.m.clone();

        move |input| {
            map(
                delimited(
                    terminated(tag("#u8("), Self::intertoken),
                    many0(terminated(Self::bytevec_byte, Self::intertoken)),
                    tag(")"),
                ),
                |bytes| m.borrow_mut().new_bytevec(bytes).into(),
            )(input)
            .map_err(|mut err| {
                let span = err.span();

                match err.code() {
                    code::ERR_TERMINATED => {
                        err.push(span, ERR_BYTEVEC_CLOSING + ECO, SexprError::BytevecClosing);
                    }
                    code if code == ERR_BYTEVEC_BYTE + ECO => (),
                    _ => err.push(span, ERR_BYTEVEC_LITERAL + ECO, SexprError::BytevecLiteral),
                }
                err
            })
        }
    }

    fn bytevec_byte(input: Input<'b>) -> ParseResult<'b, u8> {
        match Self::number(input) {
            Ok((value, cursor)) => {
                let value = value.as_int();

                if let Some(value) = value {
                    if (0..=255).contains(&value) {
                        // Type cast is safe because the value is between 0 and 255.
                        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                        return Ok((value as u8, cursor));
                    }
                }
                Err(ParseError::new(
                    Span::new(input.position(), cursor.position()),
                    ERR_BYTEVEC_BYTE + ECO,
                    SexprError::BytevecByte,
                )
                .and_semantic())
            }
            Err(err) => Err(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{mem::Mutator, Position};

    use super::*;

    const BYTEVECS: &[(&str, &str, &[u8])] = &[
        ("#u8()", "", &[]),
        ("#u8(0)", "", &[0]),
        (
            "#u8(0 1 2 3 127 253 254 255)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        ("#u8(#b0)", "", &[0]),
        (
            "#u8(#b0 #b1 #b10 #b11 #b01111111 #b11111101 #b11111110 #b11111111)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        ("#u8(#o0)", "", &[0]),
        (
            "#u8(#o0 #o1 #o2 #o3 #o177 #o375 #o376 #o377)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        ("#u8(#d0)", "", &[0]),
        (
            "#u8(#d0 #d1 #d2 #d3 #d127 #d253 #d254 #d255)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        ("#u8(#x0)", "", &[0]),
        (
            "#u8(#x0 #x1 #x2 #x3 #x7f #xfd #xfe #xff)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        (
            "#u8(#x0#x1#x2#x3#x7f#xfd#xfe#xff)",
            "",
            &[0, 1, 2, 3, 127, 253, 254, 255],
        ),
        ("#u8(;comment\n)", "", &[]),
        ("#u8(;comment\n0;comment\n)", "", &[0]),
        ("#u8(;comment\n0;comment\n1;comment\n)", "", &[0, 1]),
        ("#u8( ;comment\n )", "", &[]),
        ("#u8( ;comment\n 0 ;comment\n )", "", &[0]),
        ("#u8( ;comment\n 0 ;comment\n 1 ;comment\n )", "", &[0, 1]),
    ];

    #[test]
    fn bytevec_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.clone());

        for (i, (input, rest, expected)) in BYTEVECS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((
                m.borrow_mut().new_bytevec(expected).into(),
                Input::new(rest),
            ));

            assert_eq!(
                sexpr.bytevec()(input),
                expected,
                "byte-vector {} failed: {}",
                i + 1,
                BYTEVECS[i].0
            );
        }
    }

    #[test]
    fn bytevec_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m);

        let input = Input::new("");
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_BYTEVEC_LITERAL,
            SexprError::BytevecLiteral,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);

        let input = Input::new("#u(256)");
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(2, 1, 3)),
            ERR_BYTEVEC_LITERAL,
            SexprError::BytevecLiteral,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);

        let input = Input::new("#u8(256)");
        let expected = Err(ParseError::new(
            Span::new(Position::new(4, 1, 5), Position::new(7, 1, 8)),
            ERR_BYTEVEC_BYTE,
            SexprError::BytevecByte,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);

        let input = Input::new("#u8(0 255");
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(9, 1, 10)),
            ERR_BYTEVEC_CLOSING,
            SexprError::BytevecClosing,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);

        let input = Input::new("#u8(0 1 256 2)");
        let expected = Err(ParseError::new(
            Span::new(Position::new(8, 1, 9), Position::new(11, 1, 12)),
            ERR_BYTEVEC_BYTE,
            SexprError::BytevecByte,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);

        let input = Input::new("#u8(0 1 hello 2)");
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(8, 1, 9)),
            ERR_BYTEVEC_CLOSING,
            SexprError::BytevecClosing,
        ));
        assert_eq!(sexpr.bytevec()(input), expected);
    }
}
