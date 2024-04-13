use crate::{
    parser::{predicate, prelude::*, Code, Input, ParseResult, Span},
    value::Value,
};

use super::{
    code::{ERR_CHARACTER_CODE, ERR_CHARACTER_CODE_POINT, ERR_CHARACTER_LITERAL},
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a character literal.
    ///
    /// # Errors
    ///
    /// Fails if the input is not a character literal. Returns a [`ParseError`]
    /// with code [`ERR_CHARACTER_LITERAL`] and the error variant
    /// [`SexprError::CharacterLiteral`].
    ///
    /// # Grammar
    ///
    /// ```text
    /// character      = "#\\" <any character>
    ///                | "#\\" character_name
    ///                | "#\\x" character_code
    /// character_name = "alarm" | "backspace" | "delete | "escape"
    ///                | "newline" | "null" | "return" | "space"
    ///                | "tab"
    /// character_code = [0-9A-Fa-f]{1, 6}
    /// ```
    pub fn character(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        let mut cursor = input;

        if cursor.advance_tag("#\\x") == Some(true) {
            return Self::character_code(cursor).map(|(c, cursor)| (Value::new_char(c), cursor));
        }
        match cursor.advance_tag("#\\") {
            Some(true) => any((Self::character_name, any_char))(cursor)
                .map(|(ch, cursor)| (Value::new_char(ch), cursor))
                .map_err(|mut err| {
                    let span = err.span();

                    err.push(
                        Span::new(input.position(), span.end()),
                        ERR_CHARACTER_LITERAL + ECO,
                        SexprError::CharacterLiteral,
                    );
                    err
                }),
            None => Err(ParseError::eof(input.position())),
            Some(false) => Err(ParseError::new(
                input.position(),
                ERR_CHARACTER_LITERAL + ECO,
                SexprError::CharacterLiteral,
            )),
        }
    }

    fn character_name(input: Input<'b>) -> ParseResult<'b, char> {
        any((
            value('\x07', tag("alarm")),
            value('\x08', tag("backspace")),
            value('\x7f', tag("delete")),
            value('\x1b', tag("escape")),
            value('\n', tag("newline")),
            value('\0', tag("null")),
            value('\r', tag("return")),
            value(' ', tag("space")),
            value('\t', tag("tab")),
        ))(input)
    }

    pub(super) fn character_code(input: Input<'b>) -> ParseResult<'b, char> {
        let (code, cursor) =
            take_while_m_n(1, 6, predicate::is_hex_digit)(input).map_err(|mut err| {
                let span = err.span();

                err.push(
                    Span::new(input.position(), span.end()),
                    ERR_CHARACTER_CODE + ECO,
                    SexprError::CharacterCode,
                );
                err
            })?;
        let code = u32::from_str_radix(code, 16).unwrap();

        char::from_u32(code).map_or_else(
            || {
                Err(ParseError::new(
                    Span::new(input.position(), cursor.position()),
                    ERR_CHARACTER_CODE_POINT + ECO,
                    SexprError::CharacterCodePoint,
                )
                .and_semantic())
            },
            |code| Ok((code, cursor)),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::Position;

    use super::*;

    const CHARACTERS: &[(&str, char, &str)] = &[
        ("#\\a", 'a', ""),
        ("#\\alarm", '\x07', ""),
        ("#\\backspace", '\x08', ""),
        ("#\\delete", '\x7f', ""),
        ("#\\escape", '\x1b', ""),
        ("#\\newline", '\n', ""),
        ("#\\null", '\0', ""),
        ("#\\return", '\r', ""),
        ("#\\space", ' ', ""),
        ("#\\tab", '\t', ""),
        ("#\\x41", 'A', ""),
        ("#\\x61", 'a', ""),
        ("#\\x7f", '\x7f', ""),
        ("#\\x1b", '\x1b', ""),
        ("#\\x0a", '\n', ""),
        ("#\\x00", '\0', ""),
        ("#\\x0d", '\r', ""),
        ("#\\x20", ' ', ""),
        ("#\\x09", '\t', ""),
        ("#\\x41ly", 'A', "ly"),
        ("#\\x61ly", 'a', "ly"),
        ("#\\x7fly", '\x7f', "ly"),
        ("#\\x1bly", '\x1b', "ly"),
        ("#\\x0aly", '\n', "ly"),
        ("#\\x00ly", '\0', "ly"),
        ("#\\x0dly", '\r', "ly"),
        ("#\\x20ly", ' ', "ly"),
        ("#\\x09ly", '\t', "ly"),
    ];

    #[test]
    fn character_success() {
        let parser = |input| Sexpr::<0>::character(Input::new(input));

        for (i, (input, expected, output)) in CHARACTERS.iter().enumerate() {
            assert_eq!(
                parser(input),
                Ok((Value::new_char(*expected), Input::new(output))),
                "character {} failed",
                i + 1
            );
        }
    }

    #[test]
    fn character_failure() {
        let parser = |input| Sexpr::<0>::character(Input::new(input));

        assert_eq!(
            parser("#\\"),
            Err(ParseError::new(
                Span::new(Position::default(), Position::new(2, 1, 3)),
                ERR_CHARACTER_LITERAL,
                SexprError::CharacterLiteral
            ))
        );
        assert_eq!(
            parser("#\\x"),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                ERR_CHARACTER_CODE,
                SexprError::CharacterCode
            ))
        );
        assert_eq!(
            parser("#\\xg"),
            Err(ParseError::new(
                Position::new(3, 1, 4),
                ERR_CHARACTER_CODE,
                SexprError::CharacterCode
            ))
        );
        assert_eq!(
            parser("#\\xffffff"),
            Err(ParseError::new(
                Span::new(Position::new(3, 1, 4), Position::new(9, 1, 10)),
                ERR_CHARACTER_CODE_POINT,
                SexprError::CharacterCodePoint
            ))
        );
    }
}
