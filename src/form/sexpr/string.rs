use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult, Span},
    value::Value,
};

use super::{
    code::{ERR_STRING_CLOSING, ERR_STRING_CODE, ERR_STRING_ESCAPE, ERR_STRING_LITERAL},
    Sexpr, SexprError,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Fragment {
    Escape(char),
    Ilws,
}

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a string literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// string        = "\"" string-text "\""
    /// string-text   = (string-escape | string-char)*
    /// string-escape = "\\" ( "a" | "b" | "t" | "n" | "r" | "\\" | "\""
    ///                      | "x" hex-code ";" | ilws )
    /// string-char   = <any> - ["\\" | "\""]
    /// hex-code      = [0-9a-fA-F]{1,6}
    /// ilws          = [ \t]* ("\n" | "\r\n") [ \t]*
    /// ```
    pub fn string(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| {
            delimited(char('"'), Self::string_text, char('"'))(input)
                .map(|(text, cursor)| (m.borrow_mut().new_string(text).into(), cursor))
                .map_err(|mut err| {
                    let span = err.span();

                    match err.code() {
                        code::ERR_PRECEDED => {
                            err.push(span, ERR_STRING_LITERAL + ECO, SexprError::StringLiteral)
                        }
                        code::ERR_TERMINATED => {
                            err.push(span, ERR_STRING_CLOSING + ECO, SexprError::StringClosing)
                        }
                        _ => (),
                    }
                    err
                })
        }
    }

    fn string_text(input: Input<'b>) -> ParseResult<'b, String> {
        let mut cursor = input;
        let mut text = String::new();

        while let Some(ch) = cursor.current() {
            match ch {
                '\\' => {
                    let (fragment, rest) = Self::string_escape(cursor)?;

                    match fragment {
                        Fragment::Escape(ch) => text.push(ch),
                        Fragment::Ilws => (),
                    };
                    cursor = rest;
                }
                '"' => break,
                _ => {
                    text.push(ch);
                    cursor.advance();
                }
            }
        }

        Ok((text, cursor))
    }

    fn string_escape(input: Input<'b>) -> ParseResult<'b, Fragment> {
        let mut cursor = input;

        match cursor.current() {
            Some('\\') => {
                let escape = match cursor.advance() {
                    Some('a') => Fragment::Escape('\x07'),
                    Some('b') => Fragment::Escape('\x08'),
                    Some('t') => Fragment::Escape('\t'),
                    Some('n') => Fragment::Escape('\n'),
                    Some('r') => Fragment::Escape('\r'),
                    Some('\\') => Fragment::Escape('\\'),
                    Some('"') => Fragment::Escape('"'),
                    Some('x') => {
                        cursor.advance();
                        let (ch, cursor) = context_as(
                            terminated(Self::character_code, char(';')),
                            ERR_STRING_CODE + ECO,
                            SexprError::StringCode,
                        )(cursor)?;

                        return Ok((Fragment::Escape(ch), cursor));
                    }
                    Some(_) => {
                        let mut ilws =
                            recognize(tuple((ascii::space0, ascii::line_ending, ascii::space0)));

                        return if let Ok((_, cursor)) = ilws(cursor) {
                            Ok((Fragment::Ilws, cursor))
                        } else {
                            Err(ParseError::new(
                                Span::new(input.position(), cursor.position()),
                                ERR_STRING_ESCAPE + ECO,
                                SexprError::StringEscape,
                            ))
                        };
                    }
                    None => {
                        return Err(ParseError::eof(cursor.position()).and(
                            cursor.position(),
                            ERR_STRING_ESCAPE + ECO,
                            SexprError::StringEscape,
                        ))
                    }
                };

                cursor.advance();
                Ok((escape, cursor))
            }
            Some(_) => Err(ParseError::new(
                input.position(),
                ERR_STRING_ESCAPE + ECO,
                SexprError::StringEscape,
            )),
            None => Err(ParseError::eof(cursor.position()).and(
                cursor.position(),
                ERR_STRING_ESCAPE + ECO,
                SexprError::StringEscape,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{mem::Mutator, Position};

    use super::*;

    const ESCAPES: &[(&str, &str, Fragment)] = &[
        (r#"\a"#, "", Fragment::Escape('\x07')),
        (r#"\b"#, "", Fragment::Escape('\x08')),
        (r#"\t"#, "", Fragment::Escape('\t')),
        (r#"\n"#, "", Fragment::Escape('\n')),
        (r#"\r"#, "", Fragment::Escape('\r')),
        (r#"\\"#, "", Fragment::Escape('\\')),
        (r#"\""#, "", Fragment::Escape('"')),
        (r#"\x20;"#, "", Fragment::Escape(' ')),
        ("\\\n    ", "", Fragment::Ilws),
        ("\\\r\n    ", "", Fragment::Ilws),
        (r#"\a "#, " ", Fragment::Escape('\x07')),
        (r#"\b "#, " ", Fragment::Escape('\x08')),
        (r#"\t "#, " ", Fragment::Escape('\t')),
        (r#"\n "#, " ", Fragment::Escape('\n')),
        (r#"\r "#, " ", Fragment::Escape('\r')),
        (r#"\\ "#, " ", Fragment::Escape('\\')),
        (r#"\" "#, " ", Fragment::Escape('"')),
        (r#"\x20; "#, " ", Fragment::Escape(' ')),
        ("\\    \n    ", "", Fragment::Ilws),
        ("\\    \r\n    ", "", Fragment::Ilws),
    ];

    #[test]
    fn string_escape_success() {
        let parse = Sexpr::<0>::string_escape;

        for (i, (input, rest, expected)) in ESCAPES.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((*expected, Input::new(rest)));

            assert_eq!(parse(input), expected, "string escape {} failed", i + 1);
        }
    }

    #[test]
    fn string_escape_failure() {
        let parse = Sexpr::<0>::string_escape;

        let input = Input::new(r#"\x20"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(2, 1, 3), Position::new(4, 1, 5)),
            ERR_STRING_CODE,
            SexprError::StringCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"\xg;"#);
        let expected = Err(ParseError::new(
            Position::new(2, 1, 3),
            ERR_STRING_CODE,
            SexprError::StringCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"\g"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
            ERR_STRING_ESCAPE,
            SexprError::StringEscape,
        ));

        assert_eq!(parse(input), expected);
    }

    const STRINGS: &[(&str, &str, &str)] = &[
        ("\"\"", "", ""),
        ("\"abc\"", "", "abc"),
        ("\"abc\\\\\"", "", "abc\\"),
        ("\"abc\\\\\n\"", "", "abc\\\n"),
        ("\"abc\\x20;\"", "", "abc "),
        (
            r#""abc \
            def""#,
            "",
            "abc def",
        ),
    ];

    #[test]
    fn string_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.to_owned());
        let parse = sexpr.string();

        for (i, (input, rest, expected)) in STRINGS.iter().enumerate() {
            let input = Input::new(input);
            let expected = m.borrow_mut().new_string(*expected).into();
            let expected = Ok((expected, Input::new(rest)));

            assert_eq!(parse(input), expected, "string text {} failed", i + 1);
        }
    }

    #[test]
    fn string_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m);
        let parse = sexpr.string();

        let input = Input::new(r#""\x20""#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(3, 1, 4), Position::new(5, 1, 6)),
            ERR_STRING_CODE,
            SexprError::StringCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#""\xg;""#);
        let expected = Err(ParseError::new(
            Position::new(3, 1, 4),
            ERR_STRING_CODE,
            SexprError::StringCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#""\g""#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(1, 1, 2), Position::new(2, 1, 3)),
            ERR_STRING_ESCAPE,
            SexprError::StringEscape,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#""abc"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(4, 1, 5)),
            ERR_STRING_CLOSING,
            SexprError::StringClosing,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"abc"#);
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_STRING_LITERAL,
            SexprError::StringLiteral,
        ));

        assert_eq!(parse(input), expected);
    }
}
