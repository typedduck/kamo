use crate::{
    parser::{code, predicate, prelude::*, Code, Input, ParseResult, Span},
    value::Value,
};

use super::{
    code::{
        ERR_SYMBOL_CLOSING, ERR_SYMBOL_CODE, ERR_SYMBOL_ESCAPE, ERR_SYMBOL_LITERAL,
        ERR_SYMBOL_SINGLE_DOT,
    },
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a symbol literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// symbol                 = (symbol_initial symbol_subsequent*)
    ///                        | ([+\-] 
    ///                              ( (symbol_sign_subsequent symbol_subsequent*)
    ///                              | ([.] symbol_dot_subsequent symbol_subsequent*))
    ///                        | ([.] symbol_dot_subsequent symbol_subsequent*))
    ///                        | ("|" symbol_text "|")
    /// symbol_text            = (symbol_escape | symbol_char)*
    /// symbol_escape          = "\" ( "a" | "b" | "t" | "n" | "r" | "\\" | "|"
    ///                              | "x" hex-code ";")
    /// symbol_char            = <any> - ["\\" | "|"]
    /// symbol_initial         = [a-zA-Z] | [!$%&*/:<=>?^_~]
    /// symbol_subsequent      = symbol_initial | [0-9] | [+\-.@]
    /// symbol_sign_subsequent = symbol_initial | [+\-@]
    /// symbol_dot_subsequent  = symbol_sign_subsequent | "."
    /// hex-code               = [0-9a-fA-F]{1,6}
    /// ```
    pub fn symbol(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        let m = self.m.to_owned();

        move |input| match input.current() {
            Some(ch) if Self::is_initial(ch) => {
                // This always succeeds because the initial character is already
                // checked.
                let (symbol, cursor) = take_while1(Self::is_subsequent)(input).unwrap();

                Ok((m.borrow_mut().new_symbol(symbol).into(), cursor))
            }
            Some('+') | Some('-') => {
                let mut parse = recognize(pair(
                    one_of("+-"),
                    opt(any((
                        map(
                            pair(
                                satisfy(Self::is_sign_subsequent),
                                take_while0(Self::is_subsequent),
                            ),
                            |_| (),
                        ),
                        map(
                            tuple((
                                char('.'),
                                satisfy(Self::is_dot_subsequent),
                                take_while0(Self::is_subsequent),
                            )),
                            |_| (),
                        ),
                    ))),
                ));
                // This always succeeds because the initial character is already
                // checked and all subsequent characters are optional.
                let (symbol, cursor) = parse(input).unwrap();

                Ok((m.borrow_mut().new_symbol(symbol).into(), cursor))
            }
            Some('.') => {
                let mut parse = recognize(tuple((
                    char('.'),
                    satisfy(Self::is_dot_subsequent),
                    take_while0(Self::is_subsequent),
                )));
                let (symbol, cursor) = parse(input).map_err(|mut err| {
                    let span = err.span();

                    err.push(
                        span,
                        ERR_SYMBOL_SINGLE_DOT + ECO,
                        SexprError::SymbolSingleDot,
                    );
                    err
                })?;

                Ok((m.borrow_mut().new_symbol(symbol).into(), cursor))
            }
            Some('|') => {
                let mut parse = delimited(char('|'), Self::symbol_text, char('|'));
                let (symbol, cursor) = parse(input).map_err(|mut err| {
                    let span = err.span();

                    if err.code() == code::ERR_TERMINATED {
                        err.push(span, ERR_SYMBOL_CLOSING + ECO, SexprError::SymbolClosing);
                    }
                    err
                })?;

                Ok((m.borrow_mut().new_symbol(symbol).into(), cursor))
            }
            Some(_) => Err(ParseError::new(
                input.position(),
                ERR_SYMBOL_LITERAL + ECO,
                SexprError::SymbolLiteral,
            )),
            None => Err(ParseError::eof(input.position()).and(
                input.position(),
                ERR_SYMBOL_LITERAL + ECO,
                SexprError::SymbolLiteral,
            )),
        }
    }

    fn symbol_text(input: Input<'b>) -> ParseResult<'b, String> {
        let mut cursor = input;
        let mut text = String::new();

        while let Some(ch) = cursor.current() {
            match ch {
                '\\' => {
                    let (ch, rest) = Self::symbol_escape(cursor)?;

                    text.push(ch);
                    cursor = rest;
                }
                '|' => break,
                _ => {
                    text.push(ch);
                    cursor.advance();
                }
            }
        }

        Ok((text, cursor))
    }

    fn symbol_escape(input: Input<'b>) -> ParseResult<'b, char> {
        let mut cursor = input;

        match cursor.current() {
            Some('\\') => {
                let escape = match cursor.advance() {
                    Some('a') => '\x07',
                    Some('b') => '\x08',
                    Some('t') => '\t',
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('\\') => '\\',
                    Some('|') => '|',
                    Some('x') => {
                        cursor.advance();
                        let (ch, cursor) = context_as(
                            terminated(Self::character_code, char(';')),
                            ERR_SYMBOL_CODE + ECO,
                            SexprError::SymbolCode,
                        )(cursor)?;

                        return Ok((ch, cursor));
                    }
                    Some(_) => {
                        return Err(ParseError::new(
                            Span::new(input.position(), cursor.position()),
                            ERR_SYMBOL_ESCAPE + ECO,
                            SexprError::SymbolEscape,
                        ));
                    }
                    None => {
                        return Err(ParseError::eof(cursor.position()).and(
                            cursor.position(),
                            ERR_SYMBOL_ESCAPE + ECO,
                            SexprError::StringEscape,
                        ))
                    }
                };

                cursor.advance();
                Ok((escape, cursor))
            }
            Some(_) => Err(ParseError::new(
                input.position(),
                ERR_SYMBOL_ESCAPE + ECO,
                SexprError::SymbolEscape,
            )),
            None => Err(ParseError::eof(cursor.position()).and(
                cursor.position(),
                ERR_SYMBOL_ESCAPE + ECO,
                SexprError::SymbolEscape,
            )),
        }
    }

    fn is_initial(ch: char) -> bool {
        predicate::is_alpha(ch)
            || matches!(
                ch,
                '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'
            )
    }

    fn is_subsequent(ch: char) -> bool {
        Self::is_initial(ch) || predicate::is_digit(ch) || matches!(ch, '+' | '-' | '.' | '@')
    }

    fn is_sign_subsequent(ch: char) -> bool {
        Self::is_initial(ch) || matches!(ch, '+' | '-' | '@')
    }

    fn is_dot_subsequent(ch: char) -> bool {
        Self::is_sign_subsequent(ch) || ch == '.'
    }
}

#[cfg(test)]
mod tests {
    use crate::{mem::Mutator, Position};

    use super::*;

    const ESCAPES: &[(&str, &str, char)] = &[
        (r#"\a"#, "", '\x07'),
        (r#"\b"#, "", '\x08'),
        (r#"\t"#, "", '\t'),
        (r#"\n"#, "", '\n'),
        (r#"\r"#, "", '\r'),
        (r#"\\"#, "", '\\'),
        (r#"\|"#, "", '|'),
        (r#"\x20;"#, "", ' '),
        (r#"\a "#, " ", '\x07'),
        (r#"\b "#, " ", '\x08'),
        (r#"\t "#, " ", '\t'),
        (r#"\n "#, " ", '\n'),
        (r#"\r "#, " ", '\r'),
        (r#"\\ "#, " ", '\\'),
        (r#"\| "#, " ", '|'),
        (r#"\x20; "#, " ", ' '),
    ];

    #[test]
    fn symbol_escape_success() {
        let parse = Sexpr::<0>::symbol_escape;

        for (i, (input, rest, expected)) in ESCAPES.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((*expected, Input::new(rest)));

            assert_eq!(parse(input), expected, "symbol escape {} failed", i + 1);
        }
    }

    #[test]
    fn symbol_escape_failure() {
        let parse = Sexpr::<0>::symbol_escape;

        let input = Input::new(r#"\x20"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(2, 1, 3), Position::new(4, 1, 5)),
            ERR_SYMBOL_CODE,
            SexprError::SymbolCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"\xg;"#);
        let expected = Err(ParseError::new(
            Position::new(2, 1, 3),
            ERR_SYMBOL_CODE,
            SexprError::SymbolCode,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"\g"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
            ERR_SYMBOL_ESCAPE,
            SexprError::SymbolEscape,
        ));

        assert_eq!(parse(input), expected);

        let input = Input::new(r#"\""#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(1, 1, 2)),
            ERR_SYMBOL_ESCAPE,
            SexprError::SymbolEscape,
        ));

        assert_eq!(parse(input), expected);
    }

    const SYMBOLS: &[(&str, &str, &str)] = &[
        (r#"hello"#, "", "hello"),
        (r#"..."#, "", "..."),
        (r#".."#, "", ".."),
        (r#"+"#, "", "+"),
        (r#"+soup+"#, "", "+soup+"),
        (r#"<=?"#, "", "<=?"),
        (r#"->string"#, "", "->string"),
        (r#"a34kTMNs"#, "", "a34kTMNs"),
        (r#"lambda"#, "", "lambda"),
        (r#"list->vector"#, "", "list->vector"),
        (r#"q"#, "", "q"),
        (r#"V17a"#, "", "V17a"),
        (
            r#"the-word-recursion-has-many-meanings"#,
            "",
            "the-word-recursion-has-many-meanings",
        ),
        (r#"|two words|"#, "", "two words"),
        (r#"|two\x20;words|"#, "", "two words"),
        (r#"|two\nwords|"#, "", "two\nwords"),
        (r#"|two\|words|"#, "", "two|words"),
        (r#"hello "#, " ", "hello"),
        (r#"... "#, " ", "..."),
        (r#".. "#, " ", ".."),
        (r#"+ "#, " ", "+"),
        (r#"+soup+ "#, " ", "+soup+"),
        (r#"<=? "#, " ", "<=?"),
        (r#"->string "#, " ", "->string"),
        (r#"a34kTMNs "#, " ", "a34kTMNs"),
        (r#"lambda "#, " ", "lambda"),
        (r#"|two words| "#, " ", "two words"),
    ];

    #[test]
    fn symbol_success() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m.to_owned());

        for (i, (input, rest, expected)) in SYMBOLS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((m.borrow_mut().new_symbol(expected).into(), Input::new(rest)));

            assert_eq!(sexpr.symbol()(input), expected, "symbol {} failed", i + 1);
        }
    }

    #[test]
    fn symbol_failure() {
        let m = Mutator::new_ref();
        let sexpr = Sexpr::<0>::new(m);

        let input = Input::new(r#""#);
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_SYMBOL_LITERAL,
            SexprError::SymbolLiteral,
        ));

        assert_eq!(sexpr.symbol()(input), expected);

        let input = Input::new(r#"|two words"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(0, 1, 1), Position::new(10, 1, 11)),
            ERR_SYMBOL_CLOSING,
            SexprError::SymbolClosing,
        ));

        assert_eq!(sexpr.symbol()(input), expected);

        let input = Input::new(r#"|two\" words|"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(4, 1, 5), Position::new(5, 1, 6)),
            ERR_SYMBOL_ESCAPE,
            SexprError::SymbolEscape,
        ));

        assert_eq!(sexpr.symbol()(input), expected);

        let input = Input::new(r#"|two\x20 words|"#);
        let expected = Err(ParseError::new(
            Span::new(Position::new(6, 1, 7), Position::new(8, 1, 9)),
            ERR_SYMBOL_CODE,
            SexprError::SymbolCode,
        ));

        assert_eq!(sexpr.symbol()(input), expected);

        let input = Input::new(r#"|two\xg; words|"#);
        let expected = Err(ParseError::new(
            Position::new(6, 1, 7),
            ERR_SYMBOL_CODE,
            SexprError::SymbolCode,
        ));

        assert_eq!(sexpr.symbol()(input), expected);

        let input = Input::new(r#"."#);
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_SYMBOL_SINGLE_DOT,
            SexprError::SymbolSingleDot,
        ));

        assert_eq!(sexpr.symbol()(input), expected);
    }
}
