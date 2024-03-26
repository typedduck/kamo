use crate::{
    parser::{code, prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{
    code::{ERR_COMMENT, ERR_COMMENT_CLOSE, ERR_WHITESPACE},
    Sexpr, SexprError,
};

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses optional intertoken whitespace and comments.
    ///
    /// # Grammar
    ///
    /// ```text
    /// intertoken = (whitespace | comment)*
    /// ```
    ///
    /// # Errors
    ///
    /// Comments which are not properly closed will result in an error.
    pub fn intertoken(input: Input<'b>) -> ParseResult<'b, ()> {
        let (_, mut cursor) = unicode::whitespace0(input).unwrap();

        loop {
            let (_, next) = match cursor.as_str() {
                tag if tag.starts_with(';') => Self::comment_line(cursor),
                tag if tag.starts_with("#|") => Self::comment_nested(cursor),
                _ => Ok(("", cursor)),
            }?;
            let (_, next) = unicode::whitespace0(next).unwrap();

            if cursor.position() == next.position() {
                return Ok(((), next));
            } else {
                cursor = next;
            }
        }
    }

    /// Parses a shadowed datum.
    ///
    /// # Grammar
    ///
    /// ```text
    /// shadowed = "#;" intertoken datum
    /// ```
    pub fn shadowed(&self) -> impl Fn(Input<'b>) -> ParseResult<'b, Value<'a>> + '_ {
        move |input| preceded(pair(tag("#;"), Self::intertoken), self.datum())(input)
    }

    /// Parses whitespace and comments.
    ///
    /// # Grammar
    ///
    /// ```text
    /// whitespace = <Unicode whitespace>+
    /// ```
    pub fn whitespace(input: Input<'b>) -> ParseResult<'b, &'b str> {
        unicode::whitespace1(input).map_err(|mut err| {
            let span = err.span();

            err.push(span, ERR_WHITESPACE + ECO, SexprError::Whitespace);
            err
        })
    }

    /// Parses a comment.
    ///
    /// # Grammar
    ///
    /// ```text
    /// comment        = comment-line | comment-nested
    /// comment-line   = ";" <any>* ("\n" | eof)
    /// comment-nested = "#|" comment-text comment-cont* "|#"
    /// comment-text   = <any except "#|" or "|#">*
    /// comment-cont   = comment-nested comment-text
    /// ```
    pub fn comment(input: Input<'b>) -> ParseResult<'b, &'b str> {
        let mut cursor = input;

        if let Some(true) = cursor.advance_tag(";") {
            return Self::comment_line(input);
        }
        if let Some(true) = cursor.advance_tag("#|") {
            return Self::comment_nested(input);
        }
        Err(ParseError::new(
            input.position(),
            ERR_COMMENT + ECO,
            SexprError::Comment,
        ))
    }

    fn comment_line(input: Input<'b>) -> ParseResult<'b, &'b str> {
        let (_, mut cursor) = char(';')(input)?;

        cursor.advance_while(|c| c != '\n');
        if let Some('\n') = cursor.current() {
            cursor.advance();
        }

        let output = &input.as_str()[..(cursor.position().offset() - input.position().offset())];

        Ok((output, cursor))
    }

    fn comment_nested(input: Input<'b>) -> ParseResult<'b, &'b str> {
        delimited(
            tag("#|"),
            terminated(Self::comment_text, many0_count(Self::comment_cont)),
            tag("|#"),
        )(input)
        .map(|(_, cursor)| {
            let output =
                &input.as_str()[..(cursor.position().offset() - input.position().offset())];
            (output, cursor)
        })
        .map_err(|mut err| {
            match err.code() {
                code::ERR_EOF | code::ERR_TERMINATED => {
                    let span = err.span();
                    err.push(span, ERR_COMMENT_CLOSE + ECO, SexprError::CommentClose)
                }
                _ => {}
            }
            err
        })
    }

    fn comment_text(input: Input<'b>) -> ParseResult<'b, ()> {
        let mut cursor = input;

        while cursor.current().is_some() {
            if cursor.as_str().starts_with("#|") || cursor.as_str().starts_with("|#") {
                return Ok(((), cursor));
            }
            cursor.advance();
        }
        Ok(((), cursor))
    }

    fn comment_cont(input: Input<'b>) -> ParseResult<'b, &'b str> {
        recognize(terminated(Self::comment_nested, Self::comment_text))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Span, Position};

    use super::*;

    const WHITESPACES: &[(&str, &str, &str)] = &[
        (" ", "", " "),
        (" text", "text", " "),
        ("\t", "", "\t"),
        ("\ttext", "text", "\t"),
        ("\n", "", "\n"),
        ("\ntext", "text", "\n"),
        ("\r", "", "\r"),
        ("\rtext", "text", "\r"),
        (" \t\n\r", "", " \t\n\r"),
        (" \t\n\rtext", "text", " \t\n\r"),
        (" ; comment", "; comment", " "),
        (" #| comment |#", "#| comment |#", " "),
        (" #| comment |#text", "#| comment |#text", " "),
        (" #| comment |# ", "#| comment |# ", " "),
        (" #| comment |# text", "#| comment |# text", " "),
        (" \t\n\r; comment", "; comment", " \t\n\r"),
        (" \t\n\r; comment\n \t\n\r", "; comment\n \t\n\r", " \t\n\r"),
        (" \t\n\r#| comment |#", "#| comment |#", " \t\n\r"),
        (
            " \t\n\r#| comment |# \t\n\r",
            "#| comment |# \t\n\r",
            " \t\n\r",
        ),
    ];

    #[test]
    fn whitespace_success() {
        let parse = Sexpr::<0>::whitespace;

        for (i, (input, rest, expected)) in WHITESPACES.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((*expected, Input::new(rest)));

            assert_eq!(parse(input), expected, "whitespace {} failed", i + 1);
        }
    }

    #[test]
    fn whitespace_failure() {
        let parse = Sexpr::<0>::whitespace;

        let input = Input::new("");
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_WHITESPACE,
            SexprError::Whitespace,
        ));
        assert_eq!(parse(input), expected);

        let input = Input::new("text");
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_WHITESPACE,
            SexprError::Whitespace,
        ));
        assert_eq!(parse(input), expected);
    }

    const COMMENTS: &[(&str, &str, &str)] = &[
        ("; comment\n", "", "; comment\n"),
        ("; comment\ntext", "text", "; comment\n"),
        ("; comment", "", "; comment"),
        ("#| nested |#", "", "#| nested |#"),
        ("#| nested |# text", " text", "#| nested |#"),
        (
            "#| nested #| ; nested |# |#",
            "",
            "#| nested #| ; nested |# |#",
        ),
        (
            "#| nested #| ; nested |# |# text",
            " text",
            "#| nested #| ; nested |# |#",
        ),
        (
            "#| nested #| ; nested |# |# text #| nested |#",
            " text #| nested |#",
            "#| nested #| ; nested |# |#",
        ),
    ];

    #[test]
    fn comment_success() {
        let parse = Sexpr::<0>::comment;

        for (i, (input, rest, expected)) in COMMENTS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((*expected, Input::new(rest)));

            assert_eq!(parse(input), expected, "comment {} failed", i + 1);
        }
    }

    fn error(span: impl Into<Span>) -> ParseError {
        ParseError::new(span.into(), ERR_COMMENT_CLOSE, SexprError::CommentClose)
    }

    #[test]
    fn comment_failure() {
        let parse = Sexpr::<0>::comment;

        let input = Input::new("");
        let expected = Err(ParseError::new(
            Position::new(0, 1, 1),
            ERR_COMMENT,
            SexprError::Comment,
        ));
        assert_eq!(parse(input), expected);

        let input = Input::new("#|");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(2, 1, 3),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(9, 1, 10),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #|");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(10, 1, 11),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #| ; nested");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(10, 1, 11),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #| ; nested |#");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(24, 1, 25),
        )));
        assert_eq!(parse(input), expected);
    }

    const INTERTOKENS: &[(&str, &str)] = &[
        ("", ""),
        (" ", ""),
        (" text", "text"),
        ("\t", ""),
        ("\ttext", "text"),
        ("\n", ""),
        ("\ntext", "text"),
        ("\r", ""),
        ("\rtext", "text"),
        (" \t\n\r", ""),
        (" \t\n\rtext", "text"),
        (" ; comment", ""),
        (" #| comment |#", ""),
        (" #| comment |#text", "text"),
        (" #| comment |# ", ""),
        (" #| comment |# text", "text"),
        (" \t\n\r; comment", ""),
        (" \t\n\r; comment\n \t\n\r", ""),
        (" \t\n\r#| comment |#", ""),
        (" \t\n\r#| comment #| nested |#|#", ""),
        (" \t\n\r#| comment |# \t\n\r", ""),
        (" \t\n\r#| comment #| nested |#|# \t\n\r", ""),
    ];

    #[test]
    fn intertoken_success() {
        let parse = Sexpr::<0>::intertoken;

        for (i, (input, rest)) in INTERTOKENS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok(((), Input::new(rest)));

            assert_eq!(parse(input), expected, "intertoken {} failed", i + 1);
        }
    }

    #[test]
    fn intertoken_failure() {
        let parse = Sexpr::<0>::intertoken;

        let input = Input::new("#|");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(2, 1, 3),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(9, 1, 10),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #|");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(10, 1, 11),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #| ; nested");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(10, 1, 11),
        )));
        assert_eq!(parse(input), expected);

        let input = Input::new("#| nested #| ; nested |#");
        let expected = Err(error(Span::new(
            Position::new(0, 1, 1),
            Position::new(24, 1, 25),
        )));
        assert_eq!(parse(input), expected);
    }
}
