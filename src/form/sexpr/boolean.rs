use crate::{
    parser::{prelude::*, Code, Input, ParseResult},
    value::Value,
};

use super::{code::ERR_BOOLEAN_LITERAL, Sexpr, SexprError};

const TRUE: Value = Value::new_bool(true);
const FALSE: Value = Value::new_bool(false);

impl<'a, 'b, const ECO: Code> Sexpr<'a, ECO> {
    /// Parses a boolean literal.
    ///
    /// # Grammar
    ///
    /// ```text
    /// boolean = "#t" | "#f" | "#true" | "#false"
    /// ```
    pub fn boolean(input: Input<'b>) -> ParseResult<'b, Value<'a>> {
        context_as(
            any((
                value(TRUE.to_owned(), tag("#true")),
                value(TRUE.to_owned(), tag("#t")),
                value(FALSE.to_owned(), tag("#false")),
                value(FALSE.to_owned(), tag("#f")),
            )),
            ERR_BOOLEAN_LITERAL + ECO,
            SexprError::BooleanLiteral,
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::Position;

    use super::*;

    const BOOLEANS: &[(&str, &str, bool)] = &[
        ("#true", "", true),
        ("#truely", "ly", true),
        ("#t", "", true),
        ("#tra", "ra", true),
        ("#false", "", false),
        ("#falsely", "ly", false),
        ("#f", "", false),
        ("#furr", "urr", false),
    ];

    #[test]
    fn boolean_success() {
        let parse = Sexpr::<0>::boolean;

        for (i, (input, output, expected)) in BOOLEANS.iter().enumerate() {
            let input = Input::new(input);
            let expected = Ok((Value::new_bool(*expected), Input::new(output)));

            assert_eq!(parse(input), expected, "boolean {} failed", i + 1);
        }
    }

    #[test]
    fn boolean_failure() {
        let parse = Sexpr::<0>::boolean;
        let expected = Err(ParseError::new(
            Position::default(),
            ERR_BOOLEAN_LITERAL,
            SexprError::BooleanLiteral,
        ));

        let input = Input::new("");
        assert_eq!(parse(input), expected);

        let input = Input::new("#a");
        assert_eq!(parse(input), expected);

        let input = Input::new("false");
        assert_eq!(parse(input), expected);
    }
}
