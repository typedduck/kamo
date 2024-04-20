use crate::{
    env::EnvironmentRef,
    parser::{prelude::*, Code, Input, Span},
    types::{
        parser::error::{
            code::{
                ERR_ARRAY_LENGTH, ERR_EXPECTED_TYPE, ERR_FILLED_TYPE, ERR_MALFORMED_NAME,
                ERR_NESTED_OPTION, ERR_TYPE, ERR_UNBOUND_NAME, ERR_UNDEFINED_NAME,
            },
            TypeParseError,
        },
        Type, ARRAY_MAX,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Parameter {
    Fixed(Type),
    Variadic(Type),
}

/// Parse a type from a string. Takes an error code offset as a parameter. All
/// custom errors are reported with the given code offset and correspond to the
/// `ERR_CONTEXT + offset` error domain.
///
/// This reverses the output of the `Type::to_string` method. It also is called
/// by the `Type::from_str` method.
///
/// # Grammar
///
/// ```text
/// type = ("nil" | "void" | filled) eof
/// ```
pub fn parse<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        terminated(
            context_as(
                any((
                    value(Type::nil(), tag("nil")),
                    value(Type::void(), tag("void")),
                    parse_filled::<ECO>(env.clone()),
                )),
                ERR_TYPE + ECO,
                TypeParseError::NotAType,
            ),
            eof,
        )(input)
    }
}

/// Parse a filled type from a string. Takes an error code offset as a
/// parameter. All custom errors are reported with the given code offset and
/// correspond to the `ERR_CONTEXT + offset` error domain.
///
/// # Grammar
///
/// ```text
/// filled     = (predefined | array | pair | lambda | named) "?"?
/// predefined = "any" | "type" | "bool" | "char"
///            | "symbol" | "binary" | "int" | "float"
/// ```
pub fn parse_filled<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let filled = any((
            value(Type::any(), tag("any")),
            value(Type::typedef(), tag("type")),
            value(Type::boolean(), tag("bool")),
            value(Type::character(), tag("char")),
            value(Type::symbol(), tag("symbol")),
            value(Type::binary(), tag("binary")),
            value(Type::integer(), tag("int")),
            value(Type::float(), tag("float")),
            parse_array::<ECO>(env.clone()),
            parse_pair::<ECO>(env.clone()),
            parse_lambda::<ECO>(env.clone()),
        ));
        let filled = pair(
            filled,
            map(opt(preceded(ascii::whitespace0, char('?'))), |opt| {
                opt.is_some()
            }),
        )(input);
        let mut named = pair(
            parse_named::<ECO>(env.clone()),
            map(opt(preceded(ascii::whitespace0, char('?'))), |opt| {
                opt.is_some()
            }),
        );
        let ((ty, opt), cursor) = match filled {
            Ok(value) => value,
            Err(_) => match named(input) {
                Ok(((ty, opt), cursor)) => {
                    if !(ty.is_filled() || ty.is_option()) {
                        return Err(ParseError::new(
                            input.position(),
                            ERR_FILLED_TYPE + ECO,
                            TypeParseError::NotAFilledType,
                        ));
                    }
                    ((ty, opt), cursor)
                }
                Err(mut err) => {
                    if err.code() != ERR_MALFORMED_NAME + ECO {
                        return Err(err);
                    }
                    err.push(
                        input.position(),
                        ERR_FILLED_TYPE + ECO,
                        TypeParseError::NotAType,
                    );
                    return Err(err);
                }
            },
        };

        if opt {
            let ty = Type::option(ty).map_err(|_| {
                ParseError::new(
                    input.position(),
                    ERR_NESTED_OPTION + ECO,
                    TypeParseError::NestedOption,
                )
            })?;

            Ok((ty, cursor))
        } else {
            Ok((ty, cursor))
        }
    }
}

/// Parse an array type from a string. Takes an error code offset as a
/// parameter. All custom errors are reported with the given code offset and
/// correspond to the `ERR_CONTEXT + offset` error domain.
///
/// The maximum number of elements in an array is defined by the `ARRAY_MAX`
///
/// # Panics
///
/// The function will panic if the array type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// array     = "[" filled array-len? "]"
/// array-len = ";" number
/// ```
pub fn parse_array<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let ((elem, len), cursor) = delimited(
            pair(char('['), ascii::whitespace0),
            pair(parse_filled::<ECO>(env.clone()), parse_array_len::<ECO>),
            pair(ascii::whitespace0, char(']')),
        )(input)?;
        let ty = Type::array(elem, len).expect("array type");

        Ok((ty, cursor))
    }
}

fn parse_array_len<const ECO: Code>(input: Input<'_>) -> ParseResult<'_, Option<usize>> {
    use crate::parser::prelude::*;

    opt(preceded(
        pair(char(';'), ascii::whitespace0),
        literal::natural(literal::Radix::Decimal),
    ))(input)
    .and_then(|(len, cursor)| {
        if let Some(len) = len {
            let len = usize::try_from(len).map_err(|_| {
                ParseError::new(
                    cursor.position(),
                    ERR_ARRAY_LENGTH + ECO,
                    TypeParseError::ArrayLengthCast,
                )
            })?;

            if len > ARRAY_MAX {
                return Err(ParseError::new(
                    Span::new(input.position(), cursor.position()),
                    ERR_ARRAY_LENGTH + ECO,
                    TypeParseError::ArrayLength,
                ));
            }
            Ok((Some(len), cursor))
        } else {
            Ok((None, cursor))
        }
    })
}

/// Parse a pair type from a string. Takes an error code offset as a parameter.
/// All custom errors are reported with the given code offset and correspond to
/// the `ERR_CONTEXT + offset` error domain.
///
/// # Panics
///
/// The function will panic if the pair type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// pair = "(" filled "*" filled ")"
/// ```
#[allow(clippy::similar_names)]
pub fn parse_pair<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let ((car, _, cdr), cursor) = delimited(
            pair(char('('), ascii::whitespace0),
            tuple((
                parse_filled::<ECO>(env.clone()),
                delimited(ascii::whitespace0, char('*'), ascii::whitespace0),
                parse_filled::<ECO>(env.clone()),
            )),
            pair(ascii::whitespace0, char(')')),
        )(input)?;
        let ty = Type::pair(car, cdr).expect("pair type");

        Ok((ty, cursor))
    }
}

/// Parse a lambda type from a string. Takes an error code offset as a
/// parameter. All custom errors are reported with the given code offset and
/// correspond to the `ERR_CONTEXT + offset` error domain.
///
/// # Panics
///
/// The function will panic if the lambda type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// lambda = "fn(" (arglist "->" rettype)? ")"
pub fn parse_lambda<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let (signature, cursor) = delimited(
            pair(tag("fn("), ascii::whitespace0),
            opt(pair(
                parse_param_list::<ECO>(env.clone()),
                preceded(
                    delimited(ascii::whitespace0, tag("->"), ascii::whitespace0),
                    parse_rettype::<ECO>(env.clone()),
                ),
            )),
            pair(ascii::whitespace0, char(')')),
        )(input)?;

        if let Some(((args, varg), ret)) = signature {
            let ty = Type::lambda(args, varg, ret).expect("lambda type");

            Ok((ty, cursor))
        } else {
            let ty = Type::lambda(vec![], None, Type::void()).expect("lambda type");
            Ok((ty, cursor))
        }
    }
}

/// Parse a return type from a string. Takes an error code offset as a
/// parameter. All custom errors are reported with the given code offset and
/// correspond to the `ERR_CONTEXT + offset` error domain.
///
/// # Grammar
///
/// ```text
/// rettype = ("void" | filled)
/// ```
pub fn parse_rettype<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            value(Type::void(), tag("void")),
            parse_filled::<ECO>(env.clone()),
        ))(input)
    }
}

/// Parse a parameter list from a string. Takes an error code offset as a
/// parameter. All custom errors are reported with the given code offset and
/// correspond to the `ERR_CONTEXT + offset` error domain.
///
/// # Grammar
///
/// ```text
/// param_list = (filled ("," filled)* ("," "..." filled)?)
///            | ("..." filled)
///            | "void"
/// ```
pub fn parse_param_list<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, (Vec<Type>, Option<Type>)> + 'a {
    use crate::parser::prelude::*;

    fn param_list<'a, 'b, const ECO: Code>(
        env: Option<EnvironmentRef<'a>>,
    ) -> impl Fn(Input<'b>) -> ParseResult<'b, (Vec<Type>, Option<Type>)> + 'a {
        move |input| {
            let mut varg_err = Ok(false);
            let (args, cursor) = fold_list1(
                parse_arg::<ECO>(env.clone()),
                pair(char(','), ascii::whitespace0),
                || (Vec::new(), None),
                |(mut args, mut varg), _, arg| {
                    match arg {
                        Parameter::Fixed(arg) => args.push(arg),
                        Parameter::Variadic(arg) => match varg_err {
                            Ok(true) => {
                                varg_err = Err(ParseError::new(
                                    input.position(),
                                    0,
                                    "Multiple variadic parameters in lambda type",
                                ));
                            }
                            Ok(false) => {
                                varg = Some(arg);
                                varg_err = Ok(true);
                            }
                            Err(_) => {}
                        },
                    }
                    (args, varg)
                },
            )(input)?;

            varg_err?;
            Ok((args, cursor))
        }
    }

    move |input| {
        any((
            map(tag("void"), |_| (vec![], None)),
            param_list::<ECO>(env.clone()),
        ))(input)
    }
}

fn parse_arg<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Parameter> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            map(
                preceded(
                    pair(tag("..."), ascii::whitespace0),
                    parse_filled::<ECO>(env.clone()),
                ),
                Parameter::Variadic,
            ),
            map(parse_filled::<ECO>(env.clone()), Parameter::Fixed),
        ))(input)
    }
}

/// Parse a named type from a string. Takes an error code offset as a parameter.
/// All custom errors are reported with the given code offset and correspond to
/// the `ERR_CONTEXT + offset` error domain.
///
/// # Grammar
///
/// ```text
/// named = ident_start ident_cont*
/// ```
pub fn parse_named<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let (name, cursor) = context_as(
            recognize(pair(unicode::ident_start, unicode::ident_cont)),
            ERR_MALFORMED_NAME + ECO,
            TypeParseError::MalformedName,
        )(input)?;

        if let Some(env) = env.clone() {
            if let Some(binding) = env.borrow().lookup(name) {
                let ty = binding.value().ok_or_else(|| {
                    ParseError::new(
                        cursor.position(),
                        ERR_UNBOUND_NAME + ECO,
                        TypeParseError::UnboundName(name.to_string()),
                    )
                })?;
                let ty = ty.as_type_ptr().ok_or_else(|| {
                    ParseError::new(
                        cursor.position(),
                        ERR_EXPECTED_TYPE + ECO,
                        TypeParseError::ExpectedType(
                            name.to_string(),
                            binding.typedef().to_owned(),
                        ),
                    )
                })?;

                return Ok((ty.as_ref().clone(), cursor));
            }
        }
        Err(ParseError::new(
            Span::new(input.position(), cursor.position()),
            ERR_UNDEFINED_NAME + ECO,
            TypeParseError::UndefinedName(name.to_string()),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{types::parser::code, Position};

    const TYPES: [(&str, &[u8]); 28] = [
        ("nil", &[code::NIL]),
        ("void", &[code::VOID]),
        ("any", &[code::ANY]),
        ("type", &[code::TYPE]),
        ("bool", &[code::BOOL]),
        ("char", &[code::CHAR]),
        ("char?", &[code::OPTION, code::CHAR]),
        ("int", &[code::INT]),
        ("float", &[code::FLOAT]),
        ("symbol", &[code::SYMBOL]),
        ("binary", &[code::BINARY]),
        ("[char]", &[code::ARRAY, code::CHAR]),
        ("[char]?", &[code::OPTION, code::ARRAY, code::CHAR]),
        ("[char?]", &[code::ARRAY, code::OPTION, code::CHAR]),
        (
            "[char?]?",
            &[code::OPTION, code::ARRAY, code::OPTION, code::CHAR],
        ),
        ("(char*int)", &[code::PAIR, code::CHAR, code::INT]),
        (
            "(char*int?)",
            &[code::PAIR, code::CHAR, code::OPTION, code::INT],
        ),
        (
            "(char?*int)",
            &[code::PAIR, code::OPTION, code::CHAR, code::INT],
        ),
        (
            "(char?*int?)",
            &[
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        (
            "(char*int)?",
            &[code::OPTION, code::PAIR, code::CHAR, code::INT],
        ),
        (
            "(char*int?)?",
            &[
                code::OPTION,
                code::PAIR,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        (
            "(char?*int)?",
            &[
                code::OPTION,
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::INT,
            ],
        ),
        (
            "(char?*int?)?",
            &[
                code::OPTION,
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        ("fn()", &[code::LAMBDA, code::RETURN, code::VOID]),
        ("fn(void->void)", &[code::LAMBDA, code::RETURN, code::VOID]),
        ("fn(void->int)", &[code::LAMBDA, code::RETURN, code::INT]),
        (
            "fn(int->int)",
            &[code::LAMBDA, code::INT, code::RETURN, code::INT],
        ),
        (
            "fn(int,int->int)",
            &[code::LAMBDA, code::INT, code::INT, code::RETURN, code::INT],
        ),
    ];

    #[test]
    fn parse_success() {
        for (i, (input, code)) in TYPES.iter().enumerate() {
            let input = Input::from(*input);
            let result = parse::<0>(None)(input);

            assert_eq!(
                result,
                Ok((unsafe { Type::new_unchecked(code) }, Input::from(""))),
                "Type {} failed",
                i + 1
            );
        }
    }

    #[test]
    fn parse_failure() {
        let input = Input::from("invalid");
        let result = parse::<0>(None)(input);

        assert_eq!(
            result,
            Err(ParseError::new(
                Position::new(0, 1, 1),
                ERR_TYPE,
                TypeParseError::NotAType
            ))
        );
    }
}
