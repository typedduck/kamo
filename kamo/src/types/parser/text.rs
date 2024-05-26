//! # Type parser for textual type representation
//!
//! The type parser is used to parse textual type representations. It is used to
//! parse types from strings. The parser is implemented with the combinator
//! parsing library defined in the module [`parser`](crate::parser).
//!
//! The grammar for the type parser is defined as follows:
//!
//! ```text
//! type           = ("nil" | "void" | filled-named) eof
//! filled         = ("any" | union | specific) "?"?
//! filled-named   = filled | named
//! specific       = predefined | array | pair | lambda
//! specific-named = specific | named
//! predefined     = "type" | "bool" | "char" | "int" | "float"
//!                | "symbol" | "binary"
//! array          = "[" filled-named array-len? "]"
//! array-len      = ";" number
//! pair           = "(" filled-named "*" filled-named ")"
//! union          = "(" specific-named ("|" specific-named)+ ")"
//! lambda         = "fn(" (param-list "->" rettype)? ")"
//! param-list     = (filled-named ("," filled-named)* ("," "..." filled-named)?)
//!                | ("..." filled-named)
//!                | "void"
//! rettype        = ("void" | filled)
//! named          = ident_start ident_cont* "?"?
//! ```

use crate::{
    env::EnvironmentRef,
    parser::{prelude::*, Code, Input, Span},
    types::{
        parser::error::{
            code::{
                ERR_ARRAY_LENGTH, ERR_EXPECTED_TYPE, ERR_FILLED_TYPE, ERR_MALFORMED_NAME,
                ERR_NESTED_OPTION, ERR_PREDEFINED_TYPE, ERR_SPECIFIC_TYPE, ERR_TYPE,
                ERR_UNBOUND_NAME, ERR_UNDEFINED_NAME,
            },
            TypeParseError,
        },
        Type, ARRAY_MAX,
    },
};

/// A parameter in a lambda type.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Parameter {
    Fixed(Type),
    Variadic(Type),
}

/// Parse a type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// This reverses the output of the `Type::to_string()` method. It also is
/// called by the `Type::from_str()` method.
///
/// # Grammar
///
/// ```text
/// type = ("nil" | "void" | filled-named) eof
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
                    parse_filled_named::<ECO>(env.clone()),
                )),
                ERR_TYPE + ECO,
                TypeParseError::NotAType,
            ),
            eof,
        )(input)
    }
}

/// Parse a filled type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// filled  = ("any" | union | specific) "?"?
/// ```
pub fn parse_filled<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let ((ty, opt), cursor) = pair(
            context_as(
                any((
                    value(Type::any(), tag("any")),
                    parse_specific::<ECO>(env.clone()),
                    parse_union::<ECO>(env.clone()),
                )),
                ERR_FILLED_TYPE + ECO,
                TypeParseError::NotAFilledType,
            ),
            map(opt(preceded(ascii::whitespace0, char('?'))), |opt| {
                opt.is_some()
            }),
        )(input)?;

        if opt {
            let ty = Type::option(ty).map_err(|_| {
                ParseError::new(
                    Span::new(input, cursor),
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

/// Parse a filled or named type from a string.
///
/// This function is used to parse a type that can be either a filled type or a
/// named type. Named types are types that are defined in the environment. If a
/// named type is given, the type must meet the predicate [`Type::is_filled()`].
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// filled-named = filled | named
/// ```
pub fn parse_filled_named<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            parse_filled::<ECO>(env.clone()),
            context_as(
                verify(parse_named::<ECO>(env.clone()), Type::is_filled),
                ERR_FILLED_TYPE + ECO,
                TypeParseError::NotAFilledType,
            ),
        ))(input)
    }
}

/// Parse a specific type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// specific = predefined | array | pair | lambda
/// ```
pub fn parse_specific<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            parse_predfined::<ECO>,
            parse_array::<ECO>(env.clone()),
            parse_pair::<ECO>(env.clone()),
            parse_lambda::<ECO>(env.clone()),
        ))(input)
    }
}

/// Parse a specific or named type from a string.
///
/// This function is used to parse a type that can be either a specific type or
/// a named type. Named types are types that are defined in the environment. If
/// a named type is given, the type must meet the predicate
/// [`Type::is_specific()`].
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// specific-named = specific | named
/// ```
pub fn parse_specific_named<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            parse_specific::<ECO>(env.clone()),
            context_as(
                verify(parse_named::<ECO>(env.clone()), Type::is_specific),
                ERR_SPECIFIC_TYPE + ECO,
                TypeParseError::NotASpecificType,
            ),
        ))(input)
    }
}

/// Parse a predefined type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// predefined = "type" | "bool" | "char" | "int" | "float" | "symbol"
///            | "binary"
/// ```
///
/// # Errors
///
/// Returns an error if the input does not match any of the predefined types.
pub fn parse_predfined<const ECO: Code>(input: Input<'_>) -> ParseResult<'_, Type> {
    use crate::parser::prelude::*;

    any((
        value(Type::typedef(), tag("type")),
        value(Type::boolean(), tag("bool")),
        value(Type::character(), tag("char")),
        value(Type::integer(), tag("int")),
        value(Type::float(), tag("float")),
        value(Type::symbol(), tag("symbol")),
        value(Type::binary(), tag("binary")),
    ))(input)
    .map_err(|mut err| {
        err.push(
            input.position(),
            ERR_PREDEFINED_TYPE + ECO,
            TypeParseError::NotAPredefinedType,
        );
        err
    })
}

/// Parse an array type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
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
/// array     = "[" filled-named array-len? "]"
/// array-len = ";" number
/// ```
pub fn parse_array<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let ((elem, len), cursor) = delimited(
            pair(char('['), ascii::whitespace0),
            pair(
                parse_filled_named::<ECO>(env.clone()),
                parse_array_len::<ECO>,
            ),
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

/// Parse a pair type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Panics
///
/// The function will panic if the pair type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// pair = "(" filled-named "*" filled-named ")"
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
                parse_filled_named::<ECO>(env.clone()),
                delimited(ascii::whitespace0, char('*'), ascii::whitespace0),
                parse_filled_named::<ECO>(env.clone()),
            )),
            pair(ascii::whitespace0, char(')')),
        )(input)?;
        let ty = Type::pair(car, cdr).expect("pair type");

        Ok((ty, cursor))
    }
}

/// Parse a union type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Panics
///
/// The function will panic if the union type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// union = "(" specific ("|" specific)+ ")"
/// ```
pub fn parse_union<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let (types, cursor) = delimited(
            pair(char('('), ascii::whitespace0),
            list_m_n(
                2,
                255,
                parse_specific_named::<ECO>(env.clone()),
                delimited(ascii::whitespace0, char('|'), ascii::whitespace0),
            ),
            pair(ascii::whitespace0, char(')')),
        )(input)?;
        let ty = Type::union(types).expect("union type");

        Ok((ty, cursor))
    }
}

/// Parse a lambda type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Panics
///
/// The function will panic if the lambda type cannot be created. This should
/// never happen as the function is only called with valid input.
///
/// # Grammar
///
/// ```text
/// lambda = "fn(" (param_list "->" rettype)? ")"
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

/// Parse a return type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// rettype = ("void" | filled-named)
/// ```
pub fn parse_rettype<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        any((
            value(Type::void(), tag("void")),
            parse_filled_named::<ECO>(env.clone()),
        ))(input)
    }
}

/// Parse a parameter list from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// param-list = (filled-named ("," filled-named)* ("," "..." filled-named)?)
///            | ("..." filled-named)
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
                    parse_filled_named::<ECO>(env.clone()),
                ),
                Parameter::Variadic,
            ),
            map(parse_filled_named::<ECO>(env.clone()), Parameter::Fixed),
        ))(input)
    }
}

/// Parse a named type from a string.
///
/// Takes an error code offset as a parameter. All custom errors are reported
/// with the given code offset and correspond to the `ERR_CONTEXT + offset`
/// error domain.
///
/// # Grammar
///
/// ```text
/// named = ident_start ident_cont* "?"?
/// ```
pub fn parse_named<'a, 'b, const ECO: Code>(
    env: Option<EnvironmentRef<'a>>,
) -> impl Fn(Input<'b>) -> ParseResult<'b, Type> + 'a {
    use crate::parser::prelude::*;

    move |input| {
        let ((name, opt), cursor) = pair(
            context_as(
                recognize(pair(unicode::ident_start, unicode::ident_cont)),
                ERR_MALFORMED_NAME + ECO,
                TypeParseError::MalformedName,
            ),
            map(opt(preceded(ascii::whitespace0, char('?'))), |opt| {
                opt.is_some()
            }),
        )(input)?;

        if let Some(env) = env.clone() {
            if let Some(binding) = env.borrow().lookup(name) {
                let ty = binding.value().ok_or_else(|| {
                    ParseError::new(
                        Span::new(input, cursor),
                        ERR_UNBOUND_NAME + ECO,
                        TypeParseError::UnboundName(name.to_string()),
                    )
                })?;
                let ty = ty.as_type_ptr().ok_or_else(|| {
                    ParseError::new(
                        Span::new(input, cursor),
                        ERR_EXPECTED_TYPE + ECO,
                        TypeParseError::ExpectedType(
                            name.to_string(),
                            binding.typedef().to_owned(),
                        ),
                    )
                })?;
                let ty = ty.as_ref().clone();

                return if opt {
                    let ty = Type::option(ty).map_err(|_| {
                        ParseError::new(
                            Span::new(input, cursor),
                            ERR_NESTED_OPTION + ECO,
                            TypeParseError::NestedOption,
                        )
                    })?;

                    Ok((ty, cursor))
                } else {
                    Ok((ty, cursor))
                };
            }
        }
        Err(ParseError::new(
            Span::new(input, cursor),
            ERR_UNDEFINED_NAME + ECO,
            TypeParseError::UndefinedName(name.to_string()),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{env::Environment, mem::Mutator, types::parser::code, value::Value, Position};

    const TYPES: [(&str, &[u8]); 32] = [
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
            "(char ?*int?)",
            &[
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        (
            "(char*int) ?",
            &[code::OPTION, code::PAIR, code::CHAR, code::INT],
        ),
        (
            "(char *int?)?",
            &[
                code::OPTION,
                code::PAIR,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        (
            "(char?* int)?",
            &[
                code::OPTION,
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::INT,
            ],
        ),
        (
            "(char? * int?)?",
            &[
                code::OPTION,
                code::PAIR,
                code::OPTION,
                code::CHAR,
                code::OPTION,
                code::INT,
            ],
        ),
        (
            "(char|int)",
            &[code::UNION, code::CHAR, code::INT, code::END],
        ),
        (
            "(char|int|float)",
            &[code::UNION, code::CHAR, code::INT, code::FLOAT, code::END],
        ),
        (
            "(char|int|float|symbol)",
            &[
                code::UNION,
                code::CHAR,
                code::INT,
                code::FLOAT,
                code::SYMBOL,
                code::END,
            ],
        ),
        (
            "(char |int| float | symbol|binary)",
            &[
                code::UNION,
                code::CHAR,
                code::INT,
                code::FLOAT,
                code::SYMBOL,
                code::BINARY,
                code::END,
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
        for (i, (text, code)) in TYPES.iter().enumerate() {
            let input = Input::from(*text);
            let result = parse::<0>(None)(input);

            assert_eq!(
                result,
                Ok((unsafe { Type::new_unchecked(code) }, Input::from(""))),
                "Type {} failed: {text}",
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

        let input = Input::from("invalid?");
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

    #[test]
    fn parse_named_success() {
        let m = Mutator::new_ref();
        let env = Environment::new_ref(m.clone());

        env.borrow_mut().define(
            "type",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::typedef())),
        );
        env.borrow_mut().define(
            "bool",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::boolean())),
        );
        env.borrow_mut().define(
            "char",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::character())),
        );
        env.borrow_mut().define(
            "int",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::integer())),
        );
        env.borrow_mut().define(
            "float",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::float())),
        );
        env.borrow_mut().define(
            "symbol",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::symbol())),
        );
        env.borrow_mut().define(
            "binary",
            Type::typedef(),
            Some(Value::new_type(m.clone(), Type::binary())),
        );
        env.borrow_mut().define(
            "maybe_binary",
            Type::typedef(),
            Some(Value::new_type(
                m.clone(),
                Type::option(Type::binary()).unwrap(),
            )),
        );

        let input = Input::from("type");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::typedef(), Input::from(""))));

        let input = Input::from("bool");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::boolean(), Input::from(""))));

        let input = Input::from("char");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::character(), Input::from(""))));

        let input = Input::from("int");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::integer(), Input::from(""))));

        let input = Input::from("float");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::float(), Input::from(""))));

        let input = Input::from("symbol");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::symbol(), Input::from(""))));

        let input = Input::from("binary");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(result, Ok((Type::binary(), Input::from(""))));

        let input = Input::from("binary?");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(
            result,
            Ok((Type::option(Type::binary()).unwrap(), Input::from("")))
        );
    }

    #[test]
    fn parse_named_failure() {
        let m = Mutator::new_ref();
        let env = Environment::new_ref(m.clone());

        let input = Input::from("invalid");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(7, 1, 8)),
                ERR_UNDEFINED_NAME,
                TypeParseError::UndefinedName("invalid".to_string())
            ))
        );

        let input = Input::from("invalid?");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(8, 1, 9)),
                ERR_UNDEFINED_NAME,
                TypeParseError::UndefinedName("invalid".to_string())
            ))
        );

        env.borrow_mut().define("unbound", Type::typedef(), None);

        let input = Input::from("unbound");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(7, 1, 8)),
                ERR_UNBOUND_NAME,
                TypeParseError::UnboundName("unbound".to_string())
            ))
        );

        env.borrow_mut().define(
            "maybe_bool",
            Type::typedef(),
            Some(Value::new_type(
                m.clone(),
                Type::option(Type::boolean()).unwrap(),
            )),
        );

        let input = Input::from("maybe_bool?");
        let result = parse_named::<0>(Some(env.clone()))(input);

        assert_eq!(
            result,
            Err(ParseError::new(
                Span::new(Position::new(0, 1, 1), Position::new(11, 1, 12)),
                ERR_NESTED_OPTION,
                TypeParseError::NestedOption
            ))
        );
    }
}
