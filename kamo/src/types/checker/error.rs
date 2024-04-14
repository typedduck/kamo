use std::{error::Error, fmt};

use crate::{
    parser::ParseError,
    types::{Type, TypeError},
};

#[allow(clippy::module_name_repetitions)]
#[derive(PartialEq)]
pub enum TypeCheckError {
    ArityMismatch(String, usize, usize),
    MinArityMismatch(String, usize, usize),
    MaxArityMismatch(String, usize, usize),
    NotASymbol,
    NotAList,
    NotAType,
    NotAFunction(Type),
    InproperList,
    UnknownType(String),
    UnknownOperator(String),
    UnexpectedOperatorType(String, Type, Vec<Type>),
    ExpectedType(Type, Type, String),
    ExpectedReturnType(Type, Type, String),
    UndefinedVariable(String),
    UnboundVariable(String),
    MalformedDeclaration,
    MalformedParameter,
    MalformedParamDecls,
    MalformedReturnDecl,
    TypeError(TypeError),
    ParseError(ParseError),
}

impl Error for TypeCheckError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::TypeError(err) => Some(err),
            Self::ParseError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ArityMismatch(tag, expected, found) => {
                write!(
                    f,
                    "arity mismatch for {tag}: expected {expected}, found {found}"
                )
            }
            Self::MinArityMismatch(tag, expected, found) => {
                write!(
                    f,
                    "arity mismatch for {tag}: expected at least {expected}, found {found}"
                )
            }
            Self::MaxArityMismatch(tag, expected, found) => {
                write!(
                    f,
                    "arity mismatch for {tag}: expected at most {expected}, found {found}"
                )
            }
            Self::NotASymbol => write!(f, "not a symbol"),
            Self::NotAList => write!(f, "not a list"),
            Self::NotAType => write!(f, "not a type"),
            Self::NotAFunction(ty) => write!(f, "not a function: {ty}"),
            Self::InproperList => write!(f, "inproper list ending with a dotted value"),
            Self::UnknownType(expr) => write!(f, "unknown type: {expr}"),
            Self::UnknownOperator(oper) => write!(f, "unknown operator: {oper}"),
            Self::UnexpectedOperatorType(oper, found, allowed) => write!(
                f,
                "unexpected operator type for operator `{}`: found {}, allowed {}",
                oper,
                found,
                allowed
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::ExpectedType(expected, found, value) => write!(
                f,
                "expected type {expected}, found {found} for value `{value}`"
            ),
            Self::ExpectedReturnType(expected, found, value) => write!(
                f,
                "expected return type {expected}, found {found} for function `{value}`"
            ),
            Self::UndefinedVariable(var) => write!(f, "undefined variable: {var}"),
            Self::UnboundVariable(var) => write!(f, "unbound variable: {var}"),
            Self::MalformedDeclaration => {
                write!(f, "malformed declaration: expected `<symbol>` or `(<symbol> <type>)`")
            }
            Self::MalformedParameter => write!(
                f,
                "malformed parameter: expected `(<symbol> . <type>)` or `(<symbol> <type>)`"
            ),
            Self::MalformedParamDecls => write!(
                f,
                "malformed parameter declarations: expected `((<symbol> . <type>) ...)`, `((<symbol> <type>) ...)` or `()`"
            ),
            Self::MalformedReturnDecl => write!(
                f,
                "malformed return declaration: expected `-> <type>``"
            ),
            Self::TypeError(err) => write!(f, "{err}"),
            Self::ParseError(err) => write!(f, "{err}"),
        }
    }
}

impl fmt::Debug for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl From<TypeError> for TypeCheckError {
    fn from(err: TypeError) -> Self {
        Self::TypeError(err)
    }
}

impl From<ParseError> for TypeCheckError {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}
