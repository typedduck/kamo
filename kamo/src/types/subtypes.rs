use std::fmt;

use super::Type;

/// A type that represents a homogeneous collection of values.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType {
    /// The type of the elements in the array. May be a filled or optional type.
    pub elem: Type,
    /// The length of the array. If `None`, the array is of unknown length and
    /// can be of any length.
    pub len: Option<usize>,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.len {
            Some(len) => write!(f, "[{}; {}]", self.elem, len),
            None => write!(f, "[{}]", self.elem),
        }
    }
}

impl PartialEq<Type> for ArrayType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_array() {
            self.len == other.len && self.elem == other.elem
        } else {
            false
        }
    }
}

/// A type that represents a function.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LambdaType {
    /// The types of the positional parameters of the function. May be filled or
    /// optional types.
    pub params: Vec<Type>,
    /// The type of the variadic parameter of the function, if any. May be a
    /// filled or optional type.
    pub variadic: Option<Type>,
    /// The type of the result of the function. It may be `void`, a filled or
    /// optional type.
    pub result: Type,
}

impl fmt::Display for LambdaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        match (self.params.len(), &self.variadic) {
            (0, None) => write!(f, "void")?,
            (0, Some(varg)) => write!(f, "...{varg}")?,
            (1, None) => write!(f, "{}", self.params[0])?,
            (1, Some(varg)) => write!(f, "{}, ...{}", self.params[0], varg)?,
            (n, None) => {
                for arg in &self.params[..n - 1] {
                    write!(f, "{arg}, ")?;
                }
                write!(f, "{}", self.params[n - 1])?;
            }
            (n, Some(varg)) => {
                for arg in &self.params[..n - 1] {
                    write!(f, "{arg}, ")?;
                }
                write!(f, "{}, ...{}", self.params[n - 1], varg)?;
            }
        }
        write!(f, " -> {})", self.result)
    }
}

impl PartialEq<Type> for LambdaType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_lambda() {
            self.params.len() == other.params.len()
                && self.params == other.params
                && self.variadic == other.variadic
                && self.result == other.result
        } else {
            false
        }
    }
}

/// A type that represents an optional value. This type indicates that a value
/// may be absent and hence be `nil`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OptionType {
    /// The type of the value, if any. It must be a filled type. Optional types
    /// cannot be nested.
    pub some: Type,
}

impl fmt::Display for OptionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}?", self.some)
    }
}

impl PartialEq<Type> for OptionType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_option() {
            self.some == other.some
        } else {
            false
        }
    }
}

/// A type that represents a pair of values, also known as a cons cell. This
/// type is used to represent linked lists and other recursive data structures.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PairType {
    /// The type of the first element in the pair. Also referred to as the `car`
    /// or the `head`. May be a filled or optional type.
    pub head: Type,
    /// The type of the second element in the pair. Also referred to as the
    /// `cdr` or the `tail`. May be a filled or optional type.
    pub tail: Type,
}

impl fmt::Display for PairType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} * {})", self.head, self.tail)
    }
}

impl PartialEq<Type> for PairType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_pair() {
            self.head == other.head && self.tail == other.tail
        } else {
            false
        }
    }
}

/// A type that represents a union of types. This type is used to represent
/// values that may be of different types at runtime. It is equivalent to a
/// or-pattern in a type system.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnionType {
    /// The types that are part of the union. The types must be filled types and
    /// there must be at least two types.
    pub types: Vec<Type>,
}

impl fmt::Display for UnionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "or({}", self.types[0])?;
        for ty in &self.types[1..] {
            write!(f, ", {ty}")?;
        }
        write!(f, ")")
    }
}

impl PartialEq<Type> for UnionType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_union() {
            self.types.len() == other.types.len()
                && self.types.iter().all(|ty| other.types.contains(ty))
        } else {
            false
        }
    }
}
