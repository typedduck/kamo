use std::fmt;

use super::Type;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType {
    pub elem: Type,
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LambdaType {
    pub args: Vec<Type>,
    pub varg: Option<Type>,
    pub ret: Type,
}

impl fmt::Display for LambdaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        match (self.args.len(), &self.varg) {
            (0, None) => write!(f, "void")?,
            (0, Some(varg)) => write!(f, "...{varg}")?,
            (1, None) => write!(f, "{}", self.args[0])?,
            (1, Some(varg)) => write!(f, "{}, ...{}", self.args[0], varg)?,
            (n, None) => {
                for arg in &self.args[..n - 1] {
                    write!(f, "{arg}, ")?;
                }
                write!(f, "{}", self.args[n - 1])?;
            }
            (n, Some(varg)) => {
                for arg in &self.args[..n - 1] {
                    write!(f, "{arg}, ")?;
                }
                write!(f, "{}, ...{}", self.args[n - 1], varg)?;
            }
        }
        write!(f, " -> {})", self.ret)
    }
}

impl PartialEq<Type> for LambdaType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_lambda() {
            self.args.len() == other.args.len()
                && self.args == other.args
                && self.varg == other.varg
                && self.ret == other.ret
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OptionType {
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PairType {
    pub car: Type,
    pub cdr: Type,
}

impl fmt::Display for PairType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} * {})", self.car, self.cdr)
    }
}

impl PartialEq<Type> for PairType {
    fn eq(&self, other: &Type) -> bool {
        if let Some(other) = other.as_pair() {
            self.car == other.car && self.cdr == other.cdr
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnionType {
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
