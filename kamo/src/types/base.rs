use std::{
    fmt,
    hash::{Hash, Hasher},
    slice::Iter,
    str::FromStr,
};

use crate::{
    env::Parameters,
    mem::{Root, Trace},
    parser::{Input, ParseError},
};

use super::{
    parser::{binary, code, text, TypeCodeError},
    ArrayType, LambdaType, OptionType, PairType, TypeError, TypeVec, UnionType,
};

/// # Grammar:
///
/// The binary representation of a type is a sequence of type codes. The type
/// codes are defined in the [`code`] module. The grammar of the type code is
/// defined as follows:
///
/// ```text
/// type             = (filled | option | code::VOID | code::NIL) eof
/// filled           = any | union | specific
/// specific         = code::TYPE | code::BOOL | code::CHAR | code::INT
///                  | code::FLOAT | code::SYMBOL | code::BINARY
///                  | array | pair | lambda
/// filled-or-option = filled | option
/// array            = code::ARRAY filled-or-option fixed
/// pair             = code::PAIR filled-or-option filled-or-option
/// option           = code::OPTION filled
/// union            = code::UNION specific specific+ code::END
/// lambda           = code::LAMBDA args varg return
/// args             = (filled-or-option)*
/// varg             = (code::VARIADIC filled-or-option)?
/// return           = code::RETURN (code::VOID | filled-or-option)
/// fixed            = (fixed0 | fixed1 | fixed2 | fixed3 | fixed4)?
/// fixed0           = code::FIXED0..=code::FIXED0_MAX
/// fixed1           = code::FIXED1..=code::FIXED1_MAX byte
/// fixed2           = code::FIXED2..=code::FIXED2_MAX byte byte
/// fixed3           = code::FIXED3..=code::FIXED3_MAX byte byte byte
/// fixed4           = code::FIXED4..=code::FIXED4_MAX byte byte byte byte
/// byte             = 0x00..=0xFF
/// ```
#[derive(Clone, Debug, Eq)]
pub struct Type(TypeVec);

impl Type {
    /// Creates a new type by parsing the type code.
    ///
    /// # Errors
    ///
    /// Returns an error if the type code is invalid.
    #[inline]
    pub fn new(code: impl AsRef<[u8]>) -> Result<Self, TypeCodeError> {
        binary::parse(code.as_ref())
    }

    /// Creates a new type by taking the raw code.
    ///
    /// # Safety
    ///
    /// The type code is taken as is. It is the responsibility of the caller,
    /// that the type code is correct.
    #[must_use]
    #[inline]
    pub unsafe fn new_unchecked(code: impl AsRef<[u8]>) -> Self {
        Self(code.as_ref().into())
    }

    /// Returns the inner type code which is a sequence of type codes by
    /// consuming the type.
    #[must_use]
    #[inline]
    pub fn into_inner(self) -> TypeVec {
        self.0
    }

    /// Creates a new array type. If the size is not specified, the array is
    /// dynamic.
    ///
    /// # Errors
    ///
    /// Returns an error if the element type is not filled or an option or if the
    /// size is larger than [`ARRAY_MAX`](super::ARRAY_MAX).
    #[allow(clippy::cast_possible_truncation)]
    pub fn array(elem: Self, size: Option<usize>) -> Result<Self, TypeError> {
        let mut code = TypeVec::default();
        code.push(code::ARRAY);

        if !(elem.is_filled() || elem.is_option()) {
            return Err(TypeError::ArrayElemNotFilled);
        }
        code.extend(elem.0);

        match size {
            Some(size @ 0x0000_0000_0000..=0x0000_0000_000f) => {
                code.push(code::FIXED0 | size as u8);
            }
            Some(size @ 0x0000_0000_0010..=0x0000_0000_0fff) => {
                code.push(code::FIXED1 | (size >> 8) as u8);
                code.push(size as u8);
            }
            Some(size @ 0x0000_0000_1000..=0x0000_000f_ffff) => {
                code.push(code::FIXED2 | (size >> 16) as u8);
                code.push((size >> 8) as u8);
                code.push(size as u8);
            }
            Some(size @ 0x0000_0010_0000..=0x0000_0fff_ffff) => {
                code.push(code::FIXED3 | (size >> 24) as u8);
                code.push((size >> 16) as u8);
                code.push((size >> 8) as u8);
                code.push(size as u8);
            }
            Some(size @ 0x0000_1000_0000..=0x000f_ffff_ffff) => {
                code.push(code::FIXED4 | (size >> 32) as u8);
                code.push((size >> 24) as u8);
                code.push((size >> 16) as u8);
                code.push((size >> 8) as u8);
                code.push(size as u8);
            }
            Some(size) => return Err(TypeError::ArraySizeTooLarge(size)),
            None => {}
        }
        Ok(Self(code))
    }

    /// Returns `true` if the type is an array type.
    #[must_use]
    #[inline]
    pub fn is_array(&self) -> bool {
        self.0.first() == Some(&code::ARRAY)
    }

    /// Returns [`ArrayType`] if the type is an array type.
    ///
    /// # Panics
    ///
    /// Panics if the type is a malformed array type. This is a bug and should be
    /// reported.
    #[must_use]
    pub fn as_array(&self) -> Option<ArrayType> {
        if self.is_array() {
            let input = &self.0.as_slice()[1..];
            let (elem, cursor) = binary::parse_filled_or_option(input, 0).expect("array elem-type");
            let (len, _) =
                binary::parse_fixed(cursor, input.len() - cursor.len()).expect("array size");

            Some(ArrayType { elem, len })
        } else {
            None
        }
    }

    /// Creates a new lambda type.
    ///
    /// # Errors
    ///
    /// Returns an error if an parameter type is not filled or an option, if the
    /// variadic type is not filled or an option or if the return type is not
    /// filled, an option or `void`.
    pub fn lambda<I>(args: I, varg: Option<Self>, ret: Self) -> Result<Self, TypeError>
    where
        I: IntoIterator<Item = Self>,
    {
        let mut code = TypeVec::default();
        code.push(code::LAMBDA);
        for (i, arg) in args.into_iter().enumerate() {
            if !(arg.is_filled() || arg.is_option()) {
                return Err(TypeError::ParamNotFilled(i + 1));
            }
            code.extend(arg.0);
        }
        if let Some(varg) = varg {
            if !(varg.is_filled() || varg.is_option()) {
                return Err(TypeError::VariadicNotFilled);
            }
            code.push(code::VARIADIC);
            code.extend(varg.0);
        }
        code.push(code::RETURN);
        code.extend(ret.0);
        Ok(Self(code))
    }

    /// Returns `true` if the type is a lambda type.
    #[must_use]
    #[inline]
    pub fn is_lambda(&self) -> bool {
        self.0.first() == Some(&code::LAMBDA)
    }

    /// Returns [`LambdaType`] if the type is a lambda type.
    ///
    /// # Panics
    ///
    /// Panics if the type is a malformed lambda type. This is a bug and should
    /// be reported.
    #[must_use]
    pub fn as_lambda(&self) -> Option<LambdaType> {
        if self.is_lambda() {
            let code = &self.0.as_slice()[1..];
            let (args, code) = binary::parse_args(code, 0).expect("lambda arg-types");
            let (varg, code) = binary::parse_varg(code, 0).expect("lambda varg-type");
            let (ret, _) = binary::parse_return(code, 0).expect("lambda return-type");

            Some(LambdaType {
                params: args,
                variadic: varg,
                result: ret,
            })
        } else {
            None
        }
    }

    /// Creates a new option type.
    ///
    /// # Errors
    ///
    /// Returns an error if the inner type is not filled or if the inner type is
    /// an option itself.
    pub fn option(ty: Self) -> Result<Self, TypeError> {
        let mut code = TypeVec::default();

        if ty.is_option() {
            return Err(TypeError::NestedOption);
        }
        if !ty.is_filled() {
            return Err(TypeError::OptionNotFilled);
        }
        code.push(code::OPTION);
        code.extend(ty.0);
        Ok(Self(code))
    }

    /// Returns `true` if the type is an option type.
    #[must_use]
    #[inline]
    pub fn is_option(&self) -> bool {
        self.0.first() == Some(&code::OPTION)
    }

    /// Returns [`OptionType`] if the type is an option type.
    ///
    /// # Panics
    ///
    /// Panics if the type is a malformed option type. This is a bug and should
    /// be reported.
    #[must_use]
    pub fn as_option(&self) -> Option<OptionType> {
        if self.is_option() {
            let code = &self.0.as_slice()[1..];
            let (some, _) = binary::parse_filled(code, 0).expect("option some-type");

            Some(OptionType { some })
        } else {
            None
        }
    }

    /// Creates a new pair type.
    ///
    /// # Errors
    ///
    /// Returns an error if the car or cdr type is not filled or an option.
    #[allow(clippy::similar_names)]
    pub fn pair(car: Self, cdr: Self) -> Result<Self, TypeError> {
        let mut code = TypeVec::default();
        code.push(code::PAIR);
        if !(car.is_filled() || car.is_option()) {
            return Err(TypeError::PairCarNotFilled);
        }
        code.extend(car.0);
        if !(cdr.is_filled() || cdr.is_option()) {
            return Err(TypeError::PairCdrNotFilled);
        }
        code.extend(cdr.0);
        Ok(Self(code))
    }

    /// Returns `true` if the type is a pair type.
    #[must_use]
    #[inline]
    pub fn is_pair(&self) -> bool {
        self.0.first() == Some(&code::PAIR)
    }

    /// Returns [`PairType`] if the type is a pair type.
    ///
    /// # Panics
    ///
    /// Panics if the type is a malformed pair type. This is a bug and should be
    /// reported.
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn as_pair(&self) -> Option<PairType> {
        if self.is_pair() {
            let code = &self.0.as_slice()[1..];
            let (car, code) = binary::parse_filled_or_option(code, 0).expect("pair car-type");
            let (cdr, _) = binary::parse_filled_or_option(code, 0).expect("pair cdr-type");

            Some(PairType { car, cdr })
        } else {
            None
        }
    }

    /// Creates a new union type. The member types must be specific and there
    /// must be at least two members.
    ///
    /// # Errors
    ///
    /// Returns an error if a member type is not specific or if there are less
    /// than two members.
    pub fn union<I>(types: I) -> Result<Self, TypeError>
    where
        I: IntoIterator<Item = Self>,
    {
        let mut code = TypeVec::default();
        let mut count = 0;

        code.push(code::UNION);
        for ty in types {
            if !ty.is_specific() {
                return Err(TypeError::UnionNotSpecific(ty));
            }
            code.extend(ty.0);
            count += 1;
        }
        if count < 2 {
            return Err(TypeError::UnionTooFewMembers);
        }
        code.push(code::END);
        Ok(Self(code))
    }

    /// Returns `true` if the type is a union type.
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn is_union(&self) -> bool {
        self.0.first() == Some(&code::UNION)
    }

    /// Returns [`UnionType`] if the type is a union type.
    ///
    /// # Panics
    ///
    /// Panics if the type is a malformed union type. This is a bug and should be
    /// reported.
    #[must_use]
    pub fn as_union(&self) -> Option<UnionType> {
        if self.is_union() {
            let mut code = &self.0.as_slice()[1..];
            let mut types = Vec::new();
            let mut offset = 1;

            loop {
                assert!(!code.is_empty(), "union not terminated");
                if code.first() == Some(&code::END) {
                    break;
                }
                match binary::parse_specific(code, offset) {
                    Ok((ty, rest)) => {
                        types.push(ty);
                        offset += code.len() - rest.len();
                        code = rest;
                    }
                    Err(err) => panic!("{err}"),
                }
            }
            assert!(types.len() >= 2, "union with less than 2 members");
            Some(UnionType { types })
        } else {
            None
        }
    }

    /// Returns `true` if the type is a specific type.
    #[must_use]
    pub fn is_specific(&self) -> bool {
        matches!(
            self.0.as_slice(),
            [code::TYPE
                | code::BOOL
                | code::CHAR
                | code::INT
                | code::FLOAT
                | code::BINARY
                | code::SYMBOL]
                | [code::ARRAY | code::PAIR | code::LAMBDA, ..]
        )
    }

    /// Returns `true` if the type is a filled type.
    #[must_use]
    #[inline]
    pub fn is_filled(&self) -> bool {
        self.is_specific() || self.is_any() || self.is_union()
    }

    /// Returns an iterator over the type code.
    #[inline]
    pub fn iter(&self) -> Iter<'_, u8> {
        self.0.iter()
    }

    /// Returns the type code as a slice.
    #[must_use]
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl<'a> IntoIterator for &'a Type {
    type IntoIter = std::slice::Iter<'a, u8>;
    type Item = &'a u8;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_slice().hash(state);
    }
}

impl PartialEq<Self> for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.0.as_slice() == other.0.as_slice() {
            return true;
        }

        if let Some(lhs) = self.as_option() {
            return &lhs == other;
        }
        if let Some(lhs) = self.as_array() {
            return &lhs == other;
        }
        if let Some(lhs) = self.as_pair() {
            return &lhs == other;
        }
        if let Some(lhs) = self.as_lambda() {
            return &lhs == other;
        }
        false
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.first().copied() {
            Some(code::NIL) => write!(f, "nil"),
            Some(code::VOID) => write!(f, "void"),
            Some(code::ANY) => write!(f, "any"),
            Some(code::TYPE) => write!(f, "type"),
            Some(code::BOOL) => write!(f, "bool"),
            Some(code::CHAR) => write!(f, "char"),
            Some(code::INT) => write!(f, "int"),
            Some(code::FLOAT) => write!(f, "float"),
            Some(code::SYMBOL) => write!(f, "symbol"),
            Some(code::BINARY) => write!(f, "binary"),
            Some(code::ARRAY) => self.as_array().expect("array type").fmt(f),
            Some(code::PAIR) => self.as_pair().expect("pair type").fmt(f),
            Some(code::LAMBDA) => self.as_lambda().expect("lambda type").fmt(f),
            Some(code::OPTION) => self.as_option().expect("option type").fmt(f),
            Some(code::UNION) => self.as_union().expect("union type").fmt(f),
            Some(_) | None => Ok(()),
        }
    }
}

impl<'a> Trace<'a> for Type {
    fn trace(&self, _: &mut Vec<Root<'a>>) {
        // A type is always traced by the mutator.
    }
}

macro_rules! impl_simple {
    ($($name:ident => $value:expr),+ $(),*) => {
        impl Type {
            $(#[must_use] #[inline] pub fn $name() -> Self { $value.clone() })+
            paste::paste! {
                $(#[must_use] #[inline] pub fn [<is_ $name>](&self) -> bool { *self == $value })+
            }
        }
    };
}

impl_simple! {
    nil     => *code::T_NIL,    void      => *code::T_VOID,
    any     => *code::T_ANY,    typedef   => *code::T_TYPE,
    boolean => *code::T_BOOL,   character => *code::T_CHAR,
    integer => *code::T_INT,    float     => *code::T_FLOAT,
    string  => *code::T_STRING, symbol    => *code::T_SYMBOL,
    binary  => *code::T_BINARY, vector    => *code::T_VECTOR,
    list    => *code::T_LIST,
}

impl AsRef<Self> for Type {
    #[inline]
    fn as_ref(&self) -> &Self {
        self
    }
}

impl FromStr for Type {
    type Err = ParseError;

    /// Parses a type from the string. User defined types are not supported,
    /// because they cannot be resolved without an environment.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = Input::new(s);
        let (ty, _) = text::parse::<0>(None)(input)?;

        Ok(ty)
    }
}

impl<'a> From<(&Parameters<'a>, &Self)> for Type {
    fn from((args, ret): (&Parameters<'a>, &Self)) -> Self {
        let fixed = args
            .fixed
            .iter()
            .map(|arg| arg.1.clone())
            .collect::<Vec<_>>();
        let variadic = args.variadic.as_ref().map(|arg| arg.1.clone());
        Self::lambda(fixed, variadic, ret.to_owned()).expect("lambda type")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_create_success() {
        assert_eq!(Type::new([code::NIL]), Ok(Type::nil()));
        assert_eq!(Type::new([code::VOID]), Ok(Type::void()));
        assert_eq!(Type::new([code::ANY]), Ok(Type::any()));
        assert_eq!(Type::new([code::BOOL]), Ok(Type::boolean()));
        assert_eq!(Type::new([code::CHAR]), Ok(Type::character()));
        assert_eq!(Type::new([code::INT]), Ok(Type::integer()));
        assert_eq!(Type::new([code::FLOAT]), Ok(Type::float()));
        assert_eq!(Type::new([code::SYMBOL]), Ok(Type::symbol()));
        assert_eq!(Type::new([code::BINARY]), Ok(Type::binary()));
        assert_eq!(Type::new([code::ARRAY, code::CHAR]), Ok(Type::string()));
        assert_eq!(Type::new([code::ARRAY, code::ANY]), Ok(Type::vector()));
        assert_eq!(
            Type::new([code::ARRAY, code::INT]),
            Ok(Type::array(Type::integer(), None).unwrap())
        );
        assert_eq!(
            Type::new([code::PAIR, code::ANY, code::ANY]),
            Ok(Type::pair(Type::any(), Type::any()).unwrap())
        );
        assert_eq!(
            Type::new([code::LAMBDA, code::ANY, code::ANY, code::RETURN, code::ANY]),
            Ok(Type::lambda(vec![Type::any(), Type::any()], None, Type::any()).unwrap())
        );
        assert_eq!(
            Type::new([
                code::OPTION,
                code::LAMBDA,
                code::ANY,
                code::ANY,
                code::RETURN,
                code::ANY
            ]),
            Ok(Type::option(
                Type::lambda(vec![Type::any(), Type::any()], None, Type::any()).unwrap()
            )
            .unwrap())
        );
        assert_eq!(
            Type::new([
                code::LAMBDA,
                code::ANY,
                code::ANY,
                code::VARIADIC,
                code::ANY,
                code::RETURN,
                code::ANY
            ]),
            Ok(Type::lambda(
                vec![Type::any(), Type::any()],
                Some(Type::any()),
                Type::any()
            )
            .unwrap())
        );
        assert_eq!(
            Type::new([code::UNION, code::INT, code::ARRAY, code::CHAR, code::END]),
            Ok(Type::union(vec![
                Type::integer(),
                Type::array(Type::character(), None).unwrap()
            ])
            .unwrap())
        );
        assert_eq!(
            Type::new([
                code::OPTION,
                code::UNION,
                code::INT,
                code::ARRAY,
                code::CHAR,
                code::END
            ]),
            Ok(Type::option(
                Type::union(vec![
                    Type::integer(),
                    Type::array(Type::character(), None).unwrap()
                ])
                .unwrap()
            )
            .unwrap())
        );
        assert_eq!(
            Type::new([
                code::UNION,
                code::BOOL,
                code::INT,
                code::ARRAY,
                code::CHAR,
                code::END
            ]),
            Ok(Type::union(vec![
                Type::boolean(),
                Type::integer(),
                Type::array(Type::character(), None).unwrap()
            ])
            .unwrap())
        );
    }

    #[test]
    fn type_create_failure() {
        assert_eq!(
            Type::new([code::ARRAY]),
            Err(TypeCodeError::ExpectedTypeOrOption(1))
        );
        assert_eq!(
            Type::new([code::ARRAY, code::ARRAY]),
            Err(TypeCodeError::ExpectedTypeOrOption(2))
        );
        assert_eq!(
            Type::new([code::ARRAY, code::ARRAY, code::ARRAY]),
            Err(TypeCodeError::ExpectedTypeOrOption(3))
        );
        assert_eq!(
            Type::new([code::PAIR]),
            Err(TypeCodeError::ExpectedTypeOrOption(1))
        );
        assert_eq!(
            Type::new([code::PAIR, code::ANY]),
            Err(TypeCodeError::ExpectedTypeOrOption(3))
        );
        assert_eq!(
            Type::new([code::PAIR, code::PAIR, code::ANY]),
            Err(TypeCodeError::ExpectedTypeOrOption(4))
        );
        assert_eq!(
            Type::new([code::LAMBDA]),
            Err(TypeCodeError::ExpectedReturn(1))
        );
        assert_eq!(
            Type::new([code::LAMBDA, code::ANY]),
            Err(TypeCodeError::ExpectedReturn(2))
        );
        assert_eq!(
            Type::new([code::LAMBDA, code::VARIADIC]),
            Err(TypeCodeError::ExpectedTypeOrOption(2))
        );
        assert_eq!(
            Type::new([code::LAMBDA, code::ANY, code::VARIADIC]),
            Err(TypeCodeError::ExpectedTypeOrOption(3))
        );
        assert_eq!(
            Type::new([code::UNION]),
            Err(TypeCodeError::ExpectedSpecificTypeEof(1))
        );
        assert_eq!(
            Type::new([code::UNION, code::END]),
            Err(TypeCodeError::ExpectedSpecificType(1))
        );
        assert_eq!(
            Type::new([code::UNION, code::OPTION, code::BOOL, code::END]),
            Err(TypeCodeError::ExpectedSpecificType(1))
        );
        assert_eq!(
            Type::new([code::UNION, code::BOOL, code::END]),
            Err(TypeCodeError::ExpectedSpecificType(2))
        );
        assert_eq!(
            Type::new([code::UNION, code::BOOL, code::CHAR]),
            Err(TypeCodeError::ExpectedMemberOrEnd(3))
        );
    }

    #[test]
    fn type_is() {
        assert!(Type::nil().is_nil());
        assert!(Type::void().is_void());
        assert!(Type::any().is_any());
        assert!(Type::boolean().is_boolean());
        assert!(Type::character().is_character());
        assert!(Type::integer().is_integer());
        assert!(Type::float().is_float());
        assert!(Type::string().is_string());
        assert!(Type::symbol().is_symbol());
        assert!(Type::binary().is_binary());
        assert!(Type::vector().is_vector());
        assert!(Type::pair(Type::any(), Type::any()).unwrap().is_pair());
        assert!(Type::array(Type::any(), None).unwrap().is_array());
        assert!(Type::lambda(vec![], None, Type::any()).unwrap().is_lambda());
    }

    #[test]
    fn type_as_array() {
        assert_eq!(Type::nil().as_array(), None);
        assert_eq!(Type::void().as_array(), None);
        assert_eq!(Type::any().as_array(), None);
        assert_eq!(Type::boolean().as_array(), None);
        assert_eq!(Type::character().as_array(), None);
        assert_eq!(Type::integer().as_array(), None);
        assert_eq!(Type::float().as_array(), None);
        assert_eq!(
            Type::string().as_array(),
            Some(ArrayType {
                elem: Type::character(),
                len: None
            })
        );
        assert_eq!(Type::symbol().as_array(), None);
        assert_eq!(Type::binary().as_array(), None);
        assert_eq!(
            Type::vector().as_array(),
            Some(ArrayType {
                elem: Type::any(),
                len: None
            })
        );
        assert_eq!(
            Type::pair(Type::any(), Type::any()).unwrap().as_array(),
            None
        );
        assert_eq!(
            Type::lambda(vec![], None, Type::any()).unwrap().as_array(),
            None
        );
    }

    #[test]
    fn type_as_pair() {
        assert_eq!(Type::nil().as_pair(), None);
        assert_eq!(Type::void().as_pair(), None);
        assert_eq!(Type::any().as_pair(), None);
        assert_eq!(Type::boolean().as_pair(), None);
        assert_eq!(Type::character().as_pair(), None);
        assert_eq!(Type::integer().as_pair(), None);
        assert_eq!(Type::float().as_pair(), None);
        assert_eq!(Type::string().as_pair(), None);
        assert_eq!(Type::symbol().as_pair(), None);
        assert_eq!(Type::binary().as_pair(), None);
        assert_eq!(Type::vector().as_pair(), None);
        assert_eq!(
            Type::pair(Type::any(), Type::any()).unwrap().as_pair(),
            Some(PairType {
                car: Type::any(),
                cdr: Type::any()
            })
        );
        assert_eq!(
            Type::lambda(vec![], None, Type::any()).unwrap().as_pair(),
            None
        );
    }

    #[test]
    fn type_as_lambda() {
        assert_eq!(Type::nil().as_lambda(), None);
        assert_eq!(Type::void().as_lambda(), None);
        assert_eq!(Type::any().as_lambda(), None);
        assert_eq!(Type::boolean().as_lambda(), None);
        assert_eq!(Type::character().as_lambda(), None);
        assert_eq!(Type::integer().as_lambda(), None);
        assert_eq!(Type::float().as_lambda(), None);
        assert_eq!(Type::string().as_lambda(), None);
        assert_eq!(Type::symbol().as_lambda(), None);
        assert_eq!(Type::binary().as_lambda(), None);
        assert_eq!(Type::vector().as_lambda(), None);
        assert_eq!(
            Type::pair(Type::any(), Type::any()).unwrap().as_lambda(),
            None
        );
        assert_eq!(
            Type::lambda(vec![], None, Type::any()).unwrap().as_lambda(),
            Some(LambdaType {
                params: vec![],
                variadic: None,
                result: Type::any()
            })
        );
        assert_eq!(
            Type::lambda(vec![Type::any()], None, Type::any())
                .unwrap()
                .as_lambda(),
            Some(LambdaType {
                params: vec![Type::any()],
                variadic: None,
                result: Type::any()
            })
        );
        assert_eq!(
            Type::lambda(vec![Type::any()], Some(Type::any()), Type::any())
                .unwrap()
                .as_lambda(),
            Some(LambdaType {
                params: vec![Type::any()],
                variadic: Some(Type::any()),
                result: Type::any()
            })
        );
        assert_eq!(
            Type::lambda(vec![Type::any(), Type::any()], None, Type::any())
                .unwrap()
                .as_lambda(),
            Some(LambdaType {
                params: vec![Type::any(), Type::any()],
                variadic: None,
                result: Type::any()
            })
        );
        assert_eq!(
            Type::lambda(
                vec![Type::any(), Type::any()],
                Some(Type::any()),
                Type::any()
            )
            .unwrap()
            .as_lambda(),
            Some(LambdaType {
                params: vec![Type::any(), Type::any()],
                variadic: Some(Type::any()),
                result: Type::any()
            })
        );
    }

    #[test]
    fn type_display() {
        assert_eq!(format!("{}", Type::nil()), "nil");
        assert_eq!(format!("{}", Type::void()), "void");
        assert_eq!(format!("{}", Type::any()), "any");
        assert_eq!(format!("{}", Type::boolean()), "bool");
        assert_eq!(format!("{}", Type::character()), "char");
        assert_eq!(format!("{}", Type::integer()), "int");
        assert_eq!(format!("{}", Type::float()), "float");
        assert_eq!(format!("{}", Type::string()), "[char]");
        assert_eq!(format!("{}", Type::symbol()), "symbol");
        assert_eq!(format!("{}", Type::binary()), "binary");
        assert_eq!(format!("{}", Type::vector()), "[any]");
        assert_eq!(
            format!("{}", Type::pair(Type::any(), Type::any()).unwrap()),
            "(any * any)"
        );
        assert_eq!(
            format!("{}", Type::array(Type::any(), None).unwrap()),
            "[any]"
        );
        assert_eq!(
            format!("{}", Type::lambda(vec![], None, Type::void()).unwrap()),
            "fn(void -> void)"
        );
        assert_eq!(
            format!("{}", Type::lambda(vec![], None, Type::any()).unwrap()),
            "fn(void -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(vec![Type::any()], None, Type::any()).unwrap()
            ),
            "fn(any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(vec![Type::any()], Some(Type::any()), Type::any()).unwrap()
            ),
            "fn(any, ...any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(vec![Type::any(), Type::any()], None, Type::any()).unwrap()
            ),
            "fn(any, any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(
                    vec![Type::any(), Type::any()],
                    Some(Type::any()),
                    Type::any()
                )
                .unwrap()
            ),
            "fn(any, any, ...any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(
                    vec![Type::any(), Type::any(), Type::any()],
                    None,
                    Type::any()
                )
                .unwrap()
            ),
            "fn(any, any, any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(
                    vec![Type::any(), Type::any(), Type::any()],
                    Some(Type::any()),
                    Type::any()
                )
                .unwrap()
            ),
            "fn(any, any, any, ...any -> any)"
        );
        assert_eq!(
            format!(
                "{}",
                Type::lambda(
                    vec![Type::any(), Type::any()],
                    None,
                    Type::lambda(vec![Type::any()], None, Type::any()).unwrap()
                )
                .unwrap()
            ),
            "fn(any, any -> fn(any -> any))"
        );
    }

    lazy_static! {
        static ref TYPES: [(&'static str, Type); 19] = [
            ("nil", Type::nil()),
            ("void", Type::void()),
            ("any", Type::any()),
            ("bool", Type::boolean()),
            ("char", Type::character()),
            ("binary", Type::binary()),
            ("int", Type::integer()),
            ("float", Type::float()),
            ("symbol", Type::symbol()),
            ("type", Type::typedef()),
            // ("", Type::()),
            (
                "fn()",
                Type::lambda(
                    vec![],
                    None,
                    Type::void()
                )
                .unwrap()
            ),
            (
                "fn(void -> any)",
                Type::lambda(
                    vec![],
                    None,
                    Type::any()
                )
                .unwrap()
            ),
            (
                "fn(void -> void)",
                Type::lambda(
                    vec![],
                    None,
                    Type::void()
                )
                .unwrap()
            ),
            (
                "fn(void -> any)",
                Type::lambda(
                    vec![],
                    None,
                    Type::any()
                )
                .unwrap()
            ),
            (
                "fn(any -> void)",
                Type::lambda(
                    vec![Type::any()],
                    None,
                    Type::void()
                )
                .unwrap()
            ),
            (
                "fn(any -> any)",
                Type::lambda(
                    vec![Type::any()],
                    None,
                    Type::any()
                )
                .unwrap()
            ),
            (
                "fn(...any -> any)",
                Type::lambda(
                    vec![],
                    Some(Type::any()),
                    Type::any()
                )
                .unwrap()
            ),
            (
                "fn(any, any, any, ...any -> any)",
                Type::lambda(
                    vec![Type::any(), Type::any(), Type::any()],
                    Some(Type::any()),
                    Type::any()
                )
                .unwrap()
            ),
            (
                "fn(any, any -> fn(any -> any))",
                Type::lambda(
                    vec![Type::any(), Type::any()],
                    None,
                    Type::lambda(vec![Type::any()], None, Type::any()).unwrap()
                )
                .unwrap()
            ),
        ];
    }

    #[test]
    fn type_parse() {
        for (i, (s, ty)) in TYPES.iter().enumerate() {
            assert_eq!(Type::from_str(s), Ok(ty.clone()), "type {} failed", i + 1);
        }
    }

    lazy_static! {
        static ref EQUALS: [(&'static str, &'static str); 0] = [];
    }

    #[test]
    fn type_equals() {
        for (i, (t1, t2)) in EQUALS.iter().enumerate() {
            let t1 = Type::from_str(t1).unwrap_or_else(|_| panic!("{}: type 1", i + 1));
            let t2 = Type::from_str(t2).unwrap_or_else(|_| panic!("{}: type 2", i + 1));
            assert!(t1 == t2, "type {} failed", i + 1);
        }
    }
}
