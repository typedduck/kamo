use std::ptr::NonNull;

#[cfg(feature = "types")]
use crate::types::Type;
use crate::{
    mem::{MutatorRef, Pointer, Slot},
    value::SmartString,
};

use super::{ByteVector, Pair, Value, ValueKind, Vector};

/// A unique identifier for a [`Value`].
///
/// This is used to identify a [`Value`] without having to borrow the [`Value`]
/// itself. This is useful for storing [`Value`]s as keys in a
/// [`HashMap`](std::collections::HashMap). It is also useful for storing
/// [`Value`]s in a [`HashSet`](std::collections::HashSet) because it is
/// guaranteed that two [`ValueId`]s are equal if and only if the corresponding
/// [`Value`]s are identical. This is achieved by storing the address of the
/// [`Value`]s as the identifier along with a tag that identifies the type of
/// [`Value`].
///
/// Given the [`Mutator`](crate::mem::Mutator) where the original [`Value`] was
/// allocated, a [`ValueId`] can be converted back into a [`Value`] using
/// [`ValueId::into_value()`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(ValueTag, u64);

impl ValueId {
    /// Given the [`Mutator`](crate::mem::Mutator), converts this [`ValueId`]
    /// into a [`Value`]. Returns `None` if the [`ValueId`] is invalid for the
    /// given [`Mutator`](crate::mem::Mutator).
    #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
    pub fn into_value<'a>(self, m: &MutatorRef<'a>) -> Option<Value<'a>> {
        // Casting with truncation is safe because it inverst the conversion
        // from a `Value`.
        match self.0 {
            ValueTag::Nil => Some(Value::new_nil()),
            ValueTag::Bool => Some(Value::new_bool(self.1 != 0)),
            ValueTag::Int => Some(Value::new_int(self.1 as i64)),
            ValueTag::Float => Some(Value::new_float(f64::from_bits(self.1))),
            ValueTag::Char => char::from_u32(self.1 as u32).map(Value::new_char),
            ValueTag::String => {
                let ptr = NonNull::new(self.1 as *mut Slot<SmartString>)?;
                Some(m.borrow_mut().into_string(ptr)?.into())
            }
            ValueTag::Symbol => {
                let ptr = NonNull::new(self.1 as *mut Slot<Box<str>>)?;
                Some(m.borrow_mut().into_symbol(ptr)?.into())
            }
            ValueTag::Bytevec => {
                let ptr = NonNull::new(self.1 as *mut Slot<ByteVector>)?;
                Some(m.borrow_mut().into_bytevec(ptr)?.into())
            }
            ValueTag::Vector => {
                let ptr = NonNull::new(self.1 as *mut Slot<Vector<'_>>)?;
                Some(m.borrow_mut().into_vector(ptr)?.into())
            }
            ValueTag::Pair => {
                let ptr = NonNull::new(self.1 as *mut Slot<Pair<'_>>)?;
                Some(m.borrow_mut().into_pair(ptr)?.into())
            }
            #[cfg(feature = "types")]
            ValueTag::Type => {
                let ptr = NonNull::new(self.1 as *mut Slot<Type>)?;
                Some(m.borrow_mut().into_type(ptr)?.into())
            }
        }
    }

    /// Returns the tag that identifies the type of [`Value`] that this
    /// [`ValueId`] corresponds to.
    #[must_use]
    pub const fn tag(self) -> ValueTag {
        self.0
    }
}

impl From<&Value<'_>> for ValueId {
    #[allow(clippy::cast_sign_loss)]
    #[inline]
    fn from(value: &Value<'_>) -> Self {
        match value.kind() {
            ValueKind::Nil => Self(ValueTag::Nil, 0),
            ValueKind::Bool(value) => Self(ValueTag::Bool, u64::from(*value)),
            ValueKind::Integer(value) => Self(ValueTag::Int, *value as u64),
            ValueKind::Float(value) => Self(ValueTag::Float, value.to_bits()),
            ValueKind::Char(value) => Self(ValueTag::Char, *value as u64),
            ValueKind::String(_, value) => Self(ValueTag::String, value.as_ptr() as u64),
            ValueKind::Symbol(_, value) => Self(ValueTag::Symbol, value.as_ptr() as u64),
            ValueKind::Bytevec(_, value) => Self(ValueTag::Bytevec, value.as_ptr() as u64),
            ValueKind::Vector(_, value) => Self(ValueTag::Vector, value.as_ptr() as u64),
            ValueKind::Pair(_, value) => Self(ValueTag::Pair, value.as_ptr() as u64),
            #[cfg(feature = "types")]
            ValueKind::Type(_, value) => Self(ValueTag::Type, value.as_ptr() as u64),
        }
    }
}

impl From<&Pointer<'_, SmartString>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, SmartString>) -> Self {
        Self(ValueTag::String, ptr.as_ptr() as u64)
    }
}

impl From<&Pointer<'_, Box<str>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Box<str>>) -> Self {
        Self(ValueTag::Symbol, ptr.as_ptr() as u64)
    }
}

impl From<&Pointer<'_, ByteVector>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, ByteVector>) -> Self {
        Self(ValueTag::Bytevec, ptr.as_ptr() as u64)
    }
}

impl From<&Pointer<'_, Vector<'_>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Vector<'_>>) -> Self {
        Self(ValueTag::Vector, ptr.as_ptr() as u64)
    }
}

impl From<&Pointer<'_, Pair<'_>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Pair<'_>>) -> Self {
        Self(ValueTag::Pair, ptr.as_ptr() as u64)
    }
}

#[cfg(feature = "types")]
#[cfg_attr(docsrs, doc(cfg(feature = "types")))]
impl From<&Pointer<'_, Type>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Type>) -> Self {
        Self(ValueTag::Type, ptr.as_ptr() as u64)
    }
}

/// A tag that identifies the type of a [`Value`].
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueTag {
    Nil,
    Bool,
    Int,
    Float,
    Char,
    String,
    Symbol,
    Bytevec,
    Vector,
    Pair,
    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    Type,
}

#[cfg(test)]
mod tests {
    use crate::mem::Mutator;

    use super::*;

    const BOOLS: &[bool] = &[false, true];

    #[test]
    fn cast_bool() {
        let mutator = Mutator::new_ref();

        for boolean in BOOLS.iter().copied() {
            let value = Value::new_bool(boolean);
            let id = ValueId::from(&value);
            assert_eq!(id.into_value(&mutator), Some(value));
        }
    }

    const CHARS: &[char] = &[
        'a',
        'b',
        'c',
        'd',
        'e',
        'f',
        'g',
        'h',
        'i',
        'j',
        '🦀',
        '\u{10ffff}',
    ];

    #[test]
    fn cast_char() {
        let mutator = Mutator::new_ref();

        for character in CHARS.iter().copied() {
            let value = Value::new_char(character);
            let id = ValueId::from(&value);
            assert_eq!(id.into_value(&mutator), Some(value));
        }
    }

    const INTEGERS: &[i64] = &[0, 42, -42, i64::MIN, i64::MAX];

    #[test]
    fn cast_integer() {
        let mutator = Mutator::new_ref();

        for integer in INTEGERS.iter().copied() {
            let value = Value::new_int(integer);
            let id = ValueId::from(&value);
            assert_eq!(id.into_value(&mutator), Some(value));
        }
    }

    const FLOATS: &[f64] = &[
        0.0,
        42.0,
        -42.0,
        f64::MIN,
        f64::MAX,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
    ];

    #[test]
    fn cast_float() {
        let mutator = Mutator::new_ref();

        for float in FLOATS.iter().copied() {
            let value = Value::new_float(float);
            let id = ValueId::from(&value);
            assert_eq!(id.into_value(&mutator), Some(value));
        }
    }
}
