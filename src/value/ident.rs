use std::ptr::NonNull;

use crate::{
    mem::{Slot, MutatorRef, Pointer},
    value::SmartString,
};

use super::{Pair, Value, ValueKind, Vector};

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
pub struct ValueId(ValueTag, usize);

impl ValueId {
    /// Given the [`Mutator`](crate::mem::Mutator), converts this [`ValueId`]
    /// into a [`Value`]. Returns `None` if the [`ValueId`] is invalid for the
    /// given [`Mutator`](crate::mem::Mutator).
    pub fn into_value(self, m: MutatorRef<'_>) -> Option<Value<'_>> {
        match self.0 {
            ValueTag::Nil => Some(Value::new_nil()),
            ValueTag::Bool => Some(Value::new_bool(self.1 != 0)),
            ValueTag::Int => Some(Value::new_int(self.1 as i64)),
            ValueTag::Float => Some(Value::new_float(self.1 as f64)),
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
                let ptr = NonNull::new(self.1 as *mut Slot<Vec<u8>>)?;
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
        }
    }
}

impl From<&Value<'_>> for ValueId {
    #[inline]
    fn from(value: &Value<'_>) -> Self {
        match value.kind() {
            ValueKind::Nil => Self(ValueTag::Nil, 0),
            ValueKind::Bool(value) => Self(ValueTag::Bool, *value as usize),
            ValueKind::Integer(value) => Self(ValueTag::Int, *value as usize),
            ValueKind::Float(value) => Self(ValueTag::Float, *value as usize),
            ValueKind::Char(value) => Self(ValueTag::Char, *value as usize),
            ValueKind::String(_, value) => Self(ValueTag::String, value.as_ptr() as usize),
            ValueKind::Symbol(_, value) => Self(ValueTag::Symbol, value.as_ptr() as usize),
            ValueKind::Bytevec(_, value) => Self(ValueTag::Bytevec, value.as_ptr() as usize),
            ValueKind::Vector(_, value) => Self(ValueTag::Vector, value.as_ptr() as usize),
            ValueKind::Pair(_, value) => Self(ValueTag::Pair, value.as_ptr() as usize),
        }
    }
}

impl From<&Pointer<'_, SmartString>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, SmartString>) -> Self {
        Self(ValueTag::String, ptr.as_ptr() as usize)
    }
}

impl From<&Pointer<'_, Box<str>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Box<str>>) -> Self {
        Self(ValueTag::Symbol, ptr.as_ptr() as usize)
    }
}

impl From<&Pointer<'_, Vec<u8>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Vec<u8>>) -> Self {
        Self(ValueTag::Bytevec, ptr.as_ptr() as usize)
    }
}

impl From<&Pointer<'_, Vector<'_>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Vector<'_>>) -> Self {
        Self(ValueTag::Vector, ptr.as_ptr() as usize)
    }
}

impl From<&Pointer<'_, Pair<'_>>> for ValueId {
    #[inline]
    fn from(ptr: &Pointer<'_, Pair<'_>>) -> Self {
        Self(ValueTag::Pair, ptr.as_ptr() as usize)
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
}

#[cfg(test)]
mod tests {}
