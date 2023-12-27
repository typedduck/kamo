use std::{hash, ptr::NonNull};

use crate::{mem::Slot, value::SmartString};

use super::{Pair, ValueTag, Vector, ByteVector};

/// The `ValueKind` enum represents the different kinds of values that can be
/// stored in a [`Value`](super). It is the inner part of the `Value` struct and
/// is not meant to be used directly. Instead, the `Value` struct should be used
/// to access values.
/// 
/// This type is made public in order to allow efficient pattern matching on
/// values. For example, the following code checks if a value is a pair:
/// 
/// ```ignore
/// match value.kind() {
///    ValueKind::Pair(_, _) => println!("It's a pair!"),
///   _ => println!("It's not a pair!"),
/// }
/// ```
/// 
/// The following variants are all immediate values. They are stored directly in
/// the `Value` struct:
/// 
/// - `Nil` is the empty list.
/// - `Bool` is a boolean value.
/// - `Char` is a character.
/// - `Integer` is an integer.
/// - `Float` is a floating point number.
/// 
/// If the feature `evaluate` is enabled, the following variants are also
/// immediate values:
/// 
/// - `Primitive` is a primitive function.
/// - `Special` is a special form, which is evaluated at compile time.
/// 
/// The following variants are all pointers to entries maintained by the
/// memory manager [`Mutator`](crate::mem). They are all wrapped in a
/// [`NonNull<T>`] to avoid the overhead of [`RefCell<T>`](std::ptr) and to be
/// copied easily. They are automatically locked when they are cloned, and
/// unlocked when they are dropped. This is to ensure that they are not
/// collected while they are still in use.
///
/// Certain values such as [`Pair`] and [`Vector`] do not lock their entries,
/// because they are not directly accessible by the user and may define
/// cyclic references. Instead, they are locked on access. On garbage
/// collection, the memory manager will trace through all values and mark
/// their entries as reachable. If an entry is not marked, it is collected.
///
/// All the heap-allocated variants have a `bool` field. This is the lock bit.
/// If the bit is set, the entry is locked. If the bit is not set, the entry is
/// unlocked. This is to avoid an extra check when unlocking the entry. If the
/// entry is already unlocked, then it is not necessary to unlock it again.
///
/// The second field is the pointer to the entry which is wrapped in a
/// [`NonNull<Slot<T>>`]. The [`Slot<T>`] holds the actual value. Slots are
/// allocated in [`Bucket`](crate::mem)s, which are allocated in
/// [`Arena`](crate::mem)s and managed by the [`Mutator`](crate::mem).
/// 
/// - `Pair` is a cons cell. It contains two values, the `car` and the `cdr`.
/// - `String` is a string.
/// - `Symbol` is a symbol.
/// - `Bytevec` is a bytevector.
/// - `Vector` is a vector.
/// 
/// If the feature `evaluate` is enabled, the following variant is also a
/// pointer to an entry:
/// 
/// - `Procedure` is a callable procedure.
#[derive(Debug)]
pub enum ValueKind<'a> {
    /// The empty list.
    Nil,
    /// A boolean value.
    Bool(bool),
    /// A character.
    Char(char),
    /// An integer.
    Integer(i64),
    /// A floating point number.
    Float(f64),
    /// A cons cell. It contains two values, the `car` and the `cdr`.
    Pair(bool, NonNull<Slot<Pair<'a>>>),
    /// A string.
    String(bool, NonNull<Slot<SmartString>>),
    /// A symbol. All symbols are interned, so there is only one instance of each
    /// symbol.
    Symbol(bool, NonNull<Slot<Box<str>>>),
    /// A bytevector.
    Bytevec(bool, NonNull<Slot<ByteVector>>),
    /// A vector.
    Vector(bool, NonNull<Slot<Vector<'a>>>),
}

impl<'a> ValueKind<'a> {
    /// The cloned values are all unlocked and the boolean is set to `false`.
    /// This is safe because cloning is only possible through the `Value`
    /// struct, which automatically locks the entry. Setting here the boolean to
    /// `true` would result in a double unlock.
    pub(super) fn new_clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Bool(value) => Self::Bool(*value),
            Self::Char(value) => Self::Char(*value),
            Self::Integer(value) => Self::Integer(*value),
            Self::Float(value) => Self::Float(*value),
            Self::Pair(_, ptr) => Self::Pair(false, *ptr),
            Self::String(_, ptr) => Self::String(false, *ptr),
            Self::Symbol(_, ptr) => Self::Symbol(false, *ptr),
            Self::Bytevec(_, ptr) => Self::Bytevec(false, *ptr),
            Self::Vector(_, ptr) => Self::Vector(false, *ptr),
        }
    }
}

/// This manual implementation of [`Hash`](std::hash) is necessary because
/// [`f64`] does not implement `Hash`.
impl<'a> hash::Hash for ValueKind<'a> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Nil => ValueTag::Nil.hash(state),
            Self::Bool(value) => {
                ValueTag::Bool.hash(state);
                value.hash(state);
            }
            Self::Char(value) => {
                ValueTag::Char.hash(state);
                value.hash(state);
            }
            Self::Integer(value) => {
                ValueTag::Int.hash(state);
                value.hash(state);
            }
            Self::Float(value) => {
                ValueTag::Float.hash(state);
                value.to_bits().hash(state);
            }
            Self::Pair(_, ptr) => {
                ValueTag::Pair.hash(state);
                ptr.hash(state);
            }
            Self::String(_, ptr) => {
                ValueTag::String.hash(state);
                ptr.hash(state);
            }
            Self::Symbol(_, ptr) => {
                ValueTag::Symbol.hash(state);
                ptr.hash(state);
            }
            Self::Bytevec(_, ptr) => {
                ValueTag::Bytevec.hash(state);
                ptr.hash(state);
            }
            Self::Vector(_, ptr) => {
                ValueTag::Vector.hash(state);
                ptr.hash(state);
            }
        }
    }
}

/// This manual implementation of [`PartialEq`](std::cmp) first checks if the
/// values are identical. If they are not, it checks if the values are equal.
impl<'a> PartialEq for ValueKind<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(a), Self::Bool(b)) => *a == *b,
            (Self::Char(a), Self::Char(b)) => *a == *b,
            (Self::Integer(a), Self::Integer(b)) => *a == *b,
            (Self::Float(a), Self::Float(b)) => *a == *b,
            (Self::Pair(_, a), Self::Pair(_, b)) => {
                if a != b {
                    let a = unsafe { a.as_ref() };
                    let b = unsafe { b.as_ref() };
                    a == b
                } else {
                    true
                }
            }
            (Self::String(_, a), Self::String(_, b)) => {
                if a != b {
                    let a = unsafe { a.as_ref() };
                    let b = unsafe { b.as_ref() };
                    a == b
                } else {
                    true
                }
            }
            (Self::Symbol(_, a), Self::Symbol(_, b)) => {
                if a != b {
                    let a = unsafe { a.as_ref() };
                    let b = unsafe { b.as_ref() };
                    a == b
                } else {
                    true
                }
            }
            (Self::Bytevec(_, a), Self::Bytevec(_, b)) => {
                if a != b {
                    let a = unsafe { a.as_ref() };
                    let b = unsafe { b.as_ref() };
                    a == b
                } else {
                    true
                }
            }
            (Self::Vector(_, a), Self::Vector(_, b)) => {
                if a != b {
                    let a = unsafe { a.as_ref() };
                    let b = unsafe { b.as_ref() };
                    a == b
                } else {
                    true
                }
            }
            _ => false,
        }
    }
}
