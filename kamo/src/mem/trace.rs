use std::ptr::NonNull;

#[cfg(feature = "types")]
use crate::types::Type;
use crate::value::{ByteVector, Pair, SmartString, Vector};

use super::Slot;

/// A trait for tracing a value.
///
/// This trait is implemented for all types that can be traced. Values are
/// traced by the [`Mutator`](crate::mem::Mutator) when the garbage collector
/// is running. Tracing a value means that the value is marked as reachable.
/// This is done by adding the value to a list of reachable values.
///
/// When the method [`trace`](crate::mem::Trace::trace) is called, the value
/// should add all of its traceable values to the list of reachable values.
/// These pending values will be traced later. This is done to prevent stack
/// overflows. If a value is traced, then all of its traceable values will be
/// traced, too. This is done iteratively until all reachable values are
/// traced.
///
/// A value must be traced if it is unlocked and is reachable from the value
/// for which this call to [`trace`](crate::mem::Trace::trace) was made. All
/// locked values are roots and will be traced by the mark phase of the
/// garbage collector.
pub trait Trace<'a> {
    fn trace(&self, traced: &mut Vec<Root<'a>>);
}

/// Implements [`Trace`](crate::mem::Trace) for [`SmartString`]. Since a
/// [`SmartString`] does not contain any traceable values, this is a no-op.
impl<'a> Trace<'a> for SmartString {
    #[inline]
    fn trace(&self, _: &mut Vec<Root<'a>>) {}
}

/// Implements [`Trace`](crate::mem::Trace) for [`String`]. Since a [`String`]
/// does not contain any traceable values, this is a no-op.
impl<'a> Trace<'a> for String {
    #[inline]
    fn trace(&self, _: &mut Vec<Root<'a>>) {}
}

/// Implements [`Trace`](crate::mem::Trace) for [`str`]. Since a [`str`] does
/// not contain any traceable values, this is a no-op. `Box<str>` also are
/// used for symbols and represent immutable strings.
impl<'a> Trace<'a> for Box<str> {
    #[inline]
    fn trace(&self, _: &mut Vec<Root<'a>>) {}
}

/// Implements [`Trace`](crate::mem::Trace) for [`ByteVector`]. Since a
/// [`ByteVector`] does not contain any traceable values, this is a no-op.
impl<'a> Trace<'a> for ByteVector {
    #[inline]
    fn trace(&self, _: &mut Vec<Root<'a>>) {}
}

/// A trait for converting a value into a [`Root`](crate::mem::Root).
///
/// This trait is implemented for all types that can be traced. This trait is
/// used to convert a value into a [`Root`](crate::mem::Root) when tracing. If
/// a value does not need to be traced, then it does not need to be converted
/// into a [`Root`](crate::mem::Root) and can return `None`.
///
/// The [`Value`](crate::value::Value) type implements this trait, so it can be
/// converted into a [`Root`](crate::mem::Root), if the internal value needs to
/// be traced. [`Root`] implenments this trait, too. It returns `Some(*self)`.
/// This is because [`Root`] is already a [`Root`](crate::mem::Root) and
/// therfore must be convertable into a [`Root`](crate::mem::Root).
pub trait ToRoot<'a> {
    fn to_root(&self) -> Option<Root<'a>>;
}

/// An enum of all possible traceable roots.
///
/// Only the [`Pair`](crate::value::Pair) and [`Vector`](crate::value::Vector)
/// require tracing. The other types are either not allocated on the heap or
/// are allocated on the heap but do not contain any traceable values. Tracing
/// is only required for values that may contain other values, which are not
/// locked.
///
/// Root references the slot that contains the value as a pointer. This makes
/// it easier and more efficient to trace the value. This is safe, because the
/// `Root`s are only used during tracing by the
/// [`Mutator`](crate::mem::Mutator).
///
/// If a value contains other values, which are locked, or
/// [`Pointer`](crate::mem::Pointer)s, then it does not need to be traced.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
pub enum Root<'a> {
    /// A pointer to a slot that contains a [`ByteVector`].
    Bytevec(NonNull<Slot<ByteVector>>),
    /// A pointer to a slot that contains a [`SmartString`].
    String(NonNull<Slot<SmartString>>),
    /// A pointer to a slot that contains a [`Box<str>`].
    Symbol(NonNull<Slot<Box<str>>>),
    /// A pointer to a slot that contains a [`Pair`].
    Pair(NonNull<Slot<Pair<'a>>>),
    /// A pointer to a slot that contains a [`Vector`].
    Vector(NonNull<Slot<Vector<'a>>>),
    #[cfg(feature = "types")]
    /// A pointer to a slot that contains a [`Type`].
    Type(NonNull<Slot<Type>>),
}

impl<'a> ToRoot<'a> for Root<'a> {
    #[inline]
    /// Returns `Some(*self)`.
    fn to_root(&self) -> Option<Root<'a>> {
        Some(*self)
    }
}
