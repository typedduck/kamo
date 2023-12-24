use std::{
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::value::{Pair, SmartString, ValueId, Vector};

use super::{Mutator, Root, Slot, Trace};

/// A smart pointer to a value in the heap.
///
/// When allocating values in the heap, the [`Mutator`](crate::mem::Mutator)
/// returns a pointer to the allocated [`Slot`](crate::mem::Slot). This pointer
/// is wrapped in a [`Pointer`](crate::mem::Pointer) and returned to the
/// caller. The caller can then use this pointer to access the value in the
/// heap. When the pointer is dropped, the lock on the entry is released.
///
/// The [`Pointer`](crate::mem::Pointer) type implements [`Deref`] and
/// [`DerefMut`], so it can be used like a normal reference. It also implements
/// [`Clone`], so it can be copied. When a [`Pointer`](crate::mem::Pointer) is
/// cloned, the lock on the entry is acquired again. This is done to ensure that
/// the entry is not unlocked while it is still being used.
///
/// The [`Pointer`](crate::mem::Pointer) type also implements [`Trace`]. This
/// allows the [`Pointer`](crate::mem::Pointer) to be traced by the
/// [`Mutator`](crate::mem::Mutator). This is required, because the
/// [`Pointer`](crate::mem::Pointer) may contain a value that needs to be
/// traced.
///
/// For every type that is allocated in the heap, there is a corresponding
/// [`Pointer`](crate::mem::Pointer) type. There is no method to create a
/// [`Pointer`](crate::mem::Pointer) directly. Instead, the
/// [`Mutator`](crate::mem::Mutator) returns a [`Pointer`](crate::mem::Pointer)
/// when a value is allocated in the heap. A pointer may be created by
/// converting a [`NonNull<Slot<T>>`](std::ptr::NonNull) pointer to a
/// [`Pointer<T>`](crate::mem::Pointer) using the [`From`] trait. This is safe
/// to do as long as the lock on the [`NonNull<Slot<T>>`](std::ptr::NonNull)
/// pointer is still acquired.
///
/// Additionally the traits [`PartialEq`], [`Eq`], [`PartialOrd`], [`Ord`], and
/// [`Hash`](std::hash::Hash) are implemented for
/// [`Pointer`](crate::mem::Pointer). These traits are implemented by comparing
/// the pointers by their identity. This is safe to do, because the pointers are
/// unique. It also makes it possible to use pointers as keys in a
/// [`HashMap`](std::collections::HashMap) or
/// [`BTreeMap`](std::collections::BTreeMap).
/// 
/// The lifetime `'a` is the lifetime of the [`Mutator`](crate::mem::Mutator)
/// via the [`Arena`](crate::mem::Arena) and the [`Bucket`](crate::mem::Bucket)
/// that the pointer was allocated in.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer<'a, T: fmt::Debug>(NonNull<Slot<T>>, PhantomData<Mutator<'a>>);

impl<'a, T: fmt::Debug> Pointer<'a, T> {
    /// Creates a new pointer to the given [`NonNull<Slot<T>>`](Slot). This
    /// locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is a valid pointer and is occupied.
    pub fn new(ptr: NonNull<Slot<T>>) -> Self {
        let mut ptr = Self(ptr, PhantomData);

        ptr.lock();
        ptr
    }

    /// Creates a new pointer to the given [`NonNull<Slot<T>>`](Slot). This
    /// does not lock the entry.
    ///
    /// # Safety
    ///
    /// It is the responsibility of the caller to ensure that the lock on the
    /// entry is acquired prior to calling this method. The pointer must be
    /// valid and the entry must be occupied.
    pub unsafe fn new_unchecked(ptr: NonNull<Slot<T>>) -> Self {
        Self(ptr, PhantomData)
    }

    /// Consumes this pointer and returns a non-null pointer to the entry. The
    /// lock on the entry is released as part of this operation.
    ///
    /// # Safety
    ///
    /// The [`NonNull<Slot<T>>`](Slot) pointer does not hold a lock on the
    /// entry. Any memory operation after this method is called may result in
    /// the collection of the value pointed to by this pointer. The caller must
    /// ensure that the pointer is not used after this method is called until
    /// the lock is acquired again.
    #[inline]
    pub unsafe fn into_inner(self) -> NonNull<Slot<T>> {
        self.0
    }

    /// Returns a non-null pointer to the entry.
    ///
    /// # Safety
    ///
    /// Does not lock the the copy of the entry pointed to. The caller must
    /// ensure that the lock is acquired. The non-null pointer is only valid as
    /// long as the `Pointer` is alive.
    pub unsafe fn as_inner(&self) -> NonNull<Slot<T>> {
        self.0
    }

    /// Consumes this pointer and returns a raw pointer to the entry.
    ///
    /// # Safety
    ///
    /// Does not release the lock on the entry. The caller must ensure that
    /// the lock is released.
    #[inline]
    pub unsafe fn into_raw(self) -> *mut Slot<T> {
        let ptr = self.0.as_ptr();
        std::mem::forget(self);
        ptr
    }

    /// Returns a raw pointer to the entry.
    pub fn as_ptr(&self) -> *const Slot<T> {
        self.0.as_ptr()
    }

    #[inline]
    fn lock(&mut self) {
        let entry = unsafe { self.0.as_mut() };
        entry.lock();
    }

    #[inline]
    fn unlock(&mut self) {
        let entry = unsafe { self.0.as_mut() };
        entry.unlock();
    }
}

impl<'a> Pointer<'a, Pair<'a>> {
    /// Returns the identifier of the value pointed to by this pointer.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }
}

impl<'a> Pointer<'a, SmartString> {
    /// Returns the identifier of the value pointed to by this pointer.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }
}

impl<'a> Pointer<'a, Box<str>> {
    /// Returns the identifier of the value pointed to by this pointer.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }
}

impl<'a> Pointer<'a, Vec<u8>> {
    /// Returns the identifier of the value pointed to by this pointer.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }
}

impl<'a> Pointer<'a, Vector<'a>> {
    /// Returns the identifier of the value pointed to by this pointer.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }
}

impl<'a, T: fmt::Debug> Deref for Pointer<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        let entry = unsafe { self.0.as_ref() };
        entry.value().expect("undefined-pointer")
    }
}

impl<'a, T: fmt::Debug> DerefMut for Pointer<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        let entry = unsafe { self.0.as_mut() };
        entry.value_mut().expect("undefined-pointer")
    }
}

impl<'a, T: fmt::Debug> Clone for Pointer<'a, T> {
    fn clone(&self) -> Self {
        let mut cloned = Self(self.0, PhantomData);
        cloned.lock();
        cloned
    }
}

impl<'a, T: fmt::Debug> Drop for Pointer<'a, T> {
    fn drop(&mut self) {
        self.unlock();
    }
}

impl<'a, T: Trace<'a> + fmt::Debug> Trace<'a> for Pointer<'a, T> {
    /// Traces the value pointed to by this pointer.
    ///
    /// # Panics
    ///
    /// Panics if the [`Slot<T>`] pointed to by this pointer is undefined. If
    /// this happens, then there is a bug in the garbage collector.
    fn trace(&self, traced: &mut Vec<Root<'a>>) {
        let value = unsafe { self.0.as_ref() };
        let value = value.value().expect("undefined-pointer");

        value.trace(traced)
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Pointer<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entry = unsafe { self.0.as_ref() };
        f.debug_tuple("Pointer").field(entry).finish()
    }
}

impl<'a, T: fmt::Debug + fmt::Display> fmt::Display for Pointer<'a, T> {
    /// Formats the value pointed to by this pointer. The value is formatted
    /// using the [`Display`](std::fmt::Display) implementation of the value.
    ///
    /// # Note
    ///
    /// If the value is undefined, then `<undefined>` is returned. This is
    /// an invalid value, so it should never be returned. In order to ensure
    /// that formatting never fails, the string `<undefined>` is returned on
    /// this case.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entry = unsafe { self.0.as_ref() };
        if let Some(value) = entry.value() {
            fmt::Display::fmt(value, f)
        } else {
            write!(f, "<undefined>")
        }
    }
}

impl<'a> From<NonNull<Slot<Pair<'a>>>> for Pointer<'a, Pair<'a>> {
    /// Creates a new pointer to the given
    /// [`NonNull<Slot<Pair>>`](crate::mem::Slot) pointer and locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is occupied.
    #[inline]
    fn from(ptr: NonNull<Slot<Pair<'a>>>) -> Self {
        Self::new(ptr)
    }
}

impl<'a> From<NonNull<Slot<SmartString>>> for Pointer<'a, SmartString> {
    /// Creates a new pointer to the given [`NonNull<Slot<SmartString>>`](Slot)
    /// pointer and locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is occupied.
    #[inline]
    fn from(ptr: NonNull<Slot<SmartString>>) -> Self {
        Self::new(ptr)
    }
}

impl<'a> From<NonNull<Slot<Box<str>>>> for Pointer<'a, Box<str>> {
    /// Creates a new pointer to the given [`NonNull<Slot<Box<str>>>`](Slot)
    /// pointer and locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is occupied.
    #[inline]
    fn from(ptr: NonNull<Slot<Box<str>>>) -> Self {
        Self::new(ptr)
    }
}

impl<'a> From<NonNull<Slot<Vec<u8>>>> for Pointer<'a, Vec<u8>> {
    /// Creates a new pointer to the given [`NonNull<Slot<Vec<u8>>>`](Slot)
    /// pointer and locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is occupied.
    #[inline]
    fn from(ptr: NonNull<Slot<Vec<u8>>>) -> Self {
        Self::new(ptr)
    }
}

impl<'a> From<NonNull<Slot<Vector<'a>>>> for Pointer<'a, Vector<'a>> {
    /// Creates a new pointer to the given [`NonNull<Slot<Vector>>`](Slot)
    /// pointer and locks the entry.
    ///
    /// # Panics
    ///
    /// Panics if the entry cannot be locked. This happens if the entry is
    /// unoccupied. It is the responsibility of the caller to ensure that the
    /// entry is occupied.
    #[inline]
    fn from(ptr: NonNull<Slot<Vector<'a>>>) -> Self {
        Self::new(ptr)
    }
}
