use std::{
    collections::TryReserveError,
    fmt,
    ops::{Deref, Index},
    slice::{Iter, SliceIndex},
};

use crate::mem::{Root, ToRoot, Trace};

use super::Value;

/// A growable, heap-allocated vector of [`Value`]s.
///
/// In contrast to [`Vec`], a `Vector` is a garbage-collected type and can be
/// used in [`Value`]s. The `Value`s in a `Vector` are automatically unlocked
/// when inserted and locked when removed. When the `Vector` is reachable on
/// garbage collection, the `Value`s are also reachable and will be traced.
/// Unlocking the values when they are inserted is necessary, because the
/// values may be cyclic. Locking the values when they are removed is necessary,
/// because the values may be removed from the vector and dropped.
///
/// The interface of `Vector` is similar to [`Vec`], but it does not implement
/// all of its methods. For example, `Vector` does not implement `DerefMut` and
/// `IndexMut` because it is not possible to mutate a `Value` in place.
///
/// A `Vector` can not be created directly. Instead, use
/// [`Value::new_vector()`](super::Value::new_vector).
pub struct Vector<'a> {
    elements: Vec<Value<'a>>,
}

impl<'a> Vector<'a> {
    #[must_use]
    pub(crate) fn new(mut elements: Vec<Value<'a>>) -> Self {
        for element in &mut elements {
            element.unlock();
        }
        Self { elements }
    }

    /// Returns the number of elements the vector can hold without reallocating.
    #[must_use]
    #[inline]
    pub fn capacity(&self) -> usize {
        self.elements.capacity()
    }

    /// Clears the vector, removing all values. Retains the allocated memory for
    /// reuse.
    #[inline]
    pub fn clear(&mut self) {
        self.elements.clear();
    }

    /// Returns a reference to an [`Value`] or `None` if the index is out of
    /// bounds.
    #[must_use]
    #[inline]
    pub fn get(&self, index: usize) -> Option<&Value<'a>> {
        self.elements.get(index)
    }

    /// Sets the value of an element at a given index and returns the old value.
    /// If the `index` is equal to the length of the vector, then the value is
    /// appended to the vector.
    ///
    /// # Panics
    ///
    /// Panics if the `index` is greater than the length of the vector.
    pub fn set(&mut self, index: usize, mut new_elem: Value<'a>) -> Option<Value<'a>> {
        // Check bounds here to avoid panicking after unlocking the value.
        assert!(index <= self.elements.len(), "index out of bounds");
        new_elem.unlock();
        if let Some(element) = self.elements.get_mut(index) {
            let mut old_elem = std::mem::replace(element, new_elem);
            old_elem.lock();
            Some(old_elem)
        } else {
            self.elements.insert(index, new_elem);
            None
        }
    }

    /// Inserts the [`Value`] at the given `index`, shifting all elements after
    /// it to the right.
    #[inline]
    pub fn insert(&mut self, index: usize, mut element: Value<'a>) {
        element.unlock();
        self.elements.insert(index, element);
    }

    /// Returns `true` if the vector contains no elements.
    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Returns the length of the vector.
    #[must_use]
    #[inline]
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Removes and returns the last element of the vector, or `None` if it is
    /// empty.
    #[inline]
    pub fn pop(&mut self) -> Option<Value<'a>> {
        self.elements.pop().map(|mut element| {
            element.lock();
            element
        })
    }

    /// Appends an element to the back of the vector.
    #[inline]
    pub fn push(&mut self, mut element: Value<'a>) {
        element.unlock();
        self.elements.push(element);
    }

    /// Removes and returns the element at the given `index`, shifting all
    /// elements after it to the left. Panics if the `index` is out of bounds.
    #[inline]
    pub fn remove(&mut self, index: usize) -> Value<'a> {
        let mut element = self.elements.remove(index);
        element.lock();
        element
    }

    /// Reserves capacity for at least `additional` more elements to be inserted
    /// in the given `Vector`.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.elements.reserve(additional);
    }

    /// Reserves the minimum capacity for exactly `additional` more elements to
    /// be inserted in the given `Vector`. It will not reserve more space than
    /// necessary.
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.elements.reserve_exact(additional);
    }

    /// Retains only the elements specified by the predicate.
    #[inline]
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Value<'a>) -> bool,
    {
        self.elements.retain(f);
    }

    /// Retains only the elements specified by the predicate, passing each value
    /// as a mutable reference.
    #[inline]
    pub fn retain_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut Value<'a>) -> bool,
    {
        self.elements.retain_mut(f);
    }

    /// Shrinks the capacity of the vector with a lower bound. The capacity will
    /// remain at least as large as the length of the vector.
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.elements.shrink_to(min_capacity);
    }

    /// Shrinks the capacity of the vector as much as possible. It will drop
    /// down as close as possible to the length but the allocator may still
    /// inform the vector that there is space for a few more elements.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.elements.shrink_to_fit();
    }

    /// Removes and returns the element at the given `index`, replacing it with
    /// the last element of the vector. Panics if the `index` is out of bounds.
    #[inline]
    pub fn swap_remove(&mut self, index: usize) -> Value<'a> {
        let mut element = self.elements.swap_remove(index);
        element.lock();
        element
    }

    /// Truncates the vector, dropping all values after `len`. If `len` is
    /// greater than the current length, this has no effect.
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.elements.truncate(len);
    }

    /// Trys to reserve capacity for at least `additional` more elements to be
    /// inserted in the given `Vector`. Returns an error if the capacity would
    /// overflow.
    ///
    /// # Errors
    ///
    /// Returns an error if the capacity would overflow.
    #[inline]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.elements.try_reserve(additional)
    }

    /// Trys to reserve the minimum capacity for exactly `additional` more
    /// elements to be inserted in the given `Vector`. It will not reserve more
    /// space than necessary. Returns an error if the capacity would overflow.
    ///
    /// # Errors
    ///
    /// Returns an error if the capacity would overflow.
    #[inline]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.elements.try_reserve_exact(additional)
    }

    /// Returns an iterator over the [`Value`]s of the vector.
    pub fn iter(&self) -> Iter<Value<'a>> {
        self.elements.iter()
    }
}

impl<'a> Deref for Vector<'a> {
    type Target = [Value<'a>];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.elements
    }
}

impl<'a, I: SliceIndex<[Value<'a>]>> Index<I> for Vector<'a> {
    type Output = I::Output;

    /// Returns a reference to an element or subslice depending on the type of
    /// index. Panics if the index is out of bounds.
    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.elements.index(index)
    }
}

impl<'a, 'b> IntoIterator for &'b Vector<'a> {
    type Item = &'b Value<'a>;
    type IntoIter = Iter<'b, Value<'a>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.elements.iter()
    }
}

impl<'a> Trace<'a> for Vector<'a> {
    fn trace(&self, traced: &mut Vec<Root<'a>>) {
        for element in &self.elements {
            if let Some(root) = element.to_root() {
                traced.push(root);
            }
        }
    }
}

impl<'a> PartialEq for Vector<'a> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl<'a> fmt::Debug for Vector<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.elements.iter()).finish()
    }
}
