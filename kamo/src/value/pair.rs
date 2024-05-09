use std::collections::HashSet;

use crate::mem::{Pointer, Root, ToRoot, Trace};

use super::Value;

/// The next value in a list.
#[derive(Clone, Debug, PartialEq)]
pub enum Next<'a> {
    Pair(Pointer<'a, Pair<'a>>),
    Dot(Value<'a>),
    Nil,
}

/// A cons cell holding two values of any type.
///
/// A pair is a cons cell. It contains two values, the `car` and the `cdr`.
/// The locks on the values are released when they are added to a pair. On
/// garbage collection, the pair traces the values.
///
/// A pair is a proper list if the `cdr` is a pair or a nil value. It is
/// circular if the `cdr` points to itself or to a pair that is circular.
/// A pair can not be created directly. Instead, use:
///
/// - [`Mutator::new_pair()`](crate::mem::Mutator::new_pair), allocates a new
///   pair.
/// - [`Value::new_pair()`](super::Value::new_pair) or
///   [`Value::new_cons()`](super::Value::new_cons), constructs a cons cell from
///   two values.
/// - [`Value::new_list()`](super::Value::new_list) or
///   [`Value::new_dotlist()`](super::Value::new_dotlist), constructs a list
///   from a [`Vec<Value>`].
/// - [`Value::new_list_map()`](super::Value::new_list_map) or
///   [`Value::new_dotlist_map()`](super::Value::new_dotlist_map), constructs a
///   list from a [`Vec<T>`] by applying a function to each element.
#[derive(Debug)]
pub struct Pair<'a> {
    head: Value<'a>,
    tail: Value<'a>,
}

impl<'a> Pair<'a> {
    /// Creates a new pair from two values. The values are unlocked and are
    /// traced on garbage collection.
    pub(crate) fn new(mut head: Value<'a>, mut tail: Value<'a>) -> Self {
        head.unlock();
        tail.unlock();

        Self { head, tail }
    }

    /// Returns the next value in the list.
    #[must_use]
    pub fn next(&self) -> Next<'a> {
        self.tail.as_pair_ptr().map_or_else(
            || {
                if self.tail.is_nil() {
                    Next::Nil
                } else {
                    Next::Dot(self.tail.clone())
                }
            },
            Next::Pair,
        )
    }

    /// Returns a reference to the head of the pair.
    #[must_use]
    #[inline]
    pub const fn car(&self) -> &Value<'a> {
        &self.head
    }

    /// Returns a reference to the tail of the pair.
    #[must_use]
    #[inline]
    pub const fn cdr(&self) -> &Value<'a> {
        &self.tail
    }

    /// Returns: `(car (car <self>))`
    #[must_use]
    #[inline]
    pub fn caar(&self) -> Option<Value<'a>> {
        self.car().as_pair().map(|pair| pair.car().clone())
    }

    /// Returns: `(cdr (car <self>))`
    #[must_use]
    #[inline]
    pub fn cadr(&self) -> Option<Value<'a>> {
        self.cdr().as_pair().map(|pair| pair.car().clone())
    }

    /// Returns: `(car (cdr <self>))`
    #[must_use]
    pub fn cdar(&self) -> Option<Value<'a>> {
        self.car().as_pair().map(|pair| pair.cdr().clone())
    }

    /// Returns: `(cdr (cdr <self>))`
    #[must_use]
    pub fn cddr(&self) -> Option<Value<'a>> {
        self.cdr().as_pair().map(|pair| pair.cdr().clone())
    }

    /// Returns: `(car (car (car <self>)))`
    #[must_use]
    pub fn caaar(&self) -> Option<Value<'a>> {
        self.caar()
            .and_then(|value| value.as_pair().map(|pair| pair.car().clone()))
    }

    /// Returns: `(cdr (car (car <self>)))`
    #[must_use]
    pub fn caadr(&self) -> Option<Value<'a>> {
        self.cadr()
            .and_then(|value| value.as_pair().map(|pair| pair.car().clone()))
    }

    /// Returns: `(car (cdr (car <self>)))`
    #[must_use]
    pub fn cadar(&self) -> Option<Value<'a>> {
        self.cdar()
            .and_then(|value| value.as_pair().map(|pair| pair.car().clone()))
    }

    /// Returns: `(cdr (cdr (car <self>)))`
    #[must_use]
    pub fn caddr(&self) -> Option<Value<'a>> {
        self.cddr()
            .and_then(|value| value.as_pair().map(|pair| pair.car().clone()))
    }

    /// Returns: `(car (car (cdr <self>)))`
    #[must_use]
    pub fn cdaar(&self) -> Option<Value<'a>> {
        self.caar()
            .and_then(|value| value.as_pair().map(|pair| pair.cdr().clone()))
    }

    /// Returns: `(cdr (car (cdr <self>)))`
    #[must_use]
    pub fn cdadr(&self) -> Option<Value<'a>> {
        self.cadr()
            .and_then(|value| value.as_pair().map(|pair| pair.cdr().clone()))
    }

    /// Returns: `(car (cdr (cdr <self>)))`
    #[must_use]
    pub fn cddar(&self) -> Option<Value<'a>> {
        self.cdar()
            .and_then(|value| value.as_pair().map(|pair| pair.cdr().clone()))
    }

    /// Returns: `(cdr (cdr (cdr <self>)))`
    #[must_use]
    pub fn cdddr(&self) -> Option<Value<'a>> {
        self.cddr()
            .and_then(|value| value.as_pair().map(|pair| pair.cdr().clone()))
    }

    /// Sets the head of the pair. Returns the old head.
    #[inline]
    pub fn set_car(&mut self, mut value: Value<'a>) -> Value<'a> {
        value.unlock();
        let mut old_value = std::mem::replace(&mut self.head, value);

        old_value.lock();
        old_value
    }

    /// Sets the tail of the pair. Returns the old tail.
    #[inline]
    pub fn set_cdr(&mut self, mut value: Value<'a>) -> Value<'a> {
        value.unlock();
        let mut old_value = std::mem::replace(&mut self.tail, value);

        old_value.lock();
        old_value
    }

    /// Returns `true` if the pair is a proper list. Returns `false` if the pair
    /// is not a proper list or if the list is circular.
    #[must_use]
    pub fn is_list(&self) -> bool {
        let mut seen = HashSet::new();
        let mut tail = &self.tail;

        seen.insert(std::ptr::from_ref::<Pair<'_>>(self) as usize);
        while let Some(pair) = tail.as_pair() {
            let pair_ptr = std::ptr::from_ref::<Pair<'_>>(pair) as usize;

            if seen.contains(&pair_ptr) {
                return false;
            }
            seen.insert(pair_ptr);
            tail = &pair.tail;
        }
        tail.is_nil()
    }

    /// Returns the length of the list. Circular lists are handled correctly.
    #[must_use]
    pub fn len(&self) -> usize {
        let mut len = 1;
        let mut seen = HashSet::new();
        let mut tail = &self.tail;

        seen.insert(std::ptr::from_ref::<Pair<'_>>(self) as usize);
        while let Some(pair) = tail.as_pair() {
            let pair_ptr = std::ptr::from_ref::<Pair<'_>>(pair) as usize;

            if seen.contains(&pair_ptr) {
                return len;
            }
            seen.insert(pair_ptr);
            len += 1;
            tail = &pair.tail;
        }
        len
    }

    /// Returns always `false` because a pair is never empty.
    #[must_use]
    #[inline]
    pub const fn is_empty(&self) -> bool {
        false
    }
}

impl<'a> Trace<'a> for Pair<'a> {
    fn trace(&self, traced: &mut Vec<Root<'a>>) {
        if let Some(root) = self.head.to_root() {
            traced.push(root);
        }
        if let Some(root) = self.tail.to_root() {
            traced.push(root);
        }
    }
}

impl<'a> PartialEq for Pair<'a> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let lhs = std::ptr::from_ref::<Self>(self).cast::<u8>();
        let rhs = std::ptr::from_ref::<Self>(other).cast::<u8>();

        if std::ptr::eq(lhs, rhs) {
            true
        } else {
            self.head == other.head && self.tail == other.tail
        }
    }
}
