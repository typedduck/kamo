use std::{fmt, marker::PhantomData, ptr::NonNull};

use crate::{
    mem::{Mutator, MutatorRef, Pointer, Root, ToRoot},
    value::SmartString,
};

use super::{Pair, ValueId, ValueKind, ValueTag, Vector, Visitor, ByteVector};

/// A value in the Kamo runtime. This is a wrapper around [`ValueKind`].
/// 
/// There are two main types of values: immediate values and heap values.
/// 
/// * Immediate values are values that are stored directly in the value. They
///  are `nil`, booleans, characters, integers, and floats.
/// * Heap values are values that are stored in the heap. They are pairs,
/// strings, symbols, byte-vectors, vectors, and procedures.
/// 
/// See [`ValueKind`] for more information.
/// 
/// Value provides a safe interface to the values in the runtime. It is
/// implemented as a wrapper around [`ValueKind`]. It provides methods to
/// convert the value into a Rust type, to check the type of the value, and to
/// visit the value. Heap values are automatically locked and unlocked when
/// necessary.
#[derive(Hash, PartialEq)]
pub struct Value<'a> {
    inner: ValueKind<'a>,
    marker: PhantomData<&'a Mutator<'a>>,
}

impl<'a> Value<'a> {
    #[inline]
    pub(crate) const fn new(inner: ValueKind<'a>) -> Self {
        Self {
            inner,
            marker: PhantomData,
        }
    }

    /* #region Constructors */

    /// Creates a new nil immediate-value.
    #[inline]
    pub const fn new_nil() -> Self {
        Self::new(ValueKind::Nil)
    }

    /// Creates a new boolean immediate-value.
    #[inline]
    pub const fn new_bool(value: bool) -> Self {
        Self::new(ValueKind::Bool(value))
    }

    /// Creates a new character immediate-value.
    #[inline]
    pub const fn new_char(value: char) -> Self {
        Self::new(ValueKind::Char(value))
    }

    /// Creates a new integer immediate-value.
    #[inline]
    pub const fn new_int(value: i64) -> Self {
        Self::new(ValueKind::Integer(value))
    }

    /// Creates a new float immediate-value.
    #[inline]
    pub const fn new_float(value: f64) -> Self {
        Self::new(ValueKind::Float(value))
    }

    /// Creates a new symbol.
    #[inline]
    pub fn new_symbol(m: MutatorRef<'a>, name: impl AsRef<str>) -> Self {
        let symbol = m.borrow_mut().new_symbol(name);
        symbol.into()
    }

    /// Creates a new string.
    #[inline]
    pub fn new_string(m: MutatorRef<'a>, string: impl AsRef<str>) -> Self {
        let string = m.borrow_mut().new_string(string);
        string.into()
    }

    /// Creates a new byte-vector.
    #[inline]
    pub fn new_bytevec(m: MutatorRef<'a>, bytevec: impl AsRef<[u8]>) -> Self {
        let bytevec = m.borrow_mut().new_bytevec(bytevec);
        bytevec.into()
    }

    /// Creates a new vector.
    #[inline]
    pub fn new_vector(m: MutatorRef<'a>, vector: impl Into<Vec<Value<'a>>>) -> Self {
        let vector = m.borrow_mut().new_vector(vector);
        vector.into()
    }

    /// Creates a new pair. This is equivalent to [`Value::new_cons()`].
    #[inline]
    pub fn new_pair(m: MutatorRef<'a>, car: Value<'a>, cdr: Value<'a>) -> Self {
        Self::new_cons(m, car, cdr)
    }

    /// Creates a new cons cell.
    #[inline]
    pub fn new_cons(m: MutatorRef<'a>, car: Value<'a>, cdr: Value<'a>) -> Self {
        let pair = m.borrow_mut().new_pair(car, cdr);
        pair.into()
    }

    /// Creates a new proper list from an iterator of values. It takes ownership
    /// of the values and stores them in the list.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use kamo::{mem::Mutator, value::{Value, print}};
    /// let mut m = Mutator::new_ref();
    ///
    /// let list1 = Value::new_list(m.clone(), vec![
    ///     Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ]);
    /// let list2 = Value::new_list_map(m.clone(), vec![
    ///     1, 2, 3
    /// ], Value::new_int);
    ///
    /// assert_eq!(list1, list2);
    /// assert_eq!(print(list1).to_string(), "(1 2 3)");
    /// assert_eq!(print(list2).to_string(), "(1 2 3)");
    /// ```
    #[inline]
    pub fn new_list(m: MutatorRef<'a>, list: impl IntoIterator<Item = Value<'a>>) -> Self {
        Self::new_dotlist_map(m, list, None, |v| v)
    }

    /// Creates a new proper list from an iterator of values of `T`.
    /// 
    /// The values in the iterator are of type `T`. The function `f` is used to
    /// convert the values into `Value<'a>`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use kamo::{mem::Mutator, value::{Value, print}};
    /// let mut m = Mutator::new_ref();
    ///
    /// let list1 = Value::new_list(m.clone(), vec![
    ///     Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ]);
    /// let list2 = Value::new_list_map(m.clone(), vec![
    ///     1, 2, 3
    /// ], Value::new_int);
    ///
    /// assert_eq!(list1, list2);
    /// assert_eq!(print(list1).to_string(), "(1 2 3)");
    /// assert_eq!(print(list2).to_string(), "(1 2 3)");
    /// ```
    #[inline]
    pub fn new_list_map<T, F>(m: MutatorRef<'a>, list: impl IntoIterator<Item = T>, f: F) -> Self
    where
        F: FnMut(T) -> Value<'a>,
    {
        Self::new_dotlist_map(m, list, None, f)
    }

    /// Creates a new list from an iterator of values. It takes ownership of the
    /// values and stores them in the list.
    ///
    /// An alternative end of list may be set to a value other than `nil` by
    /// passing it as the second optional argument. If this argument is `None`
    /// or `Some(nil)`, the end of list is set to `nil`. A list with no elements
    /// and a non-`nil` end of list is equivalent to a dotted pair
    /// `(() . <eol>)`.
    ///
    /// This function is equivalent to [`Value::new_list_map()`] with the
    /// identity function as the last argument.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use kamo::{mem::Mutator, value::{Value, print}};
    /// let mut m = Mutator::new_ref();
    ///
    /// let list1 = Value::new_dotlist(m.clone(), vec![
    ///     Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ], None);
    /// let list2 = Value::new_dotlist_map(m.clone(), vec![
    ///     1, 2, 3
    /// ], None, Value::new_int);
    ///
    /// assert_eq!(list1, list2);
    /// assert_eq!(print(list1).to_string(), "(1 2 3)");
    /// assert_eq!(print(list2).to_string(), "(1 2 3)");
    /// 
    /// let dotlist1 = Value::new_dotlist(m.clone(), vec![
    ///    Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ], Some(Value::new_int(4)));
    /// let dotlist2 = Value::new_dotlist_map(m.clone(), vec![
    ///    1, 2, 3
    /// ], Some(4), Value::new_int);
    /// 
    /// assert_eq!(dotlist1, dotlist2);
    /// assert_eq!(print(dotlist1).to_string(), "(1 2 3 . 4)");
    /// assert_eq!(print(dotlist2).to_string(), "(1 2 3 . 4)");
    /// ```
    #[inline]
    pub fn new_dotlist(
        m: MutatorRef<'a>,
        list: impl IntoIterator<Item = Value<'a>>,
        eol: Option<Value<'a>>,
    ) -> Self {
        Self::new_dotlist_map(m, list, eol, |v| v)
    }

    /// Creates a new list from an iterator of values of `T`.
    ///
    /// The values in the iterator are of type `T`. The function `f` is used to
    /// convert the values into `Value<'a>`.
    ///
    /// An alternative end of list may be set to a value other than `nil` by
    /// passing it as the second optional argument. If this argument is `None`
    /// or `Some(nil)`, the end of list is set to `nil`. A list with no elements
    /// and a non-`nil` end of list is equivalent to a dotted pair
    /// `(() . <eol>)`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use kamo::{mem::Mutator, value::{Value, print}};
    /// let mut m = Mutator::new_ref();
    ///
    /// let list1 = Value::new_dotlist(m.clone(), vec![
    ///     Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ], None);
    /// let list2 = Value::new_dotlist_map(m.clone(), vec![
    ///     1, 2, 3
    /// ], None, Value::new_int);
    ///
    /// assert_eq!(list1, list2);
    /// assert_eq!(print(list1).to_string(), "(1 2 3)");
    /// assert_eq!(print(list2).to_string(), "(1 2 3)");
    /// 
    /// let dotlist1 = Value::new_dotlist(m.clone(), vec![
    ///    Value::new_int(1), Value::new_int(2), Value::new_int(3)
    /// ], Some(Value::new_int(4)));
    /// let dotlist2 = Value::new_dotlist_map(m.clone(), vec![
    ///    1, 2, 3
    /// ], Some(4), Value::new_int);
    /// 
    /// assert_eq!(dotlist1, dotlist2);
    /// assert_eq!(print(dotlist1).to_string(), "(1 2 3 . 4)");
    /// assert_eq!(print(dotlist2).to_string(), "(1 2 3 . 4)");
    /// ```
    pub fn new_dotlist_map<T, F>(
        m: MutatorRef<'a>,
        list: impl IntoIterator<Item = T>,
        eol: Option<T>,
        mut f: F,
    ) -> Self
    where
        F: FnMut(T) -> Value<'a>,
    {
        let mut list = list.into_iter();
        let eol = eol.map(&mut f).unwrap_or(Value::new_nil());

        if let Some(car) = list.next() {
            let mut m = m.borrow_mut();
            let head = m.new_pair(f(car), Value::new_nil());
            let mut tail = head.to_owned();

            for car in list {
                let cdr = m.new_pair(f(car), Value::new_nil());
                tail.set_cdr(cdr.to_owned().into());
                tail = cdr;
            }
            tail.set_cdr(eol);
            head
        } else {
            let ptr = m.borrow_mut().new_pair(Value::new_nil(), eol);
            ptr
        }
        .into()
    }

    /* #endregion */

    /* #region Conversion */

    /// If the value is `nil` or the empty list, returns `Some(())`. Otherwise,
    /// returns `None`.
    #[inline]
    pub const fn as_nil(&self) -> Option<()> {
        match self.inner {
            ValueKind::Nil => Some(()),
            _ => None,
        }
    }

    /// If the value is a boolean, returns it as a `bool`. Otherwise, returns
    /// `None`.
    #[inline]
    pub const fn as_bool(&self) -> Option<bool> {
        match self.inner {
            ValueKind::Bool(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a character, returns it as a `char`. Otherwise, returns
    /// `None`.
    #[inline]
    pub const fn as_char(&self) -> Option<char> {
        match self.inner {
            ValueKind::Char(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is an integer, returns it as an `i64`. Otherwise, returns
    /// `None`.
    #[inline]
    pub const fn as_int(&self) -> Option<i64> {
        match self.inner {
            ValueKind::Integer(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a float, returns it as an `f64`. Otherwise, returns
    /// `None`.
    #[inline]
    pub const fn as_float(&self) -> Option<f64> {
        match self.inner {
            ValueKind::Float(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a [`Pair`]. Otherwise, returns
    /// `None`.
    #[inline]
    pub fn as_pair(&self) -> Option<&Pair<'a>> {
        match self.inner {
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a mutable [`Pair`]. Otherwise,
    /// returns `None`.
    #[inline]
    pub fn as_pair_mut(&mut self) -> Option<&mut Pair<'a>> {
        match self.inner {
            ValueKind::Pair(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a `Pointer<'a, Pair<'a>>`.
    /// Otherwise, returns `None`.
    pub fn as_pair_ptr(&self) -> Option<Pointer<'a, Pair<'a>>> {
        match self.inner {
            ValueKind::Pair(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a [`SmartString`]. Otherwise,
    /// returns `None`.
    #[inline]
    pub fn as_string(&self) -> Option<&SmartString> {
        match self.inner {
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a mutable [`SmartString`].
    /// Otherwise, returns `None`.
    #[inline]
    pub fn as_string_mut(&mut self) -> Option<&mut SmartString> {
        match self.inner {
            ValueKind::String(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a `Pointer<'a, SmartString>`.
    /// Otherwise, returns `None`.
    pub fn as_string_ptr(&self) -> Option<Pointer<'a, SmartString>> {
        match self.inner {
            ValueKind::String(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a symbol, returns it as a `&str`. Otherwise, returns
    /// `None`.
    #[inline]
    pub fn as_symbol(&self) -> Option<&str> {
        match self.inner {
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(|e| e.as_ref()),
            _ => None,
        }
    }

    /// If the value is a symbol, returns it as a `Pointer<'a, Box<str>>`.
    /// Otherwise, returns `None`.
    pub fn as_symbol_ptr(&self) -> Option<Pointer<'a, Box<str>>> {
        match self.inner {
            ValueKind::Symbol(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a `&[u8]`. Otherwise,
    /// returns `None`.
    #[inline]
    pub fn as_bytevec(&self) -> Option<&[u8]> {
        match self.inner {
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(|e| e.as_slice()),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a mutable
    /// `&mut ByteVector`. Otherwise, returns `None`.
    #[inline]
    pub fn as_bytevec_mut(&mut self) -> Option<&mut ByteVector> {
        match self.inner {
            ValueKind::Bytevec(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a
    /// `Pointer<'a, ByteVector>`. Otherwise, returns `None`.
    pub fn as_bytevec_ptr(&self) -> Option<Pointer<'a, ByteVector>> {
        match self.inner {
            ValueKind::Bytevec(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a [`Vector`]. Otherwise, returns
    /// `None`.
    #[inline]
    pub fn as_vector(&self) -> Option<&Vector<'a>> {
        match self.inner {
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a mutable [`Vector`]. Otherwise,
    /// returns `None`.
    #[inline]
    pub fn as_vector_mut(&mut self) -> Option<&mut Vector<'a>> {
        match self.inner {
            ValueKind::Vector(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a `Pointer<'a, Vector<'a>>`.
    /// Otherwise, returns `None`.
    pub fn as_vector_ptr(&self) -> Option<Pointer<'a, Vector<'a>>> {
        match self.inner {
            ValueKind::Vector(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// Returns the inner value.
    #[inline]
    pub fn kind(&self) -> &ValueKind<'a> {
        &self.inner
    }

    /* #endregion */

    /* #region Predicates */

    /// Returns `true` if the value is `nil`.
    #[inline]
    pub const fn is_nil(&self) -> bool {
        matches!(self.inner, ValueKind::Nil)
    }

    /// Returns `true` if the value is `true`.
    #[inline]
    pub const fn is_true(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(true))
    }

    /// Returns `true` if the value is `false` or `nil`.
    #[inline]
    pub const fn is_false(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(false) | ValueKind::Nil)
    }

    /// Returns `true` if the value is a boolean.
    #[inline]
    pub const fn is_bool(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(_))
    }

    /// Returns `true` if the value is a character.
    #[inline]
    pub const fn is_char(&self) -> bool {
        matches!(self.inner, ValueKind::Char(_))
    }

    /// Returns `true` if the value is an integer.
    #[inline]
    pub const fn is_int(&self) -> bool {
        matches!(self.inner, ValueKind::Integer(_))
    }

    /// Returns `true` if the value is a float.
    #[inline]
    pub const fn is_float(&self) -> bool {
        matches!(self.inner, ValueKind::Float(_))
    }

    /// Returns `true` if the value is a number.
    #[inline]
    pub const fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    /// Returns `true` if the value is a pair.
    #[inline]
    pub const fn is_pair(&self) -> bool {
        matches!(self.inner, ValueKind::Pair(_, _))
    }

    /// Returns `true` if the value is a string.
    #[inline]
    pub const fn is_string(&self) -> bool {
        matches!(self.inner, ValueKind::String(_, _))
    }

    /// Returns `true` if the value is a symbol.
    #[inline]
    pub const fn is_symbol(&self) -> bool {
        matches!(self.inner, ValueKind::Symbol(_, _))
    }

    /// Returns `true` if the value is a byte-vector.
    #[inline]
    pub const fn is_bytevec(&self) -> bool {
        matches!(self.inner, ValueKind::Bytevec(_, _))
    }

    /// Returns `true` if the value is a vector.
    #[inline]
    pub const fn is_vector(&self) -> bool {
        matches!(self.inner, ValueKind::Vector(_, _))
    }

    /// Returns `true` if the value is an atom, or rather not a pair.
    #[inline]
    pub const fn is_atom(&self) -> bool {
        !(self.is_pair() || self.is_nil())
    }

    /// Returns `true` if the value is evaluating, or rather not an atom.
    /// Evaluating values are pairs, symbols, and `nil`.
    #[inline]
    pub const fn is_evaluating(&self) -> bool {
        self.is_symbol() || self.is_pair() || self.is_nil()
    }

    /// Returns `true` if the value is a proper list. Returns `false` if the
    /// value is not a proper list or if the list is circular.
    #[inline]
    pub fn is_list(&self) -> bool {
        if let Some(pair) = self.as_pair() {
            pair.is_list()
        } else {
            false
        }
    }

    /// Returns `Some(true)` if the value is an empty list, [`Vector`], string,
    /// symbol, or byte-vector. Returns Some(`false`) if the value is not empty
    /// and `None` otherwise.
    pub fn is_empty(&self) -> Option<bool> {
        match self.inner {
            ValueKind::Nil => Some(true),
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            _ => None,
        }
    }

    /* #endregion */

    /* #region Properties */

    /// Returns the id of the value.
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }

    /// Returns the tag of the value.
    pub fn tag(&self) -> ValueTag {
        match self.inner {
            ValueKind::Nil => ValueTag::Nil,
            ValueKind::Bool(_) => ValueTag::Bool,
            ValueKind::Char(_) => ValueTag::Char,
            ValueKind::Integer(_) => ValueTag::Int,
            ValueKind::Float(_) => ValueTag::Float,
            ValueKind::Pair(_, _) => ValueTag::Pair,
            ValueKind::String(_, _) => ValueTag::String,
            ValueKind::Symbol(_, _) => ValueTag::Symbol,
            ValueKind::Bytevec(_, _) => ValueTag::Bytevec,
            ValueKind::Vector(_, _) => ValueTag::Vector,
        }
    }

    /// Returns the length of the value. Returns `None` if the value does not
    /// have a length.
    /// 
    /// Values that have a length are pairs, vectors, strings, symbols, and
    /// byte-vectors. `nil` has a length of `0`. Calculating the length of a
    /// list is linear to the length of the list. Circular lists are calculated
    /// correctly.
    pub fn len(&self) -> Option<usize> {
        match self.inner {
            ValueKind::Nil => Some(0),
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            _ => None,
        }
    }

    /* #endregion */

    /* #region Visitors */

    /// Accepts a visitor and calls the appropriate method.
    pub fn accept<V>(&self, visitor: &mut V) -> V::Result
    where
        V: Visitor,
    {
        match self.inner {
            ValueKind::Nil => visitor.visit_nil(),
            ValueKind::Bool(true) => visitor.visit_true(),
            ValueKind::Bool(false) => visitor.visit_false(),
            ValueKind::Char(value) => visitor.visit_char(value),
            ValueKind::Integer(value) => visitor.visit_integer(value),
            ValueKind::Float(value) => visitor.visit_float(value),
            ValueKind::Pair(_, ptr) => {
                let pair = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid pair pointer");

                visitor.visit_pair(pair)
            }
            ValueKind::String(_, ptr) => {
                let string = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid string pointer");

                visitor.visit_string(string)
            }
            ValueKind::Symbol(_, ptr) => {
                let symbol = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid symbol pointer");

                visitor.visit_symbol(symbol)
            }
            ValueKind::Bytevec(_, ptr) => {
                let bytevec = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid bytevec pointer");

                visitor.visit_bytevec(bytevec)
            }
            ValueKind::Vector(_, ptr) => {
                let vector = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid vector pointer");

                visitor.visit_vector(vector)
            }
        }
    }

    /* #endregion */

    /* #region Mutators */

    pub(super) fn unlock(&mut self) {
        match &mut self.inner {
            ValueKind::Nil
            | ValueKind::Bool(_)
            | ValueKind::Char(_)
            | ValueKind::Integer(_)
            | ValueKind::Float(_) => (),
            ValueKind::Pair(locked, ptr) => {
                if *locked {
                    unsafe { ptr.as_mut() }.unlock();
                    *locked = false;
                }
            }
            ValueKind::Symbol(locked, ptr) => {
                if *locked {
                    unsafe { ptr.as_mut() }.unlock();
                    *locked = false;
                }
            }
            ValueKind::String(locked, ptr) => {
                if *locked {
                    unsafe { ptr.as_mut() }.unlock();
                    *locked = false;
                }
            }
            ValueKind::Bytevec(locked, ptr) => {
                if *locked {
                    unsafe { ptr.as_mut() }.unlock();
                    *locked = false;
                }
            }
            ValueKind::Vector(locked, ptr) => {
                if *locked {
                    unsafe { ptr.as_mut() }.unlock();
                    *locked = false;
                }
            }
        }
    }

    pub(super) fn lock(&mut self) {
        #[allow(clippy::single_match)]
        match &mut self.inner {
            ValueKind::Nil
            | ValueKind::Bool(_)
            | ValueKind::Char(_)
            | ValueKind::Integer(_)
            | ValueKind::Float(_) => (),
            ValueKind::Pair(locked, ptr) => {
                if !*locked {
                    unsafe { ptr.as_mut() }.lock();
                    *locked = true;
                }
            }
            ValueKind::Symbol(locked, ptr) => {
                if !*locked {
                    unsafe { ptr.as_mut() }.lock();
                    *locked = true;
                }
            }
            ValueKind::String(locked, ptr) => {
                if !*locked {
                    unsafe { ptr.as_mut() }.lock();
                    *locked = true;
                }
            }
            ValueKind::Bytevec(locked, ptr) => {
                if !*locked {
                    unsafe { ptr.as_mut() }.lock();
                    *locked = true;
                }
            }
            ValueKind::Vector(locked, ptr) => {
                if !*locked {
                    unsafe { ptr.as_mut() }.lock();
                    *locked = true;
                }
            }
        }
    }

    /* #endregion */
}

impl<'a> Clone for Value<'a> {
    fn clone(&self) -> Self {
        let mut cloned = Self {
            inner: self.inner.new_clone(),
            marker: self.marker,
        };

        cloned.lock();
        cloned
    }
}

impl<'a> From<Pointer<'a, Pair<'a>>> for Value<'a> {
    fn from(ptr: Pointer<'a, Pair<'a>>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::Pair(true, ptr))
    }
}

impl<'a> From<Pointer<'a, SmartString>> for Value<'a> {
    fn from(ptr: Pointer<'a, SmartString>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::String(true, ptr))
    }
}

impl<'a> From<Pointer<'a, ByteVector>> for Value<'a> {
    fn from(ptr: Pointer<'a, ByteVector>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::Bytevec(true, ptr))
    }
}

impl<'a> From<Pointer<'a, Vector<'a>>> for Value<'a> {
    fn from(ptr: Pointer<'a, Vector<'a>>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::Vector(true, ptr))
    }
}

impl<'a> From<Pointer<'a, Box<str>>> for Value<'a> {
    fn from(ptr: Pointer<'a, Box<str>>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::Symbol(true, ptr))
    }
}

impl<'a> Drop for Value<'a> {
    fn drop(&mut self) {
        self.unlock()
    }
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Value").field(&self.inner).finish()
    }
}

impl<'a> ToRoot<'a> for Value<'a> {
    /// Returns the root of the value if it is a pair or a vector. Otherwise,
    /// returns `None`.
    /// 
    /// To optimize the performance of the garbage collector, the root of a
    /// value is only returned if it is not locked. If the value is locked, it
    /// will be traversed by the garbage collector anyway.
    fn to_root(&self) -> Option<Root<'a>> {
        match &self.inner {
            ValueKind::Nil
            | ValueKind::Bool(_)
            | ValueKind::Char(_)
            | ValueKind::Integer(_)
            | ValueKind::Float(_) => None,
            ValueKind::String(locked, ptr) => {
                if !*locked {
                    Some(Root::String(*ptr))
                } else {
                    None
                }
            }
            ValueKind::Symbol(locked, ptr) => {
                if !*locked {
                    Some(Root::Symbol(*ptr))
                } else {
                    None
                }
            }
            ValueKind::Bytevec(locked, ptr) => {
                if !*locked {
                    Some(Root::Bytevec(*ptr))
                } else {
                    None
                }
            }
            ValueKind::Pair(locked, ptr) => {
                if !*locked {
                    Some(Root::Pair(*ptr))
                } else {
                    None
                }
            }
            ValueKind::Vector(locked, ptr) => {
                if !*locked {
                    Some(Root::Vector(*ptr))
                } else {
                    None
                }
            }
        }
    }
}
