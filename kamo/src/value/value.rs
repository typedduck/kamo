use std::{fmt, marker::PhantomData, ptr::NonNull};

use smallvec::SmallVec;

#[cfg(feature = "types")]
use crate::types::Type;
use crate::{
    mem::{Mutator, MutatorRef, Pointer, Root, ToRoot},
    value::SmartString,
};

use super::{ByteVector, Pair, ValueId, ValueKind, ValueTag, Vector, Visitor};

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
    #[must_use]
    #[inline]
    pub const fn new_nil() -> Self {
        Self::new(ValueKind::Nil)
    }

    /// Creates a new boolean immediate-value.
    #[must_use]
    #[inline]
    pub const fn new_bool(value: bool) -> Self {
        Self::new(ValueKind::Bool(value))
    }

    /// Creates a new character immediate-value.
    #[must_use]
    #[inline]
    pub const fn new_char(value: char) -> Self {
        Self::new(ValueKind::Char(value))
    }

    /// Creates a new integer immediate-value.
    #[must_use]
    #[inline]
    pub const fn new_int(value: i64) -> Self {
        Self::new(ValueKind::Integer(value))
    }

    /// Creates a new float immediate-value.
    #[must_use]
    #[inline]
    pub const fn new_float(value: f64) -> Self {
        Self::new(ValueKind::Float(value))
    }

    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    /// Creates a new type.
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_type(m: MutatorRef<'a>, ty: Type) -> Self {
        let ty = m.borrow_mut().new_type(ty);
        ty.into()
    }

    /// Creates a new symbol.
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_symbol(m: MutatorRef<'a>, name: impl AsRef<str>) -> Self {
        let symbol = m.borrow_mut().new_symbol(name);
        symbol.into()
    }

    /// Creates a new string.
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_string(m: MutatorRef<'a>, string: impl AsRef<str>) -> Self {
        let string = m.borrow_mut().new_string(string);
        string.into()
    }

    /// Creates a new byte-vector.
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_bytevec(m: MutatorRef<'a>, bytevec: impl AsRef<[u8]>) -> Self {
        let bytevec = m.borrow_mut().new_bytevec(bytevec);
        bytevec.into()
    }

    /// Creates a new vector.
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_vector(m: MutatorRef<'a>, vector: impl Into<Vec<Value<'a>>>) -> Self {
        let vector = m.borrow_mut().new_vector(vector);
        vector.into()
    }

    /// Creates a new pair. This is equivalent to [`Value::new_cons()`].
    #[allow(clippy::similar_names, clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_pair(m: MutatorRef<'a>, car: Value<'a>, cdr: Value<'a>) -> Self {
        Self::new_cons(m, car, cdr)
    }

    /// Creates a new cons cell.
    #[allow(clippy::similar_names, clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_cons(m: MutatorRef<'a>, car: Value<'a>, cdr: Value<'a>) -> Self {
        let pair = m.borrow_mut().new_pair(car, cdr);
        pair.into()
    }

    /// Creates a new proper list from an iterator of values. It takes ownership
    /// of the values and stores them in the list.
    ///
    /// An empty iterator returns the empty list `nil`.
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
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
    #[inline]
    pub fn new_list(m: MutatorRef<'a>, list: impl IntoIterator<Item = Value<'a>>) -> Self {
        Self::new_dotlist_map(m, list, None, |v| v)
    }

    /// Creates a new proper list from an iterator of values of `T`.
    ///
    /// The values in the iterator are of type `T`. The function `f` is used to
    /// convert the values into `Value<'a>`.
    ///
    /// An empty iterator returns the empty list `nil`.
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
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
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
    /// This function is equivalent to [`Value::new_list_map()`] with the
    /// identity function as the last argument.
    ///
    /// An empty iterator returns the empty list `nil`.
    ///
    /// An alternative end of list may be set to a value other than `nil` by
    /// passing it as the second optional argument. If this argument is `None`
    /// or `Some(nil)`, the end of list is set to `nil`.
    ///
    /// # Panics
    ///
    /// Panics if the iterator is empty and the alternative end of list is
    /// neither `None` nor `Some(nil)`.
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
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
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
    /// An empty iterator returns the empty list `nil`.
    ///
    /// An alternative end of list may be set to a value other than `nil` by
    /// passing it as the second optional argument. If this argument is `None`
    /// or `Some(nil)`, the end of list is set to `nil`.
    ///
    /// # Panics
    ///
    /// Panics if the iterator is empty and the alternative end of list is
    /// neither `None` nor `Some(nil)`.
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
    #[allow(clippy::needless_pass_by_value)]
    #[must_use]
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
        let eol = eol.map_or(Value::new_nil(), &mut f);

        if let Some(car) = list.next() {
            let mut m = m.borrow_mut();
            let head = m.new_pair(f(car), Value::new_nil());
            let mut tail = head.clone();

            for car in list {
                let cdr = m.new_pair(f(car), Value::new_nil());
                tail.set_cdr(cdr.clone().into());
                tail = cdr;
            }
            tail.set_cdr(eol);
            head.into()
        } else {
            assert!(eol.is_nil(), "alternative end of list must be nil");
            Value::new_nil()
        }
    }

    /* #endregion */

    /* #region Conversion */

    /// If the value is `nil` or the empty list, returns `Some(())`. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub const fn as_nil(&self) -> Option<()> {
        match self.inner {
            ValueKind::Nil => Some(()),
            _ => None,
        }
    }

    /// If the value is a boolean, returns it as a `bool`. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_bool(&self) -> Option<bool> {
        match self.inner {
            ValueKind::Bool(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a character, returns it as a `char`. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_char(&self) -> Option<char> {
        match self.inner {
            ValueKind::Char(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is an integer, returns it as an `i64`. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_int(&self) -> Option<i64> {
        match self.inner {
            ValueKind::Integer(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a float, returns it as an `f64`. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_float(&self) -> Option<f64> {
        match self.inner {
            ValueKind::Float(value) => Some(value),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a [`Pair`]. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_pair(&self) -> Option<&Pair<'a>> {
        match self.inner {
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a mutable [`Pair`]. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub fn as_pair_mut(&mut self) -> Option<&mut Pair<'a>> {
        match self.inner {
            ValueKind::Pair(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a pair, returns it as a `Pointer<'a, Pair<'a>>`.
    /// Otherwise, returns `None`.
    #[must_use]
    pub fn as_pair_ptr(&self) -> Option<Pointer<'a, Pair<'a>>> {
        match self.inner {
            ValueKind::Pair(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a [`SmartString`]. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub const fn as_string(&self) -> Option<&SmartString> {
        match self.inner {
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a mutable [`SmartString`].
    /// Otherwise, returns `None`.
    #[must_use]
    #[inline]
    pub fn as_string_mut(&mut self) -> Option<&mut SmartString> {
        match self.inner {
            ValueKind::String(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a string, returns it as a `Pointer<'a, SmartString>`.
    /// Otherwise, returns `None`.
    #[must_use]
    pub fn as_string_ptr(&self) -> Option<Pointer<'a, SmartString>> {
        match self.inner {
            ValueKind::String(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a symbol, returns it as a `&str`. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub fn as_symbol(&self) -> Option<&str> {
        match self.inner {
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(AsRef::as_ref),
            _ => None,
        }
    }

    /// If the value is a symbol, returns it as a `Pointer<'a, Box<str>>`.
    /// Otherwise, returns `None`.
    #[must_use]
    #[inline]
    pub fn as_symbol_ptr(&self) -> Option<Pointer<'a, Box<str>>> {
        match self.inner {
            ValueKind::Symbol(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a `&[u8]`. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub fn as_bytevec(&self) -> Option<&[u8]> {
        match self.inner {
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(SmallVec::as_slice),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a mutable
    /// `&mut ByteVector`. Otherwise, returns `None`.
    #[must_use]
    #[inline]
    pub fn as_bytevec_mut(&mut self) -> Option<&mut ByteVector> {
        match self.inner {
            ValueKind::Bytevec(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a byte-vector, returns it as a
    /// `Pointer<'a, ByteVector>`. Otherwise, returns `None`.
    #[must_use]
    #[inline]
    pub fn as_bytevec_ptr(&self) -> Option<Pointer<'a, ByteVector>> {
        match self.inner {
            ValueKind::Bytevec(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a [`Vector`]. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_vector(&self) -> Option<&Vector<'a>> {
        match self.inner {
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a mutable [`Vector`]. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub fn as_vector_mut(&mut self) -> Option<&mut Vector<'a>> {
        match self.inner {
            ValueKind::Vector(_, mut ptr) => unsafe { ptr.as_mut() }.value_mut(),
            _ => None,
        }
    }

    /// If the value is a vector, returns it as a `Pointer<'a, Vector<'a>>`.
    /// Otherwise, returns `None`.
    #[must_use]
    #[inline]
    pub fn as_vector_ptr(&self) -> Option<Pointer<'a, Vector<'a>>> {
        match self.inner {
            ValueKind::Vector(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    /// If the value is a type, returns it as a [`Type`]. Otherwise, returns
    /// `None`.
    #[must_use]
    #[inline]
    pub const fn as_type(&self) -> Option<&Type> {
        match self.inner {
            ValueKind::Type(_, ptr) => unsafe { ptr.as_ref() }.value(),
            _ => None,
        }
    }

    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    /// If the value is a type, returns it as a `Pointer<'a, Type>`. Otherwise,
    /// returns `None`.
    #[must_use]
    #[inline]
    pub fn as_type_ptr(&self) -> Option<Pointer<'a, Type>> {
        match self.inner {
            ValueKind::Type(_, ptr) => Some(ptr.into()),
            _ => None,
        }
    }

    /// Returns the inner value.
    #[must_use]
    #[inline]
    pub const fn kind(&self) -> &ValueKind<'a> {
        &self.inner
    }

    /* #endregion */

    /* #region Predicates */

    /// Returns `true` if the value is `nil`.
    #[must_use]
    #[inline]
    pub const fn is_nil(&self) -> bool {
        matches!(self.inner, ValueKind::Nil)
    }

    /// Returns `true` if the value is `true`.
    #[must_use]
    #[inline]
    pub const fn is_true(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(true))
    }

    /// Returns `true` if the value is `false` or `nil`.
    #[must_use]
    #[inline]
    pub const fn is_false(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(false) | ValueKind::Nil)
    }

    /// Returns `true` if the value is a boolean.
    #[must_use]
    #[inline]
    pub const fn is_bool(&self) -> bool {
        matches!(self.inner, ValueKind::Bool(_))
    }

    /// Returns `true` if the value is a character.
    #[must_use]
    #[inline]
    pub const fn is_char(&self) -> bool {
        matches!(self.inner, ValueKind::Char(_))
    }

    /// Returns `true` if the value is an integer.
    #[must_use]
    #[inline]
    pub const fn is_int(&self) -> bool {
        matches!(self.inner, ValueKind::Integer(_))
    }

    /// Returns `true` if the value is a float.
    #[must_use]
    #[inline]
    pub const fn is_float(&self) -> bool {
        matches!(self.inner, ValueKind::Float(_))
    }

    /// Returns `true` if the value is a number.
    #[must_use]
    #[inline]
    pub const fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    /// Returns `true` if the value is a pair.
    #[must_use]
    #[inline]
    pub const fn is_pair(&self) -> bool {
        matches!(self.inner, ValueKind::Pair(_, _))
    }

    /// Returns `true` if the value is a string.
    #[must_use]
    #[inline]
    pub const fn is_string(&self) -> bool {
        matches!(self.inner, ValueKind::String(_, _))
    }

    /// Returns `true` if the value is a symbol.
    #[must_use]
    #[inline]
    pub const fn is_symbol(&self) -> bool {
        matches!(self.inner, ValueKind::Symbol(_, _))
    }

    /// Returns `true` if the value is a byte-vector.
    #[must_use]
    #[inline]
    pub const fn is_bytevec(&self) -> bool {
        matches!(self.inner, ValueKind::Bytevec(_, _))
    }

    /// Returns `true` if the value is a vector.
    #[must_use]
    #[inline]
    pub const fn is_vector(&self) -> bool {
        matches!(self.inner, ValueKind::Vector(_, _))
    }

    #[cfg(feature = "types")]
    #[cfg_attr(docsrs, doc(cfg(feature = "types")))]
    /// Returns `true` if the value is a type.
    #[must_use]
    #[inline]
    pub const fn is_type(&self) -> bool {
        matches!(self.inner, ValueKind::Type(_, _))
    }

    /// Returns `true` if the value is an atom, or rather not a pair.
    #[must_use]
    #[inline]
    pub const fn is_atom(&self) -> bool {
        !(self.is_pair() || self.is_nil())
    }

    /// Returns `true` if the value is evaluating, or rather not an atom.
    /// Evaluating values are pairs, symbols, and `nil`.
    #[must_use]
    #[inline]
    pub const fn is_evaluating(&self) -> bool {
        self.is_symbol() || self.is_pair() || self.is_nil()
    }

    /// Returns `true` if the value is a proper list. Returns `false` if the
    /// value is not a proper list or if the list is circular.
    #[must_use]
    #[inline]
    pub fn is_list(&self) -> bool {
        self.as_pair().map_or(false, Pair::is_list)
    }

    /// Returns `Some(true)` if the value is an empty list, [`Vector`], string,
    /// symbol, or byte-vector. Returns Some(`false`) if the value is not empty
    /// and `None` otherwise.
    #[must_use]
    pub fn is_empty(&self) -> Option<bool> {
        match self.inner {
            ValueKind::Nil => Some(true),
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value().map(Pair::is_empty),
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value().map(Vector::is_empty),
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value().map(SmartString::is_empty),
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.is_empty()),
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(SmallVec::is_empty),
            _ => None,
        }
    }

    /* #endregion */

    /* #region Properties */

    /// Returns the id of the value.
    #[must_use]
    #[inline]
    pub fn id(&self) -> ValueId {
        self.into()
    }

    /// Returns the tag of the value.
    #[must_use]
    pub const fn tag(&self) -> ValueTag {
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
            #[cfg(feature = "types")]
            ValueKind::Type(_, _) => ValueTag::Type,
        }
    }

    /// Returns the length of the value. Returns `None` if the value does not
    /// have a length.
    ///
    /// Values that have a length are pairs, vectors, strings, symbols, and
    /// byte-vectors. `nil` has a length of `0`. Calculating the length of a
    /// list is linear to the length of the list. Circular lists are calculated
    /// correctly.
    #[must_use]
    pub fn len(&self) -> Option<usize> {
        match self.inner {
            ValueKind::Nil => Some(0),
            ValueKind::Pair(_, ptr) => unsafe { ptr.as_ref() }.value().map(Pair::len),
            ValueKind::Vector(_, ptr) => unsafe { ptr.as_ref() }.value().map(Vector::len),
            ValueKind::String(_, ptr) => unsafe { ptr.as_ref() }.value().map(SmartString::len),
            ValueKind::Symbol(_, ptr) => unsafe { ptr.as_ref() }.value().map(|v| v.len()),
            ValueKind::Bytevec(_, ptr) => unsafe { ptr.as_ref() }.value().map(SmallVec::len),
            _ => None,
        }
    }

    /* #endregion */

    /* #region Visitors */

    /// Accepts a visitor and calls the appropriate method.
    ///
    /// # Panics
    ///
    /// Panics if the value is a pair, string, symbol, byte-vector, vector, type,
    /// or procedure and the pointer is invalid. This is a bug and should be
    /// reported.
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
            #[cfg(feature = "types")]
            ValueKind::Type(_, ptr) => {
                let ty = unsafe { ptr.as_ref() }
                    .value()
                    .expect("invalid type pointer");

                visitor.visit_type(ty)
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
            #[cfg(feature = "types")]
            ValueKind::Type(locked, ptr) => {
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
            #[cfg(feature = "types")]
            ValueKind::Type(locked, ptr) => {
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

#[cfg(feature = "types")]
#[cfg_attr(docsrs, doc(cfg(feature = "types")))]
impl<'a> From<Pointer<'a, Type>> for Value<'a> {
    fn from(ptr: Pointer<'a, Type>) -> Self {
        let ptr = NonNull::new(unsafe { ptr.into_raw() }).expect("null-pointer");
        Self::new(ValueKind::Type(true, ptr))
    }
}

impl<'a> Drop for Value<'a> {
    fn drop(&mut self) {
        self.unlock();
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
                if *locked {
                    None
                } else {
                    Some(Root::String(*ptr))
                }
            }
            ValueKind::Symbol(locked, ptr) => {
                if *locked {
                    None
                } else {
                    Some(Root::Symbol(*ptr))
                }
            }
            ValueKind::Bytevec(locked, ptr) => {
                if *locked {
                    None
                } else {
                    Some(Root::Bytevec(*ptr))
                }
            }
            #[cfg(feature = "types")]
            ValueKind::Type(locked, ptr) => {
                if *locked {
                    None
                } else {
                    Some(Root::Type(*ptr))
                }
            }
            ValueKind::Pair(locked, ptr) => {
                if *locked {
                    None
                } else {
                    Some(Root::Pair(*ptr))
                }
            }
            ValueKind::Vector(locked, ptr) => {
                if *locked {
                    None
                } else {
                    Some(Root::Vector(*ptr))
                }
            }
        }
    }
}
