//! # Runtime value type.
//! 
//! This module contains the runtime value type. The value type is used to
//! represent the values of the language. It is used in the AST, the evaluator
//! and the compiler. The value type is a wrapper around an enum, which means
//! that it can be one of multiple types. The value type is also a smart
//! pointer, which means that it can be cloned and shared between multiple
//! owners. Cloning is cheap, because the value type uses reference counting for
//! heap allocated values.
//! 
//! There are to destinct types of values: intermediate and heap allocated
//! values.
//! 
//! ## Intermediate values
//! 
//! Intermediate values are values that are not heap allocated. They are used
//! for primitive values, like integers, floats, booleans and characters. They
//! are immutable and can be copied very cheaply.
//! 
//! There are constructors for all primitive values. For example, the
//! [`Value::new_int()`](value::Value::new_int) constructor creates an
//! intermediate integer value. Every primitive value has a corresponding
//! conversion method into the corresponding Rust primitive type. For example,
//! the [`Value::as_int()`](value::Value::as_int) method converts an integer
//! value into a Rust integer. The return type of these conversion methods is
//! [`Option`], because only when the value is of the correct type, the
//! conversion succeeds. There is no implicit conversion between types.
//! 
//! Additionally, there are predicates for all primitive values. For example,
//! the [`Value::is_int()`](value::Value::is_int) predicate returns `true` if
//! the value is an integer value.
//! 
//! In summery, there are three types of methods for intermediate values:
//! constructors, conversion methods and predicates.
//! 
//! ## Heap allocated values
//! 
//! Heap allocated values are managed by the [`Mutator`](crate::mem::Mutator).
//! They are used for complex values, like strings, vectors and pairs. They are
//! mutable and can be shared between multiple owners. They are reference
//! counted, which means that they are collected by the garbage collector when
//! they are no longer reachable.
//! 
//! There are constructors for all heap allocated values. For example, the
//! [`Value::new_string()`](value::Value::new_string) constructor creates a
//! heap allocated string value. Every heap allocated value has a corresponding
//! conversion method into the corresponding Rust type. For example, the
//! [`Value::as_string()`](value::Value::as_string) method converts a string
//! value into a Rust string. The return type of these conversion methods is
//! [`Option`], because only when the value is of the correct type, the
//! conversion succeeds.
//! 
//! In contrast to intermediate values, the conversion can also be mutable. For
//! example, the [`Value::as_string_mut()`](value::Value::as_string_mut) method
//! converts a string value into a mutable Rust string. There is also a method
//! which returns a [`Pointer<T>`](crate::mem::Pointer) to the value. This
//! pointer can be used to access the typed value directly. For example, the
//! [`Value::as_string_ptr()`](value::Value::as_string_ptr) method returns a
//! pointer to the string value. The pointer can be used to access the string
//! value directly. The pointer is smart, which means that it is reference
//! counted and can be cloned and shared between multiple owners. The reference
//! is dropped when the pointer is dropped. The lifetime of the pointer is
//! distinct from the lifetime of the value but bounded by the lifetime of the
//! [`Mutator`](crate::mem::Mutator).
//! 
//! Additionally, there are predicates for all heap allocated values. For
//! example, the [`Value::is_string()`](value::Value::is_string) predicate
//! returns `true` if the value is a string value. As with intermediate values,
//! there is no implicit conversion between types.
//! 
//! In summery, there are four types of methods for heap allocated values:
//! constructors, conversion methods, mutable conversion methods and predicates.
//! 
//! ## Value tags
//! 
//! The value type is an enum, which means that it can be one of multiple
//! types. The tag of the value determines the type of the value. The type
//! [`ValueTag`] corresponds to the tag of the value. The tag can be accessed
//! with the [`Value::tag()`](value::Value::tag) method. The tag can be used to
//! determine the type of the value.
//! 
//! The tag is mainly used by [`ValueId`].
//! 
//! ## Value ids
//! 
//! A [`ValueId`] is a unique identifier for a [`Value`]. This is used to
//! identify a [`Value`] without having to borrow the [`Value`] itself. This is
//! useful for storing [`Value`]s as keys in a
//! [`HashMap`](std::collections::HashMap). It is also useful for storing
//! [`Value`]s in a [`HashSet`](std::collections::HashSet) because it is
//! guaranteed that two [`ValueId`]s are equal if and only if the corresponding
//! [`Value`]s are identical. This is achieved by storing the address of the
//! [`Value`]s as the identifier along with a tag that identifies the type of
//! [`Value`].
//! 
//! Given the [`Mutator`](crate::mem::Mutator) where the original [`Value`]
//! was allocated, a [`ValueId`] can be converted back into a [`Value`] using
//! [`ValueId::into_value()`].
//! 
//! ## Value pairs
//! 
//! A [`Pair`] is a pair of [`Value`]s. It is used to represent the cons cells
//! of the language. Pairs are a core concept of Lisp-like languages. They are
//! used to represent lists and other data structures. A pair is a heap
//! allocated value, which means that it is managed by the
//! [`Mutator`](crate::mem::Mutator).
//! 
//! ## Printing values
//! 
//! Values can be printed using the [`print()`](printer::print) function. This
//! function uses a [`Visitor`] to print the value with the [`SimplePrinter`].
//! The [`SimplePrinter`] uses the [`SimplePrinterVisitor`] to actually print
//! the value. The [`SimplePrinterVisitor`] prints the value in a human readable
//! Scheme-like format on a single line.
//! 
//! In order to be able to print a value in all kinds of formats, the
//! [`Visitor`] trait is used. The [`Value::accept()`](value::Value::accept)
//! method accepts a [`Visitor`] and calls the corresponding method on the
//! [`Visitor`]. This allows to implement different kinds of printers for
//! different kinds of formats or languages. The [`Value`] type itself does not
//! implement any printing logic.

mod ident;
pub use ident::{ValueId, ValueTag};

mod kind;
pub use kind::ValueKind;

mod pair;
pub use pair::Pair;

mod printer;
pub use printer::{print, SimplePrinter, SimplePrinterVisitor};

#[allow(clippy::module_inception)]
mod value;
pub use value::Value;

mod vector;
pub use vector::Vector;

mod visitor;
pub use visitor::Visitor;

/// A type alias for [`SmartString<LazyCompact>`].
///
/// Smart strings are used to store strings in the value type. This is to reduce
/// memory allocations and improve performance. The lazy compact variant expands
/// the string lazily, which means that the string is only expanded when it is
/// necessary. But when it is once expanded, it will never be compacted again.
///
/// [`SmartString<LazyCompact>`]: https://docs.rs/smartstring/0.2.6/smartstring/struct.SmartString.html
pub type SmartString = smartstring::SmartString<smartstring::LazyCompact>;
