#![allow(
    clippy::single_char_lifetime_names,
    clippy::implicit_return,
    clippy::question_mark_used,
    clippy::shadow_unrelated,
    clippy::shadow_reuse,
    clippy::missing_docs_in_private_items,
    clippy::missing_trait_methods,
    clippy::min_ident_chars,
    clippy::mod_module_files
)]

//! Macros for the `kamo` crate.
//!
//! This crate is not meant to be used directly. It is used by the `kamo` crate
//! to provide the `sexpr!`, `sexpr_file!`, and `sexpr_script!` macros.
//!
//! The macros are used to parse a string literal into a single or an array of
//! `kamo::value::Value`s. The macros are defined as follows:
//!
//! - `sexpr!(<mutator>, <expression>)` - A macro for parsing a single
//!   s-expression from a string. It returns a `kamo::value::Value`.
//!
//! - `sexpr_file!(<mutator>, <filename>)` - A macro for parsing multiple
//!   s-expressions from a file. It returns an array of `kamo::value::Value`s.
//!   The array may be empty.
//!  
//! - `sexpr_script!(<mutator>, <expressions>)` - A macro for parsing multiple
//!   s-expressions from a string. It returns an array of `kamo::value::Value`s.
//!   The array may be empty.
//!  
//! The macros all take an optional `MutatorRef` identifier. This is used to allocate
//! values on the heap. If the expression does not contain any values that need to
//! be allocated on the heap, then the `Mutator` identifier can be omitted.
//!  
//! The syntax for the macros is as defined by the Scheme standard R7RS for the
//! `read` procedure. The syntactic definition is the `<datum>` in section
//! [`7.1.2 External representations`](https://standards.scheme.org/official/r7rs.pdf)
//! of the standard.
//!  
//! The syntax deviations from the standard are:
//!  
//! - The extactness (`#e` and `#i`) of numbers is not supported. Floating-point
//!   numbers are always inexact and integers are always exact.
//!  
//! - Numbers may only be signed 64-bit integers or IEEE 754 double precision
//!   floating-point numbers. The standard allows for arbitrary precision
//!   integers, rationals and complex numbers.
//!  
//! - Labels are not supported.
//!  
//! - The `#;` comment syntax is only supported in the macros which parse
//!   multiple s-expressions. The `#;` comment syntax may not be nested.
//!  
//! - Character literals defined by a hex escape sequence may have 1 to 6
//!   digits. The standard excepts 1 or more digits. The code must be a valid
//!   Unicode code point.
//!  
//! The parser is implemented with the [`pest`](https://crates.io/crates/pest)
//! crate. The grammar is defined in `src/sexpr/sexpr.pest`. This is necessary
//! because the combination parser library defined in `kamo::parser` cannot be
//! used here. It would be cyclic dependency. There will be an implementation
//! of a parser for s-expressions in the `kamo::form` module in the future. It
//! will be based on the `kamo::parser` crate and will be used by the scheme
//! interpreter.
//!  
//! ## Examples
//!  
//! ```ignore
//! use kamo::{mem::Mutator, sexpr, value::{print, Value}};
//!  
//! let m = Mutator::new_ref();
//! let value = sexpr!(m, "(1 2 3)");
//!  
//! assert_eq!(print(value).to_string(), "(1 2 3)");
//! ```
//!  
//! ```ignore
//! use kamo::{sexpr_file, value::{print, Value}};
//!  
//! let m = Mutator::new_ref();
//! let values: &[Value] = &sexpr_file!(m, "tests/sexpr/values.scm");
//!  
//! assert_eq!(values.len(), 3);
//! assert_eq!(print(values[0].clone()).to_string(), "()");
//! assert_eq!(print(values[1].clone()).to_string(), "100");
//! assert_eq!(print(values[2].clone()).to_string(), "#t");
//!
//! let values: &[Value] = &sexpr_file!("tests/sexpr/empty.scm");
//! assert_eq!(values.len(), 0);
//! ```
//!  
//! ```ignore
//! use kamo::{mem::Mutator, sexpr_script, value::{print, Value}};
//!  
//! let m = Mutator::new_ref();
//! let values: &[Value] = &sexpr_script!(m, "(define a 1)\n(define b 2)\n(+ a b)");
//!  
//! assert_eq!(values.len(), 3);
//! assert_eq!(print(values[0].clone()).to_string(), "(define a 1)");
//! assert_eq!(print(values[1].clone()).to_string(), "(define b 2)");
//! assert_eq!(print(values[2].clone()).to_string(), "(+ a b)");
//!
//! let values: &[Value] = &sexpr_script!("");
//! assert_eq!(values.len(), 0);
//! ```

extern crate alloc;

use proc_macro::TokenStream;

mod expand;
mod sexpr;

/// Parse a string of a single s-expression into a `kamo::value::Value`.
///
/// The syntax is defined by the Scheme standard R7RS for the `read` procedure.
/// For the deviations from the standard see the documentation of the `macros`
/// crate.
///
/// The macro takes an optional `MutatorRef` identifier. This is used to
/// allocate values on the heap. If the expression does not contain any values
/// that need to be allocated on the heap, then the `Mutator` identifier can be
/// omitted.
///
/// The syntax is `sexpr!(<mutator>, <expression>)`.
///
/// # Examples
///
/// ```ignore
/// use kamo::{mem::Mutator, sexpr, value::{print, Value}};
///  
/// let m = Mutator::new_ref();
/// let value = sexpr!(m, "(1 2 3)");
///  
/// assert_eq!(print(value).to_string(), "(1 2 3)");
/// ```
#[proc_macro]
pub fn sexpr(input: TokenStream) -> TokenStream {
    expand::sexpr(input.into()).into()
}

/// Parse a file of s-expressions into an array of `kamo::value::Value`s.
///
/// The syntax is defined by the Scheme standard R7RS for the `read` procedure.
/// For the deviations from the standard see the documentation of the `macros`
/// crate.
///
/// The macro takes an optional `MutatorRef` identifier. This is used to
/// allocate values on the heap. If the expression does not contain any values
/// that need to be allocated on the heap, then the `Mutator` identifier can be
/// omitted.
///
/// The syntax is `sexpr_file!(<mutator>, <filename>)`.
///
/// # Examples
///
/// ```ignore
/// use kamo::{sexpr_file, value::{print, Value}};
///  
/// let values: &[Value] = &sexpr_file!(m, "tests/sexpr/values.scm");
///  
/// assert_eq!(values.len(), 3);
/// assert_eq!(print(values[0].clone()).to_string(), "()");
/// assert_eq!(print(values[1].clone()).to_string(), "100");
/// assert_eq!(print(values[2].clone()).to_string(), "#t");
///
/// let values: &[Value] = &sexpr_file!("tests/sexpr/empty.scm");
/// assert_eq!(values.len(), 0);
/// ```
#[proc_macro]
pub fn sexpr_file(input: TokenStream) -> TokenStream {
    expand::sexpr_file(input.into()).into()
}

/// Parse a string of s-expressions into an array of `kamo::value::Value`s.
///
/// The syntax is defined by the Scheme standard R7RS for the `read` procedure.
/// For the deviations from the standard see the documentation of the `macros`
/// crate.
///
/// The macro takes an optional `MutatorRef` identifier. This is used to
/// allocate values on the heap. If the expression does not contain any values
/// that need to be allocated on the heap, then the `Mutator` identifier can be
/// omitted.
///
/// The syntax is `sexpr_script!(<mutator>, <expressions>)`.
///
/// # Examples
///
/// ```ignore
/// use kamo::{mem::Mutator, sexpr_script, value::{print, Value}};
///  
/// let m = Mutator::new_ref();
/// let values: &[Value] = &sexpr_script!(m, "(define a 1)\n(define b 2)\n(+ a b)");
///  
/// assert_eq!(values.len(), 3);
/// assert_eq!(print(values[0].clone()).to_string(), "(define a 1)");
/// assert_eq!(print(values[1].clone()).to_string(), "(define b 2)");
/// assert_eq!(print(values[2].clone()).to_string(), "(+ a b)");
///
/// let values: &[Value] = &sexpr_script!("");
///  
/// assert_eq!(values.len(), 0);
/// ```
#[proc_macro]
pub fn sexpr_script(input: TokenStream) -> TokenStream {
    expand::sexpr_script(input.into()).into()
}
