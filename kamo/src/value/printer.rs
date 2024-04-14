use std::{collections::HashSet, fmt};
use unicode_ident::{is_xid_continue, is_xid_start};

#[cfg(feature = "types")]
use crate::types::Type;

use super::{Pair, Value, Vector, Visitor};

/// This function prints a value on a single line. It formats the value in a
/// Scheme-like syntax.
///
/// The returned [`SimplePrinter`](SimplePrinter) implements the
/// [`Display`](std::fmt::Display) trait and can be used to print the value.
///
/// # Example
///
/// ```rust
/// use kamo::value::{print, Value};
///
/// let value = Value::new_int(42);
/// let printer = print(value);
///
/// assert_eq!(printer.to_string(), "42");
/// ```
#[must_use]
#[inline]
pub const fn print(value: Value<'_>) -> SimplePrinter<'_> {
    SimplePrinter(value)
}

/// A wrapper around a [`Value`](super::Value) that implements the
/// [`Display`](std::fmt::Display) trait. It can be used to print a value in a
/// Scheme-like syntax. The output is a single line.
///
/// # Example
///
/// ```rust
/// use kamo::value::{SimplePrinter, Value};
///
/// let value = Value::new_int(42);
/// let printer = SimplePrinter(value);
///
/// assert_eq!(printer.to_string(), "42");
/// ```
#[derive(Debug)]
pub struct SimplePrinter<'a>(pub Value<'a>);

impl<'a> fmt::Display for SimplePrinter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visitor = SimplePrinterVisitor(f);
        self.0.accept(&mut visitor)
    }
}

/// A visitor that prints a value in a Scheme-like syntax to the given
/// [`Formatter`](std::fmt::Formatter).
///
/// It is used by the [`SimplePrinter`](SimplePrinter) to print a value. The
/// value is printed on a single line.
///
/// # Note
///
/// The implementation of the visit-methods considers that the value may be
/// circular. In order to prevent infinite recursion, the implementation checks
/// if the value, either a list or vector, has already been visited and if so,
/// returns early. Circular lists are printed as `<cyclic list>` and circular
/// vectors as `<cyclic vector>`.
///
/// # Example
///
/// ```rust
/// use kamo::{
///     mem::Mutator,
///     value::{SimplePrinterVisitor, Value}
/// };
///
/// // A wrapper around a `Value` that implements `Display`.
/// struct SimplePrinter<'a>(pub Value<'a>);
///
/// impl<'a> std::fmt::Display for SimplePrinter<'a> {
///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///         // Create a visitor that prints the value.
///         let mut visitor = SimplePrinterVisitor(f);
///         // Accept the visitor.
///         self.0.accept(&mut visitor)
///     }
/// }
///
/// // Display a simple value.
/// let value = Value::new_int(42);
/// let printer = SimplePrinter(value);
///
/// assert_eq!(printer.to_string(), "42");
///
/// // Create a `Mutator` to allocate values.
/// let m = Mutator::new_ref();
///
/// // Display a simple list.
/// let value = Value::new_list(
///     m.clone(),
///     [Value::new_int(42), Value::new_int(43)],
/// );
/// let printer = SimplePrinter(value);
///
/// assert_eq!(printer.to_string(), "(42 43)");
///
/// // Display a cyclic list.
/// let list = Value::new_list(
///     m.clone(),
///     [Value::new_int(42), Value::new_int(43), Value::new_int(44)],
/// );
/// // Make the list cyclic by setting the cdr of the last pair to the list.
/// list.as_pair().unwrap()
///     .cddr().unwrap()
///     .as_pair_mut().unwrap()
///     .set_cdr(list.clone());
/// let printer = SimplePrinter(list);
///
/// assert_eq!(
///     printer.to_string(),
///     format!("(42 43 44 <cyclic list>)")
/// );
/// ```
pub struct SimplePrinterVisitor<'a, 'b>(pub &'a mut fmt::Formatter<'b>);

impl<'a, 'b> Visitor for SimplePrinterVisitor<'a, 'b> {
    type Result = fmt::Result;

    fn visit_nil(&mut self) -> Self::Result {
        write!(self.0, "()")
    }

    fn visit_true(&mut self) -> Self::Result {
        write!(self.0, "#t")
    }

    fn visit_false(&mut self) -> Self::Result {
        write!(self.0, "#f")
    }

    fn visit_char(&mut self, value: char) -> Self::Result {
        write!(self.0, "#\\")?;
        if value.is_ascii() {
            match value {
                '\x07' => write!(self.0, "alarm"),
                '\x08' => write!(self.0, "backspace"),
                '\x7f' => write!(self.0, "delete"),
                '\x1b' => write!(self.0, "escape"),
                '\n' => write!(self.0, "newline"),
                '\0' => write!(self.0, "null"),
                '\r' => write!(self.0, "return"),
                ' ' => write!(self.0, "space"),
                '\t' => write!(self.0, "tab"),
                value if value.is_ascii_control() => write!(self.0, "x{:02x}", value as u32),
                value => write!(self.0, "{value}"),
            }
        } else if is_xid_start(value) || is_xid_continue(value) {
            write!(self.0, "{value}")
        } else {
            write!(self.0, "u{{{:x}}}", value as u32)
        }
    }

    fn visit_integer(&mut self, value: i64) -> Self::Result {
        write!(self.0, "{value}")
    }

    fn visit_float(&mut self, value: f64) -> Self::Result {
        if value.is_nan() {
            write!(self.0, "+nan.0")
        } else if value.is_infinite() {
            write!(
                self.0,
                "{}inf.0",
                if value.is_sign_negative() { "-" } else { "+" }
            )
        } else {
            write!(self.0, "{value}")
        }
    }

    fn visit_pair(&mut self, value: &Pair<'_>) -> Self::Result {
        let mut seen = HashSet::new();
        let mut tail = value.cdr();

        write!(self.0, "(")?;
        seen.insert(value as *const Pair<'_> as usize);
        value.car().accept(self)?;
        while let Some(value) = tail.as_pair() {
            let ptr = value as *const Pair<'_> as usize;

            if seen.contains(&ptr) {
                return write!(self.0, " <cyclic list>)");
            }
            seen.insert(ptr);
            write!(self.0, " ")?;
            value.car().accept(self)?;
            tail = value.cdr();
        }
        if !tail.is_nil() {
            write!(self.0, " . ")?;
            tail.accept(self)?;
        }
        write!(self.0, ")")
    }

    fn visit_string(&mut self, value: &str) -> Self::Result {
        write!(self.0, "{value:?}")
    }

    fn visit_symbol(&mut self, value: &str) -> Self::Result {
        write!(self.0, "{value}")
    }

    fn visit_bytevec(&mut self, value: &[u8]) -> Self::Result {
        write!(self.0, "#u8(")?;
        if let Some((first, rest)) = value.split_first() {
            write!(self.0, "{first}")?;
            for byte in rest {
                write!(self.0, " {byte}")?;
            }
        }
        write!(self.0, ")")
    }

    fn visit_vector(&mut self, value: &Vector<'_>) -> Self::Result {
        let seen = value as *const Vector<'_> as usize;

        write!(self.0, "#(")?;
        if let Some((first, rest)) = value.split_first() {
            let ptr = first
                .as_vector()
                .map_or(0, |v| v as *const Vector<'_> as usize);

            if ptr == seen {
                write!(self.0, "<cyclic vector>")?;
            } else {
                first.accept(self)?;
            }
            for value in rest {
                let ptr = value
                    .as_vector()
                    .map_or(0, |v| v as *const Vector<'_> as usize);

                if ptr == seen {
                    write!(self.0, " <cyclic vector>")?;
                } else {
                    write!(self.0, " ")?;
                    value.accept(self)?;
                }
            }
        }
        write!(self.0, ")")
    }

    #[cfg(feature = "types")]
    fn visit_type(&mut self, value: &Type) -> Self::Result {
        write!(self.0, "#<type {value}>")
    }
}

#[cfg(test)]
mod tests {
    use crate::mem::Mutator;

    use super::*;

    #[allow(clippy::cognitive_complexity)]
    #[test]
    fn print_atoms() {
        let m = Mutator::new_ref();

        assert_eq!(print(Value::new_nil()).to_string(), "()");
        assert_eq!(print(Value::new_bool(true)).to_string(), "#t");
        assert_eq!(print(Value::new_bool(false)).to_string(), "#f");
        assert_eq!(print(Value::new_int(42)).to_string(), "42");
        assert_eq!(print(Value::new_float(42.0)).to_string(), "42");
        assert_eq!(print(Value::new_float(42.5)).to_string(), "42.5");
        assert_eq!(print(Value::new_char('a')).to_string(), "#\\a");
        assert_eq!(print(Value::new_char(' ')).to_string(), "#\\space");
        assert_eq!(print(Value::new_char('\n')).to_string(), "#\\newline");
        assert_eq!(print(Value::new_char('\x07')).to_string(), "#\\alarm");
        assert_eq!(print(Value::new_char('\x08')).to_string(), "#\\backspace");
        assert_eq!(print(Value::new_char('\x7f')).to_string(), "#\\delete");
        assert_eq!(print(Value::new_char('\x1b')).to_string(), "#\\escape");
        assert_eq!(print(Value::new_char('\0')).to_string(), "#\\null");
        assert_eq!(print(Value::new_char('\r')).to_string(), "#\\return");
        assert_eq!(print(Value::new_char('\t')).to_string(), "#\\tab");
        assert_eq!(print(Value::new_char('\x1f')).to_string(), "#\\x1f");
        assert_eq!(print(Value::new_char('\u{80}')).to_string(), "#\\u{80}");
        assert_eq!(
            print(Value::new_symbol(m.clone(), "foo")).to_string(),
            "foo"
        );
        assert_eq!(
            print(Value::new_bytevec(m.clone(), [0, 1, 2])).to_string(),
            "#u8(0 1 2)"
        );
        assert_eq!(print(Value::new_string(m.clone(), "")).to_string(), "\"\"");
        assert_eq!(
            print(Value::new_string(m.clone(), "foo")).to_string(),
            "\"foo\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\n")).to_string(),
            "\"foo\\n\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\"")).to_string(),
            "\"foo\\\"\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\\")).to_string(),
            "\"foo\\\\\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\t")).to_string(),
            "\"foo\\t\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\r")).to_string(),
            "\"foo\\r\""
        );
        assert_eq!(
            print(Value::new_string(m.clone(), "foo\x07")).to_string(),
            "\"foo\\u{7}\""
        );
        assert_eq!(print(Value::new_vector(m.clone(), &[])).to_string(), "#()");
        assert_eq!(
            print(Value::new_vector(m.clone(), &[Value::new_int(42)])).to_string(),
            "#(42)"
        );
        assert_eq!(
            print(Value::new_vector(
                m.clone(),
                &[Value::new_int(42), Value::new_int(43)]
            ))
            .to_string(),
            "#(42 43)"
        );
    }

    #[test]
    fn print_pairs() {
        let m = Mutator::new_ref();

        assert_eq!(
            print(Value::new_cons(
                m.clone(),
                Value::new_int(42),
                Value::new_nil()
            ))
            .to_string(),
            "(42)"
        );
        assert_eq!(
            print(Value::new_cons(
                m.clone(),
                Value::new_int(42),
                Value::new_int(43)
            ))
            .to_string(),
            "(42 . 43)"
        );
        assert_eq!(
            print(Value::new_cons(
                m.clone(),
                Value::new_nil(),
                Value::new_int(43)
            ))
            .to_string(),
            "(() . 43)"
        );
    }

    #[test]
    fn print_lists() {
        let m = Mutator::new_ref();

        assert_eq!(
            print(Value::new_list(m.clone(), [Value::new_int(42)])).to_string(),
            "(42)"
        );
        assert_eq!(
            print(Value::new_list(
                m.clone(),
                [Value::new_int(42), Value::new_int(43)]
            ))
            .to_string(),
            "(42 43)"
        );
        assert_eq!(
            print(Value::new_list(
                m.clone(),
                [Value::new_int(42), Value::new_int(43), Value::new_int(44)]
            ))
            .to_string(),
            "(42 43 44)"
        );
    }

    #[allow(clippy::similar_names)]
    #[test]
    fn print_cyclic_list() {
        let m = Mutator::new_ref();

        let list = Value::new_list(
            m.clone(),
            [Value::new_int(42), Value::new_int(43), Value::new_int(44)],
        );
        let mut last = list.as_pair().unwrap().cddr().expect("cddr");
        last.as_pair_mut().unwrap().set_cdr(list.clone());

        assert_eq!(
            print(list.clone()).to_string(),
            format!("(42 43 44 <cyclic list>)")
        );
        assert_eq!(
            print(Value::new_cons(m.clone(), list.clone(), Value::new_nil())).to_string(),
            format!("((42 43 44 <cyclic list>))")
        );
        assert_eq!(
            print(Value::new_cons(m.clone(), Value::new_nil(), list.clone())).to_string(),
            format!("(() 42 43 44 <cyclic list>)")
        );
        assert_eq!(
            print(Value::new_cons(m.clone(), list.clone(), list)).to_string(),
            format!("((42 43 44 <cyclic list>) 42 43 44 <cyclic list>)")
        );
    }

    #[test]
    fn print_cyclic_vector() {
        let m = Mutator::new_ref();

        let mut vector = Value::new_vector(
            m.clone(),
            &[Value::new_int(42), Value::new_int(43), Value::new_int(44)],
        );
        let cycle = vector.clone();

        vector.as_vector_mut().unwrap().push(cycle);

        assert_eq!(
            print(vector).to_string(),
            format!("#(42 43 44 <cyclic vector>)")
        );
    }
}
