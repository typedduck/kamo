use std::fmt;

use crate::parser::Span;

use super::code::Code;

/// Defines an error which occurred during parsing.
/// 
/// The cause is represented by a [`Span`], a [`Code`] and a message. The
/// [`Span`] indicates the position in the input where the error occurred. The
/// [`Code`] indicates the type of the error. The message is a human readable
/// description of the error.
/// 
/// The cause can be printed in two different ways. The default way only prints
/// the message. When printed in alternate mode, the [`Span`], and the message
/// are printed. In debug mode, the [`Code`] is printed, too. When alternate
/// mode is enabled the [`Span`] is printed with the byte offset.
pub struct Cause {
    span: Span,
    code: Code,
    message: Box<str>,
}

impl Cause {
    /// Creates a new `Cause` with the given `span`, `code` and `message`.
    pub fn new<S, M>(span: S, code: Code, message: M) -> Self
    where
        S: Into<Span>,
        M: fmt::Display,
    {
        Self {
            span: span.into(),
            code,
            message: message.to_string().into_boxed_str(),
        }
    }

    /// Returns the [`Span`] of the cause.
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the [`Code`] of the cause.
    #[inline]
    pub fn code(&self) -> Code {
        self.code
    }

    /// Returns the message of the cause.
    #[inline]
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl PartialEq for Cause {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.code == other.code
    }
}

impl fmt::Display for Cause {
    /// Prints the cause. In alternate mode the [`Span`] is printed before the
    /// message.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}: {}", self.span, self.message)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl fmt::Debug for Cause {
    /// Prints the cause. In alternate mode the [`Span`] is printed with the
    /// byte offset. The [`Code`] is printed before the message.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:?}: [0x{:08x}] {}", self.span, self.code, self.message)
        } else {
            write!(f, "{}: [0x{:08x}] {}", self.span, self.code, self.message)
        }
    }
}
