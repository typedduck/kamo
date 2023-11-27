use std::{error::Error as StdError, fmt};

use crate::parser::{Position, Span};

use super::{
    cause::Cause,
    code::{Code, ERR_EOF},
};

const UNEXPECTED_END_OF_FILE: &str = "unexpected end of file";

/// Defines an error which occurred during parsing.
///
/// The error is represented by an error stack which contains [`Cause`]s. Each
/// [`Cause`] represents a single error. The top [`Cause`] in the error stack
/// is the error which occurred last. The error stack can be printed in two
/// different ways. The default way prints the latest [`Cause`] in the error
/// stack. When printed with [`Debug`](std::fmt::Debug), the whole error stack
/// is printed.
///
/// If only a single cause is printed, a [`Cause`] in the stack which indicates
/// an unexpected end of file is preferred. If no such [`Cause`] exists, the
/// latest [`Cause`] is printed. See [`cause()`](ParseError) how the latest
/// [`Cause`] is determined.
pub struct ParseError {
    /// Index of the latest Cause in the error stack which indicates an
    /// unexpected end of file.
    eof: Option<u16>,
    /// The error stack.
    stack: Vec<Cause>,
    /// The optional source of the error.
    source: Option<Box<dyn StdError + 'static>>,
}

impl ParseError {
    /// Creates a new `ParseError` with the given `span`, `code` and `message`.
    ///
    /// The error stack is initialized with a single [`Cause`].
    #[inline]
    pub fn new<S, M>(span: S, code: Code, message: M) -> Self
    where
        S: Into<Span>,
        M: fmt::Display,
    {
        Self {
            eof: if code == ERR_EOF { Some(0) } else { None },
            stack: vec![Cause::new(span, code, message)],
            source: None,
        }
    }

    /// Creates a new `ParseError` at the given `pos` which indicates an
    /// unexpected end of file.
    ///
    /// The error stack is initialized with a single [`Cause`].
    #[inline]
    pub fn eof(pos: impl Into<Position>) -> Self {
        Self::eof_with(pos.into(), UNEXPECTED_END_OF_FILE)
    }

    /// Creates a new `ParseError` at the given `pos` which indicates an
    /// unexpected end of file with the given `message`.
    ///
    /// The error stack is initialized with a single [`Cause`].
    #[inline]
    pub fn eof_with<M>(pos: impl Into<Position>, message: M) -> Self
    where
        M: fmt::Display,
    {
        Self::new(pos.into(), ERR_EOF, message)
    }

    /// Pushes a new [`Cause`] onto the error stack with the given `span`,
    /// `code` and `message`.
    pub fn push<S, M>(&mut self, span: S, code: Code, message: M)
    where
        S: Into<Span>,
        M: fmt::Display,
    {
        self.stack.push(Cause::new(span, code, message));
        if code == ERR_EOF && self.eof.is_none() {
            self.eof = Some((self.stack.len() - 1).try_into().unwrap());
        }
    }

    /// Pushes a new [`Cause`] onto the error stack at the given `pos` which
    /// indicates an unexpected end of file.
    #[inline]
    pub fn push_eof(&mut self, pos: impl Into<Position>) {
        self.push(pos.into(), ERR_EOF, UNEXPECTED_END_OF_FILE);
    }

    /// Pushes a new [`Cause`] onto the error stack at the given `pos` which
    /// indicates an unexpected end of file with the given `message`.
    #[inline]
    pub fn push_eof_with<M>(&mut self, pos: impl Into<Position>, message: M)
    where
        M: fmt::Display,
    {
        self.push(pos.into(), ERR_EOF, message);
    }

    /// If the error stack contains a [`Cause`] which indicates an unexpected
    /// end of file, the latest [`Cause`] with the code [`ERR_EOF`] is returned.
    /// Otherwise the top most [`Cause`] is returned.
    #[inline]
    pub fn cause(&self) -> &Cause {
        if let Some(cause) = self.eof.as_ref().map(|i| &self.stack[*i as usize]) {
            cause
        } else {
            self.stack.last().expect("empty error stack")
        }
    }

    /// Returns an iterator over the [`Cause`]s in the error stack in reverse
    /// order. From latest to oldest.
    #[inline]
    pub fn causes(&self) -> impl Iterator<Item = &Cause> {
        self.stack.iter().rev()
    }

    /// Returns the [`Span`] of the latest [`Cause`] in the error stack. See
    /// [`cause()`](ParseError) how the latest [`Cause`] is determined.
    #[inline]
    pub fn span(&self) -> Span {
        self.cause().span()
    }

    /// Returns the error code of the latest [`Cause`] in the error stack. See
    /// [`cause()`](ParseError) how the latest [`Cause`] is determined.
    #[inline]
    pub fn code(&self) -> Code {
        self.cause().code()
    }

    /// Returns the error message of the latest [`Cause`] in the error stack.
    /// See [`cause()`](ParseError) how the latest [`Cause`] is determined.
    #[inline]
    pub fn message(&self) -> &str {
        self.cause().message()
    }

    /// Returns `true` if the error stack contains a [`Cause`] which indicates
    /// an unexpected end of file.
    #[inline]
    pub fn is_eof(&self) -> bool {
        self.eof.is_some()
    }

    /// Sets the source of the error to the given `source`. Returned by
    /// [`Error::source`](std::error).
    #[inline]
    pub fn set_source(&mut self, source: impl StdError + 'static) {
        self.source = Some(Box::new(source));
    }
}

impl StdError for ParseError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.source.as_deref()
    }
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.cause() == other.cause()
    }
}

impl fmt::Display for ParseError {
    /// Prints the latest [`Cause`] in the error stack. See
    /// [`cause()`](ParseError) how the latest [`Cause`] is determined. If
    /// printed in alternate mode, the position of the error is also printed.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}", self.cause())
        } else {
            write!(f, "{}", self.cause())
        }
    }
}

impl fmt::Debug for ParseError {
    /// Prints the whole error stack. If printed in alternate mode, the source
    /// of the error is also printed.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.stack.last().expect("empty error stack"))?;

            if self.stack.len() > 1 {
                write!(f, "\nCaused by:")?;
                for cause in self.causes().skip(1) {
                    write!(f, "\n       {:#?}", cause)?;
                }
            }
            if let Some(source) = self.source.as_ref() {
                write!(f, "\nSource:\n       {}", source)?;

                let mut cursor = source.source();
                while let Some(source) = cursor {
                    write!(f, "\n       {}", source)?;
                    cursor = source.source();
                }
            }
        } else {
            write!(f, "{:#?}", self.stack.last().expect("empty error stack"))?;

            if self.stack.len() > 1 {
                write!(f, "\nCaused by:")?;
                for cause in self.causes().skip(1) {
                    write!(f, "\n       {:?}", cause)?;
                }
            }
        }
        Ok(())
    }
}
