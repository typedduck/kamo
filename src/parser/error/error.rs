use std::{error::Error as StdError, fmt};

use crate::{
    parser::{Input, Span},
    Position,
};

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
/// The error stack can be chained with new [`Cause`]s. This is useful when
/// parsing nested structures. For example, when parsing a list of items, the
/// error stack can be chained with a [`Cause`] which indicates an unexpected
/// end of file and a [`Cause`] which indicates that the expected item is
/// missing.
///
/// An error can be set to be semantic. A semantic error should be used when the
/// error is not caused by the input but by the semantic of the input. A
/// semantic error implies that the input was read correctly until to the end.
/// For example, a number may be syntactically correct but it may be out of
/// range.
/// 
/// An error can be set to be a failure. A failure error prevents alternative
/// parsers from being tried. Optional parsers will return an error, too.
pub struct ParseError {
    /// Index of the latest Cause in the error stack which indicates an
    /// unexpected end of file.
    eof: Option<u16>,
    /// The error stack.
    stack: Vec<Cause>,
    /// The optional source of the error.
    source: Option<Box<dyn StdError + 'static>>,
    semantic: bool,
    failure: bool,
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
            semantic: false,
            failure: false,
        }
    }

    /// Creates a new `ParseError` at the position of the given `input` with the
    /// given `code` and `message`. If the `input` is at the end of file, the
    /// error is chained with a [`Cause`] which indicates an
    /// unexpected end of file and a [`Cause`] with the given `code` and
    /// `message`.
    pub fn new_at(input: Input, code: Code, message: impl fmt::Display) -> Self {
        if input.is_eof() {
            return Self::eof(input).and(input, code, message);
        }
        Self::new(input, code, message)
    }

    /// Creates a new `ParseError` at the given `pos` which indicates an
    /// unexpected end of file.
    ///
    /// The error stack is initialized with a single [`Cause`].
    #[inline]
    pub fn eof(pos: impl Into<Position>) -> Self {
        Self::new(pos.into(), ERR_EOF, UNEXPECTED_END_OF_FILE)
    }

    /// Chaining a new [`Cause`] onto the error stack with the given `span`,
    /// `code` and `message`.
    pub fn and<S, M>(self, span: S, code: Code, message: M) -> Self
    where
        S: Into<Span>,
        M: fmt::Display,
    {
        let mut this = self;
        this.push(span, code, message);
        this
    }

    /// Sets the error to be semantic and chaining it.
    ///
    /// A semantic error should be used when the error is not caused by the
    /// input but by the semantic of the input. A semantic error implies that
    /// the input was read correctly until to the end. For example, a number may
    /// syntactically correct but it may be out of range.
    pub fn and_semantic(self) -> Self {
        let mut this = self;
        this.set_semantic();
        this
    }

    /// Sets the error to be a failure and chaining it.
    /// 
    /// A failure error prevents alternative parsers from being tried. Optional
    /// parsers will return an error, too.
    pub fn and_failure(self) -> Self {
        let mut this = self;
        this.failure = true;
        this
    }

    /// Sets the source of the error and chaining it.
    pub fn and_source(self, source: impl StdError + 'static) -> Self {
        let mut this = self;
        this.set_source(source);
        this
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

    /// The top most [`Cause`] is returned.
    #[inline]
    pub fn cause(&self) -> &Cause {
        self.stack.last().expect("empty error stack")
    }

    /// Returns an iterator over the [`Cause`]s in the error stack in reverse
    /// order. From latest to oldest.
    #[inline]
    pub fn causes(&self) -> impl Iterator<Item = &Cause> {
        self.stack.iter().rev()
    }

    /// Returns the [`Span`] of the latest [`Cause`] in the error stack.
    #[inline]
    pub fn span(&self) -> Span {
        self.cause().span()
    }

    /// Returns the error code of the latest [`Cause`] in the error stack.
    #[inline]
    pub fn code(&self) -> Code {
        self.cause().code()
    }

    /// Returns the error message of the latest [`Cause`] in the error stack.
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

    /// Returns `true` if the error is semantic.
    ///
    /// A semantic error should be set when the error is not caused by the
    /// input but by the semantic of the input. A semantic error implies that
    /// the input was read correctly until to the end. For example, a number may
    /// syntactically correct but it may be out of range.
    #[inline]
    pub const fn is_semantic(&self) -> bool {
        self.semantic
    }

    /// Sets the error to be semantic.
    ///
    /// A semantic error should be set when the error is not caused by the
    /// input but by the semantic of the input. A semantic error implies that
    /// the input was read correctly until to the end. For example, a number may
    /// syntactically correct but it may be out of range.
    #[inline]
    pub fn set_semantic(&mut self) {
        self.semantic = true;
    }

    /// Returns `true` if the error is a failure.
    #[inline]
    pub const fn is_failure(&self) -> bool {
        self.failure
    }

    /// Sets the error to be a failure.
    ///
    /// A failure error prevents alternative parsers from being tried. Optional
    /// parsers will return an error, too.
    #[inline]
    pub fn set_failure(&mut self) {
        self.failure = true;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Input;

    #[test]
    fn parse_err_size() {
        assert_eq!(std::mem::size_of::<ParseError>(), 48);
    }

    #[test]
    fn parse_error_new() {
        let pos = Position::new(1, 1, 2);
        let error = ParseError::new(pos, ERR_EOF, "unexpected end of file");

        assert_eq!(error.cause().span(), pos.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(!error.is_semantic());
        assert!(error.source().is_none());
    }

    #[test]
    fn parse_error_new_at() {
        let input = Input::new("test");
        let error = ParseError::new_at(input, ERR_EOF, "unexpected end of file");

        assert_eq!(error.cause().span(), input.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(!error.is_semantic());
        assert!(error.source().is_none());
    }

    #[test]
    fn parse_error_eof() {
        let pos = Position::new(1, 1, 2);
        let error = ParseError::eof(pos);

        assert_eq!(error.cause().span(), pos.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(!error.is_semantic());
        assert!(error.source().is_none());
    }

    #[test]
    fn parse_error_and() {
        let pos = Position::new(1, 1, 2);
        let mut error = ParseError::new(pos, ERR_EOF, "unexpected end of file");

        error.push(pos, ERR_EOF, "unexpected end of file");

        assert_eq!(error.cause().span(), pos.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(!error.is_semantic());
        assert!(error.source().is_none());
    }

    #[test]
    fn parse_error_and_semantic() {
        let pos = Position::new(1, 1, 2);
        let error = ParseError::new(pos, ERR_EOF, "unexpected end of file").and_semantic();

        assert_eq!(error.cause().span(), pos.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(error.is_semantic());
        assert!(error.source().is_none());
    }

    #[test]
    fn parse_error_and_source() {
        let pos = Position::new(1, 1, 2);
        let error = ParseError::new(pos, ERR_EOF, "unexpected end of file")
            .and_source(std::io::Error::new(std::io::ErrorKind::Other, "test"));

        assert_eq!(error.cause().span(), pos.into());
        assert_eq!(error.cause().code(), ERR_EOF);
        assert_eq!(error.cause().message(), "unexpected end of file");
        assert!(error.is_eof());
        assert!(!error.is_semantic());
        assert!(error.source().is_some());
    }
}
