use std::fmt;

use crate::Position;

use super::Input;

/// A span is a range of positions in the input.
///
/// A span is composed of a start and an end position. The start position is
/// included in the span but the end position is not. The end position is the
/// position of the first character after the fragment.
///
/// If the span is empty, the start and end positions are the same. The span
/// contains no character.
#[derive(Copy, Clone, Default, PartialEq, Eq, Hash)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    /// Creates a new span from the start and end positions. The start position
    /// must be less than or equal to the end position.
    ///
    /// # Panics
    ///
    /// This function panics if the start position is greater than the end
    /// position.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use kamo::{Position, parser::Span};
    /// let start = Position::new(0, 1, 1);
    /// let end = Position::new(3, 1, 4);
    /// let span = Span::new(start, end);
    ///
    /// assert_eq!(span.start(), start);
    /// assert_eq!(span.end(), end);
    /// ```
    #[inline]
    pub fn new(start: Position, end: Position) -> Self {
        assert!(start <= end, "start > end");
        Self { start, end }
    }

    /// Creates a new span at the given position. The start and end positions
    /// are the same.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use kamo::{Position, parser::Span};
    /// let pos = Position::new(0, 1, 1);
    /// let span = Span::new_at(pos);
    ///
    /// assert_eq!(span.start(), pos);
    /// assert_eq!(span.end(), pos);
    /// ```
    #[inline]
    pub const fn new_at(pos: Position) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    /// Returns the start position of the span.
    #[inline]
    pub const fn start(&self) -> Position {
        self.start
    }

    /// Returns the end position of the span.
    #[inline]
    pub const fn end(&self) -> Position {
        self.end
    }

    /// Returns the byte offset of the start of the fragment relatively to the
    /// input. It starts at 0.
    #[inline]
    pub const fn offset(&self) -> usize {
        self.start.offset()
    }

    /// Returns the line number on which the span starts. It starts at 1.
    #[inline]
    pub const fn line(&self) -> usize {
        self.start.line()
    }

    /// Returns the charcater (utf-8 encoded) column the span start. It starts
    /// at 1.
    #[inline]
    pub const fn column(&self) -> usize {
        self.start.column()
    }

    /// Returns the first line that contains the fragment. The `source` must be
    /// the original string the span was created from otherwise an arbitrarily
    /// string will be returned.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use kamo::{Position, parser::Span};
    /// let pos = Position::new(0, 1, 1);
    /// let span = Span::new_at(pos);
    ///
    /// assert_eq!(span.line_str("abc"), "abc");
    ///
    /// let pos = Position::new(5, 2, 2);
    /// let span = Span::new_at(pos);
    ///
    /// assert_eq!(span.line_str("abc\ndef"), "def");
    /// ```
    pub fn line_str<'a>(&self, source: &'a str) -> &'a str {
        let column0 = self.before(source).0 - 1;
        let line = source[self.start.offset() - column0..].as_bytes();

        unsafe {
            std::str::from_utf8_unchecked(match memchr::memchr(b'\n', &line[column0..]) {
                None => line,
                Some(pos) => &line[..column0 + pos],
            })
        }
    }

    /// Returns the spanned fragement in the given source. The `source` must be
    /// the original string the span was created from otherwise an arbitrarily
    /// string will be returned.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use kamo::{Position, parser::Span};
    /// let start = Position::new(0, 1, 1);
    /// let end = Position::new(2, 1, 3);
    /// let span = Span::new(start, end);
    ///
    /// assert_eq!(span.fragment("abc"), "ab");
    /// ```
    #[inline]
    pub fn fragment<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.offset()..self.end.offset()]
    }

    /// Returns the bytes before the span in the given source. The `source` must
    /// be the original string the span was created from otherwise an
    /// arbitrarily string will be returned.
    fn before<'a>(&self, source: &'a str) -> (usize, &'a [u8]) {
        let before_self = source[..self.offset()].as_bytes();
        let offset = match memchr::memrchr(b'\n', before_self) {
            None => self.start.offset() + 1,
            Some(pos) => self.start.offset() - pos,
        };

        (offset, &before_self[self.start.offset() - (offset - 1)..])
    }

    /// Return the length of the span in bytes.
    #[inline]
    pub const fn len(&self) -> usize {
        self.end.offset() - self.start.offset()
    }

    /// Return if the span is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "{}:{}", self.start.line(), self.start.column(),)
        } else {
            write!(
                f,
                "{}:{}..{}:{}",
                self.start.line(),
                self.start.column(),
                self.end.line(),
                self.end.column(),
            )
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(
                f,
                "[{}] {}:{}",
                self.start.offset(),
                self.start.line(),
                self.start.column(),
            )
        } else {
            write!(
                f,
                "[{}..{}] {}:{}..{}:{}",
                self.start.offset(),
                self.end.offset(),
                self.start.line(),
                self.start.column(),
                self.end.line(),
                self.end.column(),
            )
        }
    }
}

impl Ord for Span {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Span {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Position> for Span {
    #[inline]
    fn from(pos: Position) -> Self {
        Self::new_at(pos)
    }
}

impl From<Input<'_>> for Span {
    #[inline]
    fn from(input: Input<'_>) -> Self {
        Self::new_at(input.position())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create() {
        let start = Position::new(0, 1, 1);
        let end = Position::new(2, 1, 3);
        let span = Span::new(start, end);

        assert_eq!(span.start(), start);
        assert_eq!(span.end(), end);

        let pos = Position::new(0, 1, 1);
        let span = Span::new_at(pos);

        assert_eq!(span.start(), pos);
        assert_eq!(span.end(), pos);
    }

    #[test]
    fn line_str() {
        let pos = Position::new(0, 1, 1);
        let span = Span::new_at(pos);

        assert_eq!(span.line_str("abc"), "abc");

        let pos = Position::new(3, 1, 4);
        let span = Span::new_at(pos);

        assert_eq!(span.line_str("abc"), "abc");

        let pos = Position::new(5, 2, 2);
        let span = Span::new_at(pos);

        assert_eq!(span.line_str("abc\ndef"), "def");
    }

    #[test]
    fn fragment() {
        let start = Position::new(0, 1, 1);
        let end = Position::new(2, 1, 3);
        let span = Span::new(start, end);

        assert_eq!(span.fragment("abc"), "ab");

        let start = Position::new(0, 1, 1);
        let end = Position::new(3, 1, 4);
        let span = Span::new(start, end);

        assert_eq!(span.fragment("abc"), "abc");

        let start = Position::new(0, 1, 1);
        let end = Position::new(5, 2, 2);
        let span = Span::new(start, end);

        assert_eq!(span.fragment("abc\ndef"), "abc\nd");
    }

    #[test]
    fn length() {
        let pos = Position::new(0, 1, 1);
        let span = Span::new_at(pos);

        assert_eq!(span.len(), 0);

        let start = Position::new(0, 1, 1);
        let end = Position::new(2, 1, 3);
        let span = Span::new(start, end);

        assert_eq!(span.len(), 2);

        let start = Position::new(0, 1, 1);
        let end = Position::new(3, 1, 4);
        let span = Span::new(start, end);

        assert_eq!(span.len(), 3);

        let start = Position::new(0, 1, 1);
        let end = Position::new(5, 2, 2);
        let span = Span::new(start, end);

        assert_eq!(span.len(), 5);
    }
}
