use std::{fmt, hash};

use crate::parser;

/// A position in the input. It is used to track the position of the parser in
/// the input.
///
/// The position is represented by an offset in bytes relative to the begining
/// of the input, a line number and a column number.
///
/// Lines and columns start with 1 and are clamped to `[1..=u16::MAX]` to avoid
/// overflow. The offset is not checked and it is assumed that the offset is
/// always valid and corresponds to the line and column numbers. The offset is
/// used to compare positions and may not exceed `u32::MAX`.
#[derive(Clone, Copy, Debug, Eq)]
pub struct Position {
    pub(super) offset: u32,
    pub(super) line: u16,
    pub(super) column: u16,
}

impl Position {
    /// Creates a new position. The `line` and `column` numbers start with 1.
    /// The values are clamped to `[1..=u16::MAX]` to avoid overflow. There is
    /// no check for the `offset` value. It is assumed that the `offset` is
    /// always valid and corresponds to the `line` and `column` values. The
    /// `offset` is used to compare positions and may not exceed `u32::MAX`.
    /// The `offset` is cast to `u32` and clamped to `u32::MAX`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use kamo::Position;
    /// let pos = Position::new(0, 1, 1);
    ///
    /// assert_eq!(pos.offset(), 0);
    /// assert_eq!(pos.line(), 1);
    /// assert_eq!(pos.column(), 1);
    /// ```
    #[allow(clippy::cast_possible_truncation)]
    #[must_use]
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        let line = line.clamp(1, u16::MAX as usize);
        let column = column.clamp(1, u16::MAX as usize);
        let offset = u32::try_from(offset).map_or(u32::MAX, |offset| offset);

        // Cast is safe because the values are clamped to u16::MAX.
        Self {
            offset,
            line: line as u16,
            column: column as u16,
        }
    }

    /// Creates a new position by rebasing the current position to the given
    /// `base` position.
    #[must_use]
    pub const fn rebase(&self, base: Self) -> Self {
        let offset = base.offset + self.offset;
        let line = base.line + self.line - 1;
        let column = if self.line == 1 {
            base.column + self.column - 1
        } else {
            self.column
        };

        Self {
            offset,
            line,
            column,
        }
    }

    /// Return the offset in bytes relative to the input.
    #[must_use]
    #[inline]
    pub const fn offset(&self) -> usize {
        self.offset as usize
    }

    /// Retrun the line number starting with 1.
    #[must_use]
    #[inline]
    pub const fn line(&self) -> usize {
        self.line as usize
    }

    /// Return the character (utf-8) column starting with 1.
    #[must_use]
    #[inline]
    pub const fn column(&self) -> usize {
        self.column as usize
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 1,
            column: 1,
        }
    }
}

impl PartialEq for Position {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset
    }
}

impl Ord for Position {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PartialOrd for Position {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl hash::Hash for Position {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.offset.hash(state);
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}:{}:{}", self.offset, self.line, self.column)
        } else {
            write!(f, "{}:{}", self.line, self.column)
        }
    }
}

impl From<parser::Input<'_>> for Position {
    #[inline]
    fn from(input: parser::Input<'_>) -> Self {
        input.position()
    }
}

impl From<&parser::Input<'_>> for Position {
    #[inline]
    fn from(input: &parser::Input<'_>) -> Self {
        input.position()
    }
}

impl From<&mut parser::Input<'_>> for Position {
    #[inline]
    fn from(input: &mut parser::Input<'_>) -> Self {
        input.position()
    }
}

impl From<&&mut parser::Input<'_>> for Position {
    #[inline]
    fn from(input: &&mut parser::Input<'_>) -> Self {
        input.position()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_new() {
        let pos = Position::new(0, 1, 1);

        assert_eq!(pos.offset(), 0);
        assert_eq!(pos.line(), 1);
        assert_eq!(pos.column(), 1);
    }

    #[test]
    fn position_rebase() {
        let base = Position::new(0, 1, 1);
        let pos = Position::new(1, 1, 2);

        let pos = pos.rebase(base);

        assert_eq!(pos.offset(), 1);
        assert_eq!(pos.line(), 1);
        assert_eq!(pos.column(), 2);

        let base = Position::new(46, 3, 7);

        let pos = Position::new(3, 1, 4);
        let pos = pos.rebase(base);

        assert_eq!(pos.offset(), 49);
        assert_eq!(pos.line(), 3);
        assert_eq!(pos.column(), 10);

        let pos = Position::new(7, 2, 4);
        let pos = pos.rebase(base);

        assert_eq!(pos.offset(), 53);
        assert_eq!(pos.line(), 4);
        assert_eq!(pos.column(), 4);
    }

    #[test]
    fn position_display() {
        let pos = Position::new(0, 1, 1);

        assert_eq!(format!("{pos}"), "1:1");
        assert_eq!(format!("{pos:#}"), "0:1:1");
    }
}
