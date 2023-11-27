use std::{fmt, hash};

use super::Input;

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
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// # use drake::parser::Position;
    /// let pos = Position::new(0, 1, 1);
    /// 
    /// assert_eq!(pos.offset(), 0);
    /// assert_eq!(pos.line(), 1);
    /// assert_eq!(pos.column(), 1);
    /// ```
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        let line = line.clamp(1, u16::MAX as usize);
        let column = column.clamp(1, u16::MAX as usize);

        Self {
            offset: offset as u32,
            line: line as u16,
            column: column as u16,
        }
    }

    /// Return the offset in bytes relative to the input.
    #[inline]
    pub const fn offset(&self) -> usize {
        self.offset as usize
    }

    /// Retrun the line number starting with 1.
    #[inline]
    pub const fn line(&self) -> usize {
        self.line as usize
    }

    /// Return the character (utf-8) column starting with 1.
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

impl From<Input<'_>> for Position {
    #[inline]
    fn from(input: Input<'_>) -> Self {
        input.position()
    }
}

impl From<&Input<'_>> for Position {
    #[inline]
    fn from(input: &Input<'_>) -> Self {
        input.position()
    }
}

impl From<&mut Input<'_>> for Position {
    #[inline]
    fn from(input: &mut Input<'_>) -> Self {
        input.position()
    }
}

impl From<&&mut Input<'_>> for Position {
    #[inline]
    fn from(input: &&mut Input<'_>) -> Self {
        input.position()
    }
}
