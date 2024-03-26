#[cfg(feature = "regex")]
use regex::Regex;

use crate::Position;

use super::TARGET;

/// A parser input. It is a wrapper around a string slice that keeps track of
/// the current character and position.
///
/// The input is advanced by calling `advance` or one of the `advance_*`
/// functions. The current character can be accessed with `current` and the
/// current position with `position`. It keeps track of newlines and the current
/// position is updated accordingly. Input reads are UTF-8 safe and the current
/// character is always a valid UTF-8 character. Lines and columns are counted
/// in UTF-8 characters and not in bytes. The offset is always in bytes and
/// counts the number of bytes read from the input.
///
/// The underlying string slice can be accessed with `as_str`. The length of the
/// remaining input in bytes can be accessed with `len` and if the input is
/// empty with `is_empty`. If the end of input is reached `current` returns
/// `None` and `is_eof` returns `true`.
///
/// When advancing the slice is updated and the current character and position
/// are updated. The current character is always at offset 0 in the slice.
/// Therefore the offset returned by `position` is always the number of bytes
/// read from the begining to the current character.
#[derive(Clone, Copy, Debug)]
pub struct Input<'a> {
    input: &'a str,
    current: (Option<char>, Position, bool),
}

impl<'a> Input<'a> {
    /// Creates a new input from a string slice.
    pub fn new(input: &'a str) -> Self {
        let current = if let Some((_, ch)) = Self::decode(input) {
            (Some(ch), Position::new(0, 1, 1), ch == '\n')
        } else {
            (None, Position::new(0, 1, 1), false)
        };

        log::debug!(target: TARGET, "Input: new input {:#}: {:?}", current.1, input);
        Self { input, current }
    }

    /// Creates a new input from an existing input. The new input is set to the
    /// end of input. It preserves the current position and newline state.
    pub fn eof_from(input: Input<'_>) -> Self {
        let current = (None, input.position(), input.newline());

        log::debug!(target: TARGET, "Input: eof input {:#}", current.1);
        Self { input: "", current }
    }

    /// Return the current character or `None` if end of input is reached.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.current(), Some('a'));
    /// assert_eq!(input.advance(), Some('b'));
    /// assert_eq!(input.current(), Some('b'));
    /// assert_eq!(input.advance(), Some('c'));
    /// assert_eq!(input.current(), Some('c'));
    /// assert_eq!(input.advance(), None);
    /// assert_eq!(input.current(), None);
    /// ```
    #[inline]
    pub const fn current(&self) -> Option<char> {
        self.current.0
    }

    /// Return the current position
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.position().offset(), 0);
    /// assert_eq!(input.advance(), Some('b'));
    /// assert_eq!(input.position().offset(), 1);
    /// assert_eq!(input.advance(), Some('c'));
    /// assert_eq!(input.position().offset(), 2);
    /// assert_eq!(input.advance(), None);
    /// assert_eq!(input.position().offset(), 3);
    /// assert_eq!(input.position().line(), 1);
    /// assert_eq!(input.position().column(), 4);
    /// ```
    #[inline]
    pub const fn position(&self) -> Position {
        self.current.1
    }

    /// Return if the current position marks a newline.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc\n123");
    ///
    /// assert!(!input.newline());
    /// assert_eq!(input.advance(), Some('b'));
    /// assert_eq!(input.advance(), Some('c'));
    /// assert_eq!(input.advance(), Some('\n'));
    /// assert!(input.newline());
    /// assert_eq!(input.advance(), Some('1'));
    /// ```
    #[inline]
    pub const fn newline(&self) -> bool {
        self.current.2
    }

    /// Reads and returns the next charater. After the function returns the
    /// charater is the current. Returns `None` if end of input is reached.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.advance(), Some('b'));
    /// assert_eq!(input.advance(), Some('c'));
    /// assert_eq!(input.advance(), None);
    /// assert_eq!(input.advance(), None);
    /// ```
    #[inline]
    pub fn advance(&mut self) -> Option<char> {
        if let Some(curr_ch) = self.current.0 {
            let offset = curr_ch.len_utf8();

            self.input = &self.input[offset..];

            if let Some((_, next_ch)) = Self::decode(self.input) {
                let (curr_ch, pos, newline) = &mut self.current;

                if *newline {
                    pos.column = 1;
                    pos.line += 1;
                } else if *curr_ch != Some('\r') {
                    pos.column += 1;
                }
                *curr_ch = Some(next_ch);
                *newline = next_ch == '\n';
                pos.offset += offset as u32;
                log::trace!(target: TARGET, "input: {:#}: {:?}", pos, next_ch);
                return Some(next_ch);
            } else {
                let (ch, pos, _) = &mut self.current;

                if *ch != Some('\r') {
                    pos.column += 1;
                }
                *ch = None;
                pos.offset += offset as u32;
            }
        }
        log::debug!(target: TARGET, "Input: {:#}: reached end of input", self.current.1);
        None
    }

    /// Advances to the character following the tag. Returns `true` if the tag
    /// matched, `false` otherwise. Returns `None` if end of input is reached
    /// and the tag did not match.
    ///
    /// On `None` and `Some(false)` the input is reset to the state before the
    /// function was called.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.advance_tag("abc"), Some(true));
    /// assert_eq!(input.current(), None);
    /// assert_eq!(input.position().offset(), 3);
    ///
    /// let mut input = Input::new("ab");
    ///
    /// assert_eq!(input.advance_tag("abc"), None);
    /// assert_eq!(input.position().offset(), 0);
    ///
    /// let mut input = Input::new("abdef");
    ///
    /// assert_eq!(input.advance_tag("abc"), Some(false));
    /// assert_eq!(input.position().offset(), 0);
    /// ```
    pub fn advance_tag(&mut self, tag: &str) -> Option<bool> {
        if self.input.starts_with(tag) {
            for _ in tag.chars() {
                self.advance();
            }
            return Some(true);
        }

        if self.input.len() < tag.len() {
            for (ch_in, ch_tag) in self.input.chars().zip(tag.chars()) {
                if ch_in != ch_tag {
                    return Some(false);
                }
            }
            return None;
        }

        Some(false)
    }

    #[cfg(feature = "regex")]
    /// Advances to the character following the regular expression. Returns
    /// `Some(matched)` if the regular expression matched, `None` otherwise.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// # use regex::Regex;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.advance_match(&Regex::new(r"^\w+").unwrap()), Some("abc"));
    /// assert_eq!(input.current(), None);
    /// assert_eq!(input.position().offset(), 3);
    ///
    /// let mut input = Input::new("ab");
    ///
    /// assert_eq!(input.advance_match(&Regex::new(r"^\d+").unwrap()), None);
    /// assert_eq!(input.position().offset(), 0);
    /// ```
    pub fn advance_match(&mut self, re: &Regex) -> Option<&'a str> {
        if let Some(mat) = re.find(self.input) {
            let matched = &self.input[..mat.end()];

            for _ in matched.chars() {
                self.advance();
            }
            Some(matched)
        } else {
            None
        }
    }

    /// Advances to the next character if the predicate returns `true` for the
    /// current character. Returns `true` if progress was made, `false`
    /// otherwise. Returns `None` if end of input is reached.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc");
    ///
    /// assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
    /// assert_eq!(input.current(), Some('b'));
    /// assert_eq!(input.advance_if(|c| c.is_ascii_digit()), Some(false));
    /// assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
    /// assert_eq!(input.current(), Some('c'));
    /// assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
    /// assert_eq!(input.current(), None);
    /// assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), None);
    /// ```
    pub fn advance_if<F>(&mut self, f: F) -> Option<bool>
    where
        F: FnOnce(char) -> bool,
    {
        if let Some(chr) = self.current() {
            if f(chr) {
                self.advance();
                return Some(true);
            }
            return Some(false);
        }
        None
    }

    /// Advances to the next charater as long as the predicate returns `true`
    /// for the current character. Returns the slice that matched. It may be
    /// zero length.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("abc\n123");
    ///
    /// assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "abc");
    /// assert_eq!(input.current(), Some('\n'));
    /// assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "");
    /// assert_eq!(input.advance(), Some('1'));
    /// assert_eq!(input.advance_while(|c| c.is_ascii_digit()), "123");
    /// assert_eq!(input.current(), None);
    /// assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "");
    /// ```
    pub fn advance_while<F>(&mut self, f: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let matched = self.input;
        let start = self.position().offset();

        while let Some(chr) = self.current() {
            if !f(chr) {
                break;
            }
            self.advance();
        }
        &matched[..(self.position().offset() - start)]
    }

    /// Advances to the next charater as long as the predicate returns `false`
    /// for the current character. Returns `None` if end of input is reached or
    /// `Some(matched)` if the predicate returned `true` for the current
    /// character. The matched slice may be zero length.
    ///
    /// On `None` the input is reset to the state before the function was
    /// called.
    ///
    /// ```rust
    /// # use kamo::parser::Input;
    /// let mut input = Input::new("123\nabc|");
    ///
    /// assert_eq!(input.advance_until(|c| c == '\n'), Some("123"));
    /// assert_eq!(input.current(), Some('\n'));
    /// assert_eq!(input.advance_until(|c| c == '\n'), Some(""));
    /// assert_eq!(input.advance(), Some('a'));
    /// assert_eq!(input.advance_until(|c| c == '|'), Some("abc"));
    /// assert_eq!(input.current(), Some('|'));
    /// assert_eq!(input.advance_until(|c| c.is_ascii_alphabetic()), None);
    /// ```
    pub fn advance_until<F>(&mut self, f: F) -> Option<&'a str>
    where
        F: Fn(char) -> bool,
    {
        let state = *self;
        let matched = self.input;
        let start = self.position().offset();

        while let Some(chr) = self.current() {
            if f(chr) {
                return Some(&matched[..(self.position().offset() - start)]);
            }
            self.advance();
        }
        *self = state;
        None
    }

    /// Return the length of the input in bytes.
    #[inline]
    pub const fn len(&self) -> usize {
        self.input.len()
    }

    /// Return if the input is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    /// Return if the input is at the end.
    #[inline]
    pub const fn is_eof(&self) -> bool {
        self.current.0.is_none()
    }

    /// Returns a reference to the underlying string.
    #[inline]
    pub const fn as_str(&self) -> &'a str {
        self.input
    }

    #[inline]
    fn decode(input: &str) -> Option<(usize, char)> {
        if let Some(ch) = input.chars().next() {
            let offset = ch.len_utf8();
            Some((offset, ch))
        } else {
            None
        }
    }
}

impl PartialEq for Input<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.input == other.input
    }
}

impl<'a> From<&'a str> for Input<'a> {
    fn from(input: &'a str) -> Self {
        Self::new(input)
    }
}

impl<'a> From<&'a String> for Input<'a> {
    fn from(input: &'a String) -> Self {
        Self::new(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_advance() {
        let mut input = Input::new("abc\n123");

        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert!(!input.newline());

        assert_eq!(input.advance(), Some('b'));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert!(!input.newline());

        assert_eq!(input.advance(), Some('c'));
        assert_eq!(input.current(), Some('c'));
        assert_eq!(input.position(), Position::new(2, 1, 3));
        assert!(!input.newline());

        assert_eq!(input.advance(), Some('\n'));
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance(), Some('1'));
        assert_eq!(input.current(), Some('1'));
        assert_eq!(input.position(), Position::new(4, 2, 1));
        assert!(!input.newline());

        assert_eq!(input.advance(), Some('2'));
        assert_eq!(input.current(), Some('2'));
        assert_eq!(input.position(), Position::new(5, 2, 2));
        assert!(!input.newline());

        assert_eq!(input.advance(), Some('3'));
        assert_eq!(input.current(), Some('3'));
        assert_eq!(input.position(), Position::new(6, 2, 3));
        assert!(!input.newline());

        assert_eq!(input.advance(), None);
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());

        assert_eq!(input.advance(), None);
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());
    }

    #[test]
    fn input_advance_if() {
        let mut input = Input::new("abc\n123");

        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
        assert_eq!(input.current(), Some('b'));
        assert_eq!(input.position(), Position::new(1, 1, 2));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
        assert_eq!(input.current(), Some('c'));
        assert_eq!(input.position(), Position::new(2, 1, 3));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(true));
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_alphabetic()), Some(false));
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance(), Some('1'));
        assert_eq!(input.current(), Some('1'));
        assert_eq!(input.position(), Position::new(4, 2, 1));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_digit()), Some(true));
        assert_eq!(input.current(), Some('2'));
        assert_eq!(input.position(), Position::new(5, 2, 2));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_digit()), Some(true));
        assert_eq!(input.current(), Some('3'));
        assert_eq!(input.position(), Position::new(6, 2, 3));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_digit()), Some(true));
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());

        assert_eq!(input.advance_if(|c| c.is_ascii_digit()), None);
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());
    }

    #[test]
    fn input_advance_while() {
        let mut input = Input::new("abc\n123");

        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert!(!input.newline());

        assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "abc");
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "");
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance(), Some('1'));
        assert_eq!(input.current(), Some('1'));
        assert_eq!(input.position(), Position::new(4, 2, 1));
        assert!(!input.newline());

        assert_eq!(input.advance_while(|c| c.is_ascii_digit()), "123");
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());

        assert_eq!(input.advance_while(|c| c.is_ascii_alphabetic()), "");
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());
    }

    #[cfg(feature = "regex")]
    #[test]
    fn input_advance_match() {
        let mut input = Input::new("abc\n123");

        assert_eq!(input.current(), Some('a'));
        assert_eq!(input.position(), Position::new(0, 1, 1));
        assert!(!input.newline());

        assert_eq!(
            input.advance_match(&Regex::new(r"^\w+").unwrap()),
            Some("abc")
        );
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance_match(&Regex::new(r"^\w+").unwrap()), None);
        assert_eq!(input.current(), Some('\n'));
        assert_eq!(input.position(), Position::new(3, 1, 4));
        assert!(input.newline());

        assert_eq!(input.advance(), Some('1'));
        assert_eq!(input.current(), Some('1'));
        assert_eq!(input.position(), Position::new(4, 2, 1));
        assert!(!input.newline());

        assert_eq!(
            input.advance_match(&Regex::new(r"^\d+").unwrap()),
            Some("123")
        );
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());

        assert_eq!(input.advance_match(&Regex::new(r"^\w+").unwrap()), None);
        assert_eq!(input.current(), None);
        assert_eq!(input.position(), Position::new(7, 2, 4));
        assert!(!input.newline());
    }
}
