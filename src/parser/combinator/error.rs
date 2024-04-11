use std::fmt;

/// Definition of the error messages of the combinator parsers.
#[allow(clippy::module_name_repetitions)]
pub enum CombinatorError {
    /// Indicates that the end of input was expected.
    Eof,
    /// Indicates that the input could not be matched to the required pattern.
    Recognize,
    /// Indicates that the input could not be verified.
    Verify,
}

impl fmt::Display for CombinatorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof => write!(f, "expecting end of input"),
            Self::Recognize => write!(f, "could not match input to required pattern"),
            Self::Verify => write!(f, "could not verify input"),
        }
    }
}
