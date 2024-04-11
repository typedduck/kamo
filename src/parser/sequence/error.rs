use std::fmt;

/// Definition of the error messages of the sequence parsers.
#[allow(clippy::module_name_repetitions)]
pub enum SequenceError {
    /// Indicates that the tuple element at the given index was expected.
    Tuple(usize, usize),
    /// Indicates that the first element of a list was expected.
    ListStart,
    /// Indicates that a list with at least the given number of elements was expected.
    ListM(usize),
    /// Indicates that the next element of a list was expected.
    ListNext,
    /// Indicates that the expression is not terminated correctly.
    Terminated,
    /// Indicates that the prefix of the following expression was expected.
    Preceded,
    /// Indicates that the parser is in an infinite loop.
    Infinite,
    /// Indicates that at least one element of many was expected.
    Many1,
    /// Indicates that at least the given number of elements was expected.
    ManyM(usize),
    /// Indicates that the first element of a pair was expected.
    PairFirst,
    /// Indicates that the second element of a pair was expected.
    PairSecond,
}

impl fmt::Display for SequenceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Tuple(i, n) => write!(f, "expecting element {} of {} in the sequence", i + 1, n),
            Self::ListStart => write!(f, "expecting the first element of a list"),
            Self::ListM(m) => write!(f, "expecting a list with at least {m} elements"),
            Self::ListNext => write!(f, "expecting the next list element after the separator"),
            Self::Terminated => write!(f, "expression is not terminated correctly"),
            Self::Preceded => write!(f, "expecting the prefix of the following expression"),
            Self::Infinite => write!(f, "infinite loop, there is no progress"),
            Self::Many1 => write!(f, "expecting at least one element of many"),
            Self::ManyM(m) => write!(f, "expecting at least {m} elements"),
            Self::PairFirst => write!(f, "expecting the first element of a pair"),
            Self::PairSecond => write!(f, "expecting the second element of a pair"),
        }
    }
}
