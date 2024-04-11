use std::fmt;

/// Definition of the error messages of the branching parsers.
#[allow(clippy::module_name_repetitions)]
pub enum BranchError {
    /// The parser was unable to match any of the alternative branches.
    NoMatch,
}

impl fmt::Display for BranchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoMatch => write!(f, "unable to match an alternative branch"),
        }
    }
}
