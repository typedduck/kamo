pub mod ascii;
pub mod unicode;

mod error;
#[allow(clippy::module_name_repetitions)]
pub use error::CharacterError;

mod matches;
pub use matches::*;

#[cfg(feature = "regex")]
mod match_re;
#[cfg(feature = "regex")]
pub use match_re::*;

mod take;
pub use take::*;
