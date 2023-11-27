pub mod ascii;
pub mod unicode;

mod error;
pub use error::CharacterError;

mod matches;
pub use matches::*;

#[cfg(feature = "regex")]
mod match_re;
#[cfg(feature = "regex")]
pub use match_re::*;

mod take;
pub use take::*;
