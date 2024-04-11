//! Literal parsers.

mod character;
pub use character::*;

mod error;
#[allow(clippy::module_name_repetitions)]
pub use error::LiteralError;

mod escaped;
pub use escaped::*;

mod float;
pub use float::*;

mod integer;
pub use integer::*;

mod natural;
pub use natural::*;

mod radix;
pub use radix::*;
