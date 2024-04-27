mod base;
#[allow(clippy::module_name_repetitions)]
pub use base::TypeChecker;

mod error;
pub use error::TypeCheckError;

mod params;
pub use params::Parameters;

type Result<T> = std::result::Result<T, TypeCheckError>;
