mod base;
#[allow(clippy::module_name_repetitions)]
pub use base::TypeChecker;

mod error;
pub use error::TypeCheckError;

type Result<T> = std::result::Result<T, TypeCheckError>;
