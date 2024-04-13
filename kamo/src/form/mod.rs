//! # Predefined parsers for common data formats
//! 
//! This module contains predefined parsers for common data formats. Currently
//! it contains only a parser for S-expressions, but more parsers will be added
//! in the future.
pub mod sexpr;
pub use sexpr::Sexpr;