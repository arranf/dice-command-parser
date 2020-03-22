use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
/// Errors that can occur parsing the command input.
pub enum ParserError {
    #[error("An error ocurred parsing the input.")]
    /// Implies that the string was not in the correct format.
    ParseError,
    #[error("An invalid number was entered")]
    /// Occurs when the numbers provided in the input cannot be turned into `i32`. This is likely an overflow or underflow error.
    InvalidNumberInput(#[from] ParseIntError),
    /// Reserved for errors that do not fit into other categories.
    #[error("An unknown error occurred.")]
    Unknown,
}
