use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ParserError {
    #[error("An error ocurred parsing the input.")]
    ParseError,
    #[error("An invalid number was entered")]
    InvalidNumberInput(#[from] ParseIntError),
    #[error("An unknown error occurred.")]
    Unknown,
}
