#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::pedantic::module_name_repetitions)]
#![warn(missing_docs)]
#![warn(missing_doc_code_examples)]

//! This crate provides functionality for the basic parsing of dice roll commands e.g. `d100`, `d6 + 5`, `2d20 - 1`.
//! Given some input it will produce a `DiceRoll` struct which can be used to then calculate a result.

use nom::{
    branch, bytes, character, combinator,
    error::{ParseError, VerboseError},
    multi, sequence, Err,
};

/// Provides access to the `DiceRoll` struct.
pub mod dice_roll;
/// Provides access to the `ParserError` struct.
pub mod error;

use crate::dice_roll::{DiceRoll, Operation, RollType};
use crate::error::ParserError;

#[derive(Clone, Debug, PartialEq)]
enum Modifier {
    Plus,
    Minus,
}

// 3d6, 1d8, 4d20
fn multi_dice_parser(i: &str) -> nom::IResult<&str, (Option<&str>, &str)> {
    sequence::tuple((combinator::opt(character::complete::digit1), dice_parser))(i)
}

// d6 or d8
fn dice_parser(i: &str) -> nom::IResult<&str, &str> {
    sequence::preceded(character::complete::char('d'), character::complete::digit1)(i)
}

// + or -
fn modifier_sign_parser(i: &str) -> nom::IResult<&str, Modifier> {
    branch::alt((
        combinator::value(Modifier::Plus, character::complete::char('+')),
        combinator::value(Modifier::Minus, character::complete::char('-')),
    ))(i)
}

fn operation_sign_parser(i: &str) -> nom::IResult<&str, Operation> {
    branch::alt((
        combinator::value(Operation::Addition, character::complete::char('+')),
        combinator::value(Operation::Subtraction, character::complete::char('-')),
    ))(i)
}

// 1
fn modifier_value_parser(i: &str) -> nom::IResult<&str, &str> {
    character::complete::digit1(i)
}

// + 1
fn modifier_parser(i: &str) -> nom::IResult<&str, Option<(Modifier, &str, &str)>> {
    combinator::opt(sequence::tuple((
        modifier_sign_parser,
        character::complete::multispace0,
        modifier_value_parser,
    )))(i)
}

fn dice_roll_type_parser(i: &str) -> nom::IResult<&str, RollType> {
    let result = roll_type_parser(i);
    match result {
        Ok((i, None)) => Ok((i, RollType::Regular)),
        Ok((i, Some(roll_type))) => Ok((i, roll_type)),
        Err(e) => Err(e),
    }
}

// a, d, or i
fn roll_type_parser(i: &str) -> nom::IResult<&str, Option<RollType>> {
    combinator::opt(branch::alt((
        combinator::value(
            RollType::WithAdvantage,
            branch::alt((
                // Order matters here
                bytes::complete::tag_no_case("advantage"),
                bytes::complete::tag_no_case("adv"),
                bytes::complete::tag_no_case("a"),
            )),
        ),
        combinator::value(
            RollType::WithDisadvantage,
            branch::alt((
                // Order matters here
                bytes::complete::tag_no_case("disadvantage"),
                bytes::complete::tag_no_case("dadv"),
                bytes::complete::tag_no_case("d"),
            )),
        ),
    )))(i)
}

// 3d6 + 4
fn roll_parser_with_operation(i: &str) -> nom::IResult<&str, DiceRoll> {
    let parser_output = sequence::tuple((
        operation_sign_parser,
        character::complete::multispace0,
        multi_dice_parser,
        modifier_parser,
        character::complete::multispace0,
        dice_roll_type_parser,
    ))(i);
    match parser_output {
        Ok((
            remaining, // Remaining input
            (
                operation,
                _,
                (number_of_dice, dice_sides),
                modifier,
                _, // Space
                roll_type,
            ),
        )) => Ok((
            remaining,
            dice_roll_from_parsed_items(
                number_of_dice,
                dice_sides,
                modifier,
                Some(operation),
                roll_type,
            ),
        )),
        Err(e) => Err(e),
    }
}

// 3d6 + 4
fn roll_parser_without_operation(i: &str) -> nom::IResult<&str, DiceRoll> {
    let parser_output = sequence::tuple((
        multi_dice_parser,
        modifier_parser,
        character::complete::multispace0,
        dice_roll_type_parser,
    ))(i);
    match parser_output {
        Ok((
            remaining, // Remaining input
            (
                (number_of_dice, dice_sides),
                modifier,
                _, // Space
                roll_type,
            ),
        )) => Ok((
            remaining,
            dice_roll_from_parsed_items(number_of_dice, dice_sides, modifier, None, roll_type),
        )),
        Err(e) => Err(e),
    }
}

fn dice_roll_from_parsed_items(
    number_of_dice: Option<&str>,
    dice_sides: &str,
    modifier: Option<(Modifier, &str, &str)>,
    operation: Option<Operation>,
    roll_type: RollType,
) -> DiceRoll {
    let number_of_dice: u32 = number_of_dice.map_or(Ok(1), str::parse).unwrap();
    let operation = operation.unwrap_or_else(|| Operation::Addition);
    let dice_sides: u32 = dice_sides.parse().unwrap();
    match modifier {
        None => DiceRoll::new(dice_sides, None, number_of_dice, roll_type, operation),
        Some((sign, _, value)) => {
            let modifier_value: i32 = value.parse().unwrap();
            match sign {
                Modifier::Plus => DiceRoll::new(
                    dice_sides,
                    Some(modifier_value),
                    number_of_dice,
                    roll_type,
                    operation,
                ),
                Modifier::Minus => {
                    let modifier = Some(-modifier_value);
                    DiceRoll::new(dice_sides, modifier, number_of_dice, roll_type, operation)
                }
            }
        }
    }
}

/// Takes a string of dice input and returns a `Result<DiceRoll, ParserError>`
///
/// The string will be consumed in the process and must strictly match the format of the parser.
///
/// # Examples
///
/// Standard usage:
///
/// ```
/// use dice_command_parser::{parse_line, error::ParserError};
///
/// let input = "3d6 - 5";
/// let dice_roll = parse_line(&input)?;
/// # Ok::<(), ParserError>(())
/// ```
///
/// # Errors
/// This function can fail when one of the following occurs
/// 1. The line failed to parse.
/// 2. An error occurred parsing the numbers provided. This will likely be an overflow or underflow error.
///
/// For more information see `ParserError`.
pub fn parse_line(i: &str) -> Result<Vec<DiceRoll>, ParserError> {
    let mut rolls: Vec<DiceRoll> = Vec::new();
    match roll_parser_without_operation(i) {
        Ok((remaining_to_parse, roll)) => {
            rolls.push(roll);

            // Keep attempting to match
            match multi::many0(roll_parser_with_operation)(remaining_to_parse) {
                Ok((remaining, additional_rolls)) => {
                    if !remaining.trim().is_empty() {
                        return Err(ParserError::ParseError(format!(
                            "Expected remaining input to be empty, found: {0}",
                            remaining
                        )));
                    }
                    rolls.extend(additional_rolls);
                    return Ok(rolls);
                }
                Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                    Err(ParserError::ParseError(nom::error::convert_error(
                        remaining_to_parse,
                        VerboseError::from_error_kind(e.0, e.1),
                    )))
                }
                Err(Err::Incomplete(_)) => Err(ParserError::Unknown),
            }
        }
        Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(ParserError::ParseError(
            nom::error::convert_error(i, VerboseError::from_error_kind(e.0, e.1)),
        )),
        Err(Err::Incomplete(_)) => Err(ParserError::Unknown),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dice_parser() {
        assert_eq!(dice_parser("d6 + 2"), Ok((" + 2", "6")));
        assert_eq!(
            dice_parser("6 + 2"),
            Err(nom::Err::Error(("6 + 2", nom::error::ErrorKind::Char)))
        );
    }

    #[test]
    fn test_multi_dice_parser() {
        assert_eq!(multi_dice_parser("2d6 + 2"), Ok((" + 2", (Some("2"), "6"))));
        assert_eq!(multi_dice_parser("d8 + 3"), Ok((" + 3", (None, "8"))));
    }

    #[test]
    fn test_modifier_sign() {
        assert_eq!(modifier_sign_parser("+2"), Ok(("2", Modifier::Plus)));
        assert_eq!(modifier_sign_parser("-1"), Ok(("1", Modifier::Minus)));
        assert_eq!(
            modifier_sign_parser("*1"),
            Err(nom::Err::Error(("*1", nom::error::ErrorKind::Char)))
        );
    }

    #[test]
    fn test_modifier_value_parser() {
        assert_eq!(modifier_value_parser("2  "), Ok(("  ", "2")));
        assert_eq!(modifier_value_parser("300 "), Ok((" ", "300")));
        assert_eq!(
            modifier_value_parser("o22"),
            Err(nom::Err::Error(("o22", nom::error::ErrorKind::Digit)))
        );
    }

    #[test]
    fn test_roll_type_parser() {
        assert_eq!(
            dice_roll_type_parser("advantage  "),
            Ok(("  ", RollType::WithAdvantage))
        );
        assert_eq!(
            dice_roll_type_parser("adv"),
            Ok(("", RollType::WithAdvantage))
        );
        assert_eq!(
            dice_roll_type_parser("a"),
            Ok(("", RollType::WithAdvantage))
        );

        assert_eq!(
            dice_roll_type_parser("disadvantage"),
            Ok(("", RollType::WithDisadvantage))
        );
        assert_eq!(
            dice_roll_type_parser("dadv"),
            Ok(("", RollType::WithDisadvantage))
        );
        assert_eq!(
            dice_roll_type_parser("d"),
            Ok(("", RollType::WithDisadvantage))
        );

        assert_eq!(dice_roll_type_parser(""), Ok(("", RollType::Regular)));
    }

    #[test]
    fn test_roll_parser_without_operation() {
        assert_eq!(
            roll_parser_without_operation("d6 + 2"),
            Ok((
                "",
                DiceRoll::new_regular_roll(6, Some(2), 1, Operation::Addition)
            ))
        );

        assert_eq!(
            roll_parser_without_operation("3d6 - 2"),
            Ok((
                "",
                DiceRoll::new_regular_roll(6, Some(-2), 3, Operation::Addition)
            ))
        );

        assert_eq!(
            roll_parser_without_operation("2d20-2a"),
            Ok((
                "",
                DiceRoll::new(
                    20,
                    Some(-2),
                    2,
                    RollType::WithAdvantage,
                    Operation::Addition
                )
            ))
        );
    }

    #[test]
    fn test_roll_parser_with_operation() {
        assert_eq!(
            roll_parser_with_operation("+ d6+2"),
            Ok((
                "",
                DiceRoll::new_regular_roll(6, Some(2), 1, Operation::Addition)
            ))
        );

        assert_eq!(
            roll_parser_with_operation("- 3d6-2"),
            Ok((
                "",
                DiceRoll::new_regular_roll(6, Some(-2), 3, Operation::Subtraction)
            ))
        );

        assert_eq!(
            roll_parser_with_operation("- 2d20-2a"),
            Ok((
                "",
                DiceRoll::new(
                    20,
                    Some(-2),
                    2,
                    RollType::WithAdvantage,
                    Operation::Subtraction
                )
            ))
        );
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(
            parse_line("d6"),
            Ok(vec![DiceRoll::new(
                6,
                None,
                1,
                RollType::Regular,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d20 +      5"),
            Ok(vec![DiceRoll::new(
                20,
                Some(5),
                1,
                RollType::Regular,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("2d10 - 5"),
            Ok(vec![DiceRoll::new(
                10,
                Some(-5),
                2,
                RollType::Regular,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("3d6"),
            Ok(vec![DiceRoll::new(
                6,
                None,
                3,
                RollType::Regular,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("5d20 +      5"),
            Ok(vec![DiceRoll::new(
                20,
                Some(5),
                5,
                RollType::Regular,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d0 - 5"),
            Ok(vec![DiceRoll::new(
                0,
                Some(-5),
                1,
                RollType::Regular,
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d200a"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200adv"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200advantage"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d200 A"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200 adv"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200 advantage"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithAdvantage,
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d200 disadvantage"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithDisadvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200 d"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithDisadvantage,
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200 dadv"),
            Ok(vec![DiceRoll::new(
                200,
                None,
                1,
                RollType::WithDisadvantage,
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d100 + d4"),
            Ok(vec![
                DiceRoll::new(100, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 1, RollType::Regular, Operation::Addition)
            ])
        );

        assert_eq!(
            parse_line("d100 - d6"),
            Ok(vec![
                DiceRoll::new(100, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(6, None, 1, RollType::Regular, Operation::Subtraction)
            ])
        );

        assert!(parse_line("cd20").is_err());
    }

    #[test]
    fn test_multi_dice_type_parse_line() {
        assert_eq!(
            parse_line("2d6 + 2d4"),
            Ok(vec![
                DiceRoll::new(6, None, 2, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 2, RollType::Regular, Operation::Addition)
            ])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4"),
            Ok(vec![
                DiceRoll::new(20, Some(2), 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 1, RollType::Regular, Operation::Addition)
            ])
        );

        assert_eq!(
            parse_line("d20 + d4 - 2d6"),
            Ok(vec![
                DiceRoll::new(20, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(6, None, 2, RollType::Regular, Operation::Subtraction),
            ])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4 - 2d6"),
            Ok(vec![
                DiceRoll::new(20, Some(2), 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(6, None, 2, RollType::Regular, Operation::Subtraction),
            ])
        );

        assert_eq!(
            parse_line("d20 - 6 + d4 - 2d6"),
            Ok(vec![
                DiceRoll::new(20, Some(-6), 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(4, None, 1, RollType::Regular, Operation::Addition),
                DiceRoll::new(6, None, 2, RollType::Regular, Operation::Subtraction),
            ])
        );
    }
}
