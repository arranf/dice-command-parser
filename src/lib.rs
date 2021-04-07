#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::pedantic::module_name_repetitions)]
#![warn(missing_docs)]
#![warn(missing_doc_code_examples)]

//! This crate provides functionality for the basic parsing of dice roll commands e.g. `d100`, `d6 + 5`, `2d20 - 1`.
//! Given some input it will produce a `DiceRoll` struct which can be used to then calculate a result.

use nom::{branch, bytes, character, combinator, multi, sequence, Err};

/// Provides access to the `DiceRoll` struct.
pub mod dice_roll;
/// Provides access to the `ParserError` struct.
pub mod error;

/// Provides access to the `DiceRollWithOp` struct.
pub mod dice_roll_with_op;

mod parse;

use crate::dice_roll::{DiceRoll, Operation, RollType};
use crate::dice_roll_with_op::DiceRollWithOp;
use crate::error::ParserError;
use crate::parse::terminated_spare;

// + or - or end
fn modifier_terminating_symbol_or_nothing(i: &str) -> nom::IResult<&str, &str> {
    branch::alt((
        bytes::complete::tag("+"),
        bytes::complete::tag("-"),
        combinator::eof,
    ))(i)
}

// + or -
fn operation_parser(i: &str) -> nom::IResult<&str, Operation> {
    branch::alt((
        combinator::value(Operation::Addition, character::complete::char('+')),
        combinator::value(Operation::Subtraction, character::complete::char('-')),
    ))(i)
}

// advantage or disadvantage
// d20
fn roll_type_parser(i: &str) -> nom::IResult<&str, Option<RollType>> {
    combinator::opt(branch::alt((
        combinator::value(
            RollType::WithAdvantage,
            terminated_spare(
                bytes::complete::tag_no_case("a"),
                modifier_terminating_symbol_or_nothing,
            ),
        ),
        combinator::value(
            RollType::WithDisadvantage,
            terminated_spare(
                bytes::complete::tag_no_case("d"),
                modifier_terminating_symbol_or_nothing,
            ),
        ),
    )))(i)
}

// Returns RollType::Regular if no adv or disadv is parsed
fn dice_roll_type_parser(i: &str) -> nom::IResult<&str, RollType> {
    let result = roll_type_parser(i);
    match result {
        Ok((i, None)) => Ok((i, RollType::Regular)),
        Ok((i, Some(roll_type))) => Ok((i, roll_type)),
        Err(e) => Err(e),
    }
}

fn dice_parser(i: &str) -> nom::IResult<&str, (Option<&str>, &str, &str)> {
    sequence::tuple((
        combinator::opt(character::complete::digit1),
        bytes::complete::tag_no_case("d"),
        character::complete::digit1,
    ))(i)
}

fn roll_parser(i: &str) -> nom::IResult<&str, DiceRoll> {
    // Order matters
    branch::alt((
        dice_op_number_class_parser,
        dice_op_number_parser,
        dice_class_parser,
        dice_into_dice_roll_parser,
    ))(i)
}

// d20+1a
fn dice_op_number_class_parser(i: &str) -> nom::IResult<&str, DiceRoll> {
    let result = sequence::tuple((
        dice_parser,
        operation_parser,
        terminated_spare(
            character::complete::digit1,
            combinator::not(sequence::tuple((
                bytes::complete::tag_no_case("d"),
                character::complete::digit1,
            ))),
        ),
        dice_roll_type_parser,
    ))(i);
    match result {
        Ok((remaining, ((number_of_dice, _, dice_sides), operation, modifier, roll_type))) => Ok((
            remaining,
            dice_roll_from_parsed_items(
                number_of_dice,
                dice_sides,
                Some(operation),
                Some(modifier),
                roll_type,
            ),
        )),
        Err(e) => Err(e),
    }
}

// 2d20+1
fn dice_op_number_parser(i: &str) -> nom::IResult<&str, DiceRoll> {
    let result = sequence::tuple((
        dice_parser,
        operation_parser,
        terminated_spare(
            character::complete::digit1,
            combinator::not(bytes::complete::tag_no_case("d")),
        ),
    ))(i);
    match result {
        Ok((remaining, ((number_of_dice, _, dice_sides), operation, modifier))) => Ok((
            remaining,
            dice_roll_from_parsed_items(
                number_of_dice,
                dice_sides,
                Some(operation),
                Some(modifier),
                RollType::Regular,
            ),
        )),
        Err(e) => Err(e),
    }
}

// 2d8a
fn dice_class_parser(i: &str) -> nom::IResult<&str, DiceRoll> {
    let result = sequence::tuple((dice_parser, dice_roll_type_parser))(i);
    match result {
        Ok((remaining, ((number_of_dice, _, dice_sides), roll_type))) => Ok((
            remaining,
            dice_roll_from_parsed_items(number_of_dice, dice_sides, None, None, roll_type),
        )),
        Err(e) => Err(e),
    }
}

// 3d6
fn dice_into_dice_roll_parser(i: &str) -> nom::IResult<&str, DiceRoll> {
    let (remaining, (number_of_dice, _, dice_sides)) = dice_parser(i)?;
    Ok((
        remaining,
        dice_roll_from_parsed_items(number_of_dice, dice_sides, None, None, RollType::Regular),
    ))
}

fn statement_parser(i: &str) -> nom::IResult<&str, Vec<DiceRollWithOp>> {
    let (remaining, (operation, roll, later_rolls)) = sequence::tuple((
        operation_parser,
        roll_parser,
        branch::alt((
            statement_parser,
            combinator::value(Vec::new(), character::complete::space0), // TODO: Error?
        )),
    ))(i)?;
    let mut rolls = Vec::with_capacity(later_rolls.len() + 1);
    rolls.push(DiceRollWithOp::new(roll, operation));
    for roll in later_rolls {
        rolls.push(roll);
    }
    Ok((remaining, rolls))
}

fn single_statement_into_roll_with_op(i: &str) -> nom::IResult<&str, DiceRollWithOp> {
    let (remaining, (operator, roll)) =
        sequence::tuple((combinator::opt(operation_parser), roll_parser))(i)?;
    Ok((
        remaining,
        DiceRollWithOp::new(roll, operator.unwrap_or(Operation::Addition)),
    ))
}

fn parse_initial(i: &str) -> Result<(&str, DiceRollWithOp), ParserError> {
    match single_statement_into_roll_with_op(i) {
        Ok((remaining, roll)) => Ok((remaining, roll)),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            print!("{0}", e);
            // TODO: Actual error handling here
            Err(ParserError::Unknown)
        }
        Err(Err::Incomplete(_)) => Err(ParserError::Unknown),
    }
}

fn dice_roll_from_parsed_items(
    number_of_dice: Option<&str>,
    dice_sides: &str,
    modifier_operation: Option<Operation>,
    modifier_value: Option<&str>,
    roll_type: RollType,
) -> DiceRoll {
    let number_of_dice: u32 = number_of_dice.map_or(Ok(1), str::parse).unwrap();
    let dice_sides: u32 = dice_sides.parse().unwrap();
    if modifier_operation.is_some() && modifier_value.is_some() {
        let modifier_operation = modifier_operation.unwrap();
        let modifier_value: i32 = modifier_value.unwrap().parse().unwrap();
        match modifier_operation {
            Operation::Addition => {
                return DiceRoll::new(dice_sides, Some(modifier_value), number_of_dice, roll_type)
            }
            Operation::Subtraction => {
                let modifier = Some(-modifier_value);
                return DiceRoll::new(dice_sides, modifier, number_of_dice, roll_type);
            }
        }
    }
    DiceRoll::new(dice_sides, None, number_of_dice, roll_type)
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
pub fn parse_line(i: &str) -> Result<Vec<DiceRollWithOp>, ParserError> {
    let whitespaceless: String = i.replace(" ", "");
    let mut dice_rolls: Vec<DiceRollWithOp> = Vec::new();

    let (remaining, new_rolls) = parse_initial(&whitespaceless)?;

    dice_rolls.push(new_rolls);
    if remaining.is_empty() {
        return Ok(dice_rolls);
    }

    match multi::many1(statement_parser)(remaining) {
        Ok((remaining, new_rolls)) => {
            if !remaining.trim().is_empty() {
                return Err(ParserError::ParseError(format!(
                    "Expected remaining input to be empty, found: {0}",
                    remaining
                )));
            }
            dice_rolls.extend(new_rolls.concat());
        }
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            return Err(ParserError::ParseError(format!("{0}", e)));
        }
        Err(Err::Incomplete(_)) => {
            return Err(ParserError::Unknown);
        }
    }
    Ok(dice_rolls)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dice_parser() {
        assert_eq!(dice_parser("2d6 + 2"), Ok((" + 2", (Some("2"), "d", "6"))));
        assert_eq!(dice_parser("d8 + 3"), Ok((" + 3", (None, "d", "8"))));
        assert_eq!(dice_parser("d6 + 2"), Ok((" + 2", (None, "d", "6"))));
        assert_eq!(dice_parser("d6+2+"), Ok(("+2+", (None, "d", "6"))));
        assert_eq!(dice_parser("d1"), Ok(("", (None, "d", "1"))));
        assert!(dice_parser("6 + 2").is_err());
    }

    #[test]
    fn test_operation_parser() {
        assert_eq!(operation_parser("+2"), Ok(("2", Operation::Addition)));
        assert_eq!(operation_parser("-1"), Ok(("1", Operation::Subtraction)));

        assert_eq!(operation_parser("*1").is_err(), true);
    }

    #[test]
    fn test_roll_type_parser() {
        assert_eq!(
            dice_roll_type_parser("a"),
            Ok(("", RollType::WithAdvantage))
        );

        assert_eq!(
            dice_roll_type_parser("d"),
            Ok(("", RollType::WithDisadvantage))
        );

        assert_eq!(dice_roll_type_parser(""), Ok(("", RollType::Regular)));

        assert_eq!(dice_roll_type_parser("e"), Ok(("e", RollType::Regular)));
        assert_eq!(dice_roll_type_parser("+"), Ok(("+", RollType::Regular)));
        assert_eq!(
            dice_roll_type_parser("d+"),
            Ok(("+", RollType::WithDisadvantage))
        );
    }

    #[test]
    fn test_roll_parser() {
        assert_eq!(
            roll_parser("d6+2"),
            Ok(("", DiceRoll::new_regular_roll(6, Some(2), 1)))
        );

        assert_eq!(
            roll_parser("3d6-2"),
            Ok(("", DiceRoll::new_regular_roll(6, Some(-2), 3)))
        );

        assert_eq!(
            roll_parser("2d20-2a"),
            Ok(("", DiceRoll::new(20, Some(-2), 2, RollType::WithAdvantage)))
        );

        assert_eq!(
            roll_parser("2d20-2a+d4"),
            Ok((
                "+d4",
                DiceRoll::new(20, Some(-2), 2, RollType::WithAdvantage)
            ))
        );

        assert_eq!(
            roll_parser("2d20-2+d4"),
            Ok(("+d4", DiceRoll::new(20, Some(-2), 2, RollType::Regular)))
        );

        assert_eq!(
            roll_parser("2d6+d4"),
            Ok(("+d4", DiceRoll::new(6, None, 2, RollType::Regular)))
        );
        assert_eq!(
            roll_parser("2d6+2d4"),
            Ok(("+2d4", DiceRoll::new(6, None, 2, RollType::Regular)))
        );

        assert_eq!(
            roll_parser("3d4+1d"),
            Ok(("", DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage)))
        );

        assert_eq!(
            roll_parser("3d4+1d"),
            Ok(("", DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage)))
        );

        assert_eq!(
            roll_parser("d1d"),
            Ok(("", DiceRoll::new(1, None, 1, RollType::WithDisadvantage)))
        )
    }

    #[test]
    fn test_parse_initial() {
        assert_eq!(
            parse_initial("d6"),
            Ok((
                "",
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial("4d6+10a"),
            Ok((
                "",
                DiceRollWithOp::new(
                    DiceRoll::new(6, Some(10), 4, RollType::WithAdvantage),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial("d6+d4"),
            Ok((
                "+d4",
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial("4d6+10a-d4"),
            Ok((
                "-d4",
                DiceRollWithOp::new(
                    DiceRoll::new(6, Some(10), 4, RollType::WithAdvantage),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial("-d1d-4d4d"),
            Ok((
                "-4d4d",
                DiceRollWithOp::new(
                    DiceRoll::new(1, None, 1, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ))
        );
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(
            parse_line("d6"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(6, None, 1, RollType::Regular,),
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d20 +      5"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(20, Some(5), 1, RollType::Regular,),
                Operation::Addition,
            )])
        );
        assert_eq!(
            parse_line("2d10 - 5"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(10, Some(-5), 2, RollType::Regular,),
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("3d6"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(6, None, 3, RollType::Regular,),
                Operation::Addition,
            )])
        );
        assert_eq!(
            parse_line("5d20 +      5"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(20, Some(5), 5, RollType::Regular,),
                Operation::Addition
            )])
        );

        // TODO: Make this an error
        assert_eq!(
            parse_line("d0 - 5"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(0, Some(-5), 1, RollType::Regular,),
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d200a"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithAdvantage),
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d200 A"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithAdvantage),
                Operation::Addition
            )])
        );
        assert_eq!(
            parse_line("d200 d"),
            Ok(vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithDisadvantage),
                Operation::Addition
            )])
        );

        assert_eq!(
            parse_line("d100 + d4"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(100, None, 1, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Addition
                )
            ])
        );

        assert_eq!(
            parse_line("d100 - d6"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(100, None, 1, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular),
                    Operation::Subtraction
                )
            ])
        );

        assert!(parse_line("cd20").is_err());

        assert_eq!(
            parse_line("2d6 + 2d4"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 2, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 2, RollType::Regular),
                    Operation::Addition
                )
            ])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, Some(2), 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ])
        );

        assert_eq!(
            parse_line("d20 + d4 - 2d6"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 2, RollType::Regular,),
                    Operation::Subtraction
                ),
            ])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4 - 2d6"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, Some(2), 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 2, RollType::Regular,),
                    Operation::Subtraction
                ),
            ])
        );

        assert_eq!(
            parse_line("d20 - 6 + d4 - 2d6"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, Some(-6), 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 2, RollType::Regular,),
                    Operation::Subtraction
                ),
            ])
        );

        assert_eq!(
            parse_line("6d20 - 3d4+1d"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, None, 6, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ])
        );

        assert_eq!(
            parse_line("-d1d - 4d4d"),
            Ok(vec![
                DiceRollWithOp::new(
                    DiceRoll::new(1, None, 1, RollType::WithDisadvantage),
                    Operation::Subtraction
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 4, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ])
        );
    }

    #[test]
    fn test_statement_parser() {
        assert_eq!(
            statement_parser("+2d4"),
            Ok((
                "",
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 2, RollType::Regular),
                    Operation::Addition
                )]
            ))
        );

        assert_eq!(
            statement_parser("-3d12-4a"),
            Ok((
                "",
                vec![DiceRollWithOp::new(
                    DiceRoll::new(12, Some(-4), 3, RollType::WithAdvantage),
                    Operation::Subtraction
                )]
            ))
        );
    }
}
