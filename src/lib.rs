#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::pedantic::module_name_repetitions)]
#![warn(missing_docs)]
#![warn(missing_doc_code_examples)]

//! This crate provides functionality for the basic parsing of dice roll commands e.g. `d100`, `d6 + 5`, `2d20 - 1`.
//! Given some input it will produce a `DiceRollWithOp` struct which can be used to then calculate a result.

use nom::{branch, bytes, character, combinator, sequence, Err};

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
fn parse_end_of_input_or_modifier(i: &str) -> nom::IResult<&str, &str> {
    branch::alt((
        bytes::complete::tag("+"),
        bytes::complete::tag("-"),
        bytes::complete::tag(","),
        combinator::eof,
    ))(i)
}

// + or -
fn parse_operator_as_value(i: &str) -> nom::IResult<&str, Operation> {
    branch::alt((
        combinator::value(Operation::Addition, character::complete::char('+')),
        combinator::value(Operation::Subtraction, character::complete::char('-')),
    ))(i)
}

// Returns RollType::Regular if no adv or disadv is parsed
fn parse_roll_type(i: &str) -> nom::IResult<&str, RollType> {
    let result = combinator::opt(branch::alt((
        combinator::value(
            RollType::WithAdvantage,
            // Only parse advantage if preceding a operating separating dice strings or at the end of input
            terminated_spare(
                bytes::complete::tag_no_case("a"),
                parse_end_of_input_or_modifier,
            ),
        ),
        combinator::value(
            RollType::WithDisadvantage,
            // Only parse advantage if preceding a operating separating dice strings or at the end of input
            terminated_spare(
                bytes::complete::tag_no_case("d"),
                parse_end_of_input_or_modifier,
            ),
        ),
    )))(i);
    match result {
        Ok((i, None)) => Ok((i, RollType::Regular)),
        Ok((i, Some(roll_type))) => Ok((i, roll_type)),
        Err(e) => Err(e),
    }
}

// Matches: 3d6
fn parse_dice_parts(i: &str) -> nom::IResult<&str, (Option<&str>, &str, &str)> {
    sequence::tuple((
        combinator::opt(character::complete::digit1),
        bytes::complete::tag_no_case("d"),
        character::complete::digit1,
    ))(i)
}

// Matches: 3d6-1a, 3d6-1, 3d6
fn parse_roll_as_value(i: &str) -> nom::IResult<&str, DiceRoll> {
    // Order matters
    branch::alt((parse_dice_with_operator, parse_dice_without_operator))(i)
}

// Matches: 3d6-1a or 3d6-1
fn parse_dice_with_operator(i: &str) -> nom::IResult<&str, DiceRoll> {
    let result = sequence::tuple((
        parse_dice_parts,
        parse_operator_as_value,
        terminated_spare(
            character::complete::digit1,
            combinator::not(sequence::tuple((
                bytes::complete::tag_no_case("d"),
                character::complete::digit1,
            ))),
        ),
        parse_roll_type,
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

// Matches: 2d8a or 2d8
fn parse_dice_without_operator(i: &str) -> nom::IResult<&str, DiceRoll> {
    let result = sequence::tuple((parse_dice_parts, parse_roll_type))(i);
    match result {
        Ok((remaining, ((number_of_dice, _, dice_sides), roll_type))) => Ok((
            remaining,
            dice_roll_from_parsed_items(number_of_dice, dice_sides, None, None, roll_type),
        )),
        Err(e) => Err(e),
    }
}

fn parse_statement_with_leading_op(i: &str) -> nom::IResult<&str, Vec<DiceRollWithOp>> {
    let (remaining, (operation, roll, later_rolls)) = sequence::tuple((
        parse_operator_as_value,
        parse_roll_as_value,
        branch::alt((
            parse_statement_with_leading_op, // Recursive call
            combinator::value(Vec::new(), character::complete::space0), // Base case
        )),
    ))(i)?;
    let mut rolls = Vec::with_capacity(later_rolls.len() + 1);
    rolls.push(DiceRollWithOp::new(roll, operation));
    for roll in later_rolls {
        rolls.push(roll);
    }
    Ok((remaining, rolls))
}

// TODO: Refactor
fn parse_initial_statement(i: &str) -> nom::IResult<&str, DiceRollWithOp> {
    let (remaining, (operator, roll)) = sequence::tuple((
        combinator::opt(parse_operator_as_value),
        parse_roll_as_value,
    ))(i)?;
    Ok((
        remaining,
        DiceRollWithOp::new(roll, operator.unwrap_or(Operation::Addition)),
    ))
}

fn parse_statements(i: &str) -> nom::IResult<&str, Vec<DiceRollWithOp>> {
    // TODO: Make `later_rolls` an Option<>
    let (remaining, (operation, roll, later_rolls)) = sequence::tuple((
        parse_operator_as_value,
        parse_roll_as_value,
        branch::alt((
            parse_statement_with_leading_op, // Recursive call
            // TODO: Use combinator that consumes the 'empty string' i.e. doesn't consume input instead of value
            combinator::value(Vec::new(), character::complete::space0), // Base case
        )),
    ))(i)?;
    let mut rolls = Vec::with_capacity(later_rolls.len() + 1);
    rolls.push(DiceRollWithOp::new(roll, operation));
    for roll in later_rolls {
        rolls.push(roll);
    }
    Ok((remaining, rolls))
}

fn parse_group(i: &str) -> nom::IResult<&str, Vec<DiceRollWithOp>> {
    let (remaining, (initial_roll, additional_rolls)) = sequence::tuple((
        parse_initial_statement,
        // TODO: Try combinator::opt()
        branch::alt((
            parse_statements,
            // TODO: Use combinator that consumes the 'empty string' i.e. doesn't consume input instead of value
            combinator::value(Vec::new(), character::complete::space0),
        )),
    ))(i)?;

    let mut rolls = Vec::with_capacity(additional_rolls.len() + 1);
    rolls.push(initial_roll);
    for roll in additional_rolls {
        rolls.push(roll);
    }
    Ok((remaining, rolls))
}

fn parse_groups(i: &str) -> nom::IResult<&str, Vec<Vec<DiceRollWithOp>>> {
    let (remaining, (group_rolls, other_groups)) = sequence::tuple((
        parse_group,
        combinator::opt(sequence::tuple((
            character::complete::char(','),
            parse_groups,
        ))),
    ))(i)?;

    let other_groups_size = match &other_groups {
        Some((_, rolls)) => rolls.len(),
        None => 0,
    };

    let mut rolls: Vec<Vec<DiceRollWithOp>> = Vec::with_capacity(other_groups_size + 1);
    rolls.push(group_rolls);
    if other_groups.is_some() {
        let (_, other_groups_rolls) = other_groups.unwrap();
        rolls.extend(other_groups_rolls);
    }
    Ok((remaining, rolls))
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
pub fn parse_line(i: &str) -> Result<Vec<Vec<DiceRollWithOp>>, ParserError> {
    let whitespaceless: String = i.replace(" ", "");

    match parse_groups(&whitespaceless) {
        Ok((remaining, dice_rolls)) => {
            if !remaining.trim().is_empty() {
                return Err(ParserError::ParseError(format!(
                    "Expected remaining input to be empty, found: {0}",
                    remaining
                )));
            }
            return Ok(dice_rolls);
        }
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            return Err(ParserError::ParseError(format!("{0}", e)));
        }
        Err(Err::Incomplete(_)) => {
            return Err(ParserError::Unknown);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dice_parts() {
        assert_eq!(
            parse_dice_parts("2d6 + 2"),
            Ok((" + 2", (Some("2"), "d", "6")))
        );
        assert_eq!(parse_dice_parts("d8 + 3"), Ok((" + 3", (None, "d", "8"))));
        assert_eq!(parse_dice_parts("d6 + 2"), Ok((" + 2", (None, "d", "6"))));
        assert_eq!(parse_dice_parts("d6+2+"), Ok(("+2+", (None, "d", "6"))));
        assert_eq!(parse_dice_parts("d1"), Ok(("", (None, "d", "1"))));
        assert!(parse_dice_parts("6 + 2").is_err());
    }

    #[test]
    fn test_parse_operator_as_value() {
        assert_eq!(
            parse_operator_as_value("+2"),
            Ok(("2", Operation::Addition))
        );
        assert_eq!(
            parse_operator_as_value("-1"),
            Ok(("1", Operation::Subtraction))
        );

        assert_eq!(parse_operator_as_value("*1").is_err(), true);
    }

    #[test]
    fn test_roll_type_parser() {
        assert_eq!(parse_roll_type("a"), Ok(("", RollType::WithAdvantage)));

        assert_eq!(parse_roll_type("d"), Ok(("", RollType::WithDisadvantage)));

        assert_eq!(parse_roll_type(""), Ok(("", RollType::Regular)));

        assert_eq!(parse_roll_type("e"), Ok(("e", RollType::Regular)));
        assert_eq!(parse_roll_type("+"), Ok(("+", RollType::Regular)));
        assert_eq!(parse_roll_type("d+"), Ok(("+", RollType::WithDisadvantage)));
    }

    #[test]
    fn test_parse_roll_as_value() {
        assert_eq!(
            parse_roll_as_value("d6+2"),
            Ok(("", DiceRoll::new_regular_roll(6, Some(2), 1)))
        );

        assert_eq!(
            parse_roll_as_value("3d6-2"),
            Ok(("", DiceRoll::new_regular_roll(6, Some(-2), 3)))
        );

        assert_eq!(
            parse_roll_as_value("2d20-2a"),
            Ok(("", DiceRoll::new(20, Some(-2), 2, RollType::WithAdvantage)))
        );

        assert_eq!(
            parse_roll_as_value("2d20-2a+d4"),
            Ok((
                "+d4",
                DiceRoll::new(20, Some(-2), 2, RollType::WithAdvantage)
            ))
        );

        assert_eq!(
            parse_roll_as_value("2d20-2+d4"),
            Ok(("+d4", DiceRoll::new(20, Some(-2), 2, RollType::Regular)))
        );

        assert_eq!(
            parse_roll_as_value("2d6+d4"),
            Ok(("+d4", DiceRoll::new(6, None, 2, RollType::Regular)))
        );
        assert_eq!(
            parse_roll_as_value("2d6+2d4"),
            Ok(("+2d4", DiceRoll::new(6, None, 2, RollType::Regular)))
        );

        assert_eq!(
            parse_roll_as_value("3d4+1d"),
            Ok(("", DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage)))
        );

        assert_eq!(
            parse_roll_as_value("3d4+1d"),
            Ok(("", DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage)))
        );

        assert_eq!(
            parse_roll_as_value("d1d"),
            Ok(("", DiceRoll::new(1, None, 1, RollType::WithDisadvantage)))
        );
        assert_eq!(
            parse_roll_as_value("d20d,d4"),
            Ok((
                ",d4",
                DiceRoll::new(20, None, 1, RollType::WithDisadvantage)
            ))
        );
    }

    #[test]
    fn test_parse_initial() {
        assert_eq!(
            parse_initial_statement("d6"),
            Ok((
                "",
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial_statement("4d6+10a"),
            Ok((
                "",
                DiceRollWithOp::new(
                    DiceRoll::new(6, Some(10), 4, RollType::WithAdvantage),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial_statement("d6+d4"),
            Ok((
                "+d4",
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial_statement("4d6+10a-d4"),
            Ok((
                "-d4",
                DiceRollWithOp::new(
                    DiceRoll::new(6, Some(10), 4, RollType::WithAdvantage),
                    Operation::Addition
                )
            ))
        );

        assert_eq!(
            parse_initial_statement("-d1d-4d4d"),
            Ok((
                "-4d4d",
                DiceRollWithOp::new(
                    DiceRoll::new(1, None, 1, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ))
        );

        assert_eq!(
            parse_initial_statement("d20d,d4"),
            Ok((
                ",d4",
                DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::WithDisadvantage),
                    Operation::Addition
                )
            ))
        );
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(
            parse_line("d6"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(6, None, 1, RollType::Regular,),
                Operation::Addition
            )]])
        );
        assert_eq!(
            parse_line("d20 +      5"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(20, Some(5), 1, RollType::Regular,),
                Operation::Addition,
            )]])
        );
        assert_eq!(
            parse_line("2d10 - 5"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(10, Some(-5), 2, RollType::Regular,),
                Operation::Addition
            )]])
        );
        assert_eq!(
            parse_line("3d6"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(6, None, 3, RollType::Regular,),
                Operation::Addition,
            )]])
        );
        assert_eq!(
            parse_line("5d20 +      5"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(20, Some(5), 5, RollType::Regular,),
                Operation::Addition
            )]])
        );

        // TODO: Make this an error
        assert_eq!(
            parse_line("d0 - 5"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(0, Some(-5), 1, RollType::Regular,),
                Operation::Addition
            )]])
        );

        assert_eq!(
            parse_line("d200a"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithAdvantage),
                Operation::Addition
            )]])
        );

        assert_eq!(
            parse_line("d200 A"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithAdvantage),
                Operation::Addition
            )]])
        );
        assert_eq!(
            parse_line("d200 d"),
            Ok(vec![vec![DiceRollWithOp::new(
                DiceRoll::new(200, None, 1, RollType::WithDisadvantage),
                Operation::Addition
            )]])
        );

        assert_eq!(
            parse_line("d100 + d4"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(100, None, 1, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Addition
                )
            ]])
        );

        assert_eq!(
            parse_line("d100 - d6"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(100, None, 1, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular),
                    Operation::Subtraction
                )
            ]])
        );

        assert!(parse_line("cd20").is_err());

        assert_eq!(
            parse_line("2d6 + 2d4"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(6, None, 2, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 2, RollType::Regular),
                    Operation::Addition
                )
            ]])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, Some(2), 1, RollType::Regular,),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular,),
                    Operation::Addition
                )
            ]])
        );

        assert_eq!(
            parse_line("d20 + d4 - 2d6"),
            Ok(vec![vec![
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
            ]])
        );

        assert_eq!(
            parse_line("d20 + 2 + d4 - 2d6"),
            Ok(vec![vec![
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
            ]])
        );

        assert_eq!(
            parse_line("d20 - 6 + d4 - 2d6"),
            Ok(vec![vec![
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
            ]])
        );

        assert_eq!(
            parse_line("6d20 - 3d4+1d"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(20, None, 6, RollType::Regular),
                    Operation::Addition
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, Some(1), 3, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ]])
        );

        assert_eq!(
            parse_line("-d1d - 4d4d"),
            Ok(vec![vec![
                DiceRollWithOp::new(
                    DiceRoll::new(1, None, 1, RollType::WithDisadvantage),
                    Operation::Subtraction
                ),
                DiceRollWithOp::new(
                    DiceRoll::new(4, None, 4, RollType::WithDisadvantage),
                    Operation::Subtraction
                )
            ]])
        );
        assert_eq!(
            parse_line("d20, d4"),
            Ok(vec![
                vec![DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Addition
                )]
            ])
        );
        assert_eq!(
            parse_line("d20, d4, d6, d100, 3d100"),
            Ok(vec![
                vec![DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(100, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(100, None, 3, RollType::Regular),
                    Operation::Addition
                )]
            ])
        );
        assert_eq!(
            parse_line("d20, -d4, -d6, -d100+2, -3d100-6d"),
            Ok(vec![
                vec![DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::Regular),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Subtraction
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(6, None, 1, RollType::Regular),
                    Operation::Subtraction
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(100, Some(2), 1, RollType::Regular),
                    Operation::Subtraction
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(100, Some(-6), 3, RollType::WithDisadvantage),
                    Operation::Subtraction
                )]
            ])
        );
        assert_eq!(
            parse_line("d20d, d4"),
            Ok(vec![
                vec![DiceRollWithOp::new(
                    DiceRoll::new(20, None, 1, RollType::WithDisadvantage),
                    Operation::Addition
                )],
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 1, RollType::Regular),
                    Operation::Addition
                )]
            ])
        );
    }

    #[test]
    fn test_statement_parser() {
        assert_eq!(
            parse_statement_with_leading_op("+2d4"),
            Ok((
                "",
                vec![DiceRollWithOp::new(
                    DiceRoll::new(4, None, 2, RollType::Regular),
                    Operation::Addition
                )]
            ))
        );

        assert_eq!(
            parse_statement_with_leading_op("-3d12-4a"),
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
