#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::pedantic::module_name_repetitions)]
#![warn(missing_docs)]
#![warn(missing_doc_code_examples)]

//! This crate provides functionality for the basic parsing of dice roll commands e.g. `d100`, `d6 + 5`, `2d20 - 1`.
//! Given some input it will produce a `DiceRoll` struct which can be used to then calculate a result.

use nom::{branch, bytes, character, combinator, sequence};

/// Provides access to the `DiceRoll` struct.
pub mod dice_roll;
/// Provides access to the `ParserError` struct.
pub mod error;

use crate::dice_roll::{DiceRoll, RollType};
use crate::error::ParserError;

#[derive(Clone, Debug, PartialEq)]
enum Modifier {
    Plus,
    Minus,
}

fn multi_dice_parser(i: &str) -> nom::IResult<&str, (Option<&str>, &str)> {
    sequence::tuple((combinator::opt(character::complete::digit1), dice_parser))(i)
}

fn dice_parser(i: &str) -> nom::IResult<&str, &str> {
    sequence::preceded(character::complete::char('d'), character::complete::digit1)(i)
}

fn modifier_sign_parser(i: &str) -> nom::IResult<&str, Modifier> {
    branch::alt((
        combinator::value(Modifier::Plus, character::complete::char('+')),
        combinator::value(Modifier::Minus, character::complete::char('-')),
    ))(i)
}

fn modifier_value_parser(i: &str) -> nom::IResult<&str, &str> {
    character::complete::digit1(i)
}

fn dice_roll_type_parser(i: &str) -> nom::IResult<&str, RollType> {
    let result = roll_type_parser(i);
    match result {
        Ok((i, None)) => Ok((i, RollType::Regular)),
        Ok((i, Some(roll_type))) => Ok((i, roll_type)),
        Err(e) => Err(e),
    }
}

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
        combinator::value(
            RollType::Independent,
            branch::alt((
                // Order matters here
                bytes::complete::tag_no_case("independent"),
                bytes::complete::tag_no_case("ind"),
                bytes::complete::tag_no_case("i"),
            )),
        ),
    )))(i)
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
pub fn parse_line(i: &str) -> Result<DiceRoll, ParserError> {
    let parser_output = combinator::all_consuming(sequence::tuple((
        multi_dice_parser,
        character::complete::multispace0,
        combinator::opt(sequence::tuple((
            modifier_sign_parser,
            character::complete::multispace0,
            modifier_value_parser,
        ))),
        character::complete::multispace0,
        dice_roll_type_parser,
    )))(i);

    match parser_output {
        Ok((
            _, // Remaining input
            (
                (number_of_dice, dice_sides),
                _, // space
                optional_modifier,
                _, // Space
                roll_type,
            ),
        )) => {
            let number_of_dice: u32 = number_of_dice.map_or(Ok(1), str::parse)?;
            let dice_sides: u32 = dice_sides.parse()?;
            match optional_modifier {
                None => Ok(DiceRoll::new(dice_sides, None, number_of_dice, roll_type)),
                Some((sign, _, value)) => {
                    let modifier_value: i32 = value.parse()?;
                    match sign {
                        Modifier::Plus => Ok(DiceRoll::new(
                            dice_sides,
                            Some(modifier_value),
                            number_of_dice,
                            roll_type,
                        )),
                        Modifier::Minus => {
                            let modifier = Some(-modifier_value);
                            Ok(DiceRoll::new(
                                dice_sides,
                                modifier,
                                number_of_dice,
                                roll_type,
                            ))
                        }
                    }
                }
            }
        }
        // TODO: Find a way to include Nom error info in ParseError.
        Err(_) => Err(ParserError::ParseError),
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

        assert_eq!(
            dice_roll_type_parser("independent"),
            Ok((" ", RollType::Independent))
        );
        assert_eq!(
            dice_roll_type_parser("ind"),
            Ok(("", RollType::Independent))
        );
        assert_eq!(dice_roll_type_parser("i"), Ok(("", RollType::Independent)));
        assert_eq!(dice_roll_type_parser("I"), Ok(("", RollType::Independent)));

        assert_eq!(dice_roll_type_parser(""), Ok(("", RollType::Regular)));
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(
            parse_line("d6"),
            Ok(DiceRoll::new(6, None, 1, RollType::Regular))
        );
        assert_eq!(
            parse_line("d20 +      5"),
            Ok(DiceRoll::new(20, Some(5), 1, RollType::Regular))
        );
        assert_eq!(
            parse_line("2d10 - 5"),
            Ok(DiceRoll::new(10, Some(-5), 2, RollType::Regular))
        );
        assert_eq!(
            parse_line("3d6"),
            Ok(DiceRoll::new(6, None, 3, RollType::Regular))
        );
        assert_eq!(
            parse_line("5d20 +      5"),
            Ok(DiceRoll::new(20, Some(5), 5, RollType::Regular))
        );
        assert_eq!(
            parse_line("d0 - 5"),
            Ok(DiceRoll::new(0, Some(-5), 1, RollType::Regular))
        );

        assert_eq!(
            parse_line("d200a"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );
        assert_eq!(
            parse_line("d200adv"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );
        assert_eq!(
            parse_line("d200advantage"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );

        assert_eq!(
            parse_line("d200 A"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );
        assert_eq!(
            parse_line("d200 adv"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );
        assert_eq!(
            parse_line("d200 advantage"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithAdvantage))
        );

        assert_eq!(
            parse_line("d200 disadvantage"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithDisadvantage))
        );
        assert_eq!(
            parse_line("d200 d"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithDisadvantage))
        );
        assert_eq!(
            parse_line("d200 dadv"),
            Ok(DiceRoll::new(200, None, 1, RollType::WithDisadvantage))
        );

        assert_eq!(
            parse_line("d200 independent"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );
        assert_eq!(
            parse_line("d200 ind"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );
        assert_eq!(
            parse_line("d200 i"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );

        assert_eq!(
            parse_line("d200independent"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );
        assert_eq!(
            parse_line("d200ind"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );
        assert_eq!(
            parse_line("d200i"),
            Ok(DiceRoll::new(200, None, 1, RollType::Independent))
        );

        assert_eq!(parse_line("cd20"), Err(ParserError::ParseError));
    }
}
