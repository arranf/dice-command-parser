use nom::{branch, bytes, character, combinator, sequence};

pub mod dice_roll;
pub mod error;

use crate::dice_roll::DiceRoll;
use crate::error::ParserError;

#[derive(Clone, Debug, PartialEq)]
enum Modifier {
    Plus,
    Minus,
}

fn roll_command_parser(i: &str) -> nom::IResult<&str, &str> {
    bytes::complete::tag_no_case("!roll")(i)
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

pub fn parse_line(i: &str) -> Result<DiceRoll, ParserError> {
    let parser_output = combinator::all_consuming(sequence::tuple((
        roll_command_parser,
        character::complete::space0,
        multi_dice_parser,
        character::complete::space0,
        combinator::opt(sequence::tuple((
            modifier_sign_parser,
            character::complete::space0,
            modifier_value_parser,
        ))),
    )))(i);

    match parser_output {
        Ok((
            _, // Remaining input
            (
                _, // Roll command
                _, // space
                (number_of_dice, dice_sides),
                _, //space
                optional_modifier,
            ),
        )) => {
            let number_of_dice: u32 = number_of_dice.map_or(Ok(1), |v| v.parse())?;
            let dice_sides: u32 = dice_sides.parse()?;
            match optional_modifier {
                None => Ok(DiceRoll::new(dice_sides, None, number_of_dice)),
                Some((sign, _, value)) => {
                    let modifier_value: i32 = value.parse()?;
                    match sign {
                        Modifier::Plus => Ok(DiceRoll::new(
                            dice_sides,
                            Some(modifier_value),
                            number_of_dice,
                        )),
                        Modifier::Minus => {
                            let modifier = Some(modifier_value as i32 * -1);
                            Ok(DiceRoll::new(dice_sides, modifier, number_of_dice))
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
    fn test_roll_command_parser() {
        assert_eq!(
            roll_command_parser("!roll d6 + 2"),
            Ok((" d6 + 2", "!roll"))
        );
        assert_eq!(
            roll_command_parser("!roLL d6 + 2"),
            Ok((" d6 + 2", "!roLL"))
        );
        assert_eq!(
            roll_command_parser("/roll d6 + 2"),
            Err(nom::Err::Error((
                "/roll d6 + 2",
                nom::error::ErrorKind::Tag
            )))
        );
    }

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
    fn test_parse_line() {
        assert_eq!(parse_line("!roll d6"), Ok(DiceRoll::new(6, None, 1)));
        assert_eq!(
            parse_line("!roll d20 +      5"),
            Ok(DiceRoll::new(20, Some(5), 1))
        );
        assert_eq!(
            parse_line("!roll 2d10 - 5"),
            Ok(DiceRoll::new(10, Some(-5), 2))
        );
        assert_eq!(parse_line("!roll 3d6"), Ok(DiceRoll::new(6, None, 3)));
        assert_eq!(
            parse_line("!roll 5d20 +      5"),
            Ok(DiceRoll::new(20, Some(5), 5))
        );
        assert_eq!(
            parse_line("!roll d0 - 5"),
            Ok(DiceRoll::new(0, Some(-5), 1))
        );
        assert_eq!(parse_line("roll d20"), Err(ParserError::ParseError));
        assert_eq!(parse_line("roll d20"), Err(ParserError::ParseError));
        assert_eq!(parse_line("d20"), Err(ParserError::ParseError));
    }
}
