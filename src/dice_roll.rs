#[derive(Clone, Debug, PartialEq)]
/// This struct represents the information required to calculate the result of a dice roll given the command string.
/// Validating the sanity of each of the parameters is left to the user. e.g. The number of dice to roll could be 0.
pub struct DiceRoll {
    /// How many dice should be rolled.
    pub number_of_dice_to_roll: u32,
    /// How many faces each dice has.
    pub dice_sides: u32,
    /// The optional fixed modifier that should be applied to each dice roll. Can be positive or negative.
    pub modifier: Option<i32>,
    /// Whether the roll has advantage, disadvantage, or is a regular roll
    pub roll_type: RollType,
    /// How the roll should be applied
    pub operation: Operation,
}

impl DiceRoll {
    /// A convinience method for creating a `DiceRoll`.
    ///
    /// # Examples
    ///
    /// This represents a d6 with no modifier
    /// ```
    /// use dice_command_parser::dice_roll::{DiceRoll, RollType, Operation};
    ///
    /// let dice_roll = DiceRoll::new(6, None, 1, RollType::Regular, Operation::Addition);
    /// ```
    ///
    /// This represents two d20 with a +1 modifier rolling with advantage
    /// ```
    /// use dice_command_parser::dice_roll::{DiceRoll, RollType, Operation};
    ///
    /// let dice_roll = DiceRoll::new(20, Some(1), 2, RollType::WithAdvantage, Operation::Subtraction);
    /// ```
    #[must_use]
    pub fn new(
        dice_sides: u32,
        modifier: Option<i32>,
        number_of_dice_to_roll: u32,
        roll_type: RollType,
        operation: Operation,
    ) -> Self {
        DiceRoll {
            dice_sides,
            modifier,
            number_of_dice_to_roll,
            roll_type,
            operation,
        }
    }

    /// A convinience method for creating a `DiceRoll`, without advantage or disadvantage.
    ///
    /// # Examples
    ///
    /// This represents a d6 with no modifier
    /// ```
    /// use dice_command_parser::dice_roll::{DiceRoll, Operation};
    ///
    /// let dice_roll = DiceRoll::new_regular_roll(6, None, 1, Operation::Addition);
    /// ```
    ///
    /// This represents two d20 with a +1 modifier.
    /// ```
    /// use dice_command_parser::dice_roll::{DiceRoll, Operation};
    ///
    /// let dice_roll = DiceRoll::new_regular_roll(20, Some(1), 2, Operation::Addition);
    /// ```
    #[must_use]
    pub fn new_regular_roll(
        dice_sides: u32,
        modifier: Option<i32>,
        number_of_dice_to_roll: u32,
        operation: Operation,
    ) -> Self {
        DiceRoll {
            dice_sides,
            modifier,
            number_of_dice_to_roll,
            roll_type: RollType::Regular,
            operation,
        }
    }
}

/// Represents whether a roll has advantage, disadvantage, independence, or not.
#[derive(Clone, Debug, PartialEq)]
pub enum RollType {
    /// The roll has advantage and the highest of the two rolls for a set of dice is taken.
    WithAdvantage,
    /// The roll has disadvantage and the lowest of the two rolls for a set of dice is taken.
    WithDisadvantage,
    /// A regular roll occurs - only one roll needs to occur.
    Regular,
}

impl Default for RollType {
    fn default() -> Self {
        Self::Regular
    }
}

/// Represents whether a roll should be added or taken away from the total
#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    /// The roll should be added towards the overall total
    Addition,
    /// The roll should be taken away from the overall total
    Subtraction,
}
