#[derive(Clone, Default, Debug, PartialEq)]

/// This struct represents the information required to calculate the result of a dice roll given the command string.
/// Validating the sanity of each of the parameters is left to the user. e.g. The number of dice to roll could be 0.
pub struct DiceRoll {
    /// How many dice should be rolled.
    pub number_of_dice_to_roll: u32,
    /// How many faces each dice has.
    pub dice_sides: u32,
    /// The optional fixed modifier that should be applied to each dice roll. Can be positive or negative.
    pub modifier: Option<i32>,
}

impl DiceRoll {
    #[must_use]
    /// A convinience method for creating a `DiceRoll`.
    ///
    /// # Examples
    ///
    /// This represents a d6 with no modifier
    /// ```
    /// let dice_roll = DiceRoll::new(6, None, 1);
    /// ```
    ///
    /// This represents two d20 with a +1 modifier
    /// ```
    /// let dice_roll = DiceRoll::new(20, Some(+1), 2);
    /// ```
    pub fn new(dice_sides: u32, modifier: Option<i32>, number_of_dice_to_roll: u32) -> Self {
        DiceRoll {
            dice_sides,
            modifier,
            number_of_dice_to_roll,
        }
    }
}
