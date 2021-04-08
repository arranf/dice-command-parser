use crate::dice_roll::{DiceRoll, Operation};

#[derive(Clone, Debug, PartialEq)]
/// A struct that contains the dice roll, plus whether the roll is negative or positive.
pub struct DiceRollWithOp {
    /// The `DiceRoll`
    pub dice_roll: DiceRoll,
    /// How the roll should be applied. Either it should have a positive impact on the total sum, or a negative one
    pub operation: Operation,
}

/// A convinience method for creating a `DiceRollWithOp`.
///
/// # Examples
///
/// This represents a d6 with no modifier
/// ```
/// use dice_command_parser::{dice_roll::{DiceRoll, Operation}, dice_roll_with_op::DiceRollWithOp};
///
/// let dice_roll = DiceRollWithOp::new(DiceRoll::new_regular_roll(6, None, 1), Operation::Addition);
/// ```
impl DiceRollWithOp {
    #[must_use]
    /// Creates a new `DiceRollWithOp`
    pub fn new(dice_roll: DiceRoll, operation: Operation) -> Self {
        DiceRollWithOp {
            dice_roll,
            operation: operation,
        }
    }
}
