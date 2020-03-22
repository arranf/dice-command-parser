#[derive(Clone, Default, Debug, PartialEq)]
pub struct DiceRoll {
    pub number_of_dice_to_roll: u32,
    pub dice_sides: u32,
    pub modifier: Option<i32>,
}

impl DiceRoll {
    pub fn new(dice_sides: u32, modifier: Option<i32>, number_of_dice_to_roll: u32) -> Self {
        DiceRoll {
            dice_sides,
            modifier,
            number_of_dice_to_roll,
        }
    }
}
