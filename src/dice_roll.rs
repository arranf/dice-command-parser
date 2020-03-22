#[derive(Clone, Default, Debug, PartialEq)]
pub struct DiceRoll {
    pub dice_sides: u32,
    pub modifier: Option<i32>,
}

impl DiceRoll {
    pub fn new(dice_sides: u32, modifier: Option<i32>) -> Self {
        DiceRoll {
            dice_sides,
            modifier,
        }
    }
}
