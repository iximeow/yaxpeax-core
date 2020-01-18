use data::Direction;

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Use {
    Read, Write, ReadWrite
}

impl Use {
    pub fn first_use(&self) -> Direction {
        match self {
            Use::Read | Use::ReadWrite => {
                Direction::Read
            },
            Use::Write => {
                Direction::Write
            }
        }
    }
}
