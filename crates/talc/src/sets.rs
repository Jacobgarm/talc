#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UrSet {
    Empty,
    Bools,
    Integers,
    Rationals,
    Algebraics,
    Reals,
    Complex,
    Matrices {
        ring: Box<UrSet>,
        width: usize,
        height: usize,
    },
    Powerset(Box<UrSet>),
}

impl UrSet {
    pub fn is_subset(&self, other: &Self) -> bool {
        use UrSet::*;
        match (self, other) {
            (a, b) if a == b => true,
            (Empty, _) => true,
            (Integers, Rationals | Algebraics | Reals | Complex) => true,
            (Rationals, Algebraics | Reals | Complex) => true,
            (Reals, Complex) => true,
            (Algebraics, Complex) => true,
            (
                Matrices {
                    ring: ring1,
                    width: width1,
                    height: height1,
                },
                Matrices {
                    ring: ring2,
                    width: width2,
                    height: height2,
                },
            ) if width1 == width2 && height1 == height2 => ring1.is_subset(ring2),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Set {
    set: UrSet,
}
