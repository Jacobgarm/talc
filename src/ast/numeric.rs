use malachite::num::basic::traits::{NegativeOne, One, Zero};
use malachite::num::conversion::traits::RoundingFrom;
use malachite::num::float::NiceFloat;
use malachite::rounding_modes::RoundingMode;
use malachite::{Integer, Rational};
use malachite_float::ComparableFloat;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Numeric {
    Integer(Integer),
    Rational(Rational),
    Small(NiceFloat<f64>),
    Big(ComparableFloat),
}

use Numeric::*;

impl Numeric {
    pub const ZERO_INT: Self = Integer(Integer::ZERO);
    pub const ONE_INT: Self = Integer(Integer::ONE);
    pub const NEGATIVE_ONE_INT: Self = Integer(Integer::NEGATIVE_ONE);

    pub fn is_negative(&self) -> bool {
        match self {
            Integer(int) => *int < 0,
            Rational(rat) => *rat < 0,
            Small(float) => float.0.is_sign_negative(),
            Big(float) => float.is_sign_negative(),
        }
    }

    pub fn floatify(self) -> Self {
        let val = match self {
            Integer(int) => f64::rounding_from(&int, RoundingMode::Nearest).0,
            Rational(rat) => f64::rounding_from(&rat, RoundingMode::Nearest).0,
            Small(float) => float.0,
            Big(float) => f64::rounding_from(&float.0, RoundingMode::Nearest).0,
        };
        Small(NiceFloat(val))
    }
}
