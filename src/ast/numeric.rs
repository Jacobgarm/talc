use malachite::num::basic::traits::{NegativeOne, One, Zero};
use malachite::num::conversion::traits::RoundingFrom;
use malachite::num::float::NiceFloat;
use malachite::rounding_modes::RoundingMode;
use malachite::{Integer, Rational};
use malachite_float::{ComparableFloat, Float};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Numeric {
    Integer(Integer),
    Rational(Rational),
    Big(ComparableFloat),
    Small(NiceFloat<f64>),
}

use Numeric::*;

impl Numeric {
    pub const ZERO_INT: Self = Integer(Integer::ZERO);
    pub const ONE_INT: Self = Integer(Integer::ONE);
    pub const NEGATIVE_ONE_INT: Self = Integer(Integer::NEGATIVE_ONE);

    pub fn is_zero(&self) -> bool {
        match self {
            Integer(int) => *int == 0,
            Rational(rat) => *rat == 0,
            Big(float) => float.is_zero(),
            Small(float) => float.0 == 0.0,
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Integer(int) => *int == 1,
            Rational(rat) => *rat == 1,
            Big(float) => float.0 == 1,
            Small(float) => float.0 == 1.0,
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Integer(int) => *int < 0,
            Rational(rat) => *rat < 0,
            Big(float) => float.is_sign_negative(),
            Small(float) => float.0.is_sign_negative(),
        }
    }

    pub fn precision(&self) -> u8 {
        match self {
            Integer(_) => 3,
            Rational(_) => 2,
            Big(_) => 1,
            Small(_) => 0,
        }
    }

    pub fn smallify(self) -> Self {
        let val = match self {
            Integer(int) => f64::rounding_from(&int, RoundingMode::Nearest).0,
            Rational(rat) => f64::rounding_from(&rat, RoundingMode::Nearest).0,
            Big(float) => f64::rounding_from(&float.0, RoundingMode::Nearest).0,
            Small(_) => return self,
        };
        Small(NiceFloat(val))
    }

    pub fn bigify(self, prec: u64) -> Self {
        let val = match self {
            Integer(int) => Float::from_integer_min_prec(int),
            Rational(rat) => Float::from_rational_prec(rat, prec).0,
            Big(_) => return self,
            Small(float) => todo!(),
        };
        Big(ComparableFloat(val))
    }

    pub fn rationalify(self) -> Self {
        let val = match self {
            Integer(int) => Rational::from_integers(int, Integer::ONE),
            Rational(_) => return self,
            Big(float) => todo!(),
            Small(float) => todo!(),
        };
        Rational(val)
    }
}

pub fn common_form(a: Numeric, b: Numeric) -> (Numeric, Numeric) {
    match (&a, &b) {
        (Integer(..), Integer(..))
        | (Rational(..), Rational(..))
        | (Small(..), Small(..))
        | (Big(..), Big(..)) => (a, b),
        (Small(..), _) => (a, b.smallify()),
        (_, Small(..)) => (a.smallify(), b),
        (Big(a_float), _) => {
            let b = b.bigify(a_float.get_prec().unwrap());
            (a, b)
        }
        (_, Big(b_float)) => {
            let a = a.bigify(b_float.get_prec().unwrap());
            (a, b)
        }
        (Rational(_), _) => (a, b.rationalify()),
        (_, Rational(_)) => (a.rationalify(), b),
    }
}

pub fn most_precise_first<'a>(a: &'a Numeric, b: &'a Numeric) -> (&'a Numeric, &'a Numeric) {
    if a.precision() >= b.precision() {
        (a, b)
    } else {
        (b, a)
    }
}

impl std::ops::Add for &Numeric {
    type Output = Numeric;
    fn add(self, rhs: Self) -> Self::Output {
        match most_precise_first(self, rhs) {
            (Integer(a), Integer(b)) => Integer(a + b),
            (Integer(a), Rational(b)) => Rational(Rational::from(a) + b),
            (Rational(a), Rational(b)) => Rational(a + b),
            _ => todo!(),
        }
    }
}

impl std::ops::Add for Numeric {
    type Output = Numeric;
    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl std::ops::Mul for &Numeric {
    type Output = Numeric;
    fn mul(self, rhs: Self) -> Self::Output {
        match most_precise_first(self, rhs) {
            (Integer(a), Integer(b)) => Integer(a * b),
            (Integer(a), Rational(b)) => Rational(Rational::from(a) * b),
            (Rational(a), Rational(b)) => Rational(a * b),
            _ => todo!(),
        }
    }
}

impl std::ops::Mul for Numeric {
    type Output = Numeric;
    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl From<Numeric> for crate::ast::Exp {
    fn from(value: Numeric) -> Self {
        Self::Number(value)
    }
}
