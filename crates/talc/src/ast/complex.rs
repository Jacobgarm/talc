use super::{Exp, RealNum};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComplexNum {
    pub real: RealNum,
    pub imag: RealNum,
}

impl ComplexNum {
    pub const I: Self = Self {
        real: RealNum::ZERO_INT,
        imag: RealNum::ONE_INT,
    };

    pub fn new(real: RealNum, imag: RealNum) -> Self {
        Self { real, imag }
    }

    pub fn from_real(real: RealNum) -> Self {
        Self {
            real,
            imag: RealNum::ZERO_INT,
        }
    }

    pub fn from_imag(imag: RealNum) -> Self {
        Self {
            real: RealNum::ZERO_INT,
            imag,
        }
    }

    pub fn is_zero(&self) -> bool {
        self.real.is_zero() && self.imag.is_zero()
    }

    pub fn is_exact_zero(&self) -> bool {
        self.real.is_exact_zero() && self.imag.is_exact_zero()
    }

    pub fn is_real(&self) -> bool {
        self.imag.is_zero()
    }

    pub fn is_exact_real(&self) -> bool {
        self.imag.is_exact_zero()
    }

    pub fn is_imag(&self) -> bool {
        self.real.is_zero()
    }

    pub fn is_exact_imag(&self) -> bool {
        self.real.is_exact_zero()
    }
}

impl std::ops::Add for &ComplexNum {
    type Output = ComplexNum;

    fn add(self, rhs: Self) -> Self::Output {
        ComplexNum {
            real: &self.real + &rhs.real,
            imag: &self.imag + &rhs.imag,
        }
    }
}

impl std::ops::Add for ComplexNum {
    type Output = ComplexNum;

    fn add(self, rhs: Self) -> Self::Output {
        ComplexNum {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}

impl std::ops::Mul for &ComplexNum {
    type Output = ComplexNum;

    fn mul(self, rhs: Self) -> Self::Output {
        ComplexNum {
            real: &self.real * &rhs.real - &self.imag * &rhs.imag,
            imag: &self.real * &rhs.imag + &self.imag * &rhs.real,
        }
    }
}

impl std::ops::Mul for ComplexNum {
    type Output = ComplexNum;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl From<RealNum> for ComplexNum {
    fn from(value: RealNum) -> Self {
        Self {
            real: value,
            imag: RealNum::ZERO_INT,
        }
    }
}

impl TryFrom<ComplexNum> for RealNum {
    type Error = &'static str;
    fn try_from(value: ComplexNum) -> Result<Self, Self::Error> {
        if value.imag.is_zero() {
            Ok(value.real)
        } else {
            Err("nonzero real part")
        }
    }
}

impl From<ComplexNum> for Exp {
    fn from(value: ComplexNum) -> Self {
        Exp::Complex(value)
    }
}
