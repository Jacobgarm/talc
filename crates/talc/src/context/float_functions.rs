use malachite::num::basic::traits::{One, OneHalf};
use malachite::num::conversion::traits::RoundingFrom;
use malachite::rounding_modes::RoundingMode;
use malachite::{num::arithmetic::traits::Factorial, num::basic::traits::Zero, Natural, Rational};
use memoize::memoize;

pub fn riemann_zeta(s: f64) -> f64 {
    (1..10000).map(|n| f64::from(n).powf(-s)).sum()
}

pub fn riemann_zeta_em(s: f64) -> f64 {
    const N: i32 = 10000;
    const M: u64 = 10;
    let mut z: f64 = (1..N).map(|n| f64::from(n).powf(-s)).sum();
    z += 0.5 * f64::from(N).powf(-s);
    z += f64::from(N).powf(1.0 - s) / (s - 1.0);
    for k in 1..=M {
        let mut t = f64::rounding_from(
            bernoulli(2 * k) / Rational::from(Natural::factorial(2 * k)),
            RoundingMode::Nearest,
        )
        .0;

        t *= (0..=2 * k - 2).map(|j| s + j as f64).product::<f64>();
        t *= f64::from(N).powf(1.0 - s - (2 * k) as f64);
        z += t;
    }
    z
}

#[memoize]
fn bernoulli(n: u64) -> Rational {
    if n == 1 {
        Rational::ONE_HALF
    } else if n % 2 == 1 {
        Rational::ZERO
    } else {
        Rational::ONE
            - (0..n)
                .map(|k| {
                    bernoulli(k) / Rational::from_unsigneds(n - k + 1, 1)
                        * Rational::from(choose(n, k))
                })
                .sum::<Rational>()
    }
}

fn choose(n: u64, k: u64) -> Natural {
    Natural::factorial(n) / (Natural::factorial(k) * Natural::factorial(n - k))
}
