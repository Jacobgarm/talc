#![feature(iterator_try_collect)]

mod strings;
pub use strings::{lower_superscript, raise_superscript};

pub fn try_one_to_zero_index(one_index: &[usize]) -> Option<Vec<usize>> {
    one_index
        .iter()
        .map(|i| if *i == 0 { None } else { Some(i - 1) })
        .try_collect()
}
