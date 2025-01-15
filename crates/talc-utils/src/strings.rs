use itertools::Itertools;

const SUPER: &str = "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁽⁾";
const NORMAL: &str = "0123456789+-()";

pub fn lower_superscript(s: &str) -> String {
    let mut new = s.to_owned();
    for (sd, d) in SUPER.chars().zip_eq(NORMAL.chars()) {
        new = new.replace(sd, &d.to_string());
    }
    new
}

pub fn raise_superscript(s: &str) -> String {
    let mut new = s.to_owned();
    for (sd, d) in SUPER.chars().zip_eq(NORMAL.chars()) {
        new = new.replace(d, &sd.to_string());
    }
    new
}
