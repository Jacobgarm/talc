use std::collections::HashMap;

use super::Exp;

//macro_rules! subs {
//    ($s:literal, $( $t:expr ),+) => {
//        let expr = super::parse($s);
//        let mut map = HashMap::new();
//        $( map.insert("") );*
//        $crate::ast::substitute(expr)
//    };
//}
impl Exp {
    #[must_use]
    pub fn replace(&self, pat: &Exp, with: &Exp) -> Self {
        if self == pat {
            with.clone()
        } else {
            self.map(|e| e.replace(pat, with))
        }
    }

    #[must_use]
    pub fn substitute(&self, map: &HashMap<Exp, Exp>) -> Self {
        if let Some(replacement) = map.get(self) {
            replacement.clone()
        } else {
            self.map(|e| e.substitute(map))
        }
    }
}
