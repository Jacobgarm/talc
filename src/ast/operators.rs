use std::{cell::LazyCell, collections::HashMap};

use crate::typing::ExpType;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Factorial,
    Abs,
    Arg,
    Floor,
    Ceil,
    Norm,
    Not,
}

impl UnaryOp {
    pub const ALL: [Self; 7] = [
        Self::Factorial,
        Self::Abs,
        Self::Arg,
        Self::Floor,
        Self::Ceil,
        Self::Norm,
        Self::Not,
    ];

    pub fn symbols(&self) -> (&'static str, &'static str) {
        use UnaryOp::*;
        match self {
            Factorial => ("", "!"),
            Abs => ("|", "|"),
            Arg => ("~", ""),
            Floor => ("⌊", "⌋"),
            Ceil => ("⌈", "⌉"),
            Norm => ("||", "||"),
            Not => ("¬", ""),
        }
    }

    pub fn is_wrapping(&self) -> bool {
        let (pre, post) = self.symbols();
        !pre.is_empty() && !post.is_empty()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}

impl Associativity {
    pub fn is_left(self) -> bool {
        self == Self::Left
    }

    pub fn is_right(self) -> bool {
        self == Self::Right
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Infix {
    Dyadic(DyadicOp),
    Assoc(AssocOp),
    Relation(Relation),
}

impl Infix {
    pub fn symbol(self) -> char {
        match self {
            Self::Dyadic(op) => op.symbol(),
            Self::Assoc(op) => op.symbol(),
            Self::Relation(op) => op.symbol(),
        }
    }

    pub fn precedence(self) -> u8 {
        match self {
            Self::Dyadic(op) => op.precedence(),
            Self::Assoc(op) => op.precedence(),
            Self::Relation(op) => op.precedence(),
        }
    }

    pub fn precedence_associativity(prec: u8) -> Associativity {
        #![allow(clippy::manual_range_patterns)]
        match prec {
            0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 => Associativity::Left,
            10 => Associativity::Right,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum DyadicOp {
    Mod,
    Pow,

    CrossProd,
    DotProd,

    LogicImplies,
    LogicEquiv,

    SetDifference,
    SymDifference,
}

impl DyadicOp {
    pub const ALL: [Self; 8] = [
        Self::Mod,
        Self::Pow,
        Self::CrossProd,
        Self::DotProd,
        Self::LogicImplies,
        Self::LogicEquiv,
        Self::SetDifference,
        Self::SymDifference,
    ];

    pub const COMMUTATIVES: [Self; 3] = [Self::DotProd, Self::LogicEquiv, Self::SymDifference];

    pub fn is_commutative(self) -> bool {
        Self::COMMUTATIVES.contains(&self)
    }

    pub fn precedence(self) -> u8 {
        use DyadicOp::*;
        match self {
            LogicImplies | LogicEquiv => 0,
            // LogicOr 1
            // LogicXor 2
            // LogicAnd 3
            // Eq | Neq | Leq | Geq | Gt | Lt | Approx | Elem | Subset | SubsetEq | Superset
            // SupersetEq => 4,
            // Union 5
            SymDifference | SetDifference => 6, // Intersection
            // Addition 7
            Mod => 8, // Multiplication
            DotProd | CrossProd => 9,
            Pow => 10,
        }
    }

    pub fn symbol(self) -> char {
        use DyadicOp::*;
        match self {
            Mod => '%',
            Pow => '^',
            CrossProd => '×',
            DotProd => '∙',

            LogicImplies => '⇒',
            LogicEquiv => '⇔',

            SetDifference => '\\',
            SymDifference => 'Δ',
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssocOp {
    Add,
    Mul,
    Union,
    Intersection,
    LogicAnd,
    LogicOr,
    LogicXor,
}

impl AssocOp {
    pub const ALL: [Self; 7] = [
        Self::Add,
        Self::Mul,
        Self::Union,
        Self::Intersection,
        Self::LogicAnd,
        Self::LogicOr,
        Self::LogicXor,
    ];

    pub fn symbol(self) -> char {
        use AssocOp::*;
        match self {
            LogicOr => '∨',
            LogicXor => '⊻',
            LogicAnd => '∧',
            Union => '∪',
            Intersection => '∩',
            Add => '+',
            Mul => '*',
        }
    }

    pub fn precedence(self) -> u8 {
        use AssocOp::*;
        match self {
            LogicOr => 1,
            LogicXor => 2,
            LogicAnd => 3,
            Union => 5,
            Intersection => 6,
            Add => 7,
            Mul => 8,
        }
    }

    pub fn accepted_types(self) -> &'static [ExpType] {
        use AssocOp::*;
        match self {
            LogicOr | LogicXor | LogicAnd => &[ExpType::Bool],
            Union | Intersection => &[ExpType::Set],
            Add => &[ExpType::Numeric, ExpType::Matrix],
            Mul => &[ExpType::Numeric, ExpType::Matrix],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Relation {
    Eq,
    Neq,
    Approx,

    Lt,
    Gt,
    Leq,
    Geq,

    Elem,
    Subset,
    Superset,
    SubsetEq,
    SupersetEq,
}

impl Relation {
    pub const ALL: [Self; 12] = [
        Self::Eq,
        Self::Neq,
        Self::Approx,
        Self::Lt,
        Self::Gt,
        Self::Leq,
        Self::Geq,
        Self::Elem,
        Self::Subset,
        Self::Superset,
        Self::SubsetEq,
        Self::SupersetEq,
    ];

    pub fn symbol(self) -> char {
        use Relation::*;
        match self {
            Eq => '=',
            Neq => '≠',
            Lt => '<',
            Gt => '>',
            Leq => '≤',
            Geq => '≥',
            Approx => '≈',
            Elem => '∈',
            Subset => '⊂',
            Superset => '⊃',
            SubsetEq => '⊆',
            SupersetEq => '⊇',
        }
    }

    pub fn precedence(self) -> u8 {
        use Relation::*;
        match self {
            Eq | Neq | Leq | Geq | Gt | Lt | Approx | Elem | Subset | SubsetEq | Superset
            | SupersetEq => 4,
        }
    }
}

thread_local! {
    static CHAR_INFIX_MAP: LazyCell<HashMap<char, Infix>> = LazyCell::new(||{
        let mut map = HashMap::new();
        for op in DyadicOp::ALL {
            map.insert(op.symbol(), Infix::Dyadic(op));
        }
        for op in AssocOp::ALL {
            map.insert(op.symbol(), Infix::Assoc(op));
        }
        for op in Relation::ALL {
            map.insert(op.symbol(), Infix::Relation(op));
        }
        map.insert('-', Infix::Assoc(AssocOp::Add));
        map.insert('/', Infix::Assoc(AssocOp::Mul));
        map
        }
    );
}
pub fn infix_from_char(c: char) -> Option<Infix> {
    CHAR_INFIX_MAP.with(|map| map.get(&c).cloned())
}
