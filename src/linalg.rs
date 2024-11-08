use crate::ast::Exp;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Matrix {
    rows: Vec<Vec<Exp>>,
}

impl Matrix {
    pub fn try_from_rows(rows: Vec<Vec<Exp>>) -> Option<Self> {
        if let Some(first) = rows.first() {
            let num_cols = first.len();
            for row in rows.iter().skip(1) {
                if row.len() != num_cols {
                    return None;
                }
            }
        }
        Some(Self { rows })
    }

    pub fn try_from_cols(mut cols: Vec<Vec<Exp>>) -> Option<Self> {
        let num_rows = if let Some(first) = cols.first() {
            first.len()
        } else {
            return Some(Self { rows: cols });
        };
        let mut rows = Vec::new();
        for _ in 0..num_rows {
            let mut row = Vec::new();
            for col in cols.iter_mut() {
                let entry = col.pop()?;
                row.push(entry);
            }
            rows.insert(0, row);
        }

        Some(Self { rows })
    }

    pub fn rows(&self) -> &Vec<Vec<Exp>> {
        &self.rows
    }

    pub fn height(&self) -> usize {
        self.rows.first().map_or(0, |row| row.len())
    }

    pub fn width(&self) -> usize {
        self.rows.len()
    }

    pub fn size(&self) -> (usize, usize) {
        (self.height(), self.width())
    }
}

//impl std::ops::Add for Matrix {
//    type Output = Self;
//
//    fn add(mut self, rhs: Self) -> Self::Output {
//        for (row1, row2) in self.rows().iter_mut().zip(rhs.rows().into_iter()) {}
//        self
//    }
//}
