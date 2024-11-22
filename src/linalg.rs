use itertools::Itertools;

use crate::ast::{AssocOp, Exp};

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

    pub fn from_col(col: Vec<Exp>) -> Self {
        Self {
            rows: col.into_iter().map(|entry| vec![entry]).collect_vec(),
        }
    }

    pub fn from_row(row: Vec<Exp>) -> Self {
        Self { rows: vec![row] }
    }

    pub fn filled(exp: Exp, size: (usize, usize)) -> Self {
        Self {
            rows: vec![vec![exp; size.1]; size.0],
        }
    }

    pub fn diagonal(diag: Vec<Exp>) -> Self {
        let size = diag.len();
        let rows = diag
            .into_iter()
            .enumerate()
            .map(|(index, exp)| {
                let mut row = vec![Exp::ZERO; size];
                row[index] = exp;
                row
            })
            .collect_vec();
        Self { rows }
    }

    pub fn identity(size: usize) -> Self {
        Self::diagonal(vec![Exp::ONE; size])
    }

    pub fn is_square(&self) -> bool {
        self.width() == self.height()
    }

    pub fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(&Exp) -> Exp,
    {
        Self {
            rows: self
                .rows
                .iter()
                .map(|row| row.iter().map(&mut f).collect_vec())
                .collect_vec(),
        }
    }

    pub fn try_map<F, E>(&self, mut f: F) -> Result<Self, E>
    where
        F: FnMut(&Exp) -> Result<Exp, E>,
    {
        Ok(Self {
            rows: self
                .rows
                .iter()
                .map(|row| row.iter().map(&mut f).try_collect())
                .try_collect()?,
        })
    }

    pub fn scale(&self, exp: &Exp) -> Self {
        self.map(|entry| Exp::assoc_combine(AssocOp::Mul, exp.clone(), entry.clone()))
    }

    pub fn rows(self) -> Vec<Vec<Exp>> {
        self.rows
    }

    pub fn rows_ref(&self) -> &Vec<Vec<Exp>> {
        &self.rows
    }

    pub fn height(&self) -> usize {
        self.rows.len()
    }

    pub fn width(&self) -> usize {
        self.rows.first().map_or(0, |row| row.len())
    }

    pub fn size(&self) -> (usize, usize) {
        (self.height(), self.width())
    }

    pub fn iter_by_rows(&self) -> impl Iterator<Item = &Exp> {
        self.rows_ref().iter().flatten()
    }
}

impl From<Matrix> for Exp {
    fn from(value: Matrix) -> Self {
        Self::Matrix(value)
    }
}

impl std::ops::Index<(usize, usize)> for Matrix {
    type Output = Exp;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        &self.rows[index.0][index.1]
    }
}

impl std::ops::Add for &Matrix {
    type Output = Matrix;

    fn add(self, rhs: Self) -> Self::Output {
        let mut rows = Vec::new();
        for (row1, row2) in self.rows_ref().iter().zip(rhs.rows_ref().iter()) {
            let row = row1
                .iter()
                .zip(row2.iter())
                .map(|(exp1, exp2)| Exp::assoc_combine(AssocOp::Add, exp1.clone(), exp2.clone()))
                .collect_vec();
            rows.push(row);
        }
        Matrix { rows }
    }
}

impl std::ops::Add for Matrix {
    type Output = Matrix;

    fn add(self, rhs: Self) -> Self::Output {
        let mut rows = Vec::new();
        for (row1, row2) in self.rows().into_iter().zip_eq(rhs.rows().into_iter()) {
            let row = row1
                .into_iter()
                .zip_eq(row2.into_iter())
                .map(|(exp1, exp2)| Exp::assoc_combine(AssocOp::Add, exp1, exp2))
                .collect_vec();
            rows.push(row);
        }
        Matrix { rows }
    }
}

impl std::ops::Mul for &Matrix {
    type Output = Matrix;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut rows = Vec::new();
        let common_len = self.width();
        if common_len != rhs.height() {
            panic!("incompatible matrix sizes")
        }
        for i in 0..self.height() {
            let mut row = Vec::new();
            for j in 0..rhs.width() {
                row.push(
                    (0..common_len)
                        .map(|k| {
                            Exp::assoc_combine(
                                AssocOp::Mul,
                                self[(i, k)].clone(),
                                rhs[(j, k)].clone(),
                            )
                        })
                        .reduce(|a, b| Exp::assoc_combine(AssocOp::Add, a, b))
                        .expect("empty matrix"),
                )
            }
            rows.push(row);
        }

        Matrix { rows }
    }
}

impl std::ops::Mul for Matrix {
    type Output = Matrix;
    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}
