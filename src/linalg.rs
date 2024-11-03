use crate::ast::Exp;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Matrix {
    rows: Vec<Vec<Exp>>,
}

impl Matrix {
    pub fn from_rows(rows: Vec<Vec<Exp>>) -> Result<Self, ()> {
        if let Some(first) = rows.first() {
            let num_cols = first.len();
            for row in rows.iter().skip(1) {
                if row.len() != num_cols {
                    return Err(());
                }
            }
        }
        Ok(Self { rows })
    }

    pub fn from_cols(mut cols: Vec<Vec<Exp>>) -> Result<Self, ()> {
        let num_rows = if let Some(first) = cols.first() {
            first.len()
        } else {
            return Ok(Self { rows: cols });
        };
        let mut rows = Vec::new();
        for i in 0..num_rows {
            let mut row = Vec::new();
            for col in cols.iter_mut() {
                let Some(entry) = col.pop() else {
                    return Err(());
                };
                row.push(entry);
            }
            rows.insert(0, row);
        }

        Ok(Self { rows })
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
