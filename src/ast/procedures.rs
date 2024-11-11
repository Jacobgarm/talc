use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::Display, strum::EnumString)]
pub enum ProcedureKind {
    #[strum(serialize = "approx")]
    Approximate,

    #[strum(serialize = "assuming")]
    Assuming,

    #[strum(serialize = "PW")]
    Piecewise,

    #[strum(serialize = "deri")]
    Derivative,

    #[strum(serialize = "integral")]
    Integral,

    #[strum(serialize = "sum")]
    Sum,

    #[strum(serialize = "prod")]
    Product,

    #[strum(serialize = "lim")]
    Limit,

    #[strum(serialize = "simp")]
    Simplify,

    #[strum(serialize = "expand")]
    Expand,

    #[strum(serialize = "collect")]
    Collect,

    #[strum(serialize = "isolate")]
    Isolate,

    #[strum(serialize = "solve")]
    Solve,

    #[strum(serialize = "subs")]
    Substitute,

    #[strum(serialize = "eval")]
    Evaluate,

    #[strum(serialize = "vec")]
    ColumnVector,

    #[strum(serialize = "rowvec")]
    RowVector,

    #[strum(serialize = "mat")]
    Matrix,

    #[strum(serialize = "diag")]
    DiagonalMatrix,

    #[strum(serialize = "id_matrix")]
    IdentityMatrix,

    #[strum(serialize = "grad")]
    Gradient,

    #[strum(serialize = "hessian")]
    Hessian,

    #[strum(serialize = "jacobian")]
    Jacobian,

    #[strum(serialize = "eval_default")]
    EvalDefaultContext,
}

impl ProcedureKind {
    pub fn description(self) -> &'static str {
        use ProcedureKind::*;
        match self {
            Approximate => "Approximates the given expression. If precision is specified, calculates the value to at least that many digits",
            _ => ""
        }
    }

    pub fn signature(self) -> &'static [&'static [&'static str]] {
        use ProcedureKind::*;
        match self {
            Approximate => &[&["exp"], &["[precision"]],
            Assuming => &[&["exp"], &["assumption_1", "..."]],
            Piecewise => &[&["exp_1", "condition_1"], &["..."]],
            Derivative => &[&["exp", "variable"]],
            Integral => &[&["exp", "variable", "[lower_bound", "upper_bound"]],
            Sum => &[&["exp", "variable", "start", "end", "[filter_condition"]],
            Product => &[&["exp", "variable", "start", "end", "[filter_condition"]],
            Limit => &[&["exp", "variable", "limit_point"]],
            Simplify => &[&["exp"]],
            Expand => &[&["exp"]],
            Collect => &[&["exp"]],
            Isolate => &[&["exp", "variable"]],
            Solve => &[&["equation_1", "..."], &["[var_1", "..."]],
            Substitute => &[&["exp"], &["a_1", "b_1"], &["..."]],
            Evaluate => &[&["exp"]],
            ColumnVector => &[&["entry_1", "..."]],
            RowVector => &[&["entry_1", "..."]],
            Matrix => &[
                &["entry_11", "entry_12", "..."],
                &["entry_21", "entry_22", "..."],
                &["..."],
            ],
            DiagonalMatrix => &[&["entry_1", "..."]],
            IdentityMatrix => &[&["size"]],
            Gradient => &[&["exp", "[eval_point"]],
            Hessian => &[&["exp", "[eval_point"]],
            Jacobian => &[&["exp", "[eval_point"]],
            EvalDefaultContext => &[&["exp"]],
        }
    }

    pub fn eagerly_eval_args(self) -> bool {
        use ProcedureKind::*;
        matches!(self, ColumnVector | RowVector | Matrix | DiagonalMatrix)
    }

    pub fn signature_string(self) -> String {
        let sig = self.signature();
        sig.iter().map(|sub| sub.join(", ")).join("; ")
    }
}
