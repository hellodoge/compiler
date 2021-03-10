#[derive(Debug)]
pub(crate) enum Stmt {
    Skip,
    Read(String),
    Write(Expr),
    Assign(String, Expr),
    Seq(Box<Stmt>, Box<Stmt>),
}

#[derive(Debug)]
pub(crate) enum Expr {
    Const(i32),
    Var(String),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum BinOp {
    Add,
    Mul,
    Sub,
    Div,
    Mod,
}

impl BinOp {
    pub(crate) fn expr(b: BinOp, l: Expr, r: Expr) -> Expr {
        Expr::BinOp(b, Box::new(l), Box::new(r))
    }
}