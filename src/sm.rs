use crate::expr::{Stmt, Expr, BinOp};

#[derive(Clone, Debug)]
pub(crate) enum Instr {
    Push(i32),
    BinOp(BinOp),
    Read,
    Write,
    LD(String),
    ST(String),
}

fn compile_expr(expr: Expr) -> Vec<Instr> {
    match expr {
        Expr::Const(x) => vec![Instr::Push(x)],
        Expr::Var(n) => vec![Instr::LD(n)],
        Expr::BinOp(b, l, r) => {
            [compile_expr(*l), compile_expr(*r), vec![Instr::BinOp(b)]].concat()
        }
    }
}

pub(crate) fn compile(stmt: Stmt) -> Vec<Instr> {
    match stmt {
        Stmt::Skip => vec![],
        Stmt::Read(x) => vec![Instr::Read, Instr::ST(x)],
        Stmt::Write(x) => vec![compile_expr(x), vec![Instr::Write]].concat(),
        Stmt::Assign(n, x) => vec![compile_expr(x), vec![Instr::ST(n)]].concat(),
        Stmt::Seq(r, l) => vec![compile(*r), compile(*l)].concat(),
    }
}