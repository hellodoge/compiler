#![allow(dead_code)]

use std::collections::HashSet;

use crate::sm;

#[derive(Debug, Clone)]
pub(crate) enum Instr {
    BinOp(&'static str, Opnd, Opnd),
    Mov(Opnd, Opnd),
    Push(Opnd),
    Pop(Opnd),
    Div(Opnd),
    Set(&'static str, &'static str),
    Call(String),
    Ret,
}

const REGS: &'static [&'static str] = &["ebx", "ecx", "esi", "edi", "eax", "edx", "ebp", "esp"];
const OPERATE_REGS: usize = 4;
const WORD_SIZE: usize = 4;

const EBX: Opnd = Opnd::Register(0);
const ECX: Opnd = Opnd::Register(1);
const ESI: Opnd = Opnd::Register(2);
const EDI: Opnd = Opnd::Register(3);
const EAX: Opnd = Opnd::Register(4);
const EDX: Opnd = Opnd::Register(5);
const EBP: Opnd = Opnd::Register(6);
const ESP: Opnd = Opnd::Register(7);

#[derive(Debug, Clone)]
pub(crate) enum Opnd {
    Register(usize),
    Stack(usize),
    Variable(String),
    Const(i32),
}


#[derive(Default, Debug)]
struct Environment {
    slots: usize,
    stack: Vec<Opnd>,
    locals: HashSet<String>,
}

pub(crate) enum CompilationError {
    VariableNotDeclared(String),
    StackEmpty,
}

impl Environment {
    fn global(&mut self, v: String) {
        self.locals.insert(v);
    }

    fn var_declared(&self, s: &str) -> bool {
        self.locals.contains(s)
    }

    fn allocate(&mut self) -> Opnd {
        let (opnd, n) = match self.stack.last() {
            None => (Opnd::Register(0), 0),
            Some(Opnd::Stack(n)) => (Opnd::Stack(*n + 1), *n + 1),
            Some(Opnd::Register(n)) if *n < OPERATE_REGS =>
                (Opnd::Register(*n + 1), self.slots),
            _ => (Opnd::Stack(0), 1)
        };
        self.stack.push(opnd.clone());
        self.slots = std::cmp::max(self.slots, n);
        return opnd;
    }

    fn push(&mut self, opnd: Opnd) {
        self.stack.push(opnd);
    }

    fn pop(&mut self) -> Result<Opnd, CompilationError> {
        match self.stack.pop() {
            Some(x) => Ok(x),
            None => return Err(CompilationError::StackEmpty)
        }
    }

    fn new() -> Environment {
        Environment::default()
    }
}

fn compile(sm_code: Vec<sm::Instr>) -> Result<(Vec<Instr>, Environment), CompilationError> {

    fn compile_instr(instr: sm::Instr, env: &mut Environment) -> Result<Vec<Instr>, CompilationError> {
        Ok(match instr {
            sm::Instr::Push(x) => {
                let opnd = env.allocate();
                vec![Instr::Mov(Opnd::Const(x), opnd)]
            }
            sm::Instr::ST(s) => {
                let opnd = env.pop()?;
                env.global(s.clone());
                vec![Instr::Mov(opnd, Opnd::Variable(s))]
            }
            sm::Instr::LD(s) => {
                let opnd = env.allocate();
                if !env.var_declared(&s) {
                    return Err(CompilationError::VariableNotDeclared(s))
                }
                vec![Instr::Mov(Opnd::Variable(s), opnd)]
            }
            sm::Instr::Read => {
                let opnd = env.allocate();
                vec![Instr::Call("read_int".into()), Instr::Mov(EAX, opnd)]
            }
            sm::Instr::Write => {
                let opnd = env.pop()?;
                vec![Instr::Push(opnd), Instr::Call("write_int".into()),
                     Instr::BinOp("addl", Opnd::Const(WORD_SIZE as i32), ESP)]
            }
            sm::Instr::BinOp(op) => compile_binop(op, env)?
        })
    }

    use crate::expr::BinOp;

    fn compile_binop(op: BinOp, env: &mut Environment) -> Result<Vec<Instr>, CompilationError> {
        let r = env.pop()?;
        let l = env.pop()?;
        let result = env.allocate();
        Ok(match op {
            BinOp::Add | BinOp::Mul | BinOp::Sub => {
                let instr = match op {
                    BinOp::Add => "addl",
                    BinOp::Mul => "imull",
                    BinOp::Sub => "subl",
                    _ => unreachable!()
                };

                match (&l, &r) {
                    (&Opnd::Stack(_), &Opnd::Stack(_)) => {
                        vec![Instr::Mov(l, EAX), Instr::BinOp(instr, r, EAX), Instr::Mov(EAX, result)]
                    }
                    _ => vec![Instr::BinOp(instr, r, l.clone()), Instr::Mov(l, result)]
                }
            }
            BinOp::Div => vec![Instr::Mov(l, EAX), Instr::Mov(Opnd::Const(0), EDX),
                               Instr::Div(r), Instr::Mov(EAX, result)],
            BinOp::Mod => vec![Instr::Mov(l, EAX), Instr::Mov(Opnd::Const(0), EDX),
                               Instr::Div(r), Instr::Mov(EDX, result)],
            BinOp::CmpL | BinOp::CmpG | BinOp::CmpLe | BinOp::CmpGe | BinOp::CmpE | BinOp::CmpNe => {
                let suffix = match op {
                    BinOp::CmpL => "l",
                    BinOp::CmpG => "g",
                    BinOp::CmpLe => "le",
                    BinOp::CmpGe => "ge",
                    BinOp::CmpE => "e",
                    BinOp::CmpNe => "ne",
                    _ => unreachable!()
                };

                let mut instr = Vec::new();

                let l = match (&l, &r) {
                    (&Opnd::Stack(_), &Opnd::Stack(_)) => {
                        instr.push(Instr::Mov(l, EDX));
                        EDX
                    },
                    _ => l
                };

                instr.extend(vec![Instr::Mov(Opnd::Const(0), EAX), Instr::BinOp("cmp", r, l),
                                  Instr::Set(suffix, "al"), Instr::Mov(EAX, result)]);
                instr
            }
        })
    }

    let mut env = Environment::new();
    let mut program = Vec::new();

    for instr in sm_code {
        program.extend(compile_instr(instr, &mut env)?);
    }

    Ok((program, env))
}

pub(crate) fn compile_unit(sm_code: Vec<sm::Instr>) -> Result<(Vec<Instr>, Vec<String>), CompilationError> {
    use Instr::{Push, Mov, BinOp, Pop, Ret};

    let (code, env) = compile(sm_code)?;

    let code = vec![
        vec![Push(EBP), Mov(ESP, EBP), BinOp("subl", Opnd::Const(env.slots as i32), ESP)],
        code,
        vec![Mov(EBP, ESP), Pop(EBP), Mov(Opnd::Const(0), EAX), Ret]
    ].concat();

    Ok((code, env.locals.into_iter().collect()))
}

pub(crate) fn generate_asm(code: Vec<Instr>, vars: Vec<String>,
                           buffer: &mut impl std::io::Write)
                           -> std::io::Result<()> {

    write!(buffer, ".data\n")?;
    for var in vars.into_iter() {
        write!(buffer, "{}_global:\t.int\t0\n", var)?;
    }

    write!(buffer, ".text\n")?;
    write!(buffer, "\n.globl\tmain\n")?;
    write!(buffer, "main:\n")?;
    for instr in code.into_iter() {
        write!(buffer, "{}\n", instr)?;
    }

    Ok(())
}

impl std::fmt::Display for Opnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opnd::Register(i) => write!(f, "%{}", REGS.get(*i)
                .expect("Register out of bounds")),
            Opnd::Stack(i) => write!(f, "-{}(%ebp)", (i + 1) * WORD_SIZE),
            Opnd::Variable(x) => write!(f, "{}_global", x),
            Opnd::Const(i) => write!(f, "${}", i),
        }
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::BinOp(op, opnd1, opnd2) =>
                write!(f, "\t{}\t{},\t{}", op, opnd1, opnd2),
            Instr::Div(opnd) => write!(f, "\tidivl\t{}", opnd),
            Instr::Mov(opnd1, opnd2) => write!(f, "\tmovl\t{},\t{}", opnd1, opnd2),
            Instr::Push(opnd) => write!(f, "\tpushl\t{}", opnd),
            Instr::Pop(opnd) => write!(f, "\tpopl\t{}", opnd),
            Instr::Set(suf, register) => write!(f, "\tset{}\t%{}", suf, register),
            Instr::Call(func) => write!(f, "\tcall\t{}", func),
            Instr::Ret => write!(f, "\tret")
        }
    }
}