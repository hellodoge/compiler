use crate::x86;
use crate::sm;
use crate::parser;


pub(crate) fn compile_x86(path: std::path::PathBuf, out: std::path::PathBuf) -> Result<(), String> {
    let unparsed = match std::fs::read_to_string(path) {
        Ok(x) => x,
        Err(e) => return Err(e.to_string())
    };
    let parsed = match parser::parse(&unparsed) {
        Ok(x) => x,
        Err(e) => return Err(e.to_string())
    };
    let sm_code = sm::compile(parsed);
    let (compiled, vars) = match x86::compile_unit(sm_code) {
        Ok((c, v)) => (c, v),
        Err(e) => return Err(e.to_string())
    };
    let mut outfile = match std::fs::OpenOptions::new().write(true).create(true).open(out) {
        Ok(x) => x,
        Err(e) => return Err(e.to_string())
    };
    match x86::generate_asm(compiled, vars, &mut outfile) {
        Ok(_) => Ok(()),
        Err(e) => return Err(e.to_string())
    }
}

impl std::fmt::Display for x86::CompilationError {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {

        use crate::x86::CompilationError;

        match self {
            CompilationError::VariableNotDeclared(x) =>
                write!(f, "Variable {} might be not declared", x),
            _ => write!(f, "Internal error, please send bug report"),
        }
    }
}