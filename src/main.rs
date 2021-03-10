extern crate pest;
#[macro_use]
extern crate pest_derive;

mod expr;
mod sm;
mod x86;
mod driver;
mod parser;

fn main() {
    let mut args = std::env::args();
    let path = args.next().expect("Compiler path not found");
    let source = match args.next() {
        Some(x) if args.next().is_none() => x,
        _ => {
            println!("usage: {} [source file]", path);
            return
        }
    };

    use std::path::PathBuf;

    match driver::compile_x86(PathBuf::from(source), PathBuf::from("out.s")) {
        Ok(_) => (),
        Err(e) => println!("error! {}", e)
    }
}
