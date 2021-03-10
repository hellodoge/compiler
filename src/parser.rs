use crate::expr::{Expr, Stmt, BinOp};

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct ProgramParser;

pub (crate) fn parse(unparsed: &str) -> Result<Stmt, pest::error::Error<Rule>> {
    let program = ProgramParser::parse(Rule::program, unparsed)?;

    fn parse_pairs(mut pairs: Pairs<Rule>) -> Stmt {
        let current = match pairs.next() {
            Some(x) => x,
            None => return Stmt::Skip
        };
        let current = parse_stmt(current);
        match pairs.peek() {
            Some(_) => Stmt::Seq(Box::new(current), Box::new(parse_pairs(pairs))),
            None => current
        }
    }

    Ok(parse_pairs(program))
}

fn parse_stmt(pair: Pair<Rule>) -> Stmt {
    match pair.as_rule() {
        Rule::read => Stmt::Read(pair.into_inner().next().unwrap().as_str().to_owned()),
        Rule::write => Stmt::Write(parse_expr(pair.into_inner().next().unwrap())),
        Rule::assign => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap();
            let expr = parse_expr(inner.next().unwrap());
            Stmt::Assign(name.as_str().into(), expr)
        }
        Rule::EOI => Stmt::Skip,
        _ => unreachable!()
    }
}

fn parse_expr(pair: Pair<Rule>) -> Expr {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::add, Assoc::Left) |
            Operator::new(Rule::sub, Assoc::Left),
        Operator::new(Rule::mul, Assoc::Left) |
            Operator::new(Rule::div, Assoc::Left) |
            Operator::new(Rule::mod_, Assoc::Left),
    ]);

    fn build_expr(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> Expr {
        let primary = |pair| build_expr(pair, climber);

        let binop = |lhs, op: Pair<Rule>, rhs| BinOp::expr(match op.as_rule() {
            Rule::add => BinOp::Add,
            Rule::sub => BinOp::Sub,
            Rule::mul => BinOp::Mul,
            Rule::div => BinOp::Div,
            Rule::mod_ => BinOp::Mod,
            _ => unreachable!()
        }, lhs, rhs);

        match pair.as_rule() {
            Rule::expr => {
                let pairs = pair.into_inner();
                climber.climb(pairs, primary, binop)
            }
            Rule::primary => pair.into_inner().next().map(primary).unwrap(),
            Rule::number => {
                let number = pair.as_str().parse().unwrap();
                Expr::Const(number)
            }
            Rule::ident => Expr::Var(pair.as_str().into()),
            _ => unreachable!()
        }
    }

    build_expr(pair, &climber)
}

