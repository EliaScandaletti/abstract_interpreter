use std::fmt::Display;

use pest::{iterators::Pairs, pratt_parser::PrattParser, Parser};

#[derive(Parser)]
#[grammar = "syntax/while.pest"]
pub struct WParser;

lazy_static::lazy_static! {
    static ref ARITH_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::infix(add, Left) | Op::infix(sub, Left))
        .op(Op::infix(mul, Left) | Op::infix(div, Left))
        .op(Op::prefix(neg))
    };
}

lazy_static::lazy_static! {
    static ref BOOL_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::infix(and, Left))
        .op(Op::infix(or, Left))
        .op(Op::prefix(not))
    };
}

lazy_static::lazy_static! {
    static ref STM_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::infix(comp, Left))
    };
}

pub type Numeral = i32;

pub type Variable = String;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum AExp {
    Num(Numeral),
    Var(Variable),
    Add(Box<AExp>, Box<AExp>),
    Sub(Box<AExp>, Box<AExp>),
    Mul(Box<AExp>, Box<AExp>),
    Div(Box<AExp>, Box<AExp>),
    Neg(Box<AExp>),
    PreInc(Variable),
    PreDec(Variable),
    PostInc(Variable),
    PostDec(Variable),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BExp {
    True,
    False,
    Eq(AExp, AExp),
    Neq(AExp, AExp),
    Lt(AExp, AExp),
    And(Box<BExp>, Box<BExp>),
    Or(Box<BExp>, Box<BExp>),
    Not(Box<BExp>),
}

impl BExp {
    pub fn not(e: BExp) -> Self {
        match e {
            BExp::Not(ne) => *ne,
            _ => BExp::Not(e.into()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stm {
    AExp(AExp),
    BExp(BExp),
    Ass(Variable, AExp),
    Skip,
    IfThenElse(BExp, Box<Stm>, Box<Stm>),
    While(BExp, Box<Stm>),
    Comp(Box<Stm>, Box<Stm>),
}

fn aexp_ast(pairs: Pairs<Rule>) -> AExp {
    ARITH_PARSER
        .map_primary(|pair| match pair.as_rule() {
            Rule::num => AExp::Num(pair.as_str().parse().unwrap()),
            Rule::var => AExp::Var(pair.as_str().into()),
            Rule::aexp => aexp_ast(pair.into_inner()),
            Rule::pre_inc => {
                let var = pair.into_inner().next().unwrap();
                AExp::PreInc(var.as_str().into())
            }
            Rule::pre_dec => {
                let var = pair.into_inner().next().unwrap();
                AExp::PreDec(var.as_str().into())
            }
            Rule::post_inc => {
                let var = pair.into_inner().next().unwrap();
                AExp::PostInc(var.as_str().into())
            }
            Rule::post_dec => {
                let var = pair.into_inner().next().unwrap();
                AExp::PostDec(var.as_str().into())
            }
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => AExp::Add(lhs.into(), rhs.into()),
            Rule::sub => AExp::Sub(lhs.into(), rhs.into()),
            Rule::mul => AExp::Mul(lhs.into(), rhs.into()),
            Rule::div => AExp::Div(lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => AExp::Neg(rhs.into()),
            _ => unreachable!(),
        })
        .parse(pairs)
}

fn bexp_ast(pairs: Pairs<Rule>) -> BExp {
    BOOL_PARSER
        .map_primary(|pair| match pair.as_rule() {
            Rule::tt => BExp::True,
            Rule::ff => BExp::False,
            Rule::eq => {
                let mut it = pair.into_inner();
                let lhs = it.next().unwrap();
                let rhs = it.next().unwrap();
                BExp::Eq(aexp_ast(lhs.into_inner()), aexp_ast(rhs.into_inner()))
            }
            Rule::neq => {
                let mut it = pair.into_inner();
                let lhs = it.next().unwrap();
                let rhs = it.next().unwrap();
                BExp::Neq(aexp_ast(lhs.into_inner()), aexp_ast(rhs.into_inner()))
            }
            Rule::le => {
                let mut it = pair.into_inner();
                let lhs = it.next().unwrap();
                let rhs = it.next().unwrap();
                BExp::Lt(aexp_ast(lhs.into_inner()), aexp_ast(rhs.into_inner()))
            }
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::and => BExp::And(lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::not => BExp::Not(rhs.into()),
            _ => unreachable!(),
        })
        .parse(pairs)
}

fn stm_ast(pairs: Pairs<Rule>) -> Stm {
    STM_PARSER
        .map_primary(|pair| match pair.as_rule() {
            Rule::stm => stm_ast(pair.into_inner()),
            Rule::aexp => Stm::AExp(aexp_ast(pair.into_inner())),
            Rule::bexp => Stm::BExp(bexp_ast(pair.into_inner())),
            Rule::skip => Stm::Skip,
            Rule::ass => {
                let mut it = pair.into_inner();
                let var = it.next().unwrap();
                let exp = it.next().unwrap();
                Stm::Ass(var.as_str().into(), aexp_ast(exp.into_inner()))
            }
            Rule::ifelse => {
                let mut it = pair.into_inner();
                let cond = it.next().unwrap();
                let if_ = it.next().unwrap();
                let else_ = it.next().unwrap();
                Stm::IfThenElse(
                    bexp_ast(cond.into_inner()),
                    stm_ast(if_.into_inner()).into(),
                    stm_ast(else_.into_inner()).into(),
                )
            }
            Rule::whiledo => {
                let mut it = pair.into_inner();
                let cond = it.next().unwrap();
                let body = it.next().unwrap();
                Stm::While(
                    bexp_ast(cond.into_inner()),
                    stm_ast(body.into_inner()).into(),
                )
            }
            _ => unreachable!("{:?}", pair),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::comp => Stm::Comp(lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .parse(pairs)
}

pub fn build_ast(input: &str) -> Stm {
    let mut progr_pair = WParser::parse(Rule::program, input).expect("Syntax error: ");
    let pairs = progr_pair.next().unwrap().into_inner();
    stm_ast(pairs)
}

impl Display for AExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AExp::Num(n) => write!(f, "{n}"),
            AExp::Var(v) => write!(f, "{v}"),
            AExp::Add(aexp1, aexp2) => write!(f, "({aexp1} + {aexp2})"),
            AExp::Sub(aexp1, aexp2) => write!(f, "({aexp1} - {aexp2})"),
            AExp::Mul(aexp1, aexp2) => write!(f, "({aexp1} * {aexp2})"),
            AExp::Div(aexp1, aexp2) => write!(f, "({aexp1} / {aexp2})"),
            AExp::Neg(aexp) => write!(f, "-({aexp})"),
            AExp::PreInc(var) => write!(f, "++{var}"),
            AExp::PreDec(var) => write!(f, "--{var}"),
            AExp::PostInc(var) => write!(f, "{var}++"),
            AExp::PostDec(var) => write!(f, "{var}--"),
        }
    }
}

impl Display for BExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BExp::True => write!(f, "true"),
            BExp::False => write!(f, "false"),
            BExp::Eq(aexp1, aexp2) => write!(f, "({aexp1} = {aexp2})"),
            BExp::Neq(aexp1, aexp2) => write!(f, "({aexp1} != {aexp2})"),
            BExp::Lt(aexp1, aexp2) => write!(f, "({aexp1} < {aexp2})"),
            BExp::And(bexp1, bexp2) => write!(f, "({bexp1} && {bexp2})"),
            BExp::Or(bexp1, bexp2) => write!(f, "({bexp1} || {bexp2})"),
            BExp::Not(bexp) => write!(f, "!({bexp})"),
        }
    }
}

fn fmt(stm: &Stm, i: usize) -> String {
    let x = "";
    let ii = i + 4;
    match stm {
        Stm::AExp(aexp) => format!("{x:i$}{aexp}"),
        Stm::BExp(bexp) => format!("{x:i$}{bexp}"),
        Stm::Ass(var, aexp) => format!("{x:i$}{var} := {aexp}"),
        Stm::Skip => format!("{x:i$}skip"),
        Stm::IfThenElse(g, stm1, stm2) => {
            format!(
                "{x:i$}if {g} then\n{}\n{x:i$}else\n{}\n{x:i$}endif",
                fmt(&*stm1, ii),
                fmt(&*stm2, ii)
            )
        }
        Stm::While(g, stm) => format!("{x:i$}while {g} do\n{}\n{x:i$}done", fmt(&*stm, ii)),
        Stm::Comp(stm1, stm2) => format!("{};\n{}", fmt(&*stm1, i), fmt(&*stm2, i)),
    }
}

impl Display for Stm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", fmt(self, 0))
    }
}
