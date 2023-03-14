use std::{collections::BTreeSet, fmt::Display};

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
        .op(Op::prefix(wid))
    };
}

pub type Numeral = i32;

pub type Variable = String;

pub type Id = usize;
fn get_id() -> Id {
    static mut ID: Id = 1;
    unsafe {
        let ret = ID;
        ID += 1;
        ret
    }
}

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

impl AExp {
    fn vars(&self) -> BTreeSet<Variable> {
        match self {
            AExp::Num(_) => BTreeSet::new(),
            AExp::Var(x)
            | AExp::PreInc(x)
            | AExp::PreDec(x)
            | AExp::PostInc(x)
            | AExp::PostDec(x) => {
                let mut ret = BTreeSet::new();
                ret.insert(x.clone());
                ret
            }
            AExp::Add(e1, e2) | AExp::Sub(e1, e2) | AExp::Mul(e1, e2) | AExp::Div(e1, e2) => {
                let it1 = e1.vars().into_iter();
                let it2 = e2.vars().into_iter();
                it1.chain(it2).collect()
            }
            AExp::Neg(e) => e.vars(),
        }
    }

    fn numerals(&self) -> BTreeSet<Numeral> {
        match self {
            AExp::Num(n) => {
                let mut ret = BTreeSet::new();
                ret.insert(n.clone());
                ret
            }
            AExp::Var(_)
            | AExp::PreInc(_)
            | AExp::PreDec(_)
            | AExp::PostInc(_)
            | AExp::PostDec(_) => BTreeSet::new(),
            AExp::Add(e1, e2) | AExp::Sub(e1, e2) | AExp::Mul(e1, e2) | AExp::Div(e1, e2) => {
                let it1 = e1.numerals().into_iter();
                let it2 = e2.numerals().into_iter();
                it1.chain(it2).collect()
            }
            AExp::Neg(e) => e.numerals(),
        }
    }
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

    fn vars(&self) -> BTreeSet<Variable> {
        match self {
            BExp::True | BExp::False => BTreeSet::new(),
            BExp::Eq(e1, e2) | BExp::Neq(e1, e2) | BExp::Lt(e1, e2) => {
                let it1 = e1.vars().into_iter();
                let it2 = e2.vars().into_iter();
                it1.chain(it2).collect()
            }
            BExp::And(e1, e2) | BExp::Or(e1, e2) => {
                let it1 = e1.vars().into_iter();
                let it2 = e2.vars().into_iter();
                it1.chain(it2).collect()
            }
            BExp::Not(e) => e.vars(),
        }
    }

    fn numerals(&self) -> BTreeSet<Numeral> {
        match self {
            BExp::True | BExp::False => BTreeSet::new(),
            BExp::Eq(e1, e2) | BExp::Neq(e1, e2) | BExp::Lt(e1, e2) => {
                let it1 = e1.numerals().into_iter();
                let it2 = e2.numerals().into_iter();
                it1.chain(it2).collect()
            }
            BExp::And(e1, e2) | BExp::Or(e1, e2) => {
                let it1 = e1.numerals().into_iter();
                let it2 = e2.numerals().into_iter();
                it1.chain(it2).collect()
            }
            BExp::Not(e) => e.numerals(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stm {
    AExp(Id, bool, AExp),
    BExp(Id, bool, BExp),
    Ass(Id, bool, Variable, AExp),
    Skip(Id, bool),
    IfThenElse(Id, bool, BExp, Box<Stm>, Box<Stm>),
    While(Id, bool, BExp, Box<Stm>),
    Comp(bool, Box<Stm>, Box<Stm>),
}

impl Stm {
    pub fn id(&self) -> &Id {
        match self {
            Stm::AExp(id, _, _)
            | Stm::BExp(id, _, _)
            | Stm::Ass(id, _, _, _)
            | Stm::Skip(id, _)
            | Stm::IfThenElse(id, _, _, _, _)
            | Stm::While(id, _, _, _) => id,
            Stm::Comp(_, s1, _) => s1.id(),
        }
    }

    pub fn next_id(&self) -> Id {
        match self {
            Stm::AExp(id, _, _)
            | Stm::BExp(id, _, _)
            | Stm::Ass(id, _, _, _)
            | Stm::Skip(id, _) => id + 1,
            Stm::IfThenElse(_, _, _, _, s) | Stm::While(_, _, _, s) | Stm::Comp(_, _, s) => {
                s.id() + 1
            }
        }
    }

    pub fn get_vars(&self) -> BTreeSet<Variable> {
        match self {
            Stm::AExp(_, _, e) => e.vars(),
            Stm::BExp(_, _, e) => e.vars(),
            Stm::Ass(_, _, x, e) => {
                let mut ret = e.vars();
                ret.insert(x.clone());
                ret
            }
            Stm::Skip(_, _) => BTreeSet::new(),
            Stm::IfThenElse(_, _, g, s1, s2) => {
                let it1 = g.vars().into_iter();
                let it2 = s1.get_vars().into_iter();
                let it3 = s2.get_vars().into_iter();
                it1.chain(it2).chain(it3).collect()
            }
            Stm::While(_, _, g, s) => {
                let it1 = g.vars().into_iter();
                let it2 = s.get_vars().into_iter();
                it1.chain(it2).collect()
            }
            Stm::Comp(_, s1, s2) => {
                let it1 = s1.get_vars().into_iter();
                let it2 = s2.get_vars().into_iter();
                it1.chain(it2).collect()
            }
        }
    }

    pub fn get_numerals(&self) -> BTreeSet<Numeral> {
        match self {
            Stm::AExp(_, _, e) | Stm::Ass(_, _, _, e) => e.numerals(),
            Stm::BExp(_, _, e) => e.numerals(),
            Stm::Skip(_, _) => BTreeSet::new(),
            Stm::IfThenElse(_, _, g, s1, s2) => {
                let it1 = g.numerals().into_iter();
                let it2 = s1.get_numerals().into_iter();
                let it3 = s2.get_numerals().into_iter();
                it1.chain(it2).chain(it3).collect()
            }
            Stm::While(_, _, g, s) => {
                let it1 = g.numerals().into_iter();
                let it2 = s.get_numerals().into_iter();
                it1.chain(it2).collect()
            }
            Stm::Comp(_, s1, s2) => {
                let it1 = s1.get_numerals().into_iter();
                let it2 = s2.get_numerals().into_iter();
                it1.chain(it2).collect()
            }
        }
    }
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
        .map_primary(|pair| {
            let id = get_id();
            match pair.as_rule() {
                Rule::stm => stm_ast(pair.into_inner()),
                Rule::aexp => Stm::AExp(id, false, aexp_ast(pair.into_inner())),
                Rule::bexp => Stm::BExp(id, false, bexp_ast(pair.into_inner())),
                Rule::skip => Stm::Skip(id, false),
                Rule::ass => {
                    let mut it = pair.into_inner();
                    let var = it.next().unwrap();
                    let exp = it.next().unwrap();
                    Stm::Ass(id, false, var.as_str().into(), aexp_ast(exp.into_inner()))
                }
                Rule::ifelse => {
                    let mut it = pair.into_inner();
                    let cond = it.next().unwrap();
                    let if_ = it.next().unwrap();
                    let else_ = it.next().unwrap();
                    Stm::IfThenElse(
                        id,
                        false,
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
                        id,
                        false,
                        bexp_ast(cond.into_inner()),
                        stm_ast(body.into_inner()).into(),
                    )
                }
                _ => unreachable!("{:?}", pair),
            }
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::comp => Stm::Comp(false, lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::wid => match rhs {
                Stm::AExp(id, _, e) => Stm::AExp(id, true, e),
                Stm::BExp(id, _, e) => Stm::BExp(id, true, e),
                Stm::Ass(id, _, x, e) => Stm::Ass(id, true, x, e),
                Stm::Skip(id, _) => Stm::Skip(id, true),
                Stm::IfThenElse(id, _, g, e1, e2) => Stm::IfThenElse(id, true, g, e1, e2),
                Stm::While(id, _, g, e) => Stm::While(id, true, g, e),
                Stm::Comp(_, s1, s2) => Stm::Comp(true, s1, s2),
            },
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
        Stm::AExp(_, _, aexp) => format!("{x:i$}{aexp}"),
        Stm::BExp(_, _, bexp) => format!("{x:i$}{bexp}"),
        Stm::Ass(_, _, var, aexp) => format!("{x:i$}{var} := {aexp}"),
        Stm::Skip(_, _) => format!("{x:i$}skip"),
        Stm::IfThenElse(_, _, g, stm1, stm2) => {
            format!(
                "{x:i$}if {g} then\n{}\n{x:i$}else\n{}\n{x:i$}endif",
                fmt(&stm1, ii),
                fmt(&stm2, ii)
            )
        }
        Stm::While(_, _, g, stm) => format!("{x:i$}while {g} do\n{}\n{x:i$}done", fmt(&stm, ii)),
        Stm::Comp(_, stm1, stm2) => format!("{};\n{}", fmt(&stm1, i), fmt(&stm2, i)),
    }
}

impl Display for Stm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", fmt(self, 0))
    }
}
