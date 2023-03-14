use std::{collections::BTreeMap, fmt::Display, fs};

use abstraction::{AbsDomain, AbsValueDomain};
use colored::Colorize;
use control_flow_graph::ControlFlowGraph;
use interpreter::AIResult;
use syntax::{build_ast, Stm};

use crate::{
    abstraction::interval::{IntervalValueDomain, Limit},
    interpreter::{
        interval_interpreter::{IntervalDomain, IntervalInterpreter},
        AbstractInterpreter,
    },
    syntax::Label,
};

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod abstraction;
mod control_flow_graph;
pub mod interpreter;
pub mod syntax;

fn main() {
    let input = fs::read_to_string("src/test.while").unwrap();
    println!("Paring input:\n{}", input);

    let prog = build_ast(&input);
    println!("\nParsed input:\n{}", prog);

    let graph = ControlFlowGraph::new(&prog);
    let vars = prog.get_vars();
    let nums = prog.get_numerals();

    let dlb = match nums.first() {
        Some(n) => Limit::Num(n.clone()),
        None => Limit::InfN,
    };
    let dub = match nums.last() {
        Some(n) => Limit::Num(n.clone()),
        None => Limit::InfP,
    };
    let v_dom = IntervalValueDomain::new(dlb, dub);
    let dom = IntervalDomain::new(v_dom, &vars);

    let ai = IntervalInterpreter::new(dom);
    let inv = ai.execute(graph);
    println!("\n{:-<30}", "Output:".blue());
    println!("{}", pretty_result(&prog, &inv))
}

fn pretty_result<AVD, AD>(prog: &Stm, inv: &AIResult<AVD, AD>) -> String
where
    AVD: AbsValueDomain,
    AD: AbsDomain<AVD>,
    AD::State: Display,
{
    fn get_inv<S>(stm: &Stm, inv: &BTreeMap<Label, S>) -> (Label, Option<S>)
    where
        S: Clone + Display,
    {
        let id = stm.id();
        (*id, inv.get(id).cloned())
    }

    fn print_nice<S>(stm: &Stm, inv: &BTreeMap<Label, S>, i: usize) -> String
    where
        S: Clone + Display,
    {
        let x = "";
        let ii = i + 4;
        let (id, s) = get_inv(stm, inv);
        let ss = match s {
            Some(s) => format!("{id}: {s}").purple(),
            None => "".into(),
        };
        match stm {
            Stm::AExp(_, _, aexp) => format!("{ss}\n{x:i$}{aexp}"),
            Stm::BExp(_, _, bexp) => format!("{ss}\n{x:i$}{bexp}"),
            Stm::Ass(_, _, var, aexp) => format!("{ss}\n{x:i$}{var} := {aexp}"),
            Stm::Skip(_, _) => format!("{ss}\n{x:i$}skip"),
            Stm::IfThenElse(_, _, g, stm1, stm2) => {
                format!(
                    "{ss}\n{x:i$}if {g} then\n{}\n{x:i$}else\n{}\n{x:i$}endif",
                    print_nice(&stm1, inv, ii),
                    print_nice(&stm2, inv, ii),
                )
            }
            Stm::While(_, _, g, stm) => {
                format!(
                    "{ss}\n{x:i$}while {g} do\n{}\n{x:i$}done",
                    print_nice(&stm, inv, ii),
                )
            }
            Stm::Comp(_, stm1, stm2) => {
                format!(
                    "{};\n{}",
                    print_nice(&stm1, inv, i),
                    print_nice(&stm2, inv, i)
                )
            }
        }
    }

    let body = print_nice(prog, &inv.inv, 8);
    format!("{body}\n{}", inv.last_inv.to_string().purple())
}
