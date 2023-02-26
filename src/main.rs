use std::{collections::BTreeMap, fmt::Display, fs};

use abstraction::{AIResult, AbsDomain};
use colored::Colorize;
use control_flow_graph::ControlFlowGraph;
use syntax::{build_ast, Stm};

use crate::{
    abstraction::{interval_interpreter::IntervalInterpreter, AbstractInterpreter},
    syntax::Id,
};

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod abstraction;
mod control_flow_graph;
pub mod syntax;

fn main() {
    let input = fs::read_to_string("src/test.while").unwrap();
    println!("Paring input:\n{}", input);
    let prog = build_ast(&input);
    println!("\nParsed input:\n{}", prog);
    let graph = ControlFlowGraph::new(&prog);
    let inv = IntervalInterpreter::execute(graph);
    println!("\n{:-<30}", "Output:".blue());
    println!("{}", pretty_result(&prog, &inv))
}

fn pretty_result<S>(prog: &Stm, inv: &AIResult<S>) -> String
where
    S: AbsDomain + Display,
{
    fn get_inv<'a, S>(stm: &Stm, inv: &'a BTreeMap<Id, S>) -> (Id, Option<&'a S>)
    where
        S: AbsDomain + Display,
    {
        let id = match stm {
            Stm::AExp(id, _) => id,
            Stm::BExp(id, _) => id,
            Stm::Ass(id, _, _) => id,
            Stm::Skip(id) => id,
            Stm::IfThenElse(id, _, _, _) => id,
            Stm::While(id, _, _) => id,
            Stm::Comp(id, _, _) => id,
        };
        (*id, inv.get(id))
    }

    fn print_nice<S>(stm: &Stm, inv: &BTreeMap<Id, S>, i: usize) -> String
    where
        S: AbsDomain + Display,
    {
        let x = "";
        let ii = i + 4;
        let (id, s) = get_inv(stm, inv);
        let ss = match s {
            Some(s) => format!("{id}: {s}").purple(),
            None => "".into(),
        };
        match stm {
            Stm::AExp(_, aexp) => format!("{ss}\n{x:i$}{aexp}"),
            Stm::BExp(_, bexp) => format!("{ss}\n{x:i$}{bexp}"),
            Stm::Ass(_, var, aexp) => format!("{ss}\n{x:i$}{var} := {aexp}"),
            Stm::Skip(_) => format!("{ss}\n{x:i$}skip"),
            Stm::IfThenElse(_, g, stm1, stm2) => {
                format!(
                    "{ss}\n{x:i$}if {g} then\n{}\n{x:i$}else\n{}\n{x:i$}endif",
                    print_nice(&stm1, inv, ii),
                    print_nice(&stm2, inv, ii),
                )
            }
            Stm::While(_, g, stm) => {
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
