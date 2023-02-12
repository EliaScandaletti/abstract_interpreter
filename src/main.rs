use std::{collections::BTreeMap, fmt::Display, fs};

use abstraction::{AIResult, AbsDomain};
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
    let inv = IntervalInterpreter::<-50, 2000>::execute(graph);
    println!("\n{:-<30}", "Output:");
    print_result(&prog, &inv);
}

fn print_result<S>(prog: &Stm, inv: &AIResult<S>)
where
    S: AbsDomain + Display,
{
    fn get_inv<'a, S>(stm: &Stm, inv: &'a BTreeMap<Id, S>) -> Option<&'a S>
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
        inv.get(id)
    }

    fn print_nice<S>(stm: &Stm, inv: &BTreeMap<Id, S>, i: usize) -> String
    where
        S: AbsDomain + Display,
    {
        let x = "";
        let ii = i + 4;
        let s = get_inv(stm, inv);
        match stm {
            Stm::AExp(_, aexp) => format!("[{}]\n{x:i$}{aexp}", s.unwrap()),
            Stm::BExp(_, bexp) => format!("[{}]\n{x:i$}{bexp}", s.unwrap()),
            Stm::Ass(_, var, aexp) => format!("[{}]\n{x:i$}{var} := {aexp}", s.unwrap()),
            Stm::Skip(_) => format!("[{}]\n{x:i$}skip", s.unwrap()),
            Stm::IfThenElse(_, g, stm1, stm2) => {
                format!(
                    "[{}]\n{x:i$}if {g} then\n{}\n{x:i$}else\n{}\n{x:i$}endif",
                    s.unwrap(),
                    print_nice(&stm1, inv, ii),
                    print_nice(&stm2, inv, ii),
                )
            }
            Stm::While(_, g, stm) => {
                format!(
                    "[{}]\n{x:i$}while {g} do\n{}\n{x:i$}done",
                    s.unwrap(),
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

    println!("{}", print_nice(prog, &inv.inv, 16))
}
