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
        Some(n) => Limit::Num(n.clone() - 1),
        None => Limit::InfN,
    };
    let dub = match nums.last() {
        Some(n) => Limit::Num(n.clone() + 1),
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
    let AIResult { inv, exit_point } = inv;

    fn pretty_inv<S>(id: &Label, w: bool, inv: &BTreeMap<Label, S>) -> String
    where
        S: Clone + Display,
    {
        let s = inv.get(id).unwrap().clone();
        format!("{id}{}: {s}", if w { "w" } else { " " })
            .purple()
            .to_string()
    }

    fn print_nice<S>(stm: &Stm, inv: &BTreeMap<Label, S>, i: usize) -> String
    where
        S: Clone + Display,
    {
        let x = "";
        let ii = i + 4;
        let ss = pretty_inv(stm.id(), stm.widening(), inv);
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
            Stm::Comp(stm1, stm2) => {
                format!(
                    "{};\n{}",
                    print_nice(&stm1, inv, i),
                    print_nice(&stm2, inv, i)
                )
            }
        }
    }

    let i = 8;
    let body = print_nice(prog, inv, i);
    format!("{body}\n{}", pretty_inv(exit_point, false, inv))
}
