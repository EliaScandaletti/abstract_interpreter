use std::fs;

use control_flow_graph::Program;
use syntax::build_ast;

use crate::abstraction::{interval_interpreter::IntervalInterpreter, AbstractInterpreter};

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod abstraction;
mod control_flow_graph;
mod math;
pub mod syntax;

fn main() {
    let input = fs::read_to_string("src/test.while").unwrap();
    println!("{}", input);
    let ast = build_ast(&input);
    println!("{}", ast);
    let prg = Program::new(ast);
    println!("{:?}", prg);
    let inv = IntervalInterpreter::<-50, 2000>::execute(prg);
    println!("{:#?}", inv)
}
