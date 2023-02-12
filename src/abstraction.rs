pub mod interval;
pub mod interval_interpreter;
pub mod nor_rel_domain;

use std::{
    collections::{BTreeMap, VecDeque},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{
    control_flow_graph::{Command, ControlFlowGraph, Label},
    syntax::{AExp, BExp, Id, Variable},
};

pub trait AbsValue:
    PartialOrd
    + Neg<Output = Self>
    + Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + Sized
{
    fn bot() -> Self;
    fn top() -> Self;
    fn lub(rhs: &Self, lhs: &Self) -> Self;
    fn glb(rhs: &Self, lhs: &Self) -> Self;
    // fn widening(rhs: &Self, lhs: &Self) -> Self;
    // fn narrowing(rhs: &Self, lhs: &Self) -> Self;
    // fn gamma<T>(val: &Self) -> T;
    // fn alpha<T>(val: T) -> Self;
}

pub trait AbsDomain: PartialEq {
    type Value: AbsValue + Clone;

    fn ass(&mut self, var: &Variable, val: Self::Value);
    fn get_var(&self, var: &Variable) -> Self::Value;

    fn bot() -> Self;
    fn top() -> Self;
    fn lub(rhs: &Self, lhs: &Self) -> Self;
    fn glb(rhs: &Self, lhs: &Self) -> Self;
    // fn widening(rhs: &Self, lhs: &Self) -> Self;
    // fn narrowing(rhs: &Self, lhs: &Self) -> Self;
    // fn gamma<T>(val: &Self) -> T;
    // fn alpha<T>(val: T) -> Self;
}

#[derive(Debug)]
pub struct AIResult<S>
where
    S: AbsDomain,
{
    pub inv: BTreeMap<Id, S>,
    pub last_inv: S,
}

pub trait AbstractInterpreter<S>
where
    S: AbsDomain + Clone,
{
    fn aexp(state: &mut S, exp: &AExp) -> S::Value;

    fn bexp(state: &mut S, exp: &BExp) -> S;

    fn evaluate(mut state: S, comm: &Command) -> S {
        match comm {
            Command::AExp(aexp) => {
                Self::aexp(&mut state, &aexp);
                state
            }
            Command::BExp(bexp) => {
                Self::bexp(&mut state, &bexp);
                state
            }
            Command::Ass(var, aexp) => {
                let val = Self::aexp(&mut state, aexp);
                state.ass(var, val);
                state
            }
            Command::Skip => state,
            Command::Guard(guard) => Self::bexp(&mut state, guard),
        }
    }

    fn execute(graph: ControlFlowGraph) -> AIResult<S> {
        let arcs = graph.arcs();
        let labels = graph.labels();
        let entry_point = graph.entry_point();
        let exit_point = graph.exit_point();

        let fwd_dep: BTreeMap<Label, Vec<Label>> = labels
            .iter()
            .map(|l| {
                let dep = arcs
                    .iter()
                    .filter(|((f, _), _)| f == l)
                    .map(|((_, t), _)| *t)
                    .collect();
                (*l, dep)
            })
            .collect();
        let bwd_dep: BTreeMap<Label, Vec<Label>> = labels
            .iter()
            .map(|l| {
                let dep = arcs
                    .iter()
                    .filter(|((_, t), _)| t == l)
                    .map(|((f, _), _)| *f)
                    .collect();
                (*l, dep)
            })
            .collect();

        let mut queue = VecDeque::from([entry_point]);

        let mut invariants: BTreeMap<_, _> = labels.iter().map(|l| (*l, S::bot())).collect();

        while let Some(n) = queue.pop_front() {
            let old = invariants.get(&n).unwrap();
            let new = bwd_dep.get(&n).unwrap().iter().fold(S::top(), |s, p| {
                let ps = invariants.get(p).unwrap();
                let comm = arcs.get(&(*p, n)).unwrap();
                let ns = Self::evaluate(ps.clone(), comm);
                S::lub(&s, &ns)
            });

            if *old != new {
                invariants.insert(n, new);
                match fwd_dep.get(&n) {
                    Some(vs) => queue.extend(vs),
                    None => (),
                }
            }
        }

        let map = graph.label_to_id();
        let last_inv = invariants.get(&exit_point).unwrap().clone();
        let inv = invariants
            .into_iter()
            .filter(|(l, _)| *l != exit_point)
            .map(|(l, s)| (*map.get(&l).unwrap(), s))
            .collect();

        AIResult { inv, last_inv }
    }
}
