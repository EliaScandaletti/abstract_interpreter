pub mod interval_interpreter;

use std::collections::{BTreeMap, VecDeque};

use crate::{
    abstraction::{AbsDomain, AbsValueDomain},
    control_flow_graph::{Command, ControlFlowGraph, Label},
    syntax::{AExp, BExp, Id},
};

#[derive(Debug)]
pub struct AIResult<AVD, AD>
where
    AVD: AbsValueDomain,
    AD: AbsDomain<AVD>,
{
    pub inv: BTreeMap<Id, AD::State>,
    pub last_inv: AD::State,
}

pub trait AbstractInterpreter<AVD, AD>
where
    AVD: AbsValueDomain,
    AD: AbsDomain<AVD> + Clone,
{
    fn value_domain(&self) -> &AVD;
    fn domain(&self) -> &AD;

    fn new(dom: AD) -> Self;

    fn aexp(&self, state: &mut AD::State, exp: &AExp) -> AVD::Value;

    fn bexp(&self, state: &mut AD::State, exp: &BExp) -> AD::State;

    fn evaluate(&self, mut state: AD::State, comm: &Command) -> AD::State {
        match comm {
            Command::AExp(aexp) => {
                self.aexp(&mut state, &aexp);
                state
            }
            Command::BExp(bexp) => {
                self.bexp(&mut state, &bexp);
                state
            }
            Command::Ass(var, aexp) => {
                let val = self.aexp(&mut state, aexp);
                self.domain().ass(&mut state, var, val);
                state
            }
            Command::Skip => state,
            Command::Guard(guard) => self.bexp(&mut state, guard),
        }
    }

    fn execute(&self, graph: ControlFlowGraph) -> AIResult<AVD, AD> {
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

        let mut invariants: BTreeMap<_, _> =
            labels.iter().map(|l| (*l, self.domain().bot())).collect();
        *invariants.get_mut(&entry_point).unwrap() = self.domain().top();

        let mut queue: VecDeque<_> = labels.iter().filter(|&&l| l != entry_point).collect();

        while let Some(n) = queue.pop_front() {
            let old = invariants.get(&n).unwrap();
            let new = bwd_dep
                .get(&n)
                .unwrap()
                .iter()
                .fold(self.domain().bot(), |s, p| {
                    let ps = invariants.get(p).unwrap();
                    let comm = arcs.get(&(*p, *n)).unwrap();
                    let ns = self.evaluate(ps.clone(), comm);
                    self.domain().lub(&s, &ns)
                });

            if *old != new {
                invariants.insert(n.clone(), new);
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
