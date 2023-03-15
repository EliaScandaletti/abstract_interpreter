use std::collections::{BTreeMap, BTreeSet};

use crate::syntax::{AExp, BExp, Label, Meta, Stm, Variable};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Command {
    Guard(BExp),
    AExp(AExp),
    BExp(BExp),
    Ass(Variable, AExp),
    Skip,
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    labels: BTreeSet<Label>,
    wid_pts: BTreeSet<Label>,
    entry_point: Label,
    exit_point: Label,
    arcs: BTreeMap<(Label, Label), Command>,
}

impl ControlFlowGraph {
    pub fn labels(&self) -> &BTreeSet<Label> {
        &self.labels
    }

    pub fn wid_pts(&self) -> &BTreeSet<Label> {
        &self.wid_pts
    }

    pub fn entry_point(&self) -> &Label {
        &self.entry_point
    }

    pub fn exit_point(&self) -> &Label {
        &self.exit_point
    }

    pub fn arcs(&self) -> &BTreeMap<(Label, Label), Command> {
        &self.arcs
    }

    pub fn new(ast: &Stm) -> Self {
        match ast {
            Stm::AExp(Meta { id, widening, .. }, _)
            | Stm::BExp(Meta { id, widening, .. }, _)
            | Stm::Ass(Meta { id, widening, .. }, _, _)
            | Stm::Skip(Meta { id, widening, .. }) => {
                let wid_pts = if *widening {
                    [*id].into()
                } else {
                    BTreeSet::new()
                };

                let entry_point = *id;
                let exit_point = id + 1;

                let labels = [entry_point, exit_point].into();

                let comm = match ast {
                    Stm::AExp(_, aexp) => Command::AExp(aexp.clone()),
                    Stm::BExp(_, bexp) => Command::BExp(bexp.clone()),
                    Stm::Ass(_, var, val) => Command::Ass(var.clone(), val.clone()),
                    Stm::Skip(_) => Command::Skip,
                    _ => unreachable!(),
                };
                let arcs = BTreeMap::from([((*id, exit_point), comm)]);

                Self {
                    labels,
                    wid_pts,
                    entry_point,
                    exit_point,
                    arcs,
                }
            }

            Stm::IfThenElse(Meta { id, widening, .. }, g, stm1, stm2) => {
                let g1 = Self::new(&stm1);
                let g2 = Self::new(&stm2);

                let wid_pts = if *widening { vec![*id] } else { vec![] }
                    .into_iter()
                    .chain(g1.wid_pts.into_iter())
                    .chain(g2.wid_pts.into_iter())
                    .collect();

                let entry_point = *id;
                let exit_point = g2.exit_point;

                let labels = [entry_point, exit_point]
                    .into_iter()
                    .chain(g1.labels.into_iter())
                    .chain(g2.labels.into_iter())
                    .collect();

                let gu = Command::Guard(g.clone());
                let ngu = Command::Guard(BExp::not(g.clone()));
                let arcs = [((*id, g1.entry_point), gu), ((*id, g2.entry_point), ngu)]
                    .into_iter()
                    .chain(g1.arcs.into_iter().map(|((f, mut t), c)| {
                        if t == g1.exit_point {
                            t = g2.exit_point
                        }
                        ((f, t), c)
                    }))
                    .chain(g2.arcs.into_iter())
                    .collect();

                Self {
                    labels,
                    wid_pts,
                    entry_point,
                    exit_point,
                    arcs,
                }
            }

            Stm::While(Meta { id, widening, .. }, g, stm) => {
                let gr = Self::new(&stm);

                let entry_point = *id;
                let exit_point = gr.exit_point;

                let labels = [entry_point, exit_point]
                    .into_iter()
                    .chain(gr.labels.into_iter())
                    .collect();

                let wid_pts = if *widening { vec![*id] } else { vec![] }
                    .into_iter()
                    .chain(gr.wid_pts.into_iter())
                    .collect();

                let gu = Command::Guard(g.clone());
                let ngu = Command::Guard(BExp::not(g.clone()));

                let arcs = [((*id, gr.entry_point), gu), ((*id, gr.exit_point), ngu)]
                    .into_iter()
                    .chain(gr.arcs.into_iter().map(|((f, mut t), c)| {
                        if t == gr.exit_point {
                            t = *id
                        }
                        ((f, t), c)
                    }))
                    .collect();

                Self {
                    labels,
                    wid_pts,
                    entry_point,
                    exit_point,
                    arcs,
                }
            }

            Stm::Comp(stm1, stm2) => {
                let g1 = Self::new(&stm1);
                let g2 = Self::new(&stm2);

                let entry_point = g1.entry_point;
                let exit_point = g2.exit_point;

                let labels = g1.labels.into_iter().chain(g2.labels.into_iter()).collect();

                let wid_pts = g1
                    .wid_pts
                    .into_iter()
                    .chain(g2.wid_pts.into_iter())
                    .collect();

                let arcs = g1
                    .arcs
                    .into_iter()
                    .map(|((f, mut t), c)| {
                        if t == g1.exit_point {
                            t = g2.entry_point
                        }
                        ((f, t), c)
                    })
                    .chain(g2.arcs.into_iter())
                    .collect();

                Self {
                    labels,
                    wid_pts,
                    entry_point,
                    exit_point,
                    arcs,
                }
            }
        }
    }
}
