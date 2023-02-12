use std::collections::{BTreeMap, BTreeSet};

use crate::syntax::{AExp, BExp, Stm, Variable};

pub type Label = u32;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Command {
    Guard(BExp),
    AExp(AExp),
    BExp(BExp),
    Ass(Variable, AExp),
    Skip,
}

struct ProgramBuilder {
    labels: BTreeSet<Label>,
    entry_point: Option<Label>,
    exit_point: Option<Label>,
    arcs: BTreeMap<(Label, Label), Command>,
    next_label: Label,
}

impl ProgramBuilder {
    fn new(next_label: Label) -> Self {
        Self {
            labels: BTreeSet::new(),
            entry_point: None,
            exit_point: None,
            arcs: BTreeMap::new(),
            next_label,
        }
    }

    fn add_arc(&mut self, from: Label, com: Command, to: Label) -> &mut Self {
        self.labels.insert(from);
        self.labels.insert(to);
        self.arcs.insert((from, to), com);
        self
    }

    fn add_subgraph(
        &mut self,
        entry_point: Label,
        mut sub: Program,
        exit_point: Label,
    ) -> &mut Self {
        self.next_label = sub.refresh_labels(self.next_label);
        sub.rename_label(sub.entry_point, entry_point);
        sub.rename_label(sub.exit_point, exit_point);

        self.labels.extend(&sub.labels);
        self.arcs.extend(sub.arcs);
        self
    }

    fn set_entry(&mut self, entry_point: Label) -> &mut Self {
        self.entry_point = Some(entry_point);
        self
    }

    fn set_exit(&mut self, exit_point: Label) -> &mut Self {
        self.exit_point = Some(exit_point);
        self
    }

    fn finalize(&self) -> Program {
        Program {
            labels: self.labels.clone(),
            entry_point: self.entry_point.unwrap(),
            exit_point: self.exit_point.unwrap(),
            arcs: self.arcs.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    labels: BTreeSet<Label>,
    entry_point: Label,
    exit_point: Label,
    arcs: BTreeMap<(Label, Label), Command>,
}

impl Program {
    pub fn labels(&self) -> BTreeSet<Label> {
        self.labels.clone()
    }

    pub fn entry_point(&self) -> Label {
        self.entry_point
    }

    pub fn exit_point(&self) -> Label {
        self.exit_point
    }

    pub fn arcs(&self) -> BTreeMap<(Label, Label), Command> {
        self.arcs.clone()
    }

    fn refresh_labels(&mut self, min: Label) -> Label {
        let mut max = 0;
        self.labels = self
            .labels
            .clone()
            .into_iter()
            .map(|l| {
                max = std::cmp::max(max, l);
                l + min
            })
            .collect();
        self.entry_point += min;
        self.exit_point += min;
        self.arcs = self
            .clone()
            .arcs
            .into_iter()
            .map(|((a, b), c)| ((a + min, b + min), c))
            .collect();
        max + min
    }

    fn rename_label(&mut self, from: Label, to: Label) {
        self.labels = self
            .clone()
            .labels
            .into_iter()
            .map(|l| if l == from { to } else { l })
            .collect();

        if self.entry_point == from {
            self.entry_point = to
        };
        if self.exit_point == from {
            self.exit_point = to
        };

        self.arcs = self
            .arcs
            .iter_mut()
            .map(|((mut a, mut b), c)| {
                if a == from {
                    a = to
                }
                if b == from {
                    b = to
                }
                ((a, b), c.clone())
            })
            .collect();
    }

    fn prettify(&mut self) -> &mut Self {
        let mut map = BTreeMap::<Label, Label>::new();

        map.insert(self.entry_point, 0);
        map.insert(self.exit_point, (self.labels.len() - 1) as Label);
        self.labels.iter().for_each(|l| {
            if !map.contains_key(l) {
                map.insert(*l, (map.len() - 1) as Label);
            }
        });

        self.labels = self
            .clone()
            .labels
            .into_iter()
            .map(|l| *map.get(&l).unwrap())
            .collect();
        self.entry_point = *map.get(&self.entry_point).unwrap();
        self.exit_point = *map.get(&self.exit_point).unwrap();
        self.arcs = self
            .clone()
            .arcs
            .into_iter()
            .map(|((f, t), c)| ((*map.get(&f).unwrap(), *map.get(&t).unwrap()), c))
            .collect();
        self
    }

    pub fn new(ast: Stm) -> Self {
        match ast {
            Stm::AExp(_) | Stm::BExp(_) | Stm::Ass(_, _) | Stm::Skip => {
                let comm = match ast {
                    Stm::AExp(aexp) => Command::AExp(aexp),
                    Stm::BExp(bexp) => Command::BExp(bexp),
                    Stm::Ass(var, val) => Command::Ass(var, val),
                    Stm::Skip => Command::Skip,
                    _ => unreachable!(),
                };
                ProgramBuilder::new(2)
                    .set_entry(0)
                    .set_exit(1)
                    .add_arc(0, comm, 1)
                    .finalize()
            }

            Stm::IfThenElse(g, ast1, ast2) => ProgramBuilder::new(4)
                .set_entry(0)
                .set_exit(3)
                .add_arc(0, Command::Guard(g.clone()), 1)
                .add_arc(0, Command::Guard(BExp::not(g.into())), 2)
                .add_subgraph(1, Self::new(*ast1), 3)
                .add_subgraph(2, Self::new(*ast2), 3)
                .finalize(),

            Stm::While(g, stm) => ProgramBuilder::new(3)
                .set_entry(0)
                .set_exit(2)
                .add_arc(0, Command::Guard(g.clone()), 1)
                .add_subgraph(1, Self::new(*stm), 0)
                .add_arc(0, Command::Guard(BExp::not(g.into())), 2)
                .finalize(),

            Stm::Comp(stm1, stm2) => ProgramBuilder::new(3)
                .set_entry(0)
                .set_exit(2)
                .add_subgraph(0, Self::new(*stm1), 1)
                .add_subgraph(1, Self::new(*stm2), 2)
                .finalize(),
        }
        .prettify()
        .clone()
    }
}
