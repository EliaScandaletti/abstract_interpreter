use std::{
    fmt::Display,
    ops::{Add, Mul, Neg, Sub},
};

use crate::syntax::Numeral;

use super::AbsValueDomain;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Limit {
    InfP,
    InfN,
    Num(Numeral),
}

impl PartialOrd for Limit {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Limit {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        use Limit::*;
        match (self, other) {
            (InfP, InfP) => Equal,
            (InfP, _) => Greater,
            (_, InfP) => Less,
            (InfN, InfN) => Equal,
            (InfN, _) => Less,
            (_, InfN) => Greater,
            (Num(s), Num(o)) => s.cmp(o),
        }
    }
}

impl Neg for Limit {
    type Output = Self;

    fn neg(self) -> Self::Output {
        use Limit::*;
        match self {
            InfP => InfN,
            InfN => InfP,
            Num(x) => Num(-x),
        }
    }
}

impl Add for Limit {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use Limit::*;
        match (self, rhs) {
            (InfP, InfN) | (InfN, InfP) => unreachable!(),
            (InfP, _) | (_, InfP) => InfP,
            (InfN, _) | (_, InfN) => InfN,
            (Num(a), Num(b)) => Num(a + b),
        }
    }
}

impl Sub for Limit {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Mul for Limit {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        use Limit::*;
        match (self, rhs) {
            (Num(0), _) | (_, Num(0)) => Num(0),
            (InfP, InfP) | (InfN, InfN) => InfP,
            (InfP, InfN) | (InfN, InfP) => InfN,
            (Num(a), Num(b)) => Num(a * b),
            (Num(n), o) | (o, Num(n)) => {
                if n > 0 {
                    o
                } else {
                    -o
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Interval {
    Bot,
    Int { lb: Limit, ub: Limit },
}

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        use Interval::*;
        match (self, other) {
            (Bot, Bot) => Some(Equal),
            (Bot, Int { .. }) => Some(Less),
            (Int { .. }, Bot) => Some(Greater),
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => {
                if a < c && d < b {
                    Some(Greater)
                } else if c < a && b < d {
                    Some(Less)
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntervalValueDomain {
    dlb: Limit,
    dub: Limit,
}

impl IntervalValueDomain {
    pub fn new(dlb: Limit, dub: Limit) -> Self {
        Self { dlb, dub }
    }

    pub fn from_couple(&self, mut lb: Limit, mut ub: Limit) -> Interval {
        if lb > ub {
            return Interval::Bot;
        }
        if lb == ub {
            if let Limit::Num(_) = lb {
                return Interval::Int { lb, ub };
            }
        }
        if lb < self.dlb {
            lb = Limit::InfN
        }
        if ub > self.dub {
            ub = Limit::InfP
        }
        Interval::Int { lb, ub }
    }
}

impl AbsValueDomain for IntervalValueDomain {
    type Value = Interval;

    fn value_from_num(&self, num: &Numeral) -> Self::Value {
        self.from_couple(Limit::Num(num.clone()), Limit::Num(num.clone()))
    }

    fn cmp(&self, lhs: &Self::Value, rhs: &Self::Value) -> bool {
        lhs < rhs
    }

    fn neg(&self, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        match rhs {
            Bot => Bot,
            &Int { lb: a, ub: b } => self.from_couple(-b, -a),
        }
    }

    fn add(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (&Int { lb: a, ub: b }, &Int { lb: c, ub: d }) => self.from_couple(a + c, b + d),
        }
    }

    fn sub(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        self.add(lhs, &self.neg(rhs))
    }

    fn mul(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (&Int { lb: a, ub: b }, &Int { lb: c, ub: d }) => {
                let lb = std::cmp::min(std::cmp::min(a * c, a * d), std::cmp::min(b * c, b * d));
                let ub = std::cmp::max(std::cmp::max(a * c, a * d), std::cmp::max(b * c, b * d));
                self.from_couple(lb, ub)
            }
        }
    }

    fn div(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        use Limit::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (&Int { lb: a, ub: b }, &Int { lb: c, ub: d }) => {
                if c == Num(0) && d == Num(0) {
                    Bot
                } else if Num(1) <= c {
                    let lb = std::cmp::min(
                        // a / c
                        match (a, c) {
                            (InfP, _) | (_, InfP | InfN) => unreachable!(),
                            (InfN, Num(_)) => InfN,
                            (Num(a), Num(c)) => Num(a / c),
                        },
                        // a / d
                        match (a, d) {
                            (InfP, _) | (_, InfN) => unreachable!(),
                            (InfN, _) => InfN,
                            (Num(_), InfP) => Num(0),
                            (Num(a), Num(d)) => Num(a / d),
                        },
                    );
                    let ub = std::cmp::max(
                        // b / c
                        match (b, c) {
                            (InfN, _) | (_, InfP | InfN) => unreachable!(),
                            (InfP, Num(_)) => InfP,
                            (Num(b), Num(c)) => Num(b / c),
                        },
                        // b / d
                        match (b, d) {
                            (InfN, _) | (_, InfN) => unreachable!(),
                            (InfP, _) => InfP,
                            (Num(_), InfP) => Num(0),
                            (Num(b), Num(d)) => Num(b / d),
                        },
                    );
                    self.from_couple(lb, ub)
                } else if d <= Num(-1) {
                    let lb = std::cmp::min(
                        // b / c
                        match (b, c) {
                            (InfN, _) | (_, InfP) => unreachable!(),
                            (InfP, InfN) => InfN,
                            (InfP, Num(_)) => InfN,
                            (Num(_), InfN) => Num(0),
                            (Num(b), Num(c)) => Num(b / c),
                        },
                        // b / d
                        match (b, d) {
                            (InfN, _) | (_, InfP | InfN) => unreachable!(),
                            (InfP, Num(_)) => InfN,
                            (Num(b), Num(d)) => Num(b / d),
                        },
                    );
                    let ub = std::cmp::max(
                        // a / c
                        match (a, c) {
                            (InfP, _) | (_, InfP) => unreachable!(),
                            (InfN, InfN) => InfP,
                            (InfN, Num(_)) => InfP,
                            (Num(_), InfN) => Num(0),
                            (Num(a), Num(c)) => Num(a / c),
                        },
                        // a / d
                        match (a, d) {
                            (InfP, _) | (_, InfP | InfN) => unreachable!(),
                            (InfN, Num(_)) => InfP,
                            (Num(a), Num(d)) => Num(a / d),
                        },
                    );
                    self.from_couple(lb, ub)
                } else {
                    self.lub(
                        &(self.div(lhs, &self.glb(rhs, &self.from_couple(Num(1), InfP)))),
                        &(self.div(lhs, &self.glb(rhs, &self.from_couple(InfN, Num(-1))))),
                    )
                }
            }
        }
    }

    fn bot(&self) -> Self::Value {
        Interval::Bot
    }

    fn top(&self) -> Self::Value {
        self.from_couple(Limit::InfN, Limit::InfP)
    }

    fn lub(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, Bot) => Bot,
            (Bot, rhs) => *rhs,
            (lhs, Bot) => *lhs,
            (&Int { lb: a, ub: b }, &Int { lb: c, ub: d }) => {
                let lb = std::cmp::min(a, c);
                let ub = std::cmp::max(b, d);
                self.from_couple(lb, ub)
            }
        }
    }

    fn glb(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (&Int { lb: a, ub: b }, &Int { lb: c, ub: d }) => {
                let lb = std::cmp::max(a, c);
                let ub = std::cmp::min(b, d);
                self.from_couple(lb, ub)
            }
        }
    }
}

impl Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Interval::Bot => write!(f, "dead_code"),
            Interval::Int { lb, ub } => match (lb, ub) {
                (Limit::InfP, _) | (_, Limit::InfN) => unreachable!(),
                (Limit::InfN, Limit::InfP) => write!(f, "integer"),
                (Limit::InfN, Limit::Num(ub)) => write!(f, "<= {ub}"),
                (Limit::Num(lb), Limit::InfP) => write!(f, ">= {lb}"),
                (Limit::Num(lb), Limit::Num(ub)) => {
                    if *lb == *ub {
                        write!(f, "= {lb}")
                    } else {
                        write!(f, "in [{lb}; {ub}]")
                    }
                }
            },
        }
    }
}
