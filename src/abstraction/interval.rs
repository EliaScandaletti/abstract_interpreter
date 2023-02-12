use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::syntax::Numeral;

use super::AbsValue;

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
pub enum Interval<const LB: Numeral, const UB: Numeral> {
    Bot,
    Int { lb: Limit, ub: Limit },
}

impl<const LB: Numeral, const UB: Numeral> From<Numeral> for Interval<LB, UB> {
    fn from(value: Numeral) -> Self {
        (Limit::Num(value), Limit::Num(value)).into()
    }
}

impl<const LB: Numeral, const UB: Numeral> From<(Limit, Limit)> for Interval<LB, UB> {
    fn from((mut lb, mut ub): (Limit, Limit)) -> Self {
        if lb < Limit::Num(LB) {
            lb = Limit::InfN
        }
        if ub > Limit::Num(UB) {
            ub = Limit::InfP
        }
        if lb <= ub {
            Self::Int { lb, ub }
        } else {
            Self::Bot
        }
    }
}

impl<const LB: Numeral, const UB: Numeral> PartialOrd for Interval<LB, UB> {
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

impl<const LB: Numeral, const UB: Numeral> AbsValue for Interval<LB, UB> {
    fn bot() -> Self {
        Interval::Bot
    }

    fn top() -> Self {
        (Limit::InfN, Limit::InfP).into()
    }

    fn lub(lhs: &Self, rhs: &Self) -> Self {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, Bot) => Bot,
            (Bot, rhs) => *rhs,
            (lhs, Bot) => *lhs,
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => {
                let lb = *std::cmp::min(a, c);
                let ub = *std::cmp::max(b, d);
                (lb, ub).into()
            }
        }
    }

    fn glb(lhs: &Self, rhs: &Self) -> Self {
        use Interval::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => {
                let lb = *std::cmp::max(a, c);
                let ub = *std::cmp::min(b, d);
                if lb <= ub {
                    (lb, ub).into()
                } else {
                    Bot
                }
            }
        }
    }
}

impl<const LB: Numeral, const UB: Numeral> Neg for Interval<LB, UB> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        use Interval::*;
        match self {
            Bot => Bot,
            Int { lb: a, ub: b } => (-b, -a).into(),
        }
    }
}

impl<const LB: Numeral, const UB: Numeral> Add for Interval<LB, UB> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use Interval::*;
        match (self, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => (a + c, b + d).into(),
        }
    }
}

impl<const LB: Numeral, const UB: Numeral> Sub for Interval<LB, UB> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl<const LB: Numeral, const UB: Numeral> Mul for Interval<LB, UB> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        use Interval::*;
        match (self, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => {
                let lb = std::cmp::min(std::cmp::min(a * c, a * d), std::cmp::min(b * c, b * d));
                let ub = std::cmp::max(std::cmp::max(a * c, a * d), std::cmp::max(b * c, b * d));
                (lb, ub).into()
            }
        }
    }
}

impl<const LB: Numeral, const UB: Numeral> Div for Interval<LB, UB> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        use Interval::*;
        use Limit::*;
        match (self, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (Int { lb: a, ub: b }, Int { lb: c, ub: d }) => {
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
                    (lb, ub).into()
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
                    (lb, ub).into()
                } else {
                    Self::lub(
                        &(self / Self::glb(&rhs, &(Num(1), InfP).into())),
                        &(self / Self::glb(&rhs, &(InfN, Num(-1)).into())),
                    )
                }
            }
        }
    }
}

impl<const LB: i32, const UB: i32> Display for Interval<LB, UB> {
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
