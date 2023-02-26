use crate::{
    abstraction::AbsValue,
    syntax::{AExp, BExp, Variable},
};

use super::{
    interval::{Interval, Limit},
    nor_rel_domain::NonRelationalDomain,
    AbsDomain, AbstractInterpreter,
};

pub struct IntervalInterpreter {}

type IntervalDomain = NonRelationalDomain<Interval>;

enum IntervalExpTree {
    Num(Interval),
    Var(Variable, Interval),
    Add(Box<Self>, Box<Self>, Interval),
    Sub(Box<Self>, Box<Self>, Interval),
    Mul(Box<Self>, Box<Self>, Interval),
    Div(Box<Self>, Box<Self>, Interval),
    Neg(Box<Self>, Interval),
    PreInc(Variable, Interval),
    PreDec(Variable, Interval),
    PostInc(Variable, Interval),
    PostDec(Variable, Interval),
}

impl IntervalExpTree {
    fn build(state: &mut IntervalDomain, exp: &AExp) -> Self {
        use IntervalExpTree::*;
        match exp {
            AExp::Num(n) => Num((Limit::Num(*n), Limit::Num(*n)).into()),
            AExp::Var(var) => Var(var.clone(), state.get_var(var)),
            AExp::Add(e1, e2) => {
                let sub1 = Self::build(state, e1);
                let sub2 = Self::build(state, e2);
                Add(sub1.into(), sub2.into(), Interval::top())
            }
            AExp::Sub(e1, e2) => {
                let sub1 = Self::build(state, e1);
                let sub2 = Self::build(state, e2);
                Sub(sub1.into(), sub2.into(), Interval::top())
            }
            AExp::Mul(e1, e2) => {
                let sub1 = Self::build(state, e1);
                let sub2 = Self::build(state, e2);
                Mul(sub1.into(), sub2.into(), Interval::top())
            }
            AExp::Div(e1, e2) => {
                let sub1 = Self::build(state, e1);
                let sub2 = Self::build(state, e2);
                Div(sub1.into(), sub2.into(), Interval::top())
            }
            AExp::Neg(e) => {
                let sub = Self::build(state, e);
                Neg(sub.into(), Interval::top())
            }
            AExp::PreInc(var) => {
                let mut val = state.get_var(var);
                val = val + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, val);
                PreInc(var.into(), val)
            }
            AExp::PreDec(var) => {
                let mut val = state.get_var(var);
                val = val - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, val);
                PreDec(var.into(), val)
            }
            AExp::PostInc(var) => {
                let val = state.get_var(var);
                let inc = val + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, inc);
                PostInc(var.into(), val)
            }
            AExp::PostDec(var) => {
                let val = state.get_var(var);
                let dec = val - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, dec);
                PostDec(var.into(), val)
            }
        }
    }

    fn evaluate(&mut self, state: &mut IntervalDomain) -> Interval {
        use IntervalExpTree::*;
        match self {
            Num(i) => *i,
            Var(var, i) => {
                let res = Interval::glb(&state.get_var(var), i);
                state.ass(var, res);
                *i = res;
                *i
            }
            Add(e1, e2, i) => {
                let i1 = e1.evaluate(state);
                let i2 = e2.evaluate(state);
                *i = i1 + i2;
                *i
            }
            Sub(e1, e2, i) => {
                let i1 = e1.evaluate(state);
                let i2 = e2.evaluate(state);
                *i = i1 - i2;
                *i
            }
            Mul(e1, e2, i) => {
                let i1 = e1.evaluate(state);
                let i2 = e2.evaluate(state);
                *i = i1 * i2;
                *i
            }
            Div(e1, e2, i) => {
                let i1 = e1.evaluate(state);
                let i2 = e2.evaluate(state);
                *i = i1 / i2;
                *i
            }
            Neg(e, i) => {
                *i = -e.evaluate(state);
                *i
            }
            PreInc(var, i) => {
                *i = state.get_var(var);
                *i = *i + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, *i);
                *i
            }
            PreDec(var, i) => {
                *i = state.get_var(var);
                *i = *i - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, *i);
                *i
            }
            PostInc(var, i) => {
                *i = state.get_var(var);
                let inc = *i + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, inc);
                *i
            }
            PostDec(var, i) => {
                *i = state.get_var(var);
                let dec = *i - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, dec);
                *i
            }
        }
    }

    fn get_interval(&self) -> Interval {
        use IntervalExpTree::*;
        match self {
            Num(i) => *i,
            Var(_, i) => *i,
            Add(_, _, i) => *i,
            Sub(_, _, i) => *i,
            Mul(_, _, i) => *i,
            Div(_, _, i) => *i,
            Neg(_, i) => *i,
            PreInc(_, i) => *i,
            PreDec(_, i) => *i,
            PostInc(_, i) => *i,
            PostDec(_, i) => *i,
        }
    }

    fn force(&mut self, state: &mut IntervalDomain, int: Interval) {
        use IntervalExpTree::*;
        match self {
            Num(i) => {
                *i = Interval::glb(i, &int);
                if *i == Interval::bot() {
                    *state = IntervalDomain::bot();
                }
            }
            Var(var, i) => {
                *i = Interval::glb(i, &int);
                state.ass(var, *i);
            }
            Add(e1, e2, i) => {
                *i = Interval::glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 + i2
                e1.force(state, *i - i2); // force i1 = i - i2
                e2.force(state, *i - i1); // force i2 = i - i1
            }
            Sub(e1, e2, i) => {
                *i = Interval::glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 - i2
                e1.force(state, *i + i2); // force i1 = i + i2
                e2.force(state, i1 - *i); // force i2 = i1 - i
            }
            Mul(e1, e2, i) => {
                *i = Interval::glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 * i2
                e1.force(state, *i / i2); // force i1 = i / i2
                e2.force(state, *i / i1); // force i2 = i / i1
            }
            Div(e1, e2, i) => {
                *i = Interval::glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 / i2
                e1.force(state, *i * i2); // force i1 = i * i2
                e2.force(state, i1 / *i); // force i2 = i1 / i
            }
            Neg(e, i) => {
                *i = Interval::glb(i, &int);
                // i = -j where j = e.get_interval()
                e.force(state, -*i); // force j = -i
            }
            PreInc(var, i) => {
                *i = Interval::glb(i, &int);
                state.ass(var, *i);
            }
            PreDec(var, i) => {
                *i = Interval::glb(i, &int);
                state.ass(var, *i);
            }
            PostInc(var, i) => {
                *i = Interval::glb(i, &int);
                state.ass(var, *i + (Limit::Num(1), Limit::Num(1)).into());
            }
            PostDec(var, i) => {
                *i = Interval::glb(i, &int);
                state.ass(var, *i - (Limit::Num(1), Limit::Num(1)).into());
            }
        }
    }
}

impl AbstractInterpreter<IntervalDomain> for IntervalInterpreter {
    fn aexp(state: &mut IntervalDomain, exp: &AExp) -> Interval {
        match exp {
            AExp::Num(n) => Interval::Int {
                lb: Limit::Num(*n),
                ub: Limit::Num(*n),
            },
            AExp::Var(var) => state.get_var(var),
            AExp::Add(e1, e2) => {
                let int1 = Self::aexp(state, e1);
                let int2 = Self::aexp(state, e2);
                let res = int1 + int2;
                if res == Interval::Bot {
                    *state = IntervalDomain::Bot
                }
                res
            }
            AExp::Sub(e1, e2) => {
                let int1 = Self::aexp(state, e1);
                let int2 = Self::aexp(state, e2);
                let res = int1 - int2;
                if res == Interval::Bot {
                    *state = IntervalDomain::Bot
                }
                res
            }
            AExp::Mul(e1, e2) => {
                let int1 = Self::aexp(state, e1);
                let int2 = Self::aexp(state, e2);
                let res = int1 * int2;
                if res == Interval::Bot {
                    *state = IntervalDomain::Bot
                }
                res
            }
            AExp::Div(e1, e2) => {
                let int1 = Self::aexp(state, e1);
                let int2 = Self::aexp(state, e2);
                let res = int1 / int2;
                if res == Interval::Bot {
                    *state = IntervalDomain::Bot
                }
                res
            }
            AExp::Neg(e) => {
                let res = -Self::aexp(state, e);
                if res == Interval::Bot {
                    *state = IntervalDomain::Bot
                }
                res
            }
            AExp::PreInc(var) => {
                let mut val = state.get_var(var);
                val = val + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, val);
                val
            }
            AExp::PreDec(var) => {
                let mut val = state.get_var(var);
                val = val - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, val);
                val
            }
            AExp::PostInc(var) => {
                let val = state.get_var(var);
                let inc = val + (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, inc);
                val
            }
            AExp::PostDec(var) => {
                let val = state.get_var(var);
                let dec = val - (Limit::Num(1), Limit::Num(1)).into();
                state.ass(var, dec);
                val
            }
        }
    }

    fn bexp(state: &mut IntervalDomain, exp: &BExp) -> IntervalDomain {
        use BExp::*;
        match exp {
            True => state.clone(),
            False => IntervalDomain::bot(),
            Eq(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(state, aexp1).into(),
                    IntervalExpTree::build(state, aexp2).into(),
                    Interval::top(),
                );

                s.evaluate(&mut old.clone());

                let mut ret = old;
                // aexp1 - aexp2 = 0
                s.force(&mut ret, (Limit::Num(0), Limit::Num(0)).into());
                ret
            }
            Neq(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(state, aexp1).into(),
                    IntervalExpTree::build(state, aexp2).into(),
                    Interval::top(),
                );

                s.evaluate(&mut old.clone());

                let mut ret1 = old.clone();
                // aexp1 - aexp2 < 0
                s.force(&mut ret1, (Limit::InfN, Limit::Num(1)).into());
                let mut ret2 = old;
                // aexp1 - aexp2 > 0
                s.force(&mut ret2, (Limit::Num(1), Limit::InfP).into());
                IntervalDomain::lub(&ret1, &ret2)
            }
            Lt(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(state, aexp1).into(),
                    IntervalExpTree::build(state, aexp2).into(),
                    Interval::top(),
                );

                s.evaluate(&mut old.clone());

                let mut ret = old;
                // aexp1 - aexp2 < 0
                s.force(&mut ret, (Limit::InfN, Limit::Num(-1)).into());
                ret
            }
            And(bexp1, bexp2) => {
                let mut tt1 = Self::bexp(state, bexp1);
                Self::bexp(state, bexp2);
                let tt2 = Self::bexp(&mut tt1, bexp2);
                tt2
            }
            Or(bexp1, bexp2) => {
                let tt1 = Self::bexp(state, bexp1);
                let tt2 = Self::bexp(state, bexp2);
                IntervalDomain::lub(&tt1, &tt2)
            }
            Not(bexp) => match &**bexp {
                True => IntervalDomain::bot(),
                False => state.clone(),
                Eq(a1, a2) => Self::bexp(state, &Neq(a1.clone(), a2.clone())),
                Neq(a1, a2) => Self::bexp(state, &Eq(a1.clone(), a2.clone())),
                Lt(aexp1, aexp2) => {
                    let old = state.clone();
                    // aexp1 - aexp2
                    let mut s = IntervalExpTree::Sub(
                        IntervalExpTree::build(state, aexp1).into(),
                        IntervalExpTree::build(state, aexp2).into(),
                        Interval::top(),
                    );

                    s.evaluate(&mut old.clone());

                    let mut ret = old;
                    // aexp1 - aexp2 >= 0
                    s.force(&mut ret, (Limit::Num(0), Limit::InfP).into());
                    ret
                }
                And(b1, b2) => {
                    Self::bexp(state, &Or(Not(b1.clone()).into(), Not(b2.clone()).into()))
                }
                Or(b1, b2) => {
                    Self::bexp(state, &And(Not(b1.clone()).into(), Not(b2.clone()).into()))
                }
                Not(e) => Self::bexp(state, &e),
            },
        }
    }
}
