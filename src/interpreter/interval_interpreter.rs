use crate::{
    abstraction::{
        interval::{Interval, IntervalValueDomain, Limit},
        nor_rel_domain::NonRelationalDomain,
        AbsValueDomain,
    },
    syntax::{AExp, BExp, Variable},
};

use super::{AbsDomain, AbstractInterpreter};

pub struct IntervalInterpreter {
    v_dom: IntervalValueDomain,
    dom: IntervalDomain,
}

pub type IntervalDomain = NonRelationalDomain<IntervalValueDomain>;

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
    fn build(
        dom: &IntervalDomain,
        v_dom: &IntervalValueDomain,
        state: &mut <IntervalDomain as AbsDomain<IntervalValueDomain>>::State,
        exp: &AExp,
    ) -> Self {
        use IntervalExpTree::*;
        match exp {
            AExp::Num(n) => Num(v_dom.value_from_num(n)),
            AExp::Var(var) => Var(var.clone(), dom.get_var(state, var)),
            AExp::Add(e1, e2) => {
                let sub1 = Self::build(dom, v_dom, state, e1);
                let sub2 = Self::build(dom, v_dom, state, e2);
                Add(sub1.into(), sub2.into(), v_dom.top())
            }
            AExp::Sub(e1, e2) => {
                let sub1 = Self::build(dom, v_dom, state, e1);
                let sub2 = Self::build(dom, v_dom, state, e2);
                Sub(sub1.into(), sub2.into(), v_dom.top())
            }
            AExp::Mul(e1, e2) => {
                let sub1 = Self::build(dom, v_dom, state, e1);
                let sub2 = Self::build(dom, v_dom, state, e2);
                Mul(sub1.into(), sub2.into(), v_dom.top())
            }
            AExp::Div(e1, e2) => {
                let sub1 = Self::build(dom, v_dom, state, e1);
                let sub2 = Self::build(dom, v_dom, state, e2);
                Div(sub1.into(), sub2.into(), v_dom.top())
            }
            AExp::Neg(e) => {
                let sub = Self::build(dom, v_dom, state, e);
                Neg(sub.into(), v_dom.top())
            }
            AExp::PreInc(var) => {
                let mut val = dom.get_var(state, var);
                val = v_dom.add(&val, &v_dom.value_from_num(&1));
                dom.ass(state, var, val);
                PreInc(var.into(), val)
            }
            AExp::PreDec(var) => {
                let mut val = dom.get_var(state, var);
                val = v_dom.sub(&val, &v_dom.value_from_num(&1));
                dom.ass(state, var, val);
                PreDec(var.into(), val)
            }
            AExp::PostInc(var) => {
                let val = dom.get_var(state, var);
                let inc = v_dom.add(&val, &v_dom.value_from_num(&1));
                dom.ass(state, var, inc);
                PostInc(var.into(), val)
            }
            AExp::PostDec(var) => {
                let val = dom.get_var(state, var);
                let dec = v_dom.sub(&val, &v_dom.value_from_num(&1));
                dom.ass(state, var, dec);
                PostDec(var.into(), val)
            }
        }
    }

    fn evaluate(
        &mut self,
        dom: &IntervalDomain,
        v_dom: &IntervalValueDomain,
        state: &mut <IntervalDomain as AbsDomain<IntervalValueDomain>>::State,
    ) -> Interval {
        use IntervalExpTree::*;
        match self {
            Num(i) => *i,
            Var(var, i) => {
                let res = v_dom.glb(&dom.get_var(state, var), i);
                dom.ass(state, var, res);
                *i = res;
                *i
            }
            Add(e1, e2, i) => {
                let i1 = e1.evaluate(dom, v_dom, state);
                let i2 = e2.evaluate(dom, v_dom, state);
                *i = v_dom.add(&i1, &i2);
                *i
            }
            Sub(e1, e2, i) => {
                let i1 = e1.evaluate(dom, v_dom, state);
                let i2 = e2.evaluate(dom, v_dom, state);
                *i = v_dom.sub(&i1, &i2);
                *i
            }
            Mul(e1, e2, i) => {
                let i1 = e1.evaluate(dom, v_dom, state);
                let i2 = e2.evaluate(dom, v_dom, state);
                *i = v_dom.mul(&i1, &i2);
                *i
            }
            Div(e1, e2, i) => {
                let i1 = e1.evaluate(dom, v_dom, state);
                let i2 = e2.evaluate(dom, v_dom, state);
                *i = v_dom.div(&i1, &i2);
                *i
            }
            Neg(e, i) => {
                *i = v_dom.neg(&e.evaluate(dom, v_dom, state));
                *i
            }
            PreInc(var, i) => {
                *i = dom.get_var(state, var);
                *i = v_dom.add(i, &v_dom.value_from_num(&1));
                dom.ass(state, var, *i);
                *i
            }
            PreDec(var, i) => {
                *i = dom.get_var(state, var);
                *i = v_dom.sub(i, &v_dom.value_from_num(&1));
                dom.ass(state, var, *i);
                *i
            }
            PostInc(var, i) => {
                *i = dom.get_var(state, var);
                let inc = v_dom.add(i, &v_dom.value_from_num(&1));
                dom.ass(state, var, inc);
                *i
            }
            PostDec(var, i) => {
                *i = dom.get_var(state, var);
                let dec = v_dom.sub(i, &v_dom.value_from_num(&1));
                dom.ass(state, var, dec);
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

    fn force(
        &mut self,
        dom: &IntervalDomain,
        v_dom: &IntervalValueDomain,
        state: &mut <IntervalDomain as AbsDomain<IntervalValueDomain>>::State,
        int: Interval,
    ) {
        use IntervalExpTree::*;
        match self {
            Num(i) => {
                *i = v_dom.glb(i, &int);
                if *i == v_dom.bot() {
                    *state = dom.bot();
                }
            }
            Var(var, i) => {
                *i = v_dom.glb(i, &int);
                dom.ass(state, var, *i);
            }
            Add(e1, e2, i) => {
                *i = v_dom.glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 + i2
                e1.force(dom, v_dom, state, v_dom.sub(i, &i2)); // force i1 = i - i2
                e2.force(dom, v_dom, state, v_dom.sub(i, &i1)); // force i2 = i - i1
            }
            Sub(e1, e2, i) => {
                *i = v_dom.glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 - i2
                e1.force(dom, v_dom, state, v_dom.add(i, &i2)); // force i1 = i + i2
                e2.force(dom, v_dom, state, v_dom.sub(&i1, i)); // force i2 = i1 - i
            }
            Mul(e1, e2, i) => {
                *i = v_dom.glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 * i2
                e1.force(dom, v_dom, state, v_dom.div(i, &i2)); // force i1 = i / i2
                e2.force(dom, v_dom, state, v_dom.div(i, &i1)); // force i2 = i / i1
            }
            Div(e1, e2, i) => {
                *i = v_dom.glb(i, &int);
                let i2 = e2.get_interval();
                let i1 = e1.get_interval();
                // i = i1 / i2
                e1.force(dom, v_dom, state, v_dom.mul(i, &i2)); // force i1 = i * i2
                e2.force(dom, v_dom, state, v_dom.div(&i1, i)); // force i2 = i1 / i
            }
            Neg(e, i) => {
                *i = v_dom.glb(i, &int);
                // i = -j where j = e.get_interval()
                e.force(dom, v_dom, state, v_dom.neg(i)); // force j = -i
            }
            PreInc(var, i) => {
                *i = v_dom.glb(i, &int);
                dom.ass(state, var, *i);
            }
            PreDec(var, i) => {
                *i = v_dom.glb(i, &int);
                dom.ass(state, var, *i);
            }
            PostInc(var, i) => {
                *i = v_dom.glb(i, &int);
                dom.ass(state, var, v_dom.add(i, &v_dom.value_from_num(&1)));
            }
            PostDec(var, i) => {
                *i = v_dom.glb(i, &int);
                dom.ass(state, var, v_dom.sub(i, &v_dom.value_from_num(&1)));
            }
        }
    }
}

impl AbstractInterpreter<IntervalValueDomain, IntervalDomain> for IntervalInterpreter {
    fn value_domain(&self) -> &IntervalValueDomain {
        &self.v_dom
    }

    fn domain(&self) -> &IntervalDomain {
        &self.dom
    }

    fn new(dom: IntervalDomain) -> Self {
        Self {
            v_dom: dom.value_domain(),
            dom,
        }
    }

    fn aexp(
        &self,
        state: &mut <IntervalDomain as AbsDomain<IntervalValueDomain>>::State,
        exp: &AExp,
    ) -> Interval {
        match exp {
            AExp::Num(n) => Interval::Int {
                lb: Limit::Num(*n),
                ub: Limit::Num(*n),
            },
            AExp::Var(var) => self.dom.get_var(state, var),
            AExp::Add(e1, e2) => {
                let int1 = self.aexp(state, e1);
                let int2 = self.aexp(state, e2);
                let res = self.v_dom.add(&int1, &int2);
                if res == Interval::Bot {
                    *state = self.dom.bot()
                }
                res
            }
            AExp::Sub(e1, e2) => {
                let int1 = self.aexp(state, e1);
                let int2 = self.aexp(state, e2);
                let res = self.v_dom.sub(&int1, &int2);
                if res == Interval::Bot {
                    *state = self.dom.bot()
                }
                res
            }
            AExp::Mul(e1, e2) => {
                let int1 = self.aexp(state, e1);
                let int2 = self.aexp(state, e2);
                let res = self.v_dom.mul(&int1, &int2);
                if res == Interval::Bot {
                    *state = self.dom.bot()
                }
                res
            }
            AExp::Div(e1, e2) => {
                let int1 = self.aexp(state, e1);
                let int2 = self.aexp(state, e2);
                let res = self.v_dom.div(&int1, &int2);
                if res == Interval::Bot {
                    *state = self.dom.bot()
                }
                res
            }
            AExp::Neg(e) => {
                let res = self.v_dom.neg(&self.aexp(state, e));
                if res == Interval::Bot {
                    *state = self.dom.bot()
                }
                res
            }
            AExp::PreInc(var) => {
                let mut val = self.dom.get_var(state, var);
                val = self.v_dom.add(&val, &self.v_dom.value_from_num(&1));
                self.dom.ass(state, var, val);
                val
            }
            AExp::PreDec(var) => {
                let mut val = self.dom.get_var(state, var);
                val = self.v_dom.sub(&val, &self.v_dom.value_from_num(&1));
                self.dom.ass(state, var, val);
                val
            }
            AExp::PostInc(var) => {
                let val = self.dom.get_var(state, var);
                let inc = self.v_dom.add(&val, &self.v_dom.value_from_num(&1));
                self.dom.ass(state, var, inc);
                val
            }
            AExp::PostDec(var) => {
                let val = self.dom.get_var(state, var);
                let dec = self.v_dom.sub(&val, &self.v_dom.value_from_num(&1));
                self.dom.ass(state, var, dec);
                val
            }
        }
    }

    fn bexp(
        &self,
        state: &mut <IntervalDomain as AbsDomain<IntervalValueDomain>>::State,
        exp: &BExp,
    ) -> <IntervalDomain as AbsDomain<IntervalValueDomain>>::State {
        use BExp::*;
        match exp {
            True => state.clone(),
            False => self.dom.bot(),
            Eq(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp1).into(),
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp2).into(),
                    self.v_dom.top(),
                );

                s.evaluate(&self.dom, &self.v_dom, &mut old.clone());

                let mut ret = old;
                // aexp1 - aexp2 = 0
                s.force(
                    &self.dom,
                    &self.v_dom,
                    &mut ret,
                    self.v_dom.value_from_num(&0),
                );
                ret
            }
            Neq(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp1).into(),
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp2).into(),
                    self.v_dom.top(),
                );

                s.evaluate(&self.dom, &self.v_dom, &mut old.clone());

                let mut ret1 = old.clone();
                // aexp1 - aexp2 < 0
                s.force(
                    &self.dom,
                    &self.v_dom,
                    &mut ret1,
                    self.v_dom.from_couple(Limit::InfN, Limit::Num(1)),
                );
                let mut ret2 = old;
                // aexp1 - aexp2 > 0
                s.force(
                    &self.dom,
                    &self.v_dom,
                    &mut ret2,
                    self.v_dom.from_couple(Limit::Num(1), Limit::InfP),
                );
                self.dom.lub(&ret1, &ret2)
            }
            Lt(aexp1, aexp2) => {
                let old = state.clone();
                // aexp1 - aexp2
                let mut s = IntervalExpTree::Sub(
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp1).into(),
                    IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp2).into(),
                    self.v_dom.top(),
                );

                s.evaluate(&self.dom, &self.v_dom, &mut old.clone());

                let mut ret = old;
                // aexp1 - aexp2 < 0
                s.force(
                    &self.dom,
                    &self.v_dom,
                    &mut ret,
                    self.v_dom.from_couple(Limit::InfN, Limit::Num(-1)),
                );
                ret
            }
            And(bexp1, bexp2) => {
                let mut tt1 = self.bexp(state, bexp1);
                self.bexp(state, bexp2);
                let tt2 = self.bexp(&mut tt1, bexp2);
                tt2
            }
            Or(bexp1, bexp2) => {
                let tt1 = self.bexp(state, bexp1);
                let tt2 = self.bexp(state, bexp2);
                self.dom.lub(&tt1, &tt2)
            }
            Not(bexp) => match &**bexp {
                True => self.dom.bot(),
                False => state.clone(),
                Eq(a1, a2) => self.bexp(state, &Neq(a1.clone(), a2.clone())),
                Neq(a1, a2) => self.bexp(state, &Eq(a1.clone(), a2.clone())),
                Lt(aexp1, aexp2) => {
                    let old = state.clone();
                    // aexp1 - aexp2
                    let mut s = IntervalExpTree::Sub(
                        IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp1).into(),
                        IntervalExpTree::build(&self.dom, &self.v_dom, state, aexp2).into(),
                        self.v_dom.top(),
                    );

                    s.evaluate(&self.dom, &self.v_dom, &mut old.clone());

                    let mut ret = old;
                    // aexp1 - aexp2 >= 0
                    s.force(
                        &self.dom,
                        &self.v_dom,
                        &mut ret,
                        self.v_dom.from_couple(Limit::Num(0), Limit::InfP),
                    );
                    ret
                }
                And(b1, b2) => {
                    self.bexp(state, &Or(Not(b1.clone()).into(), Not(b2.clone()).into()))
                }
                Or(b1, b2) => {
                    self.bexp(state, &And(Not(b1.clone()).into(), Not(b2.clone()).into()))
                }
                Not(e) => self.bexp(state, &e),
            },
        }
    }
}
