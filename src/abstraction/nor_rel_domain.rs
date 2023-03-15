use std::{collections::BTreeMap, fmt::Display};

use crate::syntax::Variable;

use super::{AbsDomain, AbsValueDomain};

#[derive(Debug, Clone)]
pub struct NonRelationalDomain<AVD>
where
    AVD: AbsValueDomain,
{
    v_dom: AVD,
    top: BTreeMap<Variable, AVD::Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NonRelationalState<Value> {
    Bot,
    Vars(BTreeMap<Variable, Value>),
}

impl<AVD> NonRelationalDomain<AVD>
where
    AVD: AbsValueDomain,
{
    pub fn new<'a, I>(v_dom: AVD, vars: &'a I) -> Self
    where
        &'a I: IntoIterator,
        <&'a I as IntoIterator>::IntoIter: Iterator<Item = &'a Variable>,
    {
        let top = vars
            .into_iter()
            .map(|v: &String| (v.clone(), v_dom.top()))
            .collect();
        Self { v_dom, top }
    }
}

impl<AVD> AbsDomain<AVD> for NonRelationalDomain<AVD>
where
    AVD: AbsValueDomain + Clone,
    AVD::Value: Clone + Display,
{
    type State = NonRelationalState<AVD::Value>;

    fn value_domain(&self) -> AVD {
        self.v_dom.clone()
    }

    fn ass(&self, state: &mut Self::State, var: &Variable, val: AVD::Value) {
        use self::NonRelationalState::*;
        if val == self.v_dom.bot() {
            *state = Bot;
        } else {
            match state {
                Bot => (),
                Vars(vars) => {
                    vars.insert(var.to_string(), val);
                }
            }
        }
    }

    fn get_var(&self, state: &Self::State, var: &Variable) -> AVD::Value {
        use self::NonRelationalState::*;
        match state {
            Bot => self.v_dom.bot(),
            Vars(vars) => vars.get(var).unwrap_or(&self.v_dom.top()).clone(),
        }
    }

    fn bot(&self) -> Self::State {
        NonRelationalState::Bot
    }

    fn top(&self) -> Self::State {
        self::NonRelationalState::Vars(self.top.clone())
    }

    fn lub(&self, lhs: &Self::State, rhs: &Self::State) -> Self::State {
        use self::NonRelationalState::*;
        match (lhs, rhs) {
            (Bot, _) => rhs.clone(),
            (_, Bot) => lhs.clone(),
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys())
                    .map(|k| match (v1.get(k), v2.get(k)) {
                        (None, Some(i)) | (Some(i), None) => (k.clone(), i.clone()),
                        (Some(i1), Some(i2)) => (k.clone(), self.v_dom.lub(i1, i2)),
                        (None, None) => unreachable!(),
                    })
                    .collect();

                Vars(v)
            }
        }
    }

    fn glb(&self, lhs: &Self::State, rhs: &Self::State) -> Self::State {
        use self::NonRelationalState::*;
        match (lhs, rhs) {
            (Bot, _) | (_, Bot) => Bot,
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys())
                    .map(|k| match (v1.get(k), v2.get(k)) {
                        (None, Some(i)) | (Some(i), None) => (k.clone(), i.clone()),
                        (Some(i1), Some(i2)) => (k.clone(), self.v_dom.glb(i1, i2)),
                        (None, None) => unreachable!(),
                    })
                    .collect();

                Vars(v)
            }
        }
    }

    fn widening(&self, lhs: &Self::State, rhs: &Self::State) -> Self::State {
        use self::NonRelationalState::*;
        let bot = self.v_dom.bot();
        match (lhs, rhs) {
            (Bot, o) | (o, Bot) => o.clone(),
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys().into_iter())
                    .cloned()
                    .map(|k| {
                        let v1 = v1.get(&k).unwrap_or(&bot);
                        let v2 = v2.get(&k).unwrap_or(&bot);
                        (k, self.v_dom.widening(v1, v2))
                    })
                    .collect();

                Vars(v)
            }
        }
    }

    fn narrowing(&self, lhs: &Self::State, rhs: &Self::State) -> Self::State {
        use self::NonRelationalState::*;
        let top = self.v_dom.top();
        match (lhs, rhs) {
            (Bot, o) | (o, Bot) => o.clone(),
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys().into_iter())
                    .cloned()
                    .map(|k| {
                        let v1 = v1.get(&k).unwrap_or(&top);
                        let v2 = v2.get(&k).unwrap_or(&top);
                        (k, self.v_dom.narrowing(v1, v2))
                    })
                    .collect();

                Vars(v)
            }
        }
    }
}

impl<Value> Display for NonRelationalState<Value>
where
    Value: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bot => write!(f, "dead code"),
            Self::Vars(vars) => {
                if vars.is_empty() {
                    write!(f, "any value")?;
                }
                {
                    let mut vars = vars.iter();
                    if let Some((var, val)) = vars.next() {
                        write!(f, "{var} {val}")?;

                        for (var, val) in vars {
                            write!(f, ", {var} {val}")?;
                        }
                    };
                }
                Ok(())
            }
        }
    }
}
