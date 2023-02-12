use std::{collections::HashMap, fmt::Display};

use crate::syntax::Variable;

use super::{AbsDomain, AbsValue};

#[derive(Debug, Clone, PartialEq)]
pub enum NonRelationalDomain<Value> {
    Bot,
    Vars(HashMap<Variable, Value>),
}

impl<Value> AbsDomain for NonRelationalDomain<Value>
where
    Value: AbsValue + Clone,
{
    type Value = Value;

    fn ass(&mut self, var: &Variable, val: Self::Value) {
        if val == Value::bot() {
            *self = NonRelationalDomain::Bot;
        } else {
            match self {
                NonRelationalDomain::Bot => (),
                NonRelationalDomain::Vars(vars) => {
                    vars.insert(var.to_string(), val);
                }
            }
        }
    }

    fn get_var(&self, var: &Variable) -> Self::Value {
        match self {
            Self::Bot => Value::bot(),
            Self::Vars(vars) => vars.get(var).unwrap_or(&Value::top()).clone(),
        }
    }

    fn bot() -> Self {
        NonRelationalDomain::Bot
    }

    fn top() -> Self {
        NonRelationalDomain::Vars(HashMap::new())
    }

    fn lub(rhs: &Self, lhs: &Self) -> Self {
        use NonRelationalDomain::*;
        match (rhs, lhs) {
            (Bot, _) => lhs.clone(),
            (_, Bot) => rhs.clone(),
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys())
                    .map(|k| match (v1.get(k), v2.get(k)) {
                        (None, Some(i)) | (Some(i), None) => (k.clone(), i.clone()),
                        (Some(i1), Some(i2)) => (k.clone(), Value::lub(i1, i2)),
                        (None, None) => unreachable!(),
                    })
                    .collect();

                Vars(v)
            }
        }
    }

    fn glb(rhs: &Self, lhs: &Self) -> Self {
        use NonRelationalDomain::*;
        match (rhs, lhs) {
            (Bot, _) => lhs.clone(),
            (_, Bot) => rhs.clone(),
            (Vars(v1), Vars(v2)) => {
                let v = v1
                    .keys()
                    .into_iter()
                    .chain(v2.keys())
                    .map(|k| match (v1.get(k), v2.get(k)) {
                        (None, Some(i)) | (Some(i), None) => (k.clone(), i.clone()),
                        (Some(i1), Some(i2)) => (k.clone(), Value::glb(i1, i2)),
                        (None, None) => unreachable!(),
                    })
                    .collect();

                Vars(v)
            }
        }
    }
}

impl<Value> Display for NonRelationalDomain<Value>
where
    Value: AbsValue + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonRelationalDomain::Bot => write!(f, "dead code"),
            NonRelationalDomain::Vars(vars) => {
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
