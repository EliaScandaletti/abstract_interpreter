pub mod interval;
pub mod nor_rel_domain;

use crate::syntax::{Numeral, Variable};

pub trait AbsValueDomain: Sized {
    type Value: PartialEq;

    fn value_from_num(&self, num: &Numeral) -> Self::Value;

    fn cmp(&self, lhs: &Self::Value, rhs: &Self::Value) -> bool;
    fn neg(&self, rhs: &Self::Value) -> Self::Value;
    fn add(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn sub(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn mul(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn div(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;

    fn bot(&self) -> Self::Value;
    fn top(&self) -> Self::Value;
    fn lub(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn glb(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn widening(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    fn narrowing(&self, lhs: &Self::Value, rhs: &Self::Value) -> Self::Value;
    // fn gamma<T>(&self, val: &Self::Value) -> T;
    // fn alpha<T>(&self, val: T) -> Self::Value;
}

pub trait AbsDomain<AVD>
where
    AVD: AbsValueDomain,
{
    type State: PartialEq + Clone;

    fn value_domain(&self) -> AVD;

    fn ass(&self, state: &mut Self::State, var: &Variable, val: AVD::Value);
    fn get_var(&self, state: &Self::State, var: &Variable) -> AVD::Value;

    fn bot(&self) -> Self::State;
    fn top(&self) -> Self::State;
    fn lub(&self, rhs: &Self::State, lhs: &Self::State) -> Self::State;
    fn glb(&self, rhs: &Self::State, lhs: &Self::State) -> Self::State;
    // fn widening(rhs: &Self::State, lhs: &Self::State) -> Self::State;
    // fn narrowing(rhs: &Self::State, lhs: &Self::State) -> Self::State;
    // fn gamma<T>(val: &Self::State) -> T;
    // fn alpha<T>(val: T) -> Self::State;
}
