use yaxpeax_arch::Arch;
use data::ValueLocations;

#[macro_use]
pub mod control_flow;
pub mod data_flow;
pub mod function_signatures;
pub mod static_single_assignment;
pub mod xrefs;
pub mod evaluators;
pub mod value_range;

pub enum CompletionStatus {
    Incomplete,
    Complete,
}

pub struct ValueRes<V> {
    value: V,
    carry: V,
}

#[allow(dead_code)]
impl<V: Value> ValueRes<V> {
    pub(crate) fn from_zero() -> Self {
        ValueRes::literal(V::from_const(0))
    }
    pub(crate) fn literal(value: V) -> Self {
        ValueRes {
            value,
            carry: V::from_const(0),
        }
    }
    pub(crate) fn unknown() -> Self {
        ValueRes {
            value: V::unknown(),
            carry: V::unknown(),
        }
    }
    pub(crate) fn parts(self) -> (V, V) {
        (self.value, self.carry)
    }
    pub(crate) fn value(self) -> V {
        self.value
    }
    pub(crate) fn carry(self) -> V {
        self.carry
    }
    pub(crate) fn zero(&self) -> V {
        self.value.eq(&V::from_const(0))
    }
    pub(crate) fn sign(&self) -> V {
        self.value.lt(&V::from_const(0))
    }
}

pub trait DFG<V, A: Arch + ValueLocations> {
    fn read_loc(&self, loc: A::Location) -> V;
    fn read<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> V {
        self.read_loc(loc.convert())
    }
    fn write_loc(&mut self, loc: A::Location, value: V);
    fn write<T: ToDFGLoc<A::Location>>(&mut self, loc: &T, value: V) {
        self.write_loc(loc.convert(), value)
    }
}

pub trait ToDFGLoc<U> {
    fn convert(&self) -> U;
}

impl<T: Clone> ToDFGLoc<T> for T {
    fn convert(&self) -> T {
        self.to_owned()
    }
}

impl<'a, T: Clone> ToDFGLoc<T> for &'a T {
    fn convert(&self) -> T {
        self.to_owned().to_owned()
    }
}

pub trait Value: Sized {
    fn unknown() -> Self;

    fn from_const(_c: u64) -> Self {
        Self::unknown()
    }

    fn from_set(xs: &[Self]) -> Self;

    fn to_const(&self) -> Option<u64>;

    fn as_bool(&self) -> Option<bool> {
        self.ne(&Value::from_const(0)).to_const().map(|x| x != 0)
    }

    fn add(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn sub(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn mul(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn or(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn and(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn xor(&self, _other: &Self) -> ValueRes<Self> {
        ValueRes::unknown()
    }

    fn modulo(&self, _other: &Self) -> Self {
        Self::unknown()
    }

    fn ne(&self, _other: &Self) -> Self {
        Self::unknown()
    }

    fn le(&self, _other: &Self) -> Self {
        Self::unknown()
    }

    fn lt(&self, other: &Self) -> Self {
        self.le(other).and(&self.eq(other).not()).value()
    }

    fn gte(&self, other: &Self) -> Self {
        self.lt(other).not()
    }

    fn eq(&self, _other: &Self) -> Self {
        Self::unknown()
    }

    fn not(&self) -> Self {
        Self::unknown()
    }

    fn sxt(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn zxt(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn shr(&self, _amt: &Self) -> Self {
        Self::unknown()
    }

    fn sar(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn shl(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn sal(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn rcl(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn rcr(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn rol(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn ror(&self, _width: &Self) -> Self {
        Self::unknown()
    }
}
