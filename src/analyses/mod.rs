use yaxpeax_arch::Arch;
use data::ValueLocations;

#[macro_use]
pub mod control_flow;
pub mod data_flow;
pub mod function_signatures;
pub mod memory_layout;
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

/// interface to query a data flow graph (dfg). this interface is .... in flux.
///
/// TODOs in order of "how hard i think they are":
/// * it should be possible to look up a def site for a value
/// * it should be possible to iterate the use sites of a value
/// * perhaps it should be possible to insert new values to the dfg? optionally? this approaches
/// supporting general patching
/// * it should be possible to detach and move values
///
/// conceptually, these graphs have vertices at places where values are read or written, edges
/// from uses to some write, and a value associated with the write describing what subsequent reads
/// will see. these graphs describe the relation between values in a machine with
/// architecture-defined locations for values to exist. in many cases these graphs are operated on
/// in a manner consistent with the most atomic changes for a given architcture - typically an
/// instruction's execution. in an ideal world, this means `DFG` would have vertices at a pair
/// `(A::Address, A::Instruction, A::Location)`; "at a given address in memory, with a
/// corresponding instruction, the value at a specific architectural location is ___".
///
/// why is using an `(Address, Location)` pair, like `(0x1234, rdi)` not sufficient to uniquely
/// identify a location? because, dear reader, data at an address is not constant. if you decode
/// data at address `0x1234`, is that before or after relocations are applied? if that address is
/// known to be modified after loading, is the instruction there before or after the modification?
/// different answers to this temporal question mean the architectural locations referenced by the
/// corresponding instruction can be totally different!
///
/// so, really, a `DFG` describes the architectural state of a program at every discrete point of
/// change for any point in the program. an eventual TODO is to key on `(Address, Generation)`
/// where a "Generation" describes some series of memory edits. this is approximately supported in
/// SSA-based DFG construction, where `Memory` is a single architectural location that can be
/// versioned - perhaps "the program" may be inferred to a distinct memory region from
/// unknown-destination memorry accesses by default? in a way, a `DFG` might be self-describing if
/// at some location `(0x1234, Gen1)` the instruction modifies code memory by writing `(0x1236,
/// Gen2)`, where finding bytes to decode the next instruction would have to be a DFG query? this
/// suggests that in the most precise case, a DFG might be backed by a `MemoryRepr` with a series
/// of edits for each generation layered on top? it's not clear how this might interact with
/// disjoint memory regions that are versioned independently.
pub trait DFG<V, A: Arch + ValueLocations, When=<A as Arch>::Address> where When: Copy, for<'dfg> &'dfg mut Self: IndirectDFG<V, A, When> {
    fn read_loc(&self, when: When, loc: A::Location) -> V;
    fn read<T: ToDFGLoc<A::Location>>(&self, when: When, loc: &T) -> V {
        self.read_loc(when, loc.convert())
    }
    fn write_loc(&mut self, when: When, loc: A::Location, value: V);
    fn write<T: ToDFGLoc<A::Location>>(&mut self, when: When, loc: &T, value: V) {
        self.write_loc(when, loc.convert(), value)
    }
    fn query_at(&mut self, when: When) -> DFGLocationQueryCursor<When, V, A, Self> {
        DFGLocationQueryCursor {
            dfg: self,
            addr: when,
            _a: std::marker::PhantomData,
            _v: std::marker::PhantomData,
        }
    }
}

pub trait IndirectDFG<V, A: Arch + ValueLocations, When=<A as Arch>::Address> where When: Copy {
    type Indirect: Indirect<V>;
    fn indirect_loc(&mut self, when: When, loc: A::Location) -> Self::Indirect;
    fn indirect<T: ToDFGLoc<A::Location>>(&mut self, when: When, loc: A::Location) -> Self::Indirect {
        self.indirect_loc(when, loc.convert())
    }
}

// TODO: grow this in a way to allow more complex data than simple associated loads/stores?
/*
trait IndirectQuery<V, A> {
    fn load(&self, address: V) -> V;
    fn store(&self, address: V, value: V);
}
*/

pub struct DFGLocationQueryCursor<'dfg, K: Copy + Sized, V, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> where for<'cursor> &'cursor mut D: IndirectDFG<V, A, K> {
    dfg: &'dfg mut D,
    addr: K,
    _a: std::marker::PhantomData<A>,
    _v: std::marker::PhantomData<V>,
}

impl<'dfg, K: Copy, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> IndirectDFGLocationQuery<V, A, K, D> for DFGLocationQueryCursor<'dfg, K, V, A, D> where for<'cursor> &'cursor mut D: IndirectDFG<V, A, K> {
    fn indirect<'cursor, T: ToDFGLoc<A::Location>>(&'cursor mut self, loc: &T) -> <&'cursor mut D as IndirectDFG<V, A, K>>::Indirect {
        self.dfg.indirect(self.addr, loc.convert())
    }
}

macro_rules! opaque_indirection_dfg {
    ($arch:ty, $value:ty, $when:ty, $loc:ty, $dfg:ty) => {
        impl<'dfg> $crate::analyses::IndirectDFG<$value, $arch, $when> for &'dfg mut $dfg {
            type Indirect = $crate::analyses::OpaqueIndirection<$value>;

            fn indirect_loc(&mut self, _when: $when, loc: $loc) -> Self::Indirect {
                $crate::analyses::OpaqueIndirection::inst()
            }
        }
    }
}

impl<'dfg, K: Copy, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> DFGLocationQuery<V, A> for DFGLocationQueryCursor<'dfg, K, V, A, D> where for<'cursor> &'cursor mut D: 'dfg + IndirectDFG<V, A, K> {
    fn read_loc(&self, loc: A::Location) -> V {
        self.dfg.read_loc(self.addr, loc)
    }
    fn read<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> V {
        self.read_loc(loc.convert())
    }
    fn write_loc(&mut self, loc: A::Location, value: V) {
        self.dfg.write_loc(self.addr, loc, value)
    }
    fn write<T: ToDFGLoc<A::Location>>(&mut self, loc: &T, value: V) {
        self.write_loc(loc.convert(), value)
    }
}

pub trait IndirectDFGLocationQuery<V, A: Arch + ValueLocations, K: Copy + Sized, Indirect: ?Sized=OpaqueIndirection<V>> where for<'cursor> &'cursor mut Indirect: IndirectDFG<V, A, K>, Self: Sized {
    fn indirect<'cursor, T: ToDFGLoc<A::Location>>(&'cursor mut self, loc: &T) -> <&'cursor mut Indirect as IndirectDFG<V, A, K>>::Indirect;
}

/// it's relatively common to want to query a dfg at one address, for multiple values/read-write
/// accesses. `DFGLocationQuery` is essentially a helper to curry a specific address for future
/// queries.
pub trait DFGLocationQuery<V, A: Arch + ValueLocations> where Self: Sized {
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

#[derive(Copy, Clone)]
pub struct ValueIndex<'v, V: ?Sized> {
    pub base: &'v V,
    pub size: usize,
}

pub trait IntoValueIndex {
    fn byte(&self) -> ValueIndex<Self> {
        ValueIndex {
            base: self,
            size: 1,
        }
    }

    fn word(&self) -> ValueIndex<Self> {
        ValueIndex {
            base: self,
            size: 2,
        }
    }

    fn dword(&self) -> ValueIndex<Self> {
        ValueIndex {
            base: self,
            size: 4,
        }
    }

    fn qword(&self) -> ValueIndex<Self> {
        ValueIndex {
            base: self,
            size: 8,
        }
    }

    fn width(&self, size: usize) -> ValueIndex<Self> {
        ValueIndex {
            base: self,
            size,
        }
    }
}

impl <T: Value> IntoValueIndex for T {}

pub trait Indirect<V> {
    /// attempt to load `address` indirectly through this value, indexed by `address`.
    ///
    /// ## panics
    ///
    /// * if `self` does not represent a value that can be indexed. for example, the value backing
    /// a register can probably not be indexed. the value backing main memory probably can be
    /// indexed.
    fn load(&self, _address: ValueIndex<V>) -> V;

    /// attempt to load `address` indirectly through this value, indexed by `address`.
    ///
    /// ## panics
    ///
    /// * if `self` does not represent a value that can be indexed. for example, the value backing
    /// a register can probably not be indexed. the value backing main memory probably can be
    /// indexed.
    fn store(&self, _address: ValueIndex<V>, _value: &V);
}

pub struct OpaqueIndirection<V> {
    _marker: std::marker::PhantomData<V>,
}

impl<V> OpaqueIndirection<V> {
    pub fn inst() -> Self {
        OpaqueIndirection {
            _marker: std::marker::PhantomData
        }
    }
}


impl<V: Value> Indirect<V> for OpaqueIndirection<V> {
    fn load(&self, _: ValueIndex<V>) -> V {
        V::unknown()
    }

    fn store(&self, _: ValueIndex<V>, _: &V) { }
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
