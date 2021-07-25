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
    pub value: V,
    pub carry: V,
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
pub trait DFG<V: Value, A: Arch + ValueLocations, When=<A as Arch>::Address> where When: Copy {
    type Indirect: IndirectQuery<V>;

    fn read_loc(&self, when: When, loc: A::Location) -> V;
    fn read<T: ToDFGLoc<A::Location>>(&self, when: When, loc: &T) -> V {
        self.read_loc(when, loc.convert())
    }
    fn write_loc(&mut self, when: When, loc: A::Location, value: V);
    fn write<T: ToDFGLoc<A::Location>>(&mut self, when: When, loc: &T, value: V) {
        self.write_loc(when, loc.convert(), value)
    }
    // in an ideal world with a GAT-enabled rustc, `Indirect` would be spelled as
    // ```
    // type Indirect<'dfg>: IndirectQuery<'dfg>;
    // ```
    // and could retain a `&'dfg mut DFG` to do queries through. because we do not have GAT, that
    // associated type cannot be written, and we must obligate `Self::Indirect` and DFG impls to
    // holding `Rc<RefCell<IndirectionStruct>>` even though we know no user of a `DFG` interface
    // can get multiple refs, let alone mutable refs.
    fn indirect_loc(&self, _when: When, _loc: A::Location) -> Self::Indirect;
    fn indirect<T: ToDFGLoc<A::Location>>(&self, when: When, loc: &T) -> Self::Indirect {
        self.indirect_loc(when, loc.convert())
    }
    fn query_at(&self, when: When) -> DFGLocationQueryCursor<When, V, A, Self> {
        DFGLocationQueryCursor {
            dfg: self,
            addr: when,
            _a: std::marker::PhantomData,
            _v: std::marker::PhantomData,
        }
    }
    fn query_at_mut(&mut self, when: When) -> DFGLocationQueryCursorMut<When, V, A, Self> {
        DFGLocationQueryCursorMut {
            dfg: self,
            addr: when,
            _a: std::marker::PhantomData,
            _v: std::marker::PhantomData,
        }
    }
}

pub struct DFGLocationQueryCursor<'dfg, K: Copy + Sized, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> {
    dfg: &'dfg D,
    addr: K,
    _a: std::marker::PhantomData<A>,
    _v: std::marker::PhantomData<V>,
}

pub struct DFGLocationQueryCursorMut<'dfg, K: Copy + Sized, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> {
    dfg: &'dfg mut D,
    addr: K,
    _a: std::marker::PhantomData<A>,
    _v: std::marker::PhantomData<V>,
}

impl<'dfg, K: Copy, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> DFGLocationQuery<V, A> for DFGLocationQueryCursor<'dfg, K, V, A, D> {
    type Indirect = D::Indirect;

    fn read_loc(&self, loc: A::Location) -> V {
        self.dfg.read_loc(self.addr, loc)
    }
    fn read<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> V {
        self.read_loc(loc.convert())
    }
    fn indirect_loc(&self, loc: A::Location) -> Self::Indirect {
        self.dfg.indirect_loc(self.addr, loc)
    }
    fn indirect<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> Self::Indirect {
        self.indirect_loc(loc.convert())
    }
}
impl<'dfg, K: Copy, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> DFGLocationQuery<V, A> for DFGLocationQueryCursorMut<'dfg, K, V, A, D> {
    type Indirect = D::Indirect;

    fn read_loc(&self, loc: A::Location) -> V {
        self.dfg.read_loc(self.addr, loc)
    }
    fn read<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> V {
        self.read_loc(loc.convert())
    }
    fn indirect_loc(&self, loc: A::Location) -> Self::Indirect {
        self.dfg.indirect_loc(self.addr, loc)
    }
    fn indirect<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> Self::Indirect {
        self.indirect_loc(loc.convert())
    }
}
impl<'dfg, K: Copy, V: Value, A: Arch + ValueLocations, D: DFG<V, A, K> + ?Sized> DFGLocationQueryMut<V, A> for DFGLocationQueryCursorMut<'dfg, K, V, A, D> {
    fn write_loc(&mut self, loc: A::Location, value: V) {
        self.dfg.write_loc(self.addr, loc, value)
    }
    fn write<T: ToDFGLoc<A::Location>>(&mut self, loc: &T, value: V) {
        self.write_loc(loc.convert(), value)
    }
}

/// it's relatively common to want to query a dfg at one address, for multiple values/read-write
/// accesses. `DFGLocationQuery` is essentially a helper to curry a specific address for future
/// queries.
pub trait DFGLocationQuery<V: Value, A: Arch + ValueLocations> where Self: Sized {
    type Indirect: IndirectQuery<V>;

    fn read_loc(&self, loc: A::Location) -> V;
    fn read<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> V {
        self.read_loc(loc.convert())
    }
    fn indirect_loc(&self, loc: A::Location) -> Self::Indirect;
    fn indirect<T: ToDFGLoc<A::Location>>(&self, loc: &T) -> Self::Indirect {
        self.indirect_loc(loc.convert())
    }
}

pub trait DFGLocationQueryMut<V: Value, A: Arch + ValueLocations> where Self: Sized + DFGLocationQuery<V, A> {
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

pub trait IndirectQuery<V> {
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

    /// check if `address` is a load known to this query cursor. get the referenced value if so,
    /// `None` if not.
    ///
    /// ## panics
    ///
    /// * if `self` does not represent a value that can be indexed. for example, the value backing
    /// a register can probably not be indexed. the value backing main memory probably can be
    /// indexed.
    fn try_get_load(&self, _address: ValueIndex<V>) -> Option<V>;

    /// check if `address` is a store known to this query cursor. `Some(())` if it is, `None` if not.
    ///
    /// ## panics
    ///
    /// * if `self` does not represent a value that can be indexed. for example, the value backing
    /// a register can probably not be indexed. the value backing main memory probably can be
    /// indexed.
    fn try_get_store(&self, _address: ValueIndex<V>) -> Option<()>;
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


impl<V: Value> IndirectQuery<V> for OpaqueIndirection<V> {
    fn load(&self, _: ValueIndex<V>) -> V {
        V::unknown()
    }

    fn store(&self, _: ValueIndex<V>, _: &V) { }

    fn try_get_load(&self, _: ValueIndex<V>) -> Option<V> {
        None
    }

    fn try_get_store(&self, _: ValueIndex<V>) -> Option<()> {
        None
    }
}

pub trait Value: Sized {
//    type Indirect: IndirectQuery<Self>; // associated type defaults are unstable, but should be `= OpaqueIndirection<V>`

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

use SSAValues;
use std::rc::Rc;
use analyses::static_single_assignment::DFGRef;
use data::types::TypeSpec;

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Item<Leaf> {
    pub ty: Option<TypeSpec>,
    pub value: Expression<Leaf>,
}

impl<Leaf: fmt::Display> fmt::Display for Item<Leaf> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<A: SSAValues> Item<ValueOrImmediate<A>> where A::Data: Eq + fmt::Display {
    pub fn immediate(v: u64) -> Rc<Self> {
        Self::untyped(Expression::value(ValueOrImmediate::Immediate(v)))
    }
}

use analyses::memory_layout::Underlying;
impl<A: SSAValues> Item<ValueOrImmediate<A>> where A::Data: Underlying<Target=DFGRef<A>> + Eq + fmt::Display {
    pub fn dealiased(&self) -> Rc<Self> {
        let ty = self.ty.clone();
        let value = self.value.dealiased();
        Rc::new(Item { ty, value })
    }
}

impl<A: SSAValues> Item<ValueOrImmediate<A>> where A::Data: Eq + fmt::Display{
}

impl<Leaf> Item<Leaf> {
    pub fn untyped(value: Expression<Leaf>) -> Rc<Self> {
        Rc::new(Item {
            ty: None,
            value,
        })
    }

    pub fn opaque(ty: TypeSpec) -> Rc<Self> {
        Rc::new(Item {
            ty: Some(ty),
            value: Expression::unknown()
        })
    }

    pub fn unknown() -> Rc<Self> {
        Self::untyped(Expression::unknown())
    }

    pub fn value(leaf: Leaf) -> Rc<Self> {
        Self::untyped(Expression::value(leaf))
    }

    pub fn loadb(self: &Rc<Self>) -> Rc<Self> {
        self.load(1)
    }

    pub fn loadw(self: &Rc<Self>) -> Rc<Self> {
        self.load(2)
    }

    pub fn loadd(self: &Rc<Self>) -> Rc<Self> {
        self.load(4)
    }

    pub fn loadq(self: &Rc<Self>) -> Rc<Self> {
        self.load(8)
    }

    pub fn load(self: &Rc<Self>, size: u8) -> Rc<Self> {
        Self::untyped(Expression::Load { address: Rc::clone(self), size })
    }

    pub fn add(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Add { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn sub(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Sub { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn mul(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Mul { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn or(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Or { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn and(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::And { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn xor(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Xor { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn shl(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Shl { value: Rc::clone(self), amount: Rc::clone(other) })
    }

    pub fn shr(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Self::untyped(Expression::Shr { value: Rc::clone(self), amount: Rc::clone(other) })
    }
}

/// a very literal construction of `Value` operations into an expression tree. if `Leaf` is only
/// concrete values, this is suitable for computation. if `Leaf` can be abstract variables, this is
/// becomes a symbolic expression tree. realistically these expressions are often a mix of the two;
/// constants from immediates and variables from the rest of the program.
#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Expression<Leaf> {
    Unknown,
    Value(Leaf),
    Load { address: Rc<Item<Leaf>>, size: u8 },
    // TODO: should there be a corresponding "store"?
    // Store { address: Rc<Item<Leaf>>, size: u8, value: Rc<Item<Leaf>> },
    Add { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    Sub { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    Mul { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    Or { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    And { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    Xor { left: Rc<Item<Leaf>>, right: Rc<Item<Leaf>> },
    Shl { value: Rc<Item<Leaf>>, amount: Rc<Item<Leaf>> },
    Shr { value: Rc<Item<Leaf>>, amount: Rc<Item<Leaf>> },
}

impl<Leaf: fmt::Display> fmt::Display for Expression<Leaf> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Unknown => {
                write!(f, "<unknown>")
            }
            Expression::Value(l) => {
                write!(f, "{}", l)
            }
            Expression::Load { address, size } => {
                write!(f, "[{}:{}]", address, size)
            }
            Expression::Add { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::Sub { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::Mul { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::Or { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::And { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::Xor { left, right } => {
                write!(f, "({} + {})", left, right)
            }
            Expression::Shl { value, amount } => {
                write!(f, "({} + {})", value, amount)
            }
            Expression::Shr { value, amount } => {
                write!(f, "({} + {})", value, amount)
            }
        }
    }
}

impl<T: SSAValues> Expression<ValueOrImmediate<T>> where T::Data: Eq + fmt::Display {
    pub fn as_immediate(&self) -> Option<u64> {
        if let Expression::Value(ValueOrImmediate::Immediate(i)) = self {
            Some(*i)
        } else {
            None
        }
    }
}

impl<A: SSAValues> Expression<ValueOrImmediate<A>> where A::Data: Underlying<Target=DFGRef<A>> + Eq + fmt::Display {
    pub fn dealiased(&self) -> Self {
        match self {
            Expression::Unknown => Expression::Unknown,
            Expression::Value(ValueOrImmediate::Immediate(i)) => Expression::Value(ValueOrImmediate::Immediate(*i)),
            Expression::Value(ValueOrImmediate::Value(v)) => {
                if let Some(underlying) = v.borrow().data.as_ref().and_then(|x| x.underlying()) {
                    Expression::Value(ValueOrImmediate::Value(underlying))
                } else {
                    Expression::Value(ValueOrImmediate::Value(Rc::clone(v)))
                }
            }
            Expression::Load { address, size } => Expression::Load { address: address.dealiased(), size: *size },
            Expression::Add { left, right } => Expression::Add {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::Sub { left, right } => Expression::Sub {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::Mul { left, right } => Expression::Mul {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::Or { left, right } => Expression::Or {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::And { left, right } => Expression::And {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::Xor { left, right } => Expression::Xor {
                left: left.dealiased(), right: right.dealiased(),
            },
            Expression::Shl { value, amount } => Expression::Shl {
                value: value.dealiased(), amount: amount.dealiased(),
            },
            Expression::Shr { value, amount } => Expression::Shr {
                value: value.dealiased(), amount: amount.dealiased(),
            },
        }
    }
}

impl<Leaf> Expression<Leaf> {
    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn value(leaf: Leaf) -> Self {
        Self::Value(leaf)
    }

    /*
    pub fn add(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Add { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn sub(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Sub { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn mul(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Mul { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn or(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Or { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn and(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::And { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn xor(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Xor { left: Rc::clone(self), right: Rc::clone(other) })
    }

    pub fn shl(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Shl { value: Rc::clone(self), amount: Rc::clone(other) })
    }

    pub fn shr(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Rc::new(Expression::Shr { value: Rc::clone(self), amount: Rc::clone(other) })
    }
    */
}

use std::hash::Hash;
use std::hash::Hasher;
pub enum ValueOrImmediate<A: SSAValues> where A::Data: Eq + fmt::Display {
    Immediate(u64),
    Value(DFGRef<A>),
}

impl<A: SSAValues> ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    pub fn immediate(v: u64) -> Expression<Self> {
        Expression::value(ValueOrImmediate::Immediate(v))
    }
}

use std::fmt;
impl<A: SSAValues> fmt::Debug for ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueOrImmediate::Immediate(v) => {
                write!(f, "Immediate({})", v)
            }
            ValueOrImmediate::Value(v) => {
                write!(f, "Value({:?})", v)
            }
        }
    }
}

impl<A: SSAValues> fmt::Display for ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueOrImmediate::Immediate(v) => {
                write!(f, "{}", v)
            }
            ValueOrImmediate::Value(v) => {
                let version_string = match v.borrow().version {
                    Some(v) => { v.to_string() },
                    None => "input".to_string()
                };
                if let Some(data) = v.borrow().data.as_ref() {
                    write!(f, "{} ({:?}_{})", data, v.borrow().location, version_string)
                } else {
                    write!(f, "<unknown value> ({:?}_{})", v.borrow().location, version_string)
                }
            }
        }
    }
}

impl<A: SSAValues> Eq for ValueOrImmediate<A> where A::Data: Eq + fmt::Display { }
impl<A: SSAValues> PartialEq for ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueOrImmediate::Immediate(l), ValueOrImmediate::Immediate(r)) => {
                l == r
            }
            (ValueOrImmediate::Value(l), ValueOrImmediate::Value(r)) => {
                Rc::ptr_eq(l, r)
            }
            _ => {
                false
            }
        }
    }
}

impl<A: SSAValues> Clone for ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    fn clone(&self) -> Self {
        match self {
            ValueOrImmediate::Immediate(v) => {
                ValueOrImmediate::Immediate(*v)
            }
            ValueOrImmediate::Value(v) => {
                ValueOrImmediate::Value(Rc::clone(v))
            }
        }
    }
}

impl<A: SSAValues> Hash for ValueOrImmediate<A> where A::Data: Eq + fmt::Display {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ValueOrImmediate::Immediate(v) => {
                state.write_u8(1);
                state.write_u64(*v);
            }
            ValueOrImmediate::Value(value) => {
                state.write_u8(2);
                (crate::analyses::static_single_assignment::HashedValue { value: Rc::clone(value) }).hash(state);
            }
        }
    }
}

use analyses::static_single_assignment::{DFGRebase, DefSource, HashedValue, SSA};
impl<A: SSAValues + Arch> DFGRebase<A> for Rc<Item<ValueOrImmediate<A>>> where A::Data: Hash + Eq + fmt::Display, A::Location: DFGRebase<A> {
    fn rebase_references(&self, old_dfg: &SSA<A>, new_dfg: &SSA<A>) -> Self {
        match &self.value {
            Expression::Unknown => Item::unknown(),
            Expression::Value(leaf) => {
                match leaf {
                    ValueOrImmediate::Immediate(v) => {
                        Item::untyped(Expression::Value(ValueOrImmediate::Immediate(*v)))
                    }
                    ValueOrImmediate::Value(v) => {
                        if !new_dfg.defs.contains_key(&HashedValue { value: Rc::clone(v) }) {
                            let (old_def_addr, old_def_source) = old_dfg.get_def_site(Rc::clone(v));
                            let new_use = match old_def_source {
                                DefSource::Instruction => {
                                    new_dfg.get_use(old_def_addr, v.borrow().location.rebase_references(old_dfg, new_dfg))
                                        .value
                                },
                                DefSource::External => {
                                    new_dfg.instruction_values.values()
                                        .find_map(|values| {
                                            values.iter().find_map(|((loc, dir), dfg_ref)| {
                                                if dir == &crate::data::Direction::Read && loc == &v.borrow().location && v.borrow().version.is_none() {
                                                    Some(Rc::clone(dfg_ref))
                                                } else {
                                                    None
                                                }
                                            })
                                        })
                                        .expect(&format!("corresponding external def exists for location {:?}", v.borrow().location))
                                }
                                DefSource::Between(addr) => {
                                    Rc::clone(new_dfg.control_dependent_values
                                        .get(&old_def_addr).expect("old def addr is valid")
                                        .get(&addr).expect("between's prior addr is valid")
                                        .get(&(v.borrow().location.rebase_references(old_dfg, new_dfg), crate::data::Direction::Write))
                                        .expect("corresponding def exists in new dfg"))
                                }
                                other => {
                                    panic!("aaaa {}", other);
                                }
                            };
                            Item::untyped(Expression::Value(ValueOrImmediate::Value(new_use)))
                        } else {
                            Item::untyped(Expression::Value(ValueOrImmediate::Value(Rc::clone(v))))
                        }
                    }
                }
            }
            Expression::Load { address, size } => {
                Item::load(&address.rebase_references(old_dfg, new_dfg), *size)
            },
            Expression::Add { left, right } => {
                Item::add(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Sub { left, right } => {
                Item::sub(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Mul { left, right } => {
                Item::mul(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Or { left, right } => {
                Item::or(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::And { left, right } => {
                Item::and(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Xor { left, right } => {
                Item::xor(
                    &left.rebase_references(old_dfg, new_dfg),
                    &right.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Shl { value, amount } => {
                Item::shl(
                    &value.rebase_references(old_dfg, new_dfg),
                    &amount.rebase_references(old_dfg, new_dfg),
                )
            },
            Expression::Shr { value, amount } => {
                Item::shr(
                    &value.rebase_references(old_dfg, new_dfg),
                    &amount.rebase_references(old_dfg, new_dfg),
                )
            },
        }
    }
}

impl<A: SSAValues> Value for Rc<Item<ValueOrImmediate<A>>> where A::Data: Hash + Eq + fmt::Display {
    // type Indirect = OpaqueIndirection<Self>;

    fn unknown() -> Self {
        Item::untyped(Expression::Unknown)
    }

    fn from_const(c: u64) -> Self {
        Item::untyped(Expression::Value(ValueOrImmediate::Immediate(c)))
    }

    fn from_set(_xs: &[Self]) -> Self {
        // TODO: tag the information loss here
        Self::unknown()
    }

    fn to_const(&self) -> Option<u64> {
        if let Expression::Value(ValueOrImmediate::Immediate(c)) = self.as_ref().value {
            Some(c)
        } else {
            None
        }
    }

    fn as_bool(&self) -> Option<bool> {
        self.to_const().map(|x| x != 0)
    }

    fn add(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::Add { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn sub(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::Sub { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn mul(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::Mul { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn or(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::Or { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn and(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::And { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn xor(&self, other: &Self) -> ValueRes<Self> {
        ValueRes {
            value: Item::untyped(Expression::Xor { left: Rc::clone(self), right: Rc::clone(other) }),
            carry: Self::unknown()
        }
    }

    fn sxt(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn zxt(&self, _width: &Self) -> Self {
        Self::unknown()
    }

    fn shr(&self, amount: &Self) -> Self {
        Item::untyped(Expression::Shr { value: Rc::clone(self), amount: Rc::clone(amount) })
    }

    fn shl(&self, amount: &Self) -> Self {
        Item::untyped(Expression::Shl { value: Rc::clone(self), amount: Rc::clone(amount) })
    }
}
