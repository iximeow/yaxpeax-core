use std::rc::Rc;
use std::fmt::Debug;
use std::fmt;
use std::cell::RefCell;
use std::hash::Hash;
use std::collections::HashMap;

use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressDisplay;

use data::types::Typed;
use data::modifier;
use data::ValueLocations;
use data::Direction;

pub type DFGRef<A> = Rc<RefCell<Value<A>>>;
pub type RWMap<A> = HashMap<(<A as ValueLocations>::Location, Direction), DFGRef<A>>;
#[derive(Clone, Debug)]
pub struct PhiOp<A: SSAValues> { pub out: DFGRef<A>, pub ins: Vec<DFGRef<A>> }
pub type PhiLocations<A> = HashMap<<A as ValueLocations>::Location, PhiOp<A>>;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DefSource<A: AddressDisplay> {
    /// The defined value comes from an instruction in the underlying binary
    Instruction,
    /// The defined value comes from a phi pseudo-op
    Phi,
    /// The defined value is some custom definition - possibly automatically added or manually
    /// declared.
    Modifier(modifier::Precedence),
    /// Defined on the edge between two basic blocks - due to some value modifier, likely from
    /// conditionally defining a value after a conditional branch
    Between(A)
}

impl <A: AddressDisplay> fmt::Display for DefSource<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefSource::Instruction => write!(f, "instruction"),
            DefSource::Phi => write!(f, "phi"),
            DefSource::Modifier(modifier::Precedence::Before) => write!(f, "modifier (before)"),
            DefSource::Modifier(modifier::Precedence::After) => write!(f, "modifier (after)"),
            DefSource::Between(addr) => write!(f, "between ({:?})", addr.stringy())
        }
    }
}

// Look. Just rewrite this as a graph (one day). Vertices are DFGRef, edges ae data
// dependences. Secondary graph with vertices (DFGRef | Address) where edges are Address -> DFGRef
// (define) -> Address (use of DFGRef)
//
// in the mean time, DFGRef are growing an (Address, Location) field lol
#[derive(Debug)]
pub struct SSA<A: Arch + SSAValues> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    // TODO: Fairly sure these Rc<RefCell<...>> could all just be raw pointers
    // these aren't individually freed so Rc shouldn't be necessary?
    pub instruction_values: HashMap<A::Address, RWMap<A>>,
    pub modifier_values: HashMap<(A::Address, modifier::Precedence), RWMap<A>>,
    pub control_dependent_values: HashMap<A::Address, HashMap<A::Address, RWMap<A>>>,
    pub defs: HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource<A::Address>)>,
    pub phi: HashMap<A::Address, PhiLocations<A>>
}

#[derive(Debug)]
pub struct Value<A: SSAValues> {
    pub location: A::Location,
    // None indicates "not written anywhere in this dfg", which indicates this value can
    // be considered an input from some enclosing control flow
    pub version: Option<u32>,
    pub data: Option<A::Data>
}

impl <A: SSAValues> Hash for Value<A> where A::Location: Hash, A::Data: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.location.hash(state);
        self.version.hash(state);
        self.data.hash(state);
    }
}

pub struct DFGLValue<A: SSAValues> {
    pub value: DFGRef<A>
}

impl <A: SSAValues> DFGLValue<A> {
    pub fn update(&self, new_data: A::Data) {
        self.value.borrow_mut().data.replace(new_data);
    }
    pub fn get_data(&self) -> Option<A::Data> {
        self.value.borrow().data.clone()
    }
    pub fn as_rc(self) -> DFGRef<A> {
        self.value
    }
}

#[derive(Debug)]
pub struct HashedValue<A> {
    pub value: A
}

use std::hash::Hasher;
impl <A: SSAValues> Hash for HashedValue<DFGRef<A>> where Value<A>: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let v: &RefCell<Value<A>> = &*self.value;
        (v.borrow()).hash(state);
    }
}

impl <A: SSAValues> Eq for HashedValue<DFGRef<A>> { }

impl <A: SSAValues> PartialEq for HashedValue<DFGRef<A>> {
    fn eq(&self, other: &HashedValue<DFGRef<A>>) -> bool {
        Rc::ptr_eq(&self.value, &other.value)
    }
}

impl <A: SSAValues> PartialEq for Value<A> {
    fn eq(&self, rhs: &Value<A>) -> bool {
        self as *const Value<A> == rhs as *const Value<A>
    }
}
impl <A: SSAValues> Eq for Value<A> {}

impl <A> Value<A> where A: SSAValues {
    pub fn version(&self) -> Option<u32> {
        self.version
    }
}

impl <A: SSAValues + Arch> Value<A> {
    pub fn new(location: A::Location, version: Option<u32>) -> Value<A> {
        Value {
            location: location,
            version: version,
            data: None
        }
    }
}

pub trait SSAValues where Self: Arch + ValueLocations {
    type Data: Debug + Hash + Clone + Typed;
}

impl <A: SSAValues> SSA<A> where A::Address: Hash + Eq, A::Location: Hash + Eq {
    pub fn get_value(&self, addr: A::Address, loc: A::Location, dir: Direction) -> Option<DFGRef<A>> {
        self.instruction_values.get(&addr)
            .and_then(|addr_values| addr_values.get(&(loc, dir)))
            .map(|x| Rc::clone(x))
    }
    pub fn try_get_def(&self, addr: A::Address, loc: A::Location) -> Option<DFGRef<A>> {
        self.get_value(addr, loc, Direction::Write)
    }
    pub fn try_get_use(&self, addr: A::Address, loc: A::Location) -> Option<DFGRef<A>> {
        self.get_value(addr, loc, Direction::Read)
    }
    // TODO: These should have a #[cfg()] flag to use after heavy fuzzing that does
    // unreachable_unchecked!() for the None case here.
    //
    // that flag should also remove the try_get_* variants
    pub fn get_def(&self, addr: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_value(addr, loc, Direction::Write).unwrap() }
    }
    pub fn get_use(&self, addr: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_value(addr, loc, Direction::Read).unwrap() }
    }

    pub fn get_def_site(&self, value: DFGRef<A>) -> (A::Address, DefSource<A::Address>) {
        match self.defs.get(&HashedValue { value: Rc::clone(&value) }) {
            Some(site) => *site,
            None => {
                // This is a rather serious bug - we have a value but it was never defined.
                // well. it should be a bug. this is probably reachable if the def is actually
                // external to the current control flow graph (for example, function arguments)
                // this should be backed by a set of fake defs at function entry
                unreachable!("use with no def");
            }
        }
    }
}
