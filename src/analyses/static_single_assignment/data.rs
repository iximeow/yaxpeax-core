use std::rc::Rc;
use std::fmt::Debug;
use std::cell::RefCell;
use std::hash::Hash;
use std::collections::HashMap;

use serde::Serialize;

use yaxpeax_arch::Arch;

use data::types::Typed;

pub type DFGRef<A> = Rc<RefCell<Value<A>>>;
pub type RWMap<A> = HashMap<(<A as SSAValues>::Location, Direction), DFGRef<A>>;
pub type PhiLocations<A> = HashMap<<A as SSAValues>::Location, (DFGRef<A>, Vec<DFGRef<A>>)>;

// Look. Just rewrite this as a graph (one day). Vertices are DFGRef, edges are data
// dependences. Secondary graph with vertices (DFGRef | Address) where edges are Address -> DFGRef
// (define) -> Address (use of DFGRef)
//
// in the mean time, DFGRef are growing an (Address, Location) field lol
#[derive(Debug)]
pub struct SSA<A: Arch + SSAValues> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    // TODO: Fairly sure these Rc<RefCell<...>> could all just be raw pointers
    // these aren't individually freed so Rc shouldn't be necessary?
    pub values: HashMap<A::Address, RWMap<A>>,
    pub phi: HashMap<A::Address, PhiLocations<A>>
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
pub enum Direction {
    Read,
    Write
}

#[derive(Debug)]
pub struct Value<A: SSAValues> {
    // Temporarily necessary to map from some use back to a def site
    pub location: (A::Address, A::Location),
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
    pub fn new(addr: A::Address, location: A::Location, version: Option<u32>) -> Value<A> {
        Value {
            location: (addr, location),
            version: version,
            data: None
        }
    }
}

pub trait NoAliasing { }

impl <T> AliasInfo for T where T: NoAliasing + Clone + Copy {
    fn aliases_of(&self) -> Vec<Self> { vec![] }
    fn maximal_alias_of(&self) -> Self { self.clone() }
}

pub trait AliasInfo where Self: Sized {
    fn aliases_of(&self) -> Vec<Self>;
    fn maximal_alias_of(&self) -> Self;
}

pub trait SSAValues where Self: Arch {
    type Location: Debug + AliasInfo + Hash + Eq + Serialize;
    type Data: Debug + Hash + Clone + Typed;

    fn decompose(op: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)>;
}

impl <A: SSAValues> SSA<A> where A::Address: Hash + Eq, A::Location: Hash + Eq {
    fn get_value(&self, addr: A::Address, loc: A::Location, dir: Direction) -> Option<DFGRef<A>> {
        self.values.get(&addr)
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
}
