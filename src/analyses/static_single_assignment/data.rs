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


use num_traits::Zero;

pub type DFGRef<A> = Rc<RefCell<Value<A>>>;
pub type RWMap<A> = HashMap<(<A as ValueLocations>::Location, Direction), DFGRef<A>>;
#[derive(Clone, Debug)]
pub struct PhiOp<A: SSAValues> { pub out: DFGRef<A>, pub ins: Vec<DFGRef<A>> }
pub type PhiLocations<A> = HashMap<<A as ValueLocations>::Location, PhiOp<A>>;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DefSource<A> {
    /// The defined value comes from an instruction in the underlying binary
    Instruction,
    /// The defined value comes from a phi pseudo-op
    Phi,
    /// The defined value is some custom definition - possibly automatically added or manually
    /// declared.
    Modifier(modifier::Precedence),
    /// Defined on the edge between two basic blocks - due to some value modifier, likely from
    /// conditionally defining a value after a conditional branch
    Between(A),
    /// Definition comes from some site before this DFG, such as the case for function arguments.
    External,
}

impl <A: yaxpeax_arch::AddressDisplay> fmt::Display for DefSource<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefSource::Instruction => write!(f, "instruction"),
            DefSource::Phi => write!(f, "phi"),
            DefSource::Modifier(modifier::Precedence::Before) => write!(f, "modifier (before)"),
            DefSource::Modifier(modifier::Precedence::After) => write!(f, "modifier (after)"),
            DefSource::Between(addr) => write!(f, "between ({})", addr.show()),
            DefSource::External => write!(f, "external")
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
    pub phi: HashMap<A::Address, PhiLocations<A>>,
    pub indirect_values: HashMap<A::Address, HashMap<A::Location, HashMap<(A::Data, Direction), DFGRef<A>>>>,
    // invariant:
    // for (k, v) in external_defs {
    //   assert_eq!(k, v.location);
    //   assert!(v.version.is_none());
    // }
    pub external_defs: HashMap<A::Location, DFGRef<A>>,
}

pub struct SSAQuery<'a, A: Arch + SSAValues> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    ssa: &'a SSA<A>,
    addr: A::Address
}

pub struct NoValueDescriptions;

impl<Location> ValueDescriptionQuery<Location> for NoValueDescriptions {
    fn modifier_name(&self, _loc: Location, _dir: Direction, _precedence: modifier::Precedence) -> Option<String> { None }
    fn modifier_value(&self, _loc: Location, _dir: Direction, _precedence: modifier::Precedence) -> Option<String> { None }
}

pub trait ValueDescriptionQuery<Location> {
    fn modifier_name(&self, loc: Location, dir: Direction, precedence: modifier::Precedence) -> Option<String>;
    fn modifier_value(&self, loc: Location, dir: Direction, precedence: modifier::Precedence) -> Option<String>;
}

impl<'a, A: Arch + SSAValues> ValueDescriptionQuery<A::Location> for SSAQuery<'a, A> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    fn modifier_name(&self, loc: A::Location, dir: Direction, precedence: modifier::Precedence) -> Option<String> {
        if let Some(rwmap) = self.ssa.modifier_values.get(&(self.addr, precedence)) {
            if let Some(entry) = rwmap.get(&(loc.clone(), dir)) {
                entry.borrow().name.as_ref().map(|x| x.clone()).or_else(|| {
                    let entry = entry.borrow();
                    if let Some(version) = entry.version() {
                        Some(format!("{:?}_{}", entry.location, version))
                    } else {
                        Some(format!("{:?}_input", entry.location))
                    }
                })
            } else {
                Some(format!("modifier values, but no entry for ({:?}, {:?})", loc.clone(), dir))
            }
        } else {
            Some(format!("no modifier values at ({}, {:?})", self.addr.show(), precedence))
        }
    }

    fn modifier_value(&self, loc: A::Location, dir: Direction, precedence: modifier::Precedence) -> Option<String> {
        if let Some(rwmap) = self.ssa.modifier_values.get(&(self.addr, precedence)) {
            if let Some(entry) = rwmap.get(&(loc.clone(), dir)) {
                entry.borrow().name.as_ref().map(|x| x.clone()).or_else(|| {
                    let entry = entry.borrow();
                    if let Some(data) = entry.data.as_ref() {
                        Some(format!("{:?})", data))
                    } else {
                        None
                    }
                })
            } else {
                Some(format!("modifier values, but no entry for ({:?}, {:?})", loc.clone(), dir))
            }
        } else {
            Some(format!("no modifier values at ({}, {:?})", self.addr.show(), precedence))
        }
    }
}

pub struct Value<A: SSAValues> where A::Data: Typed {
    pub name: Option<String>,
    pub location: A::Location,
    // None indicates "not written anywhere in this dfg", which indicates this value can
    // be considered an input from some enclosing control flow
    pub version: Option<u32>,
    pub data: Option<A::Data>,
    pub used: bool,
}

impl <A: SSAValues> fmt::Debug for Value<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Value {{ ")?;
        write!(f, "{:?}", self.location)?;
        if let Some(v) = self.version {
            write!(f, "_{}", v)?;
        } else {
            write!(f, "_input")?;
        }
        if let Some(name) = self.name.as_ref() {
            write!(f, ", \"{}\"", name)?;
        }
        if let Some(data) = self.data.as_ref() {
            write!(f, ", data: {:?}", data)?;
        } else {
            write!(f, ", data: None")?;
        }
        write!(f, " }}")
    }
}

impl <A: SSAValues> fmt::Display for Value<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.location)?;
        if let Some(v) = self.version {
            write!(f, "_{}", v)?;
        } else {
            write!(f, "_input")?;
        }
        if let Some(name) = self.name.as_ref() {
            write!(f, ", \"{}\"", name)?;
        }
        if let Some(data) = self.data.as_ref() {
            write!(f, ", data: {:?}", data)?;
        } else {
            write!(f, ", data: None")?;
        }
        write!(f, " }}")
    }
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

use std::cell::Ref;
impl <A: SSAValues> DFGLValue<A> {
    pub fn update(&self, new_data: A::Data) {
        // TODO: check to see if the new value conflicts with what we're setting?
        self.value.borrow_mut().data.replace(new_data);
    }
    pub fn replace(&self, new_data: Option<A::Data>) {
        // TODO: check to see if the new value conflicts with what we're setting?
        std::mem::drop(std::mem::replace(&mut self.value.borrow_mut().data, new_data));
    }
    pub fn clear(&self) {
        self.value.borrow_mut().data.take();
    }
    pub fn get_data(&self) -> Ref<Option<A::Data>> {
        Ref::map(self.value.borrow(), |v| &v.data)
    }
    pub fn as_rc(self) -> DFGRef<A> {
        self.value
    }
}

#[derive(Debug, Clone)]
pub struct HashedValue<A: Clone> {
    pub value: A
}

use std::hash::Hasher;
impl <A: SSAValues> Hash for HashedValue<DFGRef<A>> where Value<A>: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
//        let v: &RefCell<Value<A>> = &*self.value;
        (self.value.as_ptr()).hash(state);
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

pub struct ValueDisplay<'a, A: SSAValues> {
    value: &'a Value<A>,
    show_location: bool,
}

impl <'a, A: SSAValues> ValueDisplay<'a, A> {
    pub fn with_location(self) -> Self {
        ValueDisplay {
            value: self.value,
            show_location: true,
        }
    }
}

impl <'a, A: SSAValues> fmt::Display for ValueDisplay<'a, A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.show_location {
            if let Some(name) = self.value.name.as_ref() {
                write!(f, "{}", name)?;
                if let Some(v) = self.value.version {
                    write!(f, " [at {:?}_{}]", self.value.location, v)?;
                } else {
                    write!(f, " [at {:?}_input]", self.value.location)?;
                }
            } else {
                if let Some(v) = self.value.version {
                    write!(f, " {:?}_{}", self.value.location, v)?;
                } else {
                    write!(f, " {:?}_input", self.value.location)?;
                }
            }
        } else {
            if let Some(v) = self.value.version {
                write!(f, "{:?}_{}", self.value.location, v)?;
            } else {
                write!(f, "{:?}_input", self.value.location)?;
            }
        }

        if let Some(data) = self.value.data.as_ref() {
            write!(f, " (= {:?})", data)?;
        }

        Ok(())
    }
}

impl <A> Value<A> where A: SSAValues {
    pub fn version(&self) -> Option<u32> {
        self.version
    }

    pub fn display(&self) -> ValueDisplay<A> {
        ValueDisplay {
            value: self,
            show_location: false,
        }
    }
}

impl <A: SSAValues + Arch> Value<A> {
    pub fn new(location: A::Location, version: Option<u32>) -> Value<A> {
        Value {
            location: location,
            version: version,
            data: None,
            name: None,
            used: false,
        }
    }
}

pub trait SSAValues where Self: Arch + ValueLocations {
    type Data: Debug + Hash + Clone + Typed;
}

pub trait DFGRebase<A: SSAValues + Arch> {
    fn rebase_references(&self, old_dfg: &SSA<A>, new_dfg: &SSA<A>) -> Self;
}

impl <A: SSAValues> SSA<A> where A::Address: Hash + Eq, A::Location: Hash + Eq {
    pub fn log_contents(&self) {
        println!("instruction_values:");
        // pub instruction_values: HashMap<A::Address, RWMap<A>>,
        for (addr, rwmap) in self.instruction_values.iter() {
            println!("  {:?} -> {{", addr);
            for ((loc, dir), value) in rwmap.iter() {
                println!("    {:?} {:?} value={:?}", dir, loc, value);
            }
            println!("  }}");
        }
        println!("modifier_values:");
        // pub modifier_values: HashMap<(A::Address, modifier::Precedence), RWMap<A>>,
        for ((addr, precedence), rwmap) in self.modifier_values.iter() {
            println!("  {:?}, {:?} -> {:?}", addr, precedence, rwmap);
        }
        // pub control_dependent_values: HashMap<A::Address, HashMap<A::Address, RWMap<A>>>,
        // pub defs: HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource<A::Address>)>,
        // pub phi: HashMap<A::Address, PhiLocations<A>>,
        // pub indirect_values: HashMap<A::Address, HashMap<A::Location, HashMap<(A::Data, Direction), DFGRef<A>>>>,
        println!("external_defs:");
        // pub external_defs: HashMap<A::Location, DFGRef<A>>,
        for (loc, value) in self.external_defs.iter() {
            println!("  {:?} -> {:?}", loc, value);
        }
    }

    /// collect all locations that have uses of undefined data
    pub fn get_undefineds(&self) -> Vec<A::Location> {
        let mut undefineds = Vec::new();

        for map in self.instruction_values.values() {
            for dfgref in map.values() {
                let dfgref = dfgref.borrow();
                if dfgref.version == None && dfgref.used {
                    undefineds.push(dfgref.location.clone());
                }
            }
        }

        for map in self.modifier_values.values() {
            for dfgref in map.values() {
                let dfgref = dfgref.borrow();
                if dfgref.version == None && dfgref.used {
                    undefineds.push(dfgref.location.clone());
                }
            }
        }

        for map in self.control_dependent_values.values() {
            for innermap in map.values() {
                for dfgref in innermap.values() {
                    let dfgref = dfgref.borrow();
                    if dfgref.version == None && dfgref.used {
                        undefineds.push(dfgref.location.clone());
                    }
                }
            }
        }

        for map in self.phi.values() {
            for (loc, phiop) in map.iter() {
                if !phiop.out.borrow().used {
                    continue;
                }
                for inref in phiop.ins.iter() {
                    if inref.borrow().version == None {
                        undefineds.push(loc.clone());
                    }
                }
            }
        }

        undefineds
    }

    pub fn query_at<'a>(&'a self, addr: A::Address) -> SSAQuery<'a, A> {
        SSAQuery {
            ssa: self,
            addr
        }
    }
    pub fn get_value(&self, addr: A::Address, loc: A::Location, dir: Direction) -> Option<DFGRef<A>> {
        self.instruction_values.get(&addr)
            .and_then(|addr_values| addr_values.get(&(loc, dir)))
            .map(|x| Rc::clone(x))
    }
    pub fn get_transient_value(&self, from: A::Address, to: A::Address, loc: A::Location, dir: Direction) -> Option<DFGRef<A>> {
        self.control_dependent_values.get(&from)
            .and_then(|dests| dests.get(&to))
            .and_then(|values| values.get(&(loc, dir)))
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
        DFGLValue {
            value: self.get_value(addr, loc.clone(), Direction::Write)
                .unwrap_or_else(|| {
                    panic!("Failed to get def of {:?} at {}", loc.clone(), addr.show())
                })
        }
    }
    pub fn get_use(&self, addr: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue {
            value: self.get_value(addr, loc.clone(), Direction::Read)
                .unwrap_or_else(|| {
                    panic!("Failed to get use of {:?} at {}", loc.clone(), addr.show())
                })
        }
    }

    pub fn get_transient_def(&self, from: A::Address, to: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_transient_value(from, to, loc, Direction::Write).unwrap() }
    }
    pub fn get_transient_use(&self, from: A::Address, to: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_transient_value(from, to, loc, Direction::Read).unwrap() }
    }

    pub fn try_get_def_site(&self, value: DFGRef<A>) -> Option<&(A::Address, DefSource<A::Address>)> {
        self.defs.get(&HashedValue { value: Rc::clone(&value) })
    }

    pub fn get_def_site(&self, value: DFGRef<A>) -> (A::Address, DefSource<A::Address>) {
        match self.defs.get(&HashedValue { value: Rc::clone(&value) }) {
            Some(site) => *site,
            None => {
                // This is a rather serious bug - we have a value but it was never defined.
                // well. it should be a bug. this is probably reachable if the def is actually
                // external to the current control flow graph (for example, function arguments)
                // this should be backed by a set of fake defs at function entry
                //
                // for now, return an `External` def source. the use of address zero is kind of a
                // lazy hack around this function returning a concrete address. a better address to
                // return might be the entrypoint of this DFG?
                (A::Address::zero(), DefSource::External)
            }
        }
    }
}
