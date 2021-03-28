use yaxpeax_arch::Arch;

use data::Direction;
use data::ValueLocations;
use arch::AbiDefaults;
use arch::FunctionImpl;
use arch::FunctionQuery;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum Precedence {
    Before,
    After
}

#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub struct Modifier<Addr, Expr> {
    location: (Addr, Precedence),
    modifier: Expr
}

impl <Addr: Copy + Clone, Expr> Modifier<Addr, Expr> {
    pub fn location(&self) -> (Addr, Precedence) {
        self.location.clone()
    }
}

pub trait ModifierCollection<A: Arch + ValueLocations> {
    fn before(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)>;
    fn after(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)>;
    fn between(&self, addr: A::Address, next: A::Address) -> Vec<(Option<A::Location>, Direction)>;
}

pub struct NoModifiers;

impl <A: Arch + ValueLocations> ModifierCollection<A> for NoModifiers {
    fn before(&self, _addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
    fn after(&self, _addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
    fn between(&self, _addr: A::Address, _next: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum ModifierExpression {
    /* At some point these should go through MemoSerialize, so these can be arbitrary expressions
    Below(analyses::data_flow::Data),
    Above(analyses::data_flow::Data),
    Is(analyses::data_flow::Data),
    IsNot(analyses::data_flow::Data)
    */
    Below(u64),
    Above(u64),
    Is(u64),
    IsNot(u64),
}

/// The `Vec<ModifierExpression>` are _conjunctions_. This differs from `ValueSet`, which uses a
/// sequence of values as disjunctions. This means there's no way to express a sparseset of values
/// as ModifierExpression, for the time being.
#[derive(Serialize, Deserialize, Debug)]
pub struct InstructionModifiers<A: Arch + ValueLocations> where A::Location: AbiDefaults {
    before: HashMap<A::Address, HashMap<Option<A::Location>, Vec<ModifierExpression>>>,
    after: HashMap<A::Address, HashMap<Option<A::Location>, Vec<ModifierExpression>>>,
    #[serde(skip)]
    between: HashMap<A::Address, HashMap<A::Address, HashMap<Option<A::Location>, Vec<ModifierExpression>>>>,
    pub calls: HashMap<A::Address, A::Address>,
    fn_query: Rc<RefCell<HashMap<A::Address, FunctionImpl<A::Location>>>>,
}

impl<A: Arch + ValueLocations> InstructionModifiers<A> where A::Location: AbiDefaults {
    pub fn new(fn_query: Rc<RefCell<HashMap<A::Address, FunctionImpl<A::Location>>>>) -> Self {
        InstructionModifiers {
            before: HashMap::new(),
            after: HashMap::new(),
            between: HashMap::new(),
            calls: HashMap::new(),
            fn_query,
        }
    }

    pub fn modifiers_between(&self, from: A::Address, to: A::Address) -> Option<&HashMap<Option<A::Location>, Vec<ModifierExpression>>> {
        self.between.get(&from).and_then(|tos| tos.get(&to))
    }

    pub fn add_edge_modifier(&mut self, from: A::Address, to: A::Address, loc: Option<A::Location>, expr: ModifierExpression) {
        let edges: &mut HashMap<A::Address, HashMap<Option<A::Location>, Vec<ModifierExpression>>> = self.between.entry(from).or_insert_with(|| HashMap::new());
        let edge: &mut HashMap<Option<A::Location>, Vec<ModifierExpression>> = edges.entry(to).or_insert_with(|| HashMap::new());
        let modifiers: &mut Vec<ModifierExpression> = edge.entry(loc).or_insert_with(|| Vec::new());
        modifiers.push(expr);
    }
}

impl<A: Arch + ValueLocations> ModifierCollection<A> for InstructionModifiers<A> where A::Location: AbiDefaults {
    fn between(&self, from: A::Address, to: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        let mut res = vec![];
        if let Some(modifiers) = self.modifiers_between(from, to) {
             for (k, vs) in modifiers.iter() {
                for v in vs {
                    match v {
                        ModifierExpression::IsNot(_) |
                        ModifierExpression::Below(_) |
                        ModifierExpression::Above(_) => {
                            res.push((k.to_owned(), Direction::Read));
                            res.push((k.to_owned(), Direction::Write));
                        }
                        ModifierExpression::Is(_) => {
                            res.push((k.to_owned(), Direction::Write));
                        }
                    }
                }
            }
        }
        res
   }

    fn before(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        let mut res = vec![];
        if let Some(modifiers) = self.before.get(&addr) {
            for (k, vs) in modifiers.iter() {
                for v in vs {
                    match v {
                        ModifierExpression::IsNot(_) |
                        ModifierExpression::Below(_) |
                        ModifierExpression::Above(_) => {
                            res.push((k.to_owned(), Direction::Read));
                            res.push((k.to_owned(), Direction::Write));
                        }
                        ModifierExpression::Is(_) => {
                            res.push((k.to_owned(), Direction::Write));
                        }
                    }
                }
            }
        }
        res
    }
    fn after(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        let mut res = vec![];
        if let Some(modifiers) = self.before.get(&addr) {
            for (k, vs) in modifiers.iter() {
                for v in vs {
                    match v {
                        ModifierExpression::IsNot(_) |
                        ModifierExpression::Below(_) |
                        ModifierExpression::Above(_) => {
                            res.push((k.to_owned(), Direction::Read));
                            res.push((k.to_owned(), Direction::Write));
                        }
                        ModifierExpression::Is(_) => {
                            res.push((k.to_owned(), Direction::Write));
                        }
                    }
                }
            }
        }
        if let Some(target) = self.calls.get(&addr) {
            if let Some(f) = (&*self.fn_query.borrow()).function_at(*target) {
                let layout = f.layout();
                for arg in layout.arguments.iter() {
                    res.push((arg.to_owned(), Direction::Read));
                }
                for arg in layout.clobbers.iter() {
                    res.push((arg.to_owned(), Direction::Write));
                }
                for arg in layout.returns.iter() {
                    res.push((arg.to_owned(), Direction::Write));
                }
                if let Some(ret) = &layout.return_address {
                    res.push((Some(ret.clone()), Direction::Read));
                }
                // we can add all the reads/writes of f.
            } else {
                // !! log an error somewhere !!
            }
        }
        res
    }
}
