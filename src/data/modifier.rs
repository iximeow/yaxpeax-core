use yaxpeax_arch::Arch;

use std::marker::PhantomData;

use data::Direction;
use data::ValueLocations;

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
    fn before(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
    fn after(&self, addr: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
    fn between(&self, addr: A::Address, next: A::Address) -> Vec<(Option<A::Location>, Direction)> {
        vec![]
    }
}
