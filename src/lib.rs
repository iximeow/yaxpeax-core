extern crate siphasher;
extern crate goblin;
#[macro_use] extern crate serde_derive;
extern crate serde;
extern crate serde_json;
extern crate termion;
extern crate petgraph;
extern crate num_traits;
extern crate nix;
extern crate proc_maps;
extern crate tracing;

extern crate yaxpeax_arch;

extern crate yaxpeax_arm;
extern crate yaxpeax_x86;
extern crate yaxpeax_msp430_mc;
extern crate yaxpeax_pic17;
extern crate yaxpeax_pic18;
extern crate yaxpeax_pic24;

pub mod arch;
pub mod analyses;
pub mod data;
pub mod debug;
pub mod display;
pub mod memory;
pub mod parts;
pub mod comment;
pub mod serialize;

use yaxpeax_arch::{Arch, ColorSettings};

use std::hash::Hash;
use std::collections::HashMap;

pub trait ContextTable<A: Arch + ?Sized, Ctx, CtxUpdate>: ContextRead<A, Ctx> + ContextWrite<A, CtxUpdate> { }

pub trait ContextRead<A: Arch + ?Sized, Ctx> {
    fn at(&self, address: &<A as Arch>::Address) -> Ctx;
}

pub trait ContextWrite<A: Arch + ?Sized, CtxUpdate> {
    fn put(&mut self, address: <A as Arch>::Address, update: CtxUpdate);
}

// impl <'a, T, A: Arch, Ctx, CtxUpdate> ContextTable<A, Ctx, CtxUpdate> for &'a mut T where &'a mut T: ContextRead<A, Ctx> + ContextWrite<A, CtxUpdate> { }

pub trait SyntaxedRender<A, T, F> {
    fn render(&self, context: Option<&T>, function_table: &HashMap<A, F>) -> String;
}

use analyses::static_single_assignment::SSAValues;
use analyses::static_single_assignment::SSA;
use data::ValueLocations;
pub trait SyntaxedSSARender<Architecture: Arch + SSAValues, T, F> where
    <Architecture as Arch>::Address: Eq + Hash,
    <Architecture as ValueLocations>::Location: Eq + Hash,
{
    fn render_with_ssa_values(
        &self,
        address: <Architecture as Arch>::Address,
        colors: Option<&ColorSettings>,
        context: Option<&T>,
        function_table: &HashMap<<Architecture as Arch>::Address, F>,
        ssa: &SSA<Architecture>) -> String;
}
