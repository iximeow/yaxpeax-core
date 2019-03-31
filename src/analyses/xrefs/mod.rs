use yaxpeax_arch::{Arch, Address};

use petgraph;
use petgraph::graphmap::{GraphMap, NodeTrait};

/*
 * If we can construct a global CFG, including call graphs,
 * it might be workable to identify happens-before relationships between
 * functions and data operations in functions
 *
 * eg it might be possible to statically show initializers will have run
 * before data is used?
 *
 * This would require a point to do analysis with respect to - a
 * "starting from this instruction, what will the state of the program
 * be there?"
 */

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub enum RefType {
    Code,
    Data
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub enum RefAction {
    Read,
    Write,
    Referrer // what, did you think this was HTTP?
}

pub struct XRefCollection<A: Address> {
    xrefs: GraphMap<(A, RefType, RefAction), (), petgraph::Directed>
}

impl <A: Address + NodeTrait> XRefCollection<A> {
    pub fn new() -> XRefCollection<A> {
        XRefCollection {
            xrefs: GraphMap::new()
        }
    }
    pub fn insert_from_code(&mut self, tpe: RefType, action: RefAction, from: A, to: A) {
        self.xrefs.add_edge(
            (from, RefType::Code, RefAction::Referrer),
            (to, tpe, action),
            ()
        );
    }

    pub fn delete_from_code(&mut self, tpe: RefType, action: RefAction, from: A, to: A) {
        self.xrefs.remove_edge(
            (from, RefType::Code, RefAction::Referrer),
            (to, tpe, action)
        );
    }
}

/*
 * This is not intended to live long. This does not support a notion of
 * iterative updates or modifications, removals. This does not play nice
 * with memory. This does not easily tie into updates w.r.t higher level
 * analyses.
 */
//pub fn build_xref_info<A: Arch, L, U, UTable>() -> XRefDatabase {
    /*
     * Build a collection of all reads/writes to all global-like locations.
     *
     * It would be nice to have xrefs w.r.t, say, stack local variables,
     * but that's not supportable at the moment, and needs fleshing out.
     *
     * f.ex what would be nice but is currently nonobvious is xrefs of X in
     *
     * a:
     * int X = 5;
     * int Y = X + 5;
     * return f(Y);
     *
     * f(Y):
     * return Y / 2;
     *
     * such that xrefs of a::Y would be a::f(Y)[param] and a::f(Y)::return
     *
     * but not f(Y) in general or some other b::f(Y) unless it was passed
     * as a parameter or global or something.
     */

//    XRefDatabase {}
//}
