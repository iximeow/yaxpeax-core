pub mod modifier;
pub mod types;

use yaxpeax_arch::{Address, Arch};

use arch::{AbiDefaults, FunctionImpl, FunctionQuery};

use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::fmt::Debug;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
pub enum Direction {
    Read,
    Write
}

/// `AliasInfo` provides a description of aliasing rules for a given instruction set's `Location`s.
pub trait AliasInfo where Self: Sized {
    /// retrieve all locations aliased by `self`. an x86_64 example, the `al` register aliases
    /// `ax`, `eax`, and `rax`, but *not* `ah`. this must not include `self` in the list of
    /// aliases. this aliasing relationship extends in both directions - `rax` aliases `eax`, `ax`,
    /// `al`, and `ah`.
    ///
    /// TODO: extend this for memory locations known through a disambiguator. `any[rsp_inout +
    /// 0x1234, 8]` aliases `any[rsp_input + 0x1238, 4]`, and this needs to be reported somehow for
    /// dfg construction.
    ///
    /// `aliases_of` (or some variant) should take a disambiguator and report all paritally- or
    /// fully-overlapping aliases with `Self`.
    fn aliases_of(&self) -> Vec<Self>;
    /// find the widest alias of `self`.
    ///
    /// TODO: this interface is probably incorrect. it may not be the case that there exists one
    /// widest alias. in memory analyses we may find a situation with values like:
    /// ```text
    /// Value A: | 0x0000   0x0001 |
    /// Value B:          | 0x0001   0x0002 |
    /// ```
    /// that is to say that value A and value B share the word at address `0x0001`. as a
    /// consequence, the byte at `0x0001` may not have an alias that aliases all others. often we
    /// may be able to say "all of memory" as a widest alias, but it may exist that no such concept
    /// is appropriate for some location in some instruction set.
    fn maximal_alias_of(&self) -> Self;
}

/// `ValueLocations` allows decomposition of an instruction into a series of locations and an
/// indication of them being read or written. this defines the data flow relation between
/// instructions and all locations in programs.
///
/// NOTE: **`ValueLocations` is deprecated in favor of `LocIterator`.**
///
/// implementation guidance: for correctness, `decompose` must express the most conservative
/// locations. as an example, x86_64 "push" should not be defined to use a stack-specific location
/// - `ValueLocations::decompose` should simply specify memory access, and allow a `Disambiguator`
/// with appropriate assumptions to refine the memory access into something appropriate for
/// analysis.
pub trait ValueLocations: Arch {
    type Location: Debug + Hash + Eq + Serialize + for<'de> Deserialize<'de> + Clone + AliasInfo;

    fn decompose(op: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)>;
}

/// Disambiguator is used with `LocIterator` to allow customizable refinement of locations in an
/// instruction. While in most architectures, registers are unambiguous, memory locations are often
/// much more complex. Disambiguation must be flexible because the actual scheme may vary program
/// to program, compiler to compiler, and binary to binary, all for the same architecture.
///
/// As an example for x86_64 variance, a maximally pessimistic analysis may assume all memory
/// accesses alias. This significantly complicates stack analysis, and for most programs it may be
/// acceptable to assume that stack-relative accesses do not alias, for example, heap accesses or
/// static data accesses. Because stack accesses in a single function often are all constant
/// offsets from a known pointer, a disambiguation to move all stack accesses into a stack-only
/// region we assume is not aliased by other memory means we can insert variables for stack memory
/// with a naive analysis. This same claim holds for globals and program accesses, with arbitrary
/// heap accesses still possibly requiring more intensive analysis.
pub trait Disambiguator<A: ValueLocations, LocSpec> {
    fn disambiguate(&self, instr: &A::Instruction, loc: (Option<A::Location>, Direction), spec: LocSpec) -> Option<A::Location>;
}

/// `LocationAliasDescriptions` is the rules describing how all locations in some data-flow graph
/// may overlap. it is a logical error for, as an example, for a `LocationAliasDescriptions` to be
/// used on a graph including locations from a `Disambiguator::disambiguate` that are not in
/// `Self`.
pub trait LocationAliasDescriptions<A: ValueLocations> {
    /// primarily for memory locations; returns `true` if `left` and `right` may refer to any of
    /// the same state, `false` if `left` and `right` are totally disjoint.
    /// ```
    /// Disambiguator::may_alias(rcx, ch)
    /// ```
    /// should always be true.
    /// ```
    /// Disambiguator::may_alias(any[rsp_input + 4, 4], any[rsp_input + 8, 4])
    /// ```
    /// should always be false.
    /// ```
    /// Disambiguator::may_alias(any[rcx_input + 4, 4], any[rsp_input + 4, 4])
    /// ```
    /// should be true if `rcx` or `rsp` are unknown, or are known to potentially alias.
    fn may_alias(&self, left: &A::Location, right: &A::Location) -> bool;

    /// what other locations may `loc` overlap with?
    /// returns the set of locations known to `Self`, which should be all locations in a given
    /// function, that can overlap with `loc`.
    /// TODO: some kind of iterator built on `&self` to avoid the vec alloc/collect...
    fn aliases_for(&self, loc: &A::Location) -> Vec<A::Location>;
}

pub trait LocIterator<'disambiguator, 'fns, A: ValueLocations, Location: 'static + AbiDefaults, D: Disambiguator<A, Self::LocSpec>, F: FunctionQuery<A::Address, Function=FunctionImpl<Location>>> {
    type Item;
    type LocSpec;
    type Iter: Iterator<Item = Self::Item>;
    // TODO:
    // this probably needs to grow to know about a table of functions and a mechanism to pick which
    // one(s? plural?) is called. plural, because `call [rbp]` may have a known finite set of
    // targets, and a perfectly fine analysis would consider the union of all reads and writes
    fn iter_locs(self, loc: A::Address, _: &'disambiguator D, functions: &'fns F) -> Self::Iter;
}

/*
#[allow(unused)]
macro_rules! impl_loc_iterator_transition {
    ($arch:ty) => {
        impl <'a> LocIterator<$arch::Location> for &'a $arch::Instruction {
            type Item = (Option<$arch::Location>, Direction);
            type Iter = String;
            fn iter_locs<D: Disambiguator<$arch::Location>>(self, _: &mut D) -> Self::Iter {
                $arch::decompose(self).iter_locs()
            }
        }
    }
}
*/
