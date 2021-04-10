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

/// AliasInfo provides a description of aliasing rules for a given instruction set's `Location`s.
pub trait AliasInfo where Self: Sized {
    /// Retrieve all locations aliased by `self`. An x86_64 example, the `al` register aliases
    /// `ax`, `eax`, and `rax`, but *not* `ah`. This must not include `self` in the list of
    /// aliases.
    fn aliases_of(&self) -> Vec<Self>;
    /// Find the widest alias of `self`.
    ///
    /// TODO: This interface is probably incorrect. It may not be the case that there exists one
    /// widest alias. In memory analyses we may find a situation with values like:
    /// ```text
    /// Value A: | 0x0000   0x0001 |
    /// Value B:          | 0x0001   0x0002 |
    /// ```
    /// That is to say that value A and value B share the word at address `0x0001`. As a
    /// consequence, the byte at `0x0001` may not have an alias that aliases all others. Often we
    /// may be able to say "All of memory" as a widest alias, but it may exist that no such concept
    /// is appropriate for some location in some instruction set.
    fn maximal_alias_of(&self) -> Self;
}

/// `ValueLocations` allows decomposition of an instruction into a series of locations and an
/// indication of them being read or written. This defines the data flow relation between
/// instructions and all locations in programs.
///
/// NOTE: **`ValueLocations` is deprecated in favor of `LocIterator`.**
///
/// Implementation guidance: for correctness, `decompose` must express the most conservative
/// locations. As an example, x86_64 "push" should not be defined to use a stack-specific location
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
