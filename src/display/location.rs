use yaxpeax_arch::Arch;
use data::ValueLocations;
use arch::x86_64::display::Use;

/// `OperandCursor` is a struct to select some operand and location in an instruction. for some
/// architectures, an operand may be the composition of multiple locations - consider the x86_64
/// example: `dword [rax + rcx * 4 + 0x54320]`. this may be the first operand in the enclosing
/// instruction, such as `mov dword [rax + rcx * 4 + 0x54320]`, and would thusly be referenced by
/// `OperandCursor { operand: 0, location: None }`. `rax` in the address expression would be
/// distinguished by `OperandCursor { operand: 0, location: Some(0) }`.
///
/// WIP - UI concerns in the above example:
/// `0x54320` _may_ be selectable as a location, but is left to implementors of `OperandScroll`
/// which will decide if `next()` and `prev()` would select or step over that location. For
/// example, if the immediate were small, it may be an offset into a struct, rather than a memory
/// location (such as the base of a static array or jump table).
pub struct OperandCursor {
    pub operand: u8,
    pub location: Option<u8>,
    /// some architectures have instructions where operands and locations may be implied by the
    /// instruction in question - architectures with flags regsters are typically good examples of
    /// this. in these cases, `impicit` exists to indicate to whatever uses this cursor that the
    /// operand or location being selected may not be explicitly drawn, and that extra work may be
    /// necessary to indicate the selection of the referenced element. for example, the x86 `je`
    /// instruction checks the zero flag, but is never explicitly shown. selecting `ZF` for
    /// interaction would be selecting an implicit operand.
    ///
    /// open question: how to interact with segment registers in real mode x86 programs, where we
    /// probably want to be able to scroll to segment registers, but for other x86 programs, likely
    /// do _not_ want to scroll to segment registers. in some cases we might though! is there a
    /// nice UI around needing to explicitly select a location, and skipping over it nominally?
    /// perhaps skipping in the implementation of `OperandScroll` and having ui elsewhere to pick
    /// `OperandCursor` explicitly.
    pub implicit: bool,
}

// expected that A will be some Arch::Instruction
pub trait OperandScroll<A> {
    fn next(&mut self, instr: &A) -> bool;
    fn prev(&mut self, instr: &A) -> bool;
    fn first(instr: &A) -> Self where Self: Sized;
}

pub trait LocationHighlighter<Loc> {
    // TODO: this could return an option of styling rules for the location?
    fn operand_style(&self, operand_number: u8) -> Option<()>;
    fn location_style(&self, loc: &(Loc, Use)) -> Option<()>;
}

pub struct NoHighlights;

impl <L> LocationHighlighter<L> for NoHighlights {
    fn operand_style(&self, operand_number: u8) -> Option<()> { None }
    fn location_style(&self, loc: &(L, Use)) -> Option<()> { None }
}

pub struct HighlightList<L: PartialEq> {
    // use the u8 as a bitmask of operands to highlight or not
    operands_bits: u8,
    // this might also be bitmask-able in some form? eventually??
    // this would be |L| * 2 bits, which might just be prohibitive to construct? or not. who knows.
    // for x86 this is already a u64 worth of bits. maybe something like a u64 bitmask for common
    // regs and an Option<Box<&[(L, Use)]>> for extras? that should turn into a pointer for the Box
    // and a pair of usize-sized values for the slice/length.
    location_highlights: Vec<(L, Use)>
}

impl <L: PartialEq> LocationHighlighter<L> for HighlightList<L> {
    fn operand_style(&self, operand_number: u8) -> Option<()> {
        if (self.operands_bits & (1 << operand_number)) != 0 {
            Some(())
        } else {
            None
        }
    }

    fn location_style(&self, loc: &(L, Use)) -> Option<()> {
        if self.location_highlights.contains(loc) {
            Some(())
        } else {
            None
        }
    }
}
