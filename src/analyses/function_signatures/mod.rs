use yaxpeax_arch::Arch;
use siphasher::sip128::SipHasher13;
use memory::MemoryRepr;
use analyses::control_flow::ControlFlowGraph;

pub enum IncompleteSignature {
    MissingData,
    Uncomputable
}

type SignatureResult<T> = Result<T, IncompleteSignature>;

trait FunctionSignatory<A: Arch> {
    type Signature;
    type Data;

    fn signature_of<M: MemoryRepr<A::Address>>(cfg: &ControlFlowGraph<A::Address>, data: &Self::Data, memory: &M) -> SignatureResult<Self::Signature>;
}

pub struct FunctionByteSignatory;

/*
impl FunctionSignatory<x86_64> for FunctionByteSignatory {
    type Signature = u64;
    type Data = ();

    /// A signature built from siphash 1-3 over instructions, in a BFS traversal
    /// with a particular depth ordered by start address. Additionally, this discards immediate
    /// offsets larger than some magic number. This is to have some semblance of generalizability
    /// if functions move relative to each other.
    fn signature_of<M: MemoryRepr<A::Address>>(
        cfg: &ControlFlowGraph<A::Address>,
        data: &Self::Data,
        memory: &M
    ) -> SignatureResult<Self::Signature> {
        let _hasher = SipHasher13::new();
    }
}
*/

pub struct FunctionMnemonicSignatory;

impl <A: Arch> FunctionSignatory<A> for FunctionMnemonicSignatory {
    type Signature = u64;
    type Data = ();

    /// A signature built from siphash 1-3 over instruction mnemonics, in a BFS traversal
    /// with a particular depth ordered by start address. This, subsequently, is not
    /// robust in the face of basic block reordering.
    fn signature_of<M: MemoryRepr<A::Address>>(
        _cfg: &ControlFlowGraph<A::Address>,
        _data: &Self::Data,
        _memory: &M
    ) -> SignatureResult<Self::Signature> {
        let _hasher = SipHasher13::new();
        Err(IncompleteSignature::Uncomputable)
    }
}

pub struct FunctionBlockSignatory;

impl <A: Arch> FunctionSignatory<A> for FunctionBlockSignatory {
    type Signature = Vec<u64>;
    type Data = ();

    /// A signature in the form of siphash 1-3 signatures of basic blocks in this function
    /// topologically ordered from the function entry point. This should be robust to
    /// basic block reordering, and also permits an edit distance style fuzzy equivalence
    fn signature_of<M: MemoryRepr<A::Address>>(
        _cfg: &ControlFlowGraph<A::Address>,
        _data: &Self::Data,
        _memory: &M
    ) -> SignatureResult<Self::Signature> {
        let _hasher = SipHasher13::new();
        Err(IncompleteSignature::Uncomputable)
    }
}

pub struct FunctionOrderFreeSignatory;

impl <A: Arch> FunctionSignatory<A> for FunctionOrderFreeSignatory {
    type Signature = Vec<u64>;
    type Data = ();

    /// A signature in the form of siphash 1-3 signatures of basic blocks in this function with
    /// control flow-sensitive instructions normalized to some branch-insensitive constant.
    /// This is ad-hoc, but the intention is to be robust in the face of changes like branches
    /// being negated due to compilers changing code layout or something similar. For an x86
    /// example, the following snippets should have the same signature:
    ///
    /// mov eax, [rsp + 0x124]
    /// xor ecx, ecx
    /// cmp eax, ecx
    /// jb B
    ///A:
    /// ; do A
    /// jmp C
    ///B
    /// ; do B
    ///C
    /// ret
    ///
    /// mov eax, [rsp + 0x124]
    /// xor ecx, ecx
    /// cmp eax, ecx
    /// jae B
    ///A:
    /// ; do B
    /// jmp C
    ///B
    /// ; do A
    ///C
    /// ret
    ///
    /// by virtue of nulling the jb/jae.
    fn signature_of<M: MemoryRepr<A::Address>>(
        _cfg: &ControlFlowGraph<A::Address>,
        _data: &Self::Data,
        _memory: &M
    ) -> SignatureResult<Self::Signature> {
        let _hasher = SipHasher13::new();
        Err(IncompleteSignature::Uncomputable)
    }
}

// and something clever about data flow to ditch architecture-specific signatures
