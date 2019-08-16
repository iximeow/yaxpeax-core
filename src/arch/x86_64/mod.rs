use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::static_single_assignment::SSA;
use analyses::xrefs;

use std::collections::HashMap;
use yaxpeax_x86::{x86_64, Opcode, Operand};
use std::rc::Rc;
use std::cell::RefCell;
use num_traits::Zero;

use arch::{BaseUpdate, CommentQuery, Function, FunctionQuery, Symbol, SymbolQuery, Library};
use data::{Direction, ValueLocations};

use ContextRead;
use ContextWrite;

pub mod analyses;
pub mod cpu;
pub mod debug;
pub mod display;

#[derive(Serialize)]
#[allow(non_camel_case_types)]
pub struct x86_64Data {
    pub preferred_addr: <x86_64 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>,
    pub ssa: HashMap<<x86_64 as Arch>::Address, (
        control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>,
        SSA<x86_64>
    )>,
}

impl FunctionQuery<<x86_64 as Arch>::Address> for x86_64Data {
    type Function = Function;
    fn function_at(&self, addr: <x86_64 as Arch>::Address) -> Option<&Function> {
        self.contexts.function_at(addr)
    }
}

impl CommentQuery<<x86_64 as Arch>::Address> for x86_64Data {
    fn comment_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&str> {
        self.contexts.comment_for(addr)
    }
}

impl SymbolQuery<<x86_64 as Arch>::Address> for x86_64Data {
    fn symbol_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&Symbol> {
        self.contexts.symbol_for(addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<x86_64 as Arch>::Address> {
        self.contexts.symbol_addr(sym)
    }
}

impl FunctionQuery<<x86_64 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, addr: <x86_64 as Arch>::Address) -> Option<&Function> {
        self.functions.get(&addr)
    }
}

impl CommentQuery<<x86_64 as Arch>::Address> for MergedContextTable {
    fn comment_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl SymbolQuery<<x86_64 as Arch>::Address> for MergedContextTable {
    fn symbol_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<x86_64 as Arch>::Address> {
        for (k, v) in self.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl Default for x86_64Data {
    fn default() -> Self {
        x86_64Data {
            preferred_addr: <x86_64 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            ssa: HashMap::new(),
        }
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
#[derive(Serialize)]
pub struct InstructionModifiers {
    before: HashMap<<x86_64 as Arch>::Address, HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>>>,
    after: HashMap<<x86_64 as Arch>::Address, HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>>>,
    between: HashMap<<x86_64 as Arch>::Address, HashMap<<x86_64 as Arch>::Address, HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>>>>
}

impl InstructionModifiers {
    pub fn new() -> Self {
        InstructionModifiers {
            before: HashMap::new(),
            after: HashMap::new(),
            between: HashMap::new(),
        }
    }

    pub fn modifiers_between(&self, from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address) -> Option<&HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>>> {
        self.between.get(&from).and_then(|tos| tos.get(&to))
    }

    pub fn add_edge_modifier(&mut self, from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address, loc: Option<<x86_64 as ValueLocations>::Location>, expr: ModifierExpression) {
        let edges: &mut HashMap<<x86_64 as Arch>::Address, HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>>> = self.between.entry(from).or_insert_with(|| HashMap::new());
        let edge: &mut HashMap<Option<<x86_64 as ValueLocations>::Location>, Vec<ModifierExpression>> = edges.entry(to).or_insert_with(|| HashMap::new());
        let modifiers: &mut Vec<ModifierExpression> = edge.entry(loc).or_insert_with(|| Vec::new());
        modifiers.push(expr);
    }
}

use data::modifier::ModifierCollection;

impl ModifierCollection<x86_64> for InstructionModifiers {
    fn between(&self, from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address) -> Vec<(Option<<x86_64 as ValueLocations>::Location>, Direction)> {
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

    fn before(&self, addr: <x86_64 as Arch>::Address) -> Vec<(Option<<x86_64 as ValueLocations>::Location>, Direction)> {
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
    fn after(&self, addr: <x86_64 as Arch>::Address) -> Vec<(Option<<x86_64 as ValueLocations>::Location>, Direction)> {
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
}

#[derive(Serialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub computed_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub comments: HashMap<<x86_64 as Arch>::Address, String>,
    pub xrefs: xrefs::XRefCollection<<x86_64 as Arch>::Address>,
    pub symbols: HashMap<<x86_64 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <x86_64 as Arch>::Address>,
    pub functions: HashMap<<x86_64 as Arch>::Address, Function>,
    pub function_data: HashMap<<x86_64 as Arch>::Address, RefCell<InstructionModifiers>>,
    pub function_hints: Vec<<x86_64 as Arch>::Address>,
}

#[derive(Debug)]
pub struct MergedContext {
    pub computed: Option<Rc<()>>,
    pub user: Option<Rc<()>>
}

impl Default for MergedContextTable {
    fn default() -> Self {
        MergedContextTable::create_empty()
    }
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            comments: HashMap::new(),
            xrefs: xrefs::XRefCollection::new(),
            functions: HashMap::new(),
            function_hints: Vec::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
            function_data: HashMap::new(),
        }
    }
}

/*
pub enum Value {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u64, u64),
    U256(u64, u64, u64, u64),
    U512(u64, u64, u64, u64, u64, u64, u64, u64)
}

trait PartialInstructionContext {
    pub fn reg_value(reg: RegSpec) -> Option<Value>;
    pub fn mem_value(addr: u64, width: u8) -> Option<Value>;
}
*/

pub type Update = BaseUpdate<x86Update>;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum x86Update {
    AddXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    RemoveXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    FunctionHint
}

impl ContextRead<x86_64, MergedContext> for MergedContextTable {
    fn at(&self, address: &<x86_64 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<x86_64, Update> for MergedContextTable {
    fn put(&mut self, address: <x86_64 as Arch>::Address, update: Update) {
        // println!("Applying update: {} -> {:?}", address.stringy(), update);
        match update {
            BaseUpdate::Specialized(x86Update::FunctionHint) => {
                if !self.function_hints.contains(&address) && !self.functions.contains_key(&address) {
//                    println!("Function hint: {}", address.stringy());
                    self.function_hints.push(address)
                }
            },
            BaseUpdate::Specialized(x86Update::AddXRef(tpe, action, dest)) => {
                // TODO: xrefs from non-code sources
                self.xrefs.insert_from_code(tpe, action, address, dest);
            },
            BaseUpdate::Specialized(x86Update::RemoveXRef(tpe, action, dest)) => {
                self.xrefs.delete_from_code(tpe, action, address, dest);
            }
            BaseUpdate::DefineSymbol(sym) => {
                //println!("address of {:?} recorded at {}", sym, address.stringy());
                match Symbol::to_function(&sym) {
                    Some(f) => {
                        self.functions.insert(address, f);
                    }
                    None => { }
                }
                self.symbols.insert(address, sym.clone());
                self.reverse_symbols.insert(sym, address);
            }
            BaseUpdate::DefineFunction(f) => {
                self.symbols.insert(address, Symbol(Library::This, f.name.clone()));
                self.reverse_symbols.insert(Symbol(Library::This, f.name.clone()), address);
                self.functions.insert(address, f.clone());
            }
            BaseUpdate::AddCodeComment(comment) => {
                self.comments.insert(address, comment);
            }
            _ => { /* todo: the rest */ }
        }
    }
}

impl <T> control_flow::Determinant<T, <x86_64 as Arch>::Address> for yaxpeax_x86::Instruction {
    // TODO: this assumes that instructions won't fault
    // we really don't know that, but also no T provided here gives
    // context such that we can make that determination
    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<x86_64 as Arch>::Address> {
        match self.opcode {
            Opcode::XADD |
            Opcode::MOVSS |
            Opcode::SQRTSS |
            Opcode::ADDSS |
            Opcode::SUBSS |
            Opcode::MULSS |
            Opcode::DIVSS |
            Opcode::MINSS |
            Opcode::MAXSS |
            Opcode::MOVSLDUP |
            Opcode::CVTSI2SS |
            Opcode::CVTTSS2SI |
            Opcode::CVTSS2SI |
            Opcode::CVTSS2SD |
            Opcode::MOVSD |
            Opcode::SQRTSD |
            Opcode::ADDSD |
            Opcode::SUBSD |
            Opcode::MULSD |
            Opcode::DIVSD |
            Opcode::MINSD |
            Opcode::MAXSD |
            Opcode::MOVDDUP |
            Opcode::HADDPS |
            Opcode::HSUBPS |
            Opcode::ADDSUBPS |
            Opcode::CVTSI2SD |
            Opcode::CVTTSD2SI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::LDDQU |
            Opcode::MOVSX_b |
            Opcode::MOVSX_w |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOVSXD |
            Opcode::SAR |
            Opcode::SAL |
            Opcode::SHR |
            Opcode::SHL |
            Opcode::RCR |
            Opcode::RCL |
            Opcode::ROR |
            Opcode::ROL |
            Opcode::INC |
            Opcode::DEC |
            Opcode::SBB |
            Opcode::AND |
            Opcode::XOR |
            Opcode::OR |
            Opcode::PUSH |
            Opcode::POP |
            Opcode::LEA |
            Opcode::NOP |
            Opcode::XCHG |
            Opcode::POPF |
            Opcode::ADD |
            Opcode::ADC |
            Opcode::SUB |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::MOV |
            Opcode::PUSHF |
            Opcode::WAIT |
            Opcode::CBW |
            Opcode::CDW |
            Opcode::LAHF |
            Opcode::SAHF |
            Opcode::TEST |
            Opcode::CMP |
            Opcode::INS |
            Opcode::OUTS |
            Opcode::IMUL |
            Opcode::DIV |
            Opcode::IDIV |
            Opcode::MUL |
            Opcode::NEG |
            Opcode::NOT |
            Opcode::SGDT |
            Opcode::SIDT |
            Opcode::SMSW |
            Opcode::LGDT |
            Opcode::LIDT |
            Opcode::LMSW |
            Opcode::SWAPGS |
            Opcode::RDTSCP |
            Opcode::INVLPG |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::CPUID |
            Opcode::LSL |
            Opcode::LAR |
            Opcode::CLTS |
            Opcode::SYSCALL |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::LDMXCSR |
            Opcode::STMXCSR |
            Opcode::XSAVE |
            Opcode::XSTOR |
            Opcode::XSAVEOPT |
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::CLFLUSH |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::LLDT |
            Opcode::LTR |
            Opcode::VERR |
            Opcode::VERW |
            Opcode::JMPE |
            Opcode::RDMSR |
            Opcode::WRMSR |
            Opcode::RDTSC |
            Opcode::RDPMC |
            Opcode::CLI |
            Opcode::STI |
            Opcode::CLC |
            Opcode::STC |
            Opcode::CLD |
            Opcode::STD |
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSR |
            Opcode::BSF |
            Opcode::CMPXCHG => {
                control_flow::Effect::cont()
            },
            Opcode::CALLF => {
                // TODO: honestly not sure how to model callf
                control_flow::Effect::stop()
            },
            Opcode::CALL => {
                // TODO: this is where i ought to reference context
                // to determine that the called address begins a well-formed
                // region that may or may not let the caller consider "call"
                // a single non-effectual instruction w.r.t control flow
                let dest = match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI16(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI32(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI64(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    _ => {
                        // TODO one day when ctx can let this reach ... the current
                        // exeuction context ... this may be able to be smarter
                        // (f.ex, if this jumps to a jump table, 
                        None
                    }
                };

                match dest {
                    Some(_dest) => {
                    //    control_flow::Effect::cont_and(dest)
                        control_flow::Effect::cont()
                    },
                    None => control_flow::Effect::cont()
                }

            }
            Opcode::JMP => {
                let dest = match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI16(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI32(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI64(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    _ => {
                        // TODO one day when ctx can let this reach ... the current
                        // exeuction context ... this may be able to be smarter
                        // (f.ex, if this jumps to a jump table, 
                        None
                    }
                };

                match dest {
                    Some(dest) => {
                        control_flow::Effect::stop_and(dest)
                    },
                    None => control_flow::Effect::stop()
                }
            },
            Opcode::JMPF => {
                // TODO: ...
                control_flow::Effect::stop()
            },
            Opcode::INT |
            Opcode::INTO |
            Opcode::IRET |
            Opcode::RETF |
            Opcode::SYSRET |
            Opcode::RETURN => {
                control_flow::Effect::stop()
            }
            Opcode::HLT => {
                control_flow::Effect::stop()
            },
            Opcode::Jcc(_) => {
                match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI16(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI32(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI64(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    _ => {
                        unreachable!()
                    }
                }
            },

            Opcode::UD2 |
            Opcode::Invalid => {
                control_flow::Effect::stop()
            },

            Opcode::MOVcc(_) |
            Opcode::SETcc(_) => {
                control_flow::Effect::cont()
            }

            // these have control flow dependent on the rep prefix
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::MOVS |
            Opcode::LODS |
            Opcode::STOS => {
                control_flow::Effect::cont()
            }
        }
    }
}
