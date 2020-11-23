use std::cell::RefCell;
use std::collections::HashMap;

use petgraph::visit::Bfs;

use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressDisplay;
use yaxpeax_x86::long_mode::{Arch as x86_64, RegSpec};

use yaxpeax_core::arch::x86_64::semantic;
use yaxpeax_core::arch::x86_64::x86_64Data;
use yaxpeax_core::arch::InstructionSpan;
use yaxpeax_core::analyses::{control_flow, data_flow};
use yaxpeax_core::analyses::control_flow::ControlFlowGraph;
// use yaxpeax_core::analyses::static_single_assignment::Data;
use yaxpeax_core::analyses::static_single_assignment::SSA;
use yaxpeax_core::analyses::memory_layout::MemoryLayout;
use yaxpeax_core::arch::x86_64::analyses::data_flow::{Data, Location};
use yaxpeax_core::analyses::evaluators::Evaluator;

fn do_analyses(data: &[u8]) -> (ControlFlowGraph<<x86_64 as Arch>::Address>, SSA<x86_64>) {
    // TODO: is this necessary? can this be removed from `AnalysisBuilder::new`?
    let mut x86_64_data = x86_64Data::default();

    let cfg = control_flow::AnalysisBuilder::new(&data.to_vec(), &mut x86_64_data.contexts)
        .evaluate();
    let dfg = data_flow::AnalysisBuilder::new(
        &data.to_vec(),
        &cfg,
        &*x86_64_data.contexts.functions.borrow(),
        &mut yaxpeax_core::arch::x86_64::analyses::data_flow::NoDisambiguation::default(),
    )
        .ssa_cytron();

    Evaluator::new(&data.to_vec(), &cfg, &dfg).full_function_iterate();

    (cfg, dfg)
}

#[test]
fn test_arithmetic() {
    let instructions = &[
        0xb8, 0x04, 0x00, 0x00, 0x00, // mov eax, 4
        0xb9, 0x04, 0x00, 0x00, 0x00, // mov ecx, 4
        0x48, 0x01, 0xc1, // add rcx, rax
        0xc3,
    ];

    let (cfg, dfg) = do_analyses(instructions);

    // expect that rcx == 8
    assert_eq!(dfg.get_def(10, Location::rcx()).get_data().as_ref(), Some(&Data::Concrete(8, None)));
}

#[test]
fn test_memory() {
    // synthesized from `test_stack_inference` to specifically track memory analysis, and merging
    // non-overlapping memory writes.
    let instructions = &[
        0x48, 0xc7, 0x44, 0x24, 0x04, 0x00, 0x00, 0x00, 0x00,       // mov [rsp + 0x04], 0
        0x48, 0xc7, 0x44, 0x24, 0x0c, 0x00, 0x00, 0x00, 0x04,       // mov [rsp + 0x0c], 0x4000000
        0x48, 0xc7, 0xc2, 0x34, 0x12, 0x00, 0x00,                   // mov rdx, 0x1234
        0x48, 0x89, 0x44, 0x24, 0x30,                               // mov [rsp + 0x30], rdx
        0x49, 0x89, 0xd0,                                           // mov r8, rdx
        0x4c, 0x89, 0x44, 0x24, 0x14,                               // mov [rsp + 0x14], r8
    ];

    let (mut cfg, mut dfg) = do_analyses(instructions);

    let mut mem_analysis = MemoryLayout {
        ssa: &dfg,
        segments: HashMap::new(),
    };

    let instvec = instructions.to_vec();

    let mut bfs = Bfs::new(&cfg.graph, cfg.entrypoint);
    while let Some(k) = bfs.next(&cfg.graph) {
        let block = cfg.get_block(k);
        let mut iter = instvec.instructions_spanning(<x86_64 as Arch>::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            semantic::evaluate(address, &instr, &mut mem_analysis);
        }
    }

//    println!("{:?}", mem_analysis.segments);
    println!("\n");
use yaxpeax_core::analyses::static_single_assignment::HashedValue;
use yaxpeax_core::arch::x86_64::analyses::data_flow::ANY;

    let mut bfs = Bfs::new(&cfg.graph, cfg.entrypoint);
    while let Some(k) = bfs.next(&cfg.graph) {
        let block = cfg.get_block(k);
        let mut iter = instvec.instructions_spanning(<x86_64 as Arch>::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            println!("{}: {}", address.show(), instr);
            let mut post_instr_mem_state = None;

            if let Some(mem_read) = dfg.try_get_use(address, Location::Memory(ANY)) {
                let segment = mem_analysis.segments.get(&HashedValue { value: mem_read }).unwrap();
                post_instr_mem_state = Some(segment);
//                println!("{} read : {:?}", address.show(), segment);
            }
            if let Some(mem_write) = dfg.try_get_def(address, Location::Memory(ANY)) {
                let segment = mem_analysis.segments.get(&HashedValue { value: mem_write }).unwrap();
                post_instr_mem_state = Some(segment);
//                println!("{} write: {:?}", address.show(), segment);
            }

            use yaxpeax_core::analyses::memory_layout::{LocationAccess, LocationOffset};

            if let Some(mem_state) = post_instr_mem_state {
                // try to find the stack - the stack is named `location: rsp, version: None`.
                for (value, region) in mem_state.borrow().iter() {
                    let value = value.value.borrow();
                    if value.version == None && value.location == Location::Register(yaxpeax_x86::long_mode::RegSpec::rsp()) {
                        // this is the stack, show it off!
                        let mut keys: Vec<u64> = region.accesses.keys().cloned().collect();
                        keys.sort();
                        println!("      rsp_input");
                        for key in keys {
                            let accesses = &region.accesses[&key];
                            let mut widths: Vec<u64> = accesses.keys().cloned().collect();
                            widths.sort();
                            for width in widths {
                                print!("        - [{}, {}): ", key, key + width);
                                let access = &accesses[&width];
                                if access.is_write() {
                                    print!("write, ");
                                    match access.1.as_ref().unwrap() {
                                        LocationAccess::Unknown => {
                                            println!("unknown");
                                        }
                                        LocationAccess::Symbolic {
                                            base,
                                            offset: LocationOffset::Concrete(0),
                                        } => {
                                            let v: &yaxpeax_core::analyses::static_single_assignment::Value<yaxpeax_x86::x86_64> = &*base.borrow();
                                            if let Some(version) = v.version.as_ref() {
                                                print!("{}_{}", v.location, version);
                                            } else {
                                                print!("{}_input", v.location);
                                            }
                                            if let Some(data) = v.data.as_ref() {
                                                println!(" (= {:?})", data);
                                            } else {
                                                println!(" (unknown)");
                                            }
                                        }
                                        LocationAccess::Const { diff } => {
                                            println!("{:#x}", diff);
                                        }
                                        _ => {
                                            println!("haha");
                                        }
                                    }
                                } else {
                                    // if there was an access, it was a read or a write. and it's
                                    // not a write. so it must be a read.
                                    assert!(access.is_read());
                                    println!("read");
                                }
                            }
                        }
                    }
                }
            }
        }
    }
//    println!("{:?}", dfg);
}

#[test]
fn test_stack_inference()  {
    // some function from /bin/bash, 0x9df0
    //
    // interesting observations:
    // * frame pointer elision
    // * fs-based stack check
    // * stack-struct at rsp, passed as argument (rcx)
    //   - layout is dword, qword qword qword qword qword qword qword
    // * inner call takes rdx, rcx, rsi. rsi == rax == rdx_in
    // * stack limit from fs:0x28 makes it into the struct?
    // * both conditional jumps are to non-returning functions
    //  - `call sym.imp.abort` should not fall through to __stack_chk_fail
    let instructions = &[
        0x48, 0x83, 0xec, 0x48,                                        // sub rsp, 0x48
        0x48, 0x89, 0xd0,                                              // mov rax, rdx
        0x64, 0x48, 0x8b, 0x14, 0x25, 0x28, 0x00, 0x00, 0x00,          // mov rdx, qword fs:[0x28]
        0x48, 0x89, 0x54, 0x24, 0x38,                                  // mov qword [rsp + 0x38], rdx
        0x31, 0xd2,                                                    // xor edx, edx
        0x83, 0xfe, 0x0a,                                              // cmp esi, 0x0a
        0x74, 0x68,                                                    // je $ + 0x68
        0x89, 0x34, 0x24,                                              // mov dword [rsp], esi
        0x48, 0x89, 0xe1,                                              // mov rcx, rsp
        0x48, 0xc7, 0xc2, 0xff, 0xff, 0xff, 0xff,                      // mov rdx, -1
        0x48, 0x89, 0xc6,                                              // mov rsi, rax
        0x48, 0xc7, 0x44, 0x24, 0x04, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x04], 0
        0x48, 0xc7, 0x44, 0x24, 0x0c, 0x00, 0x00, 0x00, 0x04,          // mov [rsp + 0x0c], 0x4000000
        0x48, 0xc7, 0x44, 0x24, 0x14, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x14], 0
        0x48, 0xc7, 0x44, 0x24, 0x1c, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x1c], 0
        0x48, 0xc7, 0x44, 0x24, 0x24, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x24], 0
        0x48, 0xc7, 0x44, 0x24, 0x2c, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x2c], 0
        0x48, 0xc7, 0x44, 0x24, 0x34, 0x00, 0x00, 0x00, 0x00,          // mov [rsp + 0x34], 0
        0xe8, 0x21, 0xf8, 0xff, 0xff,                                  // call some_fn_taking_a_struct_through_rcx
        0x48, 0x8b, 0x4c, 0x24, 0x38,                                  // mov rcx, [rsp + 0x38]
        0x64, 0x48, 0x33, 0x0c, 0x25, 0x28, 0x00, 0x00, 0x00,          // xor rcx, fs:[0x28]
        0x75, 0x0a,                                                    // jne $+0a
        0x48, 0x83, 0xc4, 0x48,                                        // add rsp, 0x48
        0xc3,                                                          // ret
        0xe8, 0x27, 0x7f, 0xff, 0xff,                                  // call sym.imp.abort @noreturn
        0xe8, 0x22, 0x80, 0xff, 0xff,                                  // call sym.imp.__stack_chk_fail @noreturn
    ];

    let (mut cfg, mut dfg) = do_analyses(instructions);

    let mut mem_analysis = MemoryLayout {
        ssa: &dfg,
        segments: HashMap::new(),
    };

    let instvec = instructions.to_vec();

    let mut bfs = Bfs::new(&cfg.graph, cfg.entrypoint);
    while let Some(k) = bfs.next(&cfg.graph) {
        let block = cfg.get_block(k);
        let mut iter = instvec.instructions_spanning(<x86_64 as Arch>::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            semantic::evaluate(address, &instr, &mut mem_analysis);
        }
    }

    println!("{:?}", mem_analysis.segments);
use yaxpeax_core::analyses::static_single_assignment::HashedValue;
use yaxpeax_core::arch::x86_64::analyses::data_flow::ANY;

    let mut bfs = Bfs::new(&cfg.graph, cfg.entrypoint);
    while let Some(k) = bfs.next(&cfg.graph) {
        let block = cfg.get_block(k);
        let mut iter = instvec.instructions_spanning(<x86_64 as Arch>::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            if let Some(mem_read) = dfg.try_get_use(address, Location::Memory(ANY)) {
                let segment = mem_analysis.segments.get(&HashedValue { value: mem_read }).unwrap();
//                println!("{} read : {:?}", address.show(), segment);
            }
            if let Some(mem_write) = dfg.try_get_def(address, Location::Memory(ANY)) {
                let segment = mem_analysis.segments.get(&HashedValue { value: mem_write }).unwrap();
//                println!("{} write: {:?}", address.show(), segment);
            }
        }
    }
    println!("{:?}", dfg);
}

#[test]
fn test_call_inference() {
    // ELF entrypoint
    //
    // interesting observations here:
    // rbp is zeroed and otherwise unused! this function does !not! conform to the sysv abi
    // the `call [__libc_start_main]` takes six arguments, the ?return address? is popped into a
    // register and passed as an argument. `main` is an argument in rdi.
    // the call does not return (hlt should not be in the cfg)
    let instructions = &[
        0x31, 0xed,                                                    // xor ebp, ebp
        0x49, 0x89, 0xd1,                                              // mov r9, rdx
        0x5e,                                                          // pop rsi
        0x48, 0x89, 0xe3,                                              // mov rdx, rsp
        0x48, 0x83, 0xe4, 0xf0,                                        // and rsp, 0xfffffffffffffff0
        0x50,                                                          // push rax
        0x54,                                                          // push rsp
        0x4c, 0x8d, 0x05, 0xfa, 0x6e, 0x00, 0x00,                      // lea r8, [$ + 0x6efa] ; aka 0x6f10
        0x48, 0x8d, 0x0d, 0x83, 0x6e, 0x00, 0x00,                      // lea rcx, [$ + 0x6e83] ; aka 0x6ea0
        0x48, 0x8d, 0x3d, 0xfc, 0xd6, 0xff, 0xff,                      // lea rdi, [main] ; $ + 0xffffd6fc => 0xffffd720 (rip = 0x4b0d in original)
        0xff, 0x15, 0xbe, 0xb4, 0x20, 0x00,                            // call [0x20ffd8] ; aka `reloc.__libc_start_main_216
        0xf4,                                                          // hlt
    ];
}

#[test]
fn test_cmp_const() {
    // this is a `.finit` function.
    //
    // observations to test here:
    // `cmp rax, rdi` is statically const, which means the je is const knowable (is taken)
    // additionally, `jmp rax` is a tail call
    // `pop rbp` on both paths restores `rbp` and conforms to the sysv abi
    // function does not take arguments
    // function does not write to uninitialized memory
    // function *does* read a global and may control flow to it
    let instructions = &[
        0x48, 0x8d, 0x3d, 0x99, 0xb5, 0x20, 0x00,                      // lea rdi, [0x20b5a0]
        0x55,                                                          // push rbp
        0x48, 0x8d, 0x05, 0x91, 0xb5, 0x20, 0x00,                      // lea rax, [0x20b5a0]
        0x48, 0x39, 0xf8,                                              // cmp rax, rdi
        0x48, 0x89, 0xe5,                                              // mov rbp, rsp
        0x74, 0x19,                                                    // je $+0x19
        0x48, 0x8b, 0x05, 0x92, 0xb4, 0x20, 0x00,                      // mov rax, [0x20b4b0] ; rip + 0x20b492
        0x48, 0x85, 0xc0,                                              // test rax, rax
        0x74, 0x0d,                                                    // je $+0x0d
        0x5d,                                                          // pop rbp
        0xff, 0xe0,                                                    // jmp rax
        0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,    // nop word cs:[rax + rax]
        0x5d,                                                          // pop rbp
        0xc3,                                                          // ret
    ];
}

/* and one more from /bin/bash: 
 *
 *  // this is a hashing loop, ebp *= 0x1000193; rcx += 1; ebp ^= eax; eax = *rcx; eax != 0? loop
    // rcx is an iterator starting at ptr
    let instructions = &[
        0x69, 0xed, 0x93, 0x01, 0x00, 0x01,
        0x48, 0x83, 0xc1, 0x01,
        0x31, 0xc5,
        0x0f, 0xbe, 0x01,
        0x84, 0xc0,
        0x75, 0xed,
        0xc3
    ];
    */
