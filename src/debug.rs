pub enum RunResult {
    Ok,
    HitBreakCondition,
    ExecutionError(String)
}

pub trait Peek {
    // TODO: actual failure modes more precise than None
    fn read<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<u8>;
    fn read16<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 2]>;
    fn read32<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 4]>;
    fn read64<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 8]>;
}

pub trait DebugTarget<'a, Target> {
    type WatchTarget;
    type BreakCondition;
    fn attach(&'a mut Target) -> Self;
    fn single_step(&mut self) -> Result<(), String>;
    fn run(&mut self) -> RunResult;
    fn add_watch(&mut self, Self::WatchTarget) -> Result<(), String>;
    fn add_break_condition(&mut self, Self::BreakCondition) -> Result<(), String>;
}

pub trait DBTarget2<Target> {
    type WatchTarget;
    type BreakCondition;
    fn attach(Target) -> Self;
    fn detach(self) -> Target;
    fn single_step(&mut self) -> Result<(), String>;
    fn run(&mut self) -> RunResult;
    fn add_watch(&mut self, Self::WatchTarget) -> Result<(), String>;
    fn add_break_condition(&mut self, Self::BreakCondition) -> Result<(), String>;
}
