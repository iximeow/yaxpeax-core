pub mod gdb_remote;

pub enum RunResult {
    Ok,
    HitBreakCondition,
    ExecutionError(String)
}

pub trait Peek {
    fn read_bytes<A: yaxpeax_arch::Address, W: std::io::Write>(&self, addr: A, len: u64, buf: &mut W) -> Option<()>;
    // TODO: actual failure modes more precise than None
    fn read<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<u8> {
        let mut buf = [0u8; 1];
        if self.read_bytes(addr, buf.len() as u64, &mut &mut buf[..]).is_none() {
            return None;
        } else {
            return Some(buf[0]);
        }
    }
    fn read16<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 2]> {
        let mut buf = [0u8; 2];
        if self.read_bytes(addr, buf.len() as u64, &mut &mut buf[..]).is_none() {
            return None;
        } else {
            return Some(buf);
        }
    }
    fn read32<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 4]> {
        let mut buf = [0u8; 4];
        if self.read_bytes(addr, buf.len() as u64, &mut &mut buf[..]).is_none() {
            return None;
        } else {
            return Some(buf);
        }
    }
    fn read64<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 8]> {
        let mut buf = [0u8; 8];
        if self.read_bytes(addr, buf.len() as u64, &mut &mut buf[..]).is_none() {
            return None;
        } else {
            return Some(buf);
        }
    }
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
