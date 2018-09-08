pub enum RunResult {
    Ok,
    HitBreakCondition,
    ExecutionError(String)
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
