use std::time::Duration;
use std::time::Instant;
use std::collections::HashMap;

pub struct Timer {
    label: &'static str,
    start: Instant
}

impl Timer {
    pub fn start(label: &'static str) -> Self {
        Self {
            label,
            start: Instant::now()
        }
    }

    pub fn end(self) -> (&'static str, u64) {
        (self.label, self.start.elapsed().as_nanos() as u64)
    }
}

impl<A: yaxpeax_arch::Address> Default for Timings<A> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Serialize)]
pub struct Timings<A: yaxpeax_arch::Address> {
    // Timings for some label, measured as computed for address A, in `u64` nanoseconds.
    data: HashMap<&'static str, Vec<(A, u64)>>,
}

impl<A: yaxpeax_arch::Address> Timings<A> {
    pub fn new() -> Self {
        Self { data: HashMap::new() }
    }

    pub fn record(&mut self, addr: A, timer: Timer) {
        let (label, time) = timer.end();
        let times = self.data.entry(label).or_insert_with(|| Vec::new());
        times.push((addr, time));
    }
}
