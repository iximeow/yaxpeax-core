use analyses::control_flow::{Effect, Target};
use analyses::Value;
use data::ValueLocations;
use yaxpeax_arch::{Address, Arch};

use std::fmt;

trait IsPC {
    fn is_pc(&self) -> IsPC;
}

struct InstructionPointerDFG<A> where A: Arch + ValueLocations {
    initial_effect: Effect<A::Address>,
}

impl <A: Arch + ValueLocations> InstructionPointerDFG<A> {
    // default effect is to simply fall through
    fn new() -> Self {
        Self {
            initial_effect: Effect {
                stop_after: false,
                dest: None,
            }
        }
    }
}

#[derive(Debug, Clone)]
enum EffectValue<A: Address + fmt::Debug> {
    Unknown,
    Const(u64),
    Known(Effect<A>),
}

impl<A: Address + fmt::Debug> EffectValue<A> {
    fn stop_after(&self) -> bool {
        match self {
            EffectValue::Unknown => {
                true
            },
            EffectValue::Const(_) => { unreachable!("bad effect value"); },
            EffectValue::Known(e) => { e.is_stop() }
        }
    }
}

impl<A: Address + fmt::Debug> Value for EffectValue<A> {
    fn unknown() -> Self {
        EffectValue::Unknown
    }

    fn to_const(&self) -> Option<u64> {
        None
    }

    fn from_const(c: u64) -> Self {
        EffectValue::Const(c)
    }

    fn from_set(effects: &[EffectValue<A>]) -> Self {
        debug_assert!(effects.len() != 0);
        let mut stop_after = true;
        let mut target: Option<Target<A>> = None;

        for effect in effects {
            match effect {
                EffectValue::Unknown => {
                    panic!("bad effects to merge: Unknown");
                }
                EffectValue::Const(o) => {
                    panic!("bad effects to merge: Const({})", o);
                }
                EffectValue::Known(e) => {
                    stop_after &= e.is_stop();

                    let merged_target = match (target, e.dest.as_ref()) {
                        (None, None) => {
                            None
                        }
                        (None, Some(o)) => {
                            Some(o.clone())
                        }
                        (Some(o), None) => {
                            Some(o)
                        }
                        // welllll this ought to be deduplicated...
                        /*
                        (Some(Target::Multiple(ref l)), Some(Target::Multiple(r))) => {
                            let mut vec = l.clone();
                            vec.extend_from_slice(&r);
                            Some(Target::Multiple(vec))
                        }
                        (Some(Target::Multiple(l)), Some(r)) => {
                            let mut vec = l.clone();
                            vec.push(r.clone());
                            Some(Target::Multiple(vec))
                        }
                        (Some(ref l), Some(Target::Multiple(r))) => {
                            let mut vec = r.clone();
                            vec.push(l.clone());
                            Some(Target::Multiple(vec))
                        }
                        (Some(l), Some(r)) => {
                            if &l == r {
                                Some(l)
                            } else {
                                Some(Target::Multiple(vec![l, r.clone()]))
                            }
                        }
                        */
                        _ => {
                            unsafe {
                                std::hint::unreachable_unchecked();
                            }
                        }
                    };
                    target = merged_target;
                }
            }
        }

        EffectValue::Known(Effect {
            stop_after,
            dest: target,
        })
    }
}
