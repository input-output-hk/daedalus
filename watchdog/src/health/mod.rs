//! Liveness checks for the processes the watchdog supervises: a probe that
//! asks a local HTTP endpoint whether it answers, a monitor that turns a
//! series of probe outcomes into a verdict, and a budget that bounds how
//! often a verdict may restart a process. Nothing here knows which process
//! is being checked.

pub mod budget;
pub mod monitor;
pub mod probe;
