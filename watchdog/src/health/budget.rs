//! How often a health verdict may restart a process: at most `max` restarts
//! in any `window`, counted on a sliding window.

use std::time::{Duration, Instant};

use crate::crash_window::CrashWindow;

/// Largest number of doublings applied to the failure threshold, so a long
/// run of restarts cannot overflow it.
const MAX_BACKOFF_DOUBLINGS: u32 = 16;

/// The outcome of recording one verdict.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Grant {
    /// Restarts in the window, this one included.
    pub attempt: u32,
    /// False once the cap is exceeded: the process is to be left stopped for
    /// the user instead.
    pub will_restart: bool,
}

/// Counts health restarts over a sliding window. A restart or retry the user
/// asks for clears it. A cap of 0 allows no restart at all.
#[derive(Debug)]
pub struct RestartBudget {
    // Holds every verdict in the window. It reports exhausted at `max + 1`,
    // the first verdict that no restart may follow.
    window: CrashWindow,
}

impl RestartBudget {
    pub fn new(max: u32, window: Duration) -> Self {
        Self {
            window: CrashWindow::new(max.saturating_add(1), window),
        }
    }

    /// Restarts counted in the window as of `now`.
    pub fn in_window(&self, now: Instant) -> u32 {
        self.window.in_window(now)
    }

    /// The number of consecutive failures that makes the next verdict: the
    /// base threshold doubled once for each restart already in the window, so
    /// a process that is slow rather than gone gets more time every time.
    pub fn failure_threshold(&self, base: u32, now: Instant) -> u32 {
        let doublings = self.in_window(now).min(MAX_BACKOFF_DOUBLINGS);
        base.max(1).saturating_mul(1u32 << doublings)
    }

    /// Records a verdict at `now`.
    pub fn record(&mut self, now: Instant) -> Grant {
        let exhausted = self.window.record(now);
        Grant {
            attempt: self.window.count(),
            will_restart: !exhausted,
        }
    }

    /// Forgets every restart, when the user restarts or retries.
    pub fn clear(&mut self) {
        self.window.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const HOUR: Duration = Duration::from_secs(3600);

    #[test]
    fn record_allows_restarts_up_to_the_cap() {
        let mut b = RestartBudget::new(2, HOUR);
        let t = Instant::now();
        assert_eq!(
            b.record(t),
            Grant {
                attempt: 1,
                will_restart: true
            }
        );
        assert_eq!(
            b.record(t),
            Grant {
                attempt: 2,
                will_restart: true
            }
        );
        assert_eq!(
            b.record(t),
            Grant {
                attempt: 3,
                will_restart: false
            }
        );
    }

    #[test]
    fn at_most_the_cap_in_any_window() {
        let mut b = RestartBudget::new(2, HOUR);
        let t = Instant::now();
        let m = Duration::from_secs(60);
        assert!(b.record(t).will_restart);
        assert!(b.record(t + 50 * m).will_restart);
        // Two restarts fall inside the hour that ends here.
        assert!(!b.record(t + 59 * m).will_restart);
    }

    #[test]
    fn the_window_slides_so_old_restarts_stop_counting_one_at_a_time() {
        let mut b = RestartBudget::new(2, HOUR);
        let t = Instant::now();
        let m = Duration::from_secs(60);
        b.record(t);
        b.record(t + 30 * m);
        // The first restart is 61 minutes old, the second 31: one counts.
        let g = b.record(t + 61 * m);
        assert_eq!(
            g,
            Grant {
                attempt: 2,
                will_restart: true
            }
        );
    }

    #[test]
    fn restarts_just_over_an_hour_apart_never_reach_the_cap() {
        let mut b = RestartBudget::new(1, HOUR);
        let t = Instant::now();
        for i in 0..5 {
            let g = b.record(t + (HOUR + Duration::from_secs(1)) * i);
            assert_eq!(
                g,
                Grant {
                    attempt: 1,
                    will_restart: true
                }
            );
        }
    }

    #[test]
    fn clear_forgets_the_restarts() {
        let mut b = RestartBudget::new(1, HOUR);
        let t = Instant::now();
        b.record(t);
        b.clear();
        assert_eq!(b.in_window(t), 0);
        assert_eq!(
            b.record(t),
            Grant {
                attempt: 1,
                will_restart: true
            }
        );
    }

    #[test]
    fn failure_threshold_doubles_per_restart_in_the_window() {
        let mut b = RestartBudget::new(5, HOUR);
        let t = Instant::now();
        let seen: Vec<u32> = (0..5)
            .map(|_| {
                let n = b.failure_threshold(4, t);
                b.record(t);
                n
            })
            .collect();
        assert_eq!(seen, vec![4, 8, 16, 32, 64]);
    }

    #[test]
    fn failure_threshold_returns_to_the_base_after_the_window() {
        let mut b = RestartBudget::new(5, HOUR);
        let t = Instant::now();
        b.record(t);
        b.record(t);
        assert_eq!(b.failure_threshold(4, t + HOUR + Duration::from_secs(1)), 4);
    }

    #[test]
    fn failure_threshold_saturates() {
        let mut b = RestartBudget::new(u32::MAX, HOUR);
        let t = Instant::now();
        for _ in 0..40 {
            b.record(t);
        }
        assert_eq!(b.failure_threshold(u32::MAX, t), u32::MAX);
    }

    #[test]
    fn a_cap_of_zero_allows_no_restart() {
        let mut b = RestartBudget::new(0, HOUR);
        assert!(!b.record(Instant::now()).will_restart);
    }
}
