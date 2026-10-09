use std::collections::VecDeque;
use std::time::{Duration, Instant};

/// Counts a process's unexpected exits over a sliding time window, so a
/// process that keeps crashing after it has started successfully is caught
/// as well as one that never starts.
#[derive(Debug)]
pub struct CrashWindow {
    max: u32,
    window: Duration,
    crashes: VecDeque<Instant>,
}

impl CrashWindow {
    /// `max` crashes within `window` exhaust the window. A `max` of 0
    /// disables it.
    pub fn new(max: u32, window: Duration) -> Self {
        Self {
            max,
            window,
            crashes: VecDeque::new(),
        }
    }

    /// Records a crash at `now`. Returns true when `max` crashes have now
    /// happened within the window.
    pub fn record(&mut self, now: Instant) -> bool {
        if self.max == 0 {
            return false;
        }
        while let Some(&oldest) = self.crashes.front() {
            if now.saturating_duration_since(oldest) >= self.window {
                self.crashes.pop_front();
            } else {
                break;
            }
        }
        self.crashes.push_back(now);
        self.crashes.len() >= self.max as usize
    }

    /// Crashes currently inside the window, as of the last `record`.
    pub fn count(&self) -> u32 {
        self.crashes.len() as u32
    }

    /// Forgets every recorded crash, after the user asks for a retry.
    pub fn clear(&mut self) {
        self.crashes.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MIN: Duration = Duration::from_secs(60);

    #[test]
    fn exhausts_at_max_crashes_within_the_window() {
        let t0 = Instant::now();
        let mut w = CrashWindow::new(3, 10 * MIN);
        assert!(!w.record(t0));
        assert!(!w.record(t0 + MIN));
        assert!(w.record(t0 + 2 * MIN));
        assert_eq!(w.count(), 3);
    }

    #[test]
    fn forgets_crashes_older_than_the_window() {
        let t0 = Instant::now();
        let mut w = CrashWindow::new(3, 10 * MIN);
        assert!(!w.record(t0));
        assert!(!w.record(t0 + 9 * MIN));
        // The first crash is now ten minutes old and leaves the window.
        assert!(!w.record(t0 + 10 * MIN));
        assert_eq!(w.count(), 2);
        assert!(w.record(t0 + 11 * MIN));
    }

    #[test]
    fn spaced_out_crashes_never_exhaust_it() {
        let t0 = Instant::now();
        let mut w = CrashWindow::new(5, 10 * MIN);
        for i in 0..50 {
            assert!(!w.record(t0 + i * 3 * MIN), "crash {i}");
        }
    }

    #[test]
    fn clear_starts_counting_again() {
        let t0 = Instant::now();
        let mut w = CrashWindow::new(2, 10 * MIN);
        assert!(!w.record(t0));
        w.clear();
        assert!(!w.record(t0 + MIN));
        assert!(w.record(t0 + 2 * MIN));
    }

    #[test]
    fn max_zero_disables_the_window() {
        let t0 = Instant::now();
        let mut w = CrashWindow::new(0, 10 * MIN);
        for i in 0..20 {
            assert!(!w.record(t0 + i * Duration::from_millis(1)));
        }
    }
}
