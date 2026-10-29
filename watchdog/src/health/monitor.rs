//! Turns a series of probe outcomes into a verdict. The monitor is a plain
//! state machine: it holds no clock and no timer, so its tests drive it with
//! synthetic time.

use std::time::{Duration, Instant, SystemTime};

use super::probe::Outcome;

/// Probe intervals between two ticks that the watchdog's own timer makes while
/// the machine is awake. A longer gap means the machine slept, or the
/// watchdog was starved, and the probes before it say nothing about the
/// process.
const TICKS_BEFORE_SLEEP: u32 = 3;

#[derive(Debug, Clone)]
pub struct MonitorSettings {
    pub probe_interval: Duration,
    pub probe_timeout: Duration,
    /// Consecutive failures that make a verdict, already scaled for the
    /// restarts so far.
    pub failure_threshold: u32,
    /// How long a process may go without its first answer.
    pub startup_grace: Duration,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    /// No answer yet. Timeouts are ignored, because the first request may
    /// legitimately wait for a dependency to come up; refusals are counted.
    Arming,
    /// The process has answered. Failures of every kind count.
    Healthy,
    /// The probe cannot judge this process, because of a configuration fault.
    Disabled,
    /// A verdict has been given.
    Unresponsive,
}

/// What the monitor knows when it gives a verdict.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Report {
    /// Consecutive failures at the verdict; 0 if the grace expired.
    pub failed_probes: u32,
    /// Since the last answer, or since arming if there was none.
    pub unresponsive_for: Duration,
    pub answered_once: bool,
    pub last_error: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// Nothing to act on.
    Continue,
    /// The TLS peer's certificate failed the check, or the peer refused ours.
    /// That is a configuration fault, not a fault of the process, so the
    /// monitor stops judging; the string says why.
    Disabled(String),
    /// The process is not answering.
    Unresponsive(Report),
}

pub struct Monitor {
    settings: MonitorSettings,
    state: State,
    consecutive: u32,
    armed_at: Instant,
    last_answer: Option<Instant>,
    last_tick: (Instant, SystemTime),
    last_error: String,
}

impl Monitor {
    pub fn new(settings: MonitorSettings, now: Instant, wall: SystemTime) -> Self {
        Self {
            settings,
            state: State::Arming,
            consecutive: 0,
            armed_at: now,
            last_answer: None,
            last_tick: (now, wall),
            last_error: String::new(),
        }
    }

    pub fn state(&self) -> State {
        self.state
    }

    pub fn consecutive_failures(&self) -> u32 {
        self.consecutive
    }

    /// The longest gap between two outcomes that a running timer makes: three
    /// intervals, plus the time one probe may take before it reports.
    fn max_gap(&self) -> Duration {
        self.settings.probe_interval * TICKS_BEFORE_SLEEP + self.settings.probe_timeout
    }

    /// Takes the outcome of one probe, which finished at `now` on the
    /// monotonic clock and `wall` on the wall clock.
    pub fn on_probe(&mut self, outcome: &Outcome, now: Instant, wall: SystemTime) -> Verdict {
        if matches!(self.state, State::Disabled | State::Unresponsive) {
            return Verdict::Continue;
        }
        let (last, last_wall) = std::mem::replace(&mut self.last_tick, (now, wall));
        let gap = now
            .saturating_duration_since(last)
            .max(wall.duration_since(last_wall).unwrap_or(Duration::MAX));
        let slept = gap > self.max_gap();
        if slept {
            self.consecutive = 0;
            self.armed_at = now;
        }

        match outcome {
            Outcome::Answered { .. } => {
                self.state = State::Healthy;
                self.consecutive = 0;
                self.last_answer = Some(now);
                return Verdict::Continue;
            }
            Outcome::TlsRejected(e) => {
                self.state = State::Disabled;
                return Verdict::Disabled(e.clone());
            }
            _ => {}
        }

        self.last_error = outcome
            .describe(self.settings.probe_timeout)
            .unwrap_or_default();
        if slept {
            return Verdict::Continue;
        }
        let counts = self.state == State::Healthy || *outcome != Outcome::Timeout;
        if counts {
            self.consecutive += 1;
            if self.consecutive >= self.settings.failure_threshold.max(1) {
                return self.verdict(now, self.consecutive);
            }
        }
        if self.state == State::Arming
            && now.saturating_duration_since(self.armed_at) >= self.settings.startup_grace
        {
            return self.verdict(now, 0);
        }
        Verdict::Continue
    }

    fn verdict(&mut self, now: Instant, failed_probes: u32) -> Verdict {
        self.state = State::Unresponsive;
        let since = self.last_answer.unwrap_or(self.armed_at);
        Verdict::Unresponsive(Report {
            failed_probes,
            unresponsive_for: now.saturating_duration_since(since),
            answered_once: self.last_answer.is_some(),
            last_error: self.last_error.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const OK: Outcome = Outcome::Answered { status: 200 };

    fn settings() -> MonitorSettings {
        MonitorSettings {
            probe_interval: Duration::from_secs(15),
            probe_timeout: Duration::from_secs(10),
            failure_threshold: 4,
            startup_grace: Duration::from_secs(300),
        }
    }

    /// A monitor and a clock that advances on every call.
    struct Rig {
        m: Monitor,
        now: Instant,
        wall: SystemTime,
    }

    impl Rig {
        fn new() -> Self {
            Self::with(settings())
        }

        fn with(s: MonitorSettings) -> Self {
            let now = Instant::now();
            let wall = SystemTime::now();
            Rig {
                m: Monitor::new(s, now, wall),
                now,
                wall,
            }
        }

        fn advance(&mut self, by: Duration) {
            self.now += by;
            self.wall += by;
        }

        fn probe(&mut self, o: Outcome) -> Verdict {
            self.advance(Duration::from_secs(15));
            self.m.on_probe(&o, self.now, self.wall)
        }
    }

    fn is_unresponsive(v: &Verdict) -> bool {
        matches!(v, Verdict::Unresponsive(_))
    }

    #[test]
    fn arming_ignores_timeouts() {
        let mut r = Rig::new();
        for _ in 0..10 {
            assert_eq!(r.probe(Outcome::Timeout), Verdict::Continue);
        }
        assert_eq!(r.m.state(), State::Arming);
        assert_eq!(r.m.consecutive_failures(), 0);
    }

    #[test]
    fn arming_counts_refusals_from_the_first_probe() {
        let mut r = Rig::new();
        for _ in 0..3 {
            assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
        }
        let Verdict::Unresponsive(report) = r.probe(Outcome::Refused) else {
            panic!("expected a verdict");
        };
        assert_eq!(report.failed_probes, 4);
        assert!(!report.answered_once);
        assert_eq!(report.last_error, "connect: connection refused");
        assert_eq!(report.unresponsive_for, Duration::from_secs(60));
    }

    #[test]
    fn arming_counts_io_errors_like_refusals() {
        let mut r = Rig::new();
        for _ in 0..3 {
            r.probe(Outcome::IoError("reset".into()));
        }
        assert!(is_unresponsive(&r.probe(Outcome::IoError("reset".into()))));
    }

    #[test]
    fn grace_expiry_without_an_answer_gives_a_verdict_with_no_failed_probes() {
        let mut r = Rig::new();
        let mut verdict = Verdict::Continue;
        for _ in 0..20 {
            verdict = r.probe(Outcome::Timeout);
            if is_unresponsive(&verdict) {
                break;
            }
        }
        let Verdict::Unresponsive(report) = verdict else {
            panic!("grace never expired");
        };
        assert_eq!(report.failed_probes, 0);
        assert!(!report.answered_once);
        assert_eq!(report.last_error, "timeout after 10000 ms");
        assert!(report.unresponsive_for >= Duration::from_secs(300));
    }

    #[test]
    fn the_first_answer_ends_arming() {
        let mut r = Rig::new();
        r.probe(Outcome::Refused);
        r.probe(Outcome::Refused);
        assert_eq!(r.probe(OK), Verdict::Continue);
        assert_eq!(r.m.state(), State::Healthy);
        assert_eq!(r.m.consecutive_failures(), 0);
    }

    #[test]
    fn healthy_counts_timeouts() {
        let mut r = Rig::new();
        r.probe(OK);
        for _ in 0..3 {
            assert_eq!(r.probe(Outcome::Timeout), Verdict::Continue);
        }
        let Verdict::Unresponsive(report) = r.probe(Outcome::Timeout) else {
            panic!("expected a verdict");
        };
        assert_eq!(report.failed_probes, 4);
        assert!(report.answered_once);
        // Measured from the last answer, four probes of 15 s later.
        assert_eq!(report.unresponsive_for, Duration::from_secs(60));
    }

    #[test]
    fn an_answer_resets_the_failure_count() {
        let mut r = Rig::new();
        r.probe(OK);
        for _ in 0..3 {
            r.probe(Outcome::Refused);
        }
        r.probe(OK);
        for _ in 0..3 {
            assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
        }
        assert!(is_unresponsive(&r.probe(Outcome::Refused)));
    }

    #[test]
    fn a_monotonic_gap_resets_the_count_and_skips_judgment() {
        let mut r = Rig::new();
        r.probe(OK);
        for _ in 0..3 {
            r.probe(Outcome::Refused);
        }
        // The machine slept: only the monotonic clock jumps.
        r.now += Duration::from_secs(3600);
        assert_eq!(
            r.m.on_probe(&Outcome::Refused, r.now, r.wall + Duration::from_secs(15)),
            Verdict::Continue
        );
        assert_eq!(r.m.consecutive_failures(), 0);
        // After it, the full threshold is needed again.
        for _ in 0..3 {
            assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
        }
        assert!(is_unresponsive(&r.probe(Outcome::Refused)));
    }

    #[test]
    fn a_wall_clock_gap_resets_the_count_and_skips_judgment() {
        let mut r = Rig::new();
        r.probe(OK);
        for _ in 0..3 {
            r.probe(Outcome::Refused);
        }
        // Only the wall clock jumps, as when it is set forward.
        r.wall += Duration::from_secs(3600);
        assert_eq!(
            r.m.on_probe(&Outcome::Refused, r.now + Duration::from_secs(15), r.wall),
            Verdict::Continue
        );
        assert_eq!(r.m.consecutive_failures(), 0);
    }

    #[test]
    fn a_gap_restarts_the_startup_grace() {
        let mut r = Rig::new();
        r.probe(Outcome::Timeout);
        r.advance(Duration::from_secs(3600));
        assert_eq!(
            r.m.on_probe(&Outcome::Timeout, r.now, r.wall),
            Verdict::Continue
        );
        // 290 s after the sleep is still inside a fresh grace.
        for _ in 0..19 {
            assert_eq!(r.probe(Outcome::Timeout), Verdict::Continue);
        }
        assert!(is_unresponsive(&r.probe(Outcome::Timeout)));
    }

    #[test]
    fn an_answer_after_a_gap_still_counts() {
        let mut r = Rig::new();
        r.advance(Duration::from_secs(3600));
        assert_eq!(r.m.on_probe(&OK, r.now, r.wall), Verdict::Continue);
        assert_eq!(r.m.state(), State::Healthy);
    }

    #[test]
    fn a_rejected_certificate_disables_the_monitor() {
        let mut r = Rig::new();
        r.probe(OK);
        assert_eq!(
            r.probe(Outcome::TlsRejected("bad certificate".into())),
            Verdict::Disabled("bad certificate".into())
        );
        assert_eq!(r.m.state(), State::Disabled);
        for _ in 0..20 {
            assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
        }
    }

    #[test]
    fn a_scaled_threshold_takes_more_failures() {
        let mut r = Rig::with(MonitorSettings {
            failure_threshold: 8,
            ..settings()
        });
        r.probe(OK);
        for _ in 0..7 {
            assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
        }
        assert!(is_unresponsive(&r.probe(Outcome::Refused)));
    }

    #[test]
    fn no_further_verdict_follows_the_first() {
        let mut r = Rig::new();
        for _ in 0..4 {
            r.probe(Outcome::Refused);
        }
        assert_eq!(r.m.state(), State::Unresponsive);
        assert_eq!(r.probe(Outcome::Refused), Verdict::Continue);
    }
}
