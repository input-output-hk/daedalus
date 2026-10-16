//! Recognises the log signatures of a hole in the chain database.
//!
//! The node reports the damage in two ways: it rejects a canonical block with
//! `BadInputsUTxO` from every peer that serves it, or it cannot open its ledger
//! because the ledger point is missing from the ImmutableDB. A detector is
//! created per node process and sees every line the process writes, so the
//! checks are plain substring tests and only a matching line is parsed.

use std::borrow::Cow;
use std::sync::{Arc, Mutex};

use tracing::warn;

use crate::protocol::{ChainDamageKind, Event, emit};

/// Number of distinct peers that must reject the same slot before it counts.
/// One misbehaving peer cannot reach it.
const PEER_THRESHOLD: usize = 3;

/// Slots tracked at once. The oldest is dropped when another arrives.
const MAX_TRACKED_SLOTS: usize = 16;

const BAD_INPUTS: &str = "BadInputsUTxO";
const CHAIN_SYNC_EXCEPTION: &str = "ChainSync.Client.Exception";
const INVALID_BLOCK_SLOT: &str = "InvalidBlock (Point (At (Block {blockPointSlot = SlotNo ";
const REMOTE_ADDRESS: &str = "remoteAddress = ";
const INVARIANT_VIOLATION: &str = "Critical invariant violation";
const POINT_GONE: &str = "that was in immutable db is gone before we could open ledgerdb";
const BLOCK_POINT_SLOT: &str = "blockPointSlot = SlotNo ";

/// One recognised signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Detection {
    pub kind: ChainDamageKind,
    pub slot: Option<u64>,
    pub last_good_slot: Option<u64>,
}

impl Detection {
    /// Write the detection to `watchdog.log` and emit the `chain_damage` event.
    pub fn report(self) {
        let slot = self
            .slot
            .map_or_else(|| "unknown".to_string(), |s| s.to_string());
        match self.last_good_slot {
            Some(last) => warn!(
                "chain damage detected: kind={}, slot={slot}, last_good_slot={last}",
                self.kind.as_str()
            ),
            None => warn!(
                "chain damage detected: kind={}, slot={slot}",
                self.kind.as_str()
            ),
        }
        emit(&Event::ChainDamage {
            kind: self.kind,
            slot: self.slot,
            last_good_slot: self.last_good_slot,
        });
    }
}

/// Peers that rejected one slot, at most `PEER_THRESHOLD` of them.
struct RejectedSlot {
    slot: u64,
    peers: Vec<String>,
}

/// Looks for chain damage in the output of one node process. Each kind is
/// reported at most once.
#[derive(Default)]
pub struct ChainDamageDetector {
    rejected: Vec<RejectedSlot>,
    block_rejected_reported: bool,
    ledger_point_reported: bool,
}

/// A detector shared by the stdout and stderr readers of one process.
pub type SharedDetector = Arc<Mutex<ChainDamageDetector>>;

impl ChainDamageDetector {
    /// A detector for the output of one cardano-node process.
    pub fn node() -> Self {
        Self::default()
    }

    /// Inspect one output line. Returns a detection the first time a kind is
    /// recognised.
    pub fn observe(&mut self, line: &str) -> Option<Detection> {
        if !self.block_rejected_reported && line.contains(BAD_INPUTS) {
            return self.observe_rejected_block(line);
        }
        if !self.ledger_point_reported && line.contains(INVARIANT_VIOLATION) {
            return self.observe_missing_ledger_point(line);
        }
        None
    }

    fn observe_rejected_block(&mut self, line: &str) -> Option<Detection> {
        if !line.contains(CHAIN_SYNC_EXCEPTION) {
            return None;
        }
        let line = strip_ansi(line);
        let slot = number_after(&line, INVALID_BLOCK_SLOT)?;
        let peer = text_between(&line, REMOTE_ADDRESS, '}')?;

        let index = match self.rejected.iter().position(|r| r.slot == slot) {
            Some(i) => i,
            None => {
                if self.rejected.len() == MAX_TRACKED_SLOTS {
                    self.rejected.remove(0);
                }
                self.rejected.push(RejectedSlot {
                    slot,
                    peers: Vec::new(),
                });
                self.rejected.len() - 1
            }
        };
        let entry = &mut self.rejected[index];
        if entry.peers.iter().any(|p| p == peer) {
            return None;
        }
        entry.peers.push(peer.to_string());
        if entry.peers.len() < PEER_THRESHOLD {
            return None;
        }
        self.block_rejected_reported = true;
        self.rejected = Vec::new();
        Some(Detection {
            kind: ChainDamageKind::NodeRejectsCanonicalBlock,
            slot: Some(slot),
            last_good_slot: None,
        })
    }

    fn observe_missing_ledger_point(&mut self, line: &str) -> Option<Detection> {
        if !line.contains(POINT_GONE) {
            return None;
        }
        self.ledger_point_reported = true;
        let line = strip_ansi(line);
        Some(Detection {
            kind: ChainDamageKind::LedgerPointMissing,
            slot: number_after(&line, BLOCK_POINT_SLOT),
            last_good_slot: None,
        })
    }
}

/// Remove ANSI escape sequences (`ESC [ ... <final byte>`) from `text`.
fn strip_ansi(text: &str) -> Cow<'_, str> {
    if !text.contains('\x1b') {
        return Cow::Borrowed(text);
    }
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c != '\x1b' {
            out.push(c);
            continue;
        }
        if chars.as_str().starts_with('[') {
            chars.next();
            for c in chars.by_ref() {
                if ('\u{40}'..='\u{7e}').contains(&c) {
                    break;
                }
            }
        }
    }
    Cow::Owned(out)
}

/// The decimal number that directly follows the first occurrence of `marker`.
fn number_after(text: &str, marker: &str) -> Option<u64> {
    let rest = &text[text.find(marker)? + marker.len()..];
    let end = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    rest[..end].parse().ok()
}

/// The non-empty text between the first `marker` and the next `end`.
fn text_between<'a>(text: &'a str, marker: &str, end: char) -> Option<&'a str> {
    let rest = &text[text.find(marker)? + marker.len()..];
    let value = &rest[..rest.find(end)?];
    (!value.is_empty()).then_some(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `ChainSync.Client.Exception` line as the node writes it, with the
    /// identifiers redacted as in the field logs. `padding` stands in for the
    /// long `ValueNotConservedUTxO` mismatch that precedes `BadInputsUTxO`.
    fn exception_line(slot: u64, peer: &str, padding: usize) -> String {
        format!(
            "\x1b[37m[2026-10-14 23:16:58.7759Z]\x1b[0m[daedalus:ChainSync.Client.Exception](Warning,30927) \
Peer is: (ConnectionId {{localAddress = <redacted>, remoteAddress = {peer}}}). \
An exception was thrown by the Chain Sync Client. \
InvalidBlock (Point (At (Block {{blockPointSlot = SlotNo {slot}, blockPointHash = <redacted>}}))) <redacted> \
(ExtValidationErrorLedger (HardForkLedgerErrorFromEra S (S (S (S (S (S (Z (WrapLedgerErr {{unwrapLedgerErr = \
BlockTransitionErrorInternal (LedgersFailure (LedgerFailure (ConwayUtxowFailure (UtxoFailure \
(ValueNotConservedUTxO Mismatch (RelEQ) {{supplied: MaryValue (Coin {pad}) ...}})))) :| \
[LedgersFailure (LedgerFailure (ConwayUtxowFailure (UtxoFailure (BadInputsUTxO (NonEmptySet \
(fromList [TxIn (TxId {{unTxId = SafeHash \"<redacted>\"}}) (TxIx {{unTxIx = 1}})]))))))])}})))))))))\n",
            pad = "9".repeat(padding)
        )
    }

    fn invariant_line(slot: u64) -> String {
        format!(
            "cardano-node.exe: Critical invariant violation: block Point (At (Block {{blockPointSlot = SlotNo {slot}, \
blockPointHash = <redacted>}})) that was in immutable db is gone before we could open ledgerdb\
Node configuration: NodeConfiguration {{...}}\n"
        )
    }

    fn rejected_block(slot: u64) -> Detection {
        Detection {
            kind: ChainDamageKind::NodeRejectsCanonicalBlock,
            slot: Some(slot),
            last_good_slot: None,
        }
    }

    #[test]
    fn fires_on_the_third_distinct_peer() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe(&exception_line(200_361_988, "192.0.2.1:3001", 10)),
            None
        );
        assert_eq!(
            d.observe(&exception_line(200_361_988, "192.0.2.2:3001", 10)),
            None
        );
        assert_eq!(
            d.observe(&exception_line(200_361_988, "192.0.2.3:3001", 10)),
            Some(rejected_block(200_361_988))
        );
    }

    #[test]
    fn three_lines_from_one_peer_do_not_fire() {
        let mut d = ChainDamageDetector::node();
        for _ in 0..3 {
            assert_eq!(
                d.observe(&exception_line(200_361_988, "192.0.2.1:3001", 10)),
                None
            );
        }
    }

    #[test]
    fn slots_are_counted_separately() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(d.observe(&exception_line(100, "192.0.2.1:3001", 1)), None);
        assert_eq!(d.observe(&exception_line(200, "192.0.2.2:3001", 1)), None);
        assert_eq!(d.observe(&exception_line(100, "192.0.2.3:3001", 1)), None);
        assert_eq!(d.observe(&exception_line(200, "192.0.2.4:3001", 1)), None);
        assert_eq!(
            d.observe(&exception_line(200, "192.0.2.5:3001", 1)),
            Some(rejected_block(200))
        );
    }

    #[test]
    fn invalid_block_without_bad_inputs_does_not_count() {
        let mut d = ChainDamageDetector::node();
        for i in 0..5 {
            let line = exception_line(300, &format!("192.0.2.{i}:3001"), 1)
                .replace("BadInputsUTxO", "OtherFailure");
            assert_eq!(d.observe(&line), None);
        }
    }

    #[test]
    fn bad_inputs_outside_the_chain_sync_client_does_not_count() {
        let mut d = ChainDamageDetector::node();
        for i in 0..5 {
            let line = exception_line(300, &format!("192.0.2.{i}:3001"), 1)
                .replace("ChainSync.Client.Exception", "Net.Mux.Remote.ExceptionExit");
            assert_eq!(d.observe(&line), None);
        }
    }

    #[test]
    fn recognises_the_35_kb_line() {
        let mut d = ChainDamageDetector::node();
        let long = |peer: &str| exception_line(200_361_799, peer, 29_000);
        assert!(long("192.0.2.1:3001").len() > 29_000);
        assert_eq!(d.observe(&long("192.0.2.1:3001")), None);
        assert_eq!(d.observe(&long("192.0.2.2:3001")), None);
        assert_eq!(
            d.observe(&long("192.0.2.3:3001")),
            Some(rejected_block(200_361_799))
        );
    }

    #[test]
    fn tracks_at_most_sixteen_slots() {
        let mut d = ChainDamageDetector::node();
        for slot in 0..20u64 {
            d.observe(&exception_line(slot, "192.0.2.1:3001", 1));
            d.observe(&exception_line(slot, "192.0.2.2:3001", 1));
        }
        assert_eq!(d.rejected.len(), MAX_TRACKED_SLOTS);
        assert!(d.rejected.iter().all(|r| r.peers.len() <= PEER_THRESHOLD));
        // Slot 0 was evicted, so its count starts again.
        assert_eq!(d.observe(&exception_line(0, "192.0.2.3:3001", 1)), None);
    }

    #[test]
    fn rejected_block_fires_once_per_process() {
        let mut d = ChainDamageDetector::node();
        for i in 0..3 {
            d.observe(&exception_line(500, &format!("192.0.2.{i}:3001"), 1));
        }
        for i in 3..10 {
            assert_eq!(
                d.observe(&exception_line(500, &format!("192.0.2.{i}:3001"), 1)),
                None
            );
        }
    }

    #[test]
    fn recognises_the_missing_ledger_point() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe(&invariant_line(200_102_375)),
            Some(Detection {
                kind: ChainDamageKind::LedgerPointMissing,
                slot: Some(200_102_375),
                last_good_slot: None,
            })
        );
    }

    #[test]
    fn missing_ledger_point_survives_ansi_codes() {
        let mut d = ChainDamageDetector::node();
        let line = format!("\x1b[31m{}\x1b[0m", invariant_line(200_102_375));
        assert_eq!(d.observe(&line).map(|x| x.slot), Some(Some(200_102_375)));
    }

    #[test]
    fn second_missing_ledger_point_emits_nothing() {
        let mut d = ChainDamageDetector::node();
        assert!(d.observe(&invariant_line(200_102_375)).is_some());
        assert_eq!(d.observe(&invariant_line(200_102_375)), None);
    }

    #[test]
    fn another_invariant_violation_is_not_the_missing_ledger_point() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe("Critical invariant violation: something else\n"),
            None
        );
    }

    #[test]
    fn unrelated_lines_are_ignored() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe("[daedalus:ChainDB.AddBlockEvent](Info) Replayed block\n"),
            None
        );
        assert_eq!(d.observe(""), None);
    }

    #[test]
    fn strip_ansi_removes_sequences_only() {
        assert_eq!(strip_ansi("\x1b[1;31mred\x1b[0m text"), "red text");
        assert_eq!(strip_ansi("plain"), "plain");
        assert_eq!(strip_ansi("trailing \x1b"), "trailing ");
    }
}
