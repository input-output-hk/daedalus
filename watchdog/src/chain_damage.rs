//! Recognises the log signatures of a hole in the chain database.
//!
//! The wallet reports it a third way, when the chain it is given skips blocks.
//!
//! The node reports the damage in two ways: it rejects a canonical block with
//! `BadInputsUTxO` from every peer that serves it, or it cannot open its ledger
//! because the ledger point is missing from the ImmutableDB. A detector is
//! created per node or wallet process and sees every line the process writes,
//! so the checks are plain substring tests and only a matching line is parsed.
//! A node detector looks only for the node signatures and a wallet detector
//! only for the wallet one.

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
const NOT_A_CONTINUATION: &str = "restoreBlocks: given chain isn't a valid continuation";
const WALLET_IS_AT: &str = "Wallet is at:";
const CHAIN_CONTINUES: &str = "continues starting from:";

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
        self.report_as(false);
    }

    /// `report` for damage found before the node started, which the watchdog
    /// does not start.
    pub fn report_held(self) {
        self.report_as(true);
    }

    fn report_as(self, node_held: bool) {
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
            node_held,
        });
    }
}

/// Peers that rejected one slot, at most `PEER_THRESHOLD` of them.
struct RejectedSlot {
    slot: u64,
    peers: Vec<String>,
}

/// Which process's output a detector reads.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Source {
    Node,
    Wallet,
}

/// Looks for chain damage in the output of one node or wallet process. Each
/// kind is reported at most once.
pub struct ChainDamageDetector {
    source: Source,
    rejected: Vec<RejectedSlot>,
    block_rejected_reported: bool,
    ledger_point_reported: bool,
    discontinuity_reported: bool,
}

/// A detector shared by the stdout and stderr readers of one process.
pub type SharedDetector = Arc<Mutex<ChainDamageDetector>>;

impl ChainDamageDetector {
    /// A detector for the output of one cardano-node process.
    pub fn node() -> Self {
        Self::new(Source::Node)
    }

    /// A detector for the output of one cardano-wallet process.
    pub fn wallet() -> Self {
        Self::new(Source::Wallet)
    }

    fn new(source: Source) -> Self {
        Self {
            source,
            rejected: Vec::new(),
            block_rejected_reported: false,
            ledger_point_reported: false,
            discontinuity_reported: false,
        }
    }

    /// Inspect one output line. Returns a detection the first time a kind is
    /// recognised.
    pub fn observe(&mut self, line: &str) -> Option<Detection> {
        match self.source {
            Source::Node => self.observe_node(line),
            Source::Wallet => self.observe_wallet(line),
        }
    }

    /// Inspect output the process has left without a newline for a while.
    /// cardano-node writes the missing ledger point error without a newline,
    /// running into the text that follows it, so the complete line can arrive
    /// only as the node exits. Each node signature carries its evidence
    /// before the text that identifies it, so a partial line that matches
    /// holds all of it. The wallet's slots
    /// follow its identifying text, and it writes whole lines, so a wallet
    /// detector waits for the newline. A kind reported here is not reported
    /// again when the rest of the line arrives.
    pub fn observe_partial(&mut self, line: &str) -> Option<Detection> {
        match self.source {
            Source::Node => self.observe_node(line),
            Source::Wallet => None,
        }
    }

    fn observe_node(&mut self, line: &str) -> Option<Detection> {
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

impl ChainDamageDetector {
    fn observe_wallet(&mut self, line: &str) -> Option<Detection> {
        if self.discontinuity_reported || !line.contains(NOT_A_CONTINUATION) {
            return None;
        }
        self.discontinuity_reported = true;
        let line = strip_ansi(line);
        let after = line.split_once(NOT_A_CONTINUATION).map_or("", |(_, r)| r);
        let (wallet_part, chain_part) = match after.split_once(CHAIN_CONTINUES) {
            Some((before, chain)) => (before, chain),
            None => (after, ""),
        };
        let last_good_slot = wallet_part
            .split_once(WALLET_IS_AT)
            .and_then(|(_, r)| edge_slot(r));
        Some(Detection {
            kind: ChainDamageKind::WalletChainDiscontinuity,
            slot: edge_slot(chain_part),
            last_good_slot,
        })
    }
}

/// The slot of the first chain point in `text`, written `-<slot>#<blockNo>`.
fn edge_slot(text: &str) -> Option<u64> {
    let hash = text.find('#')?;
    let before = &text[..hash];
    let digits = before
        .rfind(|c: char| !c.is_ascii_digit())
        .map_or(0, |i| i + 1);
    if digits == before.len() || !before[..digits].ends_with('-') {
        return None;
    }
    before[digits..].parse().ok()
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
    fn unterminated_missing_ledger_point_is_reported_once() {
        let mut d = ChainDamageDetector::node();
        let line = invariant_line(200_102_375);
        let end = line.find("Node configuration").unwrap();
        assert_eq!(
            d.observe_partial(&line[..end]).map(|x| x.slot),
            Some(Some(200_102_375))
        );
        assert_eq!(d.observe(&line), None);
    }

    #[test]
    fn partial_line_cut_before_the_signature_reports_nothing() {
        let mut d = ChainDamageDetector::node();
        let line = invariant_line(200_102_375);
        let end = line.find("that was in immutable db").unwrap();
        assert_eq!(d.observe_partial(&line[..end]), None);
        assert_eq!(d.observe(&line).map(|x| x.slot), Some(Some(200_102_375)));
    }

    #[test]
    fn partial_rejection_is_not_counted_twice() {
        let mut d = ChainDamageDetector::node();
        let first = exception_line(200_361_988, "192.0.2.1:3001", 1);
        assert_eq!(d.observe_partial(first.trim_end()), None);
        assert_eq!(d.observe(&first), None);
        assert_eq!(
            d.observe(&exception_line(200_361_988, "192.0.2.2:3001", 1)),
            None
        );
        assert_eq!(
            d.observe(&exception_line(200_361_988, "192.0.2.3:3001", 1)),
            Some(rejected_block(200_361_988))
        );
    }

    #[test]
    fn partial_wallet_line_waits_for_the_newline() {
        let mut d = ChainDamageDetector::wallet();
        assert_eq!(d.observe_partial(DISCONTINUITY.trim_end()), None);
        assert_eq!(
            d.observe(DISCONTINUITY).map(|x| (x.slot, x.last_good_slot)),
            Some((Some(200_102_408), Some(200_080_892)))
        );
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

    /// The wallet line as cardano-wallet writes it, redacted as in the field logs.
    const DISCONTINUITY: &str = "[cardano-wallet.wallet-engine:Error:53] [2026-10-15 02:03:38.48 UTC] \
Worker has exited: Unhandled exception: user error (restoreBlocks: given chain isn't a valid continuation. \
Wallet is at: <redacted><-[<redacted>-200080892#14050855] but the given chain continues starting from: \
<redacted><-[<redacted>-200102408#14051883])\n";

    #[test]
    fn recognises_the_wallet_discontinuity() {
        let mut d = ChainDamageDetector::wallet();
        assert_eq!(
            d.observe(DISCONTINUITY),
            Some(Detection {
                kind: ChainDamageKind::WalletChainDiscontinuity,
                slot: Some(200_102_408),
                last_good_slot: Some(200_080_892),
            })
        );
    }

    #[test]
    fn eleven_repeats_emit_one_wallet_event() {
        let mut d = ChainDamageDetector::wallet();
        assert!(d.observe(DISCONTINUITY).is_some());
        for _ in 0..10 {
            assert_eq!(d.observe(DISCONTINUITY), None);
        }
    }

    #[test]
    fn wallet_line_with_ansi_codes_is_recognised() {
        let mut d = ChainDamageDetector::wallet();
        let line = DISCONTINUITY.replace("-200102408#", "-\x1b[0m200102408#");
        assert_eq!(d.observe(&line).map(|x| x.slot), Some(Some(200_102_408)));
    }

    #[test]
    fn wallet_line_without_readable_points_still_reports() {
        let mut d = ChainDamageDetector::wallet();
        let d1 =
            d.observe("restoreBlocks: given chain isn't a valid continuation. Wallet is at: ?\n");
        assert_eq!(
            d1,
            Some(Detection {
                kind: ChainDamageKind::WalletChainDiscontinuity,
                slot: None,
                last_good_slot: None,
            })
        );
    }

    #[test]
    fn wallet_detector_ignores_node_signatures() {
        let mut d = ChainDamageDetector::wallet();
        assert_eq!(d.observe(&invariant_line(200_102_375)), None);
        for i in 0..4 {
            assert_eq!(
                d.observe(&exception_line(7, &format!("192.0.2.{i}:3001"), 1)),
                None
            );
        }
    }

    #[test]
    fn node_detector_ignores_the_wallet_signature() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(d.observe(DISCONTINUITY), None);
    }

    #[test]
    fn strip_ansi_removes_sequences_only() {
        assert_eq!(strip_ansi("\x1b[1;31mred\x1b[0m text"), "red text");
        assert_eq!(strip_ansi("plain"), "plain");
        assert_eq!(strip_ansi("trailing \x1b"), "trailing ");
    }
}
