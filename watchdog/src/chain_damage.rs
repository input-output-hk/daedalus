//! Recognises the log signatures of a hole in the chain database.
//!
//! The wallet reports it a third way, when the chain it is given skips blocks.
//!
//! The node reports the damage in three ways: it rejects a canonical block with
//! `BadInputsUTxO` from every peer that serves it, it cannot open its ledger
//! because the ledger point is missing from the ImmutableDB, or a client reads
//! an ImmutableDB file that is gone and the database closes under the node,
//! which then runs on without a network. A detector is
//! created per node or wallet process and sees every line the process writes,
//! so the checks are plain substring tests and only a matching line is parsed.
//! A node detector looks only for the node signatures and a wallet detector
//! only for the wallet one.

use std::borrow::Cow;
use std::sync::{Arc, Mutex};

use tokio::sync::mpsc::UnboundedSender;
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
const FS_ERROR_TYPE: &str = "fsErrorType = ";
const FS_ERROR_PATH: &str = ", fsErrorPath = ";
const FS_ERROR_STRING: &str = ", fsErrorString";
const IMMUTABLE_DIR: &str = "immutable";

/// `FsErrorType`s that mean the file is not there or ends too soon. Others
/// (`FsInsufficientPermissions`, `FsDeviceFull`, `FsTooManyOpenFiles`,
/// `FsResourceAlreadyInUse`) do not mean the data is damaged, and a rebuild
/// would not clear them.
const FILE_GONE_TYPES: [&str; 2] = ["FsResourceDoesNotExist", "FsReachedEOF"];

/// The files the ImmutableDB keeps for each chunk.
const CHUNK_EXTENSIONS: [&str; 3] = ["primary", "secondary", "chunk"];

/// A file of the ImmutableDB: chunk `number`, with the extension `ext`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChunkFile {
    pub number: u64,
    ext: &'static str,
}

impl ChunkFile {
    /// The name on disk, `04929.primary`.
    pub fn name(&self) -> String {
        format!("{:05}.{}", self.number, self.ext)
    }
}

/// One recognised signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Detection {
    pub kind: ChainDamageKind,
    pub slot: Option<u64>,
    pub last_good_slot: Option<u64>,
    /// The ImmutableDB file the node failed on, for `ImmutableFileMissing`.
    pub chunk: Option<ChunkFile>,
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
        let kind = self.kind.as_str();
        if let Some(chunk) = self.chunk {
            warn!(
                "chain damage detected: kind={kind}, chunk={}, file={}",
                chunk.number,
                chunk.name()
            );
        } else {
            let slot = self
                .slot
                .map_or_else(|| "unknown".to_string(), |s| s.to_string());
            match self.last_good_slot {
                Some(last) => {
                    warn!("chain damage detected: kind={kind}, slot={slot}, last_good_slot={last}")
                }
                None => warn!("chain damage detected: kind={kind}, slot={slot}"),
            }
        }
        emit(&Event::ChainDamage {
            kind: self.kind,
            slot: self.slot,
            last_good_slot: self.last_good_slot,
            chunk: self.chunk.map(|c| c.number),
            file: self.chunk.map(|c| c.name()),
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
    immutable_file_reported: bool,
    discontinuity_reported: bool,
    /// Where a detection that needs the node stopped goes, instead of being
    /// returned from `observe` to be reported at once.
    stop_node: Option<UnboundedSender<Detection>>,
}

/// A detector shared by the stdout and stderr readers of one process.
pub type SharedDetector = Arc<Mutex<ChainDamageDetector>>;

impl ChainDamageDetector {
    /// A detector for the output of one cardano-node process that is not
    /// stopped on a detection.
    #[cfg(test)]
    pub fn node() -> Self {
        Self::new(Source::Node)
    }

    /// A detector for the output of one cardano-wallet process.
    pub fn wallet() -> Self {
        Self::new(Source::Wallet)
    }

    /// A node detector for a process the watchdog stops when its database
    /// closes under it: that detection is sent to `stop_node`, and the
    /// receiver stops the node and then reports it.
    pub fn node_stopping(stop_node: UnboundedSender<Detection>) -> Self {
        Self {
            stop_node: Some(stop_node),
            ..Self::new(Source::Node)
        }
    }

    fn new(source: Source) -> Self {
        Self {
            source,
            rejected: Vec::new(),
            block_rejected_reported: false,
            ledger_point_reported: false,
            immutable_file_reported: false,
            discontinuity_reported: false,
            stop_node: None,
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
        if !self.immutable_file_reported && line.contains(FS_ERROR_PATH) {
            return self.observe_missing_immutable_file(line);
        }
        None
    }

    /// An `FsError` for a chunk file, in a line such as the node's
    /// `Net.Mux.Local.ExceptionExit` and `Net.Mux.Remote.ExceptionExit`
    /// traces. `ClosedDBError` and `DiffusionErrored` lines are no evidence
    /// by themselves: the database closes for other reasons too, and a
    /// rebuild of a chain that is whole would cost the user a full download.
    /// They always follow the line that names the file.
    fn observe_missing_immutable_file(&mut self, line: &str) -> Option<Detection> {
        let line = strip_ansi(line);
        let chunk = missing_immutable_file(&line)?;
        self.immutable_file_reported = true;
        let detection = Detection {
            kind: ChainDamageKind::ImmutableFileMissing,
            slot: None,
            last_good_slot: None,
            chunk: Some(chunk),
        };
        match &self.stop_node {
            // A closed receiver means the run is over, and nothing is left to stop.
            Some(tx) => {
                let _ = tx.send(detection);
                None
            }
            None => Some(detection),
        }
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
            chunk: None,
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
            chunk: None,
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
            chunk: None,
        })
    }
}

/// The ImmutableDB file in the first `FsError` of `line` whose type says the
/// file is gone and whose path is a chunk file directly inside a directory
/// named `immutable`. The path is read with either separator and with any
/// run of them, which is how a path inside an escaped string looks. A path
/// outside such a directory, another file in it, or an `FsError` of another
/// type is not a match.
fn missing_immutable_file(line: &str) -> Option<ChunkFile> {
    let mut rest = line;
    while let Some(at) = rest.find(FS_ERROR_TYPE) {
        rest = &rest[at + FS_ERROR_TYPE.len()..];
        let Some((error_type, after)) = rest.split_once(FS_ERROR_PATH) else {
            continue;
        };
        if !FILE_GONE_TYPES.contains(&error_type) {
            continue;
        }
        // The comma that ends the path stays in it, so that a name which
        // ends there is complete.
        let path = after
            .find(FS_ERROR_STRING)
            .map_or(after, |end| &after[..=end]);
        if let Some(chunk) = chunk_file_in_immutable_dir(path) {
            return Some(chunk);
        }
    }
    None
}

/// The chunk file `path` names, if its last directory is `immutable`. A name
/// that runs to the end of `path` may be cut short, so it is not accepted:
/// the line that carries the rest of it is read instead.
fn chunk_file_in_immutable_dir(path: &str) -> Option<ChunkFile> {
    let mut from = 0;
    while let Some(at) = path[from..].find(IMMUTABLE_DIR) {
        let start = from + at;
        from = start + IMMUTABLE_DIR.len();
        if path[..start].ends_with(is_separator) {
            if let Some(chunk) = chunk_file_name(&path[from..]) {
                return Some(chunk);
            }
        }
    }
    None
}

fn is_separator(c: char) -> bool {
    c == '/' || c == '\\'
}

/// The chunk file named at the start of `text`, which begins with the
/// separator after the directory: `\04929.primary ...`.
fn chunk_file_name(text: &str) -> Option<ChunkFile> {
    let name = text.trim_start_matches(is_separator);
    if name.len() == text.len() {
        return None;
    }
    let digits = name.find(|c: char| !c.is_ascii_digit())?;
    let number = name[..digits].parse().ok()?;
    let ext = name[digits..].strip_prefix('.')?;
    let ext = &ext[..ext.find(|c: char| !c.is_ascii_alphanumeric())?];
    let ext = CHUNK_EXTENSIONS.iter().copied().find(|e| *e == ext)?;
    Some(ChunkFile { number, ext })
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
            chunk: None,
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
                chunk: None,
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
                chunk: None,
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
                chunk: None,
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
    // Lines in the format of a Windows case (node 11.1.3), with the user name
    // redacted. They are shortened: ` ...` marks text left out here, and the
    // lines are constructed in the node's format, not copied whole.
    const WINDOWS_LOCAL: &str = r#"[2026-11-15 01:20:38.4634Z][daedalus:Net.Mux.Local.ExceptionExit](Notice,225) With mux bearer ConnectionId LocalAddress "\\\\.\\pipe\\cardano-node-mainnet" LocalAddress "\\\\.\\pipe\\cardano-node-mainnet@1". Miniprotocol (MiniProtocolNum 5) ResponderDir terminated with exception UnexpectedFailure (FileSystemError (FsError {fsErrorType = FsResourceDoesNotExist, fsErrorPath = C:\Users\<user>\AppData\Roaming\Daedalus Mainnet\chain\immutable\04929.primary ..."#;

    const CLOSED_DB: &str = r#"[2026-11-15 01:20:38.4643Z][daedalus:Net.Mux.Local.ExceptionExit](Notice,234) With mux bearer ConnectionId LocalAddress "\\\\.\\pipe\\cardano-node-mainnet" LocalAddress "\\\\.\\pipe\\cardano-node-mainnet@2". Miniprotocol (MiniProtocolNum 5) ResponderDir terminated with exception ApiMisuse ClosedDBError CallStack (from HasCallStack):"#;

    const DIFFUSION_ERRORED: &str = r#"[2026-11-15 01:20:38.6983Z][daedalus:Startup.DiffusionInit.DiffusionErrored](Critical,19) {"data":{"error":"ExceptionInLinkedThread \"ThreadId 32\" (ApiMisuse ClosedDBError CallStack (from HasCallStack):\n  prettyCallStack, called at ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/ImmutableDB/API.hs:392:42 in ouroboros-consensus-4.2.1.0-..."#;

    // Constructed in the node's format: the pipe name 11.4.0 used, and the
    // `fsErrorNo` and call stack lines that fs-api prints after
    // `fsErrorString`, abbreviated.
    const OLD_PIPE: &str = r#"[2026-09-14 10:02:11.0213Z][daedalus:Net.Mux.Local.ExceptionExit](Notice,225) With mux bearer ConnectionId LocalAddress "\\\\.\\pipe\\daedalus-mainnet-cardano-node.socket" LocalAddress "\\\\.\\pipe\\daedalus-mainnet-cardano-node.socket@3". Miniprotocol (MiniProtocolNum 5) ResponderDir terminated with exception UnexpectedFailure (FileSystemError (FsError {fsErrorType = FsResourceDoesNotExist, fsErrorPath = C:\Users\<user>\AppData\Roaming\Daedalus Mainnet\chain\immutable\04929.primary, fsErrorString = "openFile: does not exist (The system cannot find the file specified.)", fsErrorNo = Nothing, fsErrorStack = CallStack (from HasCallStack):
  error, called at src/System/FS/API/Types.hs:1 in fs-api:System.FS.API.Types
  ioToFsError, called at src/System/FS/IO/Internal.hs:1 in fs-api:System.FS.IO.Internal"#;

    // Constructed in the node's format, for a peer reading the chunk.
    const LINUX_REMOTE: &str = r#"[2026-11-15 01:20:38.4634Z][daedalus:Net.Mux.Remote.ExceptionExit](Notice,225) With mux bearer ConnectionId {localAddress = 192.0.2.7:3001, remoteAddress = 192.0.2.9:41222}. Miniprotocol (MiniProtocolNum 2) ResponderDir terminated with exception UnexpectedFailure (FileSystemError (FsError {fsErrorType = FsResourceDoesNotExist, fsErrorPath = /home/user/.local/share/Daedalus/mainnet/chain/immutable/04929.chunk, fsErrorString = "openat: does not exist (No such file or directory)", fsErrorStack = ..."#;

    fn missing_file(number: u64, ext: &'static str) -> Detection {
        Detection {
            kind: ChainDamageKind::ImmutableFileMissing,
            slot: None,
            last_good_slot: None,
            chunk: Some(ChunkFile { number, ext }),
        }
    }

    /// `WINDOWS_LOCAL` with another error type and path.
    fn fs_error_line(error_type: &str, path: &str) -> String {
        format!(
            "[2026-11-15 01:20:38.4634Z][daedalus:Net.Mux.Local.ExceptionExit](Notice,225) \
terminated with exception UnexpectedFailure (FileSystemError (FsError {{fsErrorType = {error_type}, \
fsErrorPath = {path}, fsErrorString = \"x\"}}))\n"
        )
    }

    #[test]
    fn recognises_a_missing_chunk_on_the_windows_local_pipe() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe(WINDOWS_LOCAL),
            Some(missing_file(4929, "primary"))
        );
    }

    #[test]
    fn recognises_a_missing_chunk_on_the_old_pipe_name() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(d.observe(OLD_PIPE), Some(missing_file(4929, "primary")));
    }

    #[test]
    fn recognises_a_missing_chunk_read_by_a_peer_on_linux() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(d.observe(LINUX_REMOTE), Some(missing_file(4929, "chunk")));
    }

    #[test]
    fn names_the_file_and_the_chunk() {
        let Some(chunk) = missing_file(4929, "primary").chunk else {
            unreachable!()
        };
        assert_eq!(chunk.number, 4929);
        assert_eq!(chunk.name(), "04929.primary");
        assert_eq!(
            ChunkFile {
                number: 123_456,
                ext: "chunk"
            }
            .name(),
            "123456.chunk"
        );
    }

    #[test]
    fn recognises_each_chunk_file_extension() {
        for ext in ["primary", "secondary", "chunk"] {
            let mut d = ChainDamageDetector::node();
            let path = format!("/data/chain/immutable/00012.{ext}");
            assert_eq!(
                d.observe(&fs_error_line("FsResourceDoesNotExist", &path)),
                Some(missing_file(12, ext))
            );
        }
    }

    #[test]
    fn recognises_a_path_inside_an_escaped_string() {
        let mut d = ChainDamageDetector::node();
        let path = r"C:\\Users\\<user>\\AppData\\Roaming\\Daedalus Mainnet\\chain\\immutable\\00007.secondary";
        assert_eq!(
            d.observe(&fs_error_line("FsResourceDoesNotExist", path)),
            Some(missing_file(7, "secondary"))
        );
    }

    #[test]
    fn recognises_a_chunk_file_that_ends_too_soon() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe(&fs_error_line(
                "FsReachedEOF",
                "/c/immutable/00003.secondary"
            )),
            Some(missing_file(3, "secondary"))
        );
    }

    #[test]
    fn recognises_the_line_after_ansi_codes() {
        let mut d = ChainDamageDetector::node();
        let line = format!("\x1b[37m{WINDOWS_LOCAL}\x1b[0m");
        assert_eq!(d.observe(&line), Some(missing_file(4929, "primary")));
    }

    #[test]
    fn reports_a_missing_chunk_once() {
        let mut d = ChainDamageDetector::node();
        assert!(d.observe(WINDOWS_LOCAL).is_some());
        assert_eq!(d.observe(WINDOWS_LOCAL), None);
        assert_eq!(d.observe(LINUX_REMOTE), None);
    }

    #[test]
    fn a_closed_database_without_a_path_is_not_damage() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(d.observe(CLOSED_DB), None);
        assert_eq!(d.observe(DIFFUSION_ERRORED), None);
    }

    #[test]
    fn the_closed_database_lines_after_the_path_report_nothing_more() {
        let mut d = ChainDamageDetector::node();
        assert!(d.observe(WINDOWS_LOCAL).is_some());
        assert_eq!(d.observe(CLOSED_DB), None);
        assert_eq!(d.observe(DIFFUSION_ERRORED), None);
    }

    #[test]
    fn paths_outside_the_immutable_directory_do_not_match() {
        for path in [
            "/data/chain/volatile/blocks-0.dat",
            "/data/chain/ledger/200102375",
            "/data/chain/immutable/notes.txt",
            "/data/chain/immutable/00012.tmp",
            "/data/chain/immutable/sub/00012.chunk",
            "/data/chain/immutable/00012",
            "/data/chain/immutable_old/00012.chunk",
            "/data/chain/myimmutable/00012.chunk",
            "/data/chain/immutable/.chunk",
            "/data/immutable-backup/chain/00012.chunk",
            "/data/chain/00012.chunk",
            "/data/chain/immutable",
        ] {
            let mut d = ChainDamageDetector::node();
            assert_eq!(
                d.observe(&fs_error_line("FsResourceDoesNotExist", path)),
                None,
                "{path}"
            );
        }
    }

    #[test]
    fn errors_that_do_not_mean_the_file_is_gone_do_not_match() {
        for error_type in [
            "FsInsufficientPermissions",
            "FsDeviceFull",
            "FsTooManyOpenFiles",
            "FsResourceAlreadyInUse",
            "FsIllegalOperation",
        ] {
            let mut d = ChainDamageDetector::node();
            assert_eq!(
                d.observe(&fs_error_line(
                    error_type,
                    "/data/chain/immutable/00012.chunk"
                )),
                None,
                "{error_type}"
            );
        }
    }

    #[test]
    fn the_error_type_and_the_path_must_belong_to_one_error() {
        let mut d = ChainDamageDetector::node();
        let line = "(FsError {fsErrorType = FsDeviceFull, fsErrorPath = /data/chain/volatile/x.dat, \
fsErrorString = \"x\"}) (FsError {fsErrorType = FsResourceAlreadyInUse, fsErrorPath = \
/data/chain/immutable/00012.chunk, fsErrorString = \"y\"})\n";
        assert_eq!(d.observe(line), None);
    }

    #[test]
    fn a_path_cut_short_waits_for_the_rest_of_the_line() {
        let mut d = ChainDamageDetector::node();
        let cut = WINDOWS_LOCAL
            .split("04929.primary")
            .next()
            .unwrap()
            .to_string();
        for end in ["", "0", "04929", "04929.", "04929.pri", "04929.primary"] {
            assert_eq!(d.observe_partial(&format!("{cut}{end}")), None, "{end}");
        }
        assert_eq!(
            d.observe(WINDOWS_LOCAL),
            Some(missing_file(4929, "primary"))
        );
    }

    #[test]
    fn a_partial_line_that_holds_the_whole_path_is_reported_once() {
        let mut d = ChainDamageDetector::node();
        assert_eq!(
            d.observe_partial(WINDOWS_LOCAL),
            Some(missing_file(4929, "primary"))
        );
        assert_eq!(d.observe(WINDOWS_LOCAL), None);
    }

    #[test]
    fn a_wallet_detector_ignores_the_node_signature() {
        let mut d = ChainDamageDetector::wallet();
        assert_eq!(d.observe(WINDOWS_LOCAL), None);
    }

    #[test]
    fn a_stopping_detector_hands_the_detection_over_instead_of_returning_it() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let mut d = ChainDamageDetector::node_stopping(tx);
        assert_eq!(d.observe(WINDOWS_LOCAL), None);
        assert_eq!(rx.try_recv().ok(), Some(missing_file(4929, "primary")));
        assert_eq!(d.observe(WINDOWS_LOCAL), None);
        assert!(rx.try_recv().is_err());
    }

    #[test]
    fn a_stopping_detector_still_returns_the_other_signatures() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let mut d = ChainDamageDetector::node_stopping(tx);
        assert!(d.observe(&invariant_line(200_102_375)).is_some());
        assert!(rx.try_recv().is_err());
    }
}
