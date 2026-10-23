//! Decides, before cardano-node starts, whether the node would stop with the
//! missing ledger point error: the ledger snapshot it replays from names a
//! block that is not in the ImmutableDB. cardano-node holds that error back
//! until it is asked to stop, so a node started on such a database leaves
//! the loading screen waiting with no output. The watchdog reports the damage
//! instead of starting it.
//!
//! A refusal keeps the node from starting, and its remedy is a download of
//! the whole chain, so the check refuses only when every step is certain.
//! Anything it cannot read or does not recognise starts the node as before.
//!
//! It mirrors ouroboros-consensus 4.2.1.0, which cardano-node 11.1.3 is
//! built with, and cardano-node 11.1.3 itself:
//!
//! - Validation of the ImmutableDB. Without a `clean` marker in the database
//!   directory, or with `--validate-db`, the node validates every chunk
//!   first (`Ouroboros.Consensus.Node.Recovery.runWithCheckedDB`,
//!   `ChainDB.Impl.Args.ensureValidateAll`), which truncates the ImmutableDB
//!   at a hole, so the snapshot becomes too recent: the node is started. With
//!   the marker it validates the most recent chunk only
//!   (`ImmutableDB.Impl.Validation.validateMostRecentChunk`): from the
//!   highest-numbered `.chunk` file down, the first chunk with a valid block
//!   holds the tip, and its index files are rebuilt from the chunk file. A
//!   chunk holds a valid block when its first block matches the CRC32 in its
//!   first secondary index entry, the comparison `ImmutableDB.Impl.Parser`
//!   makes; an empty chunk file holds none. A snapshot whose block lies in
//!   the tip's chunk is therefore left to the node.
//! - The choice of snapshot (`Storage.LedgerDB.API.initialize`,
//!   `tryNewestFirst`). Snapshots are the entries of `ledger` whose name
//!   before the first `_` reads as a number, the rest being `dsSuffix`
//!   (`Storage.LedgerDB.Snapshots.snapshotFromPath`), tried highest number
//!   first (`defaultListSnapshots`). One later than the ImmutableDB tip is
//!   skipped (`snapshotTooRecent`). One the LedgerDB backend fails to load is
//!   skipped; for the LSM backend (`Storage.LedgerDB.V2.LSM.loadSnapshot`)
//!   that is a `ledger/<name>` that is a file, a missing or invalid `meta`, a
//!   backend other than `utxohd-lsm`, a missing or unreadable `utxoSize`, or
//!   a `state` file whose CRC32 is not the checksum in `meta`. A missing LSM
//!   table snapshot, `lsm/snapshots/<name>`, makes the node fail with another
//!   error. The node replays from the first snapshot that loads
//!   (`replayStartingWith`).
//! - The start of the replay (`ImmutableDB.Impl.Iterator.getSlotInfo`). The
//!   node looks the snapshot's slot up in the primary index of the chunk the
//!   slot falls in, at relative slot `slot % chunk_slots + 1`, and also at
//!   relative slot 0 when the slot is the first of its chunk and could be
//!   shared with an epoch boundary block (`Chunks.Layout.slotMightBeEBB`).
//!   When none of them is filled the lookup fails with `EmptySlot`, and the
//!   node raises the critical invariant violation. Before the lookup the
//!   node loads that chunk's primary and secondary index and the size of its
//!   chunk file (`ImmutableDB.Impl.Index.Cache`); a missing or malformed one
//!   fails with another error.
//!
//! The check compares slots, not hashes. The hash of the snapshot's block is
//! inside the encoded ledger state in its `state` file, which the check does
//! not decode, so a filled slot holding a different block starts the node.
//! The slot is taken from the snapshot's name, which is the slot of its
//! ledger state as the node and Mithril write it; the node reads the slot
//! from the state itself.

use std::fs;
use std::io::{ErrorKind, Read};
use std::path::{Path, PathBuf};

use tracing::info;

use crate::config::{LedgerBackend, NodeStorage, WatchdogConfig};

/// Size of a secondary index entry, big-endian: block offset (8), header
/// offset (2), header size (2), CRC32 of the block (4), header hash (32),
/// slot, or epoch for an epoch boundary block (8)
/// (`ImmutableDB.Impl.Index.Secondary.Entry`).
const SECONDARY_ENTRY: usize = 56;

/// Version byte at the start of a primary index file
/// (`ImmutableDB.Impl.Index.Primary.currentVersionNumber`).
const PRIMARY_VERSION: u8 = 1;

/// Largest `meta` or `utxoSize` file read. The node writes a few dozen bytes.
const SMALL_FILE_LIMIT: u64 = 64 * 1024;

/// Largest first block read from a chunk file, far above any Cardano block.
const BLOCK_LIMIT: u64 = 64 * 1024 * 1024;

/// What the check found.
#[derive(Debug, PartialEq, Eq)]
pub enum Verdict {
    /// The node would replay from the ledger snapshot at `slot` and stop,
    /// because that slot holds no block in the ImmutableDB.
    BlockMissing { slot: u64 },
    /// The node does not stop on a missing ledger point, for this reason.
    Start(String),
    /// The check cannot tell, for this reason, and the node is started.
    Unknown(String),
}

/// The verdict for the database the node is about to be started on.
pub async fn check_before_start(config: &WatchdogConfig) -> Verdict {
    let Some(storage) = config.node.storage.clone() else {
        return Verdict::Unknown("the node's storage layout could not be read".to_string());
    };
    let db = match database_path(&config.node.args, Path::new(&config.node.state_dir)) {
        Ok(db) => db,
        Err(reason) => return Verdict::Unknown(reason),
    };
    tokio::task::spawn_blocking(move || check(&db, &storage))
        .await
        .unwrap_or_else(|e| Verdict::Unknown(format!("the check did not finish: {e}")))
}

/// The database directory in the node's arguments, relative to its working
/// directory `cwd` when it is relative. Only the one-path form is read, and
/// with `--validate-db` the node validates every chunk first.
fn database_path(args: &[String], cwd: &Path) -> Result<PathBuf, String> {
    let mut path = None;
    let mut args = args.iter();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--database-path" => {
                if path.is_some() {
                    return Err("the node is given --database-path twice".to_string());
                }
                path = Some(args.next().ok_or("--database-path has no value")?);
            }
            "--validate-db" => {
                return Err(
                    "the node is given --validate-db, so it validates every chunk first"
                        .to_string(),
                );
            }
            other
                if other.starts_with("--database-path=")
                    || other.starts_with("--immutable-database-path")
                    || other.starts_with("--volatile-database-path") =>
            {
                return Err(format!(
                    "the node is given {other}, which this check does not read"
                ));
            }
            _ => {}
        }
    }
    let path = PathBuf::from(path.ok_or("the node is given no --database-path")?);
    Ok(if path.is_absolute() {
        path
    } else {
        cwd.join(path)
    })
}

/// The verdict for the database in `db`. It reads files, so it blocks.
pub fn check(db: &Path, storage: &NodeStorage) -> Verdict {
    decide(db, storage).unwrap_or_else(Verdict::Unknown)
}

fn decide(db: &Path, storage: &NodeStorage) -> Result<Verdict, String> {
    let lsm = match &storage.backend {
        LedgerBackend::Lsm(dir) => db.join(dir),
        LedgerBackend::Other(name) => {
            return Err(format!(
                "the node's LedgerDB backend is {name}, which this check does not cover"
            ));
        }
    };
    if !is_file(&db.join("clean"))? {
        return Ok(Verdict::Start(
            "the database has no clean marker, so the node validates every chunk first".to_string(),
        ));
    }
    let immutable = db.join("immutable");
    let Some(tip_chunk) = tip_chunk(&immutable, &chunk_files(&immutable)?)? else {
        return Ok(Verdict::Start(
            "the ImmutableDB holds no block, so every ledger snapshot is too recent".to_string(),
        ));
    };
    let ledger = db.join("ledger");
    for snapshot in snapshots(&ledger)? {
        let name = snapshot.dir_name();
        let chunk = snapshot.slot / storage.chunk_slots;
        if chunk > tip_chunk {
            info!(
                "ledger snapshot {name} is past the ImmutableDB tip in chunk {tip_chunk}; \
                 the node skips it"
            );
            continue;
        }
        if chunk == tip_chunk {
            return Ok(Verdict::Start(format!(
                "ledger snapshot {name} is in chunk {chunk}, the one the node validates \
                 before it opens the ledger"
            )));
        }
        let checksum = match load(&ledger.join(&name), &lsm, &name)? {
            Load::Skipped(why) => {
                info!("the node skips ledger snapshot {name}: {why}");
                continue;
            }
            Load::Loads { checksum } => checksum,
        };
        if block_in_index(&immutable, chunk, snapshot.slot, storage.chunk_slots)? {
            return Ok(Verdict::Start(format!(
                "the block of ledger snapshot {name} is in chunk {chunk}"
            )));
        }
        // The node reads the state before it replays and skips a snapshot
        // whose state fails its checksum. Reading the whole state file is
        // left until here, where it decides between a refusal and the next
        // snapshot.
        if file_crc(&ledger.join(&name).join("state"))? != checksum {
            info!(
                "the node skips ledger snapshot {name}: its state does not match the \
                 checksum in its meta file"
            );
            continue;
        }
        return Ok(Verdict::BlockMissing {
            slot: snapshot.slot,
        });
    }
    Ok(Verdict::Start(
        "the node loads no ledger snapshot and replays the ledger from genesis".to_string(),
    ))
}

/// The file name of a chunk's file of the given type, `06331.chunk` for
/// instance (`ImmutableDB.Impl.Util.renderFile`).
fn file_name(chunk: u64, kind: &str) -> String {
    format!("{chunk:05}.{kind}")
}

fn not_found(e: &std::io::Error) -> bool {
    matches!(e.kind(), ErrorKind::NotFound | ErrorKind::NotADirectory)
}

/// Whether `path` is a file, as cardano-node's `doesFileExist` answers.
fn is_file(path: &Path) -> Result<bool, String> {
    match fs::metadata(path) {
        Ok(m) => Ok(m.is_file()),
        Err(e) if not_found(&e) => Ok(false),
        Err(e) => Err(format!("cannot read {}: {e}", path.display())),
    }
}

/// The contents of `path`, which must be a file of at most `limit` bytes.
fn read_limited(path: &Path, limit: u64) -> Result<Vec<u8>, String> {
    let file = fs::File::open(path).map_err(|e| format!("cannot open {}: {e}", path.display()))?;
    let mut bytes = Vec::new();
    file.take(limit + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    if bytes.len() as u64 > limit {
        return Err(format!("{} is larger than expected", path.display()));
    }
    Ok(bytes)
}

/// How Haskell's `readMaybe` reads `text` as a `Word64`, where the result
/// is certain: the number for plain decimal digits, and None for text
/// without a digit, which no reading accepts. Other text with a digit, such
/// as `0x10`, ` 12` or a number past 64 bits, is an error, because
/// `readMaybe` accepts forms this does not mirror.
fn read_word64(text: &str) -> Result<Option<u64>, String> {
    if !text.is_empty() && text.bytes().all(|b| b.is_ascii_digit()) {
        return text
            .parse()
            .map(Some)
            .map_err(|_| format!("{text} is past the range of a 64-bit number"));
    }
    if text.bytes().any(|b| b.is_ascii_digit()) {
        return Err(format!(
            "{text:?} may read as a number in a form this check does not read"
        ));
    }
    Ok(None)
}

/// The numbers of the `.chunk` files in `immutable`, highest first, with
/// the names read as `ImmutableDB.Impl.Util.parseDBFile` reads them: a
/// number, one dot and the file type. `.epoch` files, which the node renames
/// to `.chunk` files before it validates, leave the check undecided.
fn chunk_files(immutable: &Path) -> Result<Vec<u64>, String> {
    let entries = match fs::read_dir(immutable) {
        Ok(entries) => entries,
        Err(e) if not_found(&e) => return Ok(Vec::new()),
        Err(e) => return Err(format!("cannot list {}: {e}", immutable.display())),
    };
    let mut chunks = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| format!("cannot list {}: {e}", immutable.display()))?;
        let Ok(name) = entry.file_name().into_string() else {
            return Err(format!(
                "{} holds a name that is not Unicode",
                immutable.display()
            ));
        };
        let Some((number, kind)) = name.split_once('.') else {
            continue;
        };
        if kind != "chunk" && kind != "epoch" {
            continue;
        }
        let Some(n) = read_word64(number)? else {
            continue;
        };
        if kind == "epoch" {
            return Err(format!(
                "{} holds {name}, which the node renames before it validates",
                immutable.display()
            ));
        }
        chunks.push(n);
    }
    chunks.sort_unstable_by(|a, b| b.cmp(a));
    chunks.dedup();
    Ok(chunks)
}

/// The chunk that holds the ImmutableDB tip once the node has validated
/// the most recent chunk, or None when no chunk holds a block. The node
/// walks down from the highest-numbered chunk file, past chunk files that
/// are missing or hold no valid block, to the first that holds one.
fn tip_chunk(immutable: &Path, chunks: &[u64]) -> Result<Option<u64>, String> {
    for &chunk in chunks {
        let path = immutable.join(file_name(chunk, "chunk"));
        let size = match fs::metadata(&path) {
            Ok(m) if m.is_file() => m.len(),
            Ok(_) => continue,
            Err(e) if not_found(&e) => continue,
            Err(e) => return Err(format!("cannot read {}: {e}", path.display())),
        };
        if size == 0 {
            continue;
        }
        if !first_block_matches_index(immutable, chunk, &path, size)? {
            return Err(format!(
                "the first block of chunk {chunk} does not match its secondary index, so \
                 the node's validation decides where the ImmutableDB ends"
            ));
        }
        return Ok(Some(chunk));
    }
    Ok(None)
}

/// Whether the first block in the chunk file at `path`, `size` bytes long,
/// has the CRC32 that the first entry of the chunk's secondary index
/// records. The block runs to the second entry's block offset, or to the end
/// of the file when there is one entry.
fn first_block_matches_index(
    immutable: &Path,
    chunk: u64,
    path: &Path,
    size: u64,
) -> Result<bool, String> {
    let secondary = immutable.join(file_name(chunk, "secondary"));
    let mut index = fs::File::open(&secondary)
        .map_err(|e| format!("cannot open {}: {e}", secondary.display()))?;
    let len = index
        .metadata()
        .map_err(|e| format!("cannot read {}: {e}", secondary.display()))?
        .len();
    let entry = SECONDARY_ENTRY as u64;
    if len < entry || len % entry != 0 {
        return Err(format!(
            "{} is {len} bytes, not a whole number of entries",
            secondary.display()
        ));
    }
    let mut head = vec![
        0u8;
        if len >= 2 * entry {
            SECONDARY_ENTRY + 8
        } else {
            SECONDARY_ENTRY
        }
    ];
    index
        .read_exact(&mut head)
        .map_err(|e| format!("cannot read {}: {e}", secondary.display()))?;
    let first = Entry::parse(&head[..SECONDARY_ENTRY]);
    if first.block_offset != 0 {
        return Err(format!(
            "the first entry of {} does not start at the start of the chunk",
            secondary.display()
        ));
    }
    let end = match head.get(SECONDARY_ENTRY..) {
        Some(next) if next.len() == 8 => u64::from_be_bytes(next.try_into().expect("8 bytes")),
        _ => size,
    };
    if end == 0 || end > size || end > BLOCK_LIMIT {
        return Err(format!(
            "the first block of chunk {chunk} would end at byte {end} of a {size}-byte file"
        ));
    }
    let mut block = vec![0u8; end as usize];
    fs::File::open(path)
        .and_then(|mut f| f.read_exact(&mut block))
        .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let mut crc = Crc32::new();
    crc.update(&block);
    Ok(crc.finish() == first.checksum)
}

/// The fields of a secondary index entry that the check reads.
struct Entry {
    block_offset: u64,
    checksum: u32,
    /// The slot of a block, or the epoch of an epoch boundary block.
    slot_or_epoch: u64,
}

impl Entry {
    /// Reads one entry; `bytes` is SECONDARY_ENTRY bytes long.
    fn parse(bytes: &[u8]) -> Self {
        let u64_at = |at: usize| u64::from_be_bytes(bytes[at..at + 8].try_into().expect("8 bytes"));
        Entry {
            block_offset: u64_at(0),
            checksum: u32::from_be_bytes(bytes[12..16].try_into().expect("4 bytes")),
            slot_or_epoch: u64_at(48),
        }
    }
}

/// A ledger snapshot as the node names it.
#[derive(Debug, PartialEq, Eq)]
struct Snapshot {
    slot: u64,
    suffix: Option<String>,
}

impl Snapshot {
    /// The directory the node reads the snapshot from, its number and suffix
    /// written back (`Storage.LedgerDB.Snapshots.snapshotToDirName`).
    fn dir_name(&self) -> String {
        match self.suffix {
            Some(ref suffix) => format!("{}_{suffix}", self.slot),
            None => self.slot.to_string(),
        }
    }
}

/// The ledger snapshots in `ledger`, in the order the node tries them:
/// highest number first, and names with the same number in the order of
/// the names (`Storage.LedgerDB.Snapshots.defaultListSnapshots`, a stable
/// sort of an ordered set).
fn snapshots(ledger: &Path) -> Result<Vec<Snapshot>, String> {
    let entries = match fs::read_dir(ledger) {
        Ok(entries) => entries,
        Err(e) if not_found(&e) => return Ok(Vec::new()),
        Err(e) => return Err(format!("cannot list {}: {e}", ledger.display())),
    };
    let mut names = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| format!("cannot list {}: {e}", ledger.display()))?;
        let Ok(name) = entry.file_name().into_string() else {
            return Err(format!(
                "{} holds a name that is not Unicode",
                ledger.display()
            ));
        };
        names.push(name);
    }
    names.sort();
    let mut snapshots = Vec::new();
    for name in names {
        let (number, suffix) = match name.split_once('_') {
            Some((number, suffix)) => (number, Some(suffix.to_string())),
            None => (name.as_str(), None),
        };
        if let Some(slot) = read_word64(number)? {
            snapshots.push(Snapshot { slot, suffix });
        }
    }
    snapshots.sort_by_key(|s| std::cmp::Reverse(s.slot));
    Ok(snapshots)
}

/// What the node's LSM backend does with a snapshot before it replays, as
/// far as that is certain without decoding the ledger state.
enum Load {
    /// The node skips the snapshot, for this reason.
    Skipped(String),
    /// The node loads the snapshot, if its state has this CRC32.
    Loads { checksum: u32 },
}

/// What `Storage.LedgerDB.V2.LSM.loadSnapshot` does with the snapshot in
/// `dir`, named `name`, with the LSM database in `lsm`.
fn load(dir: &Path, lsm: &Path, name: &str) -> Result<Load, String> {
    if is_file(dir)? {
        return Ok(Load::Skipped(
            "it is a file, which the node takes for a snapshot of an older format".to_string(),
        ));
    }
    let meta_path = dir.join("meta");
    if !is_file(&meta_path)? {
        return Ok(Load::Skipped("it has no meta file".to_string()));
    }
    let meta: serde_json::Value =
        serde_json::from_slice(&read_limited(&meta_path, SMALL_FILE_LIMIT)?)
            .map_err(|e| format!("cannot parse {}: {e}", meta_path.display()))?;
    let checksum = match read_meta(&meta)? {
        Load::Loads { checksum } => checksum,
        skipped => return Ok(skipped),
    };
    let utxo_size_path = dir.join("utxoSize");
    if !is_file(&utxo_size_path)? {
        return Ok(Load::Skipped("it has no utxoSize file".to_string()));
    }
    match read_int(&read_limited(&utxo_size_path, SMALL_FILE_LIMIT)?)? {
        Some(size) if size >= 0 => {}
        _ => {
            return Ok(Load::Skipped("its utxoSize file holds no size".to_string()));
        }
    }
    let table = lsm.join("snapshots").join(name);
    match fs::metadata(&table) {
        Ok(m) if m.is_dir() => Ok(Load::Loads { checksum }),
        Ok(_) => Err(format!(
            "{} is not a directory, so the node would fail with another error",
            table.display()
        )),
        Err(e) if not_found(&e) => Err(format!(
            "the LSM database has no snapshot {name}, so the node would fail with another error"
        )),
        Err(e) => Err(format!("cannot read {}: {e}", table.display())),
    }
}

/// What the node makes of a snapshot's `meta` file, read as
/// `Storage.LedgerDB.Snapshots.SnapshotMetadata` parses it: the backend, the
/// checksum, a Word32, and the tables codec version, which must be 1. Any
/// failure skips the snapshot. A number this check cannot be sure the
/// parser reads the same way leaves it undecided.
fn read_meta(meta: &serde_json::Value) -> Result<Load, String> {
    let Some(fields) = meta.as_object() else {
        return Ok(Load::Skipped(
            "its meta file is not a JSON object".to_string(),
        ));
    };
    match fields.get("backend") {
        Some(serde_json::Value::String(backend)) if backend == "utxohd-lsm" => {}
        Some(serde_json::Value::String(backend)) => {
            return Ok(Load::Skipped(format!(
                "its meta file names the {backend} backend"
            )));
        }
        _ => return Ok(Load::Skipped("its meta file names no backend".to_string())),
    }
    let checksum = match fields.get("checksum") {
        None => return Ok(Load::Skipped("its meta file has no checksum".to_string())),
        Some(value) => value
            .as_u64()
            .and_then(|c| u32::try_from(c).ok())
            .ok_or_else(|| format!("the meta file's checksum {value} is not a 32-bit number"))?,
    };
    match fields.get("tablesCodecVersion").map(|v| (v, v.as_u64())) {
        None => Ok(Load::Skipped(
            "its meta file has no tables codec version".to_string(),
        )),
        Some((_, Some(1))) => Ok(Load::Loads { checksum }),
        Some((_, Some(version))) => Ok(Load::Skipped(format!(
            "its meta file has tables codec version {version}"
        ))),
        Some((value, None)) => Err(format!(
            "the meta file's tables codec version {value} is not a whole number"
        )),
    }
}

/// What `Data.ByteString.Char8.readInt` reads from the start of `bytes`: an
/// optional sign and the digits after it, or None when no digit follows. A
/// number past 64 bits is an error, as bytestring versions differ there.
fn read_int(bytes: &[u8]) -> Result<Option<i64>, String> {
    let (negative, rest) = match bytes.first() {
        Some(b'-') => (true, &bytes[1..]),
        Some(b'+') => (false, &bytes[1..]),
        _ => (false, bytes),
    };
    let digits = rest.iter().take_while(|b| b.is_ascii_digit()).count();
    if digits == 0 {
        return Ok(None);
    }
    let text = std::str::from_utf8(&rest[..digits]).expect("ASCII digits");
    let value: i64 = text
        .parse()
        .map_err(|_| "the utxoSize file holds a number past 64 bits".to_string())?;
    Ok(Some(if negative { -value } else { value }))
}

/// Whether the node finds a block at `slot`, in `chunk`, when it starts the
/// replay there: whether the chunk's primary index fills the slot's relative
/// slot. The secondary index must agree. The slot that starts a chunk can
/// also be an epoch boundary block's, which the node tells apart by hash, so
/// a filled boundary slot leaves the check undecided.
fn block_in_index(
    immutable: &Path,
    chunk: u64,
    slot: u64,
    chunk_slots: u64,
) -> Result<bool, String> {
    let chunk_file = immutable.join(file_name(chunk, "chunk"));
    if !is_file(&chunk_file)? {
        return Err(format!(
            "{} is missing, so the node would fail with another error",
            chunk_file.display()
        ));
    }
    // Relative slot 0 is kept for an epoch boundary block and the chunk's
    // slots follow it, so a whole primary index holds chunk_slots + 2
    // offsets after its version byte.
    let primary_path = immutable.join(file_name(chunk, "primary"));
    let primary = read_limited(&primary_path, 1 + 4 * (chunk_slots + 2))?;
    if primary.len() < 5 || (primary.len() - 1) % 4 != 0 || primary[0] != PRIMARY_VERSION {
        return Err(format!(
            "{} is not a primary index the node loads",
            primary_path.display()
        ));
    }
    let offsets: Vec<u32> = primary[1..]
        .chunks_exact(4)
        .map(|b| u32::from_be_bytes(b.try_into().expect("4 bytes")))
        .collect();
    // `Primary.containsSlot` and `Primary.isFilledSlot`.
    let filled = |relative: usize| {
        relative + 1 < offsets.len() && offsets[relative] != offsets[relative + 1]
    };
    let within = slot % chunk_slots;
    let in_primary = filled(within as usize + 1);
    if within == 0 && filled(0) && !in_primary {
        return Err(format!(
            "slot {slot} starts chunk {chunk}, which holds an epoch boundary block that the \
             node tells apart by hash"
        ));
    }
    let secondary_path = immutable.join(file_name(chunk, "secondary"));
    let secondary = read_limited(&secondary_path, SECONDARY_ENTRY as u64 * (chunk_slots + 1))?;
    if secondary.len() % SECONDARY_ENTRY != 0 {
        return Err(format!(
            "{} is not a whole number of entries",
            secondary_path.display()
        ));
    }
    let first_is_boundary = filled(0);
    let in_secondary = secondary
        .chunks_exact(SECONDARY_ENTRY)
        .enumerate()
        .any(|(i, entry)| {
            !(i == 0 && first_is_boundary) && Entry::parse(entry).slot_or_epoch == slot
        });
    if in_primary != in_secondary {
        return Err(format!(
            "the primary and secondary index of chunk {chunk} disagree about slot {slot}"
        ));
    }
    Ok(in_primary)
}

/// The CRC32 of the file at `path`, read in pieces.
fn file_crc(path: &Path) -> Result<u32, String> {
    let mut file =
        fs::File::open(path).map_err(|e| format!("cannot open {}: {e}", path.display()))?;
    let mut crc = Crc32::new();
    let mut buf = vec![0u8; 1024 * 1024];
    loop {
        let n = file
            .read(&mut buf)
            .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
        if n == 0 {
            return Ok(crc.finish());
        }
        crc.update(&buf[..n]);
    }
}

/// CRC-32 as zlib computes it (reflected, polynomial 0xEDB88320), which is
/// what the node's checksums are: `System.FS.CRC` uses the `digest`
/// package's `crc32`.
struct Crc32(u32);

const CRC_TABLE: [u32; 256] = crc_table();

const fn crc_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut c = i as u32;
        let mut bit = 0;
        while bit < 8 {
            c = if c & 1 != 0 {
                0xEDB8_8320 ^ (c >> 1)
            } else {
                c >> 1
            };
            bit += 1;
        }
        table[i] = c;
        i += 1;
    }
    table
}

impl Crc32 {
    fn new() -> Self {
        Crc32(0xFFFF_FFFF)
    }

    fn update(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.0 = CRC_TABLE[((self.0 ^ u32::from(b)) & 0xFF) as usize] ^ (self.0 >> 8);
        }
    }

    fn finish(&self) -> u32 {
        !self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A chunk size small enough to write whole primary indexes quickly.
    const CHUNK_SLOTS: u64 = 100;

    fn storage() -> NodeStorage {
        NodeStorage {
            chunk_slots: CHUNK_SLOTS,
            backend: LedgerBackend::Lsm(PathBuf::from("lsm")),
        }
    }

    fn crc(bytes: &[u8]) -> u32 {
        let mut crc = Crc32::new();
        crc.update(bytes);
        crc.finish()
    }

    /// A database directory with a `clean` marker, removed on drop.
    struct Db(PathBuf);

    impl Db {
        fn new(label: &str) -> Self {
            let path = std::env::temp_dir().join(format!(
                "wdg-ledger-point-{label}-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            fs::create_dir_all(path.join("immutable")).unwrap();
            fs::create_dir_all(path.join("ledger")).unwrap();
            fs::write(path.join("clean"), b"").unwrap();
            Db(path)
        }

        fn check(&self) -> Verdict {
            check(&self.0, &storage())
        }

        fn immutable(&self, chunk: u64, kind: &str) -> PathBuf {
            self.0.join("immutable").join(file_name(chunk, kind))
        }

        /// Writes chunk `chunk` with a block at each of `slots`: the chunk
        /// file, a secondary index entry per block carrying its CRC32, and a
        /// whole primary index. With `boundary`, an epoch boundary block
        /// comes first, in relative slot 0.
        fn chunk_with(&self, chunk: u64, boundary: bool, slots: &[u64]) {
            let mut data = Vec::new();
            let mut secondary = Vec::new();
            let mut filled = vec![false; CHUNK_SLOTS as usize + 1];
            let mut add = |relative: usize, slot_or_epoch: u64, block: Vec<u8>| {
                secondary.extend_from_slice(&(data.len() as u64).to_be_bytes());
                secondary.extend_from_slice(&[0, 0, 0, 0]);
                secondary.extend_from_slice(&crc(&block).to_be_bytes());
                secondary.extend_from_slice(&[0xAB; 32]);
                secondary.extend_from_slice(&slot_or_epoch.to_be_bytes());
                data.extend_from_slice(&block);
                filled[relative] = true;
            };
            if boundary {
                add(
                    0,
                    chunk,
                    format!("boundary block of epoch {chunk}").into_bytes(),
                );
            }
            for &slot in slots {
                assert_eq!(
                    slot / CHUNK_SLOTS,
                    chunk,
                    "slot {slot} is not in chunk {chunk}"
                );
                let relative = (slot % CHUNK_SLOTS) as usize + 1;
                add(relative, slot, format!("block at slot {slot}").into_bytes());
            }
            let mut primary = vec![PRIMARY_VERSION];
            let mut offset = 0u32;
            primary.extend_from_slice(&offset.to_be_bytes());
            for f in filled {
                if f {
                    offset += SECONDARY_ENTRY as u32;
                }
                primary.extend_from_slice(&offset.to_be_bytes());
            }
            fs::write(self.immutable(chunk, "chunk"), data).unwrap();
            fs::write(self.immutable(chunk, "secondary"), secondary).unwrap();
            fs::write(self.immutable(chunk, "primary"), primary).unwrap();
        }

        fn chunk(&self, chunk: u64, slots: &[u64]) {
            self.chunk_with(chunk, false, slots);
        }

        /// Chunks 0 to 2. Chunk 0 ends early, at slot 20: slots 30 to 99
        /// are a hole. Chunk 2 holds the tip.
        fn with_hole(label: &str) -> Self {
            let db = Db::new(label);
            db.chunk(0, &[10, 20]);
            db.chunk(1, &[110, 150]);
            db.chunk(2, &[210, 250]);
            db
        }

        fn snapshot_dir(&self, name: &str) -> PathBuf {
            self.0.join("ledger").join(name)
        }

        /// A ledger snapshot as the LSM backend writes it: `state`, a `meta`
        /// that carries the state's CRC32, `utxoSize`, and the LSM table
        /// snapshot.
        fn snapshot(&self, name: &str) {
            let dir = self.snapshot_dir(name);
            fs::create_dir_all(&dir).unwrap();
            let state = format!("ledger state {name}").into_bytes();
            fs::write(dir.join("state"), &state).unwrap();
            self.meta(
                name,
                serde_json::json!({
                    "backend": "utxohd-lsm",
                    "checksum": crc(&state),
                    "tablesCodecVersion": 1
                }),
            );
            fs::write(dir.join("utxoSize"), b"1234").unwrap();
            fs::create_dir_all(self.0.join("lsm").join("snapshots").join(name)).unwrap();
        }

        fn meta(&self, name: &str, meta: serde_json::Value) {
            fs::write(self.snapshot_dir(name).join("meta"), meta.to_string()).unwrap();
        }
    }

    impl Drop for Db {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn starts(verdict: Verdict) -> String {
        match verdict {
            Verdict::Start(reason) => reason,
            other => panic!("expected the node to start, got {other:?}"),
        }
    }

    fn unknown(verdict: Verdict) -> String {
        match verdict {
            Verdict::Unknown(reason) => reason,
            other => panic!("expected no decision, got {other:?}"),
        }
    }

    // ── The verdicts ──────────────────────────────────────────────────────

    #[test]
    fn refuses_a_snapshot_whose_block_is_missing() {
        let db = Db::with_hole("missing");
        db.snapshot("50");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
    }

    #[test]
    fn starts_when_the_snapshot_block_is_present() {
        let db = Db::with_hole("present");
        db.snapshot("150");
        assert!(starts(db.check()).contains("is in chunk 1"));
    }

    #[test]
    fn starts_without_snapshots() {
        let db = Db::with_hole("no-snapshots");
        assert!(starts(db.check()).contains("from genesis"));
        fs::remove_dir_all(db.0.join("ledger")).unwrap();
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn starts_without_a_clean_marker() {
        let db = Db::with_hole("unclean");
        db.snapshot("50");
        fs::remove_file(db.0.join("clean")).unwrap();
        assert!(starts(db.check()).contains("no clean marker"));
    }

    #[test]
    fn starts_when_the_immutable_db_holds_no_block() {
        let db = Db::new("empty-immutable");
        db.snapshot("50");
        assert!(starts(db.check()).contains("holds no block"));
    }

    // ── Which snapshot the node replays from ──────────────────────────────

    #[test]
    fn skips_a_snapshot_past_the_tip() {
        let db = Db::with_hole("too-recent");
        db.snapshot("950");
        assert!(starts(db.check()).contains("from genesis"));
        db.snapshot("50");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
    }

    #[test]
    fn leaves_a_snapshot_in_the_tip_chunk_to_the_node() {
        let db = Db::with_hole("tip-chunk");
        db.snapshot("299");
        db.snapshot("50");
        assert!(starts(db.check()).contains("the one the node validates"));
    }

    #[test]
    fn tries_the_newest_snapshot_first() {
        let db = Db::with_hole("newest");
        db.snapshot("50");
        db.snapshot("150");
        assert!(starts(db.check()).contains("is in chunk 1"));
    }

    #[test]
    fn reads_a_suffixed_snapshot() {
        let db = Db::with_hole("suffix");
        db.snapshot("50_db-analyser");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
    }

    #[test]
    fn tries_names_with_the_same_number_in_name_order() {
        let db = Db::with_hole("same-number");
        db.snapshot("50");
        db.snapshot("50_kept");
        // "50" comes before "50_kept"; skipping it leaves the suffixed one.
        db.meta(
            "50",
            serde_json::json!({"backend": "utxohd-mem", "checksum": 1, "tablesCodecVersion": 1}),
        );
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
        fs::remove_dir_all(db.snapshot_dir("50_kept")).unwrap();
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn ignores_entries_without_a_number() {
        let db = Db::with_hole("other-entries");
        fs::create_dir_all(db.snapshot_dir("lsm")).unwrap();
        fs::write(db.snapshot_dir("notes.txt"), b"").unwrap();
        db.snapshot("50");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
    }

    #[test]
    fn cannot_tell_from_a_name_that_may_read_as_a_number() {
        for name in ["0x10", " 50", "50.old", "99999999999999999999"] {
            let db = Db::with_hole("odd-name");
            fs::create_dir_all(db.snapshot_dir(name)).unwrap();
            db.snapshot("50");
            unknown(db.check());
        }
    }

    // ── Snapshots the LSM backend does not load ───────────────────────────

    #[test]
    fn skips_a_snapshot_whose_state_fails_its_checksum() {
        let db = Db::with_hole("bad-state");
        db.snapshot("60");
        fs::write(db.snapshot_dir("60").join("state"), b"changed").unwrap();
        assert!(starts(db.check()).contains("from genesis"));
        db.snapshot("30");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 30 });
    }

    #[test]
    fn skips_a_snapshot_without_a_meta_file() {
        let db = Db::with_hole("no-meta");
        db.snapshot("50");
        fs::remove_file(db.snapshot_dir("50").join("meta")).unwrap();
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn skips_a_snapshot_of_another_backend() {
        let db = Db::with_hole("other-backend");
        db.snapshot("50");
        db.meta(
            "50",
            serde_json::json!({"backend": "utxohd-mem", "checksum": 1, "tablesCodecVersion": 1}),
        );
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn skips_a_snapshot_whose_meta_is_incomplete() {
        for meta in [
            serde_json::json!({"checksum": 1, "tablesCodecVersion": 1}),
            serde_json::json!({"backend": "utxohd-lsm", "tablesCodecVersion": 1}),
            serde_json::json!({"backend": "utxohd-lsm", "checksum": 1}),
            serde_json::json!({"backend": "utxohd-lsm", "checksum": 1, "tablesCodecVersion": 2}),
            serde_json::json!(["utxohd-lsm"]),
        ] {
            let db = Db::with_hole("incomplete-meta");
            db.snapshot("50");
            db.meta("50", meta);
            assert!(starts(db.check()).contains("from genesis"));
        }
    }

    #[test]
    fn cannot_tell_from_a_meta_number_it_does_not_read() {
        for meta in [
            serde_json::json!({"backend": "utxohd-lsm", "checksum": 1.5, "tablesCodecVersion": 1}),
            serde_json::json!({"backend": "utxohd-lsm", "checksum": 4294967296u64, "tablesCodecVersion": 1}),
            serde_json::json!({"backend": "utxohd-lsm", "checksum": 1, "tablesCodecVersion": 1.0}),
        ] {
            let db = Db::with_hole("odd-meta");
            db.snapshot("50");
            db.meta("50", meta);
            unknown(db.check());
        }
        let db = Db::with_hole("unparsed-meta");
        db.snapshot("50");
        fs::write(db.snapshot_dir("50").join("meta"), b"{not json").unwrap();
        unknown(db.check());
    }

    #[test]
    fn skips_a_snapshot_without_a_utxo_size() {
        let db = Db::with_hole("no-utxo-size");
        db.snapshot("50");
        fs::remove_file(db.snapshot_dir("50").join("utxoSize")).unwrap();
        assert!(starts(db.check()).contains("from genesis"));
        fs::write(db.snapshot_dir("50").join("utxoSize"), b"-5").unwrap();
        assert!(starts(db.check()).contains("from genesis"));
        fs::write(db.snapshot_dir("50").join("utxoSize"), b" 5").unwrap();
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn skips_a_snapshot_that_is_a_file() {
        let db = Db::with_hole("legacy");
        fs::write(db.snapshot_dir("50"), b"an old snapshot").unwrap();
        assert!(starts(db.check()).contains("from genesis"));
    }

    #[test]
    fn cannot_tell_without_the_lsm_table_snapshot() {
        let db = Db::with_hole("no-lsm-snapshot");
        db.snapshot("50");
        fs::remove_dir_all(db.0.join("lsm").join("snapshots").join("50")).unwrap();
        assert!(unknown(db.check()).contains("LSM database has no snapshot 50"));
    }

    // ── The ImmutableDB as the node validates and reads it ───────────────

    #[test]
    fn finds_the_tip_below_an_empty_last_chunk() {
        let db = Db::with_hole("empty-last");
        fs::write(db.immutable(3, "chunk"), b"").unwrap();
        db.snapshot("50");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
        // Chunk 2 holds the tip, so its snapshot is left to the node.
        db.snapshot("250");
        assert!(starts(db.check()).contains("chunk 2, the one the node validates"));
    }

    #[test]
    fn finds_the_tip_below_a_missing_chunk_file() {
        let db = Db::with_hole("unpadded");
        // Listed as chunk 4, which the node opens as 00004.chunk.
        fs::write(db.0.join("immutable").join("4.chunk"), b"x").unwrap();
        db.snapshot("50");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 50 });
    }

    #[test]
    fn cannot_tell_when_the_tip_chunk_fails_its_checksum() {
        let db = Db::with_hole("bad-tip");
        fs::write(db.immutable(2, "chunk"), b"something else entirely").unwrap();
        db.snapshot("50");
        assert!(unknown(db.check()).contains("first block of chunk 2"));
    }

    #[test]
    fn cannot_tell_when_the_tip_chunk_has_no_secondary_index() {
        let db = Db::with_hole("tip-no-index");
        fs::remove_file(db.immutable(2, "secondary")).unwrap();
        db.snapshot("50");
        unknown(db.check());
    }

    #[test]
    fn cannot_tell_with_epoch_files() {
        let db = Db::with_hole("epoch-files");
        fs::write(db.0.join("immutable").join("00005.epoch"), b"").unwrap();
        db.snapshot("50");
        unknown(db.check());
    }

    #[test]
    fn cannot_tell_when_the_chunk_file_is_missing() {
        let db = Db::with_hole("missing-chunk");
        fs::remove_file(db.immutable(0, "chunk")).unwrap();
        db.snapshot("50");
        assert!(unknown(db.check()).contains("is missing"));
    }

    #[test]
    fn cannot_tell_when_an_index_file_is_missing() {
        for kind in ["primary", "secondary"] {
            let db = Db::with_hole("missing-index");
            fs::remove_file(db.immutable(0, kind)).unwrap();
            db.snapshot("50");
            unknown(db.check());
        }
    }

    #[test]
    fn cannot_tell_when_the_secondary_index_is_cut_short() {
        let db = Db::with_hole("short-secondary");
        let path = db.immutable(0, "secondary");
        let mut bytes = fs::read(&path).unwrap();
        bytes.truncate(bytes.len() - 1);
        fs::write(&path, bytes).unwrap();
        db.snapshot("50");
        unknown(db.check());
    }

    #[test]
    fn cannot_tell_when_the_primary_index_has_another_version() {
        let db = Db::with_hole("primary-version");
        let path = db.immutable(0, "primary");
        let mut bytes = fs::read(&path).unwrap();
        bytes[0] = 2;
        fs::write(&path, bytes).unwrap();
        db.snapshot("50");
        unknown(db.check());
    }

    #[test]
    fn cannot_tell_when_the_indexes_disagree() {
        // The secondary index lists slot 50 and the primary index does not.
        let db = Db::with_hole("disagree");
        db.chunk(0, &[10, 20, 50]);
        let secondary = fs::read(db.immutable(0, "secondary")).unwrap();
        db.chunk(0, &[10, 20]);
        fs::write(db.immutable(0, "secondary"), secondary).unwrap();
        db.snapshot("50");
        assert!(unknown(db.check()).contains("disagree"));
        // And the other way round.
        let primary = {
            db.chunk(0, &[10, 20, 50]);
            fs::read(db.immutable(0, "primary")).unwrap()
        };
        db.chunk(0, &[10, 20]);
        fs::write(db.immutable(0, "primary"), primary).unwrap();
        assert!(unknown(db.check()).contains("disagree"));
    }

    #[test]
    fn treats_an_unfilled_boundary_slot_as_missing() {
        let db = Db::with_hole("boundary-empty");
        db.snapshot("100");
        assert_eq!(db.check(), Verdict::BlockMissing { slot: 100 });
    }

    #[test]
    fn cannot_tell_about_a_slot_an_epoch_boundary_block_may_hold() {
        let db = Db::with_hole("boundary");
        db.chunk_with(1, true, &[110, 150]);
        db.snapshot("100");
        assert!(unknown(db.check()).contains("epoch boundary block"));
    }

    #[test]
    fn finds_a_block_after_an_epoch_boundary_block() {
        let db = Db::with_hole("after-boundary");
        db.chunk_with(1, true, &[100, 150]);
        db.snapshot("100");
        assert!(starts(db.check()).contains("is in chunk 1"));
        db.snapshot("150");
        assert!(starts(db.check()).contains("is in chunk 1"));
    }

    #[test]
    fn cannot_tell_for_another_backend() {
        let db = Db::with_hole("in-memory");
        db.snapshot("50");
        let storage = NodeStorage {
            chunk_slots: CHUNK_SLOTS,
            backend: LedgerBackend::Other("V2InMemory".to_string()),
        };
        assert!(unknown(check(&db.0, &storage)).contains("V2InMemory"));
    }

    // ── Helpers ───────────────────────────────────────────────────────────

    #[test]
    fn database_path_is_the_one_the_node_is_given() {
        let cwd = Path::new("/state");
        let args = |a: &[&str]| a.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let absolute = std::env::temp_dir().join("chain");
        assert_eq!(
            database_path(
                &args(&["run", "--database-path", absolute.to_str().unwrap()]),
                cwd
            ),
            Ok(absolute)
        );
        assert_eq!(
            database_path(&args(&["run", "--database-path", "chain"]), cwd),
            Ok(cwd.join("chain"))
        );
        for odd in [
            &["run"][..],
            &["run", "--database-path"],
            &["run", "--database-path", "a", "--database-path", "b"],
            &["run", "--database-path=chain"],
            &[
                "run",
                "--immutable-database-path",
                "a",
                "--volatile-database-path",
                "b",
            ],
            &["run", "--database-path", "chain", "--validate-db"],
        ] {
            assert!(database_path(&args(odd), cwd).is_err(), "{odd:?}");
        }
    }

    #[test]
    fn read_word64_reads_only_plain_numbers() {
        assert_eq!(read_word64("136771192"), Ok(Some(136771192)));
        assert_eq!(read_word64("007"), Ok(Some(7)));
        assert_eq!(read_word64(""), Ok(None));
        assert_eq!(read_word64("lsm"), Ok(None));
        for odd in [
            "0x10",
            "0o7",
            " 5",
            "5 ",
            "(5)",
            "-1",
            "18446744073709551616",
        ] {
            assert!(read_word64(odd).is_err(), "{odd}");
        }
    }

    #[test]
    fn read_int_reads_as_bytestring_does() {
        assert_eq!(read_int(b"1234"), Ok(Some(1234)));
        assert_eq!(read_int(b"1234\n"), Ok(Some(1234)));
        assert_eq!(read_int(b"+7"), Ok(Some(7)));
        assert_eq!(read_int(b"-7"), Ok(Some(-7)));
        assert_eq!(read_int(b""), Ok(None));
        assert_eq!(read_int(b" 7"), Ok(None));
        assert_eq!(read_int(b"-"), Ok(None));
        assert!(read_int(b"99999999999999999999").is_err());
    }

    #[test]
    fn crc32_is_the_zlib_checksum() {
        assert_eq!(crc(b"123456789"), 0xCBF4_3926);
        assert_eq!(crc(b""), 0);
    }
}
