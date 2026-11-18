//! Wallet databases that an older cardano-wallet changed so that the current
//! one cannot open them, and their repair.
//!
//! cardano-wallet v2021-11-11, which Daedalus 4.6.0 runs, rebuilds the
//! `seq_state` and `shared_state` tables of every wallet database it opens
//! from its own schema, and drops the columns later versions added:
//! `seq_state.policy_xpub`, `seq_state.change_addr_mode`,
//! `shared_state.reward_account` and `shared_state.change_addr_mode`. It
//! leaves `database_schema_version` as it was, so the current wallet runs none
//! of the migrations that add them, and its read of `seq_state` fails with a
//! `PersistMarshalError`. The wallet is missing, and restoring it fails on the
//! same file.
//!
//! The check runs before every start of cardano-wallet and only reads. The
//! repair runs at the same point once the user has asked for it, so no file
//! changes while the wallet runs. It copies each file to a backup folder
//! first, and then adds back only the dropped columns, with the values the
//! current wallet's own migrations give them.

use std::fs::{self, File, OpenOptions};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use rusqlite::config::DbConfig;
use rusqlite::{Connection, OpenFlags, OptionalExtension, TransactionBehavior};
use tracing::{info, warn};

use crate::protocol::{Event, WalletDbRepairOutcome, WalletDbRepairResult, emit};

/// File name prefixes of cardano-wallet's per-wallet databases: Byron,
/// Icarus, Shelley and shared wallets.
const KINDS: [&str; 4] = ["rnd", "ica", "she", "sha"];

/// The schema version from which the current wallet expects
/// `seq_state.change_addr_mode`: its migration from 3 to 4 adds it.
const FIRST_VERSION_WITH_CHANGE_ADDR_MODE: i64 = 4;

/// The schema version of the databases cardano-wallet v2026-10-07 writes,
/// which `CURRENT_TABLES` describes. A file recorded at a newer version is left
/// alone: `CURRENT_TABLES` does not describe it, so it is neither reported as
/// damaged nor repaired.
const CURRENT_SCHEMA_VERSION: i64 = 5;

/// Free space the backup leaves on its disk beyond the copies, so that the
/// wallet and the node can keep writing after it.
const BACKUP_SPACE_MARGIN: u64 = 100 * 1024 * 1024;

/// The columns cardano-wallet v2021-11-11 drops, with the declarations the
/// current wallet's own migrations add them with.
const DROPPED_COLUMNS: [(&str, &str, &str); 4] = [
    ("seq_state", "policy_xpub", "BLOB NULL"),
    (
        "seq_state",
        "change_addr_mode",
        "VARCHAR NOT NULL DEFAULT 'increasing'",
    ),
    ("shared_state", "reward_account", "VARCHAR NULL"),
    (
        "shared_state",
        "change_addr_mode",
        "VARCHAR NOT NULL DEFAULT 'increasing'",
    ),
];

/// Every table of a wallet database that cardano-wallet v2026-10-07 creates,
/// with its columns. A file whose tables lack any other of these columns has
/// damage that the repair does not cover.
const CURRENT_TABLES: [(&str, &[&str]); 31] = [
    ("c_b_o_r", &["tx_id", "tx_cbor", "tx_era"]),
    (
        "checkpoint",
        &[
            "wallet_id",
            "slot",
            "header_hash",
            "parent_header_hash",
            "block_height",
        ],
    ),
    (
        "cosigner_key",
        &[
            "id",
            "wallet_id",
            "credential",
            "account_xpub",
            "cosigner_index",
        ],
    ),
    ("database_schema_version", &["name", "version"]),
    ("delegation_reward", &["wallet_id", "account_balance"]),
    ("delegations", &["slot", "status", "pool", "vote"]),
    ("delta_u_tx_o_slots", &["wallet_id", "finality", "tip"]),
    (
        "delta_u_tx_o_value",
        &[
            "wallet_id",
            "slot",
            "spent",
            "tx_in_tx",
            "tx_in_ix",
            "tx_out",
            "boot",
        ],
    ),
    ("private_key", &["wallet_id", "root", "hash"]),
    (
        "protocol_parameters",
        &[
            "wallet_id",
            "fee_policy",
            "tx_max_size",
            "decentralization_level",
            "desired_pool_number",
            "minimum_utxo_value",
            "hardfork_epoch",
            "key_deposit",
        ],
    ),
    (
        "rnd_state",
        &["wallet_id", "account_ix", "gen", "hd_passphrase"],
    ),
    (
        "rnd_state_address",
        &[
            "id",
            "wallet_id",
            "slot",
            "account_ix",
            "address_ix",
            "address",
            "status",
        ],
    ),
    (
        "rnd_state_pending_address",
        &["id", "wallet_id", "account_ix", "address_ix", "address"],
    ),
    (
        "seq_state",
        &[
            "wallet_id",
            "external_gap",
            "internal_gap",
            "account_xpub",
            "policy_xpub",
            "reward_xpub",
            "derivation_prefix",
            "change_addr_mode",
        ],
    ),
    (
        "seq_state_address",
        &[
            "id",
            "wallet_id",
            "slot",
            "address",
            "address_ix",
            "role",
            "status",
        ],
    ),
    ("seq_state_pending", &["wallet_id", "pending_ix"]),
    (
        "shared_state",
        &[
            "wallet_id",
            "account_xpub",
            "pool_gap",
            "payment_script",
            "delegation_script",
            "reward_account",
            "derivation_prefix",
            "change_addr_mode",
        ],
    ),
    ("shared_state_pending", &["wallet_id", "pending_ix"]),
    (
        "submissions",
        &[
            "tx_id",
            "tx",
            "expiration",
            "acceptance",
            "wallet_id",
            "status",
            "slot",
            "block_height",
            "amount",
            "direction",
            "resubmitted",
        ],
    ),
    ("submissions_slots", &["finality", "tip", "wallet_id"]),
    (
        "tx_collateral",
        &[
            "tx_id",
            "order",
            "source_tx_id",
            "source_index",
            "source_amount",
        ],
    ),
    ("tx_collateral_out", &["tx_id", "address", "amount"]),
    (
        "tx_collateral_out_token",
        &["tx_id", "token_policy_id", "token_name", "token_quantity"],
    ),
    (
        "tx_in",
        &[
            "tx_id",
            "order",
            "source_tx_id",
            "source_index",
            "source_amount",
        ],
    ),
    (
        "tx_meta",
        &[
            "tx_id",
            "wallet_id",
            "status",
            "direction",
            "slot",
            "block_height",
            "amount",
            "data",
            "slot_expires",
            "fee",
            "script_validity",
        ],
    ),
    ("tx_out", &["tx_id", "index", "address", "amount"]),
    (
        "tx_out_token",
        &[
            "tx_id",
            "tx_index",
            "token_policy_id",
            "token_name",
            "token_quantity",
        ],
    ),
    ("tx_withdrawal", &["tx_id", "amount", "account"]),
    (
        "utxo",
        &[
            "wallet_id",
            "slot",
            "input_tx_id",
            "input_index",
            "output_address",
            "output_coin",
        ],
    ),
    (
        "utxo_token",
        &[
            "id",
            "wallet_id",
            "slot",
            "tx_id",
            "tx_index",
            "token_policy_id",
            "token_name",
            "token_quantity",
        ],
    ),
    (
        "wallet",
        &[
            "wallet_id",
            "creation_time",
            "name",
            "passphrase_last_updated_at",
            "passphrase_scheme",
            "genesis_hash",
            "genesis_start",
        ],
    ),
];

/// The folder in the wallet's state directory that holds one backup folder
/// per repair.
pub const BACKUP_FOLDER: &str = "wallet-db-backups";

/// How long a statement waits for a lock. Nothing else has the files open
/// while the wallet is stopped, so a wait means another program holds them.
const BUSY_TIMEOUT: Duration = Duration::from_secs(5);

/// Set when the user asks for the repair, and taken before the next start of
/// cardano-wallet.
static REPAIR_REQUESTED: AtomicBool = AtomicBool::new(false);

/// Records that the user asked for the repair. It runs before the next start
/// of cardano-wallet; the supervisor stops a running wallet for it when the
/// command reaches it.
pub fn request_repair() {
    info!("wallet database repair requested; it runs before cardano-wallet starts again");
    REPAIR_REQUESTED.store(true, Ordering::SeqCst);
}

/// Whether a requested repair has still to run.
pub fn repair_requested() -> bool {
    REPAIR_REQUESTED.load(Ordering::SeqCst)
}

fn take_repair_request() -> bool {
    REPAIR_REQUESTED.swap(false, Ordering::SeqCst)
}

/// Runs before every start of cardano-wallet, while no wallet process runs:
/// the repair, when the user has asked for it, and then the check, whose
/// findings are reported with `wallet_db_damaged`. `args` and `state_dir`
/// are the wallet's.
pub async fn before_wallet_start(args: &[String], state_dir: &str) {
    let repair = take_repair_request();
    let Some(dir) = database_dir(args, Path::new(state_dir)) else {
        if repair {
            warn!("wallet database repair: the wallet is given no --database directory");
            emit(&Event::WalletDbRepair {
                backup_dir: None,
                results: Vec::new(),
            });
        }
        return;
    };
    let backup_root = Path::new(state_dir).join(BACKUP_FOLDER);
    let stamp = backup_stamp(time::OffsetDateTime::now_utc());
    let work = tokio::task::spawn_blocking(move || {
        let report = repair.then(|| {
            repair_all(
                &dir,
                &backup_root,
                &stamp,
                &crate::chain_validation::free_space_bytes_sync,
            )
        });
        (report, scan(&dir))
    })
    .await;
    match work {
        Ok((report, damaged)) => {
            if let Some(report) = report {
                emit(&Event::WalletDbRepair {
                    backup_dir: report.backup_dir.map(|d| d.to_string_lossy().into_owned()),
                    results: report.results,
                });
            }
            if !damaged.is_empty() {
                emit(&Event::WalletDbDamaged { files: damaged });
            }
        }
        Err(e) => warn!("the wallet database check did not finish: {e}"),
    }
}

/// The directory cardano-wallet keeps its databases in: the value of its
/// `--database` argument, relative to `state_dir`, where the watchdog starts
/// it. None when the wallet is given none, and keeps them in memory.
fn database_dir(args: &[String], state_dir: &Path) -> Option<PathBuf> {
    let mut args = args.iter();
    let mut dir = None;
    while let Some(arg) = args.next() {
        if arg == "--database" {
            dir = args.next().cloned();
        } else if let Some(value) = arg.strip_prefix("--database=") {
            dir = Some(value.to_string());
        }
    }
    let dir = PathBuf::from(dir?);
    Some(if dir.is_absolute() {
        dir
    } else {
        state_dir.join(dir)
    })
}

/// Whether `name` is a wallet database: `<kind>.<wallet id>.sqlite`, with a
/// kind cardano-wallet uses and a wallet id of 40 hex digits. Its own backups
/// (`.sqlite.v4.bak`) and the stake pool database do not match.
fn is_wallet_db_name(name: &str) -> bool {
    let mut parts = name.split('.');
    match (parts.next(), parts.next(), parts.next(), parts.next()) {
        (Some(kind), Some(id), Some("sqlite"), None) => {
            KINDS.contains(&kind) && id.len() == 40 && id.bytes().all(|b| b.is_ascii_hexdigit())
        }
        _ => false,
    }
}

/// The names of the wallet databases in `dir` that an older wallet changed,
/// sorted. A file that cannot be read, or that has the damage `classify`
/// describes at a schema version newer than `CURRENT_SCHEMA_VERSION`, is
/// logged and not reported.
fn scan(dir: &Path) -> Vec<String> {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Vec::new(),
        Err(e) => {
            warn!(
                "wallet database check: {} cannot be listed: {e}",
                dir.display()
            );
            return Vec::new();
        }
    };
    let mut names: Vec<String> = entries
        .filter_map(|e| e.ok())
        .filter_map(|e| e.file_name().into_string().ok())
        .filter(|n| is_wallet_db_name(n))
        .collect();
    names.sort();
    let mut damaged = Vec::new();
    for name in &names {
        match check_file(&dir.join(name)) {
            Ok(Finding::Damaged) => damaged.push(name.clone()),
            Ok(Finding::Newer(version)) => info!(
                "wallet database check: {name} is at schema version {version}, newer than \
                 {CURRENT_SCHEMA_VERSION}; it is left alone"
            ),
            Ok(Finding::Intact) => {}
            Err(e) => warn!("wallet database check: {name} cannot be read: {e}"),
        }
    }
    if damaged.is_empty() {
        info!(
            "wallet database check: {} wallet databases, none changed by an older wallet",
            names.len()
        );
    } else {
        warn!(
            "wallet database check: {} of {} wallet databases were changed by an older wallet and cannot be opened: {}",
            damaged.len(),
            names.len(),
            damaged.join(", ")
        );
    }
    damaged
}

/// What the check finds in the database at `path`. Opening it changes nothing
/// on disk.
fn check_file(path: &Path) -> rusqlite::Result<Finding> {
    classify(&open_read_only(path)?)
}

/// What the check finds in a database.
#[derive(Debug, PartialEq)]
enum Finding {
    /// Nothing an older wallet changed.
    Intact,
    /// The damage `classify` describes, in a file the repair knows.
    Damaged,
    /// The same damage, in a file recorded at this schema version, newer than
    /// `CURRENT_SCHEMA_VERSION`, so not described by `CURRENT_TABLES`.
    Newer(i64),
}

/// The damage: the schema version is one at which the current wallet expects
/// `seq_state.change_addr_mode`, the column is missing, and the table holds a
/// row, which the current wallet reads to open the wallet. A Byron wallet
/// keeps no row there and does not read the table, so it opens. A shared
/// wallet reads `shared_state`, which an older wallet changes the same way,
/// but keeps no row in `seq_state`, so this check does not report it;
/// Daedalus does not create shared wallets. A file with this damage recorded at
/// a version above `CURRENT_SCHEMA_VERSION` is `Finding::Newer`, not damaged.
fn classify(conn: &Connection) -> rusqlite::Result<Finding> {
    if !table_exists(conn, "seq_state")? {
        return Ok(Finding::Intact);
    }
    let Some(version) = schema_version(conn)?.filter(|v| *v >= FIRST_VERSION_WITH_CHANGE_ADDR_MODE)
    else {
        return Ok(Finding::Intact);
    };
    if columns(conn, "seq_state")?
        .iter()
        .any(|c| c.eq_ignore_ascii_case("change_addr_mode"))
    {
        return Ok(Finding::Intact);
    }
    let rows: i64 = conn.query_row("SELECT count(*) FROM seq_state", [], |r| r.get(0))?;
    Ok(if rows == 0 {
        Finding::Intact
    } else if version > CURRENT_SCHEMA_VERSION {
        Finding::Newer(version)
    } else {
        Finding::Damaged
    })
}

/// Whether `classify` finds damage the repair can address.
fn is_damaged(conn: &Connection) -> rusqlite::Result<bool> {
    Ok(classify(conn)? == Finding::Damaged)
}

/// The file's `database_schema_version`, None when it records none.
fn schema_version(conn: &Connection) -> rusqlite::Result<Option<i64>> {
    if !table_exists(conn, "database_schema_version")? {
        return Ok(None);
    }
    conn.query_row(
        "SELECT version FROM database_schema_version WHERE name = 'schema'",
        [],
        |r| r.get(0),
    )
    .optional()
}

fn table_exists(conn: &Connection, table: &str) -> rusqlite::Result<bool> {
    conn.query_row(
        "SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?1",
        [table],
        |r| r.get::<_, i64>(0),
    )
    .map(|n| n > 0)
}

fn columns(conn: &Connection, table: &str) -> rusqlite::Result<Vec<String>> {
    let mut stmt = conn.prepare("SELECT name FROM pragma_table_info(?1)")?;
    let names = stmt.query_map([table], |r| r.get(0))?;
    names.collect()
}

/// The columns of `CURRENT_TABLES` that the file's tables lack, as
/// (table, column). Tables the file does not have are not compared: an
/// upgraded database lacks some that a new one has. SQLite compares names
/// without regard to case, and so does this.
fn missing_columns(conn: &Connection) -> rusqlite::Result<Vec<(String, String)>> {
    let mut missing = Vec::new();
    for (table, wanted) in CURRENT_TABLES {
        if !table_exists(conn, table)? {
            continue;
        }
        let have = columns(conn, table)?;
        for column in wanted {
            if !have.iter().any(|c| c.eq_ignore_ascii_case(column)) {
                missing.push((table.to_string(), column.to_string()));
            }
        }
    }
    Ok(missing)
}

fn quick_check(conn: &Connection) -> rusqlite::Result<String> {
    let mut stmt = conn.prepare("PRAGMA quick_check")?;
    let lines = stmt.query_map([], |r| r.get::<_, String>(0))?;
    Ok(lines.collect::<rusqlite::Result<Vec<_>>>()?.join("; "))
}

/// `path` with `suffix` appended to its file name, as SQLite names a
/// database's `-wal` and `-shm` files.
fn sidecar(path: &Path, suffix: &str) -> PathBuf {
    let mut name = path.as_os_str().to_owned();
    name.push(suffix);
    PathBuf::from(name)
}

/// Opens the database at `path` for reading without changing anything on
/// disk. A read-only connection to a database in WAL mode creates `-wal` and
/// `-shm` files when they are missing, and leaves them. Without a `-wal`
/// file the whole database is in the main file, so it is opened immutable,
/// which reads only that file; with one, the `-wal` file holds changes the
/// check must see.
fn open_read_only(path: &Path) -> rusqlite::Result<Connection> {
    let immutable = !sidecar(path, "-wal").exists();
    let conn = Connection::open_with_flags(
        read_only_uri(path, immutable),
        OpenFlags::SQLITE_OPEN_READ_ONLY
            | OpenFlags::SQLITE_OPEN_URI
            | OpenFlags::SQLITE_OPEN_NO_MUTEX,
    )?;
    conn.busy_timeout(BUSY_TIMEOUT)?;
    Ok(conn)
}

/// A `file:` URI for `path` that opens it read-only, and immutable when
/// `immutable`. The path is made absolute, and every byte outside letters,
/// digits and `-._~/:` is percent-encoded. On Windows the separators become
/// `/` and a drive path gets the empty authority, as in `file:///C:/...`.
fn read_only_uri(path: &Path, immutable: bool) -> String {
    let path = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    let path = path.to_string_lossy();
    #[cfg(windows)]
    let path = path.replace('\\', "/");
    let mut uri = String::from(if path.starts_with('/') {
        "file://"
    } else {
        "file:///"
    });
    for b in path.bytes() {
        if b.is_ascii_alphanumeric() || b"-._~/:".contains(&b) {
            uri.push(char::from(b));
        } else {
            uri.push_str(&format!("%{b:02X}"));
        }
    }
    uri.push_str("?mode=ro");
    if immutable {
        uri.push_str("&immutable=1");
    }
    uri
}

/// What a repair did, for `wallet_db_repair`.
#[derive(Debug)]
struct RepairReport {
    /// The folder the files were copied to; None when no file needed a copy.
    backup_dir: Option<PathBuf>,
    results: Vec<WalletDbRepairResult>,
}

/// The name of a repair's backup folder, from the time it started in UTC.
fn backup_stamp(now: time::OffsetDateTime) -> String {
    format!(
        "{:04}{:02}{:02}T{:02}{:02}{:02}Z",
        now.year(),
        u8::from(now.month()),
        now.day(),
        now.hour(),
        now.minute(),
        now.second()
    )
}

/// Creates the backup folder `<root>/<stamp>`, with a numeric suffix if a
/// folder of that name exists.
fn create_backup_dir(root: &Path, stamp: &str) -> io::Result<PathBuf> {
    fs::create_dir_all(root)?;
    let mut n = 0;
    loop {
        let dir = if n == 0 {
            root.join(stamp)
        } else {
            root.join(format!("{stamp}-{n}"))
        };
        match fs::create_dir(&dir) {
            Ok(()) => {
                sync_dir(root)?;
                return Ok(dir);
            }
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && n < 100 => n += 1,
            Err(e) => return Err(e),
        }
    }
}

/// The free space in bytes on the disk that holds `path`.
type FreeSpace<'a> = dyn Fn(&Path) -> io::Result<u64> + 'a;

/// Repairs every damaged wallet database in `db_dir`. It must run while
/// cardano-wallet is stopped. Each file is first checked, without being
/// opened for writing, to be in exactly the state an older wallet leaves,
/// and is refused otherwise. A file that passes is copied, with its `-wal`
/// and `-shm` files, to the folder `<backup_root>/<stamp>` once `free_space`
/// shows room for the copies, and the copies are synced and compared with the
/// originals before the file is opened for writing. A file that is refused,
/// or whose backup or repair fails, is left as it was; a refused file is not
/// copied. The backups are kept.
fn repair_all(
    db_dir: &Path,
    backup_root: &Path,
    stamp: &str,
    free_space: &FreeSpace<'_>,
) -> RepairReport {
    let damaged = scan(db_dir);
    if damaged.is_empty() {
        info!("wallet database repair: no wallet database needs it");
        return RepairReport {
            backup_dir: None,
            results: Vec::new(),
        };
    }
    info!("wallet database repair: {} files to repair", damaged.len());
    let mut backup = Backup {
        root: backup_root,
        stamp,
        dir: None,
        copied: false,
    };
    let results = damaged
        .into_iter()
        .map(|file| {
            let (outcome, detail) = repair_one(&db_dir.join(&file), &mut backup, free_space);
            if outcome == WalletDbRepairOutcome::Repaired {
                info!("wallet database repair: {file} repaired: {detail}");
            } else {
                warn!(
                    "wallet database repair: {file} {}, left unchanged: {detail}",
                    outcome.as_str()
                );
            }
            WalletDbRepairResult {
                file,
                outcome,
                detail,
            }
        })
        .collect();
    RepairReport {
        backup_dir: backup.finish(),
        results,
    }
}

/// The backup folder of one repair, created for the first file copied.
struct Backup<'a> {
    root: &'a Path,
    stamp: &'a str,
    /// The folder, or why it could not be created.
    dir: Option<Result<PathBuf, String>>,
    /// Whether a file was copied into it.
    copied: bool,
}

impl Backup<'_> {
    fn dir(&mut self) -> Result<PathBuf, String> {
        let (root, stamp) = (self.root, self.stamp);
        self.dir
            .get_or_insert_with(|| {
                create_backup_dir(root, stamp).map_err(|e| {
                    format!(
                        "the backup folder could not be created in {}: {e}",
                        root.display()
                    )
                })
            })
            .clone()
    }

    /// The folder that holds the copies, if any. A folder left empty, as when
    /// every copy into it failed, is removed.
    fn finish(self) -> Option<PathBuf> {
        let dir = self.dir?.ok()?;
        if self.copied {
            return Some(dir);
        }
        let _ = fs::remove_dir(&dir);
        None
    }
}

fn repair_one(
    path: &Path,
    backup: &mut Backup<'_>,
    free_space: &FreeSpace<'_>,
) -> (WalletDbRepairOutcome, String) {
    let missing = match check_repairable(path) {
        Ok(missing) => missing,
        Err(NotRepaired::Refused(reason)) => return (WalletDbRepairOutcome::Refused, reason),
        Err(NotRepaired::Failed(reason)) => return (WalletDbRepairOutcome::RepairFailed, reason),
    };
    if let Err(reason) = check_space(path, backup.root, free_space) {
        return (WalletDbRepairOutcome::BackupFailed, reason);
    }
    let backup_dir = match backup.dir() {
        Ok(dir) => dir,
        Err(reason) => return (WalletDbRepairOutcome::BackupFailed, reason),
    };
    match back_up(path, &backup_dir) {
        Ok(copied) => {
            backup.copied = true;
            info!(
                "wallet database repair: backed up and verified in {}: {}",
                backup_dir.display(),
                copied.join(", ")
            );
        }
        Err(e) => {
            return (
                WalletDbRepairOutcome::BackupFailed,
                format!("the backup failed: {e}"),
            );
        }
    }
    match write_columns(path, &missing) {
        Ok(added) => (
            WalletDbRepairOutcome::Repaired,
            format!("added {}", added.join(", ")),
        ),
        Err(NotRepaired::Refused(reason)) => (WalletDbRepairOutcome::Refused, reason),
        Err(NotRepaired::Failed(reason)) => (WalletDbRepairOutcome::RepairFailed, reason),
    }
}

/// The bytes the backup of the database at `path` takes: the file and its
/// `-wal` and `-shm` files, those that exist.
fn backup_size(path: &Path) -> io::Result<u64> {
    let mut total = 0;
    for suffix in ["", "-wal", "-shm"] {
        match fs::metadata(sidecar(path, suffix)) {
            Ok(meta) => total += meta.len(),
            Err(e) if !suffix.is_empty() && e.kind() == io::ErrorKind::NotFound => {}
            Err(e) => return Err(e),
        }
    }
    Ok(total)
}

/// `path`, or its nearest ancestor that exists.
fn existing_ancestor(path: &Path) -> &Path {
    path.ancestors().find(|p| p.exists()).unwrap_or(path)
}

/// Whether the disk that holds `backup_root` has room for the backup of the
/// database at `path` and `BACKUP_SPACE_MARGIN` besides. When the free space
/// cannot be read the copy goes ahead: a copy that fails is removed, and the
/// file is not changed.
fn check_space(path: &Path, backup_root: &Path, free_space: &FreeSpace<'_>) -> Result<(), String> {
    let size = backup_size(path).map_err(|e| format!("the files to copy cannot be read: {e}"))?;
    let needed = size.saturating_add(BACKUP_SPACE_MARGIN);
    let disk = existing_ancestor(backup_root);
    match free_space(disk) {
        Ok(free) if free >= needed => Ok(()),
        Ok(free) => Err(format!(
            "not enough free space for the backup in {}: {free} bytes free, {needed} needed",
            disk.display()
        )),
        Err(e) => {
            warn!(
                "wallet database repair: the free space in {} cannot be read ({e}); copying anyway",
                disk.display()
            );
            Ok(())
        }
    }
}

/// Copies the database at `path` and its `-wal` and `-shm` files, those that
/// exist, into `backup_dir`. Each copy is synced to disk and compared with
/// its original. Returns the names of the files copied, with their sizes.
/// When a copy fails, the copies of this database already made are removed.
fn back_up(path: &Path, backup_dir: &Path) -> io::Result<Vec<String>> {
    let result = copy_all(path, backup_dir);
    if result.is_err() {
        for suffix in ["", "-wal", "-shm"] {
            if let Some(name) = sidecar(path, suffix).file_name() {
                let _ = fs::remove_file(backup_dir.join(name));
            }
        }
    }
    result
}

fn copy_all(path: &Path, backup_dir: &Path) -> io::Result<Vec<String>> {
    let mut copied = Vec::new();
    for suffix in ["", "-wal", "-shm"] {
        let src = sidecar(path, suffix);
        if !suffix.is_empty() && !src.exists() {
            continue;
        }
        let name = src
            .file_name()
            .ok_or_else(|| io::Error::other("the file has no name"))?;
        let dst = backup_dir.join(name);
        let len = copy_verified(&src, &dst)?;
        copied.push(format!("{} {len} bytes", name.to_string_lossy()));
    }
    sync_dir(backup_dir)?;
    Ok(copied)
}

fn copy_verified(src: &Path, dst: &Path) -> io::Result<u64> {
    let len = fs::copy(src, dst)?;
    // Windows flushes a file only through a handle that can write to it.
    OpenOptions::new().write(true).open(dst)?.sync_all()?;
    if !same_contents(src, dst)? {
        return Err(io::Error::other(format!(
            "the copy {} differs from its original",
            dst.display()
        )));
    }
    Ok(len)
}

fn same_contents(a: &Path, b: &Path) -> io::Result<bool> {
    let (mut fa, mut fb) = (File::open(a)?, File::open(b)?);
    if fa.metadata()?.len() != fb.metadata()?.len() {
        return Ok(false);
    }
    let (mut ba, mut bb) = (vec![0u8; 64 * 1024], vec![0u8; 64 * 1024]);
    loop {
        let n = read_full(&mut fa, &mut ba)?;
        let m = read_full(&mut fb, &mut bb)?;
        if ba[..n] != bb[..m] {
            return Ok(false);
        }
        if n == 0 {
            return Ok(true);
        }
    }
}

/// Reads until `buf` is full or the file ends; returns the bytes read.
fn read_full(file: &mut File, buf: &mut [u8]) -> io::Result<usize> {
    let mut filled = 0;
    while filled < buf.len() {
        match file.read(&mut buf[filled..])? {
            0 => break,
            n => filled += n,
        }
    }
    Ok(filled)
}

/// Syncs a directory, so that the names created in it survive a crash.
/// Windows syncs directory entries with the files themselves.
fn sync_dir(dir: &Path) -> io::Result<()> {
    #[cfg(unix)]
    File::open(dir)?.sync_all()?;
    #[cfg(not(unix))]
    let _ = dir;
    Ok(())
}

#[derive(Debug)]
enum NotRepaired {
    /// The file is not in the state the repair is for.
    Refused(String),
    Failed(String),
}

impl From<rusqlite::Error> for NotRepaired {
    fn from(e: rusqlite::Error) -> Self {
        NotRepaired::Failed(e.to_string())
    }
}

/// Checks, without opening it for writing, that the database at `path` is in
/// exactly the state an older wallet leaves: the damage `is_damaged` finds, no
/// column missing beyond `DROPPED_COLUMNS`, and a clean `quick_check`. Returns
/// the columns to add, as (table, column); a file in any other state is refused.
fn check_repairable(path: &Path) -> Result<Vec<(String, String)>, NotRepaired> {
    let conn = open_read_only(path)?;
    if !is_damaged(&conn)? {
        return Err(NotRepaired::Refused(
            "the file is not in the state an older wallet leaves".to_string(),
        ));
    }
    let missing = missing_columns(&conn)?;
    let other: Vec<String> = missing
        .iter()
        .filter(|(t, c)| !is_dropped(t, c))
        .map(|(t, c)| format!("{t}.{c}"))
        .collect();
    if !other.is_empty() {
        return Err(NotRepaired::Refused(format!(
            "columns are missing that an older wallet does not drop: {}",
            other.join(", ")
        )));
    }
    let check = quick_check(&conn)?;
    if check != "ok" {
        return Err(NotRepaired::Refused(format!(
            "the database integrity check failed: {check}"
        )));
    }
    Ok(missing)
}

/// Opens the database at `path` for writing and adds the `missing` columns of
/// `DROPPED_COLUMNS` in one transaction. On any failure the transaction is
/// rolled back, and the database and its `-wal` file are left as they were.
fn write_columns(path: &Path, missing: &[(String, String)]) -> Result<Vec<String>, NotRepaired> {
    let mut conn = Connection::open_with_flags(
        path,
        OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_NO_MUTEX,
    )?;
    conn.busy_timeout(BUSY_TIMEOUT)?;
    let result = add_columns(&mut conn, missing);
    if result.is_err() {
        // Without this, closing the last connection copies the `-wal` file
        // into the database and deletes it.
        let _ = conn.set_db_config(DbConfig::SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE, true);
    }
    result
}

fn is_dropped(table: &str, column: &str) -> bool {
    DROPPED_COLUMNS
        .iter()
        .any(|(t, c, _)| *t == table && *c == column)
}

fn add_columns(
    conn: &mut Connection,
    missing: &[(String, String)],
) -> Result<Vec<String>, NotRepaired> {
    let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
    // Read again under the write lock: the file must be as the checks before
    // the backup found it.
    if missing_columns(&tx)? != missing || !is_damaged(&tx)? {
        return Err(NotRepaired::Failed(
            "the file changed while it was being repaired".to_string(),
        ));
    }
    let mut added = Vec::new();
    for (table, column, declaration) in DROPPED_COLUMNS {
        if missing.iter().any(|(t, c)| t == table && c == column) {
            tx.execute_batch(&format!(
                "ALTER TABLE \"{table}\" ADD COLUMN \"{column}\" {declaration}"
            ))?;
            added.push(format!("{table}.{column}"));
        }
    }
    if is_damaged(&tx)? || !missing_columns(&tx)?.is_empty() {
        return Err(NotRepaired::Failed(
            "the columns are still missing after they were added".to_string(),
        ));
    }
    let check = quick_check(&tx)?;
    if check != "ok" {
        return Err(NotRepaired::Failed(format!(
            "the database integrity check failed after the change: {check}"
        )));
    }
    tx.commit()?;
    Ok(added)
}

#[cfg(test)]
mod tests {
    use super::*;

    const FRESH: &str = include_str!("../tests/fixtures/wallet_db/fresh_v2026_10_07.sql");
    const AFTER_V2021: &str = include_str!("../tests/fixtures/wallet_db/after_v2021_11_11.sql");
    const WALLET_ID: &str = "1178ad68993ea595a93adc6fac3ec554b0ab7a33";
    const OTHER_ID: &str = "2c1d0b3a4f5e6d7c8b9a0f1e2d3c4b5a69788796";

    struct TempDir(PathBuf);

    impl TempDir {
        fn new(label: &str) -> Self {
            let path = std::env::temp_dir().join(format!(
                "watchdog-wallet-db-{}-{}-{}",
                label,
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            fs::create_dir_all(&path).unwrap();
            TempDir(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn db_name(id: &str) -> String {
        format!("she.{id}.sqlite")
    }

    /// A disk with room for any backup.
    fn plenty(_: &Path) -> io::Result<u64> {
        Ok(u64::MAX)
    }

    /// A wallet database as cardano-wallet v2026-10-07 creates it, in WAL
    /// mode, at schema version `version`, with its wallet row and, when
    /// `seq_row`, its `seq_state` row.
    fn create_fresh(path: &Path, version: i64, seq_row: bool) {
        let conn = Connection::open(path).unwrap();
        conn.query_row("PRAGMA journal_mode = WAL", [], |_| Ok(()))
            .unwrap();
        conn.execute_batch(FRESH).unwrap();
        conn.execute(
            "INSERT INTO database_schema_version (name, version) VALUES ('schema', ?1)",
            [version],
        )
        .unwrap();
        conn.execute(
            "INSERT INTO wallet VALUES (?1, '2026-11-15 09:30:00', 'repro', NULL, NULL, \
             'd4b8de7a11d929a323373cbab6c1a9bdc931beffff11db111cf9d57356ee1937', '2022-06-01 00:00:00')",
            [WALLET_ID],
        )
        .unwrap();
        if seq_row {
            conn.execute(
                "INSERT INTO seq_state VALUES (?1, 20, 20, ?2, ?3, ?4, \
                 '2147485500/2147485463/2147483648', 'increasing')",
                rusqlite::params![WALLET_ID, vec![1u8; 64], vec![2u8; 64], vec![3u8; 64]],
            )
            .unwrap();
        }
    }

    /// What cardano-wallet v2021-11-11 does to `table`, as its debug trace
    /// shows: copy the columns it knows to a temporary table, drop the table,
    /// create it from its own schema and copy the rows back.
    fn rebuild_like_v2021(conn: &Connection, table: &str, kept: &[&str]) {
        let create = AFTER_V2021
            .lines()
            .find(|l| l.starts_with(&format!("CREATE TABLE \"{table}\"(")))
            .unwrap();
        let cols = kept
            .iter()
            .map(|c| format!("\"{c}\""))
            .collect::<Vec<_>>()
            .join(",");
        conn.execute_batch(&format!(
            "BEGIN;
             CREATE TEMP TABLE \"{table}_backup\" AS SELECT {cols} FROM \"{table}\";
             DROP TABLE \"{table}\";
             {create}
             INSERT INTO \"{table}\" SELECT {cols} FROM \"{table}_backup\";
             DROP TABLE \"{table}_backup\";
             COMMIT;"
        ))
        .unwrap();
    }

    fn damage_like_v2021(conn: &Connection) {
        rebuild_like_v2021(
            conn,
            "seq_state",
            &[
                "wallet_id",
                "external_gap",
                "internal_gap",
                "account_xpub",
                "reward_xpub",
                "derivation_prefix",
            ],
        );
        rebuild_like_v2021(
            conn,
            "shared_state",
            &[
                "wallet_id",
                "account_xpub",
                "pool_gap",
                "payment_script",
                "delegation_script",
                "derivation_prefix",
            ],
        );
    }

    /// A database that cardano-wallet v2021-11-11 has opened after a newer
    /// wallet created it.
    fn create_damaged(path: &Path, version: i64, seq_row: bool) {
        create_fresh(path, version, seq_row);
        damage_like_v2021(&Connection::open(path).unwrap());
    }

    /// The bytes of the database at `path` and of its `-wal` file, None for
    /// a file that does not exist. The `-shm` file is left out: it is
    /// SQLite's index of the `-wal` file, which any reader may rebuild.
    fn snapshot(path: &Path) -> Vec<Option<Vec<u8>>> {
        ["", "-wal"]
            .iter()
            .map(|s| fs::read(sidecar(path, s)).ok())
            .collect()
    }

    fn listing(dir: &Path) -> Vec<String> {
        let mut names: Vec<String> = fs::read_dir(dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect();
        names.sort();
        names
    }

    #[test]
    fn database_dir_is_read_from_the_wallet_arguments() {
        let state = Path::new("/state");
        let args = |a: &[&str]| a.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        assert_eq!(
            database_dir(&args(&["serve", "--database", "wallets"]), state),
            Some(state.join("wallets"))
        );
        let absolute = std::env::temp_dir().join("wallets");
        assert_eq!(
            database_dir(
                &args(&["serve", &format!("--database={}", absolute.display())]),
                state
            ),
            Some(absolute)
        );
        assert_eq!(
            database_dir(&args(&["serve", "--node-socket", "x"]), state),
            None
        );
    }

    #[test]
    fn only_wallet_database_names_are_checked() {
        for kind in KINDS {
            assert!(is_wallet_db_name(&format!("{kind}.{WALLET_ID}.sqlite")));
        }
        for name in [
            format!("she.{WALLET_ID}.sqlite-wal"),
            format!("she.{WALLET_ID}.sqlite.v4.bak"),
            format!("abc.{WALLET_ID}.sqlite"),
            format!("she.{}.sqlite", &WALLET_ID[..39]),
            format!("she.{}g.sqlite", &WALLET_ID[..39]),
            "stake-pools.sqlite".to_string(),
        ] {
            assert!(!is_wallet_db_name(&name), "{name}");
        }
    }

    #[test]
    fn a_database_the_current_wallet_created_is_not_flagged_or_touched() {
        let dir = TempDir::new("fresh");
        let path = dir.path().join(db_name(WALLET_ID));
        create_fresh(&path, 5, true);
        let before = (listing(dir.path()), snapshot(&path));
        assert!(scan(dir.path()).is_empty());
        assert_eq!((listing(dir.path()), snapshot(&path)), before);
    }

    #[test]
    fn a_database_an_older_wallet_rebuilt_is_flagged_and_not_touched() {
        let dir = TempDir::new("damaged");
        let path = dir.path().join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);
        let before = (listing(dir.path()), snapshot(&path));
        assert_eq!(scan(dir.path()), vec![db_name(WALLET_ID)]);
        assert_eq!((listing(dir.path()), snapshot(&path)), before);
    }

    #[test]
    fn a_database_without_a_seq_state_row_is_not_flagged() {
        // A Byron wallet keeps no row in seq_state and does not read it. A
        // shared wallet keeps none either, so it is not reported, though it
        // reads shared_state.
        let dir = TempDir::new("no-row");
        create_damaged(&dir.path().join(db_name(WALLET_ID)), 5, false);
        assert!(scan(dir.path()).is_empty());
    }

    #[test]
    fn a_database_at_schema_version_3_is_not_flagged() {
        // The current wallet adds change_addr_mode itself when it migrates
        // version 3 to 4.
        let dir = TempDir::new("v3");
        create_damaged(&dir.path().join(db_name(WALLET_ID)), 3, true);
        assert!(scan(dir.path()).is_empty());
    }

    #[test]
    fn a_file_that_is_not_a_database_is_not_flagged() {
        let dir = TempDir::new("garbage");
        let path = dir.path().join(db_name(WALLET_ID));
        fs::write(&path, b"not a database").unwrap();
        assert!(scan(dir.path()).is_empty());
        assert_eq!(listing(dir.path()), vec![db_name(WALLET_ID)]);
    }

    #[test]
    fn a_path_with_spaces_and_uri_characters_is_read() {
        let dir = TempDir::new("Daedalus Mainnet #1 %20 & = \u{fc}");
        create_damaged(&dir.path().join(db_name(WALLET_ID)), 5, true);
        assert_eq!(scan(dir.path()), vec![db_name(WALLET_ID)]);
    }

    #[test]
    fn repair_adds_the_dropped_columns_and_keeps_the_rows() {
        let dir = TempDir::new("repair");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);

        let report = repair_all(
            &db,
            &dir.path().join(BACKUP_FOLDER),
            "20261115T093000Z",
            &plenty,
        );

        assert_eq!(
            report.backup_dir,
            Some(dir.path().join(BACKUP_FOLDER).join("20261115T093000Z"))
        );
        assert_eq!(
            report.results,
            vec![WalletDbRepairResult {
                file: db_name(WALLET_ID),
                outcome: WalletDbRepairOutcome::Repaired,
                detail: "added seq_state.policy_xpub, seq_state.change_addr_mode, \
                         shared_state.reward_account, shared_state.change_addr_mode"
                    .to_string(),
            }]
        );
        assert!(scan(&db).is_empty());
        let conn = Connection::open(&path).unwrap();
        assert!(missing_columns(&conn).unwrap().is_empty());
        let kept = conn
            .query_row(
                "SELECT wallet_id = ?1 AND external_gap = 20 AND account_xpub = ?2 \
                 AND reward_xpub = ?3 AND derivation_prefix = '2147485500/2147485463/2147483648' \
                 FROM seq_state",
                rusqlite::params![WALLET_ID, vec![1u8; 64], vec![3u8; 64]],
                |r| r.get::<_, bool>(0),
            )
            .unwrap();
        assert!(kept);
        let added: (Option<Vec<u8>>, String) = conn
            .query_row(
                "SELECT policy_xpub, change_addr_mode FROM seq_state",
                [],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .unwrap();
        assert_eq!(added, (None, "increasing".to_string()));
        assert_eq!(quick_check(&conn).unwrap(), "ok");
    }

    #[test]
    fn the_backup_holds_the_file_as_it_was_before_the_repair() {
        let dir = TempDir::new("backup");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);
        let before = snapshot(&path);

        let report = repair_all(&db, &dir.path().join(BACKUP_FOLDER), "stamp", &plenty);

        assert_eq!(report.results[0].outcome, WalletDbRepairOutcome::Repaired);
        let backup_dir = report.backup_dir.unwrap();
        assert_eq!(listing(&backup_dir), vec![db_name(WALLET_ID)]);
        assert_eq!(snapshot(&backup_dir.join(db_name(WALLET_ID))), before);
        assert_ne!(snapshot(&path)[0], before[0]);
    }

    #[test]
    fn damage_held_in_the_wal_is_found_backed_up_and_repaired() {
        // An older wallet that did not stop cleanly leaves its change in the
        // -wal file: the main file alone still has the columns.
        let dir = TempDir::new("wal");
        let live = dir.path().join("live");
        let db = dir.path().join("wallets");
        fs::create_dir(&live).unwrap();
        fs::create_dir(&db).unwrap();
        let live_path = live.join(db_name(WALLET_ID));
        create_fresh(&live_path, 5, true);
        let conn = Connection::open(&live_path).unwrap();
        conn.query_row("PRAGMA wal_autocheckpoint = 0", [], |_| Ok(()))
            .unwrap();
        damage_like_v2021(&conn);
        let path = db.join(db_name(WALLET_ID));
        for suffix in ["", "-wal", "-shm"] {
            fs::copy(sidecar(&live_path, suffix), sidecar(&path, suffix)).unwrap();
        }
        drop(conn);
        assert!(
            !is_damaged(
                &Connection::open_with_flags(
                    read_only_uri(&path, true),
                    OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_URI
                )
                .unwrap()
            )
            .unwrap()
        );
        let before = snapshot(&path);

        assert_eq!(scan(&db), vec![db_name(WALLET_ID)]);
        let report = repair_all(&db, &dir.path().join(BACKUP_FOLDER), "stamp", &plenty);

        assert_eq!(report.results[0].outcome, WalletDbRepairOutcome::Repaired);
        let backup_dir = report.backup_dir.unwrap();
        let backup = backup_dir.join(db_name(WALLET_ID));
        assert_eq!(snapshot(&backup), before);
        assert!(sidecar(&backup, "-shm").exists());
        assert!(scan(&db).is_empty());
    }

    #[test]
    fn a_file_with_other_columns_missing_is_refused_and_left_as_it_was() {
        let dir = TempDir::new("refused");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);
        Connection::open(&path)
            .unwrap()
            .execute_batch("ALTER TABLE tx_meta DROP COLUMN script_validity")
            .unwrap();
        // As after an unclean stop, so that closing a connection that wrote
        // would change the files.
        let live = Connection::open(&path).unwrap();
        live.query_row("PRAGMA wal_autocheckpoint = 0", [], |_| Ok(()))
            .unwrap();
        live.execute_batch("UPDATE wallet SET name = 'renamed'")
            .unwrap();
        let copy = db.join(db_name(OTHER_ID));
        for suffix in ["", "-wal", "-shm"] {
            fs::copy(sidecar(&path, suffix), sidecar(&copy, suffix)).unwrap();
        }
        drop(live);
        fs::remove_file(&path).unwrap();
        let before = snapshot(&copy);

        let report = repair_all(&db, &dir.path().join(BACKUP_FOLDER), "stamp", &plenty);

        assert_eq!(
            report.results,
            vec![WalletDbRepairResult {
                file: db_name(OTHER_ID),
                outcome: WalletDbRepairOutcome::Refused,
                detail: "columns are missing that an older wallet does not drop: \
                         tx_meta.script_validity"
                    .to_string(),
            }]
        );
        assert_eq!(snapshot(&copy), before);
        // A refused file is not copied.
        assert_eq!(report.backup_dir, None);
        assert!(!dir.path().join(BACKUP_FOLDER).exists());
    }

    #[test]
    fn a_database_newer_than_the_repair_knows_is_not_flagged_or_touched() {
        let dir = TempDir::new("newer");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 6, true);
        let before = snapshot(&path);
        assert_eq!(check_file(&path).unwrap(), Finding::Newer(6));
        assert!(scan(&db).is_empty());
        assert!(matches!(
            check_repairable(&path),
            Err(NotRepaired::Refused(_))
        ));

        let report = repair_all(&db, &dir.path().join(BACKUP_FOLDER), "stamp", &plenty);

        assert!(report.results.is_empty());
        assert_eq!(report.backup_dir, None);
        assert!(!dir.path().join(BACKUP_FOLDER).exists());
        assert_eq!(snapshot(&path), before);
    }

    #[test]
    fn a_file_raised_above_the_known_version_before_the_repair_is_left_as_it_was() {
        let dir = TempDir::new("raised");
        let live_path = dir.path().join(db_name(OTHER_ID));
        create_damaged(&live_path, 5, true);
        // What the checks before the backup found, at version 5.
        let missing = check_repairable(&live_path).unwrap();
        // The wallet then migrates the file. As after an unclean stop, so that
        // closing a connection that wrote would change the files.
        let live = Connection::open(&live_path).unwrap();
        live.query_row("PRAGMA wal_autocheckpoint = 0", [], |_| Ok(()))
            .unwrap();
        let path = dir.path().join(db_name(WALLET_ID));
        live.execute("UPDATE database_schema_version SET version = 6", [])
            .unwrap();
        for suffix in ["", "-wal", "-shm"] {
            fs::copy(sidecar(&live_path, suffix), sidecar(&path, suffix)).unwrap();
        }
        drop(live);
        assert_eq!(
            schema_version(&open_read_only(&path).unwrap()).unwrap(),
            Some(6)
        );
        let before = snapshot(&path);

        let result = write_columns(&path, &missing);

        assert!(matches!(
            result,
            Err(NotRepaired::Failed(ref reason)) if reason == "the file changed while it was being repaired"
        ));
        assert_eq!(snapshot(&path), before);
        let conn = open_read_only(&path).unwrap();
        assert_eq!(missing_columns(&conn).unwrap(), missing);
    }

    #[test]
    fn a_disk_without_room_for_the_backup_and_the_margin_gets_no_copy() {
        let dir = TempDir::new("space");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);
        let before = snapshot(&path);
        let needed = backup_size(&path).unwrap() + BACKUP_SPACE_MARGIN;
        let backup_root = dir.path().join(BACKUP_FOLDER);
        let asked = std::cell::RefCell::new(Vec::new());
        let short = |p: &Path| {
            asked.borrow_mut().push(p.to_path_buf());
            Ok(needed - 1)
        };

        let report = repair_all(&db, &backup_root, "stamp", &short);

        assert_eq!(report.results.len(), 1);
        assert_eq!(
            report.results[0].outcome,
            WalletDbRepairOutcome::BackupFailed
        );
        assert!(
            report.results[0]
                .detail
                .starts_with("not enough free space for the backup"),
            "{}",
            report.results[0].detail
        );
        assert_eq!(report.backup_dir, None);
        assert!(!backup_root.exists());
        assert_eq!(snapshot(&path), before);
        // Asked of the disk the backup folder would be created on.
        assert_eq!(asked.borrow().as_slice(), [dir.path().to_path_buf()]);

        let exact = |_: &Path| Ok(needed);
        let report = repair_all(&db, &backup_root, "stamp", &exact);
        assert_eq!(report.results[0].outcome, WalletDbRepairOutcome::Repaired);
    }

    #[test]
    fn a_failed_copy_leaves_no_part_of_the_backup() {
        let dir = TempDir::new("partial");
        let path = dir.path().join(db_name(WALLET_ID));
        fs::write(&path, b"main").unwrap();
        fs::write(sidecar(&path, "-wal"), b"wal").unwrap();
        let backup_dir = dir.path().join("backup");
        fs::create_dir(&backup_dir).unwrap();
        // The -wal copy cannot be written where a folder of its name is.
        fs::create_dir(backup_dir.join(format!("{}-wal", db_name(WALLET_ID)))).unwrap();

        assert!(back_up(&path, &backup_dir).is_err());

        assert_eq!(
            listing(&backup_dir),
            vec![format!("{}-wal", db_name(WALLET_ID))]
        );
        assert!(
            backup_dir
                .join(format!("{}-wal", db_name(WALLET_ID)))
                .is_dir()
        );
    }

    #[test]
    fn a_repair_that_fails_after_the_file_is_opened_leaves_it_as_it_was() {
        let dir = TempDir::new("failed");
        let live_path = dir.path().join(db_name(OTHER_ID));
        create_damaged(&live_path, 5, true);
        // As after an unclean stop: closing a connection that opened the file
        // for writing would copy the -wal file into the database.
        let live = Connection::open(&live_path).unwrap();
        live.query_row("PRAGMA wal_autocheckpoint = 0", [], |_| Ok(()))
            .unwrap();
        live.execute_batch("UPDATE wallet SET name = 'renamed'")
            .unwrap();
        let path = dir.path().join(db_name(WALLET_ID));
        for suffix in ["", "-wal", "-shm"] {
            fs::copy(sidecar(&live_path, suffix), sidecar(&path, suffix)).unwrap();
        }
        drop(live);
        let before = snapshot(&path);
        assert!(before[1].as_ref().is_some_and(|wal| !wal.is_empty()));

        // The file no longer matches what the checks before it found.
        let result = write_columns(&path, &[]);

        assert!(matches!(
            result,
            Err(NotRepaired::Failed(ref reason)) if reason == "the file changed while it was being repaired"
        ));
        assert_eq!(snapshot(&path), before);
        assert_eq!(check_file(&path).unwrap(), Finding::Damaged);
    }

    #[test]
    fn nothing_changes_when_the_backup_cannot_be_made() {
        let dir = TempDir::new("no-backup");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let path = db.join(db_name(WALLET_ID));
        create_damaged(&path, 5, true);
        let before = snapshot(&path);
        // A file where the backup folder would be created.
        let backup_root = dir.path().join(BACKUP_FOLDER);
        fs::write(&backup_root, b"").unwrap();

        let report = repair_all(&db, &backup_root, "stamp", &plenty);

        assert_eq!(report.backup_dir, None);
        assert_eq!(report.results.len(), 1);
        assert_eq!(
            report.results[0].outcome,
            WalletDbRepairOutcome::BackupFailed
        );
        assert!(
            report.results[0]
                .detail
                .starts_with("the backup folder could not be created")
        );
        assert_eq!(snapshot(&path), before);
        assert_eq!(scan(&db), vec![db_name(WALLET_ID)]);
    }

    #[test]
    fn only_damaged_files_are_backed_up_and_repaired() {
        let dir = TempDir::new("mixed");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        let damaged = db.join(db_name(WALLET_ID));
        let healthy = db.join(db_name(OTHER_ID));
        create_damaged(&damaged, 5, true);
        create_fresh(&healthy, 5, true);
        let healthy_before = snapshot(&healthy);

        let report = repair_all(&db, &dir.path().join(BACKUP_FOLDER), "stamp", &plenty);

        assert_eq!(report.results.len(), 1);
        assert_eq!(report.results[0].file, db_name(WALLET_ID));
        assert_eq!(
            listing(&report.backup_dir.unwrap()),
            vec![db_name(WALLET_ID)]
        );
        assert_eq!(snapshot(&healthy), healthy_before);
    }

    #[test]
    fn repair_without_damage_makes_no_backup() {
        let dir = TempDir::new("nothing");
        let db = dir.path().join("wallets");
        fs::create_dir(&db).unwrap();
        create_fresh(&db.join(db_name(WALLET_ID)), 5, true);
        let backup_root = dir.path().join(BACKUP_FOLDER);

        let report = repair_all(&db, &backup_root, "stamp", &plenty);

        assert_eq!(report.backup_dir, None);
        assert!(report.results.is_empty());
        assert!(!backup_root.exists());
    }

    #[test]
    fn a_second_repair_in_the_same_second_gets_its_own_folder() {
        let dir = TempDir::new("stamp");
        let root = dir.path().join(BACKUP_FOLDER);
        assert_eq!(create_backup_dir(&root, "s").unwrap(), root.join("s"));
        assert_eq!(create_backup_dir(&root, "s").unwrap(), root.join("s-1"));
    }

    #[test]
    fn backup_stamp_is_the_utc_time() {
        let t = time::OffsetDateTime::from_unix_timestamp(1_794_735_005).unwrap();
        assert_eq!(backup_stamp(t), "20261115T093005Z");
    }

    #[test]
    fn a_requested_repair_is_taken_once() {
        request_repair();
        assert!(repair_requested());
        assert!(take_repair_request());
        assert!(!repair_requested());
        assert!(!take_repair_request());
    }
}
