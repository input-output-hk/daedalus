# Watchdog: Test Coverage & Security Hardening

Status tracking for all findings from the August 2026 audit.

---

## Security Fixes

### S1 — PID-reuse race in force-kill path `supervisor.rs:116` ✅
**Priority:** Medium  
**Problem:** `force_kill_by_pid` sends SIGKILL using a bare `u32` PID 30 seconds after the node `Child` was moved into a watcher task. Once the watcher calls `wait()`, the zombie is reaped and the PID could be recycled by an unrelated process before the kill fires.  
**Fix:** Replace kill-by-PID with a `tokio::sync::oneshot` kill channel. Watcher task selects on `node_proc.wait()` vs the kill receiver; on kill signal calls `child.start_kill()` + `child.wait()`. Change `wait_for_node_exit` to accept `&mut Option<Sender<()>>` instead of `node_pid`. Remove `force_kill_by_pid` entirely.  
**Files:** `supervisor.rs` — `force_kill_by_pid`, `wait_for_node_exit`, node watcher spawn, all call sites (5 total including the inline shutdown block at line ~855).

### S2 — No size cap on stdin line reads `main.rs` ✅
**Priority:** Medium  
**Problem:** `AsyncBufReadExt::read_line` has no limit; a line without `\n` from a wedged Electron process grows unboundedly.  
**Fix:** Wrap the stdin `BufReader` with `.take(MAX_LINE_BYTES)` (4 MB) before calling `lines()`. Use `tokio::io::AsyncReadExt::take`.  
**Files:** `main.rs` — stdin reader setup.

### S3 — `wipe_chain` path not validated `mithril.rs:431` ✅
**Priority:** Low  
**Problem:** `remove_dir_all(cfg.chain_path)` with no constraint; a misconfigured `chain_path = "/"` deletes the user's home directory.  
**Fix:** Before `remove_dir_all`, assert that `chain_path` is absolute and that it is a descendant of `cfg.state_dir`. Return a `PipelineResult::Cancelled` with `INVALID_CHAIN_PATH` error code if the check fails.  
**Files:** `mithril.rs` — `run_pipeline` near line 431.

### S4 — `ShutdownPipe` missing `Drop` impl `supervisor.rs:268` ✅
**Priority:** Low  
**Problem:** On error paths before `close_write()` is called, the write fd leaks until process exit. cardano-node never receives its clean shutdown EOF; it is SIGKILL'd instead.  
**Fix:** Add `impl Drop for ShutdownPipe` that calls `self.close_write()`. Guard with a `closed` bool field so `close_write()` is idempotent (avoids double-close UB on the normal path where it's explicitly called).  
**Files:** `supervisor.rs` — `ShutdownPipe` struct and `impl`.

### S5 — Broken stdout pipe not detected `protocol.rs` ✅
**Priority:** Low  
**Problem:** If Electron closes the watchdog's stdout, `emit()` silently discards errors and the watchdog runs as an orphan indefinitely.  
**Fix:** In `emit()`, after `writeln!`, check if the error kind is `BrokenPipe`; if so, call `std::process::exit(0)` (or send a shutdown signal via a global `AtomicBool`). Use an `AtomicBool` flag set on broken pipe; check it at the top of the supervisor's command loop so shutdown is clean rather than abrupt.  
**Files:** `protocol.rs` — `emit()`. `main.rs` — check the flag in the command loop.

---

## Protocol Test Coverage

### T1 — Missing event serialization tests `protocol.rs` ✅
**Priority:** Medium  
**Missing variants:**
- `NodeBlockSyncProgress` (`kind`, `progress` fields)
- `NodeStartupStatus` (`phase` field)
- `Error` (`message` field)
- `MithrilStatus` (`phase` field)
- `MithrilProgress` (all 7 fields)
- `MithrilNotNeeded` (`local_immutable_count`, `latest_certified_immutable`)
- `MithrilSignificantlyBehind` (same fields)
- `MithrilError` (`code`, `message`)
- `ChainStatus` (`has_chain`)

**Fix:** Add `#[test]` cases in `protocol.rs` for each missing variant, asserting correct `"event"` key and field names match the TypeScript contract.

### T2 — Missing command deserialization tests `protocol.rs` ✅
**Priority:** Low  
**Missing:** `StartMithril` (with `force: true`, `wipe_chain: true`), `CancelMithril`, `ProbeMithril`, `StartNode`.  
**Fix:** Add round-trip `serde_json::from_str` tests for each.

---

## Supervisor / Mithril Unit Tests

### T3 — Unit tests for `try_parse_startup_status` `supervisor.rs` ✅
**Priority:** Medium  
**Problem:** Priority-ordered substring matching; no tests verify ordering is correct or that shorter strings don't shadow longer ones.  
**Fix:** Add `#[cfg(test)]` block in `supervisor.rs` with tests for all 8 positive cases and several negative cases (unrelated log lines, empty string).

### T4 — Unit tests for `try_parse_block_sync` `supervisor.rs` ✅
**Priority:** Medium  
**Problem:** Comma-decimal filter and percentage extraction untested for edge inputs.  
**Fix:** Add tests for: `"Replayed block"`, `"Validating chunk"`, `"Validated chunk"`, `"Pushing ledger state"`, each with `"Progress: 42.5%"`. Also test: no `Progress:` field (returns None), empty string, European locale `"Progress: 1,23%"` (→ 1.23), multiple `%` signs, missing kind match.

---

## Integration Test Gaps

### T5 — `probe_mithril` command end-to-end `tests/integration.rs` ✅
**Priority:** High (we just wired this on the TS side)  
**Problem:** No test sends `probe_mithril` and verifies `mithril_significantly_behind` or `mithril_not_needed`.  
**Approach:** Add two tests. For "not needed": start with populated chain far ahead of mock mithril's certified count; send `probe_mithril`; expect `mithril_not_needed`. For "significantly behind": need mock mithril to report a high certified count; send `probe_mithril`; expect `mithril_significantly_behind`. The mock binary already outputs JSON; we may need a variant that returns a higher immutable number.  
**Files:** `tests/integration.rs`, possibly a new mock or env-var-configurable mock.

### T6 — `wipe_chain: true` path `tests/integration.rs` ✅
**Priority:** Medium  
**Problem:** `remove_dir_all(chain_path)` branch never exercised. Bootstrap full-replace install path untested.  
**Approach:** Populate chain, then send `start_mithril` with `"wipe_chain": true`; verify pipeline runs to completion and chain directory is replaced. After `finalizing`, assert chain dir exists and staging dir is gone.  
**Files:** `tests/integration.rs`.

### T7 — Node crash after wallet ready (post-ready path) `tests/integration.rs` ✅
**Priority:** Medium  
**Problem:** Only `node_crash_before_socket` is tested. The `node_rx.changed()` arm in Phase 2 (line 800) is uncovered.  
**Approach:** Start normally; wait for `wallet_ready`; SIGKILL the node (get PID from `node_started` event); expect `node_exited` then `stopped`.  
**Files:** `tests/integration.rs`. Requires reading node PID from event.

### T8 — Mithril download failure `tests/integration.rs` ✅
**Priority:** Medium  
**Problem:** `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` error path not tested.  
**Approach:** Add a `mock-mithril-client-fail` binary (or env-var-driven failure) that exits non-zero. Test: send `start_mithril`, expect `mithril_error{code: "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED"}`, then `mithril_status{phase: "cancelled"}` or `chain_status{has_chain:false}` (re-prompt).  
**Files:** `src/bin/mock_mithril_client_fail.rs`, `tests/integration.rs`.

### T9 — Mithril conversion failure `tests/integration.rs` ✅
**Priority:** Low  
**Problem:** `PARTIAL_SYNC_CONVERSION_FAILED` error path not tested.  
**Approach:** Add a `mock-snapshot-converter-fail` binary (exits non-zero). Send `start_mithril`, download succeeds, converter fails; expect `mithril_error{code: "PARTIAL_SYNC_CONVERSION_FAILED"}`.  
**Files:** `src/bin/mock_snapshot_converter_fail.rs`, `tests/integration.rs`.

### T10 — Marker recovery on restart `tests/integration.rs` ✅
**Priority:** Low  
**Problem:** `cutover-in-progress` cleanup and `installed-awaiting-node-start` clearing on startup not tested.  
**Approach:**  
- For `cutover-in-progress`: pre-populate the marker file and a fake staging dir in the TempDir; spawn watchdog; verify staging dir is gone (watchdog cleaned it) and node starts.  
- For `installed-awaiting-node-start`: pre-populate the marker; verify it's cleared and node starts normally (no mithril).  
**Files:** `tests/integration.rs`.

### T11 — `max_restart_attempts = 0` (wallet never restarts) ✅
**Priority:** Low  
**Problem:** Config test confirms the value is accepted; no integration test verifies the behaviour.  
**Approach:** Set `max_restarts(0)`, use `MOCK_WALLET_CRASH`; expect `wallet_started` → `wallet_exited` → `wallet_unrecoverable{attempt:0}` → `stopped`.  
**Files:** `tests/integration.rs`.

---

## Test Infrastructure Fixes

### T12 — RAII temp dirs in `ipc.rs` ✅
**Priority:** Low  
**Problem:** `ipc.rs` manually calls `std::fs::remove_dir_all` at the end of each test; dirs leak on panic or timeout.  
**Fix:** Replace with the same `TempDir` RAII wrapper from `integration.rs` (or move it to a shared `tests/helpers.rs`).  
**Files:** `tests/ipc.rs`, possibly `tests/helpers.rs`.

### T13 — Document `free_port()` TOCTOU ✅
**Priority:** Informational  
**Problem:** Both test files bind port 0, release the listener, then pass the port to the watchdog. Race window on busy CI.  
**Fix:** Add a comment explaining the race and why it's acceptable (no `SO_REUSEPORT` support needed since this is test-only). No code change required unless spurious failures appear.  
**Files:** `tests/integration.rs`, `tests/ipc.rs`.

---

## Implementation Order

1. **S1** (PID-reuse) — most architecturally invasive, do first while code is fresh
2. **S4** (ShutdownPipe Drop) — small, standalone
3. **S2** (stdin size cap) — one-liner in main.rs
4. **S3** (wipe_chain validation) — small guard in mithril.rs
5. **S5** (broken stdout) — protocol.rs + main.rs flag
6. **T1+T2** (protocol tests) — pure additions to protocol.rs test block
7. **T3+T4** (supervisor unit tests) — pure additions to supervisor.rs
8. **T12** (RAII in ipc.rs) — small refactor
9. **T5** (probe_mithril integration) — most important missing integration test
10. **T6** (wipe_chain integration) — straightforward
11. **T7** (node crash post-ready) — straightforward
12. **T8+T9** (mithril failure paths) — new mock binaries
13. **T10** (marker recovery) — filesystem setup in test
14. **T11** (max_restart=0) — one-line test
15. **T13** (TOCTOU comment) — documentation only

---

## Commit Plan

- Commit 1: `fix(watchdog): security hardening — PID-reuse, stdin cap, path validation, pipe Drop, broken-pipe detection` (S1–S5)
- Commit 2: `test(watchdog): add missing protocol serialization and unit tests` (T1–T4)
- Commit 3: `test(watchdog): add missing integration tests and fix ipc.rs temp dir cleanup` (T5–T13)
