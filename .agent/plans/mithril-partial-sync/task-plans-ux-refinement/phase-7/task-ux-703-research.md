# task-ux-703 — research notes (durable findings)

Findings verified during planning (2026-07-03), independent of whether the wave lands unchanged:

- **ADR D-702a-1 governs the completed overlay.** The 4 s auto-finalize and the removal of
  "Continue to Daedalus" were a deliberate recorded amendment to locked decision #16 / PRD D9
  (`task-ux-702a-decisions.md` D-702a-1). PR feedback proposing a manual dismiss is arguing against
  a recorded ADR, not against drift — only the failure path (one-shot fire, swallowed rejection at
  `MithrilPartialSyncOverlay.tsx:84-93`) is a defect.
- **The behind-ness threshold is already absolute, not a percentage.**
  `mithrilPartialSyncThresholdImmutables = 20` (`nix/internal/launcher-config.nix:410`); one
  immutable chunk ≈ 6 h of chain, so 20 ≈ 5 days ≈ 1 epoch. Review discussion framed it as a
  percentage; that framing is stale. Consumers: `source/main/config.ts:78`,
  `MithrilPartialSyncService.ts:970`.
- **Offer vs prompt are separable surfaces.** Once the Diagnostics manual trigger renders whenever
  the feature is enabled (DD-703-2), the threshold constant gates only the proactive prompt — which
  dissolves the "allow partial sync when mildly behind" objection without lowering the prompt gate.
- **Snapshot metadata carries whole-DB sizes only** (`mithrilSnapshotMetadata.ts:17-21`: `size` /
  `total_size` / `total_db_size_uncompressed` / `size_bytes`). No per-immutable-file sizes are
  parsed, so partial-range disk estimates must be derived locally. The adopted estimate
  (DD-703-5) is snapshot-minus-chainDir with a margin proportional to the full snapshot; the margin
  direction matters because the downloaded range is always the newest, densest chunks (count-based
  proration would under-estimate).
- **Both mithril-client spawn sites detach on POSIX** (`mithrilCommandRunner.ts:162,258`,
  `detached: !environment.isWindows`) with no comment recording why; mithril-client spawns no
  grandchildren, so group-kill semantics are not obviously required. DD-703-6 removes detachment;
  if cancel-path verification disproves the no-grandchildren assumption, the recorded fallback is
  detached + PID persisted beside the partial-sync marker + startup orphan reaper.
- **T9's literal suggestion conflicts with a recorded decision — and wouldn't work.** ADR D-702a-2
  deliberately removed `wipe-and-full-sync` from the cancel dialogs (cancel is pre-cutover only;
  the chain DB is intact, so wipe is destructive and unnecessary). The real T9 defect is that
  `finalizeCancel` (`MithrilPartialSyncService.ts:403-449`) treats staging-removal failure as
  fatal, and both offered actions re-attempt the same locked `fs.remove`. Restoring wipe would hit
  the same lock (wipe also removes staging). Resolution DD-703-9: best-effort cleanup — land on
  `cancelled`, let `restart-normal` proceed on the intact DB, let startup-reclaim collect the
  leftover staging later. General lesson: review threads must be cross-checked against the
  702-series decision records before being treated as mechanical fixes; two of 32 threads (T9,
  T13) argued against recorded ADRs.
- **Cross-session recovery snapshots bypass the service.** `startInstalledNode`
  (`mithrilPartialSyncNodeStartup.ts:116,132`) emits `starting-node`/`failed` snapshots via
  `emitMithrilPartialSyncStatus` (`mithrilPartialSyncChannel.ts:132`) while
  `MithrilPartialSyncService._status` stays idle — any service-side assertion that consults
  `_status` (e.g. `_assertRecoveryActionAllowed:801`) is blind to them. The last broadcast is
  readable via `getMithrilPartialSyncStatus` (`:66`); seeding at the controller's IPC wirings is
  the minimal bridge (T5 fix).
- **GitHub thread state at planning time:** all 32 threads unresolved; T1 and T2 marked outdated
  (code moved, findings persist); T12+T31 and T20+T32 are duplicate pairs (same line, independent
  reporters); AndrewWestberg is the only external human reviewer (T2 reply, T31, T32).

Additional durable findings from the 2026-07-03 verification grill (4-agent code verification):

- **`detached` is load-bearing, not legacy.** `killProcessTree` kills POSIX trees via
  `process.kill(-pid, signal)` (`killProcessTree.ts:72`) and its docstring records the dependency
  on the runner spawning `detached` so the child leads its own process group. POSIX children
  reparent to init on parent crash regardless of `detached`, so removing it can never fix
  crash-orphaning — it only breaks group kill. (T14 refuted; DD-703-6 revised.)
- **Probe failure is invisible to the renderer.** `getPartialSyncBehindness` catches and returns
  `{ isSignificantlyBehind: false }` (`MithrilPartialSyncService.ts:1000-1007`);
  `MithrilPartialSyncAvailability` has no error field — "probe failed" and "near tip" are
  indistinguishable downstream until DD-703-10's `isProbeFailed` flag lands.
- **Cancel-orphaned staging has no startup reclaim.** The marker is written only at
  `cutover-in-progress`, and cancel is pre-cutover-only, so `handleInterruptedRecovery` returns
  early on the missing marker; the only reclaim of a cancel-orphaned staging dir is the next
  partial-sync start's `preparePartialSyncStagingDirectory` (`mithrilPartialSyncStaging.ts:40`).
- **A recursive dir-size helper already exists**: `getPathSizeBytes`
  (`chainStorageManagerShared.ts:499-520`, instance seam `ChainStorageManager._getPathSizeBytes`) —
  greps for `getDirectorySize`/`du` miss it.
- **The main copy-leak path is `_buildError`**: it copies `error.message` verbatim into
  `status.error` (`MithrilPartialSyncService.ts:1272-1290`), and `MithrilErrorView` renders that
  message in an always-visible `<p>` (`MithrilErrorView.tsx:130`) — every service throw reachable
  from `start()`/recovery catches is user-visible, which is why T16 must cover all throw sites
  (including `:1080-1082`, the one string containing banned "immutable").
- **The two kill implementations differ by design** (N1 declined): `killProcessTree` = live
  `ChildProcess`, group kill, fire-and-forget, direct-kill fallback;
  `CardanoNode._killProcessWithName` = raw pid (possibly a stored previous-session PID), single-pid
  kill, `promisedCondition` death-poll.
