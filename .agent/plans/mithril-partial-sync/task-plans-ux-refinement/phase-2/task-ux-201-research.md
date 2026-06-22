# task-ux-201 — Research Note (durable findings)

## Decisions
- **Staging derives from `path.dirname(context.mithrilWorkDir)` (a SIBLING of the resolved chain), stored
  in a new STICKY field `_stagingChainDir`.** `_getStagingRootPath()` now returns
  `path.join(path.dirname(this._stagingChainDir ?? <fallback>), 'mithril-partial-sync')`, with a
  `stateDirectoryPath` fallback when no sync has run this session. Implements PRD D7/BUG3; honors backend
  PRD:250. See [[task-ux-102-research]] for the related lock-free/disk-backed `ChainStorageManager` findings.
- **`mithrilWorkDir` is the correct source — NOT `_activeWorkDir` and NOT `managedChainPath`.**
  `resolveMithrilWorkDir` → `resolveManagedChainPathFromEntryPoint` (`chainStoragePathResolver.ts:124-125,
  :37-93`) returns the **realpath-resolved chain directory** (follows the symlink, e.g. `/mnt/x/chain`).
  So for a chain symlinked `<stateDir>/chain` → `/mnt/x/chain`, `path.dirname(mithrilWorkDir) = /mnt/x` is
  the chain's REAL volume, whereas `dirname(managedChainPath)` (the unresolved symlink `<stateDir>/chain`)
  would be the WRONG (state-dir) volume — exactly the relocated-chain bug. `_activeWorkDir` is unsafe: it
  equals the chain dir only between `start()` :129–:163, is then reassigned to a path *inside* the staging
  tree, and is null during post-terminal recovery.
- **PRD:250 ("under the resolved Mithril work directory") reconciles with D7 ("sibling of the managed
  chain")** because the resolved work directory *is* the chain directory and the staged-only-restore lock
  forbids nesting staging inside the live chain (the `isPathWithin` guard at `mithrilPartialSyncStaging.ts:32`
  throws `PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN`). The faithful realization is a sibling on the same
  volume. A directory and its parent share a filesystem, so cutover (`fs.move`) is an intra-volume rename.
- **STICKY field is load-bearing (not an oversight).** `_stagingChainDir` is set in `start()` and is
  deliberately **NOT** cleared in `_clearRuntimeWorkState()`. `restartNormal`/`wipeAndFullSync` are
  post-terminal recovery actions gated by `_assertRecoveryActionAllowed` (requires `_activeWorkDir`/
  `_currentProcess` null), so they run AFTER `start()`'s `finally` already ran `_clearRuntimeWorkState()`.
  If `_stagingChainDir` were cleared there, those cleanups (`fs.remove(_getStagingRootPath())`) would target
  the LEGACY `stateDirectoryPath/mithril-partial-sync` and **leak** the colocated dir (a BUG2-class orphan).
  The field is overwritten on the next `start()`. `cancel()` cleans before its clear so it is safe either
  way, but post-failure recovery is not — hence sticky.
- **Disk preflight: placement, code, formula, fail-open.** `_assertSufficientDiskSpace(stagingRoot)` runs in
  `start()` after `_prepareStagingDirectory` and before the `_activeWorkDir` reassignment / download /
  cutover (so `snapshot.size` is known and the staging volume exists). Required bytes =
  `max(snapshot.size * 1.2, DISK_SPACE_REQUIRED (≈4 GB, config.ts:161))`. `snapshot.size` is the FULL
  certified-DB size (a conservative over-estimate of the partial range — good for fail-closed); the floor
  covers the `size: 0` case (`normalizeSnapshotItem` defaults size to 0). Emits a typed retriable
  `'preparing'`-stage error code **`PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`**. **Fail-closed** on a positive
  `free < required` reading; **fail-open** (log + proceed) on a `checkDiskSpace` *throw* — matching
  `handleDiskSpace.ts:176-181` precedent and safe under staged-only restore (pre-download nothing is
  destructive; a true mid-download disk-full fails the download stage retriably with no live-chain mutation).
  (Do NOT model fail-open on `preflightLegacyMigration` — it has no try/catch and fails closed.)

## Gotchas / evidence (verified against live code at plan + review time)
- `movePath` (`chainStorageManagerShared.ts:369-384`) catches `EXDEV` → `fs.copy` + `fs.remove`: this is the
  cross-device-copy-today mechanism. Colocation removes the `EXDEV` path so cutover is a rename. Cutover
  machinery itself was NOT changed.
- `_getStagingRootPath()` has exactly 3 call sites: `_prepareStagingDirectory` (`:748`, during `start()`),
  `_cleanupPartialSyncArtifacts` (`:548`, cancel/restart-normal), `wipeAndFullSync` (`:327`). All in-session
  paths have `_stagingChainDir` set; cross-session leftover reclaim is task-ux-202's job.
- `'preparing'` is a valid `MithrilPartialSyncErrorStage`; `_deriveAllowedRecoveryActions` (`:516-528`) maps
  any stage NOT in `['installing','finalizing','starting-node']` to the retriable
  `['retry','restart-normal','wipe-and-full-sync']`. The new error therefore surfaces as retriable.
- `check-disk-space` (dep `package.json:216`) is a default import returning `{ free, size }`; spec mock
  pattern `jest.mock('check-disk-space', () => jest.fn())` + `require(...).mockResolvedValue({ free, ... })`
  (precedent `chainStorageValidation.spec.ts:20`). The `../config` jest mock needed `DISK_SPACE_REQUIRED`
  added (it was absent).
- **Environment quirk:** `yarn compile` fails under Node v24 on its `precompile` hook
  (`yarn typedef:sass` → `node_modules/sass/sass.default.dart.js` `SyntaxError`), NOT on TypeScript. Use
  `./node_modules/.bin/tsc --noEmit` for the type-check gate (exit 0 here). (task-ux-102's log recorded
  `yarn compile → PASS`, so this env regressed since then — likely the Node-24 bump.)
- **Test-quality MINOR (non-blocking, left as-is):** the custom-storage "intra-volume cutover" assertion
  compares two hardcoded path literals (a tautology that adds no real coverage); the genuine BUG3 proof in
  that test is the `removeMock`/`ensureDirMock` assertions that observe the service's actual staging output.
  The download-never-ran guard is a negative arg-match rather than `not.toHaveBeenCalled()` (meaningful here
  only because `resolveLatestSnapshotMetadata` is mocked). A future cleanup could tighten both.

## Residual gaps / hand-off
- **task-ux-202 (cross-session leftover staging).** The C2 startup branch must `fs.remove(stagingRoot)` for
  leftover staging on a close-without-dismiss/crash, and must derive the SAME colocated path (from the
  marker's `managedChainPath` / resolved work dir), since `_stagingChainDir` is null on a fresh service that
  never ran `start()` this session (the `_getStagingRootPath()` fallback would target the legacy default).
- **task-ux-403 (renderer error copy).** Must add bespoke localized copy for the new code
  `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` (currently surfaced with a structured English fallback message).
- **task-ux-702 (manual QA).** Validate colocation + disk preflight on default + custom chain storage across
  the 3 networks; exercise the insufficient-disk path. Edge case to watch: a chain dir that is itself a
  mount point would make `path.dirname` the mount parent (possibly a different volume) — exotic, flagged.
- Threshold/sizing: the 1.2× safety factor + 4 GB floor are heuristics (intentionally conservative because
  `snapshot.size` over-estimates the partial range); revisit only if QA shows false blocks or near-misses.

## Conflicts found between PRD / research / tasks JSON / live repo
- **Tasks-JSON / research-19 line anchors drifted.** JSON `implementationNotes` cite `_getStagingRootPath()`
  at `:567-568` and `_activeWorkDir` at `:153`; live is `:577-578` and field-decl `:95` (set `:129`,
  reassigned `:163`). research-19 row 42 cites `:567-569`. Preferred the live repo; recorded here.
- **Tasks-JSON wording "derive from `_activeWorkDir`" conflicts with the staged-only/sibling invariant.**
  `_activeWorkDir`/`mithrilWorkDir` ARE the chain dir, so a literal read would stage inside the chain (guard
  violation). Reconciled by deriving from `path.dirname(mithrilWorkDir)` (the chain's parent), which is what
  D7 "sibling of the managed chain" and PRD:250 actually require. No doc edit needed — PRD is authoritative
  and already correct; the code was reconciled to it.
- `chainStorageManager.ts` (a JSON targetPath) was NOT edited — the resolved dir is already on
  `context.mithrilWorkDir` and `DISK_SPACE_REQUIRED` already exists in `config.ts`. Smallest truthful change
  (same pattern as [[task-ux-102-research]], which added `MithrilController.ts` outside the JSON targets).
