# task-ux-201 — Plan Review Log (append-only)

Planner: 2026-06-22T13:10:00Z
Drafted the canonical plan for task-ux-201 (colocate partial-sync staging on the resolved chain volume +
disk-space preflight, PRD D7/BUG3). A read-only Plan subagent produced a design dossier; the orchestrator
independently re-verified every load-bearing anchor against live code before authoring the canonical doc.

Key design decisions resolved:
- (a) Derive staging from `path.dirname(context.mithrilWorkDir)` (a SIBLING of the managed chain), stored
  in a new field `_stagingChainDir` set at start() :129. `mithrilWorkDir` is the REALPATH-RESOLVED chain
  dir (`resolveMithrilWorkDir` -> `resolveManagedChainPathFromEntryPoint`, chainStoragePathResolver.ts
  :124-125,:37-92), so for a symlinked chain `<stateDir>/chain` -> `/mnt/x/chain` its dirname `/mnt/x` is
  the chain's REAL volume, whereas `dirname(managedChainPath)` would be the wrong (state-dir) volume. This
  is the deciding factor for `mithrilWorkDir` over `_activeWorkDir` (mutated mid-flow / null in recovery)
  and over `managedChainPath` (unresolved symlink). Default case is byte-identical to today.
- (a) Reconciled PRD:250 ("staging under the resolved Mithril work directory") with D7 ("sibling of the
  managed chain"): because the resolved work dir IS the chain dir and staged-only forbids nesting inside
  the live chain (the isPathWithin guard in mithrilPartialSyncStaging.ts:32 throws
  PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN), the faithful realization is a sibling on the same volume.
- (b) CORRECTION over the dossier: `_stagingChainDir` must be STICKY — set in start(), NOT cleared in
  `_clearRuntimeWorkState()`. Verified the cleanup ordering: restart-normal (:316) and wipe-and-full-sync
  (:327) are post-terminal recovery actions gated by `_assertRecoveryActionAllowed` (requires
  `_activeWorkDir`/`_currentProcess` null at :534), so they run AFTER start()'s finally already ran
  `_clearRuntimeWorkState()` (:246). If `_stagingChainDir` were cleared there, those cleanups would
  `fs.remove` the LEGACY `stateDirectoryPath/mithril-partial-sync` and leak the colocated dir (a BUG2-class
  orphan). cancel() cleans (:278) before its clear (:308) so it is safe regardless, but the post-failure
  recovery actions are not — hence sticky is required. The dossier proposed clearing it; rejected.
- (c) Disk preflight inserted in start() right after `_prepareStagingDirectory` (after :161, before the
  `_activeWorkDir` reassignment :163 and before download :194). New code `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`
  on stage `'preparing'` (maps via `_deriveAllowedRecoveryActions` :524-528 to the retriable set
  [retry,restart-normal,wipe-and-full-sync]). requiredBytes = max(snapshot.size * 1.2, DISK_SPACE_REQUIRED
  (~4GB floor, config.ts:161)). snapshot.size available pre-download (set :148, before :194). Fail-closed
  on positive `free < required`; fail-OPEN (log + proceed) on a checkDiskSpace throw — matches repo
  precedent (preflightLegacyMigration chainStorageManagerLayout.ts:454-462; handleDiskSpace.ts:177) and is
  safe under staged-only restore.

Verified seams (all confirmed TRUE against live code; tasks-JSON line anchors had drifted):
- `_getStagingRootPath()` at :577-578 (JSON said :567-568) joins `stateDirectoryPath` + the :57 constant.
- start() flow: `_activeWorkDir = context.mithrilWorkDir` :129, `_prepareStagingDirectory` :159-161,
  reassignment :163, download :194, cutover :218.
- `movePath` (chainStorageManagerShared.ts:369-384) catches EXDEV -> fs.copy + fs.remove (CONFIRMS the
  cross-device-copy-today claim; colocation removes EXDEV).
- guard `isPathWithin(managedChainPath, rootPath)` at mithrilPartialSyncStaging.ts:32 (sibling passes).
- `check-disk-space` default import + `{free,size}` return; mock pattern jest.mock + require (per
  chainStorageValidation.spec.ts:20). `DISK_SPACE_REQUIRED = 4*1073741274` at config.ts:161.
- spec hardcodes `/tmp/daedalus-state/mithril-partial-sync` in ~20 places; `createContext()` uses
  `mithrilWorkDir: '/tmp/mithril-workdir'` (parent /tmp) and the custom-storage test uses
  `mithrilWorkDir: '/mnt/custom-storage/chain'` — both expectations must move to the colocated paths.

Scope kept minimal: only MithrilPartialSyncService.ts + its spec change; chainStorageManager.ts (a JSON
target) is NOT edited (resolved dir already on context; DISK_SPACE_REQUIRED already in config). The
cross-session leftover-staging reclaim is explicitly handed off to task-ux-202 (C2 startup branch).

FOR THE CRITIQUER — focus on: (1) is sticky-`_stagingChainDir` truly correct for ALL `_getStagingRootPath`
call sites, or is there a path where it is stale/wrong? (2) is `path.dirname(mithrilWorkDir)` the right
sibling source vs `managedChainPath`, incl. the guard interaction in the symlink case? (3) is fail-open on
measurement error acceptable vs the literal "fails closed" acceptance wording? (4) is the preflight
placement truly "before any destructive/download step" given `_prepareStagingDirectory`'s fs.remove? (5)
is the required-bytes formula defensible / not over-engineered? (6) is the test plan concise and
small-model-implementable (esp. the ~20 expectation updates)?
Outcome: plan drafted, awaiting critique

---

Critiquer: 2026-06-22T13:35:00Z
One broad adversarial pass over task-ux-201.md, independently re-verifying load-bearing claims against
live code. All design/safety claims confirmed CORRECT.

[BLOCKER 1] Test plan omitted two existing `start()`-driving tests whose `readdirMock` keys hardcode the
OLD staging path: `'maps Mithril progress into downloading and verifying status updates'`
(MithrilPartialSyncService.spec.ts:510-594; keys at :538,:543) and `'fails in installing when converted
staged output includes volatile'` (:705-761; keys at :727,:732). With the colocated path
(`createContext().mithrilWorkDir='/tmp/mithril-workdir'` ⇒ staging `/tmp/mithril-partial-sync`), the first
test's staged-db readdir stops matching ⇒ validateConvertedStagedOutput throws PARTIAL_SYNC_STAGED_DB_INVALID
⇒ start() rejects while the test asserts `.resolves.toBeUndefined()` (:570) = HARD failure of the plan's own
`yarn test:jest` gate; the second silently tests the wrong thing. Required fix: add the :538,:543,:727,:732
readdir-key updates to the Test plan.

[MINOR 1] Fail-open precedent half-accurate: only handleDiskSpace.ts:176-181 fails open; preflightLegacyMigration
(chainStorageManagerLayout.ts:452) calls checkDiskSpace with NO try/catch (fails closed). Drop/qualify the
preflightLegacyMigration citation so the implementer doesn't copy the wrong pattern. Acceptance "fails closed
when space is insufficient" is satisfied by blocking on positive `free < required`.
[MINOR 2] createLatestSnapshot (:55-62) hardcodes size:2; the disk-fail test must extend it or spy
resolveLatestSnapshotMetadata to inject a large size — name this explicitly.
[MINOR 3] DISK_SPACE_REQUIRED (config.ts:161) is `4 * 1073741274` (note: 1073741274, not 1073741824 = 1 GiB;
off by ~550 bytes/GB) — established repo floor; reuse is correct, not this task's bug.

Verified, no defect (independently confirmed): only 3 `_getStagingRootPath()` call sites (:327, :548, :748);
sticky `_stagingChainDir` reliably set before :748 and survives into post-terminal restartNormal/wipe cleanup
(gated by `_assertRecoveryActionAllowed` :531-545 which runs after start()'s finally :246) — clearing it WOULD
leak the colocated dir, so Decision (b) is load-bearing and correct. `resolveMithrilWorkDir`
(chainStoragePathResolver.ts:124-126,:37-93) returns the realpath-resolved chain DIR, so `dirname(mithrilWorkDir)`
is the chain's real volume in the symlink case (managedChainPath would be wrong). `isPathWithin` guard
(mithrilPartialSyncStaging.ts:32) uses path.resolve (no symlink follow); a sibling passes in both default and
symlink cases — no wrong-throw / wrong-pass edge. movePath EXDEV→copy fallback confirmed
(chainStorageManagerShared.ts:369-384). Preflight placement precedes all destructive/download steps;
_prepareStagingDirectory only touches scratch. 'preparing' valid stage → retriable recovery set (:516-528);
throw lands in start() catch → status:'failed'. New code PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE collides with no
existing code. snapshot.size available pre-download (set :148,:192 before :194); formula sound. Manual-state
cancel/restart/wipe tests (:763-898) never call start() ⇒ fallback path ⇒ their removeMock assertions
(:799,:863,:892) stay correct WITHOUT change; logPath (:271) independent of staging. chainStorageManager.ts
(JSON target) correctly NOT edited; api-endpoints.md correctly irrelevant (no IPC change).
Decision: requires_changes

---

Planner: 2026-06-22T13:45:00Z
Fix pass after critique (one pass, then build, per the loop).
(1) BLOCKER 1 fixed: added a REQUIRED Test-plan callout for the two extra start()-driving tests — update the
readdirMock keys at spec.ts:538,:543 (the progress-mapping test, else HARD .resolves failure) and :727,:732
(the volatile-install test), and an explicit "tests that must stay UNCHANGED" note for the manual-state
cancel/restart/wipe tests (:763-898) and logPath (:271).
(2) MINOR 1 fixed: Decision (c) now cites only handleDiskSpace.ts:176-181 for fail-open-on-measurement-error
and explicitly warns OFF the preflightLegacyMigration pattern (no try/catch ⇒ fails closed); clarified that
"fails closed when insufficient" = blocking on a positive `free < required` reading. Helper-comment citation
updated to match.
(3) MINOR 2 fixed: the disk-fail test now explicitly says to extend createLatestSnapshot with an optional size
or spy resolveLatestSnapshotMetadata to inject a large `.snapshot.size`, rather than assuming the fixture
yields a large size.
MINOR 3 needs no doc change (DISK_SPACE_REQUIRED reuse is correct as-is). No structural/design changes; all
design + safety claims were verified TRUE by the Critiquer. Planning status set to `approved`.
Outcome: plan approved, proceeding to implementation

