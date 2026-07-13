# task-ux-702d — Implementation Review (ad-hoc finalization cleanup wave)

> Implementation + code-review log for the `task-ux-702d` wave (CAT-A.. — per-CAT docs:
> `task-ux-702d-cat-a.md`..`task-ux-702d-cat-e.md`), including the CAT-D follow-up
> hardening that closes the late-spawn child-registration race.
> Plan: `task-ux-702d.md`. Plan review: `task-ux-702d-plan-review.md`.
> Two Implementer journal entries were relocated here from the former task-ux-702d.md tail on 2026-07-01 (see the
> provenance notes at the end).

---

## Implementation

- 2026-07-01 — CAT-A/B/C landed on the shared working tree: proactive-prompt card opacity (`--theme-mithril-card-background`
  alpha `0.96 → 1`), proactive-prompt handoff-note copy/i18n update to "the Daedalus Diagnostics screen under the Help
  menu. (Ctrl + D)", and the shared partial-sync/fast-sync backdrop tokens `--theme-mithril-overlay-backdrop-start/-end`
  alpha `0.92 → 1`. Those presentation/copy slices remain documented inline in `task-ux-702d.md`.
- 2026-07-01 — CAT-D initially landed as the interruptible stage machine + tracked converter child: added
  `MithrilPartialSyncCancelledError`, `_throwIfCancelled()`, checkpoints CP-A..CP-D in `start()`, and `_runBinary`
  `onProcess` wiring so converting-stage cancel settles to `finalizeCancel()` instead of the `abandonCancel()` floor.
- 2026-07-01 — CAT-D follow-up hardening landed after manual retest showed a restart-and-cancel could still surface the
  failed prompt while a `mithril-client` download remained alive. Added `_trackCurrentProcess(child)` in
  `MithrilPartialSyncService`, routed both `_runCommand` and `_runBinary` through it, and immediately kill a
  late-registered child when `_isCancelled` is already true. Added the two new regression cases in
  `MithrilPartialSyncService.spec.ts` covering late-spawn download and converter children.

## Code Review

- 2026-07-01 — CAT-D follow-up review: **approved**. Review confirmed the service-side helper closes the async
  pre-spawn registration window in `runCommand()`/`runBinary()` without touching `mithrilCommandRunner.ts`, preserves
  the success path, keeps cancel Boundary-A-only, and adds the right focused regression coverage.

## Whole-task verification

- 2026-07-01 — `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts --runInBand` → **PASS** (51
  tests), including the new late-spawn download/conversion cases.
- 2026-07-01 — editor diagnostics (`get_errors`) on `MithrilPartialSyncService.ts` and
  `MithrilPartialSyncService.spec.ts` → **PASS** (no errors found).

## Task-state decision

`task-ux-702d` remains **pending / not completed** because this phase-7 task is `interactive_validation`: the operator
cancel-at-each-stage re-test is still pending, and the wave remains open for any additional ad-hoc CAT-F.. follow-up
items before `task-ux-702` can re-run.

## Implementer

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: at the CAT-D
> landing summarized in the Implementation section above, before the follow-up hardening entry below.)

Timestamp: 2026-07-01
Speaker: Implementer (CAT-D landing)

**Landed CAT-D on branch `feat/mithril-cat-d-cancel`** (isolated worktree branched off post-702c HEAD `1364b685c`, so
the diff is exactly CAT-D with none of the CAT-A/B/C presentation churn). Before editing, re-verified **every** CAT-D
citation against the live post-702c tree — all line numbers matched the plan (the sentinel-class seam `:100`, the four
`await` seams in `start()`, `_downloadAndVerifyPartialSnapshot`'s unconditional `downloading` re-emit, `_convertStagedSnapshot`
/ `_runBinary`, `_runCommand`'s `onProcess` precedent, and `runBinary`'s already-supported `callbacks`/`onProcess`
contract in `mithrilCommandRunner.ts:122-128,157,185,191`).

Applied the fix exactly as specified — **1 production file** (`MithrilPartialSyncService.ts`): a
`MithrilPartialSyncCancelledError` sentinel class beside `MithrilPartialSyncStageError`; a `_throwIfCancelled()` guard
next to `_clearRuntimeWorkState`; the four `this._throwIfCancelled()` checkpoints — **CP-A** (after the disk-space
preflight, before the drift re-resolve/download), **CP-B** (top of `_downloadAndVerifyPartialSnapshot`, before the
unconditional `downloading` re-emit + the download spawn), **CP-C** (after `validateStagedDownloadOutput`, before the
`verifying` emit), **CP-D** (after conversion + `validateConvertedStagedOutput`, strictly **before**
`_activatePostVerificationStage('installing')` and the `cutover-in-progress` marker write); and **Part 2** — the
fifth `callbacks` arg `{ onProcess: (child) => { this._currentProcess = child; } }` on the `runBinary(...)` call in
`_runBinary`, mirroring `_runCommand`. `mithrilCommandRunner.ts`, `cancel()`, `forceKill()`, `finalizeCancel()`,
`abandonCancel()`, the coordinator, and every status/type were left untouched. All checkpoints sit **inside** `start()`'s
`try`, so the existing `if (this._isCancelled) return;` catch swallows both the sentinel and a killed-converter
`converting` stage error with no `failed` emit.

**Tests** (`MithrilPartialSyncService.spec.ts` + `chainStorageCoordinator.spec.ts`): added the `_throwIfCancelled` unit
case; cancel-during-`preparing` (flag flipped in the disk-space stub) → `start()` resolves, no `_downloadAndVerifyPartialSnapshot`,
no `_runCommand` download, no `_installValidatedStagedSnapshot`, no `cutover-in-progress` marker, no `failed`/`downloading`
emit; cancel-during-`converting` (flag flipped in the `_convertStagedSnapshot` stub) → CP-D unwinds, no install / no
cutover marker; killed-converter (flag + thrown `converting` stage error) → swallowed, no `failed`, no cutover; Part-2
converter-tracking → the real `_runBinary` passes `onProcess`, which sets then clears `_currentProcess`; and the required
coordinator assertion that a settled cancel join routes to `finalizeCancel`, not `abandonCancel`. Registered `runBinary`
in the spec's `mithrilCommandRunner` mock. The 702c cancel-sequence specs are preserved unchanged.

**Verification (all green):** `yarn test:jest` for both specs — **90/90 passed**; `yarn compile` (tsc `--noEmit`) exit 0
with zero errors referencing the touched files; ESLint **0 errors** (only the repo's usual test/style warnings);
`prettier --check` clean on all three touched files. The Node v24 renderer verify-env caveat did not apply (main-process
TS + jest; no `.scss.d.ts`). A per-category **code-review** pass returned **no correctness findings** — it independently
confirmed the four checkpoint placements, that CP-D is strictly before the cutover marker (Boundary-A-only cancel holds),
that no new status/type/IPC/Boundary is introduced, that the success path is inert (each checkpoint a no-op; `onProcess`
tracks/clears exactly like the download path), and that `_currentProcess` is nulled with no stale-pid risk. One
non-blocking test-comment accuracy nit was applied (the preparing test unwinds at **CP-A**, not CP-A/CP-B).

**Still pending before the wave closes:** the operator cancel-at-each-stage functional re-test (verify-only,
preprod/Linux) — cancel during `preparing`, `downloading`, and `converting`; each must reach the terminal `cancelled`
prompt, not re-spawn/pop-back, remove the `mithril-partial-sync` staging dir, and never show the "Downloading the Mithril
snapshot failed / logs+quit" floor.

Decision: CAT-D IMPLEMENTED + verified; operator functional re-test pending; wave OPEN.

## Implementer

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: at the CAT-D
> follow-up hardening + doc sync, after the landing entry above.)

Timestamp: 2026-07-01T20:14:14Z
Speaker: Implementer (CAT-D follow-up hardening + doc sync)

Implemented the follow-up hardening on the shared working tree: added `_trackCurrentProcess(child)` to
`MithrilPartialSyncService`, switched both `_runCommand` and `_runBinary` to use it, and immediately kill a
late-registered child when `_isCancelled` is already true. Added the two new regression tests in
`MithrilPartialSyncService.spec.ts` covering late-spawn download and converter children. Focused validation:
`yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts --runInBand` → PASS (51 tests); touched-file
diagnostics clean.

Synced the documentation surfaces in the same pass: updated `task-ux-702d.md`, appended the follow-up critique record
to `task-ux-702d-plan-review.md`, created `task-ux-702d-impl-review.md` + `task-ux-702d-research.md`, and updated the
`task-ux-702d` entry in `mithril-partial-sync-ux-refinement-tasks.json` so it no longer says CAT-D is merely plan
approved. Operator cancel-at-each-stage re-test remains pending; the wave stays open for further ad-hoc CATs.

Decision: CAT-D hardening + doc sync landed; operator functional re-test pending; wave OPEN.

## Implementation

Timestamp: 2026-07-02T07:13:01Z
Speaker: Implementer (CAT-E Part 1 — cancel-lifecycle logging, non-behavioral)

Landed **CAT-E Part 1 only** (pure observability / logging; strictly non-behavioral) on the shared working tree, on
top of the staged, uncommitted CAT-A..D work (which was preserved exactly — nothing stashed/reset/unstaged). **Parts 2
and 3 are NOT included** and remain operator-gated (see the CAT-E doc / research entry). **committed = no** — this
folds into the single 702d commit later.

Added structured, side-effect-free `logger.info`/`logger.warn` calls (structured context objects, no string-concat of
secrets, never throwing, never altering control flow) at all prescribed Part-1 sites:

- `source/main/mithril/mithrilCommandRunner.ts` — for **both** `runBinary` and `runCommand`: a post-spawn
  `logger.info('[mithril] child spawned', { pid })` beside the `onProcess(child)` fire; a new **additive** logging-only
  `child.on('exit', (code, signal) => logger.info('[mithril] child exited', { pid, code, signal, killed }))` listener
  (there was previously no `'exit'` listener anywhere); and `logger.info('[mithril] child closed', { pid, exitCode })`
  inside the existing `'close'` handlers **before** the preserved un-truncated `resolve({ stdout, stderr, exitCode })`.
  The runner's resolve event stays `'close'` (never swapped to `'exit'`).
- `source/main/mithril/MithrilPartialSyncService.ts` — cancel() real-kill branch: `info` with
  `{ status, pid, hadChild }` **before** `this._currentProcess.kill()`; `_trackCurrentProcess()` late-kill: `info`
  before `child.kill()`; `finalizeCancel()`: `info` at entry, `info` after `_cleanupPartialSyncArtifacts()` success,
  and a `warn` on the previously-**silent** catch (surfacing the boundary-A cleanup failure with the captured
  `cancelFallbackErrorStage`); `abandonCancel()`: a `warn` at the "restart Daedalus" floor including the captured
  `_cancelFallbackErrorStage`.
- `source/main/utils/chainStorageCoordinator.ts` — `_awaitRunSettledBounded()`: a `warn` on the 15s join-timeout /
  `forceKill` escalation (`{ joinTimeoutMs: 15000, forceKillTimeoutMs: 1000 }`); `cancelPartialSync()`: an `info`
  branch-decision log on the settled → `finalizeCancel` path and a `warn` on the unsettled → `abandonCancel` path
  (nearest house prefix `'[MITHRIL] '`).

Tests (Part-1 scope — the three in-scope specs; `MithrilController.spec.ts` is Part 2 and was left untouched):
- `mithrilCommandRunner.spec.ts` — 2 new tests (runCommand + runBinary) asserting the child-spawned pid log, the
  `'exit'` listener registration + `{ code, signal }` log, the child-closed log, and that `'close'` still resolves
  `{ stdout, stderr, exitCode }` **un-truncated** (100k-char stdout).
- `MithrilPartialSyncService.spec.ts` — 4 new tests: cancel real-branch `info { status, pid, hadChild: true }`;
  finalizeCancel entry + cleanup-success `info`; the previously-silent finalizeCancel catch `warn`; the abandonCancel
  `warn` floor. (Used `expect.objectContaining(...)` to sidestep a genuine prettier 2.1.2 non-idempotent oscillation.)
- `chainStorageCoordinator.spec.ts` — added a `./logging` mock (this spec had none) + 3 branch-decision assertions
  (join-timeout/forceKill escalation `warn`, unsettled → abandonCancel `warn`, settled → finalizeCancel `info`).

Scope note: added NO `detached` spawn options, NO `killProcessTree.ts`, NO `reapPartialSyncOnShutdown`; did NOT touch
`source/main/index.ts` or `safeExitWithCode.ts`; no status enum / IPC channel / i18n / user-facing copy / control-flow /
recovery-action-set / kill-switch / staged-only / latest-snapshot-only change. Two pre-existing prettier drifts (the
`normalizeSpawnEnv` `.reduce<>` wrap in the runner and an `as unknown as` cast in the runner spec) were confirmed
present at HEAD/index and deliberately left unreformatted to keep the change scoped.

Verification (Part-1 scope): `yarn compile` (tsc --noEmit) → PASS (0 errors). `yarn lint` → PASS (warnings only,
0 errors; none introduced). `yarn test:jest` on the three specs → all PASS (mithrilCommandRunner 17/17, service 55/55,
coordinator 41/41). `prettier --check`: the 4 non-runner touched files are clean and the CAT-E hunks in the runner +
runner spec are clean; the only prettier reds are the two pre-existing HEAD-baseline drifts outside every CAT-E hunk.
The Node v24 renderer verify-env caveat does not apply (main-process TS + jest; no `.scss.d.ts`). Per the doc: the real
defect (a pipe-holding descendant wedging `'close'`) is NOT unit-testable with a mocked EventEmitter child — jest proves
plumbing/logging only; the Linux operator gate remains the sole proof of the eventual Part-2/3 fix.

Decision: CAT-E Part 1 (logging) IMPLEMENTED + verified; committed=no (folds into the single 702d commit); Parts 2/3 +
operator gate pending; wave OPEN.

## Code Review

Timestamp: 2026-07-02T07:13:01Z
Speaker: Code Reviewer (CAT-E Part 1)

One-round adversarial review across the four lenses converged (no blocking findings): **(1) Scope-lock / non-behavioral**
— confirmed the diff is logging-only (312 insertions, 0 deletions; pure additions), the two new `child.on('exit', …)`
handlers are additive logging listeners, the runner resolve stays `'close'` and its resolve payload is un-truncated, and
none of the forbidden Part-2/3 surfaces (`detached`, `killProcessTree.ts`, `reapPartialSyncOnShutdown`, `index.ts`,
`safeExitWithCode.ts`, status/IPC/i18n/recovery-set/kill-switch) were touched. **(2) Side-effect-freeness** — every new
call uses a structured context object, cannot throw or reorder control flow, and logs no secrets. **(3) Invariant
preservation** — staged-only restore, Boundary-A-only cancel, the terminal `cancelled` recovery set, latest-snapshot-only,
and the kill-switch are all intact; CAT-A..D preserved and staged. **(4) Test honesty** — the added jest cases prove the
plumbing/logging only and do not pretend to prove the runtime process-tree fix (which the operator gate owns). One
non-blocking note (the pre-existing prettier drift on the runner/runner-spec) was correctly left out of scope.

Decision: approved.

## Implementation

Timestamp: 2026-07-02T11:29:03Z
Speaker: Implementer (CAT-E Parts 2A + 2B + 2C — slot-lifecycle fix, probe gating, defense-in-depth teardown)

Landed **CAT-E Parts 2A, 2B, and 2C** (the Part-2 mechanism selected by the resolved operator gate — hypothesis (iii)
slot-clobber primary) on the shared working tree, on top of the staged CAT-A..D + Part-1 work. **Part 3 is NOT
included** — it stays evidence-gated and is expected unnecessary now that 2A makes cancel kill the direct child so
`'close'` fires (the runner still resolves on `'close'`; no close→exit swap). **committed = no** — folds into the
single 702d commit.

- **2A (slot-lifecycle fix, PRIMARY)** — `MithrilPartialSyncService.ts`: a **service-local, non-exported** opt-in
  `trackAsCancelable` options field (`PartialSyncRunCommandOptions` / `PartialSyncMetadataReadOptions`),
  destructure-**stripped** in `_runCommand` before the spread into the runner options (never on `RunCommandOptions`,
  never inferred from `_activeWorkDir`); `onProcess` is registered **only** when `trackAsCancelable === true` while
  `onLogStream` is always kept. Threaded `true` from the download `_runCommand` and both `start()`-phase metadata
  reads (via `resolveLatestSnapshotMetadata` → `_showSnapshotRaw`/`_listSnapshotsRaw` — the B1 pin); the 30s
  behind-ness probe chain and ad-hoc `listSnapshots`/`showSnapshot` stay untracked. `_runBinary` is functionally
  unchanged (invariant comments only). Plus the **C2 unconditional cancel-entry log** —
  `logger.info('MithrilPartialSyncService: cancel entry', { status, pid ?? null, hadChild })` after the `cancelling`
  emit and **before** the `if (this._currentProcess)` gate, so operator re-repro #1 directly observes an empty slot
  (note: `status` reads `'cancelling'` post-emit, and the no-active-sync early return still precedes the line).
- **2B (probe gating)** — `MithrilPartialSyncStore._refreshAvailability` gains the skip-when-active early return
  (`isWorking || status === 'cancelled'`, the plan's comment block verbatim, placed before
  `_isRefreshingAvailability = true`); the `setup()` "re-fetch on EVERY tick" design comment updated in the same
  diff; `_armAvailabilityInterval` untouched. Belt-and-braces main-side guard in
  `MithrilController.getPartialSyncAvailability` — after the `!isEnabled` short-circuit, returns
  `{ isEnabled, isSignificantlyBehind: false }` while the `_partialSyncStatus` seam reports a working or `cancelled`
  status, before `getPartialSyncBehindness` is called (`isMithrilPartialSyncWorkingStatus` imported). Known nuance:
  the guarded return carries no `certifiedEpoch`, so a renderer poll racing the narrow pre-store-gate window can
  *degrade* the frozen Diagnostics figure to networkTip-only rather than strictly freeze it — within the documented
  accepted frozen/degraded-mid-run decision.
- **2C (defense-in-depth teardown)** — new `source/main/mithril/killProcessTree.ts` (POSIX
  `process.kill(-pid, signal)` group kill with `child.kill(signal)` fallback, fallback throw swallowed + warned;
  Windows split: async `exec('taskkill /pid <pid> /t /f')` at interactive sites, `execSync` with
  `{ stdio: 'ignore', timeout: 5000, windowsHide: true }` ONLY for the sync shutdown reap; null/no-pid no-op) + a
  9-test spec. `detached: !environment.isWindows` on **both** shared-runner spawns (piped stdio kept, no `unref()`,
  no tree-kill import). The three interactive service kill sites routed through `killProcessTree` (cancel SIGTERM,
  `forceKill` SIGKILL, late-kill SIGTERM). New `MithrilPartialSyncService.forceKillForShutdown()`
  (`killProcessTree(child, 'SIGKILL', { sync: true })`) + `MithrilController.reapPartialSyncOnShutdown()`
  (`isPartialSyncActive()`-gated, fully try/caught), called **exactly once** from `safeExit()` (`index.ts:95`)
  immediately after `pauseActiveDownloads()`. `MithrilBootstrapService` untouched; the Windows packaged-build
  `taskkill` PATH concern stays documented-not-implemented (manual PR-testing phase; branch ticket if it bites).

Tests — plan cases 1-10 all present across the six specs: the slot-clobber regression (held download child survives a
real behind-ness probe whose runner invocation is pinned to carry NO `onProcess`, keep `onLogStream`, and receive the
exact options object — proving no flag leak) + the converter variant; the B1 `start()`-phase pin; the cancel-entry log
shapes incl. the empty-slot `{ status: 'cancelling', pid: null, hadChild: false }` case; store gating cases 4-5
(35 tests, stock jest config — no Node-v24 scss sidecar needed); controller guard case 6 + reap case 7 (delegation +
swallowed throw; the `forceKillForShutdown` sync-SIGKILL seam pinned in the service spec after review iteration 1);
`killProcessTree` case 8 (9 tests); `detached` case 9 (4 tests, both spawns × POSIX/Windows); coordinator case 10
(settled → `finalizeCancel` + the new never-settles 15s-join → `forceKill` → 1s → `abandonCancel` end-to-end pin).
`MithrilController.spec.ts` is a NEW spec and needs a `./MithrilStartupGate` mock beyond the plan's three (the module
graph otherwise reaches electron's `dialog`); env quirk: `setImmediate` is undefined under this jest config — async
flushes use `setTimeout(0)` (see research finding #12).

Verification (independent gate, round 1/3 — GREEN): `yarn compile` (tsc `--noEmit`) PASS (precompile regenerated the
`.scss.d.ts` itself); `yarn lint` 0 errors (warnings match the project baseline); `prettier --check` clean on all
Part-2-authored files (sole excusals: the two documented pre-existing `mithrilCommandRunner.ts`/`.spec.ts` drift
regions, confirmed verbatim at HEAD and outside every authored hunk); scoped jest all green — service 59/59,
controller 8/8, killProcessTree 9/9, runner 21/21, coordinator 42/42, store 35/35 (**174 total**).

**Operator re-repro gates #1 and #2 are now PENDING** (the wave's proof gates): **#1** cancel-at-downloading — must
reach terminal `cancelled` with no floor, no live `mithril-client` (ps), staging dir removed; the unconditional
cancel-entry log turns the slot inference into a direct `hadChild` observation. **#2** quit-mid-download — no
`mithril-client` survives app exit (the leg the earlier ps.log could not prove). Residue for the commit stage to
keep/drop deliberately (flagged by review, outside CAT-E scope): an unstaged `.gitignore` `.devcontainer` hunk and
the untracked `.agent/scheduled_tasks.lock`.

Decision: CAT-E Parts 2A/2B/2C IMPLEMENTED + verified; Part 3 evidence-gated (expected unnecessary); committed=no
(folds into the single 702d commit); operator re-repros #1/#2 pending; wave OPEN.

## Code Review

Timestamp: 2026-07-02T11:29:03Z
Speaker: Code Reviewer (CAT-E Parts 2A/2B/2C)

Two-iteration adversarial diff-vs-plan review against the locked `task-ux-702d-cat-e.md`:

- **Iteration 1 — requires_changes.** The implementation itself verified correct on every binding invariant (2A
  service-local flag stripped before the runner spread + threaded at exactly the three plan sites, probe/ad-hoc reads
  untracked; 2B guard placements + verbatim comments; 2C detached on both spawns / `killProcessTree` Windows
  async-interactive vs sync-shutdown split / single `safeExit()` reap; no Part-3 creep — runner still resolves on
  `'close'`; `MithrilBootstrapService` untouched; C2 log placement exact). **Sole mustFix:** plan test case 7's first
  leg — the `forceKillForShutdown` → `killProcessTree(child, 'SIGKILL', { sync: true })` seam — was pinned nowhere
  (the controller spec class-mocks the service, and a spec comment falsely claimed the pin lived in the service spec);
  a coverage gap + misleading comment, not a code bug — if `{ sync: true }` were dropped or the signal downgraded,
  all tests would have stayed green. Non-blocking notes: the out-of-scope `.gitignore` `.devcontainer` hunk and the
  `.agent/scheduled_tasks.lock` orchestration residue.
- **Iteration 2 — approved.** The sync-SIGKILL seam is now pinned (`(child, 'SIGKILL', objectContaining({ sync:
  true }))` plus the swallow-throw test) and every invariant re-verified on the live tree; test cases 1-10 present
  and honest (the slot-clobber regression drives the REAL probe to completion mid-download and proves the slot
  survives; the B1 pin drives the REAL `start()`); independent gate re-run green (jest across the six specs, tsc
  clean, eslint 0 errors, prettier clean on all authored files — the runner drift correctly left alone).

## Operator validation & wave closure

Timestamp: 2026-07-02
Speaker: Operator (via session close-out)

**Re-repro #1 CONFIRMED on the live app:** cancelling out of the Mithril partial sync progress overlay now reaches
the terminal cancelled prompt without error — no "restart Daedalus" / "Downloading the Mithril snapshot failed"
floor — and every option on the cancel prompt works as expected. This was the last major issue spotted for the
session. Part 3 (exit-settle) stays unimplemented (evidence-gated; not needed on the observed behavior). Re-repro
#2 (quit-mid-download) was not separately re-run this session and is deferred to the `task-ux-702` manual
PR-testing/QA phase alongside the documented Windows/macOS teardown checks.

Decision: **task-ux-702d completed** (tasks JSON `v1.10.1`, `completedAt: 2026-07-02`); wave **CLOSED** (no CAT-F
appended); all 702d work committed in the single wave commit; the deployment QA gate `task-ux-702` is unblocked.

Decision: approved (after 2 iterations).