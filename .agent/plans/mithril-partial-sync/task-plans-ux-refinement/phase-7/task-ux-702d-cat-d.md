# task-ux-702d-cat-d — cancel correctness: make the stage machine honor cancellation

> Per-category doc, decomposed from the canonical plan `task-ux-702d.md` (task-ux-702d = Mithril Sync UX
> finalization cleanup, ad-hoc wave). **Implementation record — as landed (2026-07-01, incl. the late-spawn
> follow-up hardening); operator cancel-flow validation CONFIRMED 2026-07-02 (with the CAT-E re-repro #1 —
> cancel lands on the cancelled prompt without error and all prompt options work; wave closed).**
> Parent task: `task-ux-702d`.
> Plan review: `task-ux-702d-plan-review.md`. Research: `task-ux-702d-research.md`. If this doc ever
> disagrees with live code, prefer live code and reconcile here.

## Sequencing / status position
Fourth landed CAT — the wave's first **correctness** category (CAT-A/B/C are presentation/copy). Landed in two
passes: the interruptible stage machine + converter-child tracking, then the late-spawn `_trackCurrentProcess(child)`
follow-up hardening. CAT-E builds directly on this CAT and lands strictly after it (shared service file + spec).

## Findings closed & decisions implemented
| Finding # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #4 | **Canceling a Mithril Sync does not stop the backend; the "Cleaning up…" frame is a veneer**: cancel shows the `cancelling` "Cleaning up…" frame, but the `start()` stage machine (`MithrilPartialSyncService.ts:164-278`) **never consults `_isCancelled` between stages** and `_downloadAndVerifyPartialSnapshot` re-emits `downloading` **unconditionally** (`:458-465`). A cancel that lands during `preparing` (before any subprocess exists — `cancel()` only kills a live `_currentProcess`, `:318-320`) doesn't stop anything: the machine re-spawns the download ("continues in background, seen by logs"), **overwrites** the `cancelling` frame (the "Mithril Sync screen pops back" over the sync page), can run all the way to **cutover/completion** (cancel silently ignored — a Boundary-A safety hole), and once a stage re-emits a non-`cancelling` status, `finalizeCancel` **no-ops** (`:329`) so the staging dir is **never cleaned**. The observed "Downloading the Mithril snapshot failed" prompt with **only** logs+quit is the `abandonCancel` no-settle floor (`:372-391`, `allowedRecoveryActions: []`, `stage = _cancelFallbackErrorStage`) firing because `start()` never unwinds within the bounded join. Distinct from 702c CAT-A, which built the `cancelling`/`finalizeCancel`/join machinery but left `start()`'s stage loop uninterruptible so cancel never reliably reaches the terminal. | **CAT-D** | High |

## Non-goals (CAT-D-specific, moved from the master Non-goals list)
- **CAT-D non-goals:** no new IPC channel, message shape, or status value (reuses the existing
  `cancel → cancelling → join → finalizeCancel → cancelled` machinery built by 702c CAT-A); no change to the terminal
  `cancelled` recovery set (`['retry','restart-normal']`, D-702a-2 — no wipe); no change to `abandonCancel`'s
  non-settle floor, the bounded-join timeouts, `forceKill`, `cancel()`'s kill body, or the coordinator's
  `cancelPartialSync` orchestration; **no change to `mithrilCommandRunner.ts`** — its spawn/kill mechanism and the
  direct-child SIGTERM→SIGKILL path are unchanged. The process-tracking hardening stays at the
  `MithrilPartialSyncService` call site: `_runBinary` now passes the runner's **already-supported** `onProcess`
  callback, and both `_runBinary` and `_runCommand` funnel child registration through `_trackCurrentProcess(child)` so a
  late-registered child is killed immediately when `_isCancelled` is already `true`. Process-**tree/group** termination
  (grandchildren of either binary) remains a deferred residual, not implemented (see Risks). No renderer, i18n,
  theme, SCSS, or Storybook change; **cancel stays
  Boundary-A-only** — `installing`/`finalizing` still throw in `cancel()` (`:298-302`), and CAT-D's last checkpoint sits
  *before* the `installing`/cutover marker write so a cancel can never abort a live cutover.

## Locked invariants this change must NOT break
(Full text of the master's Locked safety boundary #3 — including the substance of the former "Note on wave scope"
blockquote — plus boundary #4's CAT-D clause; the master keeps 2-3-line summaries pointing here.)
- **PRD status-contract & Boundary A/B/C model unchanged.** CAT-A/B/C are presentation-only. **CAT-D is a
  correctness fix** — mirroring how 702c CAT-A was the single correctness fix in its otherwise-presentation wave —
  that changes only `start()`'s internal control flow: it adds **no new status** (`cancelling`/`cancelled` already
  exist from 702c CAT-A), makes **no change to the PRD status contract or the Boundary A/B/C recovery model**,
  and keeps cancel **Boundary-A-only** (its last checkpoint sits before the `installing`/cutover marker; `cancel()`
  still throws for `installing`/`finalizing`). It makes the *already-locked* Boundary-A cancel contract
  (stop → clean staged artifacts → terminal `cancelled`) actually hold.
- Carry the 702-series invariants unchanged (Boundary-A-only cancel, terminal `cancelled` recovery set
  `['retry','restart-normal']` with no wipe, kill switch #1/#2, staged-only #3, latest-snapshot-only #5, no bootstrap
  regression #7) — CAT-D touches none of them (it *enforces* the Boundary-A-only cancel invariant that was being
  silently violated by the uninterruptible stage machine).
- **Vocab guardrail #8:** CAT-D adds no user-facing copy (main-process control-flow only).

## Exact files (full repo-relative paths)
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts` (the **required** coordinator-level assertion; the coordinator
  source itself is untouched by CAT-D)

Implementable from this section alone. Main-process control-flow + tests only; **no new status, no PRD status-contract
change, no Boundary-model change, no renderer/i18n/theme change.**

### Rationale (root cause, verified against the live post-702c tree, 2026-07-01)
702c CAT-A built the cancel *machinery* — a `cancelling` "Cleaning up…" working-status, a coordinator-owned
`_partialSyncRunPromise` join, a guarded `finalizeCancel()` that owns cleanup + the terminal emit, `forceKill()`
(SIGKILL escalation), and a bounded-join `abandonCancel()` no-settle floor. That machinery is correct **only if the
in-flight `start()` promise actually unwinds when a cancel is requested.** It does not, because
`MithrilPartialSyncService.start()`'s stage machine (`:164-278`) is **uninterruptible**: it never consults
`_isCancelled` between its sequential `await`s, and `_downloadAndVerifyPartialSnapshot` re-emits `downloading`
**unconditionally** at its top (`:458-465`). The consequences, all reported by the operator:

1. **Cancel during `preparing` stops nothing.** `cancel()` only kills a subprocess that already exists
   (`if (this._currentProcess) this._currentProcess.kill()`, `:318-320`). During `preparing` no download process has
   spawned yet, so cancel merely flips `_isCancelled = true` and emits `cancelling`. `start()` then proceeds into
   `_downloadAndVerifyPartialSnapshot`, which **re-emits `downloading`** (overwriting the "Cleaning up…" frame — the
   *"Mithril Sync screen pops back over to the mithril sync page"*) and **spawns the download** (*"continues the
   Mithril process in the background, seen by logs"*).
2. **It can run to cutover.** With no `_isCancelled` gate before `_convertStagedSnapshot` (`:237`),
   `_installValidatedStagedSnapshot` (`:248`), or the `cutover-in-progress` marker write (`:244`), a download that
   finishes inside the ~16 s bounded join proceeds through `verifying → converting → installing` — emptying the live
   chain *after the user asked to cancel*. This is a **Boundary-A safety hole**, not just a UX defect.
3. **The staging dir is never cleaned.** Once any later stage re-emits a non-`cancelling` status, `finalizeCancel`'s
   `if (this._status.status !== 'cancelling') return;` guard (`:329`) makes it a **no-op**, so
   `_cleanupPartialSyncArtifacts()` (`:641-644`, `fs.remove` of the `mithril-partial-sync` staging root + clear marker)
   never runs → *"leaves the mithril partial sync files lingering on their drive."*
4. **The "Downloading the Mithril snapshot failed" prompt (logs + quit only) is the `abandonCancel` floor.** When
   `start()` never unwinds within the bounded join (`PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS = 15_000` +
   `forceKill` + 1 s, `chainStorageCoordinator.ts:43-44,473-492`), the join returns `settled = false` and the
   coordinator calls `abandonCancel()` (`:372-391`), which emits terminal `failed` with `allowedRecoveryActions: []`
   and `stage = _cancelFallbackErrorStage` (captured at cancel entry — `'downloading'` if cancelled mid-download).
   `partialSyncErrorCopy.ts` maps `stage: 'downloading'` to the **"Downloading the Mithril snapshot failed"** title,
   and empty actions collapse the error view to the defensive **Quit** button + the log-path link
   (`MithrilPartialSyncOverlay.tsx:142-154`) — exactly *"only a choice to look at logs and quit."*

Follow-up manual retest after the first CAT-D landing falsified one remaining assumption: the first cancel succeeded,
but restarting Mithril Partial Sync and canceling again could still surface the same failure prompt. Live runtime
inspection (`mithril-partial-sync.log` still advancing plus `ps` showing an active `mithril-client`) proved this was
not a stale renderer state; the backend had missed a child that was still alive.

The root-cause fix has **three complementary parts**:

**Part 1 — make the stage machine interruptible** (checks between stages). Check `_isCancelled` at each `await`
boundary in `start()` (and at the top of `_downloadAndVerifyPartialSnapshot`, before the unconditional `downloading`
emit). Then a requested cancel makes `start()` throw a cancellation sentinel that its existing catch already swallows
(`if (this._isCancelled) return;`, `:262-264` — no `failed` emit), so:
- the `downloading` frame is **never re-emitted** and the download is **never re-spawned** (fixes the pop-back + the
  background process),
- the flow **never reaches `installing`/cutover** after cancel — **for every stage, including `converting`** (CP-D
  fires *after* conversion returns and *before* the install marker, so this closes the safety hole regardless of
  whether the converter was killable),
- for `preparing` / `downloading` / `verifying`, `start()` unwinds **promptly while status is still `cancelling`**, so
  the coordinator join settles → `finalizeCancel` runs (`status === 'cancelling'` still true) →
  `_cleanupPartialSyncArtifacts()` removes the staging dir + clears the marker (fixes lingering files) → terminal
  `cancelled` with `['retry','restart-normal']`.

**Part 2 — make the `converting` stage promptly interruptible** (kill the in-flight conversion child). Part 1's
checkpoints unwind cancellation only *between* awaits. The `converting` stage is an **awaited** `snapshot-converter`
subprocess (`_convertStagedSnapshot` → `_runBinary`, `:551-595`); CP-D sits *after* that await, so on its own it only
takes effect once the conversion **returns**. The converter child is not tracked in the cancelable slot: only the
download/metadata path wires `onProcess → this._currentProcess = child` (`_runCommand`, `:1113-1114`), while `_runBinary`
passes **no** callbacks (`:587-594`), so during `converting` `_currentProcess` is `null` and both `cancel()`'s `.kill()`
(`:318-320`) and `forceKill()`'s `.kill('SIGKILL')` (`:364`) are no-ops. A real LSM conversion that outruns the ~16 s
bounded run-join (`PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS = 15_000` + `forceKill` + 1 s,
`chainStorageCoordinator.ts:43-44,473-492`) would therefore block `start()` past the join → `settled = false` →
`abandonCancel()` — the exact **"Downloading the Mithril snapshot failed / logs+quit"** floor CAT-D promises to
eliminate. The fix: wire `_runBinary` to pass the runner's already-supported `onProcess` callback (mirroring
`_runCommand`) so the conversion child lands in `_currentProcess` and the *existing* `cancel()`/`forceKill()` kill path
terminates it in-flight. The killed converter makes `runBinary` resolve with a non-success exit code →
`convertSnapshotDbToLsm` throws a `converting` stage error → `start()`'s catch swallows it under `_isCancelled` → the
run settles well within the join → `finalizeCancel` (not `abandonCancel`). Part 1's CP-D remains as defense-in-depth for
the narrow window where a conversion completes successfully just as the kill lands.

**Part 3 — close the late-spawn child-registration race** (kill on registration if cancel already won). Parts 1 and 2
ensure `start()` can unwind once cancel reaches a live child or a checkpoint, but the download path still has a
pre-spawn async window: `runCommand()` awaits `buildMithrilEnv(requireKeys)` before it calls `spawn()`, and only then
fires `callbacks?.onProcess(child)`. A cancel that lands in that window sees `_currentProcess = null`, so
`cancel()`/`forceKill()` have nothing to signal; when the child registers a moment later it keeps running, and the
coordinator can still time out into `abandonCancel()` while the live `mithril-client` continues in the background. The
same registration race can theoretically hit the conversion child too. The fix is service-side, not runner-side:
funnel both `_runCommand` and `_runBinary` through a shared `_trackCurrentProcess(child)` helper that sets
`_currentProcess` and, if `_isCancelled` is already `true`, immediately `child.kill()`. That makes a late-registered
child self-terminate as soon as it appears, so the run still settles into `finalizeCancel()` instead of the failure
floor.

> **Why Part 2 needs no `mithrilCommandRunner.ts` change (verified 2026-07-01):** `runBinary` already accepts a
> `callbacks` arg and fires `onProcess(child)` on spawn (`:157`) and `onProcess(null)` on close/error (`:185,:191`) —
> the same contract `runCommand` exposes and `_runCommand` already consumes. So Part 2 is a **call-site-only** change in
> `_runBinary`; the runner file, `cancel()`'s kill body, and `forceKill()` are untouched.

This still mirrors CAT-A/B/C's philosophy (smallest change at the places the defect actually lives): the defect is the
**missing cancellation checks in `start()`**, the **untracked conversion child**, and the **late-registration race at the
tracked slot**, so all three are fixed and nothing more. No new status, channel, message, or Boundary-model change;
the 702c machinery is otherwise unchanged.

### Reference details (verified against the live post-702c tree, 2026-07-01)
- **File:** `source/main/mithril/MithrilPartialSyncService.ts`.
- **Cancel flag:** `_isCancelled` declared `:110`; reset to `false` in `start()` at `:158`; set to `true` in `cancel()`
  at `:305` (synchronously, before the `.kill()` at `:319`).
- **`start()` stage machine (the seam):** `:164-278`. Ordered `await`s with **no** interleaved cancellation check —
  `resolveLatestSnapshotMetadata` (`:175`), `resolveLocalImmutableNumber` (`:178`), `_prepareStagingDirectory`
  (`:187`), `_assertSufficientDiskSpace` (`:191`), latest-drift re-resolve (`:209`), `_downloadAndVerifyPartialSnapshot`
  (`:223`), `validateStagedDownloadOutput` (`:227`), `_convertStagedSnapshot` (`:237`), `validateConvertedStagedOutput`
  (`:238`), `_activatePostVerificationStage('installing')` (`:243`), marker write (`:244`),
  `_installValidatedStagedSnapshot` (`:248`).
- **Existing catch (unchanged):** `:261-278` — `if (this._isCancelled) return;` (`:262-264`) already suppresses the
  `failed` emit and the finally at `:277` runs `_clearRuntimeWorkState()`.
- **Unconditional `downloading` re-emit (must be gated):** `_downloadAndVerifyPartialSnapshot` top,
  `_activateProgressStage('downloading')` + `_updateStatus({ status: 'downloading', … })` at `:458-465`. (Note:
  `applyProgressUpdate` already guards `if (this._isCancelled) return;` at `:483`, so only the *initial* emit is
  ungated.)
- **cancel() Boundary-A guard (unchanged):** `if (['installing','finalizing'].includes(this._status.status)) throw …`
  (`:298-302`).
- **Coordinator join (unchanged):** `cancelPartialSync` (`chainStorageCoordinator.ts:284-300`) →
  `_awaitRunSettledBounded` (`:473-492`); settled → `finalizeCancel()`, no-settle → `abandonCancel()`.
- **Cleanup (unchanged):** `finalizeCancel()` guard `:329`, `_cleanupPartialSyncArtifacts()` `:641-644`.
- **Error class precedent:** `MithrilPartialSyncStageError` (`:86-100`) — add the sibling cancellation sentinel here.
- **Converter tracking (Part 2):**
  - **Awaited conversion spawner:** `_convertStagedSnapshot` (`:551-571`) passes `runBinary: (n, a) => this._runBinary(n, a)`
    (`:557`); `_runBinary` (`:582-595`) currently calls `runBinary(binaryName, args, workDir, { logFileName })` with **no**
    `callbacks` arg (`:587-594`).
  - **Tracked-slot precedent (to mirror):** `_runCommand` (`:1100-1121`) already wires
    `onProcess: (child) => { this._currentProcess = child; }` (`:1113-1114`) — this is the *only* site that populates the
    cancelable slot today.
  - **Runner already supports it (no change needed):** `runBinary(binaryName, args, workDir, options, callbacks?)`
    (`mithrilCommandRunner.ts:122-128`) invokes `callbacks?.onProcess(child)` on spawn (`:157`) and
    `callbacks?.onProcess(null)` on `error`/`close` (`:185,:191`).
  - **Kill path (unchanged, now reaches the converter):** `cancel()` `this._currentProcess.kill()` (`:318-320`);
    `forceKill()` `this._currentProcess?.kill('SIGKILL')` (`:362-370`). `_currentProcess` is nulled by
    `_clearRuntimeWorkState` (`:665`) and by the runner's `onProcess(null)` on close.
- **Late-spawn window (Part 3):** `mithrilCommandRunner.runCommand()` awaits `buildMithrilEnv(requireKeys)` before
  `spawn()` (`mithrilCommandRunner.ts:208-232`) and only then fires `callbacks?.onProcess(child)` (`:237`). A cancel can
  therefore win while `_currentProcess` is still `null`, even though the next child has not yet registered.
- **Service-side hardening seam (Part 3):** `_trackCurrentProcess(child)` in `MithrilPartialSyncService.ts`
  (`:639-654` in the landed code) now owns child registration for both `_runBinary` and `_runCommand`, setting
  `_currentProcess` and immediately killing a late-registered child when `_isCancelled` is already true.

### Implementation (ordered, mechanical, small-model-implementable)
1. **Add a cancellation sentinel class** beside `MithrilPartialSyncStageError` (after `:100`):
   ```ts
   class MithrilPartialSyncCancelledError extends Error {
     constructor() {
       super('Mithril partial sync was cancelled.');
       this.name = 'MithrilPartialSyncCancelledError';
     }
   }
   ```
2. **Add a private guard method** on `MithrilPartialSyncService` (place near `_clearRuntimeWorkState`, `:663`):
   ```ts
   _throwIfCancelled(): void {
     if (this._isCancelled) {
       throw new MithrilPartialSyncCancelledError();
     }
   }
   ```
3. **Insert `this._throwIfCancelled();` checkpoints** at these four seams (all inside the try/catch so the existing
   `_isCancelled` catch swallows them):
   - **CP-A** — in `start()` immediately **after** `await this._assertSufficientDiskSpace(...)` (`:191`), before the
     latest-drift re-resolve / download. Covers a cancel issued during `preparing`.
   - **CP-B** — at the **top** of `_downloadAndVerifyPartialSnapshot`, **before** `_activateProgressStage('downloading')`
     (before `:458`). *Key: prevents the `downloading` re-emit + the download re-spawn (the pop-back + background
     process).*
   - **CP-C** — in `start()` immediately **after** `await validateStagedDownloadOutput(...)` (`:227-229`), before the
     `verifying` emit (`:230-234`). Covers a cancel during `downloading`/`verifying`.
   - **CP-D** — in `start()` immediately **before** `this._activatePostVerificationStage('installing')` (`:243`),
     i.e. after `_convertStagedSnapshot` + `validateConvertedStagedOutput`. *Key safety gate: never write the
     `cutover-in-progress` marker or install after a cancel* — this holds for a cancel during `converting` **regardless
     of process tracking** (it fires once conversion returns). *Prompt* interruption of an in-flight conversion comes
     from **Part 2** (step 4): the killed converter makes conversion return/throw immediately, so `start()` settles
     within the bounded join instead of blocking to `abandonCancel`. CP-D then acts as defense-in-depth for the narrow
     window where conversion completes successfully just as the kill lands.
4. **Route child registration through a shared `_trackCurrentProcess(child)` helper (Part 2 + Part 3).** Add a private
   helper that sets `_currentProcess = child` and, when `child && this._isCancelled`, immediately `child.kill()`.
   Then use that helper from both `_runBinary` and `_runCommand` instead of assigning `_currentProcess` inline. In
   `_runBinary` (`:587-594`), add the runner's fifth `callbacks` argument, mirroring `_runCommand` (`:1112-1119`):
   ```ts
   _trackCurrentProcess(child: ChildProcess | null): void {
     this._currentProcess = child;

     if (child && this._isCancelled) {
       child.kill();
     }
   }

   return runBinary(
     binaryName,
     args,
     this._activeWorkDir || stateDirectoryPath,
     { logFileName: PARTIAL_SYNC_LOG_FILE_NAME },
     {
       onProcess: (child) => {
         this._trackCurrentProcess(child);
       },
     }
   );
   ```
   `mithrilCommandRunner.ts` already accepts and fires `onProcess` (`:157,:185,:191`), and `cancel()`/`forceKill()`
   already kill `_currentProcess`. Once both call sites share `_trackCurrentProcess(child)`, a cancel during
   `converting` kills the tracked converter in-flight, and a late-registered download/converter child self-terminates
   immediately if cancel already won the race. `_runCommand` keeps its existing `onLogStream` callback unchanged.
5. **Do not** change `start()`'s catch (`:261-278`), `cancel()`'s kill body, `finalizeCancel()`, `abandonCancel()`,
   `forceKill()`, the coordinator, `mithrilCommandRunner.ts`, or any status/type. The catch's existing
   `if (this._isCancelled) return;` already handles both `MithrilPartialSyncCancelledError` (from a checkpoint) and the
   `converting` stage error thrown when the killed converter exits non-zero — neither reaches the `failed` emit.

Diff shape: **1 source file** (`MithrilPartialSyncService.ts`) — one new class, two new methods
(`_throwIfCancelled`, `_trackCurrentProcess`), four one-line checkpoint inserts, and the shared child-registration
helper used by `_runCommand`/`_runBinary` — plus its spec. No other production file changes (the runner is untouched).

### Why this is safe / invariant-preserving
- **Boundary-A-only cancel holds.** CP-D is the last checkpoint and sits *before* the `installing` marker/cutover;
  once `installing`/`finalizing` is entered, `cancel()` already throws (`:298-302`). CAT-D never lets a cancel abort a
  live cutover.
- **Success path unaffected.** When `_isCancelled` is `false`, every checkpoint is a no-op; no status, timing, or
  ordering changes on a normal run.
- **Bootstrap flow untouched.** `MithrilBootstrapService` is a separate class; CAT-D edits only
  `MithrilPartialSyncService`.
- **Terminal `cancelled` recovery set unchanged** (`['retry','restart-normal']`); `finalizeCancel` already emits it.
- **Killing the converter mid-run is staged-only-safe (Part 2).** `snapshot-converter` writes only into the ephemeral
  `mithril-partial-sync` staging dir — never the live chain (staged-only invariant #3). `finalizeCancel` →
  `_cleanupPartialSyncArtifacts()` `fs.remove`s that dir wholesale, so a partially-written LSM output from a killed
  conversion is wiped, not left corrupting anything, and `_installValidatedStagedSnapshot` is never reached (both CP-D
  and the killed-conversion `converting` stage error route to the catch/return *before* the install marker).
- **Part 2 + Part 3 are inert on the success path.** When no cancel is issued, `_trackCurrentProcess(child)` is just the
  same slot assignment that the download path already performed inline; it never calls `child.kill()` unless
  `_isCancelled` is already true. The runner still nulls the slot on `close` (`:191`) and `_clearRuntimeWorkState`
  clears it in the `finally` (`:277,:665`). No status, timing, or ordering change on a normal run.
- **Precision on "never re-spawns" (plan-critique note):** CP-B fully gates the safety-critical **download** spawn
  (`_runCommand` at `:528`, well after CP-B), so the download is never re-spawned and the `downloading` frame is never
  re-emitted after cancel. One narrow exception is *inside* `resolveLatestSnapshotMetadata` (`:688-701`), which has no
  interior checkpoint: if a cancel lands mid-metadata and the killed `show latest` returns empty, it may fall through
  to one extra short-lived, **read-only** `mithril-client snapshot list` subprocess. This is low severity — it mutates
  nothing, settles fast (so the coordinator join still settles → `finalizeCancel`, not `abandonCancel`), and never
  touches the live chain. Gating it is **optional hardening**, not required for correctness; left out to keep the diff
  minimal. (If ever wanted: add `this._throwIfCancelled()` at the top of `resolveLatestSnapshotMetadata`, or between
  its `show`/`list` fallback.)

### Tests (add to `source/main/mithril/MithrilPartialSyncService.spec.ts`)
Drive `start()` with the stage helpers stubbed (existing spec already stubs `resolveLatestSnapshotMetadata`,
`resolveLocalImmutableNumber`, `_prepareStagingDirectory`, `_assertSufficientDiskSpace`,
`_downloadAndVerifyPartialSnapshot`, `_convertStagedSnapshot`, `_installValidatedStagedSnapshot`, etc.):
1. **Cancel during `preparing` → no re-spawn, no cutover.** Make an early stub (e.g. `_assertSufficientDiskSpace`)
   flip `_isCancelled = true` (simulating a concurrent `cancel()`), then `await start()`. Assert: `start()` resolves
   (does **not** throw out); `_downloadAndVerifyPartialSnapshot` was **not** called; `_installValidatedStagedSnapshot`
   was **not** called; no `writeMithrilPartialSyncMarker('cutover-in-progress', …)`; and the last emitted status is
   **not** `failed`/`downloading` (i.e. no `failed` emit from the catch — `_isCancelled` short-circuits it).
2. **Cancel before install → no cutover.** Flip `_isCancelled` in the `_convertStagedSnapshot` stub; assert
   `_installValidatedStagedSnapshot` is not called and no cutover marker is written.
3. **`_throwIfCancelled` unit:** throws `MithrilPartialSyncCancelledError` when `_isCancelled` is `true`, no-op when
   `false`.
4. **Preserve the 702c cancel-sequence expectations** (`cancel()` emits `cancelling`; `finalizeCancel()` emits
   `cancelled` + runs `_cleanupPartialSyncArtifacts`; the no-op cancel branch is unaffected) — do not regress them.
5. **Converter child is tracked (Part 2).** Mock the imported `runBinary` so the test can inspect the `callbacks` arg
   `_runBinary` now passes: invoke `callbacks.onProcess(fakeChild)` and assert `service._currentProcess === fakeChild`,
   then invoke `callbacks.onProcess(null)` and assert it is cleared. Proves `_runBinary` wires the cancelable slot the
   same way `_runCommand` does, so `cancel()`/`forceKill()` can reach an in-flight conversion.
6. **Late-spawn download child is killed immediately when cancel already won (Part 3).** Capture the `callbacks` arg
  from the mocked `runCommand`, set `_isCancelled = true`, invoke `callbacks.onProcess(fakeChild)`, and assert
  `service._currentProcess === fakeChild` plus `fakeChild.kill()` called once. This covers the exact
  `buildMithrilEnv()`-before-`spawn()` race seen in the follow-up manual retest.
7. **Late-spawn converter child is killed immediately when cancel already won (Part 3).** Mirror test #6 with the
  mocked `runBinary` path, proving the same helper closes the registration race for conversion too.
8. **Cancel during `converting` settles to `finalizeCancel`, not `abandonCancel` (the finding's required case).**
   Simulate a mid-conversion cancel: make the `_convertStagedSnapshot` stub (or the `runBinary` mock it drives) set
   `_isCancelled = true` **and** throw a `converting` stage error (modeling the killed converter exiting non-zero), then
   `await start()`. Assert: `start()` resolves (the `_isCancelled` catch swallows the stage error — **no `failed`
   emit**), `_installValidatedStagedSnapshot` was **not** called, and no `writeMithrilPartialSyncMarker('cutover-in-progress', …)`.
**Required coordinator-level assertion** (`chainStorageCoordinator.spec.ts`): after a cancel issued during `converting`
whose `start`/run promise settles quickly (converter killed), assert the run **settles within the bounded join** and
`finalizeCancel` — **not** `abandonCancel` — is invoked. (Also add/keep the analogous cancel-during-`preparing` settles →
`finalizeCancel` assertion.) This is the specific coverage gap the code-review finding flagged; it is no longer optional.

### i18n
- None (main-process control-flow only; vocab-neutral — no user-facing copy added or changed).

### Verification / checks
- `yarn compile` + `yarn lint` + touched-file `yarn prettier:check` green; `yarn test:jest` for
  `MithrilPartialSyncService.spec.ts` (+ the **required** `chainStorageCoordinator.spec.ts` coordinator assertion)
  green. Node v24 renderer verify-env caveat does **not** apply (main-process TS + jest, no `.scss.d.ts`).
- **Operator (verify-only, preprod/Linux — the load-bearing proof):** start a Mithril Sync; click **Cancel** during
  `preparing`, then repeat during `downloading` and during `converting`. Each time confirm: the UI stays on
  "Cleaning up…" then lands on the **terminal cancelled** prompt (Retry / Standard Sync), the sync process does **not**
  reappear or continue (check logs), the `mithril-partial-sync` staging directory is **gone** after the cancelled
  prompt appears, and the "Downloading the Mithril snapshot failed / logs+quit" prompt does **not** appear. Record the
  observed teardown time (feeds 702c U1); if a real download ever fails to die within the bounded join and the
  `abandonCancel` floor is hit, capture logs — that would indicate a process-tree-termination follow-up (residual
  below) is warranted.

## Risks (category-specific)
- Process-tree/group termination residual — promoted to CAT-E (see `task-ux-702d-cat-e.md`, Risks); the startup
  orphaned-staging sweep deferral is also tracked there.

## Operator / verify-only gates
(Moved from the master's wave-level Verification plan.)
- **CAT-D (automated):** `yarn compile` + touched-file lint/prettier + `yarn test:jest` for
  `MithrilPartialSyncService.spec.ts` (+ the **required** `chainStorageCoordinator.spec.ts` coordinator assertion)
  green. **CAT-D (operator, verify-only):** start a Mithril Sync and click **Cancel** during `preparing`, then
  `downloading`, then `converting`; each time the overlay goes "Cleaning up…" → terminal **cancelled** prompt, the
  sync does not reappear/continue (logs), the `mithril-partial-sync` staging dir is removed, and no "Downloading the
  Mithril snapshot failed / logs+quit" prompt appears.

## Cross-category coupling notes
- CAT-E extends this CAT's kill sites (`cancel()` / `forceKill()` / `_trackCurrentProcess`) and shares
  `MithrilPartialSyncService.ts`/`.spec.ts` — CAT-E lands strictly after CAT-D.
- Source comments referencing `task-ux-702d CAT-D` (service + both specs) resolve to this doc.
- Verify-command note (702b-style): CAT-D is main-process TS + jest, so the Node v24 renderer verify-env caveat does
  **not** apply (no `.scss.d.ts`).
