# task-ux-702d — Research notes (durable findings from planning and follow-up)

> Durable, reusable findings surfaced while planning and hardening `task-ux-702d`.
> Plan: `task-ux-702d.md`. Plan review: `task-ux-702d-plan-review.md`.
> Per-category implementation docs: `task-ux-702d-cat-a.md`..`task-ux-702d-cat-e.md` (decomposed 2026-07-01).

## 1. Child tracking must tolerate cancel winning before spawn registration

`mithrilCommandRunner.runCommand()` does async preparation (`buildMithrilEnv()`) before `spawn()`, and only then fires
`onProcess(child)`. A cancelable UI stage can therefore exist while `_currentProcess` is still `null`. The safe seam is
service-side registration: when a child finally registers, kill it immediately if `_isCancelled` is already true.

## 2. A live child process distinguishes backend-cancel failure from stale renderer state

If the UI shows the `abandonCancel()` failure prompt, check both `mithril-partial-sync.log` progression and `ps` for
`mithril-client` / `snapshot-converter`. Continued log progress or a live child means the backend missed a subprocess;
it is not just a renderer desync.

## 3. Reuse existing runner callbacks before widening lower-level surfaces

`mithrilCommandRunner.runBinary()` and `runCommand()` already expose `onProcess(child|null)`. Reusing that callback from
`MithrilPartialSyncService` avoided any `mithrilCommandRunner.ts` change while covering both download and conversion
paths with the same `_trackCurrentProcess(child)` helper.

## 4. Direct-pid `.kill()` does not reap a descendant — a process group does (CAT-E)

`mithrilCommandRunner.ts` spawns both binaries with `spawn(bin, args, { cwd, env })` — no `detached` (`:155`,
`:231-235`) — so the child shares Daedalus's process group and `process.kill(-pid)` is unavailable. All three kill
sites (`MithrilPartialSyncService.ts:346` cancel SIGTERM, `:391` forceKill SIGKILL, `:648` late-kill) signal only the
**direct child pid**. If `mithril-client` performs its work via a descendant, that descendant survives — matching the
live repro (active `mithril-client` after cancel and after app exit). To reap the tree: spawn `detached: true` so the
child leads its own group (pgid == pid on POSIX), then `process.kill(-pid, signal)`. This falsifies the CAT-D
assumption that mithril subprocesses are "single-process, harmless." **`detached` is POSIX-only** — on Windows it breaks
launcher tooling — "detaching breaks Windows launching the local-cluster.exe + cardano-launcher"
(documented at `CardanoSelfnodeLauncher.ts:112-115,254`); Windows already
reaps a tree via `taskkill /pid <pid> /t /f`. The in-repo precedent is `CardanoNode._killProcessWithName:1068-1115`
(POSIX `process.kill(pid)` / Windows `taskkill /t /f`) — its POSIX branch uses a *positive* pid (no group), which is
exactly the gap to close. No production tree-kill dependency exists (`tree-kill` is dev-transitive only); `find-process`
(prod dep, used by `source/main/utils/processes.ts`) is available for post-kill verification.

## 5. The coordinator settles on stdio-`'close'`, not process-`'exit'` (CAT-E)

The runner resolves its promise **only** on child `'close'` (`mithrilCommandRunner.ts:190,:265`); there is no `'exit'`
listener anywhere. `'close'` fires only after all stdio streams reach EOF, and a surviving descendant that inherited the
piped stdout/stderr keeps them open — so `'close'` (and therefore `_partialSyncRunPromise` / the coordinator's
"settled") can hang even after the direct child is signalled, expiring the ~16 s bounded join
(`chainStorageCoordinator.ts:43-44,473-492`) into `abandonCancel`. Key nuance: **killing the whole group (finding #4)
also closes the descendant's inherited pipe, so it makes `'close'` fire** — group-kill is expected to fix the settle as
a side effect. Any change that resolves the runner promise on `'exit'` instead of `'close'` risks truncating the
`stdout` that `resolveLatestSnapshotMetadata` (`snapshot list`/`show`) parses as JSON, so an exit-derived settle signal
must be *additive*, never a `close→exit` swap.

## 6. Nothing reaps the Mithril child on app quit (CAT-E)

`safeExit()` (`source/main/index.ts:87-129`) is the single shutdown funnel; it awaits only `cardanoNode.stop()` then
`process.exit()`s (`safeExitWithCode.ts:4-16`). No code touches the Mithril subprocess on quit — so it survives app
exit. Making the child `detached` *guarantees* it survives `process.exit()` (own group), so an explicit shutdown reap
becomes mandatory. The reap belongs in `safeExit()` right after `pauseActiveDownloads()`, using
`getMithrilController().isPartialSyncActive()` (`MithrilController.ts:199-204`) + the `forceKill` chain (`:449`); it must
be best-effort (try/caught) and must not block the exit.

## 7. Instrument the cancel path before changing behavior (CAT-E)

The cancel/kill/join/abandon path is near-silent: only three `warn`-**on-throw** branches
(`MithrilPartialSyncService.ts:349,:393,:650`) plus the spawn line log anything; `abandonCancel` (the exact hard
failure), the coordinator join/escalation/settled decision, and the runner's child `exit`/`close` are all unlogged.
Ship logging first and re-repro: the logs (child present? SIGTERM/SIGKILL reaching it? `'exit'` firing while `'close'`
hangs? surviving descendant in `ps`/`pstree`?) turn the next repro into a proof and decide whether the settle-semantics
change is needed at all. Logger levels/prefixes by file: `logger.info`/`warn` with `'MithrilPartialSyncService: …'`,
`'[MITHRIL] …'`/`'ChainStorageCoordinator: …'`, `'[mithril] …'`; the service spec's `../utils/logging` mock stubs only
`info`/`warn`, so a new `error`/`debug` line needs the mock extended.

## 8. Part-1 logging shipped as the diagnostic gate; operator gate RESOLVED — hypothesis (iii) slot-clobber confirmed (CAT-E, 2026-07-02)

CAT-E **Part 1 (cancel-lifecycle logging) is implemented** on the working tree (uncommitted; folds into the single 702d
commit) — non-behavioral, verified (compile/lint PASS, the three in-scope specs 17/55/41 PASS), reviewed and approved
(see `task-ux-702d-impl-review.md`, 2026-07-02T07:13:01Z). This is the deliberate realization of finding #7: instrument
before changing behavior.

**Operator gate RESOLVED (Linux/WSL2, cancel during `downloading`).** The re-repro captured decisive evidence
(`logs/ps.log`): a surviving `mithril-client` with **pid 229730, ppid 228282 (== Daedalus main), pgid 227499
(== Daedalus's pgid), sess 218848 (== Daedalus's sess)** — i.e. the **in-group direct download child** — while the
coordinator's 15s join expired into the `abandonCancel()` "restart Daedalus" floor. Read through the pre-fix
interpretation table, this **RESOLVED the Part-2 mechanism**:
- **(iii) `_currentProcess` slot-clobber is CONFIRMED** as the operative root cause (see finding #10 for the precise
  mechanism);
- **(ii) re-grouping (`setsid`/`setpgid`) is RULED OUT** by `ps` (the survivor is in Daedalus's own group/session, not
  re-parented/escaped);
- **(i)'s in-group process-group topology is present but was NOT the operative failure** — detached + group-kill alone
  would still miss a survivor whose naming slot was nulled before cancel ran.

**Part-2 mechanism SELECTED (plan-reviewed APPROVE-WITH-CHANGES, B1/B2 folded in):** **2A** slot-lifecycle fix (PRIMARY)
+ **2B** gate the behind-ness probe during active work + **2C** detached / `killProcessTree` / reap-on-quit
defense-in-depth; **Part 3** (child-`'exit'`-derived settle) stays evidence-gated and is expected NOT needed once 2A
makes cancel kill the direct child so `'close'` fires. **Parts 2 & 3 stay PLANNED, NOT implemented**; the ordered
operator re-repros (#1 after 2A, #2 after 2B+2C) remain the proof gates. See `task-ux-702d-cat-e.md` (RESOLVED box +
Implementation §2A/2B/2C).

**Addendum (2026-07-02T09:42:45Z, grounded plan review — four Opus exploration subagents vs the live post-Part-1 tree +
captured `logs/ps.log`, `logs/pub/Daedalus.json`, `logs/mithril-partial-sync.log`):** the triage above **stands**
((iii) supported, (ii) ruled out by `ps`, (i) present-but-not-operative); what is refined here is *how* (iii) is
supported and *what the `ps` capture does and does not prove.*
- **Evidence quality — the (iii) verdict is CODE-PLUS-LOG INFERENCE, not direct observation.** Part 1's landed
  cancel-entry log line sits inside the **real-kill branch** (`if (this._currentProcess)`), so an **empty** slot logs
  nothing; the captured repro contains **no** cancel-entry line and **no `hadChild` field anywhere**. The first
  cancel-related log is the coordinator join-timeout at `07:58:47.314Z`; the cancel time is *inferred*
  (timeout − 15 s ≈ `07:58:32`). Directly observed: download child **pid 229730** spawned `07:58:04.486Z` and appears
  **exactly once** in the whole `Daedalus.json` (no exit/close/kill line ever references it); probe child **pid 230124**
  (`snapshot show latest`) spawned `07:58:26.888` during `downloading` and closed cleanly `07:58:27.141`
  (→ `onProcess(null)`), ~5 s before the inferred cancel; **no kill line at cancel against any pid**; `abandonCancel`
  floor `07:58:48.316` (`settled:false`, stage `downloading`); the download was still advancing at `08:00:43`
  (2034/5845 files; 59 progress lines inside the cancel window). Post-cancel probes **230842/230843** (`07:58:55`)
  **were** SIGTERM'd by the late-arriving-child guard — proving the late-kill path works and isolating the *pre-cancel*
  slot lifecycle as the gap.
- **`ps` caveat — the snapshots prove survivor topology, NOT "survives app exit".** The `logs/ps.log` captures were
  taken while Daedalus was **still running** (Electron main 228282, launcher 228275, renderer 228377 all present), so
  they prove the abandoned-cancel survivor topology (pid 229730, ppid 228282, pgid 227499, sess 218848 — all matching
  this finding exactly), **not** the "survives app exit" half; that leg rests on the earlier operator report +
  reparenting reasoning and remains gated on operator **re-repro #2** (quit mid-download).
- **Remediation adopted into the CAT-E plan (2A):** an **unconditional** cancel-entry log `{ status, pid, hadChild }`
  placed **before** the `:345` gate, so operator **re-repro #1** *directly observes* the slot state (`hadChild:false`)
  rather than inferring it.

## 9. Reconciliation conflict: task-ux-702c is status=`pending` in the tasks JSON yet its code is committed (record, do not flip)

Noted during CAT-E Part-1 doc finalization: `task-ux-702c` is `status: "pending"` in
`mithril-partial-sync-ux-refinement-tasks.json`, but its code is already committed to the branch (commit `1364b685c`
"fix(mithril): task-ux-702c remediate manual-testing UX findings CAT-A-F"). **Resolution (prefer-live-repo):** the code
has landed, but the phase-7 tasks are `interactive_validation` — status intentionally stays `pending` until
operator-validated, so the JSON is NOT out of sync in a way that should be silently flipped. This is **recorded, not
acted on**: do not change 702c's status here. The same convention applies to 702d (Part 1 landed, status stays
pending/in-progress pending Parts 2/3 + the operator gate).

## 10. Root cause CONFIRMED: a single `_currentProcess` slot clobbered by the concurrent behind-ness probe (CAT-E, 2026-07-02)

The operative root cause of finding #5's "cancel leaves `mithril-client` running" is NOT a process-group/tree gap and
NOT `'close'`-vs-`'exit'`; it is a **`_currentProcess` slot-clobber**. There is one mutable slot `_currentProcess`
(`MithrilPartialSyncService.ts:116`). Both the cancelable download run AND the concurrent ~30s behind-ness metadata
probe (`snapshot show latest` / `snapshot list`) register through the same seam `_runCommand → onProcess →
_trackCurrentProcess`, whose `this._currentProcess = child` is **unconditional** (`:669-670`, wired for every
`_runCommand` caller at `:1221-1223`). `start()` (`:157`) and `cancel()` invalidate the behind-ness caches, so the
first availability poll during `downloading` is a guaranteed cache miss that spawns a metadata child; that child (a)
**overwrites** the download's slot on its `onProcess(child)`, then (b) **nulls** it on its own clean `'close'` via
`onProcess(null)` (`mithrilCommandRunner.ts:287` runCommand / `:201` runBinary). The download is thereafter untracked,
so the cancel SIGTERM gated on `if (this._currentProcess)` (`:345`, kill `:354`) sends nothing and `forceKill()`'s
`this._currentProcess?.kill('SIGKILL')` (`:414`) is a no-op → the download keeps running, its held-open stdio keeps the
runner's `'close'` from firing, `start()` never returns, the 15s coordinator join expires → `abandonCancel` floor; the
child also survives app exit.

**Fix (SELECTED):** an opt-in `trackAsCancelable` flag on `_runCommand` (default `false` → omit the `onProcess`
callback so metadata children never touch the slot), **threaded from the cancelable call sites** — the download
(`:591`), both `start()`-phase `resolveLatestSnapshotMetadata()` reads (`:186` preparing-resolve, `:224` latest-drift
re-resolve, plumbed through `_showSnapshotRaw`/`_listSnapshotsRaw`), and the conversion via `_runBinary` (already
conversion-only). It **MUST NOT be inferred from `this._activeWorkDir`** — the background probe's
`_getCachedLatestCertifiedImmutableNumber → resolveLatestSnapshotMetadata` read (`:837`) also runs while `_activeWorkDir`
is set during `downloading`, so inferring would re-introduce the exact clobber. Plus **gate the behind-ness probe OFF
during active work** (renderer `MithrilPartialSyncStore._refreshAvailability` + a belt-and-braces
`MithrilController.getPartialSyncAvailability` guard), which removes the concurrent metadata spawn entirely and also
eliminates its per-tick CPU cost during a run — cross-links the availability-probe CPU-cost note (702b CAT-H: the probe
forks `checkDiskSpace` + reads `immutable/` on each cache miss). Detached POSIX + `killProcessTree` + reap-on-quit are
layered defense-in-depth (2C), not the primary fix.

**Addendum (2026-07-02T09:42:45Z, grounded plan review):** the mechanism above is confirmed as **code-plus-log
inference** (see the finding #8 addendum for the full evidence rebuild), not a direct slot-emptiness observation — the
landed cancel-entry log cannot fire on an empty slot. Two figures are tightened by the captured timeline: the clobbering
probe (`pid 230124`) **overwrote then nulled** the slot at `07:58:26.888`→`.141`, ~5 s *before* the inferred cancel
(`≈07:58:32`), and it spawned ~22 s after the download started — so the **"~30 s" is the availability-polling cadence,
not the observed clobber offset**. The remediation folds the unconditional `{ status, pid, hadChild }` cancel-entry log
(finding #8 addendum) into 2A so operator re-repro #1 turns the inference into a direct observation of `hadChild:false`.

## 11. Sibling `MithrilBootstrapService` has the same slot-clobber class — separate branch ticket (CAT-E, 2026-07-02)

`MithrilBootstrapService` uses the same unconditional-`onProcess` slot seam shared by its own metadata reads and its
download, so the identical clobber exists for cancel-during-bootstrap. Per the ticketing model (locked decision 3) this
is a **separate branch-ticket follow-up**, NOT part of the CAT-E diff — recorded so it is not lost.

## 12. Main-process jest environment quirks discovered during CAT-E Part-2 test authorship (CAT-E, 2026-07-02)

Durable test-environment discoveries from landing the CAT-E Part-2 test suite (they will bite any future main-process
spec work on this branch):

- **`setImmediate` is undefined in this repo's jest environment.** Held-async flush loops in main-process specs must
  use block-bodied `await new Promise((resolve) => { setTimeout(resolve, 0); })` (or explicit `Promise.resolve()`
  microtask-flush loops), never `setImmediate`. The failure mode of an insufficient flush count is a hung `await`
  (test timeout), not a false pass.
- **`MithrilController.spec.ts` requires `jest.mock('./MithrilStartupGate')` in addition to the plan's three mocks**
  (`chainStorageCoordinator`, `logging`, `MithrilPartialSyncService`): without it the module graph reaches electron's
  `dialog` (via `mithrilPartialSyncNodeStartup`) and the IPC channel module, which cannot load under jest. The
  module-level `getMithrilController()` singleton constructs eagerly at import time (harmless with the mocks in
  place); tests should construct fresh `MithrilController` instances. The file's `beforeEach` uses
  `jest.clearAllMocks()` (implementations are NOT reset), so per-test throwing stubs must use
  `mockImplementationOnce`, not `mockImplementation`.
- **`MithrilPartialSyncStore.spec.ts` runs green under the stock `jest.config.js` on Node v24** — the anticipated
  typed-scss-modules/identity-obj-proxy sidecar (see the renderer verify-env memory) is only needed for renderer
  specs that import `.scss`; this store spec imports none, so no gitignored sidecar was (or should be) recreated
  for it.

## 13. Post-mortem: why cancel "worked" before 702c CAT-A — emergent error-swallowing, and strictness landed before settle-reliability (operator feedback, 2026-07-02)

Recorded from operator feedback during 702d validation ("cancel worked before all of this: it went straight to the
cancel prompt, re-prompted until processes settled, never entered a failure state, never required closing the app, and
always eventually offered retry-Mithril or start-standard-sync — why?"). No 702 doc previously synthesized this
post-mortem; the pieces live in D-702c-1 (`task-ux-702c-decisions.md:49-78`), the CAT-D problem statement
(`task-ux-702d-cat-d.md:17`), the CAT-E problem statement (`task-ux-702d-cat-e.md:24`), and findings #8/#10 here.
Grounded 2026-07-02 by a three-way review: historical-tree reconstruction at `3c1a0ab44` (pre-702c), the
`3c1a0ab44..1364b685c` (702c) diff, and the 702d working tree. Old-tree citations are `@ 3c1a0ab44`; current-tree
citations are working-tree lines as of 2026-07-02.

**The old flow's apparent reliability was emergent, not designed-in: failure from cancel was unrepresentable.**
1. **Fire-and-forget kill; terminal prompt emitted unconditionally.** `cancel()` sent a bare
   `this._currentProcess.kill()` (default SIGTERM — no wait, no escalation, no verification), ran staging cleanup,
   and emitted terminal `cancelled` + `['retry','restart-normal']` in the same call, regardless of whether the child
   had died (`MithrilPartialSyncService.ts:299-321 @ 3c1a0ab44`). The coordinator side was a lock-free one-line
   passthrough — no run-promise join, no timeout (`chainStorageCoordinator.ts:274-278 @ 3c1a0ab44`).
2. **Every late/side error was swallowed.** The killed download's eventual stage error hit `start()`'s catch and was
   discarded by `if (this._isCancelled) return;` (`:256-259 @ 3c1a0ab44` — the single most load-bearing swallow); a
   kill throw was warn-only (`:301-309`); a cleanup throw emitted `failed` **but still with**
   `['retry','restart-normal']` (`:322-337`). No cancel path could emit `allowedRecoveryActions: []`, so the
   Quit-only dead-end was structurally unreachable — both terminal outcomes carried the same recoverable two-button
   screen.
3. **The remembered "returns to the cancel prompt until things settle" was the D-702c-1 race itself, presenting
   benignly.** Terminal `cancelled` was emitted while the coordinator's `_partialSyncInProgress` was still `true`; a
   quick Retry was rejected ("already in progress") and the store silently swallowed the rejection
   (`MithrilPartialSyncStore.ts:395-397 @ 3c1a0ab44`), collapsing back to the cancelled prompt; waiting let the
   child's `'close'` fire, `start()` unwind, the flag clear — and Retry then succeeded. Time healed everything, so
   the flow read as "eventually lets me choose," never as broken.

**Same latent defects, different presentation.** The uninterruptible `start()` stage loop (CAT-D) and the
`_currentProcess` slot-clobber (finding #10) both predate 702c. Under the old design a missed kill was invisible: the
cancelled prompt appeared instantly, the orphaned download kept running or died later, its error was swallowed by the
`_isCancelled` guard, and in the worst case the cancel was silently ignored all the way to cutover (the Boundary-A
hole, `task-ux-702d-cat-d.md:17`). Under the post-702c design the identical defect surfaces as the bounded join
(15 s + forceKill + 1 s: `chainStorageCoordinator.ts:43-44,284-313,487-529`) expiring into `abandonCancel` —
`failed` with `allowedRecoveryActions: []` and "Restart Daedalus to continue safely"
(`MithrilPartialSyncService.ts:484-510`), rendered as the defensive Quit-only screen
(`MithrilPartialSyncOverlay.tsx:142-154`) — while `_partialSyncInProgress` stays `true` (the run never settled), so
retry/restart would be guard-rejected even if offered.

**The regression seam was sequencing, not the contract.** 702c CAT-A landed the strict contract — never advertise
Retry before it can succeed; never claim `cancelled` while work continues; bounded join with an explicit no-settle
floor — but not the mechanisms that make a genuine cancel actually settle: `start()` interruptibility (CAT-D
checkpoints CP-A..CP-D), reliable child tracking (CAT-E 2A `trackAsCancelable`), probe gating (2B), and group
kill/reap (2C). Strictness therefore converted latent, previously invisible defects into a routine user-visible
dead-end: the floor fired on genuine cancels because the run structurally could not settle.

**Durable lesson.** When replacing an optimistic, error-swallowing flow with a verify-then-report contract, land the
verification-passing mechanisms in the same change — or keep the failure floor recoverable (non-empty
`allowedRecoveryActions`) until they land. An `allowedRecoveryActions: []` terminal should be introduced last, not
first: a strict floor is only as good as the settle path feeding it.

**Why the old flow still had to change (it "worked" by hiding real defects):** Retry silently no-oped until a hidden
flag cleared (702c finding #1); a cancel could be silently ignored through cutover (Boundary-A safety hole); a live
`mithril-client` survived app exit (finding #6); staging cleanup raced a still-writing child. 702c fixed the honesty
problem first and thereby exposed the stop-reliability problems; 702d CAT-D/CAT-E fix stop-reliability. Post-702d the
`abandonCancel` floor remains by design (explicit CAT-D non-goal) and should fire only when teardown genuinely
outruns the join; operator re-repros #1/#2 remain the proof gates that the floor is now rare in practice.