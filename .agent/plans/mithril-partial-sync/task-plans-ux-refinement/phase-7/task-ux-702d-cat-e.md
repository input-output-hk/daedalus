# task-ux-702d-cat-e — cancel teardown: terminate the process group, reap on quit, and log the cancel lifecycle

> Per-category doc, decomposed from the canonical plan `task-ux-702d.md` (task-ux-702d = Mithril Sync UX
> finalization cleanup, ad-hoc wave). **Self-contained — implementable from this doc alone. Part 1 (logging) is
> LANDED; its re-repro RESOLVED the Part-2 mechanism (hypothesis (iii) `_currentProcess` slot-clobber — see the
> RESOLVED box in the Rationale). Parts 2/3 are re-scoped as 2A (slot fix, PRIMARY) → 2B (probe gating) → 2C
> (detached/`killProcessTree`/reap defense-in-depth) and remain PLANNED-not-implemented; implement 2A → operator
> re-repro #1 → 2B+2C → re-repro #2 → Part 3 only on a `'close'` hang.** Parent task: `task-ux-702d`. Plan review:
> `task-ux-702d-plan-review.md`. Research: `task-ux-702d-research.md`. If this doc ever disagrees with live code, prefer
> live code and reconcile here.

## Sequencing / status position
Fifth CAT of the wave; the only one not yet fully implemented (Part 1 logging LANDED; Parts 2/3 are PLANNED-not-implemented).
Lands strictly **after** CAT-D (shared `MithrilPartialSyncService.ts`/`.spec.ts` — see `task-ux-702d-cat-d.md`). Internal
order: Part 1 (logging) shipped first and was re-repro'd once; the evidence recorded in `task-ux-702d-research.md`
**RESOLVED the Part-2 mechanism** — hypothesis **(iii) `_currentProcess` slot-clobber** is CONFIRMED as the operative
root cause (see Rationale + Risks) — **and gates Part 3**. Part 2 is now re-scoped, ordered by dependency, into **2A
(slot-lifecycle fix, PRIMARY / load-bearing / unit-proven) → 2B (gate the behind-ness availability probe during active
work) → 2C (defense-in-depth: detached POSIX + `killProcessTree` + reap-on-quit)**.

## Findings closed & decisions implemented
| Finding # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #5 | **A genuine cancel leaves `mithril-client` running and fails cleanup on the first attempt**: once CAT-D made cancel effective, the first cancel reliably kills the *direct* download child but a surviving descendant is never reaped (`mithrilCommandRunner.ts` spawns with no `detached`, `:155,:231-235`; kills are direct-pid only, `MithrilPartialSyncService.ts:346,:391,:648`), and the coordinator settles on the runner's `'close'` (stdio-EOF, `:190,:265`) not child `'exit'`, so the held-open pipe keeps `run` pending until the ~16 s bounded join (`chainStorageCoordinator.ts:43-44,473-492`) expires → `abandonCancel` "restart Daedalus" floor while the download continues; `safeExit()` (`index.ts:87-129`) reaps only `cardanoNode`, so it also survives app exit. The cancel path is near-silent, blocking diagnosis. This is the CAT-D process-tree/group residual's trigger condition materializing. | **CAT-E** | High |

## Locked invariants this change must NOT break
(Full text of the master's Locked safety boundary #4 CAT-E clause; the master keeps a 2-3-line summary pointing
here.)
- Carry the 702-series invariants unchanged (Boundary-A-only cancel, terminal `cancelled` recovery set
  `['retry','restart-normal']` with no wipe, kill switch #1/#2, staged-only #3, latest-snapshot-only #5, no bootstrap
  regression #7). **CAT-E touches none of them:** it hardens only *process termination* (group-kill + reap-on-quit)
  and adds developer-facing logging — no status/channel/message, no Boundary-model change, no user-facing copy (Vocab
  guardrail #8 not engaged), proactive-prompt gating (offer-signal #4) untouched — and it *enforces* the same
  Boundary-A-only cancel contract by making teardown actually complete (`detached` is POSIX-only, gated
  `!environment.isWindows`, so the bootstrap/selfnode Windows-launch invariant #7 is preserved).
- **2B recorded behavior change (engages none of the above invariants):** Part 2B gates the behind-ness availability
  probe OFF while a sync is working (or terminal `cancelled`) — a documented change to the store's "re-fetch on EVERY
  tick" design (see Implementation §2B and the Progress entry). It engages none of the locked invariants: offer-signal
  #4 is untouched (the proactive prompt is already session-suppressed once an attempt starts,
  `mithrilAttemptStartedThisSession`), no user-facing copy changes, and no status/channel/message is added. **Note on
  #7 (honest framing, do NOT overclaim):** `detached: !environment.isWindows` is on the *shared* runner, so it also
  detaches `MithrilBootstrapService`'s POSIX children — in-app behavior is unchanged, but dev-terminal SIGINT/SIGHUP
  delivery to the bootstrap child changes (an **accepted dev-only regression**, see Risks); #7 is preserved for the
  launch/success/cancel paths and for Windows, not claimed "fully preserved" on POSIX.

## Exact files (full repo-relative paths)
The CAT-E touch-set (tracked in the master traceability + tasks-JSON `targetPaths`; the `MithrilPartialSyncService.ts`/`.spec.ts` pair below is re-touched from CAT-D and counted there). **2A is `MithrilPartialSyncService.ts` (+ its spec) ONLY;** 2B adds the renderer store (+ spec) and the controller guard; 2C adds the runner, `killProcessTree`, the controller reap, and `index.ts`:
- `source/main/mithril/MithrilPartialSyncService.ts` **(2A slot fix — service-only)**
- `source/main/mithril/MithrilPartialSyncService.spec.ts` **(2A tests, incl. the slot-clobber regression)**
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` **(2B — renderer probe-gating; documented behavior change)**
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` **(2B tests — extends the EXISTING spec, ~34.7 KB)**
- `source/main/mithril/mithrilCommandRunner.ts` **(2C detached; Part-1 logging already landed here)**
- `source/main/mithril/mithrilCommandRunner.spec.ts`
- `source/main/mithril/killProcessTree.ts` **(2C, new)**
- `source/main/mithril/killProcessTree.spec.ts` **(2C, new)**
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts`
- `source/main/mithril/MithrilController.ts` **(2B availability guard + 2C reap)**
- `source/main/mithril/MithrilController.spec.ts` **(new)**
- `source/main/index.ts` **(2C `safeExit` reap call)**

Implementable from this section alone. Mostly main-process control-flow + **logging** + tests; **2A and 2C are
main-process-only, but 2B adds a documented behavior change to the renderer store** (`MithrilPartialSyncStore.ts` — the
availability poll no longer probes on every tick) plus a belt-and-braces main-side guard — otherwise **no new status, no
PRD status-contract change, no Boundary-model change, no i18n/theme change, no user-facing copy** (offer-signal #4
untouched: while active the proactive prompt is already session-suppressed). CAT-E **deliberately widens the
touch-set beyond CAT-D's single file** to `source/main/mithril/mithrilCommandRunner.ts`,
`source/main/utils/chainStorageCoordinator.ts`, `source/main/mithril/MithrilController.ts`, `source/main/index.ts`,
`source/renderer/app/stores/MithrilPartialSyncStore.ts` (2B)
(+ a new tiny `source/main/mithril/killProcessTree.ts` helper) — justified in the Rationale: the operative root cause is
a `_currentProcess` slot-clobber in the service (fixed by 2A), with the concurrent behind-ness probe (gated by 2B in the
store + controller) and the runner spawn options / coordinator settle signal (2C / Part 3) as the surrounding
defense-in-depth CAT-D deliberately left untouched.

### Rationale (root cause, verified against the live post-CAT-D working tree, 2026-07-01)

> **CONFIRMED ROOT CAUSE (by code-plus-log inference — see the evidence-quality note below), operator gate RESOLVED,
> 2026-07-02 (Linux/WSL2, cancel during `downloading`; re-verified against the live post-Part-1 tree).** The three
> hypotheses below (originally undecided, gated on Part 1's diagnostic re-repro) are now settled by captured evidence —
> with the caveat, spelled out in the evidence-quality note, that the slot-emptiness AT cancel is a code-plus-log
> *inference* (Part 1's cancel-entry log is silent on an empty slot) and the "survives app exit" leg is not yet
> observed (the `ps` capture was taken while Daedalus was still alive):
>
> - **(iii) `_currentProcess` slot-clobber is CONFIRMED as the operative root cause.** There is one mutable slot
>   `_currentProcess` (`MithrilPartialSyncService.ts:116`). Both the cancelable download run AND the concurrent ~30s
>   behind-ness metadata probe (`snapshot show latest` / `snapshot list`) register through the same seam
>   `_runCommand → onProcess → _trackCurrentProcess`, whose `this._currentProcess = child` is **unconditional**
>   (`:669-670`, wired for every `_runCommand` caller at `:1221-1223`). `start()` (`:157`) and `cancel()` invalidate
>   the behind-ness caches, so the first availability poll during `downloading` is a guaranteed cache miss that spawns a
>   metadata child; that child (a) **overwrites** the download child's slot on its `onProcess(child)`, then (b) **nulls**
>   the slot on its own clean `'close'` via `onProcess(null)` (`mithrilCommandRunner.ts:287` for `runCommand`, `:201`
>   for `runBinary`). The download child is thereafter untracked, so at cancel the SIGTERM gated on
>   `if (this._currentProcess)` (`:345`, kill `:354`) sends nothing and `forceKill()`'s `this._currentProcess?.kill('SIGKILL')`
>   (`:414`) is a no-op → the download keeps running, its held-open stdio keeps the runner's `'close'` from firing,
>   `start()` never returns, the coordinator's 15s join expires → `abandonCancel()` "restart Daedalus" floor — the
>   abandoned-cancel survivor the `ps` capture directly shows. The **survives-app-exit** half is NOT shown by this
>   capture (the `ps` snapshots were taken while Daedalus was still alive — see the evidence-quality note); it rests on
>   the earlier operator report + non-`detached` reparenting reasoning (`safeExit()` never reaps the child) and remains
>   gated on re-repro #2.
> - **(ii) re-grouping (`setsid`/`setpgid`) is RULED OUT by `ps`.** The survivor's **pgid 227499 == Daedalus's pgid**,
>   its **sess 218848 == Daedalus's sess**, and its **ppid 228282 == Daedalus's main process** — i.e. it is the
>   in-group direct `mithril-client cardano-db download` child, not a re-grouped/escaped descendant. (survivor pid
>   229730; see `task-ux-702d-research.md` finding #8 and the captured `logs/ps.log`.)
> - **(i)'s in-group process-group topology is PRESENT but was NOT the operative failure.** The child does share
>   Daedalus's group, but detached + group-kill alone would still miss the survivor — because the slot that names it was
>   nulled before cancel ran. So the group-kill mechanism (Part 2C) is correct **defense-in-depth**, not the primary
>   fix; the primary fix is the slot lifecycle (Part 2A).
>
> **Evidence-quality note (observation vs inference — the verdict stands, but be precise about what proves it).** What
> the capture DIRECTLY shows: download child **pid 229730** spawned 07:58:04.486 and appears **exactly once** in the
> whole Daedalus log — no `exit`/`close`/kill line ever names it; the behind-ness probe child **pid 230124** spawned
> 07:58:26.888 during `downloading` (~22 s after the download start — the ~30 s figure is the polling *cadence*, this
> ~22 s is the observed offset) and closed cleanly 07:58:27.141 (→ `onProcess(null)`), ~5 s before the inferred cancel;
> **no kill line was logged against ANY pid at cancel**; the first cancel-related log is the coordinator join-timeout at
> **07:58:47.314Z** → `abandonCancel` at 07:58:48.316; the download kept advancing (59 progress lines inside the cancel
> window, still 2034/5845 files at 08:00:43); the two POST-cancel probes (pids 230842/230843, 07:58:55) WERE caught and
> SIGTERM'd by the late-arriving-child guard — proving the late-kill path works and isolating the *pre-cancel* slot
> lifecycle as the gap. What is INFERRED (not directly observed): the slot was **empty at cancel** — Part 1's landed
> cancel-entry log sits INSIDE the real-kill branch (`if (this._currentProcess)`), so an empty slot logs nothing, and
> indeed the repro contains **no cancel-entry line and no `hadChild` field anywhere**; the cancel time itself is inferred
> (join timeout − 15 s ≈ **07:58:32**). Separately, the `ps` snapshots were captured **while Daedalus was still running**
> (Electron main 228282, launcher 228275, renderer 228377 all present in ps.log), so `ps.log` proves the
> **abandoned-cancel survivor**, NOT the "survives app exit" half — that leg rests on the earlier operator report +
> non-`detached` reparenting reasoning and remains gated on re-repro #2. (The unconditional `hadChild` cancel-entry log
> added under 2A — step 6 — plus 2C step 12's reap and re-repro #1 upgrade the empty-slot leg from inference to direct
> observation.)
>
> **Consequence for the plan:** Part 2 is re-scoped to **2A (slot-lifecycle fix, PRIMARY) → 2B (gate the behind-ness
> probe during active work, removing the concurrent metadata spawn entirely) → 2C (detached + `killProcessTree` +
> reap-on-quit, defense-in-depth)**, with Part 3 (settle-on-exit) still evidence-gated and expected NOT needed once 2A
> makes cancel kill the direct child so its `'close'` fires. The "two structural gaps" narrative below is retained as
> the *pre-gate* hypothesis space (it motivated Part 1's diagnostic gate); read it knowing gap 1 (process-group) is
> demoted to 2C defense-in-depth, gap 2 (`'close'`-not-`'exit'` settle) stays Part-3 evidence-gated, and the operative
> root cause is the slot-clobber refinement of hypothesis (iii).

This is the **CAT-D "Process-*tree/group* termination" residual's trigger condition materializing.** CAT-D's own Risks
section pre-specified the remedy: *"If operator QA ever shows a real download or conversion surviving the bounded join
(the `abandonCancel` floor firing on a genuine cancel), file a follow-up to spawn `detached` and kill the process group
(Linux/macOS) / `taskkill /T /F` (Windows)."* Operator QA now shows exactly that. CAT-E promotes that deferred residual
to a planned/scheduled fix (not yet implemented), adds the diagnostic logging the flow was missing, and hardens the coordinator's settle signal.

**What CAT-D changed, and why the failure moved from "second attempt" to "first cancel."** CAT-D did **not** introduce
the surviving-process gap — it made cancel *effective enough to expose it on the first try*. The **earlier
second-attempt failure was a distinct, direct-child bug: the late-spawn registration race** — a cancel in the
`preparing`/pre-spawn window found `_currentProcess` still `null`, so a child registering a moment later was never
signalled — and **CAT-D Part 3 fixed exactly that** with the eager late-kill (`_trackCurrentProcess` kills a child that
registers after `_isCancelled`, `:646-657`). That fix, together with Part 1's checkpoints (CP-B, before the download
re-spawn), means the **first** cancel now *reliably* signals the in-flight `mithril-client` download child at the
download stage — the natural time to cancel a long download. But signalling the **direct** child is not the same as
reaping its **tree**: the *new* first-cancel failure is the **process-group/descendant gap** (a descendant the
direct-pid kill never reaches) plus the `'close'`-not-`'exit'` settle — the two structural gaps CAT-D left open — so the
"restart Daedalus" floor now fires on the first attempt. In short: Part 3 closed the direct-child *registration* race;
CAT-E closes the *process-tree termination* + *settle* gaps. This is a correct-but-incomplete fix surfacing its own
residual, not a regression in CAT-D's logic.

**The two structural gaps (both untouched by CAT-D, both in files CAT-D deliberately did not edit):**

1. **Direct-pid kill only — no process-group / tree kill (root cause of the surviving `mithril-client`).**
   `mithrilCommandRunner.ts` spawns both binaries with `spawn(bin, args, { cwd, env })` — **no `detached`**
   (`:155` runBinary, `:231-235` runCommand). Every kill site signals **only the direct child pid**: `cancel()`
   `this._currentProcess.kill()` (SIGTERM, `MithrilPartialSyncService.ts:346`), `forceKill()`
   `this._currentProcess?.kill('SIGKILL')` (`:391`), and the late-kill `child.kill()` in `_trackCurrentProcess`
   (`:648`). Because the child is **not** `detached`, it shares Daedalus's process group, so `process.kill(-pid)` is not
   even available (the child does not lead a group). If `mithril-client` performs the download via a descendant — or is
   slow to die — that work is not reaped. This matches the live repro precisely: `mithril-partial-sync.log` kept
   advancing and the `ps.log` capture showed an active `mithril-client` after cancel (the **after-app-exit** half is
   from the earlier operator report, not this capture — it was taken while Daedalus was still alive; see the RESOLVED
   box's evidence-quality note — a non-`detached` child reparents to init on parent exit rather than being killed, see
   gap 3).
2. **Coordinator settles on `'close'` (stdio-EOF), not `'exit'` (process death) — root cause of the false
   cleanup-failure.** The runner resolves its promise **only** on `'close'` (`:190-195` runBinary, `:265-270`
   runCommand); there is **no `'exit'` listener anywhere.** `'close'` fires only after **all** stdio streams are at EOF;
   a surviving descendant that inherited the piped stdout/stderr **holds those pipes open**, so `'close'` never fires
   even after the direct child is signalled. The coordinator's "settled" is the resolution of `_partialSyncRunPromise`
   ⟸ `start()` returns ⟸ `_runCommand`/`_runBinary` resolve ⟸ runner resolves **on `'close'`**. So a held-open pipe
   keeps `run` pending, the ~16 s bounded join (`PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS = 15_000` + `forceKill` + 1 s,
   `chainStorageCoordinator.ts:43-44,473-492`) expires → `settled = false` → `abandonCancel()` (`:299`, →
   `MithrilPartialSyncService.abandonCancel()` `:399-418`) emits the terminal **"restart Daedalus" cleanup floor**
   while the download keeps running. **Note:** killing the whole *group* (gap 1's fix) also closes the descendant's
   pipe, so fixing gap 1 is expected to make `'close'` fire — gap 2's own fix is therefore scoped as evidence-gated
   defense-in-depth (Part 3), not a speculative settle-swap.
3. **Nothing reaps the child on app quit.** `safeExit()` (`source/main/index.ts:87-129`) is the single shutdown funnel for the normal quit path
   (the `isSelfnode` before-quit early-returns at `index.ts:297,:319` call `safeExitWithCode(0)` directly and bypass the
   reap, but selfnode is a dev-only local-cluster mode with no Mithril partial sync, so there is nothing to reap on that
   path); it awaits only `cardanoNode.stop()` and then `process.exit()`s (`safeExitWithCode.ts:4-16`). **"Single funnel"
   applies to the normal GUI quit paths only (do NOT over-read it):** menu quit, window close, app-update quit, and the
   watchdog all funnel through `before-quit` → `safeExit()`, but **fatal/startup paths bypass it** — `setup.ts:197`
   `safeExitWithCode(20)`, `CardanoNode.ts:602` `safeExitWithCode(0)`, `config.ts:38` `app.exit(1)`,
   `ensureDirectoryExists.ts:14,:19` `process.exit(1)`, and `safeExitWithCode.ts:17` `relaunch()` — and would skip the 2C
   reap; acceptable (these are unlikely to coincide with an active partial sync). **No code touches the
   Mithril subprocess on quit** — confirming the "still running after app exit" half of the repro. Once we spawn
   `detached` (gap 1's fix), the child is in its own group and is *guaranteed* to survive `process.exit()`, so an
   explicit shutdown reap becomes **mandatory** (under hypothesis (i) it also fixes the pre-existing after-quit
   orphan; under (ii)/(iii) the reap inherits whatever Part-2 mechanism Part 1 selects — see step 8 and Risks).

**Why logging is the blocking problem.** The entire cancel/kill/join/abandon path is near-silent (see Reference
details): cancel entry, child-present, the SIGTERM/SIGKILL sends, the coordinator join timeout / `forceKill`
escalation / settled decision, `finalizeCancel` success/catch, the `abandonCancel` floor, and child `exit`/`close` are
**all unlogged** — only three `warn`-**on-throw** branches (which do not fire here) plus the spawn line exist. So the
current failure is undiagnosable from app logs and diagnosis relies on external `ps` + tailing
`mithril-partial-sync.log`. **CAT-E's Part 1 (logging) ships first and is load-bearing:** its re-repro produces the
decisive evidence that confirms which of the gaps above is active and whether the Part 3 settle change is needed —
turning the next repro into a proof instead of another blind failure.

**The fix has three parts, ordered by dependency:**

**Part 1 — Cancel-lifecycle logging (ship first; non-behavioral).** Add `info`/`warn` logging across the cancel path so
one more repro is decisive: cancel entry (status, `_currentProcess` pid, child-present), each SIGTERM/SIGKILL/group
send (pid, signal, group-vs-direct), the late-kill on registration, the coordinator join (timeout hit, `forceKill`
escalation, `settled` true/false, `finalizeCancel`-vs-`abandonCancel` branch), `finalizeCancel` entry/success/catch,
the `abandonCancel` floor, and — in the runner — child `'exit'` (code + signal + `killed`) and `'close'` (add both).
Behavior is unchanged; this is pure observability.

**Part 2 — RESOLVED as 2A → 2B → 2C (the operator gate confirmed hypothesis (iii) slot-clobber; see the RESOLVED box
above).** Part 1's re-repro picked the mechanism: the survivor is the in-group direct download child that the slot
never tracked at cancel time, so the fix is the `_currentProcess` slot lifecycle — NOT group-kill by itself. The three
sub-parts, ordered by dependency:

- **2A — Slot-lifecycle fix (PRIMARY, load-bearing, lands first, unit-proven).** Make metadata reads stop occupying the
  cancelable `_currentProcess` slot; only cancelable runs register into it, and their reference stays durable for the
  life of the run. Mechanism: an opt-in `trackAsCancelable` flag on `_runCommand` (default `false`) that, when false,
  omits the runner's `onProcess` callback entirely so the metadata child can neither overwrite nor null the slot.
  **The flag is threaded from the CALL SITE** — the download (`:591`) AND the two `start()` metadata reads
  (`resolveLatestSnapshotMetadata()` at `:186` preparing-resolve and `:224` latest-drift re-resolve) pass `true`; the
  conversion via `_runBinary` already tracks only for conversion; the background 30s probe path (via
  `_getCachedLatestCertifiedImmutableNumber:828 → resolveLatestSnapshotMetadata:796`, reached at `:837`) passes `false`
  (default). **It MUST NOT be inferred from `this._activeWorkDir`** — the probe's `:837` read also runs while
  `_activeWorkDir` is set during `downloading`, so inferring would re-introduce the exact clobber. This preserves
  today's cancel-at-`preparing` in-flight-kill coverage (no regression) while making the slot dedicated-by-construction.
- **2B — Gate the behind-ness availability probe during active work (documented behavior change).** Removes the
  concurrent metadata spawn entirely (defense for 2A) and eliminates the per-tick probe CPU cost while a sync runs.
  Renderer store `MithrilPartialSyncStore._refreshAvailability` skips the probe while `isWorking || status === 'cancelled'`;
  a belt-and-braces main-side guard in `MithrilController.getPartialSyncAvailability` degrades to
  `{ isEnabled, isSignificantlyBehind: false }` while active. This is a **deliberate behavior change** to the store's
  "re-fetch on EVERY tick" design — flagged for the reviewer and recorded in the invariants section. The one on-screen
  consumer of the derived figure is the Daedalus Diagnostics dialog (`isSignificantlyBehind`/`certifiedEpoch`), which
  freezes at its pre-run values if opened mid-run — accepted (the node is stopped, so the figure is not
  wrong-and-moving; see the Why-safe 2B bullet).
- **2C — Defense-in-depth: detached POSIX + `killProcessTree` + reap-on-quit.** Layered ON TOP of 2A (none of it
  replaces the slot fix). Spawn both binaries `detached: !environment.isWindows` (POSIX group leader), add a
  cross-platform `killProcessTree(child, signal, opts?)` helper with an **async-interactive / sync-shutdown Windows
  split** (POSIX `process.kill(-pid, signal)`; Windows interactive sites use non-blocking `exec` so the UI never
  freezes, and ONLY the shutdown reap uses `execSync('taskkill /pid <pid> /t /f', …)` — it must finish before
  `process.exit()`), route the three kill sites (`cancel()` `:354`, `forceKill()` `:414`, `_trackCurrentProcess`
  late-kill `:684`) through it, and add a best-effort `MithrilController.reapPartialSyncOnShutdown()` called once from
  `safeExit()` right after `pauseActiveDownloads()`. The reap-on-quit is **independently required** for the "quit
  mid-download without cancel" scenario (`cancel()`/`forceKill()` are never called on a plain quit, so 2A alone cannot
  cover it).

**Part 3 — Settle on child exit, decoupled from stdio drain (evidence-gated defense-in-depth).** The re-repro after 2A
confirms whether the fix makes `'close'` fire promptly. **Expected: it does** — 2A makes cancel SIGTERM the direct
download child (per the `ps` evidence the survivor IS the direct child, not a re-grouped descendant), whose stdio pipes
then close → the runner's `'close'` fires → the run settles; in which case Part 3 is satisfied by Part 1's `'exit'`
logging and **no settle-semantics change is made.** **Only if** evidence shows `'close'` still hangs after a confirmed
kill, add a coordinator-visible settle
signal derived from the direct child's `'exit'` (process death) while continuing to drain stdout/stderr and end the log
stream on `'close'` — i.e. **do not** swap the runner's resolve event from `'close'` to `'exit'`, because the metadata
read paths (`resolveLatestSnapshotMetadata` → `snapshot list`/`show`) parse the returned `stdout`, which is only
guaranteed complete at `'close'`. This keeps the risky change out of the diff unless the evidence demands it.

### Reference details (verified against the live post-CAT-D working tree, 2026-07-01)
- **Runner spawns (add `detached`):** `source/main/mithril/mithrilCommandRunner.ts` — `runBinary`
  `spawn(binaryPath, args, { cwd: workDir, env })` (`:155`); `runCommand`
  `spawn(binaryPath, commandArgs, { cwd: workDir, env })` (`:231-235`). Both default to `stdio: 'pipe'`;
  `attachLogStream(child, logStream)` (`:110-120`) pipes stdout/stderr to the log file; separate `'data'` listeners
  (`:169-181` `runBinary`, `:244-256` `runCommand`) feed the `onStdout`/`onStderr` accumulators. `onProcess(child)` fires post-spawn (`:157`, `:237`); `onProcess(null)` fires on `'error'`/`'close'`.
  `detached` does **not** change piped-stdio behavior on POSIX (verified) — the logStream/accumulators/close handlers
  are unaffected.
- **Runner resolve/exit seam (add `'exit'` logging; Part 3 settle if needed):** `'close'` handlers at `:190-195`
  (runBinary) and `:265-270` (runCommand); `'error'` handlers at `:184-188` / `:259-263`. There is **no `'exit'`
  listener** anywhere in the file (grep-confirmed).
- **Kill sites to route through `killProcessTree` (Part 2):** `MithrilPartialSyncService.ts` — `cancel()`
  `this._currentProcess.kill()` (SIGTERM, `:346`, inside the `try` at `:345-352`); `forceKill()`
  `this._currentProcess?.kill('SIGKILL')` (`:391`); `_trackCurrentProcess()` late-kill `child.kill()` (`:648`).
- **Cancel state machine (Part 1 logging targets):** `cancel()` `:308-353` (real path sets `_isCancelled = true` at
  `:332`, emits `cancelling`, kills at `:346`); `finalizeCancel()` `:355-387` (guard `:356`, cleanup `:361` →
  `_cleanupPartialSyncArtifacts` `:704-707`, catch `:371-383` currently silent); `forceKill()` `:389-397`;
  `abandonCancel()` `:399-418` (the "restart Daedalus" floor `:408-413`, currently silent); `_trackCurrentProcess()`
  `:639-658`.
- **Coordinator join (Part 1 logging + optional Part 3 settle):** `chainStorageCoordinator.ts` —
  `cancelPartialSync` `:284-300` (calls `cancel()` `:287`, awaits `_awaitRunSettledBounded` `:291`, branches
  `finalizeCancel` `:294-297` [live `:302`] vs `abandonCancel` `:299` [live `:313`]); `_awaitRunSettledBounded`
  `:473-492` [live `:487-514`] (15 s join → `forceKill` → 1 s); `_awaitSettledWithin` `:494-515` [live `:516`]. Timeouts
  `PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS = 15_000` / `PARTIAL_SYNC_CANCEL_FORCE_KILL_TIMEOUT_MS = 1_000` (`:43-44`). None
  of these log today. (Header caveat: these numbers are pre-Part-1, 2026-07-01; the bracketed `[live …]` are the
  verified post-Part-1 positions.)
- **Shutdown reap point (Part 2):** `safeExit()` `source/main/index.ts:87-129` — insert the reap **before**
  `safeExitWithCode(exitCode)` (both the early-return and the `await cardanoNode.stop()` branches funnel through it;
  add the reap once, right after `pauseActiveDownloads()`). `before-quit` handler `index.ts:283-327`;
  `safeExitWithCode` `source/main/utils/safeExitWithCode.ts:4-16` (`process.exit`).
- **Controller reap primitives (Part 2):** `getMithrilController()` singleton `MithrilController.ts:462-464`;
  `isPartialSyncActive()` `:199-204`; `forceKill` handler chain `_getPartialSyncDependencies().handlers.forceKill` →
  `_partialSyncService.forceKill()` (`:449`). No `dispose()`/shutdown hook exists — add a thin
  `reapPartialSyncOnShutdown()` (best-effort, try/caught) or reuse the `forceKill` chain guarded by
  `isPartialSyncActive()`.
- **Platform + kill precedent (Part 2):** `environment.isWindows`/`isMacOS`/`isLinux` from
  `source/main/environment.ts:60-72` (house style; `mithrilCommandRunner.ts` already branches on it at `:57,136,212`
  — the `runCommand` env-branch `:212` is live `:223`).
  `CardanoNode._killProcessWithName` `:1068-1115` provides the kill commands to reuse: POSIX `process.kill(pid)` /
  Windows `taskkill /pid <pid> /t /f` — CAT-E's POSIX branch changes the positive `pid` to the group `-pid`. Note
  the precedent is **bounded-async, not synchronous**: CardanoNode runs taskkill via `this._actions.exec(...)` and
  then *awaits* `promisedCondition(..., killTimeout)` (`CardanoNode.ts:1086,:1089-1092`). CAT-E deliberately uses
  the synchronous, timeout-bounded `execSync` variant instead, because the shutdown reap runs just before
  `process.exit()`. Windows-`detached` hazard documented at `CardanoSelfnodeLauncher.ts:112-115` (the comment
  "detaching breaks Windows launching the local-cluster.exe + cardano-launcher" lives ONLY there; `:254` is a *second*
  bare `detached: environment.isDev` — the mock token-metadata server — with no comment) → gate `detached` to
  `!environment.isWindows`.
- **Kill-verify helper available (not required):** `find-process` is a declared production dependency used by
  `source/main/utils/processes.ts`; `tree-kill` is **dev-transitive only** (not a declared dep) — do **not** import it
  in `source/main`. Prefer the ~15-line hand-rolled `killProcessTree` mirroring `CardanoNode`.
- **Logger (Part 1):** `logger.info(msg, data)` / `logger.warn(msg, data)` from `../utils/logging` (service/runner) and
  `./logging` (coordinator); prefixes by file — `'MithrilPartialSyncService: …'`, `'[MITHRIL] …'` /
  `'ChainStorageCoordinator: …'`, `'[mithril] …'`. Only `info`/`warn` are used in these files (and the service spec's
  logging mock stubs only `info`/`warn`).

### Implementation (ordered, mechanical, small-model-implementable)
**Part 1 first (ship + re-repro before Part 2/3 land):**
1. **Runner child lifecycle logging** — in `mithrilCommandRunner.ts`, add `child.on('exit', (code, signal) => …)` to
   both `runBinary` and `runCommand` logging `'[mithril] child exited'` with
   `{ pid: child.pid, code, signal, killed: child.killed }`, extend the existing `'close'` handlers (`:190`,`:265`)
   to log `'[mithril] child closed'` with `{ pid: child.pid, exitCode }`, **and add a post-spawn
   `logger.info('[mithril] child spawned', { pid: child.pid })` in both `runBinary` and `runCommand`** (immediately
   after `spawn()` returns, beside the `onProcess(child)` fire). The post-spawn line is required: the existing
   `'Spawning:'` line fires *before* `spawn()` returns and can never carry the pid, and the runner spawns multiple
   sequential children per run, so un-pid'd exit/close lines are ambiguous. This post-spawn pid is the ground truth
   the hypothesis-(iii) comparison needs — the service-side kill-send pid is read from the very `_currentProcess`
   slot under suspicion.
2. **Service cancel-path logging** — in `MithrilPartialSyncService.ts` add `logger.info`/`warn` lines (prefix
   `'MithrilPartialSyncService: …'`, `{ status, pid, hadChild }` context) at: `cancel()` entry (the real branch, before
   `:346`) and each kill send; `_trackCurrentProcess` on late-kill; `finalizeCancel()` entry + success + the currently
   silent catch (`:371-383`); and `abandonCancel()` entry (log the floor with the captured stage).
3. **Coordinator join logging** — in `chainStorageCoordinator.ts` add `logger.info`/`warn` (prefix `'[MITHRIL] …'`) in
   `_awaitRunSettledBounded`/`cancelPartialSync`: join-timeout hit, `forceKill` escalation, `settled` result, and the
   `finalizeCancel` vs `abandonCancel` branch taken.
4. **Ship Part 1, reproduce once**, and read the new logs to confirm: is a child present at cancel? does SIGTERM/SIGKILL
   reach it? does `'exit'` fire but `'close'` hang (gap 2)? does
   `ps -eo pid,ppid,pgid,sess,args | grep -i mithril` (or `pstree -gp <daedalus-pid>`) show a surviving
   `mithril-client` descendant (gap 1)? **Record Daedalus's own pid and pgid FIRST** — plain `pstree` and default
   `ps` do not display pgid/session. Interpret via the table below, record the finding in
   `task-ux-702d-research.md`; it **selects the Part-2 mechanism** (the three hypotheses in Risks) **and** decides
   whether Part 3's settle change is needed.

   > **Pre-fix interpretation table** (Part 1 runs before Part 2, so the non-detached child shares **Daedalus's**
   > process group — the "leader's group" question must be read against Daedalus's own pgid/sess):
   >
   > | Observation (`ps -eo pid,ppid,pgid,sess,args`) | Hypothesis | Part-2 mechanism |
   > |---|---|---|
   > | Survivor's pgid/sess == Daedalus's pgid/sess **and** its ppid is the `mithril-client` child (or `1` after that child died) | **(i)** in-group descendant | `detached` + `process.kill(-pid)` group-kill (the primary steps below) |
   > | Survivor has its **own** sess or pgid (`setsid`/`setpgid`) | **(ii)** re-grouped descendant escaping the group | descendant-tree walk / identity-based `find-process` fallback (see the `killProcessTree` caveat constraints) |
   > | Survivor's ppid == Daedalus's pid **and** its pid != the service kill-send pid (or the cancel-entry log shows `hadChild: false`) | **(iii)** stale/null `_currentProcess` slot — an untracked direct child | fix the `_currentProcess` slot lifecycle (not `detached`) |
   >
   > Note the (iii) signature is an **untracked live direct child** whose pid never appears in a kill-send log — a
   > live process does not survive SIGKILL, so "survivor pid == killed pid" is NOT the test; compare the survivor's
   > pid against the post-spawn `'[mithril] child spawned'` pids and the kill-send pids.

**Part 2 — 2A → 2B → 2C (RESOLVED: hypothesis (iii) `_currentProcess` slot-clobber; see the RESOLVED box in the
Rationale). All line numbers below are the LIVE post-Part-1 tree (2026-07-02), not the stale pre-Part-1 numbers in the
Reference details above.**

**2A — Slot-lifecycle fix (PRIMARY, service-only, lands first, unit-proven):**
5. **Add an opt-in `trackAsCancelable` flag to `_runCommand`** (`MithrilPartialSyncService.ts:1208-1229`) — **primary
   choice: carry it as a field on a service-local options type** (NOT the runner's `RunCommandOptions`, which is spread
   into runner options at `:1217`). Prefer the options-field over a 4th positional parameter because the two SHARED raw
   methods that must thread `true` — `_listSnapshotsRaw` (`:947`) and `_showSnapshotRaw` (`:976`) — call
   `_runCommand(args, { requireKeys: false })` **WITHOUT the `workDir` positional**, so a 4th positional flag would force
   them to spell out `workDir` (or `undefined`) positionally just to reach the flag slot. (A 4th parameter defaulting to
   `false` is the acceptable *alternative* if the options-field is awkward — still NOT on `RunCommandOptions`.) When the
   flag is `false`, build the runner callbacks **WITHOUT `onProcess`** — but **KEEP `onLogStream`**: a metadata read's
   `onLogStream` still overwrites the shared `_logStream` slot mid-run, harmless ONLY because `_logStream` is write-only
   today (declared `:117`, nulled `:774`, assigned `:1225`, never read). Metadata children then spawn and exit without
   ever touching `_currentProcess` — neither the overwrite (`:669-670`) nor the null.
6. **Thread `trackAsCancelable: true` from every CANCELABLE call site — plumbed from the call site, NEVER inferred from
   `this._activeWorkDir`:**
   - the download `_runCommand` (`:591`) → `true`;
   - **(B1 correction — do NOT lose cancel-at-`preparing` coverage) both `start()`-phase metadata reads** —
     `resolveLatestSnapshotMetadata()` at `:186` (preparing-resolve) and `:224` (latest-drift re-resolve) — thread
     `trackAsCancelable: true` through `resolveLatestSnapshotMetadata (:796) → _showSnapshotRaw (:971) /
     _listSnapshotsRaw (:946) → _runCommand`. These ARE part of the cancelable run: today they register into
     `_currentProcess`, so a cancel landing during `preparing` SIGTERMs an in-flight metadata child (`:345`/`:354`) and
     the late-kill (`:684`) reaps one spawned just after `_isCancelled` flips. Threading the flag **preserves that
     cancel-at-`preparing` in-flight-kill coverage (no regression)**.
   - the conversion via `_runBinary` (`:645-667`, `onProcess` `:662-664`) is conversion-only (sole consumer
     `_convertStagedSnapshot:614`), so it keeps its existing tracking wiring unchanged (opt-in by construction) — update
     its comment to note the invariant.
   - **Everything else stays untracked (default `false`):** the background 30s behind-ness probe chain
     `getPartialSyncBehindness (:891) → _getCachedLatestCertifiedImmutableNumber (:828) → resolveLatestSnapshotMetadata`
     (the `:837` read), and the ad-hoc `listSnapshots (:926)` / `showSnapshot (:937)`.
   - **CRITICAL — do NOT infer the flag from `this._activeWorkDir != null`:** the probe's `:837` read also runs while
     `_activeWorkDir` is set during `downloading`, so inferring would re-introduce the exact clobber being fixed. The
     flag MUST be an explicit argument threaded from the call site. Add a one-line invariant comment on
     `_trackCurrentProcess` (`:669`): only cancelable runs (download, `start()`-phase reads, conversion) may register —
     and in the same comment note the **`_logStream` coupling** (metadata reads keep `onLogStream`, which still
     overwrites the shared `_logStream` slot mid-run; safe ONLY while `_logStream` stays write-only — declared `:117`,
     nulled `:774`, assigned `:1225`, never read) so a future reader of `_logStream` sees it.
   The kill sites all target the now-durable reference: `cancel()` gate `:345` / SIGTERM `:354`, `forceKill()` SIGKILL
   `:414`, late-kill `:684`. **Latent bugs fixed for free** (mention in the PR): `cancel()`'s no-active guard (`:312`)
   and `_assertRecoveryActionAllowed` (`:727`) can no longer misread an in-flight metadata probe as an active sync.
   **Unconditional cancel-entry log (lands with 2A — upgrades the diagnosis from inference to observation).** At
   `cancel()`'s real-branch entry, **BEFORE** the `if (this._currentProcess)` gate (`:345`), add an *unconditional*
   `logger.info('MithrilPartialSyncService: cancel entry', { status, pid: this._currentProcess?.pid ?? null, hadChild: this._currentProcess != null })`.
   Part 1's landed cancel-entry line fires only *inside* the kill branch, so an empty slot logs nothing — the exact case
   under diagnosis (the captured repro carries no cancel-entry line and no `hadChild` field anywhere). With this line,
   operator re-repro #1 **directly observes** the slot state at cancel (`hadChild: true` post-2A, or `hadChild: false`
   were the clobber still live) instead of inferring it from pid-appearance + missing kill lines.
   **Files: `MithrilPartialSyncService.ts` (+ spec) ONLY.**

**2B — Gate the behind-ness availability probe during active work (renderer store + main-side guard; documented
behavior change):**
7. **Renderer (primary)** — in `MithrilPartialSyncStore._refreshAvailability` (`:249`) add a skip-when-active guard
   **after** the `_isTornDown || _isRefreshingAvailability` check (`:250`) and **before** `_isRefreshingAvailability = true`
   (`:254`), so a skipped tick does not consume the in-flight guard, start the 10s timeout race, or advance
   `_consecutiveStableReads` (`:103`, back-off `:279-291`):
   ```ts
   // Skip the probe while Mithril work is active or a cancel outcome is terminal-cancelled: a probe here would spawn a
   // concurrent mithril-client metadata child (and pay the fork/readdir cost) for a figure only the Diagnostics dialog
   // could display mid-run (frozen pre-run values; accepted — see task-ux-702d-cat-e.md 2B). Behavior change from
   // "re-fetch on EVERY tick".
   if (this.isWorking || this.status === 'cancelled') {
     return;
   }
   ```
   `isWorking` is the existing computed (`:151`) over `isMithrilPartialSyncWorkingStatus` (all nine working statuses
   incl. `cancelling`). Do NOT stop or re-arm the interval (`_armAvailabilityInterval`); the "poll slows, never stops"
   contract is preserved — the tick simply returns early until status leaves the active set (terminal `cancelled` →
   `retry`, or `restart-normal` → `idle`), after which the next tick resumes against freshly invalidated caches.
   **This is a deliberate behavior change to the store's "re-fetch on EVERY tick" design comment (`:113`), which must be
   updated in the same diff.**
   > **CAT-A non-regression (do NOT re-introduce the first-load bug).** CAT-A (tasks-JSON note, ISSUE-1/2) *removed* an
   > earlier `if (this.isWorking) refresh()` gate because it wrongly suppressed the availability refresh during **normal
   > (non-partial-sync) operation** — first-load invisibility. 2B's guard is the *complement*, not a revert: it skips only
   > while **partial-sync** work is active (`isWorking` here is the partial-sync `isMithrilPartialSyncWorkingStatus`
   > computed) or terminal-`cancelled`, and does NOT skip in `idle`/`failed`/`completed` — so the CAT-A first-load /
   > normal-sync self-correction path (isWorking `false`) still refreshes every tick. The guard must be an early **return**
   > inside `_refreshAvailability`, never a re-introduced `if (isWorking) refresh()` wrapper around the call.
8. **Main-side (belt-and-braces)** — in `MithrilController.getPartialSyncAvailability` (`:172`), after the `isEnabled`
   short-circuit and before the `getPartialSyncBehindness()` call (`:177`), return
   `{ isEnabled, isSignificantlyBehind: false }` when `isMithrilPartialSyncWorkingStatus(status) || status === 'cancelled'`.
   **`isMithrilPartialSyncWorkingStatus` must be IMPORTED into `MithrilController.ts`** — today only
   `isMithrilPartialSyncBlockingNodeStart` is imported (`:12`); read `status` from the `this._partialSyncStatus.status`
   seam (kept fresh by `broadcastPartialSyncStatus`, `:257-273`/`:260`, wired in `initialize()` `:116-123`).
   The store is that channel's only requester, but guard main-side so no future caller can re-introduce the concurrent
   spawn. Degrading to not-behind while active is safe (the proactive prompt is already session-suppressed, and
   `getPartialSyncBehindness` itself degrades to `{ isSignificantlyBehind: false }` on any failure, `:915-922`). A probe
   already past the guard when `start()` flips to `preparing` still spawns one metadata child — harmless post-2A (it
   cannot touch the slot), so the guard is belt-and-braces, not airtight.
   **Files: `MithrilPartialSyncStore.ts` (+ spec), `MithrilController.ts` (+ new spec, shared with 2C's reap unit).**

**2C — Defense-in-depth: detached + `killProcessTree` + reap-on-quit (layered ON TOP of 2A; none of it replaces the
slot fix):**
9. **Add `source/main/mithril/killProcessTree.ts`** with an **async-interactive / sync-shutdown Windows split** so
   interactive kills never freeze the main thread for up to 5 s, while the shutdown reap still completes before
   `process.exit()`. Signature: `killProcessTree(child: ChildProcess | null, signal: NodeJS.Signals = 'SIGTERM', opts?: { sync?: boolean }): void`.
   - POSIX (both modes): `process.kill(-pid, signal)` — instantaneous, no split needed (child is group leader via
     `detached`).
   - Windows, default async (interactive sites): `exec('taskkill /pid <pid> /t /f', { windowsHide: true, timeout: 5000 }, cb)`
     — non-blocking; the app keeps running so the command reliably launches; the callback logs and falls back to
     `child.kill()`.
   - Windows, `sync: true` (shutdown reap ONLY): `execSync('taskkill /pid <pid> /t /f', { stdio: 'ignore', timeout: 5000, windowsHide: true })`
     — must complete before `process.exit()`; a fire-and-forget exec may never launch that late.
   - `taskkill /f` is always a hard kill → the `signal` arg is documented as a Windows no-op; null/absent pid is a
     no-op; a throwing group-kill/exec falls back to `child.kill(signal)`, warned on failure.
10. **Spawn `detached` on POSIX** — set `detached: !environment.isWindows` on both spawn option objects in
    `mithrilCommandRunner.ts` (`runBinary` `:155`, `runCommand` `:243`). Keep piped stdio; do **not** `unref()`.
    **B2 — honest #7 framing (do NOT overclaim "#7 fully preserved"):** this is the *shared* runner, so `detached` also
    detaches `MithrilBootstrapService`'s POSIX children — in-app behavior is unchanged, but dev-terminal SIGINT/SIGHUP
    delivery to the bootstrap child changes = an **accepted dev-only regression** (see Risks). Do **NOT** un-detach the
    bootstrap spawn path (a per-call `detached` flag would fork the shared runner's spawn semantics for no production
    benefit); the bootstrap reap-on-quit gap is a **branch-ticket candidate**, not this diff.
11. **Route the three interactive kill sites through `killProcessTree` (async mode)** —
    `cancel()` (`:354` → `killProcessTree(this._currentProcess, 'SIGTERM')`), `forceKill()`
    (`:414` → `killProcessTree(this._currentProcess, 'SIGKILL')`), `_trackCurrentProcess` late-kill
    (`:684` → `killProcessTree(child, 'SIGTERM')`). Keep the existing try/catch wrappers and Part 1 logging.
12. **Reap on quit** — add `reapPartialSyncOnShutdown()` to `MithrilController` (singleton `:464`), guarded by
    `isPartialSyncActive()` (`:199-204`), calling `killProcessTree(service child, 'SIGKILL', { sync: true })` via a
    small service method (e.g. `forceKillForShutdown()`) rather than reaching into the private slot, fully try/caught so
    it can never throw during shutdown. Call it ONCE from `safeExit()` (`source/main/index.ts:87-129`) immediately after
    `pauseActiveDownloads()` — that single insertion precedes all three exit paths, including the STOPPING branch
    (`:103-108`) that returns **without** `safeExitWithCode`. During an active partial sync the node is STOPPED, so the
    STOPPED early return is the live exit path. **The Windows shutdown kill must be `sync: true` (`execSync`)** because
    `safeExit` reaches `process.exit()` inside `safeExitWithCode`'s stream-end callback (`safeExitWithCode.ts:10-14`);
    POSIX `process.kill(-pid)` is synchronous so it is safe. Post-2A the slot reliably holds the live
    download/conversion child, so the old find-process contingency sweep stays OUT of the diff (re-open only if manual
    PR-testing shows an escaped descendant → branch ticket). **PR synergy (precise — it depends on 2A, and the
    floor-holds-the-slot claim is not a code guarantee):** this reap reads the slot via `forceKillForShutdown()`, so its
    coverage **DEPENDS on 2A's durable slot** — the reap alone would NOT have caught this repro's *untracked* survivor
    (pid 229730 with a nulled slot). The synergy holds precisely **when `start()` is still awaiting the wedged child**
    (as in the captured repro — the download `await` at `:591` never resolved, so `start()` had not returned and the
    slot still names the live child). It is **not** a code guarantee that the `abandonCancel` floor always leaves the
    slot populated: `start()`'s catch (`:288`) hits `if (this._isCancelled) return;` (`:289-291`) and returns *through*
    the `finally` (`:303-305`), whose `_clearRuntimeWorkState` nulls the slot (`:773`); when `start()` HAS returned the
    slot is null and the reap is a harmless no-op (and in that path the tracked child was already killed/closed).
    Coverage-breadth: `isPartialSyncActive()` is
    `chainStorageCoordinator.isPartialSyncInProgress() || this._partialSyncStatus.status !== 'idle'`
    (`MithrilController.ts:199-204`) — true for EVERY non-idle status (incl. `completed` and `cancelled`, not just
    `failed`), which is *broader* than stated and harmless (in the extra states the slot is null, so the reap no-ops).
    Net: this reap closes the "survives app exit after a floored cancel" leg **only because 2A keeps the slot pointing at
    the still-awaited child**. **Files: `killProcessTree.ts` (+ new spec), `mithrilCommandRunner.ts` (+ spec),
    `MithrilController.ts` (+ new spec), `index.ts`.**

**Part 3 (evidence-gated; expected NOT needed — only if the re-repro after 2A shows `'close'` still hangs after a
confirmed direct-child kill):**
13. Add a direct-child `'exit'`-derived settle signal the coordinator can observe (e.g. resolve a per-run "process
    exited" deferred that `_awaitRunSettledBounded` races against `run`), leaving the runner's `stdout`-bearing promise
    resolving on `'close'` for the metadata paths. **Never** swap the runner's resolve from `'close'` to `'exit'` (the
    metadata JSON reads depend on fully drained stdout). Keep this change out of the diff if the re-repro shows 2A
    already makes `'close'` fire (the expected outcome, since post-2A cancel kills the direct child whose pipes close).

Diff shape: **Part 1 (LANDED)** — logging only across `MithrilPartialSyncService.ts`, `chainStorageCoordinator.ts`,
`mithrilCommandRunner.ts` (+ the service spec's logging mock). **Part 2A** — `MithrilPartialSyncService.ts` (+ spec)
only: the opt-in `trackAsCancelable` flag threaded from the download + the two `start()` reads, conversion unchanged,
plus the unconditional `hadChild` cancel-entry log (so re-repro #1 observes the slot directly).
**Part 2B** — `MithrilPartialSyncStore.ts` (+ spec) probe-gating guard + design-comment update, `MithrilController.ts`
(+ new spec) main-side guard. **Part 2C** — 1 new file (`killProcessTree.ts` + spec), `detached` flag on 2 spawns, 3
kill-site swaps, `MithrilController.reapPartialSyncOnShutdown()`, 1 `safeExit` call. **Part 3** — conditional; a small
coordinator settle signal or nothing. No new status/channel/message, no Boundary-model change; 2B is a documented
renderer-store behavior change, no i18n/theme/user-facing copy.

### Why this is safe / invariant-preserving
- **Boundary-A-only cancel unchanged.** CAT-E touches termination mechanics and logging only; it does not move any
  checkpoint, does not change `cancel()`'s `installing`/`finalizing` guard (`:325-327`, post-CAT-D), and CP-D still sits before the
  cutover marker. Cancel can still never abort a live cutover.
- **No status / PRD-contract / Boundary-model change.** `cancelling`/`cancelled`/`abandonCancel` and the recovery set
  `['retry','restart-normal']` are all unchanged; CAT-E makes the *existing* teardown actually complete.
- **2A slot fix is behavior-neutral on the success path.** Omitting `onProcess` for metadata reads only stops those
  children from touching `_currentProcess`; `_logStream` is write-only so dropping its metadata log-stream registration
  changes nothing observable; the download/conversion (and the `start()`-phase reads, per B1) still register and stay
  killable, and the late-kill branch still covers a download/conversion child that registers after `_isCancelled` flips.
  On a normal (non-cancel) run nothing changes.
- **2A benign behavior change on the cancel path (recorded).** Post-2A, POST-cancel read-only metadata probes no longer
  register via `onProcess`, so the late-kill guard no longer SIGTERMs them: they now run to completion instead of being
  killed mid-flight with a JSON-parse-fail `warn` (the observed pids 230842/230843 in the capture were late-killed
  pre-2A). Harmless — they touch nothing but the ephemeral staging read — and 2B gates most of them anyway while status
  is `cancelled`.
- **2B probe-gating engages no locked invariant.** Skipping the behind-ness probe while a sync is working/`cancelled`
  removes a concurrent metadata spawn whose derived figure is consumed on screen ONLY by the Daedalus Diagnostics dialog
  (`isPartialSyncEnabled`/`isSignificantlyBehind`/`certifiedEpoch` via `DaedalusDiagnosticsDialog.tsx:133-146`, rendered
  `DaedalusDiagnostics.tsx:573-577`) — which under 2B **freezes at its last pre-run values if opened mid-run** (accepted:
  during a partial sync the node is STOPPED, so the tips are not advancing — a frozen figure is not wrong-and-moving —
  and the dialog's Mithril action is already blocked while working, `isMithrilActionBlocked`,
  `DaedalusDiagnostics.tsx:581-582`); the proactive prompt is genuinely inert during a run (gated `status === 'idle'` and
  `!mithrilAttemptStartedThisSession`, offer-signal #4 untouched), the store is the availability channel's only
  requester, `getPartialSyncBehindness` already degrades to `{ isSignificantlyBehind: false }` on failure, and the poll
  resumes (slows-never-stops) once status leaves the active set. It is a documented behavior change to the store's
  "re-fetch on EVERY tick" design comment, with no user-facing copy.
- **Success path inert.** Logging is passive. `detached` changes only the child's process-group membership, not its
  stdio or exit semantics; on a normal (non-cancel) run no kill is issued, so `killProcessTree` is never called and the
  run resolves on `'close'` exactly as today. `reapPartialSyncOnShutdown` is a no-op unless `isPartialSyncActive()`.
- **`detached` is POSIX-only.** Gated `!environment.isWindows` to avoid the documented Windows launcher breakage
  (`CardanoSelfnodeLauncher.ts:112` — "detaching breaks Windows launching the local-cluster.exe + cardano-launcher");
  Windows keeps its default spawn and reaps the tree via `taskkill /t /f`, matching `CardanoNode`. macOS/Linux get the
  process group. **On Windows the SIGTERM→SIGKILL distinction collapses** — `taskkill /f` is always a hard kill — so the
  graceful-cancel stage (`cancel()` → SIGTERM) becomes a force kill and the later `forceKill` re-`taskkill`s a
  now-dead pid (a benign no-op, with a narrow pid-reuse window). This is acceptable for staged-only mid-download work
  and matches `CardanoNode`'s Windows behavior; it is documented in the `killProcessTree` contract, not silent.
- **Shared runner — bootstrap invariant #7: in-app behavior unchanged; dev-terminal signal delivery changes
  (accepted).** `runBinary`/`runCommand` are **shared with `MithrilBootstrapService`** (the fast-sync flow CAT-D
  kept separate). Adding `detached: !environment.isWindows` also makes the *bootstrap* `mithril-client` a POSIX
  group leader. **In-app behavior is unchanged** (bootstrap never calls `killProcessTree`, the shutdown reap is
  partial-sync-only / `isPartialSyncActive()`-guarded, and the `'close'` resolve / exit code / logStream path is
  identical), **but detachment does change *terminal* signal delivery on POSIX:** the bootstrap (and partial-sync)
  `mithril-client` becomes its own session leader, so a dev-terminal Ctrl+C (SIGINT to the foreground group) or
  terminal close (SIGHUP) no longer terminates it, while today it does. Production GUI launches have no controlling
  terminal and are unaffected. No-bootstrap-regression #7 is preserved for the launch/success/cancel paths; the
  dev-terminal signal path is an **accepted regression** (see Risks).
- **Group-kill is staged-only-safe.** Both binaries write only into the ephemeral `mithril-partial-sync` staging dir;
  `finalizeCancel` → `_cleanupPartialSyncArtifacts` `fs.remove`s it wholesale, so killing mid-work corrupts nothing on
  the live chain (staged-only invariant #3).
- **Shutdown reap is best-effort.** Fully try/caught and guarded by `isPartialSyncActive()`; it can neither throw nor
  block `safeExit`, which still `process.exit()`s. Worst case it degrades to today's behavior (orphan), never worse.
- **Part 3 preserves the stdout contract.** The runner keeps resolving its `stdout`-bearing promise on `'close'`, so
  `resolveLatestSnapshotMetadata`'s JSON parse of `snapshot list`/`show` output is never truncated; the settle signal is
  additive.

### Tests
**`MithrilPartialSyncService.spec.ts` — the primary automated proof (extend the `../utils/logging` mock to keep
asserting `info`/`warn`; reuse the `{ pid, kill }` fake-child + `onProcess` pattern):**
1. **Slot-clobber regression (2A — KEY NEW COVERAGE; the primary automated proof of the root cause).** With the runner
   mocked, hold a download `_runCommand` in flight after its `onProcess(downloadChild)`; then run a metadata read
   (`getPartialSyncBehindness()` or `_listSnapshotsRaw()`) to completion. Post-fix, assert the metadata invocation
   receives **NO `onProcess` callback at all** (so it can neither overwrite nor null the slot — simulating the old
   `onProcess(metaChild)` → `onProcess(null)` clobber if the mock still received callbacks); then assert `cancel()` and
   `forceKill()` still target the durable `downloadChild` (its `kill` is called — not a no-op on null, not the metadata
   child). Also assert the conversion (`_runBinary`) child still registers and is killable.
2. **B1-choice pin (2A).** A `start()`-phase metadata read (`resolveLatestSnapshotMetadata` reached via `:186`/`:224`)
   DOES register into `_currentProcess` (threaded `trackAsCancelable: true`), so a cancel during `preparing` still
   routes a kill to the in-flight metadata child — pinning the chosen B1 option so a future refactor cannot silently
   drop cancel-at-`preparing` coverage.
3. **Kill-routing (2C) + unconditional cancel-entry log (2A).** `cancel()` on a live child routes through
   `killProcessTree` with `'SIGTERM'` and emits the cancel-entry `info` log `{ status, pid, hadChild: true }`; **a
   `cancel()` that finds an EMPTY slot (no `_currentProcess`) still emits the entry log with `{ hadChild: false, pid: null }`**
   — pinning that the entry line is UNCONDITIONAL (fires even with no child to kill, the case Part 1's in-branch line
   missed); `forceKill()` routes through with `'SIGKILL'`; a late-spawn child (`_trackCurrentProcess(fakeChild)` with
   `_isCancelled = true`) routes the immediate kill through `killProcessTree`; `abandonCancel()` and `finalizeCancel()`
   (success + catch) each emit their log line.

**`MithrilPartialSyncStore.spec.ts` — probe-gating (2B):**
4. For each of `downloading`, `converting`, `cancelling`, `cancelled` (+ one sibling working state, e.g. `verifying`),
   `_refreshAvailability()` returns WITHOUT calling `mithrilPartialSyncAvailabilityChannel.request()` and WITHOUT
   mutating `_isRefreshingAvailability` / `_consecutiveStableReads`; for `idle`/`failed`/`completed` the request fires.
5. A skipped tick must NOT re-arm or clear `_availabilityRefreshInterval` (the interval keeps ticking).

**`MithrilController.spec.ts` (new) — main-side guard (2B) + reap (2C):**
6. Active status → `getPartialSyncAvailability` returns `{ isEnabled, isSignificantlyBehind: false }` WITHOUT calling
   `service.getPartialSyncBehindness`.
7. `reapPartialSyncOnShutdown()` — active → sync-mode SIGKILL issued (via `killProcessTree(..., { sync: true })`);
   inactive → no-op; a throwing kill is swallowed (never rethrows into `safeExit`).

**`killProcessTree.spec.ts` (new) — the helper (2C):**
8. POSIX → `process.kill(-pid, signal)`; Windows `sync: true` → `execSync('taskkill /pid <pid> /t /f', { stdio: 'ignore', timeout: 5000, windowsHide: true })`;
   Windows async default → `exec` (non-blocking); null/absent pid → no-op; group-kill throw → `child.kill(signal)`
   fallback; fallback throw → swallowed + warned. Use `expect.objectContaining` for the option-object assertions
   (prettier 2.1.2 oscillates on inline `('str', {obj})` matcher literals).

**`mithrilCommandRunner.spec.ts` — detached (2C):**
9. Both spawns pass `detached: true` when `environment.isWindows` is `false` (POSIX) and falsy when `true` (Windows).
   Part 1's `'exit'` log line and the un-truncated `'close'` resolve already have coverage from the landed Part 1.

**`chainStorageCoordinator.spec.ts` — settle branch (regression pins; reuse the fake-timer bounded-join idiom):**
10. Run settles within the join → `finalizeCancel` (not `abandonCancel`) + the settled=true `info` log; never settles →
    `forceKill` escalation + settled=false → `abandonCancel` `warn`. *(Part 3, only if implemented)* a run whose direct
    child `'exit'`s while `'close'` is withheld still settles via the exit-derived signal → `finalizeCancel`.

**Test-scope honesty (amended — state explicitly so no one over-reads the jest pass):** the original defect's *full
physical chain* — a real pipe-holding child wedging `'close'` into a false `abandonCancel` — remains **not
unit-testable** with a mocked EventEmitter runner (no real pipe/descendant), and the Linux/WSL2 operator gate is that
proof. **BUT the slot-clobber ITSELF is now unit-testable** (test #1) and is the primary automated regression proof for
the confirmed root cause; probe-gating, `killProcessTree`, the reap wiring, and the coordinator branch are covered too.
**Mock mechanics:** the runner/`killProcessTree` specs vary `environment.isWindows` via the established **mutable
`jest.mock('../environment', …)` object reassigned per test** (`mithrilCommandRunner.spec.ts:37-43`, `:103`, `:172`, …)
— **NOT `jest.isolateModules`** (correct any isolateModules mention for these two specs); the `killProcessTree` spec
mocks `child_process` (`exec`/`execSync`) the same way, plus a `process.kill` spy, and asserts option objects with
`expect.objectContaining`. The new `MithrilController.spec.ts` must `jest.mock('../utils/chainStorageCoordinator')`,
`'../utils/logging'`, and `'./MithrilPartialSyncService'` (or spy on `getMithrilController()._partialSyncService`)
**before** loading the module. `MithrilPartialSyncStore.spec.ts` is a **renderer** spec — the Node v24 renderer
verify-env caveat applies to it (regen `.scss.d.ts` via typed-scss-modules, use the gitignored identity-obj-proxy jest
sidecar); the main-process 2A/2C specs need neither. Only `killProcessTree.spec.ts` and `MithrilController.spec.ts` are
NEW spec paths that must be added to the tasks-JSON `targetPaths`; `MithrilPartialSyncStore.spec.ts` **already exists**
(~34.7 KB — 2B extends it, does not create it), so confirm it is already listed in `targetPaths` rather than adding it.

### i18n
- None (main-process control-flow + developer-facing logs only; no user-facing copy added or changed — Mithril-Sync
  vocabulary guardrail is not engaged).

### Verification / checks
- `yarn compile` + `yarn lint` + touched-file `yarn prettier:check` after each part lands (use `expect.objectContaining`
  in specs to avoid the prettier 2.1.2 matcher-literal oscillation; two `mithrilCommandRunner*` files carry pre-existing
  HEAD formatting drift — verify a hunk is yours before reformatting).
- `yarn test:jest` scoped to the touched specs: `MithrilPartialSyncService.spec.ts`, `MithrilPartialSyncStore.spec.ts`
  (2B), `killProcessTree.spec.ts` (new), `MithrilController.spec.ts` (new), `mithrilCommandRunner.spec.ts`,
  `chainStorageCoordinator.spec.ts`.
- **Node v24 renderer verify-env caveat applies ONLY to the 2B renderer-store spec** (`MithrilPartialSyncStore.spec.ts`):
  regenerate `.scss.d.ts` via typed-scss-modules before trusting tsc and use the gitignored identity-obj-proxy jest
  sidecar; do not treat those failures as regressions. The main-process 2A/2C specs need neither.

**Cross-platform (locked decision 3): the Linux/WSL2 operator gate is the wave's proof; NO QA-matrix gates; Windows and
macOS concerns are DOCUMENTED here and verified in the manual PR-testing phase; per-env findings become separate branch
tickets.**
- **Platform-independent by construction:** 2A is pure JS slot lifecycle — no OS branch. The cancel → cleanup → "prompt
  for sync options" flow is platform-independent (the offered recovery actions come from the backend's
  `allowedRecoveryActions`; the "restart Daedalus" floor is reachable only via `abandonCancel()`'s empty recovery set;
  no OS branch in the store or overlay). So the fix works on Windows/macOS/Linux by construction; the Linux/WSL2
  operator gate is the proof.
- **Windows concerns (documented, not gated here):** `execSync` UI-freeze → the async-interactive / sync-shutdown split
  (§2C step 9); pid-reuse on the ~15s-later `forceKill` re-taskkill (candidate mitigation: skip when
  `child.exitCode !== null || child.killed`); `taskkill.exe` PATH resolution in a packaged build (`exec`/`execSync`
  inherit `process.env`, NOT the runner's repaired spawn PATH — prefer the absolute `System32\taskkill.exe` if a
  packaged env ships a stripped PATH); `/t` reaps the visible tree but a job-breakaway worker escapes it (no Job Object
  here).
- **macOS concerns (documented, not gated here):** POSIX, so kill mechanics == Linux (`process.kill(-pid)`), but
  UNPROVEN by the Linux gate: hardened-runtime/notarized packaged spawn of the mithril binaries, the `.app`
  before-quit → `safeExit` reap path (Cmd-Q/menu quit vs window close), and whether the macOS mithril-client build
  re-groups a descendant (which would escape the group kill).
- **Operator (verify-only, preprod or Linux/WSL2 — the load-bearing proof, staged):**
  1. **After 2A:** cancel during `downloading`. Confirm via Part-1 logging + `ps`: the new unconditional cancel-entry
     log shows **`hadChild: true`** (the slot now durably holds the download child — the slot state is now directly
     observed at cancel, not inferred), SIGTERM sent with a real pid, the `[mithril] child exited/closed` lines fire,
     the coordinator logs "run settled after cancel; finalizing", the UI reaches terminal **cancelled** offering
     Retry / Restart-normal, and no `mithril-client` survives. This is the wave's proof gate (re-repro #1).
  2. **After 2B + 2C:** cancel during `preparing`, `downloading`, and `converting`; each reaches terminal **cancelled**
     with **no** "restart Daedalus"/"Downloading the Mithril snapshot failed" floor, **no** `mithril-client` alive after
     the prompt (`ps`), staging dir removed. Then start a sync and **quit** mid-download WITHOUT cancel; confirm **no**
     `mithril-client` / `snapshot-converter` survives app exit (re-repro #2). Record the observed teardown time (feeds
     702c U1).
  3. **Part 3 decision:** implement the settle-on-exit signal ONLY if re-repro #1 or #2 shows the run still unsettled
     (`'close'` hang) after a confirmed kill.
- **On-platform Windows/macOS verification is deferred to the manual PR-testing phase** (not an in-wave QA-matrix gate);
  any per-env finding becomes a separate branch ticket.

## Risks (category-specific)
- **Process-*tree/group* termination — RESOLVED to hypothesis (iii) `_currentProcess` slot-clobber (operator gate,
  2026-07-02; by code-plus-log inference — see the RESOLVED box's evidence-quality note).** CAT-D signalled only the
  direct child pid and assumed modern `mithril-client`/`snapshot-converter` are
  single-process. Operator QA falsified the "harmless" assumption (a live `mithril-client` surviving the failure prompt
  AND app exit), and Part 1's diagnostic re-repro then RESOLVED *which* mechanism among the three hypotheses:
  **the operative root cause is (iii) a `_currentProcess` slot-clobber** — the concurrent ~30s behind-ness metadata
  probe overwrites, then nulls, the download's slot (via the unconditional `_trackCurrentProcess` seam and the runner's
  `onProcess(null)` on the probe's clean `'close'`), so at cancel the slot is empty (a **code-plus-log inference**, not
  a direct observation — Part 1's cancel-entry log fires only inside the real-kill branch, so the captured repro carries
  no cancel-entry / `hadChild` line; the emptiness is deduced from download pid 229730 appearing exactly once with no
  kill line against it) and the SIGTERM/SIGKILL gated on `if (this._currentProcess)` sends nothing → the download keeps
  running → the coordinator's 15s join expires into `abandonCancel` (the abandoned-cancel survivor the `ps` capture
  directly shows — but the snapshot was taken while Daedalus was still alive, so the **survives-app-exit** leg is NOT
  yet observed and stays gated on re-repro #2). **(ii) re-grouping (`setsid`/`setpgid`) is RULED OUT by `ps`**
  (survivor pgid 227499 == Daedalus's pgid, sess 218848 == Daedalus's sess, ppid 228282 == Daedalus's main → the
  in-group direct child; survivor pid 229730). **(i)'s in-group topology is present but was NOT the operative failure**
  — detached + group-kill alone would still miss a survivor whose naming slot was nulled before cancel ran.
  **Consequence:** the PRIMARY fix is the slot lifecycle (**2A**); **group-kill (`detached` + `killProcessTree`) is
  demoted to 2C defense-in-depth** — correct for a hypothetical future forking mithril-client build, and load-bearing
  for the "quit mid-download without cancel" reap that 2A cannot cover (`cancel()`/`forceKill()` never run on a plain
  quit). `detached` stays **POSIX-only** (Windows reaps via `taskkill /t /f`, gated off `detached` to avoid the
  `CardanoSelfnodeLauncher.ts:112` launcher breakage); the POSIX path is proven by this wave's Linux/WSL2 operator gate,
  with Windows/macOS on-platform verification deferred to the manual PR-testing phase (see Verification / checks), and
  per-env findings filed as separate branch tickets. If the manual PR-testing phase ever surfaces a re-grouped/escaped
  descendant (contradicting the Linux `ps` evidence on another platform), the descendant-tree-walk / identity-based
  `find-process` contingency (previously scoped here) is re-opened as its own branch ticket — it is NOT in this diff.
- **Sibling `MithrilBootstrapService` slot-clobber (separate branch ticket, NOT this diff).** `MithrilBootstrapService`
  has the same unconditional-`onProcess` slot seam shared by its metadata reads and its download — the same clobber
  class exists for cancel-during-bootstrap. Per the ticketing model (locked decision 3) it is filed separately, not
  fixed in CAT-E.
- **2B back-off staleness window (documented).** With 2B, `_consecutiveStableReads`/cadence freeze during a run; if the
  poll had backed off to the 5-min cadence pre-run, the first post-`idle` probe can be up to ~5 min out. Accepted;
  candidate follow-up: re-arm the fast cadence on the working→terminal/idle transition (branch ticket if it matters in
  practice). **Cache-TTL nuance (why the clobber wasn't a one-shot pre-2A):** the behind-ness caches carry a 5-min TTL
  (`:76`), so a download longer than 5 min produces REPEAT probe cache-misses — *multiple* clobber windows pre-2A, not
  just the first poll after `start()`/`cancel()` invalidation. Both 2A (durable slot) and 2B (no probe at all during a
  run) neutralize this.
- **Detached POSIX children lose dev-terminal signal delivery (accepted dev-only regression, CAT-E).**
  `detached: !environment.isWindows` applies to both shared-runner spawns, so the *bootstrap* `mithril-client` also
  becomes its own session/group leader: a dev-terminal Ctrl+C (SIGINT to the foreground group) or terminal close
  (SIGHUP) no longer kills it, and bootstrap has no group-kill site and no shutdown reap — a multi-GB fast-sync
  download can be orphaned in dev workflows. Production GUI launches have no controlling terminal and are
  unaffected. Accepted explicitly. Optional hardening follow-up: widen the shutdown reap from
  `isPartialSyncActive()` to any live tracked mithril child (bootstrap's `_currentProcess` too) — noting the reap
  only fires when the app's quit path (`before-quit` → `safeExit`) actually runs, so it does not cover a raw SIGINT
  that kills Electron without `before-quit`; this documented acknowledgment, not the reap, is the floor. Do **not**
  respond by un-detaching the bootstrap spawn path (a per-call `detached` flag would fork the shared runner's spawn
  semantics for no production benefit and reintroduce the divergence invariant #7 guards against).
- **Settle-on-`'exit'` vs `'close'` (CAT-E Part 3, evidence-gated; expected NOT needed).** Post-2A, cancel SIGTERMs the
  direct download child (the `ps`-confirmed survivor), whose stdio pipes then close, so `'close'` is *expected* to fire
  and the run to settle without any settle-semantics change. The risky change is **deferred behind the re-repro after
  2A**: implemented only if `'close'` still hangs after a confirmed kill, and even then as an additive child-`'exit'`
  signal that never truncates the `stdout` the metadata reads parse (**never** a `'close'`→`'exit'` swap) — recorded
  here so a reviewer can hold CAT-E to *not* swapping the runner's resolve event speculatively. **Trade-off to state
  plainly:** an `'exit'`-settle is *semantically weaker* than `'close'` — it means the tracked child died, not that the
  whole tree's stdio reached EOF. Hypothesis (ii) (an escaped/re-grouped descendant) is **ruled out on Linux by the
  `ps` evidence**, so this trade-off is only a hypothetical-future concern (a forking mithril-client build), for which
  the 2C group/tree reap — not an exit-settle — would remain authoritative for actual termination.
- **Quit-during-wedge lingering files — addressed by CAT-E's 2C reap.** Post-2A the `_currentProcess` slot reliably
  holds the live download/conversion child, so `safeExit()`'s `reapPartialSyncOnShutdown()` (2C) kills it on quit,
  closing the in-session process leak; the quit-mid-download operator check (re-repro #2) is the gate that proves it.
  **Selfnode caveat:** the `before-quit` selfnode keep-cluster branches (`index.ts:297`,`:319`) return
  `safeExitWithCode(0)` directly, bypassing `safeExit()`, so the reap does NOT fire there — dev-only, no partial sync,
  so nothing to reap; noted so nobody chases a "reap didn't fire" ghost in selfnode runs. The *staging-dir* cleanup on a
  hard quit is still not run in-session (cleanup is owned by `finalizeCancel`, which a quit bypasses), but that dir is
  re-wiped on the next attempt's `preparePartialSyncStagingDirectory` pre-wipe (`fs.remove`) and startup Boundary-A
  recovery handles cutover-marked interruptions. A broader startup "orphaned-staging sweep" remains **deferred** (needs
  managed-chain-path resolution at startup with no active service; low value once CAT-D/CAT-E eliminate the wedge and
  the live child in the normal flow). File a follow-up if field evidence shows persistent orphans.

## Operator / verify-only gates
(Moved from the master's wave-level Verification plan.)
- **CAT-E (automated):** `yarn compile` + touched-file lint/prettier + `yarn test:jest` for
  `MithrilPartialSyncService.spec.ts` (incl. the slot-clobber regression), `MithrilPartialSyncStore.spec.ts` (2B),
  `killProcessTree.spec.ts` (new), `MithrilController.spec.ts` (new), `mithrilCommandRunner.spec.ts`, and
  `chainStorageCoordinator.spec.ts` green (the Node v24 renderer verify-env caveat applies ONLY to the 2B
  renderer-store spec).
- **CAT-E (operator, verify-only, Linux/WSL2 — the wave's proof; RESOLVED for re-repro #1):** re-repro #1 (already
  RESOLVED) confirmed hypothesis (iii) slot-clobber (captured `ps`/logs recorded in `task-ux-702d-research.md`). After
  2A → re-repro #1 pattern (cancel during `downloading`: SIGTERM reaches a real pid, `'close'`/`'exit'` fire, terminal
  **cancelled**, no survivor); after 2B + 2C → re-repro #2 (cancel during `preparing`/`downloading`/`converting`: each
  reaches terminal **cancelled**, **no** "restart Daedalus"/"Downloading the Mithril snapshot failed" floor, **no**
  `mithril-client` alive after the prompt (`ps`), staging dir removed; then **quit mid-download** and confirm **no**
  `mithril-client`/`snapshot-converter` survives app exit). Part 3 implemented only if a `'close'` hang persists after a
  confirmed kill.
- **NO QA-matrix gate.** Windows (`taskkill /t /f`, the async/sync split, pid-reuse, packaged `taskkill.exe` PATH) and
  macOS (packaged hardened-runtime spawn, `.app` before-quit reap, possible re-grouped descendant) concerns are
  DOCUMENTED (see Verification / checks + Risks) and verified in the **manual PR-testing phase**; any per-env finding
  becomes a **separate branch ticket**, not an in-wave gate.

## Cross-category coupling notes
- Builds on CAT-D's `_trackCurrentProcess` / checkpoint machinery (see `task-ux-702d-cat-d.md`); lands strictly
  after CAT-D.
- Shares the runner (`mithrilCommandRunner.ts`) with `MithrilBootstrapService` — invariant #7; see the Why-safe
  shared-runner bullet above and the detached dev-terminal regression bullet in Risks.
- Part-1 re-repro evidence is recorded in `task-ux-702d-research.md` (finding #8, RESOLVED: hypothesis (iii)
  slot-clobber confirmed).
- Verify-command note (702b-style): 2A/2C are main-process TS + jest (no `.scss.d.ts`, Node v24 caveat does **not**
  apply), but **2B's `MithrilPartialSyncStore.spec.ts` is a renderer spec**, so the Node v24 renderer verify-env caveat
  DOES apply to it (regen `.scss.d.ts` + identity-obj-proxy sidecar).
- 2B gates the behind-ness availability probe during active work — cross-links the availability-probe CPU-cost note
  (702b CAT-H): removing the per-tick probe during a run also removes its `checkDiskSpace` fork + `immutable/` readdir.

## Progress

- **2026-07-02T07:13:01Z — Part 1 (cancel-lifecycle logging) IMPLEMENTED** on the working tree (uncommitted; folds into
  the single 702d commit). Non-behavioral / pure observability: `logger.info`/`warn` added at all prescribed sites in
  `mithrilCommandRunner.ts` (`runBinary`+`runCommand` child spawned/exited/closed, with additive logging-only `'exit'`
  listeners and the `'close'` resolve left un-truncated and un-swapped), `MithrilPartialSyncService.ts` (cancel real-kill
  branch, `_trackCurrentProcess` late-kill, `finalizeCancel` entry/success/previously-silent-catch, `abandonCancel`
  restart floor), and `chainStorageCoordinator.ts` (join-timeout/`forceKill` escalation + `cancelPartialSync`
  settled/unsettled branch-decision). The three in-scope specs were extended with matching assertions (+ a `./logging`
  mock added to the coordinator spec). Verified: `yarn compile` PASS, `yarn lint` PASS (warnings only), the three specs
  PASS (17/55/41); no scope-lock deviations (no `detached`, no `killProcessTree.ts`, no `reapPartialSyncOnShutdown`, no
  `index.ts`/`safeExitWithCode.ts` touch, resolve stays `'close'`). See `task-ux-702d-impl-review.md` (2026-07-02
  Implementation + Code Review: approved).
- **2026-07-02 — operator gate RESOLVED; Part-2/3 plan re-scoped and solidified (Parts 2 & 3 still PLANNED, not
  implemented).** The Linux/WSL2 cancel-at-`downloading` re-repro captured decisive evidence (`ps`/logs in
  `task-ux-702d-research.md` finding #8: survivor pid 229730, ppid 228282 == Daedalus main, pgid 227499 == Daedalus's
  pgid, sess 218848 == Daedalus's sess; the 15s coordinator join → `abandonCancel` floor). Read through the pre-fix
  interpretation table, this **RESOLVED the Part-2 mechanism**: **hypothesis (iii) `_currentProcess` slot-clobber is
  CONFIRMED** as the operative root cause (the concurrent ~30s behind-ness probe overwrites then nulls the download's
  slot), **(ii) re-grouping is RULED OUT** by the in-group `ps` topology, and **(i)'s group topology is present but was
  NOT the operative failure**. Part 2 is re-scoped, plan-reviewed (APPROVE-WITH-CHANGES; B1/B2 folded in), and
  solidified as **2A (slot-lifecycle fix, PRIMARY — opt-in `trackAsCancelable` threaded from the download + the two
  `start()` reads, NEVER inferred from `_activeWorkDir`) + 2B (gate the behind-ness probe during active work) + 2C
  (detached + `killProcessTree` async/sync split + reap-on-quit, defense-in-depth)**, with Part 3 (settle-on-exit) still
  evidence-gated and expected NOT needed. **Status stays pending** (interactive_validation): Parts 2/3 are
  PLANNED-not-implemented; the ordered operator re-repros (#1 after 2A, #2 after 2B+2C) remain the proof gates. See the
  RESOLVED box in the Rationale, the re-scoped Implementation §2A/2B/2C, and `task-ux-702d-research.md` finding #8 (+ the
  new slot-clobber and sibling-bootstrap findings).
- **2026-07-02T09:42:45Z — grounded plan review of Part 2 (2A/2B/2C) folded in (Parts 2/3 still PLANNED, not
  implemented).** Four Opus exploration agents ran against the live post-Part-1 tree + the captured logs (`logs/ps.log`,
  `logs/pub/Daedalus.json`, `logs/mithril-partial-sync.log`); every load-bearing mechanical claim was verified — the
  threading list is exhaustive (only **two** `_currentProcess` writers, `:662`/`:1221`), the 2B insertion point is
  exact, the three kill sites are mechanical, and no mithril spawn exists outside the shared runner. Corrections C1–C8
  folded in: **(C1)** evidence-quality reframe of the RESOLVED verdict — the slot-emptiness AT cancel is a code-plus-log
  *inference* (Part 1's cancel-entry log is silent on an empty slot; the captured repro has no cancel-entry / `hadChild`
  line), and the `ps` snapshots were taken **while Daedalus was still alive**, so `ps.log` proves the abandoned-cancel
  survivor but NOT the "survives app exit" leg (that stays gated on re-repro #2); the clobber probe fired ~22 s after
  download start (~30 s is the polling cadence). **(C2)** an unconditional `hadChild` cancel-entry log added to 2A so
  re-repro #1 directly observes the slot state (mirrored into Diff shape, Tests, and re-repro #1). **(C3)** the step-12
  reap-synergy premise fixed — the floor-holds-the-slot claim is not a code guarantee (`start()`'s catch returns through
  the `finally` that nulls the slot); the reap DEPENDS on 2A's durable slot, and `isPartialSyncActive()` is broader than
  "`failed`" (every non-idle status). **(C4)** `MithrilPartialSyncStore.spec.ts` is NOT new (~34.7 KB — 2B extends it);
  only `killProcessTree.spec.ts` and `MithrilController.spec.ts` are new. **(C5)** the Daedalus Diagnostics dialog IS a
  mid-run consumer of the derived figure (freezes at pre-run values; accepted), replacing the flat "nothing on screen
  consumes" claims. **(C6)** `isMithrilPartialSyncWorkingStatus` must be imported into `MithrilController.ts` (today only
  `isMithrilPartialSyncBlockingNodeStart` is), reading the `this._partialSyncStatus.status` seam. **(C7)** the
  service-local options-field is now the PRIMARY flag variant (the two shared raw methods call `_runCommand` without the
  positional `workDir`). **(C8)** the `_logStream` write-only coupling, the 5-min cache-TTL repeat-clobber nuance, the
  benign post-cancel metadata-probe change, the qualified "single shutdown funnel" (fatal/startup paths bypass
  `safeExit`), and the live post-Part-1 line-number shifts were all recorded. Parts 2/3 remain PLANNED-not-implemented;
  the ordered operator re-repros stay the proof gates, with re-repro #1 now **directly observing** the slot via the new
  cancel-entry log rather than inferring it.
- **2026-07-02 — operator re-repro #1 CONFIRMED; CAT-E ACCEPTED; wave CLOSED.** The operator re-ran the cancel flow
  on the live app against the landed Parts 1 + 2A/2B/2C: cancelling out of the Mithril partial sync progress overlay
  now reaches the terminal **cancelled** prompt without error — no "restart Daedalus" / "Downloading the Mithril
  snapshot failed" floor — and **every option on the cancel prompt works as expected**. This was the last major issue
  spotted for the session. **Part 3 (exit-settle) stays UNIMPLEMENTED** — the confirmed behavior required no `'close'`
  hang workaround. **Re-repro #2 (quit-mid-download) was not separately re-run this session**; it rides with the
  documented Windows/macOS teardown concerns into the `task-ux-702` manual PR-testing/QA phase (per the NO-QA-matrix
  gate rule above, that deferral does not hold the wave). `task-ux-702d` → **completed** (tasks JSON `v1.10.1`,
  `completedAt: 2026-07-02`); the deployment QA gate `task-ux-702` is unblocked.
