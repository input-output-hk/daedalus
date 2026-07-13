# task-ux-702d — Plan Review (append-only)

> Append-only chronological Planner/Critiquer transcript for the `task-ux-702d` ad-hoc finalization cleanup wave.
> This log carries the wave's Planner/Critiquer entries for ALL CATs. Historical note: the CAT-A/B/C Planner
> entries and the CAT-D-revision Planner entry originally lived at the tail of `task-ux-702d.md` and were
> relocated here on 2026-07-01 with provenance notes (appended at the end, original timestamps preserved); the
> chronological transcript below therefore starts at the CAT-D plan-critique pass. Each entry: speaker label,
> ISO-8601 UTC `Timestamp:`, outcome. Critiquer entries end with `Decision: approved` or `Decision: requires_changes`.

---

## Planner

Timestamp: 2026-07-01T00:00:00Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-D)

Appended **CAT-D** — the wave's first correctness category. Operator manual testing (post-702c) reported that
canceling a Mithril Sync shows the "Cleaning up…" frame but then continues the Mithril process in the background (seen
in logs), surfaces a "Downloading the Mithril snapshot failed" prompt offering only logs+quit, pops back to the sync
page after a wait and continues, and can leave the `mithril-partial-sync` staging directory lingering on quit.

Grounded via two Explore subagents (renderer + main-process) and first-hand reads. Root cause: 702c CAT-A built the
correct cancel machinery (`cancelling` status, coordinator `_partialSyncRunPromise` join, guarded `finalizeCancel`,
`forceKill`, bounded-join `abandonCancel` floor), but `MithrilPartialSyncService.start()` (`:164-278`) is an
**uninterruptible stage machine** — it never checks `_isCancelled` between its sequential `await`s, and
`_downloadAndVerifyPartialSnapshot` re-emits `downloading` unconditionally (`:458-465`). So a cancel during `preparing`
(when `cancel()` has no live `_currentProcess` to kill, `:318-320`) only flips the flag; the machine re-emits
`downloading` (pop-back), re-spawns the download (background process), can proceed to `installing`/cutover (a
Boundary-A safety hole), and once a stage re-emits a non-`cancelling` status `finalizeCancel` no-ops on its `:329`
guard so the staging dir is never cleaned. The "download failed / logs+quit" prompt is the `abandonCancel` no-settle
floor (`:372-391`) firing because `start()` never unwinds within the ~16 s bounded join.

Locked the minimal root-cause fix: make the stage machine honor cancellation — add a `MithrilPartialSyncCancelledError`
sentinel + a `_throwIfCancelled()` guard, and insert `this._throwIfCancelled()` at four `await` seams (CP-A after the
disk-space check `:191`; CP-B at the top of `_downloadAndVerifyPartialSnapshot` before the `downloading` emit `:458`;
CP-C after `validateStagedDownloadOutput` `:229`; CP-D before `_activatePostVerificationStage('installing')` `:243`).
`start()`'s existing catch already returns on `_isCancelled` (no `failed` emit), so cancel now unwinds promptly while
status is `cancelling` → coordinator join settles → `finalizeCancel` cleans the staging dir + clears the marker →
terminal `cancelled` (`['retry','restart-normal']`). No new status/channel/message, no PRD-status-contract or
Boundary-model change, cancel stays Boundary-A-only (CP-D before the cutover marker; `installing`/`finalizing` still
throw in `cancel()`), no renderer/i18n/theme change. Diff: 1 source file + its spec. Process-tree termination and a
startup orphaned-staging sweep recorded as deferred residuals.

Outcome: plan written to `task-ux-702d.md` (CAT-D section, ordered small-model-implementable steps, tests, safety,
residuals). Dispatched a critique subagent.

## Critiquer

Timestamp: 2026-07-01T00:00:00Z
Speaker: Critiquer (subagent — one broad adversarial pass over the CAT-D plan vs. live code)

Traced the plan against the four live files (`MithrilPartialSyncService.ts`, `chainStorageCoordinator.ts`,
`mithrilCommandRunner.ts`, plus both spec files). Findings:

1. **Checkpoint completeness — SOUND.** Every `await` window where Cancel is offered is covered: pre-download
   metadata/staging awaits → CP-A (`:191`); the latest-drift re-resolve (`:209`, between CP-A and CP-B) → CP-B (the
   download `spawn` is at `:528`, well after CP-B, so CP-B definitively prevents the re-spawn + the `:458-465`
   re-emit); download tail → CP-C (`:229`, before the synchronous `verifying` emit `:231-234`); converting → CP-D
   (`:243`). Mid-download progress re-emits already suppressed by `:483`.
2. **Mid-download case — SOUND.** `cancel()` sets `_isCancelled=true` at `:305` synchronously before `.kill()` at
   `:319` (no `await` in the prologue), and `runCommand`'s `close` resolves only after the kill, so the flag is
   guaranteed true before `_downloadAndVerifyPartialSnapshot` throws `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` (`:540`);
   `start()`'s catch swallows it (`:262-264`). No false `failed` terminal.
3. **finalizeCancel guard race — SOUND.** The catch returns without emitting; `_clearRuntimeWorkState` sets no status
   and does not null `_stagingChainDir`, so `_status.status` stays `'cancelling'` (`:308`) and the `:329` guard passes;
   `_getStagingRootPath()` still resolves for removal.
4. **Coordinator join — SOUND.** The checkpoint throw is caught and `start()` returns (not rethrows), so the IIFE's
   `await handlers.start()` resolves → `runPromise` resolves → `_awaitSettledWithin` maps to `settled=true` →
   `finalizeCancel` (not `abandonCancel`).
5. **CP-D safety — SOUND.** CP-D (`:243`) is before the `cutover-in-progress` marker write (`:244`) and
   `_installValidatedStagedSnapshot` (`:248`); there is no `await` between CP-D and the synchronous `installing` flip,
   after which `cancel()` throws (`:298-302`). No cancel can slip into cutover.
6. **Invariants / regressions — SOUND.** With `_isCancelled=false` every checkpoint is a no-op; success path and all
   failure-path specs keep the existing behavior; the no-op cancel branch, Boundary throw, and the direct-`cancel()`
   sequence specs don't drive `start()` and are untouched; bootstrap is a separate class. No existing test breaks.
7. **Test adequacy — ADEQUATE.** Flipping `_isCancelled` inside an early stub matches the existing `jest.spyOn`/
   `mockImplementation` style; additions only, no rewrites. (Test #2 must reuse the success-path staged-db
   `readdirMock` setup to reach `_convertStagedSnapshot`.)
8. **Scope deferrals — DEFENSIBLE.** Startup orphaned-staging sweep is covered by the next attempt's staging pre-wipe
   + CAT-D's `finalizeCancel` cleanup; process-tree kill deferral rests on the single-process/in-process-download
   assumption, gated behind an operator-verify checkpoint.

Two **non-blocking** notes (folded into the plan): (a) a cancel during the metadata window can trigger one extra
short-lived **read-only** `snapshot list` subprocess inside `resolveLatestSnapshotMetadata` (no interior checkpoint) —
low severity, mutates nothing, settles fast, download spawn fully gated by CP-B; optional hardening only. (b) The
mid-download kill + join-settle depend on mithril-client being single-process — already the plan's operator-QA gate.

All plan line references verified accurate against the live tree.

Decision: approved

## Planner

Timestamp: 2026-07-01T20:14:14Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-D follow-up hardening)

Post-implementation manual retest showed the first CAT-D cancel flow succeeded, but restarting Mithril Partial Sync
and canceling again could still surface the same failure prompt. Live runtime inspection narrowed the residual
precisely: `mithril-partial-sync.log` kept advancing and `ps` still showed `mithril-client` running, so the backend had
missed a live child rather than merely misreporting status. That falsified the assumption that plain `_currentProcess`
tracking was sufficient once the stage-machine checkpoints and converter-child tracking were in place.

Grounded seam: `mithrilCommandRunner.runCommand()` awaits `buildMithrilEnv()` before `spawn()`, and only after spawn
does it fire `callbacks?.onProcess(child)`. So a cancel can land after the stage becomes cancelable but before the
child reaches `_currentProcess`; `cancel()`/`forceKill()` then signal nothing, and the coordinator can still fall to
`abandonCancel()` while the download keeps running in the background. Locked Part 3 of CAT-D: add
`_trackCurrentProcess(child)` in `MithrilPartialSyncService`, route both `_runCommand` and `_runBinary` through it,
and immediately `child.kill()` when `_isCancelled` is already true. This stays service-side, leaves
`mithrilCommandRunner.ts` untouched, and closes the same registration window for download and conversion.

Outcome: canonical plan updated (Scope/Rationale/Implementation/Tests/Risks/Build status) and the 702d companion docs
queued for synchronization.

## Critiquer

Timestamp: 2026-07-01T20:14:14Z
Speaker: Critiquer (follow-up adversarial pass vs live code/logs)

Verified the residual is real and local. `runCommand()` has an async pre-spawn window (`buildMithrilEnv()` before
`spawn()`, `onProcess(child)` only after spawn), so plain `_currentProcess` assignment is insufficient to guarantee a
cancel can reach every child. The proposed helper is minimal and safe: on the success path it is equivalent to the
previous assignment; on the cancel path it kills only a child that has *just* registered after `_isCancelled` is
already true. Routing `_runBinary` through the same helper is correct because it closes the same registration window
without reopening the previously-fixed converting-stage gap.

The added service-spec cases for late-spawn download/conversion children are the right focused coverage. No
`mithrilCommandRunner.ts` change is needed because the callbacks contract already exists; no status, IPC, or Boundary
model changes are introduced.

Decision: approved

## Planner

Timestamp: 2026-07-01T21:40:00Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-E)

Appended **CAT-E — cancel-teardown hardening**. Operator re-test of the CAT-D build reported that cleanup now fails on
the **first** cancel (previously it took a second partial-sync attempt), with a live `mithril-client` surviving the
failure prompt **and app exit**, and asked to also fix the poor logging on this flow. Grounded with four subagents
(main-process cancel-flow trace; plan-document conventions; logging + spec conventions; app-quit reaping map) against
the live post-CAT-D working tree.

Root cause: CAT-D did not introduce the surviving-process gap — its Part-3 late-kill closed the *direct-child
registration race* (the old second-attempt failure), which made the **first** cancel reliably signal the in-flight
download child, exposing two structural gaps CAT-D explicitly deferred: (1) no process-**group** termination — the
runner spawns without `detached` (`mithrilCommandRunner.ts:155,:231-235`) and every kill signals only the direct child
pid (`MithrilPartialSyncService.ts:346,:391,:648`), so a `mithril-client` descendant is never reaped; (2) the runner
resolves only on child `'close'` (stdio-EOF, `:190,:265`) with no `'exit'` listener, so a descendant holding the
inherited pipe wedges the ~16 s bounded join (`chainStorageCoordinator.ts:43-44,473-492`) into `abandonCancel`.
`safeExit()` (`index.ts:87-129`) reaps only `cardanoNode`, so the child survives quit; and the cancel path is
near-silent.

Locked the fix in three dependency-ordered parts: **Part 1** cancel-lifecycle logging, shipped FIRST as the diagnostic
gate; **Part 2** `detached: !environment.isWindows` + a `killProcessTree` helper (POSIX group-kill / Windows
`taskkill /t /f`) routed through the three kill sites + a `safeExit` reap; **Part 3** an evidence-gated child-`'exit'`
settle signal. `detached` POSIX-only (Windows keeps `taskkill`) to preserve the Windows-launch invariant #7. No new
status/Boundary change; cancel stays Boundary-A-only. Wrote the CAT-E section + all integration surgery to
`task-ux-702d.md`, the durable findings to `task-ux-702d-research.md`, and the mirror to `tasks.json` (v1.8.0).
Dispatched **two independent adversarial critique subagents** (technical/correctness + safety/scope/format).

Outcome: CAT-E plan authored (PLANNED, not implemented). Two critique passes returned below.

## Critiquer

Timestamp: 2026-07-01T21:55:00Z
Speaker: Critiquer (two independent adversarial subagents vs live code — technical + safety/scope/format, synthesized)

Both passes confirmed the **diagnosis is architecturally correct** (runner resolves on `'close'` with no `'exit'`
listener; the group-pgid lever is right; safety invariants hold — staged-only wipe, and the reap is a structural no-op
during `installing`/`finalizing` since those stages spawn no child, so it cannot interrupt a cutover) and that the
sub-structure/format, finding #5, and citations mostly match the CAT-D house style. Both returned
**`requires_changes`** with these material findings:

Technical (correctness/engineering):
- **[MAJOR] Windows shutdown reap raced `process.exit()`.** A fire-and-forget `exec('taskkill …')` may never launch
  before `process.exit()` fires inside the `file.stream.end(...)` callback (`safeExitWithCode.ts:10-14`), reintroducing
  the after-exit orphan on Windows. Precedent `CardanoNode._killProcessWithName` `exec`s but then *awaits*. →
  Fixed to **`execSync`** (terminal-safe; POSIX `process.kill(-pid)` is already synchronous).
- **[MAJOR] `detached`+group-kill was over-committed as proven root cause.** The bounded join already SIGKILLs the
  tracked pid, so survival-after-SIGKILL admits three hypotheses: in-group descendant (group-kill), re-grouped
  (`setsid`) descendant that **escapes** the group (needs a `find-process` tree walk), or single-process survival =
  stale/null `_currentProcess` (a different bug `detached` won't fix). → Part 2's *mechanism* is now **evidence-gated
  on Part 1's `pstree`** exactly like Part 3, with the three hypotheses documented in Risks.
- **[MAJOR] Part 3's exit-settle is semantically weaker than `'close'`** and could report `cancelled` while an escaped
  descendant keeps downloading. → Disclosed in Risks; group/tree reap kept authoritative for termination.
- **[MINOR] Reap placement** "before every `safeExitWithCode`" would miss the STOPPING branch and duplicate. → Fixed to
  a single insertion right after `pauseActiveDownloads()` (`index.ts:88`).
- **[MINOR] Windows always-force** (SIGTERM/SIGKILL collapse under `taskkill /f`), and **[MINOR] test scope**: the real
  defect is not unit-testable — jest proves plumbing, the Linux operator gate proves the fix; add a
  `reapPartialSyncOnShutdown` unit test + env-mock/`execSync`-mock mechanics. → All folded in.

Safety/scope/format:
- **[BLOCKER] Claimed a CAT-E plan-critique "recorded" in this file that did not yet exist** (3 sites). → Resolved by
  this transcript; wording now accurate.
- **[MAJOR] `detached` touches the runner shared with `MithrilBootstrapService`** (invariant #7), and the safety
  argument only defended the Windows gate. → Added a Why-safe bullet: POSIX-detached bootstrap spawns are inert
  (bootstrap issues no group-kill; reap is `isPartialSyncActive()`-guarded), so #7 holds on POSIX too.
- **[MAJOR] Mis-cited runCommand `'close'` handler `:259` (that is `'error'`; close is `:265`).** → Corrected
  everywhere (esp. the Part-1 implementation step a mechanical implementer follows literally).
- **[MINOR] Stale/loose citations & wording:** cancel guard `:298-302` → `:325-327`; CAT-E headers relabeled "post-CAT-D
  tree"; "promoted to an implemented fix" → "planned/scheduled fix"; the `CardanoSelfnodeLauncher.ts:112` "new console"
  paraphrase trimmed to the actual comment; the causal note clarified (Part 3 fixed the registration race; CAT-E fixes
  the process-tree + settle gaps); the tasks.json widening prose updated. → All applied.

All must-fix items (both BLOCKER/MAJOR sets and the recommended minors) have been applied to `task-ux-702d.md`,
`task-ux-702d-research.md`, and `tasks.json`. Residuals are now documented, not hidden: the escaped-descendant
(`setsid`) case, the exit-settle weakness, the Windows force-collapse, and the quit-time staging-dir residual.

Decision: requires_changes (all listed changes applied in the revision below)

## Planner

Timestamp: 2026-07-01T22:05:00Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-E revision)

Applied the critique. Substantive: (a) `killProcessTree` Windows branch → `execSync` so the shutdown reap cannot lose
the race with `process.exit()`; (b) Part 2's `detached`+group-kill mechanism is now **evidence-gated on Part 1's
`pstree`**, with three explicit hypotheses (in-group descendant → group-kill; re-grouped/`setsid` descendant → escapes
the group, needs a `find-process` tree walk; single-process survival → stale/null `_currentProcess`, a different bug);
(c) Part 3's exit-settle weakness disclosed, group/tree reap kept authoritative; (d) shared-runner/bootstrap invariant
#7 defended on POSIX; (e) reap placement corrected to a single `index.ts:88` insertion covering the STOPPING branch;
(f) Windows always-force collapse documented in the helper contract; (g) `reapPartialSyncOnShutdown` unit test +
mock-mechanics + "jest proves plumbing, operator proves the defect" added. Citation/wording: close handler `:259` →
`:265`; guard `:298-302` → `:325-327`; headers relabeled post-CAT-D; "implemented fix" → "planned"; new-console
paraphrase trimmed; causal narrative clarified. CAT-E remains **PLANNED, not implemented**; no code has landed. The
wave stays **open** (next ad-hoc CAT is CAT-F).

Decision: LOCKED (CAT-E — revised per plan-critique); implementation + operator proof pending; wave OPEN.

## Planner

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: before the
> CAT-D entries at the top of this log.)

Timestamp: 2026-07-01
Speaker: Planner (ad-hoc finalization cleanup wave)

Opened `task-ux-702d` as the ad-hoc finalization cleanup wave over the shipped Mithril Sync UX, mirroring the
`task-ux-702c` structure. **CAT-A** remediates a proactive-prompt readability defect surfaced on the post-702c build:
the `SyncingConnectingMithrilPrompt` card is a `position: fixed` float with no dimming backdrop, and its fill token
`--theme-mithril-card-background` was defined at alpha `0.96` in all nine static themes and the dynamic generator, so
the live wallet summary bled through and the copy was hard to read. This is distinct from 702c CAT-B (overlay
backdrop) and 702c CAT-D (chrome; the "blur" report there was a confirmed misdiagnosis — no `backdrop-filter` on this
surface). Locked the fix: flip the token alpha to `1` at every definition site (10 one-line edits — nine
`themes/daedalus/*.ts` + `createTheme.ts:599`), preserving each theme's hue; no SCSS/TSX/i18n/store/IPC/backend
change. The token is shared with the bootstrap/working overlay card (`MithrilBootstrap.scss:32`) — making it solid
there is accepted as harmless/consistent (that card sits on a dimmed backdrop). No spec/snapshot references the token,
so no test change was required. The wave is intentionally **open**: further ad-hoc cleanups append as CAT-B, CAT-C, …
Held the deployment gate open — `task-ux-702` gains a dependency on `task-ux-702d`; no `completedAt`.

Decision: LOCKED (CAT-A); wave OPEN.

## Planner

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: before the
> CAT-D entries at the top of this log.)

Timestamp: 2026-07-01
Speaker: Planner (ad-hoc finalization cleanup wave)

Appended **CAT-B** to the open wave: a copy/i18n fix on the proactive-prompt handoff note. The choice-view note body
(`SyncingConnectingMithrilPrompt.tsx:41-42` `promptHandoffNote`, rendered `:185`) named only "the Diagnostics screen"
— under-specified: no app qualifier and no way to reach it, leaving a user who picks Standard Sync without a concrete
route back to the restart entry point. Retargeted it to "the Daedalus Diagnostics screen under the Help menu (Ctrl + D)" (proper noun +
keyboard shortcut). Copy/i18n-only: tsx `defaultMessage`, `en-US.json:171`, `ja-JP.json:171`
(`スキップした場合でも、ヘルプメニューにあるDaedalus Diagnostics画面からMithril同期を開始できます。(Ctrl + D)` — not whitelisted), and the
generated `defaultMessages.json:3131` via `yarn i18n:manage` (exit 0; one-entry diff, file otherwise byte-identical).
The separate bold "Note:" label span (`promptHandoffNoteLabel`) is untouched, as is all gating/layout/SCSS. Updated
the single spec assertion (`SyncingConnectingMithrilPrompt.spec.tsx:76`) to the new body verbatim; per the Node v24
renderer verify-env caveat the full jest/tsc suite was not run. `grep` for the old fragment `from the Diagnostics
screen` across `source/` is clean. Kept the Mithril-Sync vocabulary (no "partial sync"/%/immutable). The wave stays
**open** — the next ad-hoc cleanup appends as CAT-C.

Decision: LOCKED (CAT-B); wave OPEN.

## Planner

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: before the
> CAT-D entries at the top of this log.)

Timestamp: 2026-07-01
Speaker: Planner (ad-hoc finalization cleanup wave)

Appended **CAT-C** to the open wave: a presentation/theme-token fix on the **Mithril Partial Sync overlay** backdrop.
Operator manual testing (post-702c build) reported that the overlay's **download-status** and **failure prompts** were
"slightly opaque and show the Cardano Node status page", not the solid ("light blue") backdrop the **fast-sync with
Mithril** flow shows. Grounded the root cause with an Explore subagent: both flows render the same shell and share
`MithrilBootstrap.scss` `.backdrop` (`:14` → `background: var(--theme-mithril-overlay-backdrop-start)` + `blur(5px)`),
and that token is alpha **`0.92`** in every theme. The only difference is what sits behind the scrim — fast-sync is
rendered by `LoadingPage` *instead of* `SyncingConnectingPage` over a solid `CenteredLayout` background (nothing shows
through), while the partial-sync overlay is mounted at `App.tsx:99` *over* the live `SyncingConnectingPage`, so the 8%
transparency bleeds the node status page through. The "light blue" is the light-blue theme's own backdrop hue; each
theme defines its own — confirming the per-theme token approach. Locked the fix: flip
`--theme-mithril-overlay-backdrop-start` **and** its gradient-partner `--theme-mithril-overlay-backdrop-end` alpha
`0.92 → 1` at every definition site (nine `themes/daedalus/*.ts`, two lines each, + `createTheme.ts:592/595`),
preserving each theme's hue; `-end` is currently unused by SCSS but flipped to keep the backdrop gradient pair
consistent. Styling/token-only — the shared `.backdrop` SCSS and its now-moot `blur(5px)` are untouched; no
TSX/i18n/store/IPC/backend change. The token is shared with the fast-sync bootstrap backdrop — making it solid there is
harmless (nothing meaningful behind it) and is the goal (the two Mithril backdrops become identical). `grep` confirms
no `--theme-mithril-overlay-backdrop-*` definition retains `0.92`; no spec/snapshot references the token, so no test
change was required. Mirrors CAT-A's token-alpha fix exactly. The wave stays **open** — the next ad-hoc cleanup appends
as CAT-D.

Decision: LOCKED (CAT-C); wave OPEN.

## Planner

> (Relocated 2026-07-01 from the former task-ux-702d.md journal tail; original chronological position: between this
> log's first CAT-D Critiquer pass and the late-spawn follow-up entry.)

Timestamp: 2026-07-01
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-D revision)

**Incorporated a validated code-review finding on CAT-D (converting-stage cancel).** A reviewer flagged (MAJOR) that
CAT-D over-promised converting-stage cancel: the plan guaranteed a terminal `cancelled` outcome for a cancel issued at
**any** stage incl. `converting` (scope, CP-D, operator verification), but the live `converting` path is an **awaited**
`snapshot-converter` subprocess (`_convertStagedSnapshot` → `_runBinary`, `:551-595`) whose child is **not** tracked in
the cancelable `_currentProcess` slot — only `_runCommand` wires `onProcess` (`:1113-1114`); `_runBinary` passes no
callbacks (`:587-594`). So `cancel()`/`forceKill()` (`:318-320`,`:364`) can't interrupt an in-flight conversion, and
CP-D — sitting *after* the conversion await — only fires once conversion returns. A conversion outrunning the ~16 s
bounded run-join (`chainStorageCoordinator.ts:43-44,473-492`) would route to `abandonCancel` (the "download failed /
logs+quit" floor CAT-D promises to eliminate). A validation subagent confirmed **every** mechanical claim against the
live post-702c tree and confirmed the gap is genuinely specific to `converting` (`preparing`/`downloading`/`verifying`
are unaffected — no subprocess yet, or the tracked `_runCommand` child). It also confirmed no converting-stage cancel
test exists and CAT-D is not yet implemented (`_throwIfCancelled`/`MithrilPartialSyncCancelledError` absent from
`source/main/`). **Note the safety invariant was never at risk:** CP-D sits before the install/cutover marker, so a
cancel during `converting` never reaches cutover even without process tracking — the over-promise was the *prompt
terminal-`cancelled` + cleanup* UX, not a Boundary-A safety hole.

**Decision — widen the implementation (keep the any-stage promise).** Rather than narrow CAT-D's wording, added **Part 2**
to the fix: wire `_runBinary` to pass the runner's *already-supported* `onProcess` callback (mirroring `_runCommand`),
so the conversion child lands in `_currentProcess` and the existing `cancel()`/`forceKill()` kill path terminates it
in-flight → the run settles within the bounded join → `finalizeCancel`, not `abandonCancel`. Verified this needs **no
`mithrilCommandRunner.ts` change** (`runBinary` already accepts `callbacks` and fires `onProcess` on spawn/close,
`:157,:185,:191`) and **no `cancel()`/`forceKill()` change** — it is a call-site-only edit, so CAT-D stays a 1-source-file
change (+ spec). Updated: Scope (two-part fix), CAT-D non-goals (runner untouched; `_runBinary` uses the existing
callback), Rationale (added the converting-stage analysis + the two-part framing), Reference details (converter-tracking
sites), Implementation (new wiring step), Why-safe (killing the converter is staged-only-safe; Part 2 inert on success),
Tests (converter-tracked unit + **required** converting-cancel → `finalizeCancel` service + coordinator assertions),
the process-tree residual (now covers both binaries' direct children; process-**group** kill stays the residual), the
ordered checklist, and Build status. No new status/channel/message, no PRD-status-contract or Boundary-model change;
cancel stays Boundary-A-only.

Decision: LOCKED (CAT-D revised — Part 2 added); wave OPEN.

## Planner

Timestamp: 2026-07-02T05:35:51Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-E plan remediation + master-doc decomposition)

Remediated 14 confirmed consistency/effectiveness findings against the CAT-E plan corpus (task-ux-702d.md,
task-ux-702d-research.md, tasks JSON) — M-01..M-14: hypothesis-(ii) tree-walk operating constraints made
explicit (enumerate-before-signal while the leader is alive, retained pid snapshot for the delayed SIGKILL
escalation/reap, identity-based find-process fallback scoped to the staging workDir, sync-only shutdown-reap
enumeration) [this supersedes the bare "find-process child-tree walk" phrasing in the earlier CAT-E entries of
this log — recorded here rather than rewriting those entries, per append-only]; Part-1 gating evidence made
operational (exact ps -eo pid,ppid,pgid,sess,args / pstree -gp commands, Daedalus pid+pgid recorded first,
pre-fix three-hypotheses interpretation table, pid added to runner spawn/exit/close logs); the ':190,:259'
close-handler miscite fixed at its three surviving sites (tasks JSON ×2, research.md — the master was already
correct, making this log's earlier "Corrected everywhere" claim true); the tasks-JSON reap placement re-synced
to the single post-pauseActiveDownloads() insertion; the last stale "promoted to implemented" (Scope) fixed —
this log's "All applied" record now holds; execSync taskkill given a 5 s timeout + windowsHide and the
CardanoNode "exact precedent" claim corrected to bounded-async; the Part-2 mechanism gate propagated to every
operational surface (Scope/impl/checklist/verification/tasks JSON); Windows on-platform verification explicitly
deferred to the task-ux-702 packaged-build QA matrix (new Windows cancel + quit-mid-download QA item); the
reap-on-quit claim made hypothesis-conditional with an optional scoped find-process sweep hardening; the
killProcessTree catch comment corrected (kill(-pid) does not throw for an escaped descendant); the detached
dev-terminal SIGINT/SIGHUP regression acknowledged as accepted (bootstrap orphan risk, no un-detach);
MithrilController.spec.ts added to the diff shape / spec lists / targetPaths ("eight CAT-E paths") with mock
mechanics; the Tests #10 reap unit mirrored into the tasks-JSON testCases + acceptance.

Decomposed the 1559-line master 702b-style into five self-contained per-category docs
(task-ux-702d-cat-a..e.md; move-don't-copy), the master retained as single source of truth + index (status
table, decomposition index, compressed scope/traceability/boundaries, wave-level gates, status register).
Journal relocated per the sprint rule: CAT-A/B/C + CAT-D-revision Planner entries appended above with
provenance; both Implementer entries appended to task-ux-702d-impl-review.md; three Planner entries already
near-1:1 in this log were dropped from the master (diff-checked; unique fragments, if any, noted here: the
CAT-E entry's enumerated list of master sections it updated — Scope, traceability finding #5, Locked safety
boundary #4, ordered checklist, Verification plan, Risks, Review-log paths, Planning/Build status — all of
which now live in the cat docs/master index; the CAT-D entry's "Reframed Locked safety boundary #3" sentence,
whose substance now lives in task-ux-702d-cat-d.md Locked invariants; the CAT-D follow-up entry's fuller
updated-surfaces list, which additionally named Reference details + Review-log paths). CAT-F.. append
convention recorded in the master + tasks JSON (v1.9.0).

Decision: LOCKED (CAT-E plan revised per the confirmed-findings remediation; decomposition landed); CAT-E
implementation + staged operator proof still pending; wave OPEN.

## Critiquer

Timestamp: 2026-07-02T09:42:45Z
Speaker: Critiquer (grounded review — four Opus exploration subagents vs live post-Part-1 tree + captured logs)

Re-grounded the CAT-E Part-2 plan (2A slot fix / 2B probe gating / 2C detached+killProcessTree+reap) against the live
post-Part-1 tree and an independent evidence rebuild from `logs/ps.log`, `logs/pub/Daedalus.json`, and
`logs/mithril-partial-sync.log`. **All load-bearing 2A/2B/2C mechanical claims verified:** the `_currentProcess`
threading list is exhaustive (only two writers — service `:662`/`:1221`); the 2B insertion point is exact (store
`:250`/`:254`); the three kill sites (`:354`/`:414`/`:684`) are mechanical; no mithril spawn exists outside the shared
runner; and the dependency facts hold (`find-process` is a prod dep, `tree-kill` is undeclared/dev-transitive only).

**Evidence rebuild — SUPPORTS the (iii) slot-clobber verdict, but as code-plus-log inference, not direct observation.**
Pid timeline: download **pid 229730** is spawn-only (appears exactly once in `Daedalus.json`, no exit/close/kill line);
probe **pid 230124** (`snapshot show latest`) overwrote then nulled the slot ~5 s *before* the inferred cancel (the
first cancel-related log is the coordinator join-timeout `07:58:47.314Z`; cancel ≈ timeout − 15 s ≈ `07:58:32`); there
is **no kill line at cancel** against any pid; the join-timeout falls to `abandonCancel` (`settled:false`); and the
download was still advancing at `08:00:43`. The landed Part-1 cancel-entry log sits inside the real-kill branch
(`if (this._currentProcess)`), so it **cannot fire on an empty slot** — slot-emptiness was therefore never *directly*
observed (no cancel-entry line, no `hadChild` field in the repro). The `ps.log` snapshots were captured **while Daedalus
was still running**, so they prove the survivor topology (finding #8 match: pid 229730 / ppid 228282 / pgid 227499 /
sess 218848) but **not** the "survives app exit" leg (that remains on operator re-repro #2).

**Material findings requiring changes:** the step-12 reap-synergy premise is wrong (`start()`'s catch returns *through*
the `finally`, which nulls the slot — the reap reads the slot and so DEPENDS on 2A; the reap alone would not have caught
the untracked pid-229730 survivor); `MithrilPartialSyncStore.spec.ts` is **not** new (exists, ~34.7 KB — 2B extends it;
only `killProcessTree.spec.ts` and `MithrilController.spec.ts` are new); the Daedalus Diagnostics dialog consumes
`isSignificantlyBehind`/`certifiedEpoch` mid-run (2B leaves it showing frozen pre-run values); `isPartialSyncActive()`
is `status !== 'idle'` (broader than "true for failed"); `isMithrilPartialSyncWorkingStatus` needs importing in
`MithrilController.ts`; the 2A flag should ride a service-local options field over a 4th positional param; the behind-ness
cache 5-min TTL admits repeat clobber windows on >5-min downloads; the "single shutdown funnel" claim is qualified
(fatal/startup paths bypass `safeExit`); and several Reference-details live-line shifts were recorded.

Decision: requires_changes (all listed changes applied in the revision below)

## Planner

Timestamp: 2026-07-02T09:42:45Z
Speaker: Planner (ad-hoc finalization cleanup wave — CAT-E Part-2 grounding remediation)

Applied the grounded review. Corrections **C1–C8** folded into `task-ux-702d-cat-e.md`: (C1) the evidence-quality
reframe — the (iii) verdict is now stated as code-plus-log inference (incl. the `ps`-while-alive caveat), not direct
observation; (C2) an **unconditional** `{ status, pid, hadChild }` cancel-entry log added to the 2A diff + its test +
the re-repro #1 wording, so re-repro #1 *directly* observes the empty slot; (C3) the reap-synergy premise fixed
(`start()`'s catch returns through the `finally` that nulls the slot) and the 2C reap re-scoped as **dependent on 2A**;
(C4) `MithrilPartialSyncStore.spec.ts` relabeled not-new (2B extends it); (C5) the Diagnostics mid-run staleness
documented as accepted; (C6) the `MithrilController.ts` import note; (C7) the 2A options-field made primary over the 4th
positional; (C8) additive notes (`isPartialSyncActive` breadth, TTL repeat-miss, benign post-2A post-cancel probe
completion, `_logStream` write-only coupling, shutdown-funnel bypasses, live line shifts). Mirrored to
`task-ux-702d-research.md` findings **#8/#10** (dated addenda) and to the tasks JSON (relabel note + targetPaths confirmed
— store spec already present, `killProcessTree.spec.ts`/`MithrilController.spec.ts` stay new — plus a patch version bump
to **v1.9.1**). CAT-E Parts 2/3 remain **PLANNED, not implemented**; operator re-repros #1/#2 stay the proof gates, with
re-repro #1 now a direct slot observation.

Decision: LOCKED (CAT-E Part-2 plan re-grounded; implementation + staged operator proof pending; wave OPEN).

