# task-ux-703 — PR #3337 review-comment remediation wave (CAT-A–G)

> Ad-hoc remediation wave addressing all 32 unresolved review threads on PR #3337
> (<https://github.com/input-output-hk/daedalus/pull/3337>) plus the 9 nit-level cleanups listed only in
> the multi-agent review body. Design decisions were resolved in an interactive grilling session on
> 2026-07-03 (see `task-ux-703-plan-review.md` and DD-703-1…14 below); the implementer makes no decisions.

- Sprint: Mithril Partial Sync UX Refinement — phase-7 (Backend-Correctness Re-validation And Manual QA / Rollout Gate)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved` (grill 2026-07-03 + critique pass with one Planner fix pass + verification
  grill 2026-07-03 with 4-agent code verification of all 41 items — see plan-review log)
- Build status: `pending` (pre-build; precedes the `in_progress|in_review|completed` build lifecycle)
- Interaction mode: `autonomous`
- Priority: critical · Estimated: 16h · Dependencies: task-ux-702 (completed)

## Task id + title

- **Id:** `task-ux-703`
- **Title:** PR #3337 review-comment remediation wave (CAT-A–G)

## Interaction mode (AUTHORITATIVE)

`autonomous`. Every judgment call raised by the review threads was resolved with the user during the
2026-07-03 grilling sessions and is recorded as a design decision (DD-703-1…14). Implementation is
mechanical from this doc. Do not re-open the DDs; if a step turns out to be impossible as written,
stop and escalate rather than improvising.

## Why now

PR #3337 carries the whole UX-refinement sprint. Review surfaced 1 critical + 8 major + 17 minor
verified findings across 32 unresolved threads (self multi-agent review, Copilot, and external
reviewer AndrewWestberg), plus 9 nit cleanups in the review body. The critical finding (T5) leaves a
user permanently stuck with a recovery button that always rejects; several majors dead-end real
recovery flows. The PR cannot merge with these open.

Thread ids T1–T32 used throughout refer to the digest in
`task-ux-703-pr-comment-checklist.md` (comment id, path:line, and response text per thread).

## Scope

- Fix all correctness findings (CAT-A–D), i18n/copy findings (CAT-E), and structure/perf findings
  (CAT-F) from the 32 threads.
- Apply the 9 body-only nit cleanups (CAT-G).
- Update the smoke-test cheat sheet wherever behavior changes (DD-703-7).
- Produce/maintain the PR-comment checklist doc so the author can reply to and resolve threads
  manually (DD-703-8).

## Non-goals (locked)

- **No PR writes**: the task does not post replies, resolve threads, or comment on GitHub (DD-703-8).
- **No behind-ness criterion rewrite**: `mithrilPartialSyncThresholdImmutables = 20` stays as the
  proactive-prompt gate (DD-703-3); T2 is answered in the checklist, not in code.
- **No revert of ADR D-702a-1**: the completed overlay keeps auto-finalize; only its failure path
  changes (DD-703-4).
- **No revert of ADR D-702a-2**: `wipe-and-full-sync` stays off the cancelled/failed-cancel
  dialogs; T9's trap is fixed by best-effort cleanup instead (DD-703-9).
- **No spawn-detachment change**: `detached: !environment.isWindows` stays at both spawn sites
  (DD-703-6 as revised); T14 is answered in the checklist — `killProcessTree`'s POSIX group kill
  depends on the child being a process-group leader.
- **No kill-implementation unification (N1)**: declined (DD-703-13); `killProcessTree` (tree kill of
  a live child) and `CardanoNode._killProcessWithName` (polled single-pid kill, possibly of a
  prior-session PID) are semantically different by design.
- No new manual-QA matrix run; the task-ux-702 gate stays closed (DD-703-7).
- No snapshot-selection UI, storage picker, or auto-trigger (locked boundaries below).

## Dependencies

- `task-ux-702` (completed) — this wave remediates review findings on the state the gate approved.

## Research / docs / workflows / skills consulted

- PR #3337 review threads (all 33 comments + 6 review bodies, fetched 2026-07-03).
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-prd.md` (D7, D9, D12).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-702a-decisions.md`
  (ADR D-702a-1 — completed-overlay auto-finalize).
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md`.
- `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`.
- Grilling session 2026-07-03 (`task-ux-703-plan-review.md` Planner entry).

## Finding → Category traceability

| Thread | Finding (short) | CAT | Severity |
| --- | --- | --- | --- |
| T5 | Cross-session recovery: sole wipe button always rejects (`_assertRecoveryActionAllowed` vs broadcast-only snapshot) | A | critical |
| T6 | Unguarded `fs.remove(stagingRootPath)` in node-start-verified reclaim blocks node start | A | major |
| T8 | Interrupted-cutover Quit branch neither quits nor emits status — renderer dead-ends | A | major |
| T9 | Locked staging dir traps user: retry/restart-normal loop on the same `fs.remove` failure (wipe-restore declined per DD-703-9; fix is best-effort cleanup) | A | major |
| T10 | Cleanup/recovery paths resolve staging root session-locally; marker path orphaned on custom volumes | A | major |
| T14 | Detached POSIX spawn orphans mithril-client on crash (premise refuted — detached is load-bearing for the group kill; answered in checklist, DD-703-6 rev.) | A | minor |
| T11 | Diagnostics section hidden unless significantly behind; probe failure hides manual trigger | B | major |
| T12/T31 | Start rejections swallowed after resync; `mithrilAttemptStartedThisSession` never reset | B | major |
| T15 | Recovery-action store methods rethrow/bare-await into `onClick` — unhandled rejections | B | minor |
| T21 | Behind-ness caches survive chain-directory change | B | minor |
| T28 | Sequential awaits for independent aggregator fetch + readdir | B | minor |
| T2 | Threshold constant unit/necessity question (AndrewWestberg) | B | question |
| T13 | Completed overlay: one-shot auto-finalize, swallowed rejection, no interactive fallback | C | major |
| T7 | Disk preflight uses whole-chain snapshot size for a partial range | D | major |
| T18 | `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` missing from error-code union / copy map | D | minor |
| T1/T17 | Raw stage ids `verifying`/`converting`/`installing` rendered untranslated | E | minor |
| T4 | "shutdown" → "shut down" in shared status message | E | minor |
| T16 | Service throws hardcoded English + banned "partial sync" vocabulary | E | minor |
| T19 | Overlay body renders raw `error?.message` | E | minor |
| T20/T32 | "about 1 epochs behind" — missing ICU plural (prompt + confirmation) | E | minor |
| T22 | Hardcoded English fallback in `MithrilPartialSyncSection` | E | minor |
| T3 | Opaque backdrop still pays `backdrop-filter` blur cost | F | minor |
| T23 | Error-message extraction triplicated with divergent fallbacks | F | minor |
| T24 | `isMithrilPartialSyncBlockingNodeStart` inlines the working-statuses array | F | minor |
| T25 | `PROGRESS_STATUSES` hand-copies working statuses + `completed` | F | minor |
| T26 | ~15-line spawn block duplicated in `runBinary`/`runCommand` | F | minor |
| T27 | `listSnapshots`/`showSnapshot` duplicate CLI spawn + parse pipelines | F | minor |
| T29 | Dead `behindByImmutables` observable | F | minor |
| T30 | Dead `elapsedSeconds` observable | F | minor |
| N1–N9 | Review-body nit cleanups (see CAT-G; N1 declined per DD-703-13) | G | nit |

## DESIGN DECISIONS (resolved 2026-07-03 — implementer makes none)

### DD-703-1 — Packaging
One wave task covering all 32 threads and all 9 nits; single commit
`fix(mithril): task-ux-703 remediate PR #3337 review findings CAT-A-G`.

### DD-703-2 — Diagnostics visibility (T11)
The Diagnostics "Mithril Sync" section renders **whenever `mithrilPartialSyncEnabled` is on**. Copy
adapts to probe state: significantly behind → existing recommendation + CTA; measured near tip →
informational copy + CTA still enabled; probe failed → "availability unknown" note + CTA still
enabled. The manual trigger is never hidden by behind-ness or probe failure.

### DD-703-3 — Behind-ness threshold (T2)
Keep `mithrilPartialSyncThresholdImmutables = 20` (absolute unit: immutable chunks ≈ 6 h each,
≈ 5 days) as the **proactive-prompt gate only**. With DD-703-2 the manual offer is unconditional
when enabled, which answers the reviewer's "allow when mildly behind" concern without lowering the
prompt threshold. No code change beyond DD-703-2; the offer-vs-prompt rationale ships as the
checklist reply to T2. Calibration stays a QA follow-up.

### DD-703-4 — Completed-overlay finalize (T13)
ADR D-702a-1 (auto-finalize on 4 s timeout, no "Continue to Daedalus" button) **stands**. Fix only
the failure path: on finalize rejection retry once; if the retry also rejects, surface the existing
`MithrilErrorView` with a `retry` recovery action that re-invokes finalize. No new UI surface; no
silent `.catch(() => {})`.

### DD-703-5 — Disk preflight estimate (T7)
`requiredBytes = max(snapshotTotalUncompressedBytes − chainDirBytes, 0) + 0.2 × snapshotTotalUncompressedBytes`,
floored at the existing `DISK_SPACE_REQUIRED` (4 GB) constant. `chainDirBytes` is the recursive size
of the resolved chain directory. If measuring `chainDirBytes` fails, fall back to the current
whole-snapshot bound (`snapshotSize × 1.2`). The margin is deliberately proportional to the **full**
snapshot size (headroom for chain growth + conversion), not to the delta.

### DD-703-6 — POSIX spawn (T14) — REVISED 2026-07-03 (verification grill)
The original decision (remove `detached: !environment.isWindows`) is **withdrawn**. Code
verification showed `killProcessTree` kills via `process.kill(-pid, signal)`
(`killProcessTree.ts:72`) and its docstring documents the dependency on the runner spawning
detached so the child is a process-group leader; removing detachment would push every POSIX
cancel/force-kill/shutdown-reap onto the ESRCH fallback single-kill and lose descendant reaping
(the conversion path spawns helper binaries). The finding's premise is also misattributed: POSIX
children survive a parent crash regardless of `detached` (they reparent to init), so the change
would not have fixed the stated orphaning either. Resolution: **no code change** — keep `detached`;
answer T14 in the checklist (like T2); optionally add a concise rationale comment at the spawn
options (no task ids in comments). The PID-persist + startup-reaper idea stays recorded as a future
option if crash-window orphaning ever matters.

### DD-703-7 — Verification level
Automated coverage only (jest/cucumber/storybook per fix). No manual-QA matrix re-run; the
task-ux-702 gate stays closed. The smoke-test cheat sheet **must be updated** wherever this wave
changes observable behavior (Diagnostics visibility, preflight numbers, completed-overlay failure
path, spawn/cancel semantics).

### DD-703-8 — PR follow-through
No GitHub writes from this task. Deliver
`task-ux-703-pr-comment-checklist.md`: for each thread — the original comment (id, path:line,
author), a detailed response, and a brief response — so the author can post and resolve manually.

### DD-703-9 — T9: decline wipe restore; best-effort cancel cleanup (preserves ADR D-702a-2)
Resolved with the user 2026-07-03 after the critique pass surfaced the conflict. ADR D-702a-2
deliberately removed `wipe-and-full-sync` from the cancelled/failed-cancel dialogs (cancel is
pre-cutover only; the chain DB is intact, so wipe is destructive and unnecessary) — it **stands**.
T9's genuine defect is the trap: when the staging dir is file-locked (Windows), `retry` and
`restart-normal` both loop on the same `fs.remove` failure. Fix: cancel-path staging removal
becomes **best-effort** (log + continue, mirroring T6): `restart-normal` proceeds on the intact DB
and the orphaned staging dir is reclaimed at the next partial-sync start
(`preparePartialSyncStagingDirectory` removes the staging root; cancel is pre-cutover, so no marker
exists for startup reclaim to see — corrected in the 2026-07-03 verification grill). Restoring wipe
was declined: it would attempt the same locked removal and, if it worked, would destroy a valid DB
to escape a transient lock.

### DD-703-10 — T11 probe-failed plumbing (verification grill 2026-07-03)
DD-703-2's third copy state is not derivable in the renderer today: the service collapses probe
failure into `{ isSignificantlyBehind: false }` (`MithrilPartialSyncService.ts:1000-1007`) and
`MithrilPartialSyncAvailability` carries no error signal. Add an optional `isProbeFailed?: boolean`
to `MithrilPartialSyncAvailability` (`mithril-partial-sync.types.ts:64-73`), set it in the probe
catch, pass it through `MithrilController.getPartialSyncAvailability` (`MithrilController.ts:191-193`)
and `MithrilPartialSyncStore._applyAvailability` (`MithrilPartialSyncStore.ts:322-334`); Diagnostics
branches its copy on it. The backend stays the sole owner of behind-ness (locked boundary 4).

### DD-703-11 — T15 rejection handling: catch + resync + log; no new UI surface
The planned "store the error into the overlay-visible error state" is not implementable as written:
the overlay selects `MithrilErrorView` from the backend-published status, not from a renderer-set
error, and `toStartError` yields a plain `Error` without `code`/`stage`. Resolution: each recovery
action (`cancelPartialSync`, `restartNormally`, `wipeAndFullSync`) catches, re-syncs status from
the backend, and logs. A genuine backend failure surfaces through the resynced `failed` status
(which already drives `MithrilErrorView` copy); stale-button races self-correct on resync. Also
wrap the overlay retry wiring (`App.tsx:116`, `onRetry={mithrilPartialSync.startPartialSync}`) — a
call site the original T15 list missed, which T12's broader rethrow would otherwise turn into an
unhandled rejection.

### DD-703-12 — T16 covers every user-reachable throw site
The plan's original anchor list was incomplete. `_buildError`
(`MithrilPartialSyncService.ts:1272-1290`) copies `error.message` verbatim into `status.error`,
which `MithrilErrorView` renders — so all throw prose reachable from `start()`/recovery catches is
user-visible. Convert all of: `:180` (already running), `:255-260` (disabled / latest drift),
`:355-356` (cancel after cutover), `:556-557`/`:567-568` (start/retry boundary), `:669-673`
(exit-code failure), `:805-813` (recovery boundary), `:901-903` (metadata resolve), `:1080-1082`
(certified immutable — contains banned "immutable"), `:1121-1124` (disk space, T18), and the
`_buildError` fallback `'Mithril partial sync failed'` (`:1290`) to stable error codes with intl
copy; prose moves to log lines.

### DD-703-13 — N1 declined (no kill unification)
Verification refuted the premise: `killProcessTree` (takes a live `ChildProcess`, POSIX group kill,
fire-and-forget, direct-kill fallback) and `CardanoNode._killProcessWithName` (takes a raw pid —
possibly a stored previous-session PID with no `ChildProcess` — single-pid kill plus
`promisedCondition` death-poll) are not equivalent, and `CardanoNode` is critical legacy shutdown
code. No code change; answer in the checklist nit summary.

### DD-703-14 — N4 kept, with bootstrap guards
The variant-flag refactor stands (the view already holds intl fallbacks for every overridden prop;
the actual prop count is 11, not ~13), under explicit constraints: `variant` defaults to
`'bootstrap'`; bootstrap's message defaults and its `completed` frame stay byte-identical
(`MithrilProgressView.tsx:146-151` documents that contract); `completedTransitionLabel` behavior
remains partial-sync-only; bootstrap stories/specs re-run green (locked boundary 11).

## Locked safety boundaries (honor while implementing — inline)

1. Staged-only restore; no in-place mutation of the live chain.
2. Recovery actions render **strictly from `allowedRecoveryActions`** (Boundary A vs B/C1/C2 is
   backend-authoritative); never infer from status names. CAT-A widens what the backend *emits*,
   never what the renderer infers.
3. Confirmation precedes start; the proactive prompt's deep-link lands on the confirmation modal.
4. No auto-trigger; no renderer-computed threshold — the backend owns behind-ness (DD-703-3 keeps
   the threshold in launcher config, consumed by main).
5. Latest snapshot only; no snapshot-selection UI; no user-facing storage picker.
6. Cancellation forbidden after cutover.
7. Networks: mainnet, preprod, preview.
8. No synthetic throughput / remaining-time / overall-% over IPC; never route raw mithril-client
   JSON into UI copy. User-facing vocabulary is **"Mithril Sync"** — never "partial sync",
   percentages, or "immutable" in copy (CAT-E enforces this).
9. Locked decision #16 as amended by PRD D9 **and ADR D-702a-1**: success overlay auto-finalizes;
   finalize resets backend to idle, removes staging, clears marker (DD-703-4 touches only the
   failure path).
10. Kill switch `mithrilPartialSyncEnabled` hides all partial-sync UI when off (DD-703-2's
    "always render" is still inside this gate).
11. Do not regress the empty-chain Mithril bootstrap flow (CAT-E/F touch shared progress
    components — re-run bootstrap specs).

## Files expected to change (verified against PR head `7723298239`)

- `source/main/mithril/MithrilPartialSyncService.ts` (T5, T9, T10, T11 probe flag, T16, T21, T27, T28, T7, N6)
- `source/main/mithril/mithrilPartialSyncNodeStartup.ts` (T6, T8)
- `source/main/mithril/mithrilCommandRunner.ts` (T26; optional T14 rationale comment only)
- `source/main/mithril/mithrilSnapshotMetadata.ts` (N2)
- `source/main/utils/chainStorageCoordinator.ts` (N3, T21)
- `source/main/ipc/mithrilPartialSyncChannel.ts` (T5 — `getMithrilPartialSyncStatus` read)
- `source/main/mithril/MithrilController.ts` (T5, T11 probe passthrough, T21 wiring, N5)
- `source/common/types/mithril-partial-sync.types.ts` (T18, T24, N8, error-code additions from T16, T11 `isProbeFailed` flag)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (T11 availability passthrough, T12/T31, T15, T23, T29, T30)
- `source/renderer/app/App.tsx` (T15 — catch on the overlay `onRetry` wiring at `:116`)
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (T11, T22, T23)
- `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts` (T4)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (T13, T25, N4)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx` (T19)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` (T1/T17)
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (T16, T18, T19)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss` (T3)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` (T20/T32, T23)
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` (T20/T32)
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` (N7 — redundant enabled re-check at `:718-723`)
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json` (CAT-E keys)
- `storybook/stories/**/Mithril*` + `_support/mithrilFixtures` (N9, spec fixtures from T17)
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md` (DD-703-7)
- Jest/cucumber specs adjacent to each file above.

Anchor drift: line anchors below come from the review threads against PR head `7723298239` (equal to
the local branch tip at planning time). Two threads (T1, T2) are marked *outdated* by GitHub — their
code moved but the findings persist at the anchors given here; re-verify anchors with `grep` before
editing.

## Implementation approach — ordered checklist (small-model-implementable)

Work CAT-A → CAT-G. Within a CAT, steps are ordered; each step names its thread and exact seam.

### CAT-A — Cross-session recovery & startup correctness

1. **T5 (cross-session recovery seeding).** The cross-session `starting-node`/`failed` snapshots
   with `allowedRecoveryActions: ['wipe-and-full-sync']` are emitted by
   `startInstalledNode` in `mithrilPartialSyncNodeStartup.ts:116` and `:132` via
   `emitMithrilPartialSyncStatus` (`source/main/ipc/mithrilPartialSyncChannel.ts:132`); the last
   broadcast is readable via `getMithrilPartialSyncStatus` (`mithrilPartialSyncChannel.ts:66`).
   The service's `_status` stays idle, so `_assertRecoveryActionAllowed`
   (`MithrilPartialSyncService.ts:801`) rejects the only offered action. Fix: add
   `adoptRecoverySnapshot(snapshot)` to `MithrilPartialSyncService` (sets `_status` iff the
   service is idle and the snapshot carries a non-idle status with non-empty
   `allowedRecoveryActions`); in `MithrilController`'s recovery-action IPC wirings
   (`MithrilController.ts:495` `wipeAndFullSync`, and the adjacent `retry`/`restartNormal`
   wirings), call `this._partialSyncService.adoptRecoverySnapshot(getMithrilPartialSyncStatus())`
   before delegating. Unit test: broadcast the cross-session failure snapshot, service idle, call
   `wipeAndFullSync`, assert no throw.
2. **T6 (`mithrilPartialSyncNodeStartup.ts:67`).** Wrap the `await fs.remove(marker.stagingRootPath)`
   in the node-start-verified reclaim branch in try/catch: log a warning and continue on failure;
   call `clearMithrilPartialSyncMarker()` regardless. The verified-good DB must never be blocked by
   a staging-cleanup failure. Accepted tradeoff (verified): with the marker cleared there is no
   startup sweep for the stale staging dir — it is reclaimed only by the next partial-sync start's
   `preparePartialSyncStagingDirectory`. Unit test: `fs.remove` rejects (EBUSY) → node start
   proceeds, marker cleared.
3. **T8 (`mithrilPartialSyncNodeStartup.ts:77-107`).** The interrupted-cutover dialog has two
   branches: `response === 0` wipes and returns `false` to resume a healthy full-sync boot
   (verified correct — leave it alone); the Quit branch (`response !== 0`) currently neither quits
   nor emits status, dead-ending the renderer on the connecting screen. Fix: import
   `safeExitWithCode` from `source/main/utils/safeExitWithCode.ts` and call it in the
   `response !== 0` branch after logging (no `app` import exists in this module; do not add a new
   injected dep). Unit test: Quit branch invokes `safeExitWithCode`; continue branch still wipes
   and returns `false`.
4. **T9 (`MithrilPartialSyncService.ts:403-449` `finalizeCancel`) — DD-703-9.** Do NOT restore
   `wipe-and-full-sync` (ADR D-702a-2 stands). Instead make the cancel-path staging removal
   best-effort: when `_cleanupPartialSyncArtifacts()` fails inside `finalizeCancel`, log a warning
   and still emit the `cancelled` status (recovery actions `['retry','restart-normal']`), so
   `restart-normal` proceeds on the intact DB and the orphaned staging dir is reclaimed at the next
   partial-sync start's staging prepare (no marker exists at cancel time — cancel is pre-cutover —
   so startup reclaim never sees it). Keep a `failed` emission only for genuinely unrecoverable
   teardown errors (process kill failure), not for staging-removal rejections. Update the
   `finalizeCancel` specs accordingly (locked-staging case now lands on `cancelled`, not a
   retry-loop `failed`).
5. **T10 (`MithrilPartialSyncService.ts:861` `_getStagingRootPath` + call sites).** All cleanup and
   recovery paths (`wipeAndFullSync`, `restartNormal`, `_cleanupPartialSyncArtifacts`, and the
   startup `cutover-in-progress` wipe) must resolve the staging root **from the marker when a marker
   exists**, falling back to the session-local resolver only when it doesn't. Unit test: marker with
   custom-volume `stagingRootPath` → wipe removes that path.
6. **T14 — no code change (DD-703-6 as revised).** Keep `detached: !environment.isWindows` at both
   spawn sites; the T26 shared helper is written **with** detachment preserved. Optionally add a
   concise rationale comment at the helper's spawn options (detachment makes the child a
   process-group leader so `killProcessTree` can `kill(-pid)` the whole tree; no task ids in
   comments). The checklist reply covers the rest.

### CAT-B — Start flow & availability

7. **T11 (`MithrilPartialSyncSection.tsx:130` area) — DD-703-2 + DD-703-10.** First plumb the
   probe-failure signal (DD-703-10): add `isProbeFailed?: boolean` to
   `MithrilPartialSyncAvailability`, set it in the `getPartialSyncBehindness` catch
   (`MithrilPartialSyncService.ts:1000-1007`, which currently collapses failure into
   `{ isSignificantlyBehind: false }`), and pass it through `MithrilController.ts:191-193` and
   `_applyAvailability` (`MithrilPartialSyncStore.ts:322-334`). Then remove the
   `shouldShowRecommendation`-gated early `return null`; render the section whenever the feature is
   enabled. Branch copy on probe state: behind → existing recommendation copy; not behind →
   new informational message (new i18n key, EN+JA); `isProbeFailed` → new "availability unknown"
   note (new i18n key). CTA (manual trigger to confirmation) enabled in all three. Also apply N7
   here (drop the now-redundant inner `isMithrilPartialSyncEnabled` re-check in
   `DaedalusDiagnostics`).
8. **T12/T31 (`MithrilPartialSyncStore.ts:413`, flag set at `:391`).** In `startPartialSync`: stop
   keying the rethrow on the optimistic `'stopping-node'` status. New rule: any rejection from the
   start IPC **rethrows to the caller** after the status resync **unless** the resynced status is
   `'failed'` (backend-published error owns the surface; use the exact status check — the
   terminal set is `MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES` /
   `isMithrilPartialSyncTerminalStatus`, `mithril-partial-sync.types.ts:87,106`, but `completed`/
   `cancelled` cannot result from a start rejection). On rejection with resynced status `idle`,
   also reset `mithrilAttemptStartedThisSession = false` so the proactive prompt can re-offer —
   wrap that post-await reset in `runInAction` (the flag is observed; mirror the
   `dismissCompletedOverlay` pattern at `:358`).
   Unit tests: preflight rejection surfaces in `startFromConfirmation` and the prompt re-arms.
9. **T15 (`MithrilPartialSyncStore.ts:428-446`) — DD-703-11.** `cancelPartialSync`,
   `restartNormally`, `wipeAndFullSync`: catch rejections, `await this.syncStatus()`, and log the
   extracted message — no new UI surface; the backend-published status stays the sole driver of the
   error view (a genuine failure resyncs to `failed`, which already renders `MithrilErrorView`;
   stale-button races self-correct). Also wrap the overlay retry wiring (`App.tsx:116`,
   `onRetry={mithrilPartialSync.startPartialSync}`) so T12's broader rethrow cannot produce an
   unhandled rejection from the retry Button. Never let a Button `onClick` produce an unhandled
   rejection. Unit test each action with a rejecting IPC stub (assert resync happens and nothing
   throws).
10. **T21 (`MithrilPartialSyncService.ts:964` `_invalidateBehindnessCaches`).** Invalidate
    behind-ness caches on chain-directory change: extend
    `chainStorageCoordinator._notifyDirectoryChanged` to also call the service's invalidation (add
    a wiring point in `MithrilController.resetStartupGateOnDirectoryChange`). Unit test: change
    directory → next probe re-reads instead of serving cache.
11. **T28 (`MithrilPartialSyncService.ts:983` `getPartialSyncBehindness`).** Replace the sequential
    `await` of the aggregator metadata fetch and the local immutable readdir with `Promise.all`.
    Keep `_getCachedCertifiedEpoch()` after the joined await — it reads the cache the aggregator
    fetch populates.
12. **T2 — no code change (DD-703-3).** Checklist reply only.

### CAT-C — Completed-overlay finalize resilience (DD-703-4)

13. **T13 (`MithrilPartialSyncOverlay.tsx:59,84-93`).** Replace the one-shot
    `setTimeout(... Promise.resolve(onDismissCompleted()).catch(() => {}))` with: fire at
    `COMPLETED_AUTO_DISMISS_DELAY_MS`; on rejection retry once (short fixed delay, e.g. 2 s); on
    second rejection set a local `finalizeFailed` state that renders the existing `MithrilErrorView`
    with a `retry` recovery action wired to re-invoke `onDismissCompleted`. Add the finalize-failed
    error copy key (EN+JA) to the CAT-E batch. Implementation note (verified): `'completed'` is in
    the overlay's progress set, so the failure branch needs local component state (attempt/failed
    flags) to switch the render from `MithrilProgressView` to `MithrilErrorView`. Spec: finalize
    rejects twice → error view with working retry; finalize succeeds on retry → overlay dismisses.

### CAT-D — Disk preflight (DD-703-5)

14. **T7 (`MithrilPartialSyncService.ts:1101` `_assertSufficientDiskSpace`).** Implement
    `requiredBytes = max(snapshotBytes − chainDirBytes, 0) + 0.2 × snapshotBytes`, floored at
    `DISK_SPACE_REQUIRED`; measure `chainDirBytes` with the existing recursive helper
    `getPathSizeBytes` (`chainStorageManagerShared.ts:499-520`, exposed as
    `ChainStorageManager._getPathSizeBytes`; the service already holds `_chainStorageManager` and
    `getManagedChainPath()`) — do not write new du logic;
    on measurement failure fall back to `snapshotBytes × PARTIAL_SYNC_DISK_SAFETY_FACTOR`. Keep the
    error's reported required-GB consistent with the new formula. Update the `:70` comment block.
    Unit tests: near-tip (delta ≈ 0) → floor + margin; du failure → old bound; huge delta → old-ish
    bound.
15. **T18 (`mithril-partial-sync.types.ts:33` + `partialSyncErrorCopy.ts:46`).** Add
    `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` to `MithrilPartialSyncErrorCode`; add dedicated
    disk-space copy (EN+JA: what's needed vs available, how to free space) to `COPY_BY_CODE`.

### CAT-E — i18n / copy / vocabulary

16. **T1/T17 (`MithrilStepIndicator.tsx:49,86-97`).** Add `ITEM_ID_TO_MESSAGE` entries (and
    `DOWNLOAD_SUB_IDS`/`FINALIZE_SUB_IDS` membership) for the real partial-sync stage ids
    `verifying`, `converting`, `installing` with proper messages ("Verifying snapshot…",
    "Converting snapshot format…", "Installing snapshot…"; EN+JA). Fix spec fixtures to feed the
    ids the service actually emits, not bootstrap-style ids.
17. **T4 (`MithrilSyncProcessSummary.messages.ts:16`).** "shutdown" → "shut down"; regenerate
    locale files and update any test asserting the old string.
18. **T16 (all user-reachable throw sites — DD-703-12).** Replace user-facing English prose thrown
    in `error.message` with stable error **codes** on the typed error at every site:
    `MithrilPartialSyncService.ts:180`, `:255-260`, `:355-356`, `:556-557`, `:567-568`, `:669-673`,
    `:805-813`, `:901-903`, `:1080-1082` (banned "immutable"), `:1121-1124` (T18), and the
    `_buildError` fallback `:1290` (`_buildError` copies `error.message` verbatim into
    `status.error`, which `MithrilErrorView` renders — the main leak path). Extend the T18 union as
    needed (e.g. `PARTIAL_SYNC_DISABLED`, `PARTIAL_SYNC_ALREADY_RUNNING`,
    `PARTIAL_SYNC_LAYOUT_UNSUPPORTED`); renderer maps codes to intl copy in
    `partialSyncErrorCopy.ts`. Keep the original prose in the log line, not the thrown message.
19. **T19 (`MithrilErrorView.tsx:105,130,159`).** Body copy comes only from the code/stage-keyed
    intl maps; raw `error?.message` may appear only inside the collapsed details section.
20. **T20/T32 (`SyncingConnectingMithrilPrompt.tsx:22` +
    `components/status/MithrilPartialSyncConfirmation.tsx:18`).**
    Switch both strings to ICU plural: `{epochs, plural, one {# epoch} other {# epochs}}`; update
    EN+JA locale entries (JA: plural forms collapse to `other`) and the snapshot/spec assertions.
21. **T22 (`MithrilPartialSyncSection.tsx:107,115`).** Replace the hardcoded fallback
    "Unable to start Mithril partial sync." with an intl message using approved vocabulary
    ("Unable to start Mithril Sync."), shared with the T23 helper below.

### CAT-F — Structure / duplication / dead code / perf

22. **T23 (`SyncingConnectingMithrilPrompt.tsx:151`, `MithrilPartialSyncSection` `startFromConfirmation`,
    `MithrilPartialSyncStore.toStartError`).** Extract one shared helper
    (e.g. `renderer/app/utils/mithrilErrorMessage.ts`) for error-message extraction with a single
    intl-backed fallback; use it at all three sites. Verified divergences the helper must absorb:
    the prompt fallback says "Unable to start Mithril sync." (`:151-154` — itself a hardcoded
    English string this helper fixes), the section/store say "…Mithril partial sync."; only
    `toStartError` handles the non-Error `{message}` shape; the components need a string while the
    store throws an `Error`. Unify the fallback on the T22 intl message.
23. **T24 (`mithril-partial-sync.types.ts:123`).** `isMithrilPartialSyncBlockingNodeStart` delegates
    to `isMithrilPartialSyncWorkingStatus` instead of an inlined array copy.
24. **T25 (`MithrilPartialSyncOverlay.tsx:42`).** Replace the hand-copied `PROGRESS_STATUSES` array
    with `isMithrilPartialSyncWorkingStatus(status) || status === 'completed'`.
25. **T26 (`mithrilCommandRunner.ts:159-176,255-272`).** Extract the duplicated spawn block
    (options, spawn-info log, exit/close handlers) into one helper used by `runBinary` and
    `runCommand`. Spawn options — including `detached: !environment.isWindows` — stay unchanged
    (DD-703-6 as revised). Verified parameters the helper must take: spawn args (`runCommand`
    prefixes `['--origin-tag','DAEDALUS']`), binary path (`runBinary` takes `binaryName`,
    `runCommand` hardcodes `mithril-client`), env (`runBinary` uses
    `normalizeSpawnEnv(process.env)`, `runCommand` awaits `buildMithrilEnv`), and optional
    `stdinInput` (`runBinary` only).
26. **T27 (`MithrilPartialSyncService.ts:1011` area).** Collapse the four snapshot CLI methods:
    `listSnapshots` maps `_listSnapshotsRaw` results through the normalizer; `showSnapshot` shares
    the spawn+parse pipeline.
27. **T29 (`MithrilPartialSyncStore.ts:89`) / T30 (`:71,239`).** Delete the dead
    `behindByImmutables` and `elapsedSeconds` observables and their write sites; epochs-only
    display and `startedAt`-derived elapsed time are the survivors.
28. **T3 (`MithrilBootstrap.scss:13`).** Remove the `overlay-backrop` blur mixin from the
    now-opaque `.backdrop` (or restore translucency — decision: remove the mixin; the opaque look
    shipped deliberately in 702).

### CAT-G — Review-body nits N1–N9

29. **N1 — declined, no code change (DD-703-13).** `killProcessTree.ts:55` vs `CardanoNode.ts:1080`
    are not equivalent (group kill of a live `ChildProcess` vs polled single-pid kill of a possibly
    prior-session PID); unifying would change `CardanoNode` shutdown semantics for a nit. Answer in
    the checklist nit summary.
30. **N2** `mithrilSnapshotMetadata.ts:105` `extractCertifiedEpoch`: drop the lookup paths its own
    comment declares dead; remove the bare top-level `['epoch']` fallback.
31. **N3** `chainStorageCoordinator.ts:75`: replace the fabricated availability shape with a
    boolean `isPartialSyncEnabled()` getter.
32. **N4 (DD-703-14)** `MithrilPartialSyncOverlay.tsx:187-231`: replace the 11 pre-formatted intl
    string props into `MithrilProgressView` with a `'bootstrap' | 'partial-sync'` variant flag (the
    view has intl in context and already holds message fallbacks for every one). Guards: `variant`
    defaults to `'bootstrap'`; bootstrap message defaults and the bootstrap `completed` frame stay
    byte-identical (`MithrilProgressView.tsx:146-151`); `completedTransitionLabel` behavior remains
    partial-sync-only; bootstrap stories/specs re-run green (locked boundary 11).
33. **N5** idle-status literal in `MithrilController.ts:54`, `MithrilPartialSyncService.ts:99`,
    `MithrilPartialSyncStore.ts:26`: shared `makeIdlePartialSyncStatus()` factory in the common
    types module.
34. **N6** `MithrilPartialSyncService.ts:714`: drop the dynamic `import('./mithrilCommandRunner')`
    (already statically imported).
35. **N7** covered in step 7 (Diagnostics redundant guard).
36. **N8** `mithril-partial-sync.types.ts:95`: `MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` — remove the
    no-op `'idle'` filter.
37. **N9** `MithrilProgressView.stories.tsx:22` + `MithrilBootstrap.stories.tsx`: move
    `SNAPSHOT_FILES_TOTAL = 980` to `_support/mithrilFixtures`.

### Cross-cutting closeout

38. Update the smoke-test cheat sheet (DD-703-7) for: Diagnostics section now always visible when
    enabled (including the probe-failed "availability unknown" state); new preflight numbers;
    completed-overlay failure→error-view path. (T14 spawn semantics are unchanged — no cheat-sheet
    item.)
39. Regenerate locale artifacts (`defaultMessages.json`, `en-US.json`, `ja-JP.json`) for all CAT-E
    keys; JA translations follow the ja-copy-review conventions
    (`mithril-partial-sync-ja-copy-table.md`).
40. Keep `task-ux-703-pr-comment-checklist.md` in sync if any fix deviates from the response text.

## Acceptance criteria

- Every thread T1–T32 and nit N1–N9 is either fixed per its step above or explicitly answered in
  the checklist with no code change (T2, T14, and N1 qualify).
- Cross-session `starting-node` failure: the wipe-and-full-sync button succeeds (T5 repro spec).
- No recovery flow can dead-end without an interactive recovery action (T8, T9, T13).
- Diagnostics Mithril Sync section visible whenever the feature is enabled, in all three probe
  states (T11).
- Preflight uses the DD-703-5 formula with floor and fallback (T7).
- No user-visible string is hardcoded English, uses "partial sync" vocabulary, renders a raw stage
  id, or pluralizes "1 epochs" (CAT-E).
- No unhandled promise rejection from any Mithril button action (T15, T13).
- `yarn check:all` green; bootstrap flow specs green (locked boundary 11).
- Smoke-test cheat sheet updated (DD-703-7).

## Verification plan (exact commands, run from repo root)

```bash
yarn test:jest --testPathPattern "mithril" # focused suites incl. new T5/T6/T8/T13/T7 specs
yarn test:unit                             # cucumber unit
yarn lint && yarn compile && yarn prettier:check
yarn check:all                             # final gate
```

Environment prep (Node 24 dev container): regenerate `.scss.d.ts` typings via `typed-scss-modules`
after any `.scss` edit (T3) before `yarn compile`; in specs touching `mithrilCommandRunner`, assert
spawn options with `expect.objectContaining` (prettier 2.1.2 never reaches a fixpoint on
`toHaveBeenCalledWith('str', {obj})`); both `mithrilCommandRunner` files carry pre-existing
prettier drift at HEAD — do not reformat beyond the edited hunks.

Storybook: add a finalize-failed story asserting `MithrilPartialSyncOverlay` renders
`MithrilErrorView` with a retry action, and step-label stories covering
`verifying`/`converting`/`installing` (global EN/JA locale toggle). Interactive `yarn storybook`
inspection is an optional author-run check, not part of the autonomous gate.

Tests to add/update per step are named inline above; the T5, T6, T8 startup/recovery specs live
beside `mithrilPartialSyncNodeStartup` and the service specs; overlay/step-indicator specs under
`source/renderer/app/components/loading/mithril-bootstrap/`.

## Risks / open questions

- **T14 resolved pre-build**: verification confirmed cancel relies on process-group kill semantics;
  DD-703-6 revised to no code change — this risk is retired.
- **N4 bootstrap regression**: the variant refactor touches the shared progress view; guarded by
  the byte-identical bootstrap `completed` frame constraint and the bootstrap stories/specs re-run
  (DD-703-14).
- **T7 estimate under-measure**: extra non-chain data in the chain dir shrinks the delta; the
  0.2 × snapshot margin absorbs this by design (DD-703-5). Mid-download disk-full remains covered
  by existing error recovery.
- **JA copy**: new CAT-E keys need JA strings consistent with the ja-copy-review; flag any
  uncertain translation in the impl-review log rather than guessing.
- Threshold calibration (T2) deliberately unresolved — QA follow-up, not this task.

## Required doc / research updates

- `mithril-partial-sync-smoke-test-cheat-sheet.md` (DD-703-7 items).
- `task-ux-703-research.md` — durable grill findings already recorded; append implementation
  gotchas.
- PRD: add a pointer under D7 (preflight) and D9/D-702a-1 (finalize failure path) noting the
  703 amendments.

## Review-log paths

- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-703-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-703-impl-review.md`

## Status

- Planning status: `approved` — grill session 2026-07-03, one critique pass
  (`Decision: requires_changes`, 5 blockers), one Planner fix pass resolving all blockers; then a
  verification grill 2026-07-03 (4-agent code verification of all 41 items): DD-703-6 revised (T14
  no code change), DD-703-10…14 added (T11 plumbing, T15 handling, T16 scope, N1 declined, N4
  guards); see plan-review log.
- Build status: `pending` — not started (pre-build; precedes `in_progress|in_review|completed`).
