# task-ux-203 — Plan review transcript (append-only)

Append entries at the END only. Never rewrite history. Each entry: speaker label, ISO-8601 UTC
`Timestamp:`, and an outcome. Critiquer/Code-Review entries END with `Decision: approved` or
`Decision: requires_changes`.

---

Planner:
Timestamp: 2026-06-24T14:03:14Z

Produced the canonical plan for task-ux-203 (Guarantee cancel always re-emits status — the BACKEND HALF of
PRD D5(f) / research-19 gap #39). Planning status `draft`, build status `in_progress`. Interaction mode
`autonomous` (backend-only; no user-facing copy; re-emits an existing in-memory snapshot; no
destructive/irreversible action; fully specified by D5(f)).

CORE CHANGE (smallest truthful): in `MithrilPartialSyncService.cancel()`, replace the silent `return;` on the
early-return path (`MithrilPartialSyncService.ts:270`, inside the `:265-271` branch) with
`this._updateStatus({}); return;`. `_updateStatus({})` re-merges `this._status` over itself and emits
`this._statusEmitter.emit('status', { ...this._status })` (`:873`) — a verbatim re-emit of the TRUE current
status. Do NOT fabricate `cancelled` (nothing is active to cancel in the node-stop window; fabricating a
recovery surface would lie to the renderer / violate lock #2). Reuse the existing `_updateStatus` primitive
rather than a new `_emitCurrentStatus()` helper (orchestrator's reuse preference; smallest change).

TWO PLANNING QUESTIONS — RESOLVED WITH EVIDENCE:

(a) Does `source/main/ipc/mithrilPartialSyncChannel.ts` need a change, or is the service-level re-emit
sufficient? → **VERIFIED NO CHANGE NEEDED.** Full trace of the status PUSH (independent of the cancel
channel): `mithrilPartialSyncChannel.ts:100-102` installs `_partialSyncStatusSender` that pushes any status
over the STATUS channel; `controller.initialize()` (`:104`) wires `MithrilController.ts:111-118`
(`_partialSyncService.onStatus(status => broadcastPartialSyncStatus(status))`); `broadcastPartialSyncStatus`
(`MithrilController.ts:253-269`) sets `_partialSyncStatus = status` (`:256`) AND invokes
`_partialSyncStatusSender?.(status)` (`:263`). So the `cancel()` re-emit fires the EventEmitter
(`MithrilPartialSyncService.ts:873`) → controller broadcast → renderer push. The cancel channel itself
(`mithrilPartialSyncCancelChannel`, `:41-44`/`:115-117`) is `void → void` and never carries status, so it
does not change. Also confirms task-ux-404's `syncStatus()` pull (`getPartialSyncStatus()` →
`_partialSyncStatus`, `MithrilController.ts:163-165`, refreshed at `:256`) lands on the same re-emitted
truth. The JSON targetPaths list the channel file, but the verified disposition is "no change needed";
recorded as the targetPaths reconciliation.

(b) Include or defer the optional stopping-node emit? → **DEFERRED.** Justified: (1) `stopping-node` is
documented renderer-optimistic-only (research-19 section 3); having the backend author it is a separate
status-model change (pairs with D8#19), not the cancel-resync fix. (2) The service does NOT stop the node on
cancel — `chainStorageCoordinator.cancelPartialSync` (`utils/chainStorageCoordinator.ts:274-278`) is a thin
pass-through to `handlers.cancel()` with NO `_ensureNodeStopped…` and NO mutation lock (contrast restart/wipe
at `:297`/`:325`). Emitting `stopping-node` from the service would invent an unowned transition. (3) Not
required for acceptance: a cancel producing *a* status emission already satisfies "renderer can act on it /
never sticks." Shipped change is the early-return re-emit only.

ANCHOR DRIFT CORRECTED (prefer live code): JSON description + implementationNotes and PRD D5(f) cite
`MithrilPartialSyncService.ts:241-247` (early-return) and `:249-253` (post-cutover throw); LIVE code is
`:265-271` (early-return, bare `return` at `:270`) and `:273-277` (throw). `_updateStatus` is `:842-874`
(emit at `:873`); `_shouldTrackElapsed` `:876-880`; `onStatus`/`_statusEmitter` `:126-131`/`:100`. All other
anchors in the orchestrator prompt verified accurate (cancel `:264`, controller cancel route `:404-408`,
status-sender install `mithrilPartialSyncChannel.ts:100-102`, getPartialSyncStatus `:163`).

BENIGN EDGE DOCUMENTED: `_updateStatus({})` refreshes `transferProgress.elapsedSeconds` only for tracking
statuses (`_shouldTrackElapsed` false for `idle`/`cancelled`/`failed`/`completed` — exactly the terminal-ish
statuses present in the node-stop window), so the re-emit is pure-verbatim there. Even otherwise the value is
true wall-clock elapsed, not a synthesized progress number → lock #8 intact.

LOCKED INVARIANTS honored inline: #6 (post-cutover throw `:273-277` byte-unchanged; the re-emit is on the
mutually-exclusive pre-cutover early-return branch; Test B is the regression guard) · #2 (re-emit carries
existing `allowedRecoveryActions` verbatim; no fabricated recovery surface) · #8 (existing snapshot; no
synthesized throughput/%/remaining-time; no raw client JSON) · #11 (only the cancel early-return path
touched). No fabricated `cancelled`.

FILES TO CHANGE: `source/main/mithril/MithrilPartialSyncService.ts` (one-line re-emit) +
`source/main/mithril/MithrilPartialSyncService.spec.ts` (two tests). `mithrilPartialSyncChannel.ts` NOT
changed (Decision (b)). 

TESTS: Test A — cancel during node-stop window (`_activeWorkDir`/`_currentProcess` null) emits exactly one
status equal to the current status (no fabricated `cancelled`), captured via `service.onStatus` (pattern from
`:538-540`). Test B / existing `:781-797` — post-cutover cancel still throws unchanged, with an emission-count
guard proving the early-return re-emit did not leak into the throw path. Both testCases mapped 1:1 to
acceptance criteria.

ACCEPTANCE (verbatim from JSON): (1) a cancel request always produces a status emission the renderer can act
on; (2) post-cutover cancel rejection is preserved.

VERIFICATION: `./node_modules/.bin/tsc --noEmit`; `./node_modules/.bin/jest
source/main/mithril/MithrilPartialSyncService.spec.ts`; `./node_modules/.bin/eslint` on the two touched files
(baseline-warning comparison).

Smallest-truthful-change confirmed: one production line, reuse of `_updateStatus({})`, no new
channel/helper/field, no renderer logic (task-ux-404), no abort-during-stop (D7), no coordinator change. No
open questions for the user.

---

Critiquer:
Timestamp: 2026-06-24T14:09:12Z

Did one broad pass: verified every live-code anchor, the gap/PRD/JSON coverage, the locked invariants, the
channel-file disposition, and the test plan — then EMPIRICALLY validated the core change by prototyping it and
reverting (working tree left byte-identical to pre-review).

ANCHORS — all re-verified accurate against live code (the planner's drift correction is correct):
- `cancel()` `:264`; silent early-return `:265-271`, bare `return` at `:270` (the seam) ✓
- post-cutover throw `:273-277` (lock #6) ✓ · `_updateStatus` `:842-874`, emit `:873` ✓ ·
  `_shouldTrackElapsed` `:876-880` (excludes idle/cancelled/failed/completed) ✓ ·
  `onStatus`/`_statusEmitter`/`_status` `:126-131`/`:100`/`:99` ✓
- controller subscription `MithrilController.ts:111-118` → `broadcastPartialSyncStatus` `:253-269`
  (sets `_partialSyncStatus` `:256`, dispatches `_partialSyncStatusSender?.(status)` `:262-263`) ✓ ·
  `getPartialSyncStatus()` `:163-165` ✓ · cancel route `:404-408`, handler `:443` ✓
- channel sender install `mithrilPartialSyncChannel.ts:100-102`, cancel handler `:115-117` void→void ✓ ·
  coordinator `cancelPartialSync` pass-through `chainStorageCoordinator.ts:274-278` (no node-stop, no lock,
  contrast restart/wipe) ✓ · spec import `:4`, capture pattern `:538-540`, existing post-cutover test
  `:781-797` ✓.
- Anchor drift (JSON/PRD `:241-247`/`:249-253` → live `:265-271`/`:273-277`) is real and correctly resolved
  in favor of live code.

EMPIRICAL VALIDATION (prototyped `this._updateStatus({});` + the 3 proposed tests in a scratch spec, then
fully reverted): all pass — exactly ONE verbatim emission on the early-return path for `completed` AND `idle`
(no fabricated `cancelled`); post-cutover cancel still throws the unchanged message with ZERO emissions (lock
#6 intact, no leak into the throw path); ALL 36 existing spec tests pass with the change applied;
`tsc --noEmit` clean. Coverage report confirms lines `266-270` are currently uncovered — Test A lands exactly
on the new seam. So the plan's central behavioral claims are not just argued, they are demonstrated.

COVERAGE: PRD D5(f) (`prd:297-306`) backend half fully covered; gap #39 (`research-19:392`) closed; JSON
acceptance met exactly (criterion 1 mandates "a status emission" — the re-emit IS the required deliverable,
not belt-and-suspenders). Renderer half (disable-Cancel + always-`syncStatus()`) correctly scoped to
task-ux-404; D7 abort-during-stop correctly deferred. Channel-file NO-CHANGE disposition is correctly reasoned
with a full status-push trace even though the file is in JSON targetPaths. Stopping-node polish correctly
DEFERRED (service does not own the node-stop on cancel — coordinator pass-through proves it). Locked invariants
#6/#2/#8/#11 all honored inline; the `elapsedSeconds` edge analysis is correct (non-firing for the
terminal-ish statuses in the node-stop window; truthful wall-clock even otherwise → lock #8 intact).

NON-BLOCKING OBSERVATIONS (optional implementer notes, NOT plan defects): (1) `broadcastPartialSyncStatus`
dispatches the renderer SENDER asynchronously (`Promise.resolve().then()`, `MithrilController.ts:262-263`) but
sets `_partialSyncStatus` synchronously (`:256`); the service-level spec asserts on `service.onStatus` (fires
synchronously at `:873`), so this does not affect the tests or correctness — worth a one-line mention if the
impl wants to be explicit. (2) Test B's emission-count guard is the only strictly-new assertion (the existing
`:781-797` test already proves the throw); the plan already acknowledges this. (3) The research note +
impl-review do not yet exist — consistent with the task-ux-202 template, where the research note is an
impl-time deliverable, not a plan-gate artifact; the plan lists exactly what it must record. None of these
block.

The plan is small-model-implementable from the doc alone (exact before/after blocks, exact test bodies, exact
commands), smallest-truthful (one production line + two tests, reusing `_updateStatus`), and convergent. No
changes required.

Decision: approved

---
