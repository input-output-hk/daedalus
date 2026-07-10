# task-ux-705 — Mithril interrupt path for nodes stuck in a full ledger replay

> Retrospective capture of work already committed by Samuel Leathers on 2026-07-06 through 07-09,
> on top of the task-ux-704 baseline. Six commits, all landed directly on
> `feat/mithril-partial-sync-ux-refinement`. This doc records what shipped and why; it is not a
> plan-ahead task. Commit hashes are intentionally omitted — this branch is rebased frequently, so
> commits are identified by subject line and ordinal below.

- Sprint: Mithril Partial Sync UX Refinement — phase-7
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Author: Samuel Leathers (`samuel.leathers@iohk.io`)
- Build status: `completed` (shipped commits — see inventory below)
- Priority: high · Dependencies: task-ux-704 (completed)

## Why (the gap these commits close)

The proactive Mithril prompt and the earlier UX only helped a node whose on-disk immutable-file
count is *far* behind the Mithril-certified tip (`isSignificantlyBehind`, an immutable-file-gap
threshold). A different failure mode was left uncovered: a node with recent on-disk blockchain data
but **missing, corrupted, or incompatible ledger state** (e.g. the ledger was deleted, or came from
a snapshot the node rejects). Its immutable gap is small, so `isSignificantlyBehind` stays false and
no offer ever surfaces — yet the node drops into a full from-genesis ledger replay that Mithril Sync
could short-circuit by restoring a certified snapshot (ledger state plus any needed immutable
chunks).

These five commits close that gap end to end: the backend can now target the certified position even
when immutables are already at/ahead of it, the availability probe fires on a fresh install, the
proactive prompt stops over-gating itself, an always-available inline interrupt button appears during
replay, and disk-space polling no longer interferes with (or delays) an in-flight run.

## Commit inventory

| # | Date | Subject |
| --- | --- | --- |
| 1 | 2026-07-06 | fix(mithril): remove over-restrictive gate that blocked proactive prompt during replay |
| 2 | 2026-07-06 | feat(mithril): add inline interrupt button during ledger replay |
| 3 | 2026-07-06 | fix(mithril): suppress disk space polling during partial sync and trigger node start on finalizing |
| 4 | 2026-07-06 | fix(mithril): allow ledger-only restore when immutables are at or ahead of certified tip |
| 5 | 2026-07-07 | fix(mithril): show replay interrupt button from the start of any ledger replay |
| 6 | 2026-07-09 | fix(loading): debounce Mithril sync button during ledger replay |

Net diffstat over the range: 13 files, +235 / −31.

## Work items

### WI-1 — Un-gate the proactive prompt so it surfaces during replay (commit 1)

`MithrilProactivePromptContainer.tsx` was silently blocked by two conditions that held even after the
backend had confirmed an offer-worthy gap:

- `(isBehindnessKnown || certifiedKnown)` — required a finite `networkTip.epoch` or `certifiedEpoch`.
  During node replay the network tip resolves late and `certifiedEpoch` is absent when the
  mithril-client JSON carries no beacon field, so the gate could stay closed indefinitely despite
  `isSignificantlyBehind === true`.
- `if (behindByEpochs === undefined) return null` — hid the prompt when the user had semi-recent data
  whose `localTip.epoch` was at/above the certified snapshot epoch, even though immutable files were
  still missing.

Fix: remove both. `isSignificantlyBehind` starts `false` and flips `true` only after the availability
probe succeeds, so it is sufficient anti-flash protection on its own. When `behindByEpochs` is
`undefined`, `SyncingConnectingMithrilPrompt` already falls back to *"Your node is behind the
blockchain tip."* rather than hiding.

Probe robustness rider — `MithrilPartialSyncService._getCachedLocalImmutableNumber`: a fresh install
has no `immutable/` directory (or an empty one), which previously threw before any offer could be
computed. It now returns local position `0` when the directory is absent and maps
`PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE` to `0`, so the probe fires for a no-snapshot node.

Spec: `MithrilProactivePromptContainer.spec.tsx` — the two anti-flash tests were reframed. The
"near-tip hides (epoch diff 0)" case now asserts the prompt *renders* with the generic
behind-the-tip fallback; the "hidden until behind-ness KNOWN" case now asserts the prompt *renders*
even with `isBehindnessKnown: false`. The shared fixture comment was updated to name
`isSignificantlyBehind` as the sole gate.

Files: `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx`,
`source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx`,
`source/main/mithril/MithrilPartialSyncService.ts`.

### WI-2 — Inline "Use Mithril Sync instead" interrupt button during replay (commits 2 + 5 + 6)

`isSignificantlyBehind` alone can't catch the small-immutable-gap / missing-ledger case, so an
explicit inline affordance was added: a **"Use Mithril Sync instead"** button rendered below the
`SyncingProgress` table on the replay screen.

- Visibility: shown whenever the replay screen is visible, `isPartialSyncEnabled` is true,
  `onMithrilSync` is provided, and the replay is not yet complete. No session guards — the user may
  interrupt at any point during an active replay.
- Three-step evolution captured here: commit 2 gated on `replayedBlock > 0 && < 100`; commit 5 dropped
  the `> 0` guard because a missing/incompatible-ledger node begins a genesis replay while
  `replayedBlock` sits at `0` until blocks accumulate; commit 6 stopped rendering directly off the raw
  condition and put it behind a debounce. Eligibility is now
  `isPartialSyncEnabled && replayProgress < 100 && onMithrilSync != null`, but the button only renders
  once that has held continuously for `MITHRIL_BUTTON_DEBOUNCE_MS` (2000 ms).
- Debounce mechanics (commit 6): `SyncingConnectingStatus` became a stateful component
  (`state.showMithrilButton`) with a `componentDidUpdate` timer. When eligibility first turns true it
  arms a 2 s `setTimeout`; the button is revealed only if `_isMithrilButtonEligible()` still holds when
  the timer fires. If eligibility drops before then, the pending timer is cleared and
  `showMithrilButton` resets to `false`; the timer is also cleared on unmount. The JSX now gates on
  `this.state.showMithrilButton && onMithrilSync` instead of the raw condition. This kills the flicker
  that occurred when `replayProgress` / node-state transitions momentarily satisfied the condition.
- Wiring: `isPartialSyncEnabled` and `onMithrilSync` (→ `mithrilPartialSync.startPartialSync`) thread
  from `SyncingConnectingPage` → `SyncingConnecting` → `SyncingConnectingStatus`. `SyncingProgress`
  stays a pure display component.
- Copy: new i18n message `loading.screen.mithrilSyncInterrupt` = *"Use Mithril Sync instead"*.
- Style: `.mithrilAction` (`margin-top: 20px; text-align: center`).
- Storybook: the default `SyncingConnecting` story gains `isPartialSyncEnabled` and `onMithrilSync`
  knobs for manual testing.

Files: `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingStatus.tsx`,
`.../SyncingConnectingStatus.scss`, `.../SyncingConnecting.tsx`,
`source/renderer/app/containers/loading/SyncingConnectingPage.tsx`,
`storybook/stories/nodes/syncing/SyncingConnecting.stories.tsx`.

### WI-3 — Ledger-only restore when immutables are at/ahead of the certified tip (commit 4)

The interrupt button was inert for the missing-ledger case because `derivePartialSyncRange` threw
`PARTIAL_SYNC_NO_CERTIFIED_RANGE` whenever `localImmutableNumber >= latestCertifiedImmutableNumber`.

Fix (`mithrilPartialSyncPreflight.ts`): instead of throwing, return `{ start: N, end: N }` where
`N = latestCertifiedImmutableNumber`. This restores the ledger state at the certified position plus
the one overlapping immutable chunk (harmless under `--allow-override`); the node then replays only
the small trailing gap up to its local tip.

Files: `source/main/mithril/mithrilPartialSyncPreflight.ts`.

### WI-4 — Stop disk-space polling from interfering with / delaying a run (commit 3)

On mainnet the 10-minute disk-space poll interfered with an active partial sync and, after install
completed, caused an ~8.5-minute hang before the node started. Two fixes:

- Suppress the disk-space check while partial sync is in a working phase. New
  `isMithrilPartialSyncSuppressingDiskSpaceCheck(status)` in `mithril-partial-sync.types.ts` covers
  `stopping-node`, `cancelling`, `preparing`, `downloading`, `verifying`, `converting`, `installing`.
  `handleDiskSpace` returns a stale response for those statuses. `finalizing` / `starting-node` are
  deliberately excluded so the node is allowed to start.
- Start the node promptly: `MithrilController.broadcastPartialSyncStatus` now fires
  `_restartStartupFlowAfterPartialSync()` immediately on the transition into `finalizing` (snapshot
  installed, node start imminent) instead of waiting for the next 10-minute poll tick. The trigger is
  edge-guarded (`previousStatus !== 'finalizing'`) and its rejection is logged, not thrown.

Files: `source/common/types/mithril-partial-sync.types.ts`, `source/main/utils/handleDiskSpace.ts`,
`source/main/mithril/MithrilController.ts`.

## Behavior deltas (intentional)

- New user-visible affordance: the "Use Mithril Sync instead" button on the replay screen (WI-2),
  revealed after a 2 s debounce so it does not flicker during transient replay/node-state transitions.
- The proactive prompt now appears in cases it previously suppressed (WI-1): during late-resolving
  network tip / absent beacon, and for semi-recent-data nodes whose epoch diff is ≤ 0 but whose
  backend gap is confirmed. It falls back to generic behind-the-tip copy when no epoch anchor exists.
- A Mithril restore may now target the certified position even with no missing immutable files (WI-3);
  under `--allow-override` this is the deliberate ledger-only path.
- Disk-space status is served stale (last-known) during working partial-sync phases (WI-4).

## Residual gaps / follow-ups (not addressed here)

- No automated coverage yet for the WI-2 button visibility matrix (`replayProgress` boundaries,
  `isPartialSyncEnabled` off, `onMithrilSync` absent, and now the 2 s debounce reveal/cancel timing)
  beyond the Storybook knobs — candidate for a `SyncingConnectingStatus` spec.
- WI-3's ledger-only restore relies on `--allow-override` tolerating the single overlapping immutable
  chunk; worth an explicit backend/E2E assertion of the post-restore trailing-replay behavior.
- Manual QA on Windows for the missing/corrupted/outdated-ledger replay-interrupt flow (see the
  related unclean-shutdown / chunk-revalidation finding — the "replay from scratch" symptom there was
  ImmutableDB validation, not ledger replay).
