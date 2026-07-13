# task-ux-302 — Research Notes

Durable findings from planning + implementation of the proactive syncing-screen Mithril prompt (D13).

## Decision: D13 redesign (2026-06-25, explicit user sign-off)
The originally-planned D1 design — a "Review" button that deep-links into the Diagnostics confirmation
modal via an `openDaedalusDiagnosticsDialog({ showMithrilConfirmation })` payload — was **replaced** by user
decision with a self-contained **fast/standard fork prompt + concise inline confirm + direct start**. Recorded
as PRD decision **D13** (amends D1's deep-link mechanism and D12's "sync-% retained" clause). Net effect: the
prompt is simpler (fewer files, no AppStore/Diagnostics changes, no new IPC channel) and confirmation-precedes-start
(lock #3) is preserved by the in-prompt confirm view rather than the Diagnostics modal.

Related governing-doc updates: PRD §D13 (+ pointers in D1 / UX Flow 1 / Functional reqs); tasks JSON task-ux-302
(rewritten) and task-ux-601 (now owns the sprint-wide "Mithril" vocabulary rollout + confirmation-modal sync-%
removal); memory `ux-copy-cardano-vocabulary` updated to epochs-only + "Mithril Sync"/"standard sync" vocabulary.

## Load-bearing code seams (verified against live code)
- **Start action (reused, no new channel):** `MithrilPartialSyncStore.startPartialSync` — `@action` at
  `MithrilPartialSyncStore.ts:202-231` (async; sets `START_PENDING_STATUS` = `stopping-node`, awaits
  `mithrilPartialSyncStartChannel.request()` at :215, re-throws `toStartError` on failure at :230). Its only other
  call sites are `DaedalusDiagnosticsDialog.tsx:142` (modal) and `App.tsx:115` (retry) — both go through this same
  store action. Calling it flips status into the overlay lifecycle, so the partial-sync overlay takes over the screen.
- **Start-error / late-reject reference pattern:** `MithrilPartialSyncSection.tsx:36,45-47,83-104` (with mirror specs
  at `MithrilPartialSyncSection.spec.tsx:84-152`) — the prompt's `handleStart` mirrors this (`_isMounted` guard,
  `isStarting` disable, start-error line).
- **behindByEpochs (renderer node-tip difference, D12):** computed as `Math.max(1, networkTip.epoch − localTip.epoch)`
  with a behind-unknown fallback; the canonical block lives in `DaedalusDiagnostics.tsx` (task-ux-304) and is mirrored
  into `SyncingConnectingPage.tsx`. `SyncingConnectingPage` already injects `networkStatus`, so `networkTip`/`localTip`
  were available without new plumbing.

## Naming gotcha (JSON/PRD vs live code)
The tasks JSON / PRD say `isEnabled`; the **live store observable is `isPartialSyncEnabled`**
(`MithrilPartialSyncStore.ts`), fed by the availability read model's `availability.isEnabled` field. Gate on
`isPartialSyncEnabled`. (The IPC payload field is `isEnabled`; the store getter is `isPartialSyncEnabled`.)

## Residual / follow-ups (tracked, not dead-ends)
- The deep-link seam built by tasks 301/303 — `showConfirmationOnOpen?` prop on `MithrilPartialSyncSection`
  (componentDidMount opens the modal) + `DaedalusDiagnostics` threading + `DaedalusDiagnosticsDialog.tsx:138`
  hardcoding `showMithrilPartialSyncConfirmationOnOpen={false}` + the `AppStore` listener ignoring its payload — is
  now **unused** (the prompt no longer deep-links). Left compiling and dead (fed `false`); its removal is a benign
  cleanup deferred to **task-ux-601** (or a dedicated hygiene pass). Flag so it does not become permanent dead code.
- Sprint-wide "Mithril" vocabulary + confirmation-modal sync-% removal: **task-ux-601**.
- Prompt visual/positioning polish (`.scss` uses `position: fixed; top:0`): **task-ux-502**.

## Environment note (not a code issue)
Under Node v24, `jest-css-modules-transform` cannot resolve dart-sass, and `yarn compile`/`yarn test:jest` need the
repo's documented workaround (`tsc --noEmit` directly; map `.scss` → `identity-obj-proxy` via a CLI-only jest override).
Committed jest/tsconfig config is unchanged; this is a pre-existing env defect, not a regression introduced here.

## Self-unmount-on-success behavior
Starting from the prompt does NOT flip the gate false (`isSignificantlyBehind` stays true during `stopping-node`).
Prompt removal relies on App/LoadingPage routing swapping in the partial-sync overlay once status becomes active; the
double-start safety net is the confirm view's `isStarting`-disabled "Start now". Worst case is a transient visual, not
a double-start. Flagged for the QA matrix (task-ux-702).
