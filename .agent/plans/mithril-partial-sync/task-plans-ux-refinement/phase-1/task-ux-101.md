# task-ux-101 — Add MithrilPartialSyncAvailability read model, dedicated channel, and kill-switch exposure

- Sprint: Mithril Partial Sync UX Refinement — phase-1 (Cross-Process Availability Contract And Behind-ness Signal)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: critical · Estimated: 4h · Dependencies: none

## Why now
This is the foundational phase-1 contract. Every renderer discovery surface in phase-3 gates on
`isEnabled` from this read model. Today the kill switch `launcherConfig.mithrilPartialSyncEnabled`
is enforced only deep in the main process (`chainStorageCoordinator._assertPartialSyncFeatureEnabled`,
line 376) and is INVISIBLE to the renderer — research-19 blocker #1 (row 128). The renderer shows an
always-enabled Start button that throws the raw rejection string
"Mithril partial sync is disabled by launcher configuration." when the flag is off. This task wires the
CONTRACT + kill-switch half so the renderer can later hide all partial-sync UI when the feature is off.
Implements PRD D3 and lays the contract surface for D2.

## Interaction mode justification
`autonomous`: this is a mechanical, well-bounded contract/plumbing change mirroring five existing
partial-sync channels that already exist verbatim in the repo. No product/UX judgment, no user-facing
copy, no network or destructive operation. The one nix flag flip is explicitly pre-decided by PRD D3
(branch-scoped). No operator/user involvement required.

## Scope
1. Define and export `MithrilPartialSyncAvailability` in `mithril-partial-sync.types.ts`.
2. Add ONE dedicated availability channel (constant + `RendererRequest = void` / `MainResponse =
   MithrilPartialSyncAvailability`) in `api.ts`, mirroring the five existing partial-sync channels.
3. Add the main-process channel object + an `onRequest` handler registered inside
   `handleMithrilPartialSyncRequests`, and the renderer channel object.
4. Source `isEnabled` from `launcherConfig.mithrilPartialSyncEnabled === true` via a new
   `chainStorageCoordinator.getPartialSyncAvailability()` method, reached through a thin
   `MithrilController.getPartialSyncAvailability()` passthrough (see DESIGN DECISION).
5. Flip `nix/internal/launcher-config.nix:409` `mithrilPartialSyncEnabled = false;` → `true;`
   (BRANCH-SCOPED per PRD D3).
6. Document the new channel in `.agent/system/api-endpoints.md`.

## Non-goals (explicitly task-ux-102 or later)
- Do NOT compute behind-ness. In this task `isSignificantlyBehind` is hardcoded `false` and
  `behindByImmutables` is left `undefined`. The real certified-immutable-gap math is task-ux-102.
- Do NOT widen `MithrilPartialSyncStatusSnapshot` or its channel. Availability is SEPARATE.
- Do NOT add renderer gating (hiding UI on `isEnabled`) — that is task-ux-301. Only the contract +
  the field must be available now.
- Do NOT add periodic-refresh wiring in the store — the contract supports it (one-shot `request()`),
  but store consumption is later-phase work.
- Production default and network-scoping of the kill switch are DEFERRED to the phase-7 readiness gate.

## Dependencies
None. This is the first task in the sprint.

## Research / docs / workflows / skills consulted
- PRD D2 (`...-prd.md:97-128`), PRD D3 (`...-prd.md:130-164`), Technical Design
  (`...-prd.md:630-644`).
- Tasks JSON `task-ux-101` (`...-tasks.json:39-76`) — acceptance + testCases carried verbatim below.
- Research-19 blocker #1 (row 128), §5 locks 19 + the resolved-default note (lines 226-235), glossary
  (lines 287-296) — `research/19-ux-refinement-state-and-gaps.md`.
- IPC workflow `.agent/workflows/ipc.md` (Step-by-Step create-a-channel; note this repo's
  partial-sync channels use `MainIpcChannel` + `.onRequest`, not the doc's `onReceive` example).
- `.agent/system/api-endpoints.md` (Mithril Channels table + the channel-count cell — verify exact
  line anchors and current count against the live file when editing).

## Files expected to change (exact paths)
1. `source/common/types/mithril-partial-sync.types.ts` — add + export the type.
2. `source/common/ipc/api.ts` — add channel constant + type pair; extend the type import on line 81.
3. `source/main/utils/chainStorageCoordinator.ts` — add `getPartialSyncAvailability()` method.
4. `source/main/mithril/MithrilController.ts` — add `getPartialSyncAvailability()` passthrough.
5. `source/main/ipc/mithrilPartialSyncChannel.ts` — add channel object + `onRequest` registration.
6. `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` — add renderer channel object.
7. `nix/internal/launcher-config.nix` — flip line 409 to `true`.
8. `.agent/system/api-endpoints.md` — document the new channel; bump the count cell.
9. `source/main/ipc/mithrilPartialSyncChannel.spec.ts` — add the 3 test cases (see Verification).

## DESIGN DECISION — routing seam for `getPartialSyncAvailability()` (RESOLVED)

Chosen: **(b) MithrilController passthrough delegating to a new `chainStorageCoordinator` method.**

Rationale — consistency with the EXISTING channel-routing pattern:
- Every partial-sync main handler routes `channel → getMithrilController().<method>()`
  (`mithrilPartialSyncChannel.ts:50-54, 93-107`). The controller method then delegates to
  `chainStorageCoordinator.<method>(...)` (`MithrilController.ts:383-414`). Availability must follow
  the same two-hop shape so the handler stays a thin one-liner identical to its four siblings.
- `chainStorageCoordinator.ts` already imports `launcherConfig` (line 9) and already owns the kill
  switch (`_assertPartialSyncFeatureEnabled`, line 376). It is the single source of truth for the
  flag; putting the read there avoids a second `launcherConfig` reader. The coordinator is in this
  task's targetPaths; MithrilController is not, but targetPaths is guidance — touching the controller
  here is the cleanest consistent seam and is explicitly allowed by the task prompt. Note it in the
  review log.
- Clean hand-off to task-ux-102: 102's targetPaths are `MithrilPartialSyncService.ts`,
  `mithrilPartialSyncPreflight.ts`, `chainStorageCoordinator.ts`. In 102 the coordinator method below
  is the exact place that replaces the two stubbed fields with the cached certified-immutable-gap
  result (e.g. by calling into `_partialSyncService`/preflight). The channel, controller passthrough,
  type, and renderer wrapper from THIS task do not change in 102 — only the body of
  `getPartialSyncAvailability()` does. This is the designed extension point.

Rejected (a) coordinator method called directly by the channel handler: would make the availability
handler the only partial-sync handler that bypasses the controller, breaking the uniform routing the
spec test asserts ("registers thin request handlers that delegate to the controller").

Exact signatures (sync; no I/O in this task — the future 102 cache lookup stays synchronous off a
cached value, but if 102 needs async it can change both signatures together):

```ts
// chainStorageCoordinator.ts (new public method, place near isPartialSyncInProgress at :64)
getPartialSyncAvailability(): MithrilPartialSyncAvailability {
  return {
    isEnabled: launcherConfig.mithrilPartialSyncEnabled === true,
    isSignificantlyBehind: false,      // task-ux-102 replaces with certified-immutable-gap signal
    behindByImmutables: undefined,     // task-ux-102 populates the raw gap
  };
}

// MithrilController.ts (new passthrough, place near getPartialSyncStatus at :160)
getPartialSyncAvailability(): MithrilPartialSyncAvailability {
  return chainStorageCoordinator.getPartialSyncAvailability();
}
```

## Implementation approach — ordered, mechanical steps

### Step 1 — Define the type
File: `source/common/types/mithril-partial-sync.types.ts`.
Add this export. Place it directly after `MithrilPartialSyncStatusSnapshot` (currently ends at
line 54), before the `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` const at line 56:

```ts
export type MithrilPartialSyncAvailability = {
  isEnabled: boolean;
  isSignificantlyBehind: boolean;
  behindByImmutables?: number;
};
```
Locked invariant to honor here: the renderer consumes only these three fields and NEVER thresholds
`behindByImmutables`; the backend owns the threshold.

### Step 2 — Add the channel contract in api.ts
File: `source/common/ipc/api.ts`.
2a. Extend the existing type import on line 81 (currently):
```ts
import type { MithrilPartialSyncStatusSnapshot } from '../types/mithril-partial-sync.types';
```
to also import the new type:
```ts
import type {
  MithrilPartialSyncStatusSnapshot,
  MithrilPartialSyncAvailability,
} from '../types/mithril-partial-sync.types';
```
2b. Add the new channel constant + type pair immediately AFTER the last existing partial-sync block
(`MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL`, which ends at line 478). Use the EXACT shape of
the five existing channels (constant name === string value; `RendererRequest = void`; `MainResponse`
= the read model):

```ts
export const MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL =
  'MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL';
export type MithrilPartialSyncAvailabilityRendererRequest = void;
export type MithrilPartialSyncAvailabilityMainResponse =
  MithrilPartialSyncAvailability;
```

New channel constant name (canonical): `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL`.

### Step 3 — Coordinator read method
File: `source/main/utils/chainStorageCoordinator.ts`.
3a. Add `MithrilPartialSyncAvailability` to a type import from the partial-sync types. The file does
not yet import that module; add an import near the existing type imports (lines 3-7):
```ts
import type { MithrilPartialSyncAvailability } from '../../common/types/mithril-partial-sync.types';
```
3b. Add the public method (mirrors the simple `isPartialSyncInProgress()` style at :64). It reads
`launcherConfig` (already imported at line 9, used at line 376) and stubs the behind-ness fields:
```ts
getPartialSyncAvailability(): MithrilPartialSyncAvailability {
  return {
    isEnabled: launcherConfig.mithrilPartialSyncEnabled === true,
    isSignificantlyBehind: false,
    behindByImmutables: undefined,
  };
}
```
Anchor the `isEnabled` polarity to the existing guard at `:375-379`
(`launcherConfig.mithrilPartialSyncEnabled !== true` → disabled), so `=== true` is the exact inverse.

### Step 4 — Controller passthrough
File: `source/main/mithril/MithrilController.ts`.
4a. Add `MithrilPartialSyncAvailability` to the partial-sync types import already used by this file
(it imports `MithrilPartialSyncStatusSnapshot`).
4b. Add the passthrough method next to `getPartialSyncStatus()` (:160-162), mirroring how
`startPartialSync` etc. delegate to `chainStorageCoordinator` (:383-414):
```ts
getPartialSyncAvailability(): MithrilPartialSyncAvailability {
  return chainStorageCoordinator.getPartialSyncAvailability();
}
```

### Step 5 — Main channel object + handler registration
File: `source/main/ipc/mithrilPartialSyncChannel.ts`.
5a. Add the constant to the existing `from '../../common/ipc/api'` value import (lines 3-9):
add `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL,`.
5b. Add the two new types to the type import block (lines 10-21):
`MithrilPartialSyncAvailabilityRendererRequest,` and `MithrilPartialSyncAvailabilityMainResponse,`.
5c. Declare the channel object alongside the others (after the wipe channel at :45-48). Match the
`MainIpcChannel<RendererRequest, MainResponse>` ordering used by every sibling:
```ts
const mithrilPartialSyncAvailabilityChannel: MainIpcChannel<
  MithrilPartialSyncAvailabilityRendererRequest,
  MithrilPartialSyncAvailabilityMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL);
```
5d. Register the handler INSIDE `handleMithrilPartialSyncRequests`, in the idempotent block guarded by
`mithrilPartialSyncRequestsInitialized` (current registrations at :93-107). Add it as the LAST
registration, after the wipe-and-full-sync handler (:105-107), so registration order mirrors the
declaration order from Step 5c; mirror the status handler shape exactly (thin delegate to the
controller):
```ts
mithrilPartialSyncAvailabilityChannel.onRequest(async () =>
  controller.getPartialSyncAvailability()
);
```
Note the ordering matters for the spec's positional `mockChannels[...]` indexing — see Verification.
The new `new MainIpcChannel(...)` call appears at module-eval time; placing the declaration after the
wipe channel makes the availability channel `mockChannels[5]` (the sixth created), and its `onRequest`
is registered after the four action handlers. Account for this in the spec assertions.

### Step 6 — Renderer channel object
File: `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`.
6a. Add `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL,` to the value import (lines 2-8).
6b. Add the two types to the type import (lines 9-20).
6c. Export the renderer channel. Note RendererIpcChannel's generic order is `<MainResponse,
RendererRequest>` (the INVERSE of the main side) — see the existing
`mithrilPartialSyncStatusChannel` at :27-30:
```ts
export const mithrilPartialSyncAvailabilityChannel: RendererIpcChannel<
  MithrilPartialSyncAvailabilityMainResponse,
  MithrilPartialSyncAvailabilityRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL);
```
The renderer obtains availability via `mithrilPartialSyncAvailabilityChannel.request()` (one-shot at
store setup; periodic refresh supported by re-calling). Store consumption is out of scope here.

### Step 7 — Flip the kill switch (BRANCH-SCOPED)
File: `nix/internal/launcher-config.nix`, line 409.
Change `mithrilPartialSyncEnabled = false;` → `mithrilPartialSyncEnabled = true;`.
Per PRD D3 (lines 152-157) and research-19 §5 resolved note (lines 230-235): this is enabled on THIS
branch for full manual testing ONLY. Production release default and network-scoping are DEFERRED to
the phase-7 readiness gate. Add no other nix changes.

### Step 8 — Document the channel
File: `.agent/system/api-endpoints.md`.
8a. In the "Mithril Channels" table, add a new row after the partial-sync status channel row, matching
the existing column formatting:
```
| `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL`  | Renderer → Main | Read kill-switch state + behind-ness availability (one-shot + periodic refresh) |
```
8b. Update the "Channel Categories" count cell for Mithril by +1. VERIFY the current count and exact
table/line anchors against the live file before editing (do not trust a stale count). Keep the
existing partial-sync entry wording/style consistent.

## Locked invariants this task honors (inline)
- The renderer NEVER computes or sees the threshold; it consumes only `isEnabled`,
  `isSignificantlyBehind`, and optionally `behindByImmutables`. Backend owns behind-ness.
- Availability is SEPARATE from the operation status snapshot channel
  (`MithrilPartialSyncStatusSnapshot` / `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL` are untouched).
- The kill switch `mithrilPartialSyncEnabled` is the rollout lever; when off, partial-sync UI must
  hide (renderer gating is task-ux-301; this task only makes `isEnabled` available).
- No auto-trigger; behind-ness fields are stubbed here and computed by the backend in task-ux-102.
- The nix flip is BRANCH-SCOPED; production default deferred to phase-7.

## Acceptance criteria (carried verbatim from tasks JSON)
1. Shared `MithrilPartialSyncAvailability` type compiles and is exported.
2. A dedicated availability channel exists with main and renderer wrappers, separate from
   `MithrilPartialSyncStatusSnapshot`.
3. `mithrilPartialSyncEnabled` is surfaced into the availability read model and set true on this
   branch's launcher config.
4. `api-endpoints.md` documents the new channel.

## Verification plan (exact commands)
Run from repo root `/workspaces/mithril-partial-sync-ux`:
- `yarn compile` — TypeScript must pass (validates the new type, the channel type pair, the controller
  + coordinator method signatures, and the inverse generic ordering on the renderer channel).
- `yarn lint` — no new lint errors in the six touched source files.
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` — existing tests pass + the 3
  new cases below pass.

New test cases to add in `source/main/ipc/mithrilPartialSyncChannel.spec.ts` (carry the 3 testCases
from the JSON). Mirror the existing patterns (mock `getMithrilController`, positional
`mockChannels[...]`, `loadModule()` + `handleMithrilPartialSyncRequests`). Steps:
- Add `getPartialSyncAvailability: jest.fn(() => ({ isEnabled: true, isSignificantlyBehind: false }))`
  to the controller mock (and reset it in `beforeEach`).
- Test 1 — "Renderer one-shot availability query returns isEnabled reflecting launcher config":
  after `handleMithrilPartialSyncRequests`, invoke the availability channel's registered `onRequest`
  callback (the availability channel is the 6th-created → `mockChannels[5]`; its handler is the last
  registered) and assert it resolves to the controller's availability object and that
  `getPartialSyncAvailability` was called once.
- Test 2 — "Availability channel is separate from the status snapshot channel": assert the
  availability channel object is a DIFFERENT `MainIpcChannel` instance than the status channel
  (distinct `mockChannels[1]` vs `mockChannels[5]`), and that the availability handler does not invoke
  `getPartialSyncStatus`.
- Test 3 — "isEnabled false when kill switch off; true on this branch": set
  `getPartialSyncAvailability.mockReturnValue({ isEnabled: false, isSignificantlyBehind: false })`,
  invoke the handler, assert `isEnabled === false`; then mock `{ isEnabled: true, ... }` and assert
  `true`. (This tests the channel surfaces whatever the controller/coordinator returns; the
  coordinator's `=== true` polarity is verified at compile + via the existing coordinator spec which
  already sets `mithrilPartialSyncEnabled: true`.)
- IMPORTANT: re-verify the existing positional assertions in the two current tests
  (`mockChannels[0..4]`) still hold after adding the 6th channel; if the new channel is declared AFTER
  the wipe channel as specified, indices 0-4 are unchanged and the new one is index 5.

## Risks / open questions
- Positional `mockChannels` indexing in the spec is brittle. Declaring the availability channel LAST
  among the `new MainIpcChannel(...)` calls keeps existing indices 0-4 stable. Implementer must verify
  index 5 maps to the availability channel and update only the new assertions.
- RendererIpcChannel generic order is inverted vs MainIpcChannel. Copy the existing
  `mithrilPartialSyncStatusChannel` shape exactly to avoid a swapped-generics compile error.
- `getPartialSyncAvailability()` is synchronous now. If task-ux-102's cached lookup turns out to need
  async, 102 changes both the coordinator and controller signatures together; the channel's
  `async () => controller.getPartialSyncAvailability()` already awaits a value either way.
- OPEN QUESTION (non-blocking, decide at 102 planning): the coordinator method does NOT take the
  mutation lock. For a pure flag read that is correct (matches `isPartialSyncInProgress()` at :64,
  also lock-free). Confirm task-ux-102's cached behind-ness read can likewise stay lock-free (a cached
  value, not a live FS/aggregator read on the hot path). If 102 needs disk under the lock, the seam
  may need to become async + queued. Not blocking for 101.

## Required doc / research updates
- `.agent/system/api-endpoints.md` — Step 8 (new channel row + count bump). Part of the task.
- No PRD/research edits required; D2/D3 and research-19 already describe this read model. Back-
  referencing the channel name in the PRD Technical Design (it says "exact channel name … are
  technical-design details") is optional polish, not required for acceptance.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-1/task-ux-101-plan-review.md`
  (append-only; Planner entry started).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-1/task-ux-101-impl-review.md`
  (append-only; created at implementation time).

## Final outcome (completed 2026-06-22)
Implemented exactly as planned. Added `MithrilPartialSyncAvailability` + the dedicated
`MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL` (separate from the status snapshot channel), routed
`channel → MithrilController.getPartialSyncAvailability() → chainStorageCoordinator.getPartialSyncAvailability()`;
`isEnabled = launcherConfig.mithrilPartialSyncEnabled === true`, behind-ness fields stubbed
(`false`/`undefined`) for task-ux-102. Flipped `nix/internal/launcher-config.nix:409` to `true`
(branch-scoped). Documented the channel in `api-endpoints.md` (Mithril count 10→11). Added 3 jest
tests. Verification: `yarn compile` PASS; `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`
6/6 PASS; `chainStorageCoordinator.spec.ts` 37/37 PASS (no regression); lint clean on touched files.
Plan critique: approved (no blockers). Code review: approved, independently re-verified (no blockers).
Sole deviation: touched `MithrilController.ts` (outside JSON targetPaths) as the consistent routing
seam — anticipated by the DESIGN DECISION and logged. The pre-existing `M .gitignore` change is
unrelated and excluded from this task's commit.

## Status
- Planning status: `approved`
- Build status: `completed`
