# task-ux-101 — Implementation Review Log (append-only)

Implementation: 2026-06-22T10:26:48Z
- `source/common/types/mithril-partial-sync.types.ts` — added + exported `MithrilPartialSyncAvailability` type (3 fields) after `MithrilPartialSyncStatusSnapshot`.
- `source/common/ipc/api.ts` — extended the partial-sync types import; added `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL` constant + `RendererRequest = void` / `MainResponse = MithrilPartialSyncAvailability` pair after the wipe channel.
- `source/main/utils/chainStorageCoordinator.ts` — imported the availability type; added `getPartialSyncAvailability()` reading `launcherConfig.mithrilPartialSyncEnabled === true` (inverse of the `:375-379` guard), stubbing `isSignificantlyBehind: false` / `behindByImmutables: undefined` for task-ux-102.
- `source/main/mithril/MithrilController.ts` — added the availability type to the existing import; added `getPartialSyncAvailability()` passthrough delegating to the coordinator (two-hop pattern, like siblings).
- `source/main/ipc/mithrilPartialSyncChannel.ts` — added value + type imports; declared `mithrilPartialSyncAvailabilityChannel` as a `MainIpcChannel` after the wipe channel; registered its `onRequest` LAST inside `handleMithrilPartialSyncRequests` so it is `mockChannels[5]` and existing indices 0–4 stay stable.
- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` — added value + type imports; exported `mithrilPartialSyncAvailabilityChannel` with the INVERSE `RendererIpcChannel<MainResponse, RendererRequest>` generic order (mirrors the status channel).
- `nix/internal/launcher-config.nix` — flipped line 409 `mithrilPartialSyncEnabled = false;` → `true;` (BRANCH-SCOPED per PRD D3).
- `.agent/system/api-endpoints.md` — added the new channel row after the partial-sync status row; bumped the Mithril Channel Categories count cell from 10 → 11 (verified live: 5 bootstrap + 6 partial-sync rows = 11).
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts` — added `getPartialSyncAvailability` to the controller mock (+ `availabilityEnabled` fixture) and reset it in `beforeEach`; added 3 tests (one-shot returns isEnabled & calls controller once; availability channel is a distinct instance from status and does not call `getPartialSyncStatus`; surfaces isEnabled false then true).

Verification:
- `yarn compile` (tsc --noEmit): PASS — "Done in 19.32s", no type errors. Validates the new type, channel type pair, controller/coordinator signatures, and the inverse renderer generic order.
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`: PASS — Tests: 6 passed, 6 total (3 existing + 3 new). Positional indices 0–4 unchanged; availability handler is `mockChannels[5]`.
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts`: PASS — Tests: 37 passed, 37 total (no regression from the new coordinator method).
- Lint: `npx eslint` failed for an environmental reason (`npm error Invalid property "devEngines.node"`), so ran `./node_modules/.bin/eslint <touched files>` directly: EXIT 0 — 0 errors, 23 warnings, ALL pre-existing (line numbers do not overlap new code at coordinator:69, controller:167–168, api.ts:83/483–487; none reference the new identifiers). Touched files clean of new issues.

Deviations:
- Touched `source/main/mithril/MithrilController.ts`, which is not in this task's targetPaths, as explicitly anticipated by the plan's DESIGN DECISION (controller passthrough is the consistent routing seam). No other deviations.

Code Review: 2026-06-22T10:28:38Z
Re-verification:
- `yarn compile` (tsc --noEmit): PASS — "Done in 17.87s", no type errors. Confirms the new type, the channel type pair, the coordinator/controller signatures, and the inverse `RendererIpcChannel<MainResponse, RendererRequest>` generic ordering.
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`: PASS — Test Suites: 1 passed; Tests: 6 passed, 6 total (3 pre-existing + 3 new). Existing positional assertions `mockChannels[0..4]` unchanged; availability is `mockChannels[5]`, handler registered last.

Independent re-verification of each dimension (all confirmed against live source, not just the impl log):
1. Correctness vs plan (Steps 1-8): FAITHFUL. Type exported with the exact 3 fields. Channel constant name === string value, `RendererRequest = void` / `MainResponse = MithrilPartialSyncAvailability`, matching the 5 siblings. Main side `MainIpcChannel<RendererRequest, MainResponse>` (channel ts:53-56); renderer side `RendererIpcChannel<MainResponse, RendererRequest>` (renderer ts:50-53) — inverse ordering correct. Coordinator polarity `mithrilPartialSyncEnabled === true` (coordinator ts:69) is the exact inverse of the existing `!== true` guard at `_assertPartialSyncFeatureEnabled` (ts:385). Behind-ness STUBBED: `isSignificantlyBehind: false`, `behindByImmutables: undefined`, both annotated as task-ux-102 work.
2. Locked invariants: NO regression. `MithrilPartialSyncStatusSnapshot` and `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL` are untouched (availability is a separate type + separate channel, not a widening). No renderer thresholding logic added — renderer file only declares a channel object. Backend owns behind-ness (coordinator). Nix flip is the only nix change (line 409 false→true) and is branch-scoped. No auto-trigger introduced.
3. IPC/contract drift: NONE. The existing 5 channels are byte-for-byte unchanged. `onRequest` registration is inside the `mithrilPartialSyncRequestsInitialized` idempotent block and placed LAST (channel ts:116-118), so creation order keeps status at index 1 and availability at index 5; the idempotency test still passes.
4. Bootstrap regression: NONE. Diff touches no bootstrap channel, no bootstrap progress component, no bootstrap source. Confirmed via `git --no-pager diff` file list.
5. Tests: REAL and deterministic. Test 1 asserts the one-shot handler resolves to the controller's availability object and `getPartialSyncAvailability` called exactly once. Test 2 asserts the availability channel instance is distinct from the status channel (`mockChannels[5] !== mockChannels[1]`) and that the handler does NOT call `getPartialSyncStatus`. Test 3 drives the mock to `isEnabled:false` then `true` and asserts the handler surfaces each verbatim — a legitimate pass-through assertion (channel returns the controller's value), not a mock-against-itself tautology. Coordinator `=== true` polarity is compile-checked here and exercised by the existing coordinator spec (37/37).
6. Docs: api-endpoints.md row added after the status row with consistent column formatting; count cell bumped 10 → 11. Verified against the live table: 5 bootstrap rows + 6 partial-sync rows = 11. Correct.
7. Unnecessary complexity: NONE. Minimal, mechanical mirror of existing channels. Two-hop routing (controller → coordinator singleton, confirmed `export const chainStorageCoordinator = new ChainStorageCoordinator()` at coordinator ts:490) matches all siblings.

[MINOR] `.gitignore` adds `+.devcontainer` — outside this task's declared files and unrelated to the contract. Benign env/tooling entry, not a source or contract change; no fix required, noted for cleanliness only.

No blockers.
Decision: approved
