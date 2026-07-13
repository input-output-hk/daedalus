# task-ux-101 — Plan Review Log (append-only)

Planner: 2026-06-22T10:14:14Z
Drafted the canonical plan for task-ux-101 (availability read model + dedicated IPC channel +
kill-switch exposure, PRD D3 / contract surface for D2). Resolved the open DESIGN DECISION: the new
`getPartialSyncAvailability()` read is owned by `chainStorageCoordinator` (which already imports
`launcherConfig` and owns the kill switch at :376) and reached via a thin `MithrilController`
passthrough, matching the existing two-hop routing every partial-sync channel uses
(channel → getMithrilController().method → chainStorageCoordinator.method). This keeps the new
handler a one-line sibling of the other five and gives task-ux-102 a single, clean place — the
coordinator method body — to plug in the cached certified-immutable-gap math without touching the
channel, type, controller, or renderer wrapper. In 101 the behind-ness fields are stubbed
(`isSignificantlyBehind: false`, `behindByImmutables: undefined`). New channel constant:
`MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL` (RendererRequest = void / MainResponse =
MithrilPartialSyncAvailability), kept separate from the status snapshot channel. Touching
MithrilController is outside the JSON targetPaths but is the cleanest consistent seam, allowed per the
task prompt and noted here. nix:409 flip to `true` documented as branch-scoped. One non-blocking open
question flagged: whether 102's behind-ness read can stay synchronous/lock-free like this flag read.
Outcome: plan drafted, awaiting critique

Critiquer: 2026-06-22T10:21:34Z
Broad adversarial pass over the canonical plan, with every code anchor verified against the LIVE files.

Anchor verification (all CONFIRMED accurate against the repo):
- `api.ts`: type import at line 81 (`MithrilPartialSyncStatusSnapshot` only); 5 partial-sync channels at
  454-478; `MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL` ends at 478. Type pair shape (`...RendererRequest
  = void` / `...MainResponse = <readmodel>`) matches the 5 siblings exactly.
- `mithril-partial-sync.types.ts`: `MithrilPartialSyncStatusSnapshot` ends at line 54; const at 56;
  `MithrilPartialSyncAvailability` not yet defined. Plan's insertion point is correct.
- main `mithrilPartialSyncChannel.ts`: value import 3-9, type import 10-21, channel decls 25-48, status
  `onRequest` at 93-95, idempotency guard `mithrilPartialSyncRequestsInitialized` at 90-91. Main generic
  order is `MainIpcChannel<RendererRequest, MainResponse>`; renderer is the INVERSE
  `RendererIpcChannel<MainResponse, RendererRequest>` (status channel 27-30) — plan got the inversion right.
- `chainStorageCoordinator.ts`: `launcherConfig` imported at line 9; kill-switch guard
  `_assertPartialSyncFeatureEnabled` at 375-379 with `!== true` polarity (plan's `=== true` is the exact
  inverse — correct); `isPartialSyncInProgress()` lock-free at 64; type imports 3-7; module does NOT yet
  import partial-sync types (plan's "add new import" is right).
- `MithrilController.ts`: imports `MithrilPartialSyncStatusSnapshot` at line 8; `getPartialSyncStatus()` at
  160-162; delegation pattern `chainStorageCoordinator.<method>(...)` at 383-414; `chainStorageCoordinator`
  imported as a named value at 12 (passthrough resolves).
- `nix/internal/launcher-config.nix:409`: `mithrilPartialSyncEnabled = false;` exactly at line 409.
- `api-endpoints.md`: "Mithril Channels" table at 380; count cell "10 channels" at line 360 (5 bootstrap +
  5 partial-sync). +1 → 11 is correct, and the plan instructs verify-live rather than baking a number — SAFE.

SPEC `mockChannels` INDEX REASONING — VERDICT: CORRECT, no fix needed.
Module-eval order of `new MainIpcChannel(...)` in the main file is start(28)→status(33)→cancel(38)→
restart-normal(43)→wipe(48), giving mockChannels[0..4]. The live spec confirms this exact mapping:
[1]=status (lines 98,111→idleStatus), [0]=start (114), [2]=cancel, [3]=restart-normal, [4]=wipe (117-123).
Declaring the availability channel AFTER the wipe decl (Step 5c) makes it the 6th-created →
`mockChannels[5]`, leaving 0-4 untouched. Plan's index-5 reasoning, the "separate instance" assertion
(`[1]!==[5]`), and the per-channel `onRequest.mock.calls[0][0]` retrieval are all sound and deterministic.
The mock pushes a fresh channel object per construction, so handler retrieval is by channel object, not by
registration sequence — the new tests will pass as written.

Evaluation:
- PRD/gap coverage: implements D3 (expose kill switch via availability read model, closes blocker #1
  contract half) and lays the D2 contract surface; correctly DEFERS behind-ness math to task-ux-102 with
  `isSignificantlyBehind:false` / `behindByImmutables:undefined` stubbed. No under/over-reach.
- Locked invariants: availability channel SEPARATE from status snapshot (honored); renderer never thresholds
  (honored — backend owns it); backend owns behind-ness (honored); nix flip branch-scoped per D3 (honored);
  no auto-trigger (honored). No violations.
- Contract correctness: channel/type-pair shape matches siblings; main vs renderer generic inversion correct;
  two-hop routing (controller passthrough → coordinator) is consistent with all existing handlers and does
  not break the spec's "thin request handlers that delegate to the controller" assertion.
- Docs: api-endpoints row + count bump concrete; no stale number baked in.
- Small-model-implementability: files, line anchors, exact code shapes, ordered steps, inline invariants,
  and exact verify commands (`yarn compile`/`yarn lint`/`yarn test:jest <spec>`) are all present and accurate.

[MINOR] Step 5d places the availability `onRequest` registration "after the status handler registration
(:93-95)" (i.e. 2nd registered), while the Verification section twice calls its handler "the last
registered" (lines 222, 292). Immaterial to correctness (retrieval is by channel object, not order), but the
two statements are inconsistent — either register it last after the wipe handler at :105-107, or drop the
"last registered" wording. Recommend registering after wipe so registration order mirrors declaration order.
[MINOR] Step 4 controller passthrough is declared synchronous (`getPartialSyncAvailability(): ...{ return ...}`)
whereas the 4 sibling delegations are `async`. Compiles fine because the channel wraps it in
`async () => controller.getPartialSyncAvailability()`; the plan's own risk note already covers the 102 async
path. No change required.
[MINOR] Touching `MithrilController.ts` is outside the JSON targetPaths; the plan acknowledges this and the
Planner logged it. Acceptable — it is the cleanest consistent seam and the task prompt allows it.

Decision: approved

Planner: 2026-06-22T10:22:50Z
Fix pass (MINOR #1): Step 5d now registers the availability `onRequest` LAST (after the wipe handler, :105-107) so registration order mirrors declaration order from Step 5c, resolving the "after status" vs "last registered" wording inconsistency the Critiquer flagged. MINOR #2 (sync passthrough) and #3 (MithrilController outside targetPaths) accepted as-is per the Critiquer. No other changes; plan critique was Decision: approved.
Outcome: plan approved, proceeding to implementation
