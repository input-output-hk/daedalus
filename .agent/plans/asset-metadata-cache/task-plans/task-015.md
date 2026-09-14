## Task ID and Title

`task-015` — Renderer-side channel clients.

## Why Chosen Now

`task-013` declared the channels and `task-014` registered the responders.
`task-016` cannot read the cache until the renderer has something to read it
with, and this is the last piece before the store.

## Interaction Mode

`agent_execution`.

## Scope

- `source/renderer/app/ipc/assetMetadataChannel.ts`, new: the three
  `RendererIpcChannel` instances and the correlation that turns them into a
  client the store can call twice before the first call returns.
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts`, new.

## Non-Goals

- No store, no component, no observable. `task-016` owns everything that holds
  the answers.
- No correction of `IpcChannel`, and no move to `IpcConversation`. Both are
  larger changes than this task, and the second would change the wire shape
  `task-013` and `task-014` already agreed on.
- No timeout on a request. Nothing renders behind one of these calls, so a
  request that never answers costs a closure rather than a screen.

## Dependencies

`task-013`. `task-014` is not a declared dependency and landed first anyway,
which is why the spec can state what the responder does rather than assume it.

## Research Consulted

- `asset-metadata-cache-prd.md:626-641` for why the channels carry a `requestId`
  and what "discards a response it did not issue" has to mean.
- `.agent/findings/ipc-channel-response-correlation.md` for the defect in the
  primitive, the `IpcConversation` precedent, and the three questions a general
  fix would have to answer.

## Docs, Workflows, and Skills Consulted

- `source/renderer/app/ipc/governanceChannel.ts:8-10` for the declaration shape
  and the reversed type parameter order relative to the main side.
- `source/renderer/app/ipc/nodePushChannel.ts:16-19` for the push direction.
- `source/common/ipc/lib/IpcConversation.ts:64-95` for correlation as this
  repository already does it: an id minted per request, compared on arrival, and
  a non-matching message returned from rather than resolved on.

## Live Repo Findings Verified For Planning

1. `IpcChannel.request` (`source/common/ipc/lib/IpcChannel.ts:126-145`) registers
   `receiver.once` on one response name shared by every caller of the channel.
   Two in-flight requests are two one-shot listeners on one stream, fired in
   registration order by arrival order, so out-of-order responses are delivered
   to the wrong promises rather than dropped.
2. This is why correlation cannot live inside a single call. The first response
   to arrive removes the first listener whatever it answers, so a client that
   merely checked its own promise's payload and kept waiting would wait forever:
   the message it wanted has already been consumed by the other listener. The
   waiters therefore share one registry keyed by `requestId`, and whichever
   promise settles routes the payload to the waiter that issued it.
3. `RendererIpcChannel` defaults both sender and receiver to `global.ipcRenderer`
   (`source/renderer/app/ipc/lib/RendererIpcChannel.ts:16-24`), which a spec can
   supply, so the client is testable without Electron.
4. `uuid` is already a dependency, pinned at 8.3.2 in `package.json:291`, used by
   `source/common/ipc/lib/IpcConversation.ts:2` and
   `source/renderer/app/api/utils/localStorage.ts:3`. No dependency is added.
5. `source/renderer/app/ipc/` holds 36 files, 33 named `<thing>Channel.ts`. The
   PRD's Components Affected names
   `source/renderer/app/ipc/assetMetadataChannel.ts` and `task-024` already
   expects that path.
6. The main handlers never reject (`source/main/ipc/assetMetadataChannel.ts`,
   both handlers return an answer on every path), which is what makes the
   registry sound: one response per request, always attributable.

## Files Expected To Change

- `source/renderer/app/ipc/assetMetadataChannel.ts` (new)
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts` (new)
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

Three channel declarations, and above them one shared registry per request
channel mapping `requestId` to the waiter that issued it. A request mints an id,
records its waiter, and sends. Whichever promise settles hands its payload to
the registry, which delivers it to the waiter whose id it carries and discards it
when there is no such waiter. A waiter that has not been delivered to keeps
waiting.

The alternative shapes were considered and rejected. Checking the id inside a
single call and continuing to wait cannot work, for the reason in finding 2.
Moving these channels to `IpcConversation` would correlate without a payload
field, but it changes the wire shape two landed commits already agreed on and
leaves two primitives in the tree with an undocumented rule about which to pick.

A rejection is the one case the registry cannot attribute: the payload of a
rejected response never reaches this code with an id attached. It is logged and
nothing is resolved. This is reachable only if a main handler throws, and neither
does.

## Acceptance Criteria

1. A client discards a response whose `requestId` it did not issue and keeps
   waiting, driven by two overlapping requests whose responses arrive in the
   wrong order.
2. The same case run against the raw channel, without the registry, delivers each
   payload to the wrong promise, so the correlation is shown to be load-bearing
   rather than decorative.
3. No client blocks a render waiting on the network.
4. The push channel delivers a message to a subscribed handler.
5. `yarn compile`, `yarn lint` and `yarn test:jest` pass.
6. No new dependency, no new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

A colocated `assetMetadataChannel.spec.ts` with a fake `global.ipcRenderer` that
records what was sent and lets the test answer in whatever order it chooses,
firing one-shot listeners in registration order exactly as Electron does.

- Two metadata requests, answered in reverse order: each promise resolves with
  the payload carrying its own `requestId` and its own subjects.
- The control, in the same spec: the same two responses driven straight through
  `assetMetadataChannel.request`, asserting each promise takes the other's
  payload. Without it the first case could pass against a client that happened to
  answer in order.
- A response carrying an id nobody issued is discarded, the waiter stays pending,
  and the matching response then resolves it.
- The same three cases for the image channel, which carries a union response.
- Each request sends exactly one message, on the request channel, carrying the id
  it minted and the subjects it was given.
- Two requests mint different ids.
- A push delivers to the subscribed handler.
- Nothing in the client reads a clock or a timer, so a request cannot expire; the
  case asserts a pending request is still pending after the event loop drains.

Then all four Nix checks with both files staged.

## Risks and Open Questions

- **The graph's `targetPaths` name three files:**
  `source/renderer/app/api/assets/index.ts`, `ipc/assetMetadata.ipc.ts` and
  `ipc/assetImage.ipc.ts`. One file is written instead, at the path the PRD's
  Components Affected and `task-024` both name, and matching the 33-of-36
  naming convention in that directory. No `api/assets/index.ts` layer is added:
  the task's own note says this module is a declaration and not a layer, and the
  store consumes channels directly, as `BackendStore.ts:80-85` does. The graph
  entry is corrected in this commit rather than left to contradict the tree.
- **A rejected response cannot be attributed.** Named in Implementation
  Approach, unreachable through the handlers that exist, and the alternative
  (rejecting an arbitrary waiter) would be worse than waiting.
- The defect in `IpcChannel` remains open for every other channel. That is
  recorded in `.agent/findings/` and is not this task's to close.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-015.status` to `completed`, and its
  `targetPaths` corrected to the file actually written.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-015-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-015-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The renderer can ask the cache for a subject list twice before the first answer
arrives and get both answers back, each to the caller that asked for it.

## Final Outcome

Complete.

## Self-Review

The one thing a later reader is likely to try to simplify is the shared
registry, on the grounds that each request could surely check its own response.
The control case in the spec is what answers that, and the comment above the
registry says it in one sentence.
