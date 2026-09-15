## Task ID and Title

`task-028` — Correct the IPC workflow document.

## Why Chosen Now

Three tasks in this plan built channels, and each one began by reading the code
because the document could not be trusted. It labels the renderer hop
`ipcRenderer.send / invoke` and the main hop `ipcMain.handle / on`. Neither
`ipcRenderer.invoke` nor `ipcMain.handle` exists anywhere in `source/`. Anyone
reading it to build a channel today builds the wrong thing.

## Interaction Mode

`agent_execution`.

## Scope

`.agent/workflows/ipc.md`, corrected against the code it describes. Every claim
the document makes about the mechanism, checked and either kept, fixed or
removed.

## Non-Goals

- **No code change.** If the document and the code disagree, the document is
  wrong. The one place the code is genuinely deficient, response correlation, is
  already written up as a finding and stays open; this document points at it
  rather than proposing a fix.
- No new document. The finding at
  `.agent/findings/ipc-channel-response-correlation.md` exists and is linked, not
  duplicated.
- Not the other divergences. `.agent/readme.md`, `AGENTS.md` and
  `CLAUDE.md` all state the wrong Electron version, and
  `.agent/workflows/frontend.md` and `.agent/system/state-management.md` have
  their own problems. Each is its own correction.

## Dependencies

`task-014`.

## Research Consulted

- `asset-metadata-cache-prd.md:1417-1424`, the documentation divergences found
  while building against this document.
- `.agent/findings/ipc-channel-response-correlation.md`.

## Docs, Workflows, and Skills Consulted

- The document under correction, read against `source/common/ipc/lib/IpcChannel.ts`
  rather than on its own terms.

## Live Repo Findings Verified For Planning

Each claim in the document, checked:

1. **`ipcRenderer.invoke` and `ipcMain.handle` appear nowhere.**
   `grep -rn "ipcRenderer\.invoke\|ipcMain\.handle" source/` returns nothing.
   The diagram names both, and the testing section mocks `invoke`.
2. **`ipcRenderer.send` appears twice, both outside the channel mechanism.**
   `stores/WindowStore.ts:17` sends `close-window` and `:25` sends
   `resize-window`, on bare names handled at `windows/main.ts:74` and `:79`. The
   channel mechanism reaches `send` through an injected sender rather than by
   naming it.
3. **The real mechanism is three derived names.**
   `source/common/ipc/lib/IpcChannel.ts:91-93` builds `-broadcast`, `-request`
   and `-response` from one constant. Nothing is ever sent on the bare constant.
4. **`send` and `request` differ only in which name they post on**, `:101-120`
   and `:126-145`, and each pairs with exactly one handler: `onReceive` listens
   on `-broadcast` (`:152-167`) and `onRequest` on `-request` (`:172-187`).
   Pairing them wrongly sends on a name with no listener, and neither side
   reports anything. The document says nothing about this.
5. **The preload is not a hop, and the diagram draws it as one.**
   `source/main/preload.ts:31` assigns `ipcRenderer` onto the renderer's
   `global`, which is what `RendererIpcChannel.ts:16-17` defaults to. Nothing is
   forwarded through it.
6. **The generic parameters are reversed between the two sides and the document
   gets it wrong.** `IpcChannel<Incoming, Outgoing>` is written from the point of
   view of the side declaring it, so `governanceAnchorChannel.ts:11-14` is
   `<RendererRequest, MainResponse>` and `governanceChannel.ts:8-10` is
   `<MainResponse, RendererRequest>`. The document's Step 2 and Step 4 use the
   same order on both sides.
7. **Registration is in `source/main/ipc/index.ts`, and neither the document nor
   the obvious guess names who calls it.** The default export at
   `source/main/ipc/index.ts:33` is where every handler is wired, and it is
   called from `source/main/windows/main.ts:72`, after the window exists.
   `source/main/index.ts` registers nothing.
8. **The broadcast section is wrong.** It shows
   `BrowserWindow.getAllWindows().forEach(...)` with `webContents.send(CHANNEL_NAME, data)`.
   `webContents.send(` appears nowhere in `source/`, the bare constant is never a
   wire name, and the real push is `channel.send(message, window.webContents)`,
   as at `source/main/ipc/assetMetadataChannel.ts:196-204`.
9. **Seven of the channels the document tabulates do not exist.**
   `CARDANO_STATE_CHANNEL`, `CARDANO_TLS_CONFIG_CHANNEL`,
   `CARDANO_RESTART_CHANNEL`, `CARDANO_AWAIT_UPDATE_CHANNEL`,
   `GET_CACHED_CARDANO_STATUS_CHANNEL`, `SET_CACHED_CARDANO_STATUS_CHANNEL` and
   `GET_BLOCK_SYNC_PROGRESS_CHANNEL` return zero matches across `source/`. The
   other twenty-four names in those tables all exist.
10. **The debugging snippet cannot work.** `ipcMain.on('*', ...)` is in the
    document; Electron has no wildcard channel, so it subscribes to a channel
    literally named `*`.
11. **`IpcConversation` is a different primitive and the document describes it as
    a variant of the same one.** It uses one channel name rather than three and
    correlates with a `conversationId`
    (`source/common/ipc/lib/IpcConversation.ts:44-96`). Its only users are
    `source/main/ipc/electronStoreConversation.ts` and its renderer counterpart.
12. **A channel name is a singleton and a second instance throws**
    (`IpcChannel.ts:87-89`), which is a startup failure the document does not
    mention and which a spec re-importing a module hits.
13. **`resize-window` has two handlers.** `source/main/windows/main.ts:74`
    registers one inline and `source/main/ipc/resize-window.ts:5` registers an
    identical one, wired in through `source/main/ipc/index.ts:37`. Both guard on
    the sender and both call `window.setSize`, so a resize is applied twice. A
    channel could not have produced this: `IpcChannel` throws on a second
    instance of the same name.
14. **`.agent/` is excluded from treefmt.** `perSystem/formatter.nix` excludes it,
    and no check lints or type-checks a Markdown file, so nothing verifies this
    document except a reader.

## Files Expected To Change

- `.agent/workflows/ipc.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

Rewrite the Overview, the architecture diagram, the Quick Reference, the five
steps, the channel types, the Cardano table, and the debugging and testing
sections. Keep the four-step shape of the recipe, which is right, and correct the
detail inside each step.

The diagram becomes two boxes rather than three, with the wire names on the
arrows, because the wire names are the thing the old diagram got wrong.

Every code sample is taken from a channel that exists, and every claim names the
file and line it came from, so the next person to doubt the document can check it
in one command rather than reading the primitive.

## Acceptance Criteria

1. The diagram names the mechanism the repository actually uses.
2. Every channel name the document tabulates exists in
   `source/common/ipc/api.ts`.
3. Every code sample compiles in the shape the repository uses, including the
   generic parameter order, which differs between the two sides.
4. No code change accompanies this task.
5. The document goes through an editing pass before it is committed.

## Verification Plan

- Criterion 1 by reading the corrected diagram against `IpcChannel.ts:91-93`,
  `:101-145` and `:152-187`.
- Criterion 2 by script: every `` `NAME_CHANNEL` `` in the document, matched
  against the constants declared in `source/common/ipc/api.ts`.
- Criterion 3 against the exemplar pair, `governanceAnchorChannel.ts` and
  `governanceChannel.ts`, which is also what the document now tells a reader to
  copy.
- Criterion 4 by `git diff --name-only`, which must name nothing outside
  `.agent/`.
- A grep for the machine-written tells this project's style forbids: em dashes,
  emoji, and arrows standing in for words. The ASCII diagram's arrows and the
  fat arrows in the TypeScript samples stay, because the character carries the
  meaning there.

## Risks and Open Questions

- **Nothing checks this document.** `.agent/` is outside treefmt and outside
  every CI check, so it will drift again. The only mitigation available here is
  that every claim now carries the path and line it was read from, which makes
  the next check cheap.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-028.status` to `completed`, and its
  implementation notes extended with what the correction actually found, which
  is more than the two mislabelled hops the task named.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-028-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-028-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A reader can build a channel from this document and get a working one.

## Final Outcome

Complete.

## Self-Review

The task named two wrong labels. Reading the document line by line against the
code found twelve more, including a table of seven channels that do not exist and
a generic parameter order that is reversed between the two sides. The lesson is
in the order of work: checking the two named claims and stopping would have left
a document that is right about its diagram and wrong about everything a reader
would copy from it.

Two of the twelve came from the editing pass rather than from the planning
sweep, and both were claims this plan had already written down as verified: who
calls the handler registration, and how many call sites bypass the channel
mechanism. Neither was checked by following the call chain, which is what found
them.
