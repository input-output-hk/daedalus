## Task ID and Title

`task-013` — Declare the channels and the shared entry type.

## Why Chosen Now

It is the first task of phase 3 and the only one in the phase with no
dependencies. Every other task in the phase imports from it: `task-014` builds
the main-side handlers from the constants, `task-015` builds the renderer
clients from the same constants, and `task-016` stores the entry type in an
observable map. Nothing can be written against a shape that does not exist yet.

## Interaction Mode

`agent_execution`.

## Scope

- `source/common/types/asset-metadata.types.ts`, new: `AssetResolutionState`,
  `AssetMetadataSource`, `AssetMetadataEntry`, `AssetUnresolvedSubject` and the
  correlation wrapper every request and response is built from.
- `source/common/ipc/api.ts`: three channel constants with their request and
  response types, placed after the governance block.
- `source/main/assets/assetMetadataDb.ts`: `AssetResolutionState` and
  `AssetMetadataSource` are imported from the new common module and re-exported
  under their existing names, so the two processes share one declaration rather
  than two copies of the same union.

## Non-Goals

- No handler, no client, no consumer. Nothing imports the constants in this
  commit; `task-014` and `task-015` are where they are instantiated.
- No correction of `IpcChannel` itself. The correlation defect is repository-wide
  and recorded at `.agent/findings/ipc-channel-response-correlation.md`; these three
  channels carry their own `requestId` and the rest are left as they are.
- No image bytes on the metadata entry. `hasImage` is a boolean and the bytes
  travel on their own channel.

## Dependencies

None in the task graph. Verified: `task-006` already declares the four-state
resolution union and the two-value source union in the main process, so this
task has a declaration to reconcile with rather than to invent.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md:554-582` for the
  three channel declarations and `:583-604` for the entry type.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md:606-641` for why
  the response carries `unresolved`, why the entry carries `source`, and why
  every message carries a `requestId`.

## Docs, Workflows, and Skills Consulted

- `.agent/workflows/ipc.md`, read and not followed. It labels the renderer hop
  `ipcRenderer.send` / `invoke` and the main hop `ipcMain.handle` / `on`. Neither
  `ipcRenderer.invoke` nor `ipcMain.handle` occurs anywhere under `source/`;
  `task-028` corrects the document.
- `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

1. `source/common/ipc/api.ts` is 543 lines and ends with the governance block at
   `:536-543`. The new block therefore appends rather than inserts.
2. `grep -n "ASSET" source/common/ipc/api.ts` returns one line, `:152`
   `LOAD_ASSET_CHANNEL = 'LoadAssetChannel'`. None of the three new names
   collides with it, which matters because `IpcChannel` throws on a duplicate
   name at construction (`source/common/ipc/lib/IpcChannel.ts:87-89`).
3. The file declares types both by top-level `import type` and by inline
   `import('../types/...')`. Both forms are in use, so a top-level
   `import type` of the new module matches existing practice.
4. `source/common/ipc/lib/IpcChannel.ts:91-93` derives three wire names from one
   base, and both `send` (`:101-120`) and `request` (`:126-145`) call
   `receiver.once(this._responseChannel, ...)`, resolving on the next message on
   that channel whatever request produced it. The doc comment at `:97-98` says so
   outright.
5. `source/common/ipc/lib/IpcConversation.ts:64-95` is the in-repo precedent for
   correlation: a `uuidv4` conversation id sent alongside the message, a
   persistent listener that returns early on `messageId !== conversationId`, and
   `removeListener` once the cycle closes. It correlates by an argument outside
   the payload; the three channels here carry the id inside the payload, because
   `IpcChannel` fixes the argument list and `IpcConversation` is a different
   class with a different wire shape.
6. `source/main/assets/assetMetadataDb.ts:134-140` already declares
   `AssetMetadataSource = 'registry' | 'chain'` and the four-member
   `AssetResolutionState`, and the schema at `:66-74` constrains
   `asset_resolution.state` to the same four values. Importers of those names
   today: `assetMetadataResolver.ts:6-10` and `assetImageStore.ts:2`.
7. `source/main/ipc/governanceAnchorChannel.ts:7` imports a type from
   `../../common/types/governance.types`, so a main-process module importing a
   shared type from `source/common/types` is established practice and the
   dependency runs the right way round.

## Files Expected To Change

- `source/common/types/asset-metadata.types.ts` (new)
- `source/common/ipc/api.ts`
- `source/main/assets/assetMetadataDb.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

**One declaration of the resolution state, not two.** The union already exists in
the main process and the database enforces it with a `CHECK`. A second copy in
`source/common` would be free to drift from the one the engine enforces, and the
drift would surface as a renderer rendering a state the database cannot hold.
The common module declares it, `assetMetadataDb.ts` imports and re-exports it
under the same name, and every existing importer is untouched.

**Correlation is carried by a type, not by a convention.** A shared wrapper

```ts
export type AssetIpcCorrelated<TBody> = TBody & { requestId: string };
```

builds every request and every response, including each member of the image
response union. A union member added later cannot omit the field, and a
responder that forgets to echo it fails to compile. Writing `requestId: string`
into five separate object literals would satisfy the letter of the acceptance
criterion and leave the sixth free to be written without it.

**`unresolved` is keyed by the resolution state.** The metadata response carries
`entries` and `unresolved` side by side. A subject with a row is in `entries`; a
subject without one is in `unresolved` with the state the `asset_resolution`
table holds for it, so never-asked (`pending`), known-absent (`unregistered`) and
failed are three answers rather than one absence.

**`source` is on the entry.** A chain row is confirmed against the user's own
immutable database and still carries `verified = 0`, because the registry
attestation chain never ran for it. Without `source` the strongest local proof in
the design is indistinguishable from the weakest.

**`hasImage`, not the bytes.** The metadata response is a bulk read for every
subject on screen. Carrying logo bytes in it would put megabytes on the path of
every render; the boolean says whether asking on the image channel is worth it.

## Acceptance Criteria

1. Every request type carries a `requestId` and every response echoes it,
   enforced by the type rather than by repetition.
2. `AssetResolutionState` is declared and the metadata response carries
   `unresolved` alongside `entries`.
3. `AssetMetadataEntry` carries `source`.
4. No channel name collides with an existing constant.
5. `yarn compile` passes.
6. `yarn lint` passes and `nix fmt` reports no change after formatting.
7. No new `@ts-ignore` and no new `@ts-expect-error`.
8. `package.json` and `yarn.lock` unchanged.

## Verification Plan

- `nix build '.#checks.x86_64-linux.compile' --no-link`. This is the whole test
  for a types-only change: the criteria above are compile-time properties and a
  runtime assertion cannot state them.
- The correlation criterion is verified by construction and by a negative
  compile: a scratch file omitting `requestId` from an image response member is
  compiled and observed to fail, then discarded. Asserting a type by watching the
  compiler reject a violation is the only form of that assertion that proves
  anything.
- `grep -c "ASSET_METADATA_CHANNEL\|ASSET_METADATA_UPDATE_CHANNEL\|ASSET_IMAGE_CHANNEL"`
  over `source/common/ipc/api.ts` returns 3, and a grep for each name over
  `source/` shows it declared once.
- `nix build '.#checks.x86_64-linux.lint'`, `.jest` and `.i18n`, the last two as
  regression guards: no spec and no message changes here, so a move in either is
  a fault of this change.

## Risks and Open Questions

- **Re-exporting from the database module is outside this task's declared
  `targetPaths`.** Recorded here before the work rather than discovered during
  it. The alternative is two copies of a union the engine enforces with a
  `CHECK`, which is the drift this plan's house style exists to prevent.
- **`Uint8Array` over IPC.** Electron's structured clone carries it, and
  `assetImageStore` already holds bytes in that form
  (`assetMetadataDb.ts:171-175`). No conversion is declared at the boundary.
- Nothing here is user-visible and nothing here can fail at runtime, so there is
  no open question for the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-013.status` to `completed`.
- No workflow document changes. `.agent/workflows/ipc.md` is wrong about this
  mechanism and `task-028` owns the correction.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-013-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-013-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The three channels and the shared entry type are declared, with correlation
carried by the type rather than by convention, and the resolution state has one
declaration shared by both processes.

## Final Outcome

Complete.

## Self-Review

The one judgement call is the re-export. It trades a file outside `targetPaths`
for a single source of truth on a union the database constrains. The check that
it was the right call is that `assetMetadataResolver.ts` and `assetImageStore.ts`
continue to import those names from the database module and need no edit.
