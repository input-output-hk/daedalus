## Task ID and Title

`task-029` — Manual refresh for one subject in the asset settings dialog.

## Why Chosen Now

`task-022` put the sentence about an unverifiable figure into the settings
dialog and created the container it sits in. The refresh control belongs in the
same container, and the task graph asks for an order that does not leave two
separate additions to one layout.

## Interaction Mode

`agent_execution`.

## Scope

One control, one subject. It makes the cache ask the registry about the token the
dialog is open on, ignoring the refresh window and any backoff.

## Non-Goals

- No refresh-all, and no enumeration of the cache to build one.
- No fourth IPC channel, and no second fetch path.
- No spinner, no error surface. Offline behaves as everywhere else in this
  design: nothing happens and nothing is said.

## Dependencies

`task-016`, `task-022`.

## Research Consulted

- The task's own note: the seven-day window comes from a measured registry change
  rate of about one change every 9.6 days, which is the right default and is
  wrong for a user who knows an issuer published something today.
- `task-010.md:105-110` for the measurement behind the window.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`.
- `.agent/workflows/ipc.md` is wrong about the mechanism, per the readme's first
  caution; the channels were read from `source/common/ipc` directly.

## Live Repo Findings Verified For Planning

1. **The window and the backoff are both consulted in one place.**
   `assetMetadataResolver.ts:317-341`, `_due`, filters out a subject whose
   `asset_resolution.retryAfter` is in the future and one whose row was updated
   within `ASSET_METADATA_REFRESH_MS`. Nothing else consults either.
2. **`queryAssetRegistry` does not filter.** `assetRegistryClient.ts:437-497`
   takes the subjects it is given, so bypassing `_due` is the whole of "ignore
   the window".
3. **A refresh that finds nothing new already stamps `updated_at`.**
   `assetMetadataResolver.ts:275-281` writes `storedAsWrite(previous)` on every
   successful read whether or not anything changed, and does not emit it.
4. **A refresh that finds a higher sequence number already re-runs
   verification.** `registryEntryToRow` at `:107-122` computes `verified` from
   the bytes every time it builds a row; there is no path that copies a stored
   verdict onto new content.
5. **The request channel already carries a subject list and a correlation id.**
   `common/ipc/api.ts:566-575`. A refresh is a read that ignores the window, so
   it is a field on that request rather than a channel of its own.
6. **The dialog's asset is a snapshot taken when it opened.**
   `AssetsStore.ts:276-279` stores the `AssetToken` the row handed it, and
   `AssetSettingsDialogContainer.tsx:32-40` renders that object. Nothing
   recomputes it, so a row arriving on the update channel would not reach an open
   dialog.
7. **`getAssetTokenFromToken` is the merge that would.** `utils/assets.ts:55-71`
   takes a token and a lookup and overlays what the cache knows, which is exactly
   the shape `editedAsset` needs to become.

## Files Expected To Change

- `source/common/ipc/api.ts` — one optional field on the metadata request.
- `source/main/assets/assetMetadataResolver.ts` — `request` takes a force option.
- `source/main/ipc/assetMetadataChannel.ts` — pass it through.
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`
- `source/renderer/app/ipc/assetMetadataChannel.ts` — send it.
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts`
- `source/renderer/app/actions/assets-actions.ts` — one action.
- `source/renderer/app/stores/AssetsStore.ts` — the handler, and `editedAsset`
  becomes a computed over the cache.
- `source/renderer/app/stores/AssetsStore.spec.ts`
- `source/renderer/app/containers/assets/AssetSettingsDialogContainer.tsx`
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx` and `.scss`
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`
- the four translation artifacts, regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

1. **Bypass rather than clear.** The task's wording is that the refresh clears the
   subject's staleness and its backoff row. Bypassing `_due` for that one subject
   has the same effect on what is fetched and writes nothing before the read. The
   difference matters when the fetch then fails: clearing `updated_at` first
   would leave a row that looks never-updated, so every subsequent render would
   re-schedule it. Recorded as a deviation with its reason.

2. **One field on the existing request.** `refresh?: boolean`, read with `=== true`.

3. **One subject.** The store sends the dialog's subject alone. There is no call
   site that passes more than one with the flag set, and nothing enumerates the
   cache.

4. **`editedAsset` becomes a computed** over the token the dialog was opened with
   and the current cache, through `getAssetTokenFromToken`. Without it a refresh
   would land in the cache and never reach the screen it was asked from.

5. **The control lives in the footer `task-022` created.** A flat button, no
   spinner, no error.

## Acceptance Criteria

1. Refreshing a subject inside its retry-after window issues a fetch rather than
   being suppressed.
2. A refresh that finds no sequence-number change leaves the row values unchanged
   and stamps `updated_at`.
3. A refresh that finds a higher sequence number rewrites the row and re-runs
   verification.
4. With the transport unavailable, the dialog keeps rendering the cached row.
5. No code path refreshes more than one subject per user action.
6. `compile`, `lint`, `i18n`, `stylelint`, `jest` and `cucumber-unit` green from
   `nix build`.
7. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The resolver spec drives criterion 1 against a real database with a
  `retryAfter` in the future, asserting the transport is called; and the same
  subject without the flag, asserting it is not. One case without its complement
  would pass against a resolver that ignored the window for everything.
- Criterion 2 is asserted on the row's values and its `updated_at` separately, so
  a refresh that rewrote the row with identical content would still be
  distinguishable from one that only stamped it.
- Criterion 3 drives a higher sequence number whose attestation does not verify,
  asserting the stored `verified` moves to false. A case where new content
  verifies would pass even if the verdict were copied from the stored row.
- The channel spec asserts the flag reaches the resolver, and the renderer spec
  asserts it reaches the wire.
- The store spec asserts one subject per user action, and that the dialog's asset
  follows the cache.
- The dialog spec asserts the control is present, calls back with the asset it is
  open on, and that a transport that answers nothing leaves the row on screen.
- All six Nix checks.

## Risks and Open Questions

- **A manual refresh does not reset the failure count.** A subject that is
  unregistered stays on its growing automatic backoff, and each manual attempt
  adds to it. That is the conservative reading: the control lets a user ask now,
  not defeat the schedule.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-029.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-029-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-029-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The settings dialog can ask the cache to read the registry again for the one
token it is open on, and shows what comes back.

## Final Outcome

Complete.

## Self-Review

The way this grows into a refresh-all is someone passing the dialog's flag with a
longer list. The store's handler takes one subject rather than a list, so
widening it is a signature change rather than an argument change.
