## Task ID and Title

`task-024` — Render the asset logo in the token row header.

## Why Chosen Now

`task-011` fetches and stores logos, `task-012` bounds the table, `task-013`
declares the image channel and `task-014` answers on it. Nothing reads any of it.
Until a surface renders a logo, the image column is bytes on disk with no reader,
which is the state the task graph says to resolve by building this or by deleting
all four.

## Interaction Mode

`agent_execution`.

## Scope

One image, in the token row header, for a subject the cache has a logo for. The
request is made when such a row renders, one subject at a time, and at most once
per subject for the life of the renderer.

## Non-Goals

- No placeholder. A row with no logo keeps exactly the layout it has now.
- Nowhere else. Not the send form, not the transaction list, not the settings
  dialog. One surface is what the image column needs to earn its place.
- No change to what the main process stores or how it decides a logo is
  acceptable. The cap, the magic-byte detection and the refusal of SVG are
  `task-011`'s and stay as they are.
- No eager prefetch. A row that is never rendered never asks.

## Dependencies

`task-011`, `task-016`.

## Research Consulted

- `asset-metadata-cache-prd.md:1548-1556`, which records the question of whether
  the logo surface earns its column as closed by the tracked request that asks
  for images directly.
- `asset-metadata-cache-prd.md:551-652`, the IPC shapes: one subject per image
  request, and `hasImage` on the bulk read so a row can tell whether asking is
  worth it.

## Docs, Workflows, and Skills Consulted

- `.agent/system/architecture.md` for the renderer layering.
- Not `.agent/workflows/ipc.md`, which describes a mechanism this repository does
  not use. `task-028` corrects it.

## Live Repo Findings Verified For Planning

1. **The channel is built and answers three ways.**
   `source/main/ipc/assetMetadataChannel.ts:168-187` answers `present` with a
   media type and bytes, `absent` for a subject the registry has no logo for, and
   `absent` rather than a rejection when the fetch fails.
   `source/renderer/app/ipc/assetMetadataChannel.ts:102-115` is the correlated
   client for it.
2. **`hasImage` reaches the store and stops there.** It is on
   `AssetMetadataEntry` (`source/common/types/asset-metadata.types.ts:64-75`) and
   is set by `toEntry` at `source/main/ipc/assetMetadataChannel.ts:84`, but
   `AssetsStore._assetFor` (`stores/AssetsStore.ts:250-259`) does not put it on
   the domain object, so no component can see it. Carrying it is the same four
   edits `task-019` made for `recommendedDecimalsVerified`: the type, the domain
   class, the merge helper and the store.
3. **A component may import a channel client directly.** Precedent:
   `components/chain-storage/ChainStorageLocationPicker.tsx:12` and
   `components/widgets/forms/FileUploadWidget.tsx:5` both import
   `showOpenDialogChannel`. So the request does not have to be threaded through
   four levels of props.
4. **The row is rebuilt on every store change.**
   `WalletTokensPage.tsx` is an `observer` and calls
   `getNonZeroAssetTokens(walletTokens, getAsset)`, which produces a fresh plain
   object per token per render. So `hasImage` flipping false to true when a row
   resolves reaches the header as a changed prop rather than as a mutation.
5. **The subject is derived, not taken from `uniqueId`.** The store keys on
   `${policyId}${assetName}` (`stores/AssetsStore.ts:19-20`), and
   `getAssetTokenFromToken` falls back to the same concatenation only when the
   token carries no `uniqueId`. Deriving in the component removes the assumption
   that the two always agree.
6. **`Buffer` is available in the renderer** and is already used there:
   `api/utils/index.ts:10`, `utils/crypto.ts:104`. There is no
   `Content-Security-Policy` anywhere under `source/`, so a `data:` URL renders.
7. **The bulk read is the only thing that knows about images, and it never
   carries bytes.** `readImageSubjects` selects the primary key column alone
   (`main/ipc/assetMetadataChannel.ts:144-146`), so `hasImage` costs an index
   lookup and not a blob read.

## Files Expected To Change

- `source/renderer/app/api/assets/types.ts`
- `source/renderer/app/domains/Asset.ts`
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/stores/AssetsStore.spec.ts`
- `source/renderer/app/ipc/assetMetadataChannel.ts`
- `source/renderer/app/ipc/assetMetadataChannel.spec.ts`
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletTokenHeader.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletTokenHeader.scss`
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletTokenHeader.spec.tsx` (new)
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**`hasImage` onto the merged row**, four edits, the same shape `task-019` used.
An unresolved subject carries `false`, which is correct rather than unknown: the
cache has no row, so it certainly has no logo for it, and when the row arrives
the value arrives with it.

**One memo in the channel client.** `requestAssetImageUrl(subject)` returns the
same promise for the same subject for the life of the renderer, and resolves to a
`data:` URL or to null. The memo holds the promise rather than the resolved
value, so two rows mounting in the same frame share one request rather than
issuing two and deduplicating afterwards. The bytes become a URL there rather
than in the component, so the encoding has one reader and the component holds a
string.

**The header asks in an effect and renders an image when one comes back.** The
effect is keyed on the subject and on `hasImage`, so a row that resolves into
having a logo asks then rather than never. It is guarded against setting state
after unmount, because a list is scrolled and a request may outlive the row that
issued it.

The size cap is CSS, in the header's own stylesheet. A registry logo has no
guaranteed dimensions and the measured corpus runs from 806 bytes to 65 KiB with
no stated pixel size.

## Acceptance Criteria

1. A row whose subject has a logo renders it; a row without one renders exactly
   what it renders today, with no placeholder element.
2. Scrolling a list of many tokens issues at most one image request per subject,
   asserted on the wire rather than on a render count.
3. A subject the cache answers `absent` for renders no image and does not ask
   again.
4. `hasImage` survives the merge onto the row, asserted through
   `getAssetTokenFromToken` and not only on the store.
5. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The channel spec drives criterion 2 against the fake `ipcRenderer` it already
  has: two calls for one subject send one message and both callers get the same
  URL. The same case in its negative form, two calls for two subjects, sends two,
  so the memo cannot pass by never sending anything.
- Criterion 3 in the same spec: an `absent` response resolves to null and a
  second call for that subject sends nothing further. Remembering "no logo" is
  the case that matters for a wallet holding many tokens the registry has no
  picture for.
- The memo is module state and the suite cannot reset it, so each case uses its
  own subject. Stated rather than worked around with a test-only export.
- The header spec renders the row three ways: a subject with a logo, a subject
  with `hasImage` false, and a subject with `hasImage` true whose answer is
  `absent`. The first asserts an image with the media type in its source; the
  other two assert no image element at all, which is what "no placeholder" means
  in a form a test can check.
- A fourth header case drives the flip: rendered first with `hasImage` false and
  no request made, then re-rendered with it true and the request made. This is
  the case that fails if the effect is keyed on the subject alone.
- The store spec asserts `hasImage` on the row that reaches a component, driven
  through `getAssetTokenFromToken`, because a field the store sets and the merge
  helper drops passes every store-level assertion and reaches nothing.
- All six Nix checks, `stylelint` included because a stylesheet changes.

## Risks and Open Questions

- **The memo never expires.** A `data:` URL for a logo capped at 256 KiB, one per
  held subject, is the same order of memory as the row list it belongs to, and
  the alternative is re-encoding on every scroll. Named here rather than solved:
  nothing in this plan evicts it, and the process that holds it is the renderer,
  which is torn down with the window.
- **The header is the only reader.** If another surface wants a logo later it
  gets the same client and the same memo; nothing here is specific to this row.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-024.targetPaths` widened to the files
  that carry `hasImage` to the component and to the two stylesheets and specs;
  `task-024.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-024-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-024-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A token the issuer published a logo for shows it, once per subject per session.

## Final Outcome

Complete.

## Self-Review

The temptation is to ask for every row's logo on the bulk read and be done. That
puts megabytes on the path of every render for a picture, which is the reason the
image channel is separate in the first place. The other temptation is to let each
row hold its own request, which is correct until the user scrolls and the row
remounts. Both are avoided by one memo in the one module that already owns the
channel.
