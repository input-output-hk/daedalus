## Task ID and Title

`task-035` — Ingest CIP-25 and CIP-68 as chain-sourced rows, and render their
names.

## Why Chosen Now

`task-033` asks an index and `task-034` decides whether to believe it, and
nothing joins the two. Until this lands the chain channel has no caller and no
surface, and the setting `task-032` renders changes nothing a user can observe.

## Interaction Mode

`agent_execution`.

## Scope

The resolver's second pass, the row it writes, the selected source crossing the
process boundary, and one more rung in the name resolution order.

## Non-Goals

- **No decimal places, ever, from this channel.** Every chain row carries
  `decimals` NULL. That is locked decision 10 in its mechanical form.
- **No image fetch.** A CIP-25 image is a URI, usually `ipfs://`, not inline
  bytes. The URI is stored in the metadata column and nothing fetches it; an
  IPFS gateway is a further external dependency and is not in scope.
- No schema change. `asset_metadata` already has `source`, `slot` and the CHECK
  constraints that make a chain row's shape unwritable any other way.
- No freshness rule. `task-036` tiers the refresh window per channel; until then
  a chain row takes the same seven-day window a registry row does.

## Dependencies

`task-006`, `task-016`, `task-034`.

## Research Consulted

- `asset-metadata-cache-prd.md:975-990`, what is written and why every fixed
  column is fixed.
- `asset-metadata-cache-prd.md:968-975`, CIP-68 on weaker footing than CIP-25.
- `asset-metadata-cache-prd.md:229`, locked decision 10.

## Docs, Workflows, and Skills Consulted

- Not `.agent/workflows/ipc.md` for the channel shape. `task-028` corrected it,
  and the request type is read from `source/common/ipc/api.ts` regardless.

## Live Repo Findings Verified For Planning

1. **The schema already refuses a wrong chain row.**
   `assetMetadataDb.ts:50-55`: `CHECK (source IN ('registry', 'chain'))`,
   `CHECK (source <> 'registry' OR slot IS NULL)` and
   `CHECK (source <> 'chain' OR sequence_number IS NULL)`. Both
   source-conditioned rejections already have cases, from `task-026`.
2. **The resolver's precedence rule is already half-written.**
   `assetMetadataResolver.ts:_supersedes` returns true when the stored row is
   not a registry row, so a registry answer replaces a chain row. The other
   direction has no code because nothing wrote a chain row: a chain row must not
   replace a registry one, and the way to do that is not to build one for a
   subject the registry answered.
3. **The name resolution order already anticipates this rung.**
   `utils/assetName.ts:31-33` says a CIP-25 or CIP-68 name sits between the
   registry name and the decoded name, and that nothing produces one yet.
4. **A chain name and a registry name arrive the same way.** The cache has one
   `name` column and `AssetsStore.metadataOf` puts it on `metadata.name`, so the
   renderer cannot tell them apart without the row's `source`.
5. **`source` is already on `AssetMetadataEntry`** (`asset-metadata.types.ts`)
   and is dropped at `AssetsStore._assetFor`, which is the same four-edit gap
   `hasImage` had in `task-024`: the api type, the domain class, the merge
   helper and the store.
6. **`domains/Asset.ts:39-46` is a `pick` list**, so a field the store sets and
   the list omits is silently dropped on `update`. That is the trap the PRD's
   open question 5 records.
7. **The channel is registered with no chain path.**
   `source/main/ipc/index.ts:51` is `handleAssetMetadataRequests(window)`, and
   the chain path is resolved at `source/main/index.ts:360-369` into a local of
   an async function that runs later. So the handler resolves it itself, from
   `stateDirectoryPath` and the `CUSTOM-CHAIN-PATH` key, the same way.
8. **A custom chain path only takes effect on a backend restart.**
   `BackendLifecycle.setCustomChainPath` rebuilds the watchdog config and
   restarts the node, so a value read when the handlers are built is the value
   the node is running against.
9. **The recorded fixture's CIP-25 name is an array.**
   `{"name": ["Northwind Demo"]}`. CIP-25 splits a string over 64 bytes into an
   array, and minters also write single values that way, so both spellings have
   to be read.

## Files Expected To Change

- `source/main/assets/assetMetadataResolver.ts`
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`
- `source/main/assets/immutableBlockReader.ts`
- `source/main/ipc/assetMetadataChannel.ts`
- `source/common/ipc/api.ts`
- `source/renderer/app/ipc/assetMetadataChannel.ts`
- `source/renderer/app/api/assets/types.ts`
- `source/renderer/app/domains/Asset.ts`
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/utils/assetName.ts`
- `source/renderer/app/utils/assetName.spec.ts`
- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/stores/AssetsStore.spec.ts`
- `source/renderer/app/components/assets/Asset.tsx`
- `source/renderer/app/components/assets/Asset.spec.tsx`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**The chain pass runs after the registry pass, over what the registry did not
answer.** That is the filter the PRD's privacy argument depends on: fungible
holdings are what the registry answers for, so they never reach the index. It is
computed from the entries just written plus the rows already stored, not from a
separate query.

**Precedence is enforced by not asking.** A subject with a registry row is
excluded before the request, so no chain row is ever built for it, and the
question of which write wins never arises.

**The row's four fixed columns are each a rule.** `source` is `chain`, `slot` is
the mint block's, `sequence_number` is NULL and `decimals` is NULL with
`verified` false. Three of the four are also enforced by the schema; `decimals`
is not, and it is the one that matters most, so it is asserted directly.

**The name is the CIP-25 name, or the CIP-68 name where the index has one.**
Where both exist the CIP-68 datum is the live record. The whole payload goes
into the metadata column as JSON, and only the name is read out of it.

**The selected source travels with each read.** The setting is the renderer's
and the client is in the main process. A field on the existing request is one
edit and cannot fall out of step; a channel of its own would be a second thing
to keep synchronised and a startup order to get right.

**The renderer learns which channel a name came from.** `source` reaches the
domain object through the same four edits `hasImage` took, and
`resolveAssetName` gains a rung that reads it. A chain name is **not** marked
minter-chosen: it is in the transaction that minted the asset, which had to
satisfy the minting policy, so it is bound to that policy. A decoded asset name
is bound to nothing, which is what the marker exists for.

## Acceptance Criteria

1. A written chain row has `source` `chain`, a slot, and NULL `sequence_number`,
   NULL `decimals` and `verified` false.
2. A subject with a registry row is not overwritten by a chain row, and the
   index is not asked about it at all.
3. A pointer inside the volatile window writes no row and records `pending` with
   a retry inside the window.
4. An NFT with a CIP-25 name renders it, and renders its fingerprint before the
   row exists.
5. A chain row never changes the decimals used to format an amount, and the
   user's own setting still applies over one.
6. A chain name is not marked minter-chosen, and the same string from the
   registry is not marked as a chain name.
7. The decimals resolution order specs from `task-019` pass unchanged.
8. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
9. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The resolver cases run the whole channel: a stubbed pointer transport, a
  temporary immutable database written from the recorded preprod block, and the
  real confirmation in between. What they assert is the row, not the calls.
- Criterion 2 is asserted twice and the second half is the one that matters: the
  stored row is still the registry's, **and** the pointer transport was never
  called. A test that only checked the row would pass against an implementation
  that asked and then discarded the answer.
- Criterion 3 asserts the retry is the constant **and** that it is less than
  twelve hours, so a later change to the constant that put it outside the window
  fails rather than passing on equality with itself.
- Criterion 5 is driven through `getAssetTokenFromToken` rather than on the
  store, because a field the store sets and the merge helper drops passes every
  store-level assertion and reaches no component. That is finding 6's trap.
- Criterion 6 is driven both ways: the same name with `source` `chain` and with
  `source` `registry`, asserting different provenance.
- Criterion 7 needs no new case; it is the existing suite passing.
- All six Nix checks.

## Risks and Open Questions

- **CIP-68 is stored unconfirmed.** The mint transaction is confirmed; the datum
  the index reports is not, because it lives at a spendable output. The row does
  not distinguish the two, and a reader of the database cannot tell which kind of
  name a chain row carries. Acceptable because the value is a name, which is the
  same class of risk an unverified registry name already carries, but it is a
  real gap and it is not visible in the schema.
- **The chain path is read once.** A user who moves their chain directory while
  the application is running has a resolver pointed at the old one until the
  next start. The node restarts on that change, so the window is short, but it
  exists.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-035.targetPaths` widened to the IPC
  types, the channel, the name resolver, the domain and the merge helper;
  `task-035.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-035-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-035-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

An NFT the registry has never heard of shows the name its minter published,
after the user's own node has confirmed that they published it.

## Final Outcome

Complete.

## Self-Review

The row shape is the whole task and the temptation is to treat it as plumbing.
Three of its four fixed columns are enforced by the schema and one is not:
nothing in the database stops a later change writing a Koios decimal place into
`decimals`, and every amount on every surface would then be formatted by a
number no signature stands behind. That is why the assertion is on the row and
not on the code that builds it.
