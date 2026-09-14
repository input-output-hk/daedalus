# Task task-004: Delete three dead symbols

## Task ID and Title

- ID: `task-004`
- Title: `Delete three dead symbols`

## Why Chosen Now

`task-004` has no dependencies in the task graph and is the last task in phase 1. It is a deletion
with no behaviour attached: three exported names with no reader anywhere in the repository.

It is scheduled here for two reasons the task graph gives. Removing the names now keeps them out of
the diffs that follow, and `getUniqueId` in particular keys a subject as asset name followed by
policy id, the opposite of the order every live site uses and the opposite of the order the cache's
primary key takes. A dead function that spells the new schema's key backwards is a trap for whoever
writes that schema.

## Interaction Mode

- Mode: `agent_execution`

The acceptance criteria are a repository-wide grep and two Nix checks. Nothing needs a running node,
a network fetch or an operator.

## Scope

- Delete `getUniqueId` from `source/renderer/app/utils/assets.ts`.
- Delete `source/renderer/app/api/assets/requests/getUnknownAsset.ts` entirely, and the
  `GetUnknownAssetRequest` type it is the only reader of.
- Delete `FALLBACK_TOKEN_METADATA_SERVER_URL` from `source/main/config.ts`, with the comment that
  describes it.

Revertible on its own, and independent of the other three phase-1 tasks: `task-001` touched the asset
pill, `task-002` touched two merge helpers in `utils/assets.ts` that do not call `getUniqueId`, and
`task-003` touched the send form's amount field.

## Non-Goals

- No other deletion. `getAssets.ts`, `ApiAsset`, `ApiAssets`, `GetAssetsRequest` and
  `GetAssetsResponse` all still have readers and belong to `task-018`.
- `MOCK_TOKEN_METADATA_SERVER_URL` and `MOCK_TOKEN_METADATA_SERVER_PORT` at `source/main/config.ts`
  stay. They belong to the bundled mock server, which is not in scope for this plan at all.
- No change to the live fallback at `source/main/index.ts:215-218`, which is what actually supplies
  `--token-metadata-server` when the launcher config does not.
- No change to `getAssetTokens` at `utils/assets.ts:106-113`. The PRD lists it for deletion alongside
  `getUniqueId` at `:1367`, but it has two live readers, `WalletTokensPage.tsx:42` and
  `WalletSummaryPage.tsx:123`, and `task-017` owns moving them. Deleting it here would not compile.

## Dependencies

- None in the task graph. `task-004` has `"dependencies": []`.
- Practical dependency: Nix, for the checks.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-004` entry, plus
  `task-017` and `task-018` to confirm which neighbouring deletions belong elsewhere.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the deletion list at `:304` and
  the components-affected section at `:1359-1392`.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md`, the rule that a divergence found in passing is recorded rather than worked around.
- Workflows: none. There is no behaviour to exercise.
- Skills: none apply. No message, no style, no store registration.

## Live Repo Findings Verified For Planning

Verified at `896910237` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

The command run for each name, from the repository root, excluding `node_modules` and `coverage`:

```
grep -rn "<name>" --include='*.ts' --include='*.tsx' --include='*.js' --include='*.json' \
  --include='*.md' --include='*.nix' --include='*.feature' .
```

| Name | Hits in source | Hits in plan documents |
|---|---|---|
| `getUniqueId` | `utils/assets.ts:316` only, its own declaration | 4 |
| `getUnknownAsset` | `api/assets/requests/getUnknownAsset.ts:7` only, its own declaration | 4 |
| `GetUnknownAssetRequest` | `api/assets/types.ts:81`, plus `getUnknownAsset.ts:2` and `:9` | 2 |
| `FALLBACK_TOKEN_METADATA_SERVER_URL` | `source/main/config.ts:167` only, its own declaration | 3 |

So every one of the three is dead outside its own module, and `GetUnknownAssetRequest`'s only reader
is the module being deleted with it. The hits in plan documents are prose describing this deletion.

Detail on each.

- **`getUniqueId`** at `source/renderer/app/utils/assets.ts:316-322` returns
  `` `${assetName}${policyId}` ``. Every live site keys the other way round: `api.ts:3216` and
  `:3227` build `` `${policyId}${assetName}` `` for the wallet balance, `api.ts:3384` does the same
  for an asset, `AssetsStore.ts:67` builds the lookup key the same way, and `task-002` added the same
  concatenation as the derivation for a transaction-originated row. The task graph's line citation
  for it, `:301-307`, is stale by fifteen lines because `task-002` landed in the same file; the
  declaration itself is unchanged.
- **`getUnknownAsset`** at `source/renderer/app/api/assets/requests/getUnknownAsset.ts:7-17` issues
  `GET /v2/{wallets|byron-wallets}/{id}/assets/{policyId}`. The sibling `getAssets.ts` is the only
  other module in that directory and it is a live reader, so the directory survives.
- **`FALLBACK_TOKEN_METADATA_SERVER_URL`** at `source/main/config.ts:167-168` is
  `'https://metadata.world.dev.cardano.org'`. The live fallback is the literal at
  `source/main/index.ts:217`, `'https://tokens.cardano.org'`, reached as
  `metadataUrl ?? 'https://tokens.cardano.org'`. The two do not name the same host and the dead one
  names a development network's metadata server, so it is not merely unused, it would be wrong if it
  were used. `source/main/index.ts:21-26` imports four names from `./config` and this is not one of
  them.

**Type-level consequence of the third deletion.** `grep -c "^export type" api/assets/types.ts`
returns 14. `GetUnknownAssetRequest` at `:81-84` is one of them; the thirteen that remain all have
live readers, and four of them are `task-018`'s to remove later.

**Nothing else reaches the deleted module.** `grep -rn "requests/getUnknownAsset" source` returns
nothing, so no barrel file or index re-exports it.

## Files Expected To Change

- `source/renderer/app/utils/assets.ts` — `getUniqueId` removed.
- `source/renderer/app/api/assets/requests/getUnknownAsset.ts` — file removed.
- `source/renderer/app/api/assets/types.ts` — `GetUnknownAssetRequest` removed.
- `source/main/config.ts` — `FALLBACK_TOKEN_METADATA_SERVER_URL` and its comment removed.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-004` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-004*.md` — this plan and its two review logs.

No deviation from the task graph's `targetPaths`. All four are listed.

## Implementation Approach

1. Remove the four declarations named above, each with the comment that belongs to it and nothing
   else.
2. Re-run the four greps and confirm each name is gone from source.
3. Run the checks. `compile` is the one that matters: it is the tool that proves nothing imported
   what was removed.

No spec is added. A deletion of unreferenced code has no behaviour to assert, and a test that
asserts a symbol does not exist is a test that fails the next time someone legitimately reintroduces
the name. The grep and `compile` are the evidence.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn lint` and `yarn compile` pass.** Run as `nix build '.#checks.x86_64-linux.lint'` and
   `nix build '.#checks.x86_64-linux.compile'`.
2. **A repository-wide grep for each of the three names returns nothing outside history.** Run after
   the deletion, with the plan documents that describe the deletion named as the expected remaining
   hits rather than treated as a failure.

Three criteria this task adds to its own closure:

3. `jest` and `i18n` are green from `nix build` too. A deletion that breaks a suite is exactly the
   kind of thing a narrow acceptance list misses.
4. The deletion is confined to the four declarations. Nothing adjacent is tidied at the same time,
   because a deletion commit whose diff contains unrelated edits cannot be reviewed as a deletion.
5. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

Repository verification already done for planning is under Live Repo Findings. Execution
verification:

- The same four greps run for planning, repeated over the whole repository rather than over `source`
  alone, so the result is comparable with the table above. The only remaining hits must be the plan
  documents that describe this deletion: this plan and its review logs, the task graph entry and the
  PRD. Any hit outside those is a failure.
- `nix build '.#checks.x86_64-linux.compile' --no-link` — the check that actually proves no importer
  survives.
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.jest' --no-link` — expected to be unchanged from the previous
  task's result, 75 suites and 989 tests, since nothing under test is touched. A change in that count
  would mean something was reached that the grep said was not.
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `stylelint` and `storybook` are not run. No SCSS and no story is touched, and neither check reads
  any of the four files. Said rather than silently skipped.
- `git diff --stat` read to confirm four source files and no others, plus the plan documents.

If a check reports a substituted result rather than a local build, it is re-run with `--rebuild`.

## Risks and Open Questions

1. **A deletion is only as good as the search that preceded it.** The search was four greps over
   every text extension in the repository, not just TypeScript, so a reference from a Cucumber
   feature, a Nix expression or a JSON fixture would have been caught. Dynamic access by string
   would not, but none of these three names is reachable that way: two are module exports consumed
   by `import`, and the third is a type that exists only at compile time.
2. **`getUnknownAsset` is an endpoint call, and deleting it removes the only description of that
   endpoint from the codebase.** `task-018`'s notes already record that the same endpoint path
   appears in the two request modules and that `getUnknownAsset` goes here, so the knowledge is not
   lost from the plan. The endpoint itself is unaffected; nothing was calling it.
3. **The PRD lists `getAssetTokens` for deletion in the same sentence as `getUniqueId`.** It is not
   deleted here, because it has two live readers and `task-017` owns moving them. Named so the
   difference between this task and that PRD line is deliberate rather than an oversight.
4. No open questions for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-004`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-004-plan-review.md` and `task-004-impl-review.md` as the cycle requires.
- No PRD change. Its deletion list at `:304` is satisfied by this task.
- One stale line citation recorded rather than edited: the task graph places `getUniqueId` at
  `utils/assets.ts:301-307`; it is at `:316-322` after `task-002`. The declaration is unchanged.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-004-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-004-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-004` complete. Reviewed and approved in `task-004-impl-review.md`.
- `getUniqueId`, `getUnknownAsset` with its request type, and `FALLBACK_TOKEN_METADATA_SERVER_URL`
  are gone. A repository-wide grep for all four names now returns only the plan documents that
  describe the deletion.
- Checks, all from `nix build` and all built locally: `compile` exit 0, `lint` exit 0, `i18n` exit 0,
  `jest` 75 suites and 989 tests with 986 passed and 3 skipped, unchanged from before the deletion.
- Phase 1 of this plan is complete with this task.

## Self-Review

- The plan deletes exactly the four declarations the task graph names and nothing else, and says why
  the two neighbouring deletions the PRD mentions in the same breath are not in it.
- Every claim under Live Repo Findings carries a `path:line` or the command that produced it, taken
  at `896910237`.
- The decision not to add a spec is stated with its reason rather than left as an omission.
