Implementation: Iteration 1
Timestamp: 2026-09-15T12:55:00Z

Changes made:
- `source/common/ipc/api.ts`: `refresh?: boolean` on the metadata request.
- `source/main/assets/assetMetadataResolver.ts`: `request` takes `{ force }` and
  skips the due rule when it is set.
- `source/main/ipc/assetMetadataChannel.ts`: passes it through.
- `source/renderer/app/ipc/assetMetadataChannel.ts`: `requestAssetMetadata` takes
  `{ refresh }` and puts it on the wire.
- `source/renderer/app/actions/assets-actions.ts`: `onAssetSettingsRefresh`.
- `source/renderer/app/stores/AssetsStore.ts`: the handler, and `editedAsset`
  becomes a computed over the held token and the cache.
- `source/renderer/app/containers/assets/AssetSettingsDialogContainer.tsx`: the
  trigger.
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx` and `.scss`:
  the control, in the footer `task-022` created.
- five specs, and the four translation artifacts.

Files touched:
- the nine source files above
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`,
  `source/main/ipc/assetMetadataChannel.realfs.spec.ts`,
  `source/renderer/app/ipc/assetMetadataChannel.spec.ts`,
  `source/renderer/app/stores/AssetsStore.spec.ts`,
  `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`
- `.agent/plans/asset-metadata-cache/task-plans/task-029.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-029-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-029-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**`editedAsset` is held as the token the row handed over and merged on read.**
The store keeps `_editedAsset` and exposes `editedAsset` as a computed through
`getAssetTokenFromToken`. The quantity and address stay the token's, because the
cache holds neither; the metadata, decimals and verdict come from the cache as it
stands now. Without this the refresh control would land a new row in the cache
that the dialog it was pressed in could not show.

**The store's handler takes one asset, not a list.** Acceptance criterion 5 says
no code path refreshes more than one subject per user action, and a signature
that cannot express a list is cheaper to keep true than a comment saying so.

Verification run:

- `jest` over the five specs — 32 resolver, 31 across the two channels, 28 store
  and 15 dialog.
- **Criterion 1 with its complement in the same case.** A subject with a
  `retryAfter` sixty seconds in the future is requested twice: once ordinarily,
  asserting the transport was not called at all, and once forced, asserting it
  was. A resolver that ignored the window for everything passes the second half
  and fails the first.
- **Criterion 2 asserts the values and the stamp separately.** The row is written
  at `NOW - 5000` and the assertion checks `updatedAt` moved to `NOW` while
  ticker, decimals, verified and sequence number did not, so a refresh that
  rewrote identical content is still distinguishable from one that restamped.
- **Criterion 3 is driven with content that fails verification.** The seeded row
  is `verified: true`; the refresh returns the same entry at sequence number 1,
  which the stored signature does not cover, and the stored verdict is asserted
  to have moved to `false`. Driving it with content that verifies would pass even
  if the verdict were carried across from the stored row.
- **Criterion 4**: the resolver keeps the cached row with a failing transport,
  and the dialog keeps rendering its row and raises nothing after a click.
- The channel cases assert the flag rather than infer it: the main handler's
  resolver is replaced with a recorder and the two calls come back as
  `{ force: false }` and `{ force: true }`; the renderer's fake `ipcRenderer`
  records `refresh: false` and `refresh: true` on the wire.
- The store cases assert one subject and one call per user action, and that the
  dialog's asset follows the cache: `editedAsset.metadata` is null before a row
  arrives and carries the ticker after, with the token's own quantity intact.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `9m445zyh064xjqz9mk4bvmj36wsd5k2v-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `xlz5nzb8r1wqyh0xqaa2j6fqrl640gpb-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `9lxih1pikf8pp37z5g735157ggav5l01-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `0r1c3a134s4m7p5zhi14nl83vrblf1k8-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 86 suites passed, 1326
  tests with 1323 passed and 3 skipped, exit 0. The previous state of this branch
  was 86 suites and 1309 tests, so seventeen tests were added across five
  existing suites and nothing else moved.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None beyond the one the plan already recorded: the refresh bypasses the window
  and the backoff rather than clearing the columns they are read from.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T13:00:00Z

Acceptance criteria, each against the evidence:

1. *A refresh inside the retry-after window fetches.* Met, with the complement in
   the same case.

2. *No change leaves the values alone and restamps.* Met, asserted on the two
   separately.

3. *A higher sequence number rewrites and re-verifies.* Met, and driven so that a
   copied verdict fails.

4. *The dialog keeps rendering the cached row with the transport unavailable.*
   Met at both layers.

5. *No path refreshes more than one subject per user action.* Met by the
   signature and asserted on the call.

6-7. *All six checks, suppressions, dependencies.* Met.

The deviation to weigh is bypassing rather than clearing. The task's wording says
the refresh clears the subject's staleness and its backoff row. Doing that
literally writes to the database before the fetch, and the fetch is most likely
to fail for exactly the user who presses this control while offline; the row
would then look never-updated and every render afterwards would re-schedule it.
Bypassing fetches the same thing and leaves the cache untouched when the fetch
finds nothing. One consequence is worth naming: a manual refresh does not reset
the failure count, so an unregistered subject stays on its growing automatic
backoff. That is the conservative reading of a control that lets a user ask now
rather than defeat the schedule.

The other change worth naming is `editedAsset`. It was a snapshot taken when the
dialog opened, and a refresh control whose result cannot reach the screen it was
pressed on is not a control. It is now a computed over the token the row handed
over and the cache as it stands, so the quantity stays the token's and everything
the cache knows arrives as it resolves.

Decision: approved
