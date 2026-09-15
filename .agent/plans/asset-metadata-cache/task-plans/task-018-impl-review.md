Implementation: Iteration 1
Timestamp: 2026-09-15T07:20:00Z

Changes made:
- `source/renderer/app/stores/AssetsStore.ts`: the interval and its constant, the
  one-shot listener that also triggered a refresh, `_refreshAssetsData`, the
  request map and both request helpers, and the refresh call in the settings
  submit.
- `source/renderer/app/api/api.ts`: `getAssets`, `_createAssetFromServerData`,
  the in-memory metadata carry-over and four imports that went with them.
- `source/renderer/app/api/assets/requests/getAssets.ts`: deleted, and with it
  the last file in that directory.
- `source/renderer/app/api/assets/types.ts`: `ApiAsset`, `ApiAssets`,
  `GetAssetsRequest`, `GetAssetsResponse` and `StoredAssetMetadata`, plus the
  domain import only `GetAssetsResponse` used. The comment above `Asset` said the
  type was fetched from the assets endpoint and now says where it comes from.
- `source/renderer/app/stores/AssetsStore.spec.ts`: one case for the timer.

Files touched:
- the four source files above
- `source/renderer/app/stores/AssetsStore.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-018.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-018-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-018-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One correction to the approved plan, recorded rather than edited into it:

The Verification Plan asked for a case asserting `setup` registers no timer,
written as a count of pending timers. It was written that way and it failed: one
timer is pending after `setup`, and it belongs to `Request.execute`
(`stores/lib/Request.ts:39`), which the favourites request goes through and
which has nothing to do with polling. Counting timers therefore asserts something
other than the property. The case now spies on the repeating timer specifically,
asserts it is never called, and advances ten minutes of simulated time against a
poll that used to fire every sixty seconds, asserting no metadata request is
issued in that window.

Verification run:

- `jest source/renderer/app/stores/AssetsStore --coverage=false` — 19 passed, of
  which one is new.
- The grep sweep, over `source`, `tests` and `features`, excluding
  `getAssetsLocalData` which is a different function that stays: `getAssets`,
  `ASSETS_REFRESH_INTERVAL`, `ApiAsset`, `GetAssetsRequest`,
  `GetAssetsResponse`, `storedAssetMetadata` and `StoredAssetMetadata` each
  return zero matches.
- The wallet's `/assets` path appears nowhere under `source/` now that the
  request module is gone.
- The eighteen existing store cases are the regression guard for what stays, and
  the one that matters most here is the per-token decimal setting: it still
  round-trips with the refresh call removed, which is what shows that call was
  serving the endpoint rather than the user.

Checks, all four through Nix:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0. This is the
  principal check for a deletion of this shape: every name removed was exported,
  so a missed reader fails here rather than at runtime.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `104z0sp78db4kyijl8gcxca6mawv58xc-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `a6m5s33mxm4ydk5qm8ddi841al8za11r-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 84 suites passed, 1252
  tests with 1249 passed and 3 skipped, exit 0. The previous state of this branch
  was 84 suites and 1251 tests, so one test was added and nothing else moved.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`; three went with the code they
suppressed.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The timer case, for the reason under the correction above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T07:28:00Z

Acceptance criteria, each against the evidence:

1. *No request to the wallet's `/assets` path can be issued.* Met structurally:
   the module that named the path and the method that called it are both gone,
   and the path appears nowhere under `source/`.

2. *The grep sweep returns nothing.* Met, seven names over three directories.

3. *No timer in `AssetsStore`.* Met, and asserted against the repeating timer
   rather than against a count that a promise helper was already contributing to.
   The correction is the useful part of this record: the first version of the
   case would have passed only by accident and failed for an unrelated reason.

4-5. *Jest, lint, compile, suppressions, dependencies.* All met.

What this commit removes is worth stating plainly: a request per wallet every
sixty seconds for the lifetime of the application, whose response was mapped into
domain objects and then read by nothing. The in-memory carry-over goes with it,
and its reason for existing goes too: it survived the endpoint omitting metadata
on a later poll, and there is no later poll.

This is the revert boundary the task graph names. Reverting this commit alone
leaves the renderer with no source of metadata, because every consumer has
already moved.

Summary: Phase 3 is complete. The renderer asks the main process for the rows it
has, gets them back without a network round trip, receives the rest as they
resolve, and nothing polls.

Decision: approved
