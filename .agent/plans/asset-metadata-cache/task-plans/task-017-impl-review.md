Implementation: Iteration 1
Timestamp: 2026-09-15T06:45:00Z

Changes made:
- `source/renderer/app/containers/wallet/WalletTokensPage.tsx` and
  `WalletSummaryPage.tsx`: both build their list with
  `getNonZeroAssetTokens(walletTokens, getAsset)`.
- `source/renderer/app/containers/wallet/WalletSendPage.tsx`: the selected asset
  is resolved against the holdings-derived list, which is computed before it is
  needed rather than after.
- `source/renderer/app/components/wallet/WalletSendForm.tsx`: `selectedAsset`
  takes the merged row type.
- `source/renderer/app/utils/assets.ts`: `getAssetTokens`, `getAssetToken`,
  `getToken` and `getZeroToken` deleted; `searchAssets` tests only fields that
  are strings.
- `source/renderer/app/stores/AssetsStore.ts`: `all` deleted.
- `source/renderer/app/utils/assets.spec.ts`: five cases for `searchAssets`.

Files touched:
- the six source files above
- `source/renderer/app/utils/assets.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-017.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-017-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-017-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One deviation discovered by the compiler rather than by the plan:

`WalletSendForm`'s `selectedAsset` prop was typed as the domain `Asset`, which
carries an `update` method, and the holdings-derived list holds merged rows.
`compile` failed at `WalletSendPage.tsx:169` with the prop's declaration site
named. The prop is read for one field, `uniqueId`, at `WalletSendForm.tsx:208`,
so the type was widened to the merged row rather than the value cast at the call
site. The domain import was the only other use of that name in the file and went
with it. `WalletSendForm.tsx` is in `task-023`'s target paths for an unrelated
reason and this change does not touch what `task-023` will.

Verification run:

- `jest source/renderer/app/utils/assets --coverage=false` — 15 passed, of which
  5 are new.
- The search cases are driven against a row built by `getAssetTokenFromToken`
  with a lookup that misses, which is the shape a cold cache produces, rather
  than against a hand-made object that might not have the same absences.
- Four three-letter searches that a coercion would match are each asserted to
  match nothing: `und` and `fin` for `"undefined"`, `obj` and `ect` for
  `"[object Object]"`. Before the fix, `und` matched every unresolved row.
- The complement is asserted in the same group, so the fix cannot pass by
  matching nothing at all: a published name, ticker and description each still
  match, an unresolved row still matches through its policy id, its asset name
  and its decoded name, a resolved row still matches through its fingerprint, and
  a search under three characters still returns everything.
- `grep` over `source/` for `getAssetTokens`, `getAssetToken`, `getZeroToken`,
  `getToken` and `assets.all` returns nothing outside history.

Criterion 5, the CSV export, is a consequence rather than a new behaviour.
`transactionsCsvGenerator.ts:190` falls back to `'unknown fingerprint'` only when
`getAsset` returns nothing, and after `task-016` that happens only for an
identity that cannot have a CIP-14 fingerprint at all, which is asserted there by
three cases. An asset the wallet held two years ago and no longer holds resolves
through the same path as one it holds now.

Checks, all four through Nix:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0, after the
  failure described above.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `06rgp6dxfg7isz152wjqypazj7ckdh5c-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `m16p25i0j2m3j4f0047mzm21y7xm6709-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 84 suites passed, 1251
  tests with 1248 passed and 3 skipped, exit 0. The previous state of this branch
  was 84 suites and 1246 tests, so five tests were added to an existing suite and
  nothing else moved.

`nix fmt` was run and changed two files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`; one existing suppression went
with the `searchAssets` rewrite, because the list it silenced no longer holds a
non-string.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- `WalletSendForm.tsx`, for the reason above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T06:52:00Z

Acceptance criteria, each against the evidence:

1. *Both list surfaces render every held token on a cold cache.* Met
   structurally: both now map over what the wallet reports, and
   `getNonZeroAssetTokens` has three existing cases asserting it keeps every
   token when no subject, or only some, has a cached row.

2. *The send form resolves the clicked token from holdings.* Met. The line that
   would have kept this broken is the one that resolved against the cache, and
   it is gone rather than guarded.

3. *A three-letter search matches nothing through an absent field.* Met, four
   ways, with the complement asserted so the fix cannot pass vacuously.

4. *The four helpers and `all` are gone.* Met, by grep rather than by reading the
   diff.

5. *CSV export resolves a fingerprint for a historically held asset.* Met as a
   consequence, argued above rather than restated as a test that would assert
   `getAsset`'s behaviour a second time.

6-7. *Jest, lint, compile, suppressions, dependencies.* All met, and one
   suppression was removed.

The compile failure is the useful part of this record. The prop type was the last
place the endpoint's domain object was assumed, and the type system named it
rather than leaving it to be found when a send form opened with nothing selected.

Summary: Nothing reads the endpoint's list. Every surface that shows tokens now
builds its rows from what the wallet holds and overlays what the cache knows, so
a cold cache renders a complete list in fingerprint order. The search no longer
matches every unresolved row on a search for `und`. What is left of the endpoint
is a poll nobody reads, which is `task-018`.

Decision: approved
