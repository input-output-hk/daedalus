Implementation: Iteration 1
Timestamp: 2026-09-15T06:05:00Z

Changes made:
- `source/renderer/app/stores/AssetsStore.ts`: two observable maps, the
  fingerprint memo, `details` and `getAsset` rebuilt on them, the subject
  reaction, the update subscription, and the per-token decimal setting moved off
  the endpoint's path.
- `source/renderer/app/stores/AssetsStore.spec.ts`: new. Eighteen cases across
  six groups.

Files touched:
- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/stores/AssetsStore.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-016.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-016-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-016-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

A correction to the approved plan, recorded rather than edited into it:

Acceptance criterion 5 says `searchAssets('und', ...)` does not match an
unresolved row once this task fills the fingerprint. It was written, it failed,
and the premise was wrong. `searchAssets` at `utils/assets.ts:283-300` tests its
regex against eight fields, and four of them are undefined on an unresolved row:
`fingerprint`, and `name`, `ticker` and `description` taken from a null
`metadata`. `RegExp.prototype.test` coerces its argument, so each undefined field
is the literal string `"undefined"` and any one of them matches `und`. Filling
the fingerprint removes one source of four.

Measured rather than reasoned about: with the fingerprint filled and `metadata`
set to `{ name: 'x' }`, the row still matches, because `ticker` and `description`
are undefined. The fix belongs where the coercion is, in `searchAssets`, and
`utils/assets.ts` is `task-017`'s to change; it is done there with its own cases.
The finding `task-002`'s review recorded is therefore narrower than it read: this
task removes the fingerprint as a source of the false match and does not close
the hole.

What the spec asserts here instead is the part this task owns: the merged row a
surface renders carries its identity and its fingerprint for a subject the cache
has no row for, carries the registry's values once it resolves, and is still
searchable by a published name.

Two details decided during implementation:

`getAsset` returns a memoised row for an unresolved subject. Nothing about such
a row can change while it stays unresolved, because the moment an entry or a
setting arrives the observable maps answer instead, and the alternative is a new
domain object and a blake2b digest per rendered row per paint.

`details` is built from the union of the two maps' keys rather than from the
metadata map alone, so a subject the user has set decimals for but the cache has
not resolved is still a row. Without that, a decimal setting on an unresolved
asset would be written to storage and never read back.

One intermediate state, named so it is not mistaken for a defect: between this
commit and `task-017`, the token list and the summary still render from the
endpoint's `all`, so a decimal setting changed in the dialog reaches the store
immediately but the two list surfaces show it on the next poll. `task-017` moves
both surfaces onto `getAsset` and the delay goes with them.

Verification run:

- `jest source/renderer/app/stores/AssetsStore --coverage=false` — 18 passed.
- The suite sets `configure({ enforceActions: 'observed' })` as the application
  does at `index.tsx:30`, so a mutation outside an action throws rather than
  passing quietly. Every case runs under it, including the one that reads through
  an `autorun`.
- A cached row is read with `requestAssetMetadata` asserted not to have been
  called at all, which is the "no network on a read" property at this layer.
- An unresolved subject returns identity plus the CIP-14 fingerprint, asserted
  against `assetFingerprint` rather than a literal, with `metadata` null. The
  same call twice returns the same object.
- A malformed identity returns nothing, driven three ways: a policy id that is
  not hex, one of the wrong length, and an asset name over the consensus limit.
- The push: an `autorun` reading `getAsset` records `undefined` then `BTED` after
  one `_onMetadataResolved`, with no request in between.
- The subject list merges holdings and transaction assets and deduplicates the
  subject that appears in both; a second run asks for nothing; a newly held
  subject is asked for on its own rather than with the whole list again.
- The decimal setting: read from browser storage at startup, written on submit
  with the exact arguments the storage API takes, readable through `getAsset`
  immediately, and applied to a subject with no cached row. The registry's value
  stays separate under `recommendedDecimals`, which is what the disagreement
  warning compares against.

Checks, all four through Nix with the new spec staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `97hx4fmswm09xw08bg00v4ddbi070hgn-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `28wqlzjc2y2b5wsxg1q8pljsdqldib3z-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 84 suites passed, 1246
  tests with 1243 passed and 3 skipped, exit 0. The previous state of this branch
  was 83 suites and 1228 tests, so one suite and eighteen tests were added and
  nothing else moved.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`; the three that remain in the
file are the ts-migrate suppressions that were already there.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- Acceptance criterion 5, for the reason under the correction above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T06:12:00Z

Acceptance criteria, each against the evidence:

1. *A cached row is returned with no request made.* Met, asserted on the mock
   rather than on timing.

2. *An unresolved subject returns a row carrying the locally computed
   fingerprint.* Met, and the value is computed in the assertion by the same
   function under test's collaborator, so a wrong fingerprint fails rather than
   a changed literal.

3. *Nothing comes back only for a malformed identity.* Met, three ways.

4. *A pushed update is visible to an observer without a further request.* Met
   through an `autorun`, which is what the components do.

5. *`searchAssets('und', ...)` does not match an unresolved row.* **Not met, and
   the correction above says why in terms of a measurement rather than an
   opinion.** The criterion assumed one source of the coercion and there are
   four. What this task can do it does, and the rest is `task-017`'s with its own
   cases. Recording it as met would have been the easy and wrong thing.

6. *The reaction asks once per subject, merged and deduplicated.* Met, with the
   second run and the newly held subject as separate cases, because "asks once"
   and "asks only for what is new" are different properties.

7. *The decimal setting round-trips.* Met, including for an unresolved subject,
   which is the case the union of the two maps exists for.

8. *No MobX strict-mode error.* Met by configuring strict mode in the suite
   rather than by inspecting the code. Constructing domain objects inside a
   computed is the part that would have failed if it were going to.

9-10. *Jest, lint, compile, suppressions, dependencies.* All met.

The judgement to weigh is `getAsset` answering for a subject the cache has never
heard of. It is what makes a cold cache render, what gives the CSV export a
fingerprint for an asset the wallet held two years ago, and it is a departure
from the graph's wording, argued in the plan before the work rather than
discovered in review.

Summary: `details` and `getAsset` keep their signatures and now read an
observable map fed by a request that answers from disk and a push that arrives
when the registry does. A token the wallet holds has an identity and a
fingerprint whether or not anything has been cached about it. The endpoint is
still there and still polling; `task-017` moves the last two readers off it and
`task-018` takes it away.

Decision: approved
