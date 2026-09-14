Implementation: Iteration 1
Timestamp: 2026-09-14T18:38:00Z

Changes made:
- `source/renderer/app/utils/assets.ts`: `getAssetTokenFromToken` now spreads the token and overlays only `fingerprint`, `metadata`, `decimals` and `recommendedDecimals` from the lookup. `uniqueId` falls back to the policy id followed by the asset name and `assetNameASCII` to `hexToString(assetName)`, which is what a token built from a transaction response needs and what a token built from the wallet balance already carries. The parameter is renamed from `asset` to `token`, since it was never an asset.
- `source/renderer/app/utils/assets.ts`: `getNonZeroAssetTokens` loses `.filter((token) => !!token.uniqueId)` and its `@TOKEN TODO` comment. The `.map` and the `.sort` are unchanged.
- `source/renderer/app/utils/assets.ts`: `sortAssets` compares `fingerprint || ''` at all four dereference points, through two named locals rather than inline, with a comment saying why a row can lack a fingerprint.
- `source/renderer/app/utils/assets.spec.ts`: new, ten cases across the two exported units.

Files touched:
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/utils/assets.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-002.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-002-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-002-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

The deviation, evidenced before it was taken:

The plan required the comparator guard to be shown necessary rather than argued. The merge helper and the filter deletion were applied first, the spec was written, and the suite was run against the unguarded comparator. It failed exactly as predicted:

```
● getNonZeroAssetTokens › keeps every token when no subject has a cached row

  TypeError: Cannot read properties of undefined (reading 'localeCompare')

    203 |     if (sortBy === 'fingerprint') {
    204 |       if (sortDirection === 'asc') {
  > 205 |         return fingerprint1.localeCompare(fingerprint2);
        |                             ^
    at sort (source/renderer/app/utils/assets.ts:205:29)
    at Array.sort (<anonymous>)
    at getNonZeroAssetTokens (source/renderer/app/utils/assets.ts:157:6)
```

One thing the run showed that the plan did not anticipate, and it strengthens the case rather than weakening it. The case with one resolved and one unresolved row **passed** unguarded. `Array.prototype.sort` over two elements calls the comparator once as `(second, first)`, so the resolved row landed in `fingerprint1` and `'asset1zzz'.localeCompare(undefined)` coerces its argument to the string `"undefined"` instead of throwing. Whether the crash happens therefore depends on which position the unresolved row lands in, which is an intermittent failure by construction: a wallet holding one unresolved token among resolved ones renders, and a wallet holding two unresolved tokens does not. The guard was then applied and all ten cases pass.

Verification run, all four checks through Nix on a dirty tree with the new spec staged, each built locally rather than substituted:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built `kxjgq08v7f7rh0ryfyk2fcacrdwnjq1l-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `13pb6pvc7zzk1qy7x1w850kr6sxpfgmj-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built `f3kn5x4a9giv61i7bw9awrbjdsg09ccl-daedalus-i18n.drv`, exit 0. No message changed in this task, so this check is a regression guard rather than a result.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 74 suites passed, 975 tests with 972 passed and 3 skipped, 6 snapshots passed, exit 0.

Against the baseline taken at `366da04dd` before any change, which was 73 suites and 965 tests with 962 passed and 3 skipped: one suite and ten tests added, nothing else moved.

No new `@ts-ignore` and no new `@ts-expect-error`: `git diff` adds neither and the new spec contains neither.

Deviations from the approved plan:
- None in scope or approach. The single planned deviation from the task graph, the comparator guard, was taken and evidenced first.
- One detail decided during implementation: the guard is two named locals, `sortableFingerprint1` and `sortableFingerprint2`, rather than `(fingerprint1 || '')` inline at four points. Four inline fallbacks are four places for a later edit to reintroduce the bug at one of them.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T18:41:00Z

Acceptance criteria, each against the evidence:

1. *A cold-cache render of the send form, the send confirmation and the transaction list shows every token the wallet holds, with fingerprint and raw quantity, before any metadata resolves.* **Partially met, as the plan required it to be reported.** Every token survives the merge and the sort: the spec drives `getNonZeroAssetTokens` with a lookup that returns `undefined` for every subject and asserts both rows come back with their `uniqueId`, and the previously crashing sort now completes. The fingerprint half of the criterion is not this task's to meet. `fingerprint` is `undefined` on an unresolved row by design, `task-005` computes one locally and `task-016` attaches it. The row exists, carries its raw quantity, and does not crash the surface it renders on; that is the part this task is responsible for.

2. *`uniqueId` for a row originating in a transaction is derived as `policyId + assetName`.* Met. Driven with a token shaped exactly as the transaction mapping builds one, with no `uniqueId` and no `assetNameASCII`, and asserted against the same key `AssetsStore` builds for the subject. A second case drives the same token shape against a lookup that hits and asserts the derived `uniqueId` still wins.

3. *`getNonZeroAssetTokens` no longer filters on `uniqueId`, and the filter is deleted rather than left always-true.* Met. The diff removes the line and its comment; `grep -n "uniqueId" source/renderer/app/utils/assets.ts` returns the destructure, the fallback and the unrelated `isTokenMissingInWallet` guard, and no filter.

4. *`yarn test:jest` passes.* Met, through `nix build '.#checks.x86_64-linux.jest'`, which built locally and reported 74 suites and 975 tests.

5. *Identity is never taken from the lookup.* Met, and asserted directly rather than implied: a lookup returning a different `uniqueId`, `policyId`, `assetName` and `assetNameASCII` for the same subject leaves all four of the token's values unchanged.

6. *A fully populated token merged against a fully populated lookup produces exactly what the current implementation produces.* Met. The merge is asserted with `toEqual` against a complete object literal, so an added or dropped field fails the case rather than passing quietly. This is what makes the change a no-op on the two wallet-balance call sites.

7. *`compile`, `lint` and `i18n` green from `nix build`.* Met. All three built locally on this machine rather than being substituted, which the log records by derivation path.

8. *No new `@ts-ignore` and no new `@ts-expect-error`.* Met.

Findings carried forward rather than fixed here:
- `isLoadingAssets` is now permanently false at `WalletSendPage.tsx:148` and `WalletTransactionsList.tsx:244`, because the merge never drops a row. That is the intended direction and `task-023` owns the spinner and the condition.
- `searchAssets` at `utils/assets.ts:269-285` tests a regex against `fingerprint`, which is now reachable as `undefined` and coerces to the string `"undefined"`. A three-character search for `und` would match every unresolved row. Out of scope, recorded in the plan under Risks, and closed by `task-016` filling the field.
- `task-016`'s `dependencies` in the task graph omit `task-005`, while its implementation notes require the locally computed fingerprint from it. Surfaced for whoever picks `task-016` up.

Summary: The change does what the task graph asked, with one deviation that a recorded test failure proves was necessary. The merge helper's contract is now stated in the file rather than implied by which fields happen to be destructured where, and the one case the task exists to prevent, identity coming from the lookup, is asserted rather than inferred from a passing suite.

Decision: approved
