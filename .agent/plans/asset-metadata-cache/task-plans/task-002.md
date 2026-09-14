# Task task-002: Take merged-token identity from the token, not from the asset lookup

## Task ID and Title

- ID: `task-002`
- Title: `Take merged-token identity from the token, not from the asset lookup`

## Why Chosen Now

`task-002` has no dependencies in the task graph and is the second task in phase 1. It is
renderer-only: no cache, no IPC channel, no network call and no new dependency.

It is scheduled ahead of the cache because the whole cache design turns on it. `task-016` rewires
`AssetsStore` to read a store that starts empty, and `getNonZeroAssetTokens` currently drops every
row whose subject is not in that store. Landing the cache first would empty the send form, the send
confirmation and the transaction list on first run, and the emptiness would look like a cache bug
rather than the merge bug it actually is. Two later tasks name `task-002` as a dependency in the
graph, `task-016` and `task-023`, and `task-016` is itself a dependency of six more, so it is on the
critical path rather than beside it.

It is also correct on its own terms today. A token the wallet holds is dropped from the send form
whenever the assets poll has not yet returned, which is every render between application start and
the first successful poll.

## Interaction Mode

- Mode: `agent_execution`

Every acceptance criterion resolves to a Jest spec or to one of the four Nix checks. Nothing needs a
running node, a network fetch or an operator.

## Scope

- `getAssetTokenFromToken` takes `uniqueId`, `policyId`, `assetName` and `assetNameASCII` from the
  token, and takes only `metadata`, `decimals` and `recommendedDecimals` from the asset lookup.
- For a row that originates in a transaction the token carries neither `uniqueId` nor
  `assetNameASCII`, so both are derived: `uniqueId` as `policyId` followed by `assetName`, and
  `assetNameASCII` by the same `hexToString` decode the two wallet-balance mappings already use.
- `getNonZeroAssetTokens` loses its `.filter((token) => !!token.uniqueId)`. The filter is deleted,
  not left always-true.
- `sortAssets` stops dereferencing `fingerprint` without a guard, because deleting the filter is what
  first makes an absent fingerprint reachable by the comparator. Recorded as a deviation below, with
  the evidence that the task's own acceptance criterion cannot hold without it.
- A colocated `utils/assets.spec.ts` covering the merge, the transaction-originated derivation, the
  survival of an unresolved row, and the rule that identity is never taken from the lookup.

Revertible on its own. Reverting the commit restores the filter and the lookup-sourced identity
exactly. No other phase-1 task reads anything this task adds: `task-003` touches
`components/wallet/send-form/AssetInput.tsx`, `task-004` touches three symbols this task does not
read, and `task-005` adds a new file.

## Non-Goals

- No fingerprint. `fingerprint` stays absent on an unresolved row. `task-005` computes one locally
  and `task-016` wires it in. Nothing here reaches for the assets endpoint to fill it.
- No change to `getAssetToken` or `getAssetTokens` at `utils/assets.ts:74-113`. Those are fed by
  `assets.all`, every element of which already carries `uniqueId` and `fingerprint`, so the filter
  at `:112` is not the same filter and is `task-017`'s to move.
- No change to the `isLoadingAssets` conditions, which this task makes permanently false at two of
  their four sites. `task-023` owns the spinner and its condition, and names `task-002` as a
  dependency.
- No change to `transactionsCsvGenerator.ts:190-191` or to the `'unknown fingerprint'` literal.
  `task-016` owns it.
- No change to `AssetsStore.all` or to `WalletSendPage.tsx:134`. `task-016` owns deriving `all` from
  holdings.
- No change to `searchAssets`, to `assetNameASCII`'s meaning, or to which decoder produces it.

## Dependencies

- None in the task graph. `task-002` has `"dependencies": []` in
  `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.
- Practical dependency: Nix, for the four checks. Verified working on this branch: `compile`, `lint`
  and `i18n` are green at `366da04dd` and `jest` is green with 73 suites and 965 tests, 962 passed
  and 3 skipped, confirmed by a forced local rebuild rather than a substituted result.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the merge-helper section at
  `:1254-1275` and the two further consumers at `:1277-1292`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-002` entry, plus
  `task-016`, `task-017` and `task-023` to establish who owns each consequence.
- `.agent/plans/asset-metadata-cache/prompt.md`, the enabling fact.
- `.agent/plans/asset-metadata-cache/task-plans/task-001.md` as the immediate precedent.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md`, the conventions section: `type` over `interface`, `Array<T>` over `T[]`, colocated
    `<Unit>.spec.ts`, `describe` naming the exported unit with no article, `it` in present-tense
    third person.
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map.
- Skills:
  - None apply. No message, no theme variable and no SCSS changes in this task.

## Live Repo Findings Verified For Planning

Verified at `366da04dd` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The defect, as it stands.**

- `source/renderer/app/utils/assets.ts:126-128` destructures `uniqueId` out of the lookup:
  `const { fingerprint, metadata, decimals, recommendedDecimals, uniqueId } = getAsset(policyId, assetName) || {};`
  The `|| {}` means every one of the five is `undefined` when the lookup misses.
- `source/renderer/app/utils/assets.ts:148` is `.filter((token) => !!token.uniqueId)`, so a row whose
  lookup missed is dropped.
- `source/renderer/app/stores/AssetsStore.ts:72-73` is the lookup: it indexes `this.details` by the
  policy id followed by the asset name. `details` is built at `:63-70` by reducing `this.all`, which
  is `get(request, 'result.assets', [])` at `:60`. Before the first successful poll `all` is `[]`, so
  every lookup misses and every row is dropped.

**Three call sites, and the one that behaves differently.**

Verified by `grep -rn "getNonZeroAssetTokens" source`.

| Call site | Tokens passed | Carries `uniqueId` | Carries `assetNameASCII` |
|---|---|---|---|
| `containers/wallet/WalletSendPage.tsx:144` | `wallet.assets.total` | yes | yes |
| `containers/wallet/dialogs/send-confirmation/SendConfirmation.container.tsx:47` | `activeWallet.assets.total` | yes | yes |
| `components/wallet/transactions/WalletTransactionsList.tsx:240` | `tx.assets` | **no** | **no** |

- `api/api.ts:3213-3236` builds `walletAssets`. `uniqueId` is set at `:3216` and `:3227` as
  `` `${policyId}${assetName}` `` and `assetNameASCII` at `:3221` and `:3232` as
  `hexToString(assetName)`. Those are the first two rows of the table. The task graph cites `:3218`
  and `:3229`, which are the lines where the already-computed `uniqueId` is placed into the object
  literal; the assignments are two lines earlier in this tree.
- `api/api.ts:3321-3329` builds `transactionAssets` with exactly `policyId`, `assetName`, `quantity`
  and `address`. Neither `uniqueId` nor `assetNameASCII` is present, and `api.ts:3355` carries a
  `@ts-ignore` for precisely that mismatch against the declared `Token` type. So the third row of
  the table is not an edge case: **every** transaction-list row needs both fields derived, whether
  or not the lookup hits.
- `api/assets/types.ts:49-56` declares `uniqueId: string` as required on `Token` and
  `assetNameASCII?: string` as optional. The transaction path satisfies neither at runtime, which is
  what the `@ts-ignore` hides.

**Deleting the filter makes an absent `fingerprint` reachable by the comparator.**

- `utils/assets.ts:149` sorts the merged rows with `sortAssets('fingerprint', 'asc')`.
- `sortAssets` dereferences the field with no guard at four points: `:189`, `:192`, `:197` and
  `:200`, each `fingerprintN.localeCompare(...)`.
- Today the `:148` filter guarantees the comparator only ever sees rows whose lookup hit, and every
  element of `assets.all` carries a `fingerprint` (`api.ts:3394-3400` constructs `Asset` with the
  endpoint's `fingerprint`; `domains/Asset.ts:14` defaults it to `''`). Remove the filter and the
  comparator receives `undefined`, and `undefined.localeCompare` is a `TypeError`.
- `Array.prototype.sort` does not call the comparator for a zero- or one-element array, so the crash
  needs a wallet holding two or more unresolved tokens. That is the ordinary cold-cache case, not a
  rare one.
- This is the finding that the task graph's note "the fingerprint sort inside `getNonZeroAssetTokens`
  is unchanged" does not anticipate. The sort *order* is unchanged; the comparator has to stop
  throwing.

**Two consequences that belong to later tasks, recorded rather than fixed.**

- `isLoadingAssets` is `hasRawAssets && totalAssets < totalRawAssets` at
  `WalletSendPage.tsx:148` and `WalletTransactionsList.tsx:244`. Both are computed from the output of
  `getNonZeroAssetTokens`, which after this change never drops a row, so both become permanently
  false. `Transaction.tsx:666` and `WalletSendForm.tsx:101` are the consumers. This is the intended
  direction, since a row that renders immediately has nothing to wait for, and `task-023` deletes the
  spinner and the condition. The other two sites, `WalletTokensPage.tsx:46` and
  `WalletSummaryPage.tsx:129`, read `getAssetTokens` and are untouched here.
- `utils/transactionsCsvGenerator.ts:190-191` resolves `getAsset(policyId, assetName)` and falls back
  to the literal `'unknown fingerprint'`. It does not go through `getAssetTokenFromToken`, so this
  task neither helps nor harms it. `task-016` owns it, and `TransactionsStore.ts:390-408` is the
  caller that passes `allFiltered` rather than the rendered page.
- `containers/wallet/WalletSendPage.tsx:134` destructures `all: allAssets` and `:115-117` resolves
  the clicked token from it. That path is `AssetsStore.all`, not this helper. `task-016` owns it.

**Existing test coverage.**

- There is no `source/renderer/app/utils/assets.spec.ts`. `ls source/renderer/app/utils/*.spec.ts`
  returns eight files and none of them covers `utils/assets.ts`.
- `jest.config.js:156` is `testMatch: ['**/?(*.)+(spec|test).[tj]s?(x)']`, so a colocated
  `assets.spec.ts` is picked up with no config change.
- `grep -rn "getAssetTokenFromToken\|getNonZeroAssetTokens" tests storybook` returns nothing, so no
  Cucumber feature and no story depends on the current behaviour.

**A note on the Nix checks and this worktree.**

- `perSystem/checks.nix:15-32` builds every JS check from `srcWithoutNix`, which
  `nix/internal/common.nix:259-295` derives from `inputs.self`. For a flake, that is the git tree:
  **a new file that has not been `git add`ed is invisible to every check.** New spec files are staged
  before any check is run.
- Verified that the checks see this worktree's source rather than a cached upstream result: a
  deliberate `const __probe: number = "not a number";` appended to `utils/waitFor.ts` made
  `nix build '.#checks.x86_64-linux.compile'` fail with `TS2322` at `waitFor.ts:12`, and the file was
  restored afterwards. `nix build '.#checks.x86_64-linux.jest' --rebuild` built locally and reported
  73 suites and 965 tests.

## Files Expected To Change

- `source/renderer/app/utils/assets.ts` — the merge helper, the deleted filter, the comparator guard.
- `source/renderer/app/utils/assets.spec.ts` — new. The merge, the derivation, the survival of an
  unresolved row.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-002` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-002*.md` — this plan and its two review logs.

One deviation from the task graph's `targetPaths`, recorded rather than taken silently:

1. `sortAssets` in the same file gains a guard on `fingerprint`. The task graph lists
   `utils/assets.ts` as a target path, so the file is in scope; what is beyond the task graph's
   wording is touching a second function in it. The reason is under Live Repo Findings: the task's
   first acceptance criterion is a cold-cache render of three surfaces, and without the guard that
   render is a `TypeError` for any wallet holding two or more unresolved tokens.

`api/api.ts` is **not** changed. Deriving the two fields in the merge helper keeps the change inside
one function and inside one revertible commit, and the `@ts-ignore` at `api.ts:3355` that hides the
`Token` mismatch is not this task's to remove.

## Implementation Approach

1. **`getAssetTokenFromToken`.**

   Destructure the token, spread it, and overlay only the four fields the lookup owns:

   ```ts
   const { policyId, assetName, assetNameASCII, uniqueId } = token;
   const { fingerprint, metadata, decimals, recommendedDecimals } =
     getAsset(policyId, assetName) || {};
   return {
     ...token,
     uniqueId: uniqueId || `${policyId}${assetName}`,
     assetNameASCII: assetNameASCII || hexToString(assetName || ''),
     fingerprint,
     metadata,
     decimals,
     recommendedDecimals,
   };
   ```

   The two fallbacks exist only for the transaction path. For the two wallet-balance call sites both
   fields are already present and the fallback is not evaluated, so those rows are byte-identical to
   what they are today.

   `hexToString` rather than `hexToPrintableAsciiString`: `assetNameASCII` is a *search* field, not a
   rendered one. Its only readers are `searchAssets` at `utils/assets.ts:269-285` and `getZeroToken`
   at `:37-48`, and `api.ts:3221` and `:3232` populate it with `hexToString`. Deriving it with a
   different decoder would make a transaction row searchable by different text than the wallet row
   for the same asset. `task-001` deliberately left `assetNameASCII` alone for the same reason, and
   nothing renders it.

2. **`getNonZeroAssetTokens`.** Delete `.filter((token) => !!token.uniqueId)` at `:148`, with its
   `@TOKEN TODO` comment. The `.map` and the `.sort` are unchanged.

3. **`sortAssets`.** Compare `fingerprint1 || ''` against `fingerprint2 || ''` at all four
   dereference points. Two rows that both lack a fingerprint compare equal, and `Array.prototype.sort`
   is stable, so they keep the order the wallet handed them in. Deterministic, and the case
   disappears entirely once `task-016` supplies a locally computed fingerprint for every row.

4. **Doc comments.** The block at `:115-121` says the helper "combines with the data from the Asset".
   Reword it to say identity comes from the token and only the registry-sourced fields come from the
   lookup, since that is the property the next four tasks depend on.

5. **Spec.** `source/renderer/app/utils/assets.spec.ts`, colocated, `describe` per exported unit.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **A cold-cache render of the send form, the send confirmation and the transaction list shows every
   token the wallet holds, with fingerprint and raw quantity, before any metadata resolves.** Settled
   by a spec that drives `getNonZeroAssetTokens` with a `getAsset` returning `undefined` for every
   subject and asserts that the output length equals the input length, that each row keeps its
   `uniqueId`, `policyId`, `assetName`, `quantity` and `address`, and that the call does not throw.
   The "with fingerprint" clause is met by `task-005` and `task-016`, not here; this task's part is
   that the row exists at all and that its absence of a fingerprint does not crash the sort. The
   criterion is therefore **partially** met by this task, and the Final Outcome says so rather than
   reporting it as met.
2. **`uniqueId` for a row originating in a transaction is derived as `policyId + assetName`, because
   `api.ts:3321-3329` carries neither `uniqueId` nor `assetNameASCII`.** Settled by a spec driving a
   token shaped exactly as `api.ts:3321-3329` builds one and asserting both derived values, with the
   derived `uniqueId` asserted equal to the key `AssetsStore.ts:67` builds for the same subject.
3. **`getNonZeroAssetTokens` no longer filters on `uniqueId`, and the filter is deleted rather than
   left always-true.** Settled by reading the diff and by `grep -n "uniqueId" utils/assets.ts`.
4. **`yarn test:jest` passes.** Run as `nix build '.#checks.x86_64-linux.jest' --no-link`.

Four criteria this task adds to its own closure:

5. Identity is never taken from the lookup. A lookup that returns a *different* `uniqueId`,
   `policyId` or `assetName` for a subject must not change the merged row. This is the regression the
   task exists to prevent and it is asserted directly rather than implied.
6. A row whose token and lookup are both fully populated merges to exactly what the current
   implementation produces. The change is meant to be invisible on the two wallet-balance call sites,
   and that is asserted field by field rather than left to the passing of the rest of the suite.
7. `compile`, `lint` and `i18n` are green from `nix build`, not from host tooling.
8. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

Repository verification already done for planning is under Live Repo Findings. Execution
verification:

- `nix build '.#checks.x86_64-linux.jest' --no-link`, with the new spec staged so the flake can see
  it.
- The spec drives these cases, each named rather than implied:
  - A token with no cached row: every identity field survives, `metadata`, `decimals`,
    `recommendedDecimals` and `fingerprint` are `undefined`.
  - A token with a cached row: gains `metadata`, `decimals`, `recommendedDecimals` and `fingerprint`,
    keeps its own `quantity` and `address`.
  - A transaction-originated token, shaped as `api.ts:3321-3329` builds it: `uniqueId` derived as
    `policyId + assetName`, `assetNameASCII` derived by `hexToString`, and the derived `uniqueId`
    equal to the key `AssetsStore.ts:67` builds.
  - A transaction-originated token whose lookup also hits: the derived `uniqueId` still wins over
    the lookup's.
  - A hostile lookup returning a different `uniqueId`, `policyId` and `assetName`: the merged row
    keeps the token's.
  - An asset name whose bytes are not text: `assetNameASCII` is still derived and still does not
    throw, because the field is a search field and `hexToString` is total.
  - `getNonZeroAssetTokens` over two tokens with no cached rows: both survive, the call does not
    throw, and the order is deterministic. This is the case that fails without the comparator guard,
    so the guard is verified by a test rather than by inspection.
  - `getNonZeroAssetTokens` over a mix of resolved and unresolved tokens: all survive and the
    resolved ones sort by fingerprint.
  - A fully populated token merged against a fully populated lookup: every field of the result
    asserted explicitly, so the no-op on the two wallet-balance call sites is checked rather than
    assumed.
- The comparator guard is shown to be **necessary**, not merely present: the
  `getNonZeroAssetTokens` case above is run once against the unguarded comparator and the resulting
  `TypeError` is recorded verbatim in the implementation review log before the guard is applied. A
  deviation from the task graph argued from reasoning alone is a deviation nobody can check.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff` read for new `@ts-ignore`, of which there must be none.

If a check reports a substituted result rather than a local build, it is re-run with `--rebuild` so
the reported result is one this machine produced.

## Risks and Open Questions

1. **The transaction list gains rows it did not have.** Today a transaction referencing an asset the
   poll never returned shows fewer asset rows than the transaction contains, and
   `isLoadingAssets` renders a spinner in the gap. After this change the row renders with a raw
   quantity and, until `task-005` and `task-016` land, no fingerprint and no name. That is a visible
   change on today's code, not only under the future cache. It is the honest rendering: the row
   exists on chain either way. Named here so it is not a surprise at review.
2. **A row with no fingerprint renders an empty identity between this task and `task-016`.**
   `Asset.tsx:213-217` already guards with `fingerprint || ''`, so it renders empty rather than
   throwing. Bounded, and closed by `task-005` plus `task-016`, which is why both are in the same
   phase sequence.
3. **The comparator guard changes ordering for rows that lack a fingerprint.** They sort first in
   ascending order and among themselves keep wallet order. The alternative, sorting them by
   `uniqueId`, would give a different order again once `task-016` fills the fingerprints in. Sorting
   the unknowns together and leaving them stable is the smaller commitment.
4. **`searchAssets` passes `fingerprint` straight into `regex.test(item)`.** With `fingerprint`
   `undefined` that coerces to the string `"undefined"`, so a three-character search for `und` would
   match every unresolved row. Pre-existing in shape, since `metadata` is already passed as an object
   into the same list, but newly reachable because unresolved rows now survive. Not fixed here: the
   `@ts-ignore` at `:283` is the marker that this predicate needs its own pass, it is not in this
   task's acceptance criteria, and `task-016` removes the condition by filling the field. Recorded
   per `CLAUDE.md`.
5. **Open question for the project owner, not blocking:** whether the transaction list should show
   anything at all for an asset whose subject resolves to nothing once the spinner goes in
   `task-023`. Current behaviour after this change is a raw quantity beside an empty identity. The
   alternative is a placeholder. `task-023`, which deletes that spinner and names `task-002`
   as a dependency, is the right place to settle it; naming it here so it is carried rather than
   discovered.

## Required Docs, Research, and Tracking Updates

- Update `task-002`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-002-plan-review.md` and `task-002-impl-review.md` as the cycle requires.
- No PRD change. The PRD at `:1254-1275` describes exactly what is built. Its line citations for the
  two `uniqueId` assignments are two lines off in this tree and the correct ones are recorded above
  rather than edited into the PRD, which is under review on a separate branch.
- No `.agent/` documentation correction. The divergence found in passing, `searchAssets` testing a
  regex against a possibly-undefined field, is a source defect and is recorded under Risks.
- One inconsistency in the task graph itself, recorded and not edited here because editing another
  task's entry from inside this one would hide it: `task-016`'s implementation notes require the
  locally computed fingerprint from `task-005`, but `task-005` is not in `task-016`'s
  `dependencies`, which lists `task-001`, `task-002`, `task-014` and `task-015`. Whoever picks up
  `task-016` needs `task-005` landed first. Surfaced in the handoff.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-002-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-002-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-002` complete. Reviewed and approved in `task-002-impl-review.md`.
- Identity on a merged row comes from the token. A token the wallet holds, or a transaction
  references, now survives the merge whether or not anything has been cached about its subject, and
  `getNonZeroAssetTokens` no longer has a filter to drop it.
- Acceptance criterion one is met in the part this task owns and not in the part it does not. Every
  token renders with its raw quantity; the fingerprint arrives with `task-005` and `task-016`.
- The one deviation from the task graph, a guard on the fingerprint comparator, was evidenced by
  running the new spec against the unguarded comparator first. The recorded failure also showed the
  crash is position-dependent: a wallet with one unresolved token among resolved ones renders, one
  with two unresolved tokens does not.
- Checks, all from `nix build` and all built locally rather than substituted: `compile` exit 0,
  `lint` exit 0, `i18n` exit 0, `jest` 74 suites and 975 tests with 972 passed and 3 skipped. The
  baseline at `366da04dd` was 73 suites and 965 tests.
- Carried forward: `isLoadingAssets` is now permanently false at two of its four sites, which is
  `task-023`'s to remove; `searchAssets` tests a regex against a possibly-undefined `fingerprint`,
  which `task-016` closes by filling the field; and `task-016`'s graph entry is missing `task-005`
  from its dependencies although its notes require it.

## Self-Review

- The plan implements what the task graph specifies and deviates in exactly one place, the
  comparator guard, which is recorded with the evidence that the task's own first acceptance
  criterion cannot hold without it.
- Every claim under Live Repo Findings carries a `path:line`, a command or a count taken at
  `366da04dd`.
- The three consumers the PRD names as losing historically-held assets are each traced to the task
  that owns them rather than fixed here.
