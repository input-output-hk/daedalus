## Task ID and Title

`task-023` — Delete the whole-list spinner and the condition behind it.

## Why Chosen Now

`task-017` moved every token surface onto `getNonZeroAssetTokens(walletTokens,
getAsset)`, which returns one row per held token whether or not the cache knows
anything about it, and `task-018` removed the endpoint that used to return fewer
rows than the wallet holds. The condition the spinner reads,
`hasRawAssets && totalAssets < totalRawAssets`, compares a list against the list
it was built from. It is now false at every one of its four sites, and a branch
that can never be taken is worse than no branch: it reads as a state the
application can be in.

## Interaction Mode

`agent_execution`.

## Scope

The four computations of `isLoadingAssets`, the props that carry it, and every
branch that reads it. The spinner that replaced the whole token list, and the
"fetching token data" placeholder that replaced a transaction's asset rows, both
go with it.

## Non-Goals

- No change to what any surface renders in the states that remain. Removing a
  branch that is never taken must not move a pixel.
- Not the restore spinner. `WalletTokens.tsx:140-149` renders a spinner while the
  wallet is restoring, which is a real state and stays.
- Not `isLoadingTransactions`, which is a different prop on the same components
  and is fed by a request that genuinely executes.

## Dependencies

`task-002`, `task-018`.

## Research Consulted

- `asset-metadata-cache-prd.md:1234-1300`, the cold cache and the send path: a
  row renders from what the wallet holds, so there is no state in which the list
  is empty because metadata has not arrived.
- `asset-metadata-cache-prd.md:1347-1416`, Components Affected, which names the
  spinner as the surface a separated holdings path makes unreachable.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`, for what happens to a message whose
  only reader is deleted.

## Live Repo Findings Verified For Planning

1. **`getNonZeroAssetTokens` is one row per token.** `utils/assets.ts:79-85` maps
   over its `tokens` argument and sorts; it filters nothing. So
   `assetTokens.length` equals the length of the list it was given at all four
   sites, and `totalAssets < totalRawAssets` is false whenever `hasRawAssets` is
   true.
2. **Four computations, all of the same shape.**
   `WalletTokensPage.tsx:48-51`, `WalletSummaryPage.tsx:126-129`,
   `WalletSendPage.tsx:145-148` and `WalletTransactionsList.tsx:241-244`.
3. **`hasRawAssets` is not cache-derived and two of the four sites still need
   it.** `WalletSummaryPage.tsx:184` and `WalletSendPage.tsx:168` both read it
   for `hasAssetsEnabled && hasRawAssets`. It is `wallet.assets.total.length > 0`,
   a fact about holdings, and it stays at those two sites and goes at the other
   two, where nothing else reads it.
4. **Eleven readers.** `WalletTokens.tsx:48,77,83`;
   `WalletTokensList.tsx:26,61,80,124,129,135-140`; `WalletSummary.tsx:38,72,108`;
   `WalletSendForm.tsx:101`; `Transaction.tsx:267,478,666-680`;
   `WalletNoTokens.tsx:39,51,55`; and two specs,
   `WalletSendForm.spec.tsx:123,681` and `WalletTokens.spec.tsx:40`.
5. **`WalletSendForm.tsx` declares the prop and never reads it.**
   `grep -n isLoadingAssets source/renderer/app/components/wallet/WalletSendForm.tsx`
   returns one line, the declaration at `:101`.
6. **`WalletNoTokens`'s branch is already dead.** It is rendered only from
   `WalletTokensList.tsx:125-132`, under `if (!hasTokens)`, and `hasTokens` is
   `assets.length || isLoadingAssets`, so `isLoadingAssets` is false wherever
   `WalletNoTokens` is reached. Its `!isLoadingAssets &&` guard at `:55` therefore
   never suppresses anything today. The prop is deliberately removed rather than
   kept, which settles the question the task graph left open.
7. **Deleting the `Transaction.tsx` branch orphans a message.**
   `wallet.transaction.fetchingTokenData` is defined at `Transaction.tsx:171-175`
   and read only at `:676`. `yarn i18n:manage` rewrites the three locale files and
   `translations/messages.json` when a message disappears, and `perSystem/checks.nix:59-75`
   fails if the tracked artifacts are not what the source produces, so the
   regenerated artifacts belong in this commit.
8. **The task graph's target paths do not name storybook, and `tsc` does.**
   `package.json:45` is `tsc --noEmit` and `tsconfig.json` excludes only
   `node_modules`, so `storybook/` is type-checked; `package.json:43` lints it
   too. Five stories pass the prop:
   `Transaction.stories.tsx:199`, `WalletSummary.stories.tsx:244`,
   `WalletTokens.stories.tsx:153`, `WalletTokensList.stories.tsx:165` and
   `WalletSend.stories.tsx:313,341,371,401`. They are in scope, and the graph's
   `targetPaths` are corrected to say so.

## Files Expected To Change

- `source/renderer/app/containers/wallet/WalletTokensPage.tsx`
- `source/renderer/app/containers/wallet/WalletSummaryPage.tsx`
- `source/renderer/app/containers/wallet/WalletSendPage.tsx`
- `source/renderer/app/components/wallet/transactions/WalletTransactionsList.tsx`
- `source/renderer/app/components/wallet/transactions/Transaction.tsx`
- `source/renderer/app/components/wallet/transactions/Transaction.scss`
- `source/renderer/app/components/wallet/summary/WalletSummary.tsx`
- `source/renderer/app/components/wallet/WalletSendForm.tsx`
- `source/renderer/app/components/wallet/WalletSendForm.spec.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.spec.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-tokens-list/WalletTokensList.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-no-tokens/WalletNoTokens.tsx`
- the five storybook stories above
- the four translation artifacts, regenerated
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

One change, because the four computations and their consumers are one graph. The
order inside it is bottom-up so that each intermediate state is a compile error
rather than a silently defaulted prop: remove the prop from the leaf components
first, then from the components that pass it down, then the computations.

`hasTokens` in `WalletTokens.tsx` and `WalletTokensList.tsx` becomes
`assets.length`. `hasSearch` in `WalletTokensList.tsx` loses its first clause.
The spinner branch goes, leaving `noResults` and the list, so `LoadingSpinner` is
no longer imported there.

`WalletNoTokens` loses the prop and renders its token count unconditionally.

`Transaction.tsx` loses the placeholder branch, so the asset list is rendered
wherever `this.hasAssets` holds. The message and its stylesheet rule go with it.

## Acceptance Criteria

1. `jest`, `lint` and `compile` pass from `nix build`, and `stylelint`, `i18n`
   and `cucumber-unit` with them, because this commit touches a stylesheet and
   removes a message.
2. No spinner and no placeholder can appear on the token list, the wallet
   summary, the send form or the transaction list for a reason to do with asset
   metadata, at any point during a cold start.
3. `grep -rn isLoadingAssets source storybook tests` returns nothing.
4. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `compile` is the principal check. Every removal is of a declared prop on a
  typed component, so a consumer left behind fails there rather than at runtime.
  This is why the removal is bottom-up and in one commit: a partial removal does
  not compile, which is the property the task graph asserts.
- `grep` for criterion 3, over `source`, `storybook` and `tests`, plus a
  separate sweep for `hasRawAssets`, `totalRawAssets` and `totalAssets` to show
  the two that stay are the two that are read.
- `WalletTokens.spec.tsx`'s five existing cases are the regression guard for the
  surface that carried the spinner: they render the list with tokens and with
  none and assert what appears. They keep passing with the prop gone, which is
  what says the visible states did not move.
- `WalletSendForm.spec.tsx`'s existing cases cover the send form with the prop
  removed from both renders.
- `i18n` is a result rather than a guard here: it is what says the artifacts in
  this commit are the ones the source now produces, with the orphaned message
  gone from all four.
- `stylelint` because `Transaction.scss` loses a rule.
- `cucumber-unit` because `task-040`'s scenarios drive the send form.

## Risks and Open Questions

- The one judgement is `WalletNoTokens`. The task graph asks whether its prop is
  deliberately kept; the finding above is that its branch is unreachable from its
  only call site even before this change, so keeping it would preserve a guard
  that guards nothing. Removed, and the reason recorded here rather than left as
  a question.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-023.targetPaths` widened by the five
  storybook stories, `Transaction.scss` and `WalletTokens.spec.tsx`;
  `task-023.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-023-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-023-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

Nothing in the token surfaces waits for asset metadata, because nothing can.

## Final Outcome

Complete.

## Self-Review

The risk in a deletion this wide is removing a branch that was reachable for a
reason nobody wrote down. Each of the four conditions was read against the
function that feeds it rather than against the graph's description of it, and
the one prop whose removal is a judgement rather than a consequence,
`WalletNoTokens`, is argued from its only call site.
