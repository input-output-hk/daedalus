Planner: Iteration 1
Timestamp: 2026-09-15T14:10:00Z

The first plan followed the task graph's `targetPaths` and its four named sites,
and stopped at `source/`.

Critique:

- **The scope is short by five files.** `package.json:45` is `tsc --noEmit` and
  `tsconfig.json` excludes only `node_modules`, so `storybook/` is compiled;
  `package.json:43` lints `source storybook utils`. Five stories pass
  `isLoadingAssets`, so a plan that stops at `source/` does not compile. The
  graph's `targetPaths` are wrong rather than the plan being over-wide, and the
  correction belongs in the graph.
- **Deleting the `Transaction.tsx` branch orphans a message and the plan did not
  say so.** `wallet.transaction.fetchingTokenData` has exactly one reader. The
  `i18n` check compares the tracked artifacts against what the source produces,
  so the regenerated locale files and `translations/messages.json` are part of
  this commit and not of `task-025`.
- **`hasRawAssets` was being removed at all four sites.** It is read at
  `WalletSummaryPage.tsx:184` and `WalletSendPage.tsx:168` for something else
  entirely, and it is derived from the wallet rather than from the cache. Two of
  the four go; two stay.
- **`WalletNoTokens` was left as an open question, which is what the graph asked
  for and is not good enough.** The question is answerable from the call site:
  the component is reached only when `!hasTokens`, and `hasTokens` includes
  `isLoadingAssets`, so its guard is already unreachable. A plan that leaves this
  to implementation leaves a prop whose fate is decided by whoever is typing.

What changed in response: the five storybook stories, `Transaction.scss` and
`WalletTokens.spec.tsx` added to the files expected to change and to the graph's
`targetPaths`; findings 3, 6, 7 and 8 added with their evidence; the acceptance
criteria widened from three checks to six, because a stylesheet and a message
both change; `WalletNoTokens` resolved in the plan rather than deferred.

Scope guard: no behaviour visible to a user changes. Every branch removed is one
whose condition is false at every site that computes it, and the two spinners
that stay — the restore spinner and the transactions spinner — are named in
Non-Goals so they are not swept up.

Outcome: approved
