Implementation: Iteration 1
Timestamp: 2026-09-15T14:55:00Z

Changes made:
- The four computations removed: `WalletTokensPage.tsx`, `WalletSummaryPage.tsx`,
  `WalletSendPage.tsx` and `WalletTransactionsList.tsx`. `hasRawAssets` kept at
  the two sites that read it for something else.
- The prop removed from every consumer: `WalletTokens.tsx`,
  `WalletTokensList.tsx`, `WalletSummary.tsx`, `WalletSendForm.tsx`,
  `Transaction.tsx` and `WalletNoTokens.tsx`.
- `WalletTokensList.tsx`: the spinner branch and the `LoadingSpinner` import.
- `Transaction.tsx`: the placeholder branch, and the `fetchingTokenData` message
  that had no other reader.
- `Transaction.scss`: the rule the placeholder used, and the `animations` import
  that rule was the only user of.
- `WalletSendForm.spec.tsx`, `WalletTokens.spec.tsx` and five storybook stories.
- the four translation artifacts, regenerated.

Files touched:
- the thirteen source, spec and stylesheet files above
- `storybook/stories/wallets/transactions/Transaction.stories.tsx`,
  `storybook/stories/wallets/summary/WalletSummary.stories.tsx`,
  `storybook/stories/wallets/tokens/WalletTokens.stories.tsx`,
  `storybook/stories/wallets/tokens/WalletTokensList.stories.tsx`,
  `storybook/stories/wallets/send/WalletSend.stories.tsx`
- `source/renderer/app/i18n/locales/{defaultMessages,en-US,ja-JP}.json` and
  `translations/messages.json`
- `.agent/plans/asset-metadata-cache/task-plans/task-023.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-023-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-023-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**`Transaction.scss` loses an import as well as a rule.** The placeholder was the
only `@include animated-ellipsis` in the file, and the mixin file declares one
mixin and nothing else, so the import had no remaining effect. Checked rather
than assumed: `grep -n "@include" source/renderer/app/components/wallet/transactions/Transaction.scss`
returns nothing after the rule is gone.

**`WalletTokens.tsx`'s `hasTokens` is now a boolean rather than a number.** It was
`assets.length || isLoadingAssets`, which is `0` for an empty list; with the
second operand gone it is written `assets.length > 0` rather than left as a
number in a `&&` position. `WalletTokensList.tsx` had the same expression and
reads it once, so there it is inlined as `if (!assets.length)`.

Verification run:

- The regenerated translation artifacts change by exactly one message in each of
  the four files and by nothing else, which is both the intended result and the
  evidence that the `i18n:manage` round trip is a no-op on an unchanged tree:
  `git diff --stat` over the four reports 12 deletions and 0 insertions.
- `grep -rn isLoadingAssets source storybook tests` returns nothing; exit code 1.
- `grep -rn "hasRawAssets\|totalRawAssets\|totalAssets" source storybook tests`
  returns four lines, all of them `hasRawAssets` at `WalletSummaryPage.tsx:126`
  and `:180` and `WalletSendPage.tsx:145` and `:165`, which are the two
  declarations and the two reads that are about holdings rather than about the
  cache.
- The test count is the regression guard for a change that adds no test: 88
  suites and 1336 tests, identical to the state of this branch before the change.
  A component whose rendered output moved would fail one of the six existing
  `WalletTokens` and `WalletTokensList` cases or one of the six snapshots.

Checks, all six through Nix with every change staged, plus `storybook` because
this is the first commit on the branch to touch a story:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `4ivl4ahylna5xb930vplivaghdf3drc3-daedalus-compile.drv`, exit 0. This is the
  principal check for this change: every removal is of a declared prop on a typed
  component, so a consumer left behind fails here.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `wrvnjgcdwx2zk4d25g45rhljldyn3dc8-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `plq27yqv51pqiq2zvhrhqjj3kprybh6z-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `m4qwxmfqij0rdvqr9jsrkrg77gmqxvfm-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 88 suites passed, 1336
  tests with 1333 passed and 3 skipped, 6 snapshots, exit 0.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.
- `nix build '.#checks.x86_64-linux.storybook' --no-link` — built
  `bspnw8nm4y7ii1qngpj36symnhbnfl82-daedalus-storybook-build.drv`, exit 0.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The storybook stories, the stylesheet and the orphaned message were all
  recorded in the plan before the work.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T15:02:00Z

Acceptance criteria, each against the evidence:

1. *Six checks green.* Met, and `storybook` run as a seventh because a story
   changed.

2. *No spinner and no placeholder for a metadata reason.* Met structurally, which
   is the only way it can be met: there is no longer an expression that could
   select one. The two spinners that remain are the restore spinner in
   `WalletTokens.tsx` and the transactions spinner, both fed by state that
   genuinely changes, and both named in the plan's Non-Goals.

3. *The grep sweep.* Met, and the complementary sweep for the three derived names
   is the more informative one: it shows the two survivors are the two that were
   never about the cache.

4. *No new suppressions, dependencies unchanged.* Met.

The judgement to weigh is `WalletNoTokens`, which the task graph left as a
question. Answering it from the call site rather than from the prop's name is
what makes the answer checkable: the component is reached only under
`if (!hasTokens)`, `hasTokens` included `isLoadingAssets`, so the guard inside it
could never have been true. Keeping the prop would have preserved a condition
that was already dead before this phase, on the grounds that it looked
deliberate.

The identical test count is worth naming rather than passing over. This commit
adds no test, and that is correct: it removes branches, and a test for a branch
that cannot be taken is a test for nothing. What stands in its place is the
existing suite continuing to pass unchanged, plus `compile`, which is what
actually finds a missed consumer.

Summary: A row is rendered from what the wallet holds, so there is no longer a
moment at which the list knows about a token and declines to show it. Eleven
readers of a permanently false flag are gone, along with the two surfaces they
selected and one message nobody can reach.

Decision: approved
