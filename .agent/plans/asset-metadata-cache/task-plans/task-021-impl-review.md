Implementation: Iteration 1
Timestamp: 2026-09-15T13:40:00Z

Changes made:
- `source/common/config/electron-store.config.ts` and
  `source/common/types/electron-store.types.ts`: one key,
  `DECIMAL-PLACES-NOTICE`.
- `source/renderer/app/api/utils/localStorage.ts`: the get, set and unset trio,
  in the shape `getStakingInfoWasOpen` and its siblings already use.
- `source/renderer/app/actions/profile-actions.ts`:
  `acknowledgeDecimalPlacesNotice`.
- `source/renderer/app/stores/ProfileStore.ts`: the observable, the startup read
  with the fresh-profile rule, and the dismissal.
- `source/renderer/app/stores/ProfileStore.spec.ts`: new. Five cases.
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.tsx`
  and `.scss`: the banner.
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.spec.tsx`:
  new. Five cases.
- `source/renderer/app/containers/wallet/WalletTokensPage.tsx`: two props.
- the four translation artifacts, regenerated.

Files touched:
- the nine source files and two new specs above
- `.agent/plans/asset-metadata-cache/task-plans/task-021.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-021-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-021-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail decided during implementation:

The component's prop defaults to acknowledged. A surface that renders
`WalletTokens` without being wired to the profile store therefore shows nothing,
rather than showing the banner on every render forever. The default is asserted
by its own case, because it is the behaviour a second call site would get by
accident.

One case from the plan was dropped rather than made to pass: a re-render asserting
the banner disappears when the profile's flag flips. `createTestBed` renders its
argument inside a wrapper it does not export, so `rerender` would have replaced
the provider tree rather than the component under it. What the case would have
shown is covered without it: the banner's presence is a pure function of two
props, each asserted on its own, and the persistence it was standing in for is
the store's and is asserted there.

Verification run:

- `jest .../wallet-tokens/WalletTokens .../stores/ProfileStore --coverage=false`
  — 10 passed, all new.
- The copy is asserted in the order the PRD requires rather than as one blob:
  the entering-an-amount change first and by example (`1.5 and not 1500000`),
  displayed balances second, and the user's own setting still overriding both.
  A rewrite that dropped one of the three fails on that clause.
- Criterion 2 is driven with an empty holdings list, which comes from the wallet
  object rather than the cache, so a profile with nothing to send sees nothing
  whatever the cache has resolved.
- Criterion 1 is asserted as a callback rather than as the banner hiding itself.
  Local state satisfies "shown once" for one session and fails the criterion that
  matters.
- Criterion 3 is driven as a restart: the flag is written, then a second store is
  built reading what the first wrote, and it shows nothing.
- Criterion 5 is the case named "is never shown to a profile being created now":
  with the terms of use not yet accepted, the flag is written without the banner
  ever being shown, so it does not turn up later once they are accepted.
- The startup default has its own case: before the read returns, the flag reads
  as acknowledged.
- Criterion 4 needs no case and is asserted by construction. `AssetInput` has no
  prop and no path to the banner's state, and the label cases in
  `AssetInput.spec.tsx` render it with no banner in the tree at all. The label is
  not conditional on anything this task adds.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `xvjyz7ay82ic8ycgygnpinfy3zciab6h-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `zhvywhbxjqkxra8zk7npdibfalagd11q-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `m394w7gd4yiglj7azm680qmli4rmamkg-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `5d7q4082da34zhc5gkn9jirrchwsccyy-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 88 suites passed, 1336
  tests with 1333 passed and 3 skipped, exit 0. The previous state of this branch
  was 86 suites and 1326 tests, so two suites and ten tests were added and
  nothing else moved.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` was run and changed three files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The dropped re-render case, for the reason above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T13:45:00Z

Acceptance criteria, each against the evidence:

1. *Shown once, then never again after dismissal.* Met, across the component and
   the store: the component reports, the store writes, and a second store reads
   it back.

2. *Not shown to a profile holding no tokens.* Met, from the wallet's holdings.

3. *Survives a restart without reappearing.* Met, driven as a second store rather
   than argued from the presence of a storage call.

4. *The amount field states its unit with the banner present and dismissed.* Met
   by construction, argued rather than tested twice.

5. *Not shown on a fresh profile created after the update.* Met. The terms of use
   are the marker, and the reason they are the only one available is in the
   plan's findings: the update-completed key is written by the in-application
   updater alone, so it is absent for anyone who installed the new version
   directly.

6-7. *All six checks, suppressions, dependencies.* Met.

Two judgements worth naming.

**The flag starts acknowledged.** The alternative shows the banner for the
fraction of a second every storage read takes, at every start, to every user who
dismissed it months ago. Appearing a moment late costs nothing.

**The fresh-profile rule can be wrong only in the safe direction.** If a profile
reached the token list without ever having accepted the terms, it would be marked
as new and never shown the banner. Not showing a migration notice to someone with
no habit to correct costs nothing; showing one to someone who does is the case
this exists for, and that user has accepted the terms at some earlier version.

Stated plainly, because it decides how much weight the other two mitigations
carry: this is the weakest of the three. It is dismissible, it is shown once, and
a user who clicks it away without reading it is left with the unit label under
the amount field and the snapshot that clears the field when a denomination
moves. That ordering is why this task is last in the phase rather than first.

Decision: approved
