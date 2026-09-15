Implementation: Iteration 1
Timestamp: 2026-09-15T02:56:20Z

Changes made:
- Added a `parameters` export to `storybook/preview.tsx` carrying
  `options.storySort.order`, a 27-entry nested array reproducing the group and panel order the
  barrel produced.
- Recorded four findings in the `task-011` entry and set its status to `completed`.

Files touched:
- `storybook/preview.tsx`
- the tasks JSON and the three `task-011` plan documents

Verification run:
- The order array was generated from the barrel reconstruction rather than transcribed, then pasted
  once. The reconstruction itself came from a depth-first evaluation of the barrel import graph at
  `5311ce0d0`: 14 groups, 49 titles.
- The decisive check ran the shipped comparator. `storySort` from the installed
  `@storybook/store/dist/cjs/storySort.js` was applied to the 258 registrations the `task-001`
  extractor reports, with the order array read directly out of `preview.tsx` rather than retyped
  into the test, so the value tested is the value shipped. The 49 titles come out in exactly the
  barrel-era sequence. First acceptance criterion met on its second limb, with the order stated.
- `nix build --no-link .#checks.x86_64-linux.storybook` exit 0. Second acceptance criterion met.
  This proves the parameter parses and the build survives it, which is a weaker statement than the
  comparator run and is why both were done.
- `nix build --no-link .#checks.x86_64-linux.compile` exit 0 on derivation
  `lkq92rv2p1ac5hwh353sdv7dl3hynkp2-daedalus-compile.drv`, and `.lint` exit 0.
- `prettier --check storybook/preview.tsx` clean under the repository's own prettier 2.1.2, checked
  because this file gained 60 lines and prettier is not in the CI check set.
- The sidebar artifact is byte-identical to the previous capture: 258 registrations, 49 titles, 14
  groups, `UNREACHABLE 0`. Membership did not move, which is the invariant; order is what changed.

Recorded difference from the barrel era:

- Story order inside a panel is not pinned. `storySort.js:22-24` returns `0` for two stories sharing
  a title unless `includeNames` is set, and `sortStoriesV6` breaks that tie on `fileNameOrder`,
  which under the glob is `require.context` order. Seven panels are assembled from more than one
  file and hold 45 of the 258 registrations: `Wallets / Settings` (7 files),
  `Wallets / Add Wallet` (5), `Wallets / Transactions` (4), `News / Overlays` (3),
  `Wallets / Tokens` (3), `Navigation / Sidebar` (2), `Wallets / Summary` (2). Their stories now
  list in file path order. The remaining 42 panels come from one file each and are unaffected.
  Pinning the rest would need `includeNames: true` and an order array naming all 258 stories,
  maintained by hand on every addition.

Deviations from the approved plan:
- None.

User interaction is now required:
- No.

Outcome: Both acceptance criteria met, the sequence verified against the shipped comparator, and
three checks green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T03:00:45Z

Summary:
- Approved. The sidebar order is now a stated value rather than a side effect of import order, the
  stated value is the one users already knew, and the check that establishes it is the comparator
  the preview will actually run.

Blocking findings:
- None.

Non-blocking observations:
- Reading the order array out of `preview.tsx` for the test, rather than pasting a copy into the
  test, is the detail that makes the verification worth anything. A test carrying its own copy of
  the array proves the two copies agree and nothing about what ships.
- Recovering the barrel order instead of choosing one was the right instinct. Nothing in the task
  entry required it, an invented order would have been defensible on its own terms, and it would
  still have reshuffled a tree people navigate by memory.
- The plan was right that the `task-001` baseline cannot satisfy this criterion as worded, and right
  to say so rather than diff against an alphabetical sort and call it a match. That decision was
  taken in `task-001` for a good reason and this is the task that pays the cost of it.
- Quantifying the within-panel limitation at seven panels and 45 registrations is what makes it a
  finding rather than a caveat. It also sizes the alternative honestly: a 258-entry list maintained
  by hand is worse than file path order, and saying so is a judgment the next reader can check.
- `storybook:build` passing was reported as the weaker of the two signals rather than as the result.
  That distinction has held all through this phase and it is the reason the glob probe existed one
  task ago.

Approval bar:
- Met. `task-011` is complete. `task-012` is unblocked.

Decision: approved
