Implementation: Iteration 1
Timestamp: 2026-09-15T01:14:30Z

Changes made:
- Removed `storybook/stories/staking/StakingWrapper.tsx`,
  `storybook/stories/wallets/_utils/HardwareWalletWithNavigationLayout.tsx`,
  `storybook/stories/wallets/_utils/defaultWalletProps.tsx` and the zero-byte
  `storybook/preview-head.html`.
- Removed `storybook-addon-swc` from `package.json` and its `storybook-addon-swc@1.1.7` block from
  `yarn.lock`.
- Recorded the re-verification result and the lockfile trade in the `task-007` entry, and set its
  status to `completed`.

Files touched:
- 4 removals
- `package.json`, `yarn.lock`
- the four `.agent/plans/storybook-modernization/` files

Verification run:
- The manifest and lockfile edits were gated before anything else.
  `nix build --no-link .#internal.x86_64-linux.node_modules` rebuilt in 3 minutes 24 seconds to
  `5dpfzm1vdaly00adgqd0k8milmmlgk62-daedalus-node_modules`. That derivation installs with
  `--frozen-lockfile` (`nix/internal/common.nix:315`) and takes its offline cache from `yarn.lock`
  (`:298`), so a manifest and lockfile that disagreed would have failed there rather than later.
  `package.json` also re-parses as JSON.
- The orphan walk after the removal reports 116 files under `storybook/`, 116 reachable, 0 orphaned,
  against 119, 116 and 3 before. The removal orphaned nothing further, which is what the absence of
  a cascade predicted. First acceptance criterion met, by reachability rather than by mention.
- The sidebar is byte-identical to the pre-change capture under `cmp`: 258 registrations, 49 titles,
  14 groups, `UNREACHABLE 0`. This is the criterion the plan added and the strongest one available:
  a change that removes only unreachable modules must move nothing.
- `nix build --no-link .#checks.x86_64-linux.storybook` exit 0, `.compile` exit 0 on derivation
  `dg7zrim49pnfimsngzy20z7cfjngbbjp-daedalus-compile.drv`, `.i18n` exit 0, and `.lint` exit 0 at
  5391 warnings against 5410 after `task-004`. Second acceptance criterion met.
- `git status --short`: four `D`, two `M`, nothing untracked outside `.agent/`.
- The near-miss guard held. `storybook/stories/wallets/_utils/HardwareWalletsWrapper.tsx` is present
  and untouched, along with `CreateWalletScreens.tsx`, `WalletWithNavigationLayout.tsx`,
  `WalletsTransactionsWrapper.ts` and `WalletsWrapper.tsx`. Five of seven, as planned.

Deviations from the approved plan:
- None. The removals were performed by the operator against the list this task produced.

User interaction is now required:
- No.

Outcome: Both acceptance criteria met, the sidebar is unchanged byte for byte, the orphan count is
zero, and four checks are green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T01:18:50Z

Summary:
- Approved. Four files removed, a dependency dropped from both the manifest and the lockfile, and
  the sidebar provably unmoved. Phase 1 is complete.

Blocking findings:
- None.

Non-blocking observations:
- Gating the lockfile edit on the `node_modules` build is the part of this task that could have gone
  wrong quietly. A `yarn.lock` diff is not readable in the sense that matters; the question is
  whether yarn still resolves the tree under `--frozen-lockfile`, and only the derivation that runs
  the install answers it. Running that first, before any check, is the right order.
- Leaving two unrequested ranges rather than hand-pruning fourteen blocks is the correct trade and
  is stated in the commit body as well as the entry, so a reviewer who finds `swc-loader@^0.1.15`
  still present reads a decision rather than an oversight.
- A byte-identical sidebar is a better result than a passing check here. Every other task in this
  phase had to explain what moved; this one had to show that nothing did, and `cmp` says so.
- The null re-verification is worth the words it took. `task-007` depends on `task-002` and
  `task-003` for exactly this, and the only way to know three deletions orphaned nothing further was
  to walk the graph again rather than re-read the list.
- Measuring reachability rather than mention is the right instrument in this repository
  specifically. `Legacy.stories.tsx` sat in the corpus for years being mentioned by nothing and
  loaded by nothing while every check stayed green, and `task-003` removed it one commit ago.

Approval bar:
- Met. `task-007` is complete, phase 1 is complete, and `task-010` is unblocked.

Decision: approved
