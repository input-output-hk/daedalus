Implementation: Iteration 1
Timestamp: 2026-09-14T22:41:30Z

Changes made:
- Removed the four story files documenting screens no affordance reaches:
  `wallets/paperWallets/PaperWallets.stories.tsx` (5 registrations),
  `wallets/legacyWallets/TransferFunds.stories.tsx` (2),
  `wallets/legacyWallets/LegacyNotification.stories.tsx` (1) and
  `staking/CountdownParty.stories.tsx` (1). Both `paperWallets/` and `legacyWallets/` are emptied
  and so absent from the tree.
- Removed three registrations from `storybook/stories/staking/Staking.stories.tsx`: the whole
  `Decentralization / Countdown` panel, and `Info` and `Info Countdown`.
- Removed what only those three read: the `StakingCountdown`, `StakingInfo` and
  `StakingInfoCountdown` imports, `defaultPercentage`, `defaultStartDateTime`, the
  `startDateTimeKnob` helper, the `date` binding in the knobs import, and the `countdown`, `info` and
  `info-countdown` entries in `pageNames`. Simplified the two decorator branches that only the
  countdown story reached.
- Removed the four barrel entries from `storybook/stories/wallets/index.ts` and
  `storybook/stories/index.ts`.
- Corrected the knob figure in the `task-002` entry from 13 to 19 with the breakdown, and set
  `task-002.status` to `completed`.

Files touched:
- 4 removals under `storybook/stories/`
- `storybook/stories/staking/Staking.stories.tsx`
- `storybook/stories/wallets/index.ts`
- `storybook/stories/index.ts`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`
- `.agent/plans/storybook-modernization/task-plans/task-002.md`
- `.agent/plans/storybook-modernization/task-plans/task-002-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-002-impl-review.md`

Verification run:
- `git status --short` before any check: four `D` entries, three `M` entries, and nothing untracked
  under `storybook/`. That precondition matters here in both directions, because an unstaged removal
  is invisible to the flake source and a barrel pointing at a file the build can still see would
  have passed for the wrong reason.
- Sidebar regenerated with the `task-001` extractor and diffed against the baseline. Removed, and
  nothing else: `Countdown party` and `Decentralization Countdown` under
  `Decentralization / Countdown`; `Info` and `Info Countdown` under `Decentralization / Staking`,
  taking that panel from 16 to 14; `Legacy Notification`, `Transfer Funds - Step1` and
  `Transfer Funds - Step2` under `Wallets / Legacy Wallets`; and all five under
  `Wallets / Paper Wallets`. Twelve registrations, and three panels gone because every registration
  they held was among the twelve. The one other line of difference against the baseline is the
  addition `task-006` made and recorded. Second acceptance criterion met on both readings.
- Counts: `REGISTRATIONS 261` and `PANELS 50` after, against 273 and 53 before this task, and 272
  and 53 in the baseline.
- `nix build --no-link .#checks.x86_64-linux.storybook` exit 0. This is the check that tests the
  barrel edits: `compile` does not report an unresolvable relative import in this repository, which
  was measured during `task-006`, so a barrel entry left pointing at a removed file would have
  passed `compile` and failed here.
- `nix build --no-link .#checks.x86_64-linux.compile` exit 0, on derivation
  `p9d3hpssnxqpdspkrm63k8vh1nisl6cw-daedalus-compile.drv`, which is not the
  `09fldjchx059x42zin99x8v1dvdq0hwk` of the previous commit.
- `nix build --no-link -L .#checks.x86_64-linux.lint` exit 0, 5468 warnings against 5483 before.
  The 15 that went were in the deleted files. First acceptance criterion met.
- `git diff --name-only` and `git diff --cached --name-only` together list seven paths, all under
  `storybook/stories/`. No file under `source/renderer/app/components` or
  `source/renderer/app/config` is modified. Third acceptance criterion met.
- A grep for the four deleted basenames across `source/`, `storybook/` and `tests/` returns nothing.
- Knob measurement, cross-checked against the PRD: the corpus held 396 call sites before this task
  and holds 377 after. 396 is the whole-corpus figure the PRD's Status Log of 2026-09-14 records, so
  the 19 that left reconcile exactly rather than approximately.

Deviations from the approved plan:
- None in the change. The file removals were performed by the operator against the list this task
  produced, which is how the plan's step 3 specifies it.

User interaction is now required:
- No.

Outcome: All three acceptance criteria met, the sidebar diff is the intended twelve removals, and
the three checks are green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-14T22:46:10Z

Summary:
- Approved. Twelve registrations removed and twelve accounted for, three panels named as casualties
  rather than left to be discovered, and no file under `source/` touched.

Blocking findings:
- None.

Non-blocking observations:
- Reporting the sidebar both ways is what keeps the baseline usable. A bare statement that the diff
  showed 13 differences would have read as a violation; a bare statement that it showed 12 removals
  would have been incomplete. Both readings are now on the record with the reason they differ.
- The knob correction earned the detour. The task entry said 13, the measurement says 19, and the
  independent check is that the corpus goes 396 to 377 against the PRD's own 396. A figure that
  reconciles with a number derived somewhere else is worth far more than a recount of the same
  files.
- Naming the three vanished panels in the commit body is the right call. A panel disappearing is the
  most visible thing this change does and the only part a registration count does not explain.
- The decorator simplification was argued before it was made and remains the one judgment call here.
  It stands: a test against `id: 'countdown'` in a file where no registration carries that id is a
  false statement, `no-unused-vars` is a warning in this repository and would not have caught the
  imports either, and nothing in the check set looks at a dead branch.
- The `_utils` question was correctly deferred. `TransferFunds.stories.tsx` imported
  `WalletsWrapper`, which three surviving files still import, and `task-007` exists to re-verify the
  whole directory after this task and `task-003`.

Approval bar:
- Met. `task-002` is complete. `task-003` and `task-007` are unblocked on this dependency.

Decision: approved
