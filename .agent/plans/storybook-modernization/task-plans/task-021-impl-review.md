Implementation: Iteration 1
Timestamp: 2026-09-15T14:52:20Z

Changes made:
- `staking/Staking.stories.tsx`: every story that spread the story context into a support component
  now passes the props that component declares, taken from `_support/globals.ts`. That is nine
  stories across `StakingDelegationCenterStory`, `StakePoolsStory`, `StakePoolsTableStory`,
  `StakingDelegationSteps`, `StakingUndelegateConfirmationStory` and
  `StakingUndelegateConfirmationResultStory`. `PoolsIndex`, `StakePoolsList` and `Rewards` become
  objects with `render` and `parameters` rather than bindings with properties attached.
- `staking/RedeemItnRewards.stories.tsx`: all six stories take the CSF object form with `render`,
  `parameters` and, for two of them, `name`. Seven unused imports removed.

Verification:
- Empty-render burn-down 3 to 0. The scan reports `with no args declared: 0`. Every story in the
  corpus that reads its first argument now has args declared, and the three that do are
  `news/IncidentOverlay`'s.
- `compile` 14 to 3. The three left are `governance/DRepDirectory` twice and `governance/Delegation`
  once, all `task-022`'s.
- `lint` exit 0. `storybook` exit 0.
- Label set from `index.json`: 258 entries across 49 titles, identical to the pre-conversion set.

On the `@ts-ignore` criterion, which is not met: the tranche goes from 7 to 0, and the corpus from
124 to 117. Each of the seven is accounted for by the error it suppressed, and all seven sat on a
spread this change removes:

- four `ts-migrate(2739)`, "Type ... is missing the following properties", on
  `<StakingDelegationCenterStory {...props} ...>` and `<StakePoolsStory {...props} isLoading />`.
  The spread of an untyped context could not satisfy a declared prop type, and now the props are
  passed by name.
- two `ts-migrate(2769)`, "No overload matches this call", on the two `StakingDelegationSteps`
  spreads, for the same reason.
- one `ts-migrate(2345)`, "Argument of type ... is not assignable", on
  `UndelegateConfirmationResult`, which spread the context into a component declaring `{ locale }`.

None of the seven was suppressing a fact about the component. Each was suppressing the consequence
of handing a typed component an untyped object, which is the defect the tranche exists to remove.

One correction during the work:

- `RedeemItnRewards.stories.tsx` imported `React`, `withKnobs`, `StoryLayout`, `StoryProvider`,
  `StoryDecorator`, `CATEGORIES_BY_NAME` and `StakingWithNavigation`, and used none of them. Each
  name appears exactly once in the file, in its own import. They were copied across when the
  preparatory commit split the file, and nothing since has reported them: `tsc` does not flag an
  unused import under this configuration and the lint rule is off. Removed here.

Deviations from the approved plan:
- None.

Outcome: The last three empty renders are gone, eleven type errors with them, and the burn-down is
at zero; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T14:58:45Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- Declaring in advance that the `@ts-ignore` criterion could not hold, and why, is what makes the 7
  to 0 readable as a result rather than as a slip. The criterion was written for a tranche that
  renames things; this one changes what is passed.
- The seven are useful evidence about locked decision 8. They are not suppressions of an awkward
  truth about a component; they are scaffolding that existed only because an untyped object was
  being handed to a typed interface. Carrying all 229 across was the right default, and this is the
  first tranche where a meaningful number of them turned out to be scaffolding rather than debt.
- Seven unused imports surviving a file split, invisible to `tsc` and to `lint`, is a small version
  of the same theme. Nothing was watching, so nothing said.
- The burn-down reaching zero is worth stating plainly: the scan finds no story in the corpus that
  reads an argument nothing fills, and the three that read their first argument all declare args.

Approval bar:
- Met. `task-021` is complete.

Decision: approved
