Planner: Iteration 1
Timestamp: 2026-09-15T14:30:15Z

Plan Summary:
- Created `task-021.md` with the 21 sections. Two story files.

Repo-Verified Findings Used To Shape The Plan:
- The last three empty-render sites are here, two of them visible only since the scan learned to
  follow a binding.
- Eleven of the fourteen `TS2339` are here.
- Nine stories spread the story context into support components that declare their props, and the
  seven `@ts-ignore` in the file sit on exactly those spreads.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T14:34:40Z

Blocking gaps:
- The plan claimed the `@ts-ignore` count would be unchanged, because that is what the acceptance
  criterion says. It will not be. Every one of the seven suppresses the mismatch between an untyped
  context spread and a declared prop type, and the change removes the spreads. Writing the criterion
  down and then quietly missing it is worse than saying in advance that it cannot hold and why.
- The entry describes nine dynamic registrations as hand work. They were inlined in the preparatory
  commit and the codemod took them. The plan should say so rather than leaving a reader to wonder
  which nine were meant.

Non-blocking observations:
- Seven imports in `RedeemItnRewards.stories.tsx` are unused and were unused the moment the file was
  created. Worth removing while the file is being rewritten, and worth recording as a defect in the
  split that created it.
- `isLoading` is required on `StakePoolsStory`. Passing it explicitly will read as a behaviour change
  in the diff and is not one.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T14:38:55Z

Plan Updates In Response To Critique:
- Said the `@ts-ignore` criterion cannot hold, and why, before running.
- Explained what happened to the nine dynamic registrations.
- Added the seven unused imports to the approach.
- Added the `isLoading` note to the risks.

Outcome: Canonical task plan revised after critique and approved for build execution
