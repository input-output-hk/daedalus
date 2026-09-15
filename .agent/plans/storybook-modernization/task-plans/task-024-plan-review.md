Planner: Iteration 1
Timestamp: 2026-09-15T16:35:15Z

Plan Summary:
- Created `task-024.md` with the 21 sections. Three files change.

Repo-Verified Findings Used To Shape The Plan:
- The required grep returns nothing in any story file.
- Seven spreads remain, all into components that are separately given what they need.
- `themeId` and the `themesIds` import exist only to feed the pass-through.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T16:38:40Z

Blocking gaps:
- The plan leant on a green `compile` as the evidence that nothing lost a prop. The `task-009`
  census says exactly why that is not enough: a story forwarding its whole argument by spread
  compiles and lints and fails only by rendering a component with props missing. What makes the
  green meaningful here is that the tranches replaced every spread with declared props, and that
  claim needs demonstrating rather than asserting.

Non-blocking observations:
- Two suppressions describe a `({ locale })` signature that no longer exists anywhere. Worth
  removing while the file is open, and worth checking they suppress nothing rather than assuming.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T16:41:55Z

Plan Updates In Response To Critique:
- Added a probe: remove one explicit `currentTheme`, require `compile` to go red, restore it. A
  green check is only evidence if the check can go red.
- Added the two suppressions to the file list and the approach.

Outcome: Canonical task plan revised after critique and approved for build execution
