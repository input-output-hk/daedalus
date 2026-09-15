Planner: Iteration 1
Timestamp: 2026-09-15T13:35:20Z

Plan Summary:
- Created `task-020.md` with the 21 sections. Twenty-seven files, of which eleven change, plus five
  outside the tranche.

Repo-Verified Findings Used To Shape The Plan:
- Six of the seven remaining empty-render sites are here.
- `StoryLayout` needs `currentTheme`, has no default for it, and six call sites supply it only
  through a context spread that the prop pass-through fills.
- `WalletSettings` spreads the whole story context into a component declaring one prop.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T13:39:45Z

Blocking gaps:
- The plan first scoped the `StoryLayout` fix to the two sites under `wallets/`. That leaves four
  identical sites in three other tranches, and the consequence is worse than untidiness: after
  `task-024` its own check would pass, because no story file reads a global from props, while four
  layouts silently render with an undefined theme. The plan has to either take all six or say why
  the state in between is safe, and it is not.
- Nothing said how the removal of the six side-effect imports would be checked. If the glob does not
  reach those siblings, stories disappear and the build stays green.

Non-blocking observations:
- `localeOf` returning `string` will not satisfy `WalletSettingsScreen`, which declares `Locale`.
  Better to narrow the helper than to cast at the call site.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T13:44:10Z

Plan Updates In Response To Critique:
- Took all six `StoryLayout` sites and added a section saying why the sweep crosses tranches.
- Named the label set as the check on the side-effect imports.
- Added the `localeOf` return type to the approach.

Outcome: Canonical task plan revised after critique and approved for build execution
