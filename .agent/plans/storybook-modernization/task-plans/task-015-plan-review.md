Planner: Iteration 1
Timestamp: 2026-09-15T07:40:20Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-015.md` with the 21 sections.
- Scope held to the manifest, the lockfile, `main.ts` and `preview.tsx`.
- Classified `agent_execution`, on the strength of the host carrying the pinned yarn 1.22.21.

Repo-Verified Findings Used To Shape The Plan:
- Host yarn is 1.22.21, exactly the version `package.json` pins, so the regenerated lockfile is the
  pinned resolver's own output.
- The four packages to remove have no 8.x release under those names.
- `@storybook/addon-knobs@8.0.1` peers four packages Yarn 1 will not install on its own.
- The `config.resolve` merge fix from `task-013` is mandatory rather than cosmetic.
- `perSystem/checks.nix:78` runs `yarn storybook:build`, so the script name is load-bearing and the
  binary behind it is not.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T07:45:05Z

Blocking gaps:
- The plan opens a red window and does not say what closes it. "Red until task-025" is a duration,
  not a mechanism. The entry has to name which task removes the last `storiesOf` import and
  therefore which one makes `compile` green, separately from which ones make the result correct.
- The plan expects two checks to fail and does not say what an acceptable failure looks like. A red
  check is not self-explaining: `storybook` failing on configuration means this task is wrong, and
  `storybook` failing on the corpus means it is right, and the plan has to commit to that
  distinction before seeing the output rather than rationalising whatever appears.

Non-blocking observations:
- Regenerating the lockfile on the host is the one step depending on local tooling. Worth saying why
  it is acceptable and what arbitrates the result.
- `--ignore-scripts` should be justified against this repository's own warning about native builds
  here.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T07:49:40Z

Plan Updates In Response To Critique:
- Added a "The red window" section naming `task-017` as what makes `compile` green again and the
  tranches, `task-023` and `task-025` as what make the result correct, and stating that Hydra red on
  a draft pull request is the intended state.
- Added to the acceptance criteria what an acceptable failure looks like for each check, and stated
  that a configuration failure means the task is not done and is fixed before the commit lands.
- Added the lockfile-on-host reasoning and named the `node_modules` derivation as the arbiter.
- Justified `--ignore-scripts` against the repository's own guidance.

Outcome: Canonical task plan revised after critique and approved for build execution
