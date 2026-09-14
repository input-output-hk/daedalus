Planner: Iteration 1
Timestamp: 2026-09-14T20:44:10Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-008.md` with the 21 sections the
  task-plans readme requires.
- Scope held to one line of `tsconfig.json`.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 11 at
  `:268-272`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-008`.
- `.agent/plans/storybook-modernization/task-plans/readme.md` for the cycle and section list, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `AGENTS.md`, `CLAUDE.md` and `CHANGELOG.md:71` for the decorator and class-field history.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed `tsconfig.json:24` and its trailing comment.
- Found three SWC configurations setting the equivalent to `false`, not the two the task entry
  names: `storybook/main.ts:87`, `jest.config.js:195` and `source/renderer/webpack.config.js:52`.
  The third is the renderer build the application ships.
- Confirmed `tsconfig.json:14` targets `es2019`, below the `ES2022` threshold at which TypeScript's
  own default for the option becomes `true`.
- Confirmed the pre-change `compile` check is green and recorded its derivation path.
- Confirmed `.prettierignore` excludes top-level JSON other than `package.json`, so the edit cannot
  be reflowed by a formatter.

Planned Approach:
- Flip the value, leave the comment, confirm the diff is one line, run the three checks.

Scope Guard / Self-Review:
- No decorator setting, no bundler configuration, no MobX change.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T20:47:35Z

Blocking gaps:
- The task entry asks for the error count before and after and says a difference is a finding for
  the commit body. The plan says it will run the flake check, which reports a build result, not a
  count. Those are not the same instrument and the plan has to say how it reconciles them, or the
  acceptance criterion is being answered with the wrong measurement.
- The task entry says the commit touches exactly one line of one file, and every task in this phase
  commits a plan, two review logs and a status update alongside its change. The plan cannot satisfy
  that criterion as literally worded and does not say so. Stating the reading, and why `.agent/`
  cannot affect a check, is a sentence; leaving it implicit invites the next reader to conclude the
  criterion was missed.

Non-blocking observations:
- The trailing comment describes the option rather than the value and reads oddly beside `false`.
  Leaving it is right in a one-line commit, but the plan should say it was noticed.
- The direction of the change is worth stating. `true` is the setting that produces class-field
  errors, so a red check after this change would not be about class fields, and that shapes the
  failure response.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T20:51:20Z

Plan Updates In Response To Critique:
- Added the finding that the `compile` check stops at the first error, so "zero errors" and "green"
  are the same statement in this environment, and stated the before count as zero on that basis.
- Added a paragraph under Files Expected To Change giving the reading of the one-line criterion and
  the four reasons a file under `.agent/` cannot affect a check.
- Added the trailing comment to Risks as noticed and deliberately left.
- Added the direction-of-change reasoning to the failure response in the verification plan.

Resulting Approved Plan Shape:
- One line of `tsconfig.json`, three flake checks, a derivation-path comparison, and one tracking
  update.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the change.
- Scope is unchanged: one line.

Outcome: Canonical task plan revised after critique and approved for build execution
