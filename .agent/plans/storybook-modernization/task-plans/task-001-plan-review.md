Planner: Iteration 1
Timestamp: 2026-09-14T18:31:40Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-001.md` with the 21 sections the
  task-plans readme requires.
- Scope held to capturing the registration corpus, committing the artifact and the tool that
  produces it, and correcting the two defects the task entry carries.
- Classified the task `agent_execution`. The task entry offers a browser walk and a static
  extraction as equal methods; the static one runs here and the browser one does not.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the Status Log entry of
  2026-09-14 at `:1171-1199` and locked decision 16 at `:338-366`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-001` and the
  acceptance criteria of `task-002`, `task-003`, `task-004` and `task-006`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/readme.md` for the cycle and section list,
  `task-062.md` for the immediately preceding state of the corpus, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `.agent/workflows/storybook.md` read for the registration model only, per the readme's caution.
- `.agent/skills/storybook-creation/SKILL.md` deliberately not followed.

Repo-Verified Findings Used To Shape The Plan:
- Extracted every registration with the TypeScript compiler API: 15 groups, 53 distinct `storiesOf`
  titles across 73 calls, 272 registrations, 270 of them reachable from the barrel.
- Cross-checked the total independently: `grep -rno '\.add(' storybook/stories` returns 272, five of
  which are not registrations, leaving 267, plus 5 colocated registrations under `source/`.
- Established that the task entry's raw-grep figure of 270 contradicts its own arithmetic and that
  the three `moment().add()` paths it names were moved into `_support/` by `task-062`.
- Established that 9 of the 272 labels are not string literals: 8 resolve through the `pageNames`
  map at `storybook/stories/staking/Staking.stories.tsx:41-51` and one is a template literal at
  `:111`. All nine resolve, so the baseline carries no placeholder.
- Established that 2 registrations are unreachable from the barrel, both in
  `storybook/stories/staking/Legacy.stories.tsx`, which is the task's second acceptance criterion.
- Confirmed `nix/internal/common.nix:269` excludes any directory named `.agent` from
  `srcWithoutNix`, and recorded the four check derivation paths at `bfc6ec3d2` so the claim can be
  tested rather than asserted.
- Confirmed all four relevant flake checks are green at `bfc6ec3d2`.

Planned Approach:
- Parse each file containing `storiesOf(`, attribute every `.add()` to its owning `storiesOf` title
  through the receiver chain, and resolve labels from literals, file-local constants and template
  literals, reporting anything else as unresolved.
- Walk the module graph from `storybook/stories/index.ts` to mark barrel reachability.
- Emit a sorted tree plus an index carrying `path:line`.
- Commit the artifact and the extractor under `task-plans/`.
- Correct the two defects in the `task-001` entry.

Scope Guard / Self-Review:
- No deletion, no story edit, no configuration change.
- No PRD edit: the PRD's figures reproduce.
- No attempt to make the extractor outlive phase 3.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T18:36:05Z

Blocking gaps:
- The plan commits a tool and then proposes to verify the baseline by re-running it, without saying
  how that tool finds a compiler. The worktree has no `node_modules`, because the flake checks build
  their own. A committed script that cannot run in the tree it is committed to is not reproducible,
  and "re-run it" is not an instruction anyone can follow. The plan has to state the resolution rule
  and the fact that this environment needs `NODE_PATH` to satisfy it.
- The plan offers "the four derivation paths are unchanged" as evidence that a documentation commit
  cannot move a check, and never says the files must be staged first. Unstaged, that comparison is
  guaranteed to succeed and proves nothing, because `nix build` on a git source cannot see an
  untracked file. This is the same trap `task-062` wrote down, pointing the other way, and it is
  worse here because the expected result is "no change", which is exactly what a broken measurement
  also reports.
- The plan never says what a later task actually diffs. It produces a two-section artifact and
  leaves the reader to guess whether the index section's `path:line` column is part of the
  comparison. It is not, because line numbers move for reasons that have nothing to do with the
  sidebar, and saying so is the difference between a baseline and a file that generates false
  failures from the second task onward.

Non-blocking observations:
- One count in the findings does not reproduce. The plan claims 37 `.add(` calls put the label on
  the following line; `grep -rn '\.add($'` over `storybook/stories` and `source` returns 35. The
  argument the number supports, that a regex cannot do this job, is unaffected, but a figure that
  does not reproduce in a section whose whole purpose is reproducible figures is the wrong kind of
  error to leave in.
- The plan is right that sidebar order is not captured and right about why, and the reasoning is
  worth keeping rather than trimming: a captured order would encode barrel import order, which the
  PRD has already settled on replacing.
- Committing a fourth and fifth file under `task-plans/` departs from the readme's three-file
  listing. The plan says so and gives the reason. It stays an observation.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T18:41:20Z

Plan Updates In Response To Critique:
- Added a finding recording that the worktree carries no `node_modules` and that the extractor
  resolves `typescript` by `require` from an installed tree, failing with a stated message rather
  than a stack trace. Added to the verification plan that the reproduction run is made with
  `NODE_PATH` pointed at an installed tree and that this is reported rather than left implicit.
- Added a staging step to the implementation approach, ahead of any flake check, with the reason
  written out: the evidence here is that derivations are unchanged, and unchanged derivations over
  an unstaged tree are what a broken measurement also produces. Added the same precondition to the
  verification plan.
- Added a paragraph to the implementation approach naming the tree section as the diff surface, the
  index section as provenance that is expected to move, and the rule that the baseline file is never
  edited to match a later change.
- Corrected the multi-line `.add(` figure from 37 to 35 and stated what was measured.

Resulting Approved Plan Shape:
- An AST extraction over every file containing `storiesOf(`, with receiver-chain attribution,
  constant and template resolution, and an explicit unresolved count.
- A module-graph walk from the barrel for reachability.
- One artifact carrying a sorted tree and a provenance index, plus the extractor that produces it,
  both committed under `task-plans/`.
- Verification by regenerating the artifact from the committed tool, by comparing the four check
  derivation paths across a staged commit, and by re-running the three checks.
- Two corrections to the `task-001` entry in the tasks JSON, plus its status.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the change itself.
- The verification of record is still the flake check, and the host `yarn compile` is still reported
  as what it is.
- Scope is unchanged: the capture, the tool, and the tracking corrections.

Outcome: Canonical task plan revised after critique and approved for build execution
