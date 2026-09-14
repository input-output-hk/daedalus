Planner: Iteration 1
Timestamp: 2026-09-14T17:46:10Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-062.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the 15 renames, the import rewrites they force, and the tracking entries the
  renames invalidate.
- Classified the task `agent_execution`, because both required checks reproduce here as flake
  checks over the same Nix-built `node_modules` CI uses.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 16 at
  `:338-366`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-062`, `task-010`,
  `task-004`, `task-006`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md:461-476` and
  `research/04-tooling-alternatives.md:43`.
- `.agent/plans/storybook-modernization/task-plans/readme.md` for the cycle and section list, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `.agent/workflows/storybook.md` read for the registration model only, per the readme's caution
  that it teaches `storiesOf` and knobs and is rewritten by `task-060`.
- `.agent/skills/storybook-creation/SKILL.md` deliberately not followed; `:26` asserts the corpus
  uses `storiesOf()` rather than CSF, which is true and irrelevant to a rename.

Repo-Verified Findings Used To Shape The Plan:
- Classified all 84 convention-matching files by whether they contain `storiesOf(` or a line
  beginning `export default`: 69 register through `storiesOf`, 14 have neither, and 1 has a default
  export that is not a meta object. The register-nothing set is exactly the 15 in `targetPaths`, and
  no sixteenth file has drifted into the shape since the PRD was written.
- Confirmed the one default export is
  `storybook/stories/wallets/settings/WalletSettingsScreen.stories.tsx:139`, an anonymous React
  component with no `title`.
- Confirmed the 6.4.22 auto-title path the PRD cites is present in the installed tree:
  `node_modules/@storybook/client-api/dist/cjs/StoryStoreFacade.js:206`, `:211` and `:215`, package
  version `6.4.22`.
- Enumerated every importer of the 15. Seven files, fifteen specifiers, all static ES imports; none
  of the 15 contains `require(` or a dynamic `import(`, so the set is complete.
- Established that `storybook/stories/loading/_support/` is existing precedent for a per-directory
  `_support/` beside the story files it serves.
- Confirmed `tsconfig.json` declares no `include`, so every `.ts` and `.tsx` file is in the
  `tsc --noEmit` program and a dangling importer cannot hide.
- Confirmed `.eslintrc` has no rule keyed on a story filename glob and `.prettierignore` selects by
  extension, so neither tool changes behaviour when a file leaves the convention.
- Confirmed `perSystem/checks.nix:16-31` copies a prebuilt `node_modules` into the sandbox and
  `:53`, `:79` are the `compile` and `storybook` checks, so the flake checks reproduce CI exactly.
- Recorded that the host `yarn compile` is red for four pre-existing errors at
  `source/renderer/app/utils/crypto.ts:107`, `:109` and
  `source/renderer/app/utils/dataSerialization.ts:309`, that they reproduce identically in the
  untouched `master` clone, and that they trace to duplicate `@types/node` copies in the locally
  installed tree.

Planned Approach:
- `git mv` each file into a `_support/` directory beside its current location, dropping the
  `.stories` infix.
- Add one directory level to every relative specifier inside the moved files, with the
  `_support/utils` specifiers stated explicitly rather than derived.
- Rewrite the fifteen specifiers across the seven importers.
- Prove the sidebar did not move by extracting `storiesOf()` panel titles and `.add()` labels before
  and after, sorted and path-free.
- Verify with the flake checks rather than the host toolchain.

Scope Guard / Self-Review:
- No CSF conversion, no default exports added, no knob work, no glob.
- No deletion, including `Epochs`, which `task-004` owns.
- No edit to the PRD or the research notes.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T17:49:02Z

Blocking gaps:
- The plan runs flake checks but never says the renames have to be staged first. `nix build` on a
  git source sees tracked content only. A check run against an unstaged tree builds the pre-change
  source and passes for the wrong reason, which is the single most likely way this task reports a
  green result it did not earn. The precondition has to be written down, not assumed from `git mv`
  happening to stage both sides.
- The plan repeats the task entry's acceptance wording, byte-identical to the `task-001` baseline,
  without noticing that `task-001` is `pending` and no baseline artifact exists. Restating a
  criterion that cannot be evaluated is exactly the PRD-gone-stale failure the task-plans readme
  warns about. The plan has to say the baseline does not exist and name what it substitutes.
- No failure response. The plan says what to run and what a pass looks like, and nothing about what
  a differing extraction or a red check means, so an implementer hitting either has no instruction
  beyond improvising.

Non-blocking observations:
- The commit shape is unstated. A rename and the import it forces are not separable into two green
  commits, and that is worth one sentence rather than being rediscovered.
- Rollback is trivial here and saying so costs a line.
- The `_support` name is used for two different things on some paths. The plan already notes this
  and correctly points at `loading/_support/` as precedent, so it stays an observation.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T17:52:40Z

Plan Updates In Response To Critique:
- Added a staging step to the implementation approach, ahead of both flake checks, with the
  precondition stated as 15 `R` entries and no untracked file under `storybook/` in
  `git status --short`. Added the same precondition to the verification plan.
- Added a paragraph under the acceptance criteria recording that `task-001` is still `pending`, that
  no baseline artifact exists to diff against, that the substitute is a before-and-after extraction
  scoped to this change alone, and that the `task-001` baseline supersedes it when it lands.
- Added the failure response for both checks: a differing extraction means a registering file was
  edited and the edit is reverted rather than the sidebar accepted; a red check means a specifier
  gained the wrong number of levels.
- Stated the commit shape and why the change cannot be split into two green commits.
- Added rollback to the risk register.

Resulting Approved Plan Shape:
- 15 `git mv` operations into per-directory `_support/`, dropping `.stories`.
- One added directory level on every relative specifier inside the moved files, with the two
  `_support/utils` cases stated explicitly.
- Fifteen specifier rewrites across seven importers.
- Staged, then verified with `nix build .#checks.x86_64-linux.{compile,storybook,lint}` and a
  path-free panel-and-label extraction diffed across the change.
- Two `targetPaths` corrections in the tasks JSON, plus the `task-062` status.

Scope Guard / Self-Review:
- The revision closes the three blocking gaps and adds nothing to the change itself.
- The verification of record is still the flake check, and the host run is still reported as what it
  is rather than as a pass.
- Scope is unchanged: renames, forced import rewrites, tracking.

Outcome: Canonical task plan revised after critique and approved for build execution
