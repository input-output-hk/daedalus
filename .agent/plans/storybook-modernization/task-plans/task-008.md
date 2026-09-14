# Task task-008: Set useDefineForClassFields to false in tsconfig.json

## Task ID and Title

- ID: `task-008`
- Title: `Set useDefineForClassFields to false in tsconfig.json`

## Why Chosen Now

`task-008.dependencies` is `[]` and no task depends on it. The task entry calls it adjacent to this
work rather than part of it, and locked decision 11 requires it to land as its own commit precisely
so that anything it disturbs is attributable to it and to nothing else.

It is taken now because it is one of the four tasks in this phase that can be completed in this
environment. `task-002`, `task-003`, `task-004` and `task-007` each remove a file from the working
tree, and file removal is refused here.

## Interaction Mode

- Mode: `agent_execution`

The change is one line and its verification is `yarn compile`, which reproduces here as
`nix build .#checks.x86_64-linux.compile` over the CI `node_modules`.

## Scope

- Set `useDefineForClassFields` to `false` at `tsconfig.json:24`.

## Non-Goals

- No change to `tsconfig.json:17` `experimentalDecorators` or `:18` `emitDecoratorMetadata`.
- No change to `storybook/main.ts:79`, `:84` or `:87`, or to the matching settings in
  `jest.config.js` and `source/renderer/webpack.config.js`. All three bundlers already carry the
  value this change adopts.
- No MobX change of any kind. This makes the type checker agree with the runtimes; it does not
  begin a MobX 6 migration.
- No change to the comment text on the line, beyond the value.

## Dependencies

- None. `task-008.dependencies` is `[]`, and no task lists `task-008`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 11 at
  `:268-272`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-008`
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md`, for the repository's MobX and decorator conventions
  - `CHANGELOG.md:71`, which records the Electron 24 to 41 upgrade as the change that introduced the
    SWC `legacyDecorator` and `useDefineForClassFields` settings
- Workflows:
  - None applicable.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `1adbcde72`, 2026-09-14, against the working tree.

- `tsconfig.json:24` reads `"useDefineForClassFields": true,` with the trailing comment
  `/* Emit ECMAScript-standard-compliant class fields. */`. The task entry's line reference is
  accurate.
- Three bundler configurations set the SWC equivalent to `false`, not the two the task entry names:
  - `storybook/main.ts:87`, in the handwritten `swc-loader` rule in `webpackFinal`
  - `jest.config.js:195`
  - `source/renderer/webpack.config.js:52`

  The third is the renderer build the application actually ships, which makes the case stronger than
  the task entry states: every runtime that executes this code uses `false`, and only the type
  checker uses `true`.
- Each of the three sits beside the same pair of comments, that MobX 5 uses legacy decorators and
  that class fields must use assignment so MobX prototype setters can intercept them during
  initialization. `storybook/main.ts:85-86` is one instance.
- `source/renderer/app/index.tsx:27` carries a comment that states the same dependency in prose,
  that a write goes through MobX's setter because `useDefineForClassFields: false` is set in the SWC
  configuration.
- `tsconfig.json:14` sets `"target": "es2019"`. TypeScript's own default for
  `useDefineForClassFields` is `true` only from `ES2022`, and `false` below it, so this change
  restores the compiler default for the configured target rather than opting into anything.
- `tsconfig.json` declares no `include` and excludes only `node_modules`, so the setting applies to
  every `.ts` and `.tsx` file in the tree. There is no subset of the program that keeps the old
  semantics.
- `nix build --no-link .#checks.x86_64-linux.compile` at `1adbcde72` exits 0, so the error count
  before the change is zero. The check fails the build on the first error, so "zero errors" and
  "green" are the same statement here.
- The `compile` derivation at `1adbcde72` is `s5mhd4b761b2lbl154ypx95x41348b6w-daedalus-compile.drv`.
  `tsconfig.json` is inside `srcWithoutNix`, so this change must move it.
- `.prettierignore` ignores every top-level JSON file except `package.json`, so `tsconfig.json` is
  not formatted by prettier and the edit cannot be reflowed.

## Files Expected To Change

- `tsconfig.json`, one line

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-008.status`
- `.agent/plans/storybook-modernization/task-plans/task-008.md`
- `.agent/plans/storybook-modernization/task-plans/task-008-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-008-impl-review.md`

The task entry's acceptance says the commit touches exactly one line of one file. Read literally
that excludes this plan and the status update, which every task in this phase commits alongside its
change. It is read as a statement about the change surface: one line of one file under version
control outside `.agent/`, which is excluded from `srcWithoutNix`, from `yarn lint`, from prettier
and from treefmt and therefore cannot affect any check. Nothing else in `source/`, `storybook/`,
`nix/`, `perSystem/` or the repository root moves.

## Implementation Approach

1. Change `true` to `false` on `tsconfig.json:24`, leaving the trailing comment as it is. The
   comment describes what `true` does, and it stays accurate as a description of the option rather
   than of the value; rewriting it would put a second change in a commit whose whole point is to
   carry one.
2. Record the error count before and after. Before is zero, from a green `compile` check at
   `1adbcde72`. After must also be zero.
3. Confirm the diff is a single line.
4. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- `yarn compile` passes with no new errors. Evidence:
  `nix build .#checks.x86_64-linux.compile` succeeds on a derivation that moved, and the error count
  is zero on both sides because the check stops at the first error.
- The commit touches exactly one line of one file. Evidence: `git diff --numstat` reports
  `1 1 tsconfig.json` and nothing else outside `.agent/`.

Added for this plan:

- `yarn lint` and `yarn storybook:build` also pass. Neither is asked for and neither reads
  `tsconfig.json`, but the phase constraint is that the trunk never carries a red
  `storybook:build`, and confirming costs one command each.

## Verification Plan

Already run for planning:

- The three SWC settings, read at `storybook/main.ts:87`, `jest.config.js:195` and
  `source/renderer/webpack.config.js:52`.
- `tsconfig.json:14` target, which determines what the compiler default would be.
- The pre-change `compile` check and its derivation path.

To run for the build:

- `nix build --no-link .#checks.x86_64-linux.compile`
- `nix build --no-link .#checks.x86_64-linux.lint`
- `nix build --no-link .#checks.x86_64-linux.storybook`
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a path other than
  `s5mhd4b761b2lbl154ypx95x41348b6w-daedalus-compile.drv`.
- `git diff --numstat`, expecting one insertion and one deletion in `tsconfig.json`.

If `compile` goes red, the error list is the finding and it belongs in the commit body, as the task
entry says. The direction of the change makes that unlikely: `useDefineForClassFields: true` is what
produces the class-field errors, `TS2612` among them, and `false` can only remove them. A red check
here would therefore mean something other than class-field semantics moved, and the right response
is to report it rather than to absorb it into this commit.

## Risks and Open Questions

- The change makes the type checker agree with three bundlers. If a fourth build path exists that
  was relying on the `true` semantics, it is not in this repository: the grep for
  `useDefineForClassFields` returns the three configurations, `tsconfig.json`, one prose comment and
  one changelog entry, and nothing else.
- The trailing comment on the line describes the option rather than the value, so it reads a little
  oddly beside `false`. Changing it would mean editing a second thing in a commit that is
  deliberately one line. It is left, and this is the record that it was noticed rather than missed.
- This matters more during a MobX 6 migration than it does here, which the task entry says and which
  this plan does not attempt to improve on.
- Rollback is `git revert` of a single commit.

## Required Docs, Research, and Tracking Updates

- Set `task-008.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- No PRD change. Locked decision 11 describes the change and remains accurate. Its claim that two
  places set the SWC equivalent is one short of the three that do; the third strengthens the same
  argument rather than changing it, and the PRD's Status Log is append-only, so the finding is
  recorded here rather than by editing it.
- No research-note change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-008-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-008-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `tsconfig.json:24` reads `"useDefineForClassFields": false,`.
- `git diff --numstat` reports one insertion and one deletion in `tsconfig.json` and nothing else
  outside `.agent/`.

## Final Outcome

- `task-008` completed. The type checker now checks against the class-field semantics all three
  bundlers already use.
- The error count is zero before and zero after. `nix build --no-link .#checks.x86_64-linux.compile`
  exits 0 on both sides, and the derivation moved from
  `s5mhd4b761b2lbl154ypx95x41348b6w-daedalus-compile.drv` to
  `09fldjchx059x42zin99x8v1dvdq0hwk-daedalus-compile.drv`, so the second result is about the changed
  tree. There is no finding for the commit body.
- `nix build --no-link .#checks.x86_64-linux.lint` and `.storybook` also exit 0, with 5483 lint
  warnings, the count the corpus already carried.

## Self-Review

- The plan found a third SWC configuration the task entry does not name, and it is the renderer
  build the application ships, which is the one that most matters to the argument.
- The plan states what "zero error count difference" means in an environment where the check stops
  at the first error, rather than quoting a count it cannot produce.
- The plan says how it reads the one-line acceptance criterion against a cycle that commits plan
  documents, rather than letting the discrepancy sit unremarked.
- The odd trailing comment is recorded as noticed and deliberately left, so the next reader does not
  file it as an oversight.
