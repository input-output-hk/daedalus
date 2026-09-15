# Task task-013: Pre-flight Storybook 8.6.x against React 16.14.0 in a scratch worktree

## Task ID and Title

- ID: `task-013`
- Title: `Pre-flight Storybook 8.6.x against React 16.14.0 in a scratch worktree`

## Why Chosen Now

`task-013.dependencies` is `[task-012]`, which is complete. It is the first task of phase 3 and it
gates `task-014`, which gates the hop branch.

It exists because phase 3 is one landing that cannot be decomposed. `tsconfig.json` declares no
`include` and excludes only `node_modules`, so every story file is in the `tsc --noEmit` program,
and a single file still importing `storiesOf` from `@storybook/react` fails `yarn compile` the
moment the manifest moves, whether or not the indexer sees that file. The branch is therefore red
from `task-015` until the conversion completes. Everything this pre-flight finds is something that
would otherwise be found inside that window.

It carries a kill criterion: if `react-polymorph` misbehaves under the 8.6.x preview runtime, the
answer is to stop and re-plan rather than to open the branch.

## Interaction Mode

- Mode: `agent_execution`

The npm registry is reachable from here, which is what makes this executable at all:
`registry.npmjs.org` answers `200`. What cannot be done is opening a browser, so "renders" is
established through Storybook 8's own portable-stories API under jsdom rather than by looking at a
preview. That substitution is argued under Verification Plan rather than assumed.

## Scope

- Stand up Storybook 8.6.x with `@storybook/react-webpack5` in a scratch project outside the
  repository, carrying this repository's `webpackFinal` body and all four legacy-decorator settings.
- Render, through the 8.6.x story pipeline: `react-polymorph` via `StoryDecorator`'s `ThemeProvider`,
  a MobX observing component, and the full `StoryProvider` and `StoryLayout` shell.
- Establish whether `withKnobs(story, context)` is still callable as a plain function at
  `@storybook/addon-knobs@8.0.1`.
- Settle the story-signature rule for the two shapes the corpus mixes.
- Record every deviation the `webpackFinal` body needs, before the branch opens.

## Non-Goals

- Nothing from this task is merged. No change to `storybook/main.ts`, `preview.tsx`,
  `StoryDecorator.tsx` or `StoryProvider.tsx` on the branch, all four of which are in the task's
  `targetPaths` and are read rather than edited.
- No codemod. `task-014` owns that.
- No manifest change on the branch. `task-015` owns that.
- No conversion of any real story file. The pre-flight's CSF stories are written by hand and live
  only in the scratch project.

## Dependencies

- `task-012`, complete.
- `task-014` depends on this task, and `task-015` on that.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 1 at
  `:166-179` for why the route stops at 8.6.x, and locked decision 12 at `:274-283`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-013`, `task-014`,
  `task-015` and the phase 3 header
- `.agent/plans/storybook-modernization/research/02-storybook-upgrade-path.md`, sections 7 and 8
- `.agent/plans/storybook-modernization/research/03-react-upgrade-gate.md`
- `.agent/plans/storybook-modernization/task-plans/task-009.md`, for the two story signatures and
  their counts
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md`, particularly the MobX decorator conventions and the note that
    `configure({ enforceActions: 'observed' })` is live
- Workflows:
  - `.agent/workflows/storybook.md`, read for the current runtime shape. It documents 6.4.22 and is
    rewritten by `task-060`.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `147e5af31`, 2026-09-15, against the working tree and the npm registry.

- The registry is reachable: `registry.npmjs.org` returns `200` for `@storybook/react`. Without that
  this task would be `manual_execution`.
- `@storybook/react` and `@storybook/react-webpack5` publish 8.6.18 as the last 8.6.x, dated
  2026-03-06. Both peer `react` and `react-dom` at
  `^16.8.0 || ^17.0.0 || ^18.0.0 || ^19.0.0-beta`, so this repository's 16.14.0 is inside the
  declared range. That is the peer-range commitment the task entry says is not proof, which is why
  this pre-flight exists.
- `@storybook/react@8.6.18` marks both `typescript` and `@storybook/test` as optional peers, so
  neither is required to stand the framework up.
- `@storybook/addon-knobs` has no 8.6.x. Its last release is **8.0.1**, published 2024-06-19. It
  peers `@storybook/theming`, `@storybook/components`, `@storybook/core-events` and
  `@storybook/manager-api` at `^8.0.0`, and all four publish 8.6.18, so the constraint is satisfiable
  at this line. The task entry's claim is confirmed against the registry rather than assumed.
- The repository pins `@storybook/addon-knobs` at **6.4.0** (`package.json`), not 8.0.1. The bump is
  part of `task-015`'s manifest edit and is recorded here because the pre-flight has to install
  8.0.1 to test it.
- The four legacy-decorator settings, all of which must survive: `tsconfig.json:17`
  `experimentalDecorators`, and in the `swc-loader` rule in `webpackFinal`, `storybook/main.ts:79`
  `parser.decorators`, `:84` `transform.legacyDecorator` and `:87`
  `transform.useDefineForClassFields`. MobX 5.15.7 with `mobx-react` 6.3.1 breaks under TC39
  decorators, and it breaks at runtime rather than at build, so nothing in the check set would catch
  a dropped setting.
- The `webpackFinal` body to carry across, from `storybook/main.ts`: the `swc-loader` rule for
  `\.tsx?$`; the SCSS rule with `css-loader` `modules` and `localIdentName: '[name]_[local]'`; the
  plain `.css` rule through `MiniCssExtractPlugin`; `.inline.svg` through `svg-inline-loader` with
  `type: 'javascript/auto'`; the asset rule for fonts and images excluding `.inline.svg`; two
  `ProvidePlugin` entries for `Buffer` and `process`; two `NormalModuleReplacementPlugin` rules
  swapping `@trezor/transport` nodeusb and udp transports for their browser builds;
  `experiments.syncWebAssembly`; and a `resolve.fallback` map with nine polyfills and five stubs
  (`child_process`, `dgram`, `fs`, `usb`, `node-gyp-build` all `false`).
- The story-signature question is real and has two shapes in the corpus, measured during `task-009`:
  18 functions read the value from the first render argument, written `(props)` or `({ locale })`,
  and 16 from the second, written `(_, props)`. Two comments in the corpus each describe one shape
  and each is correct about its own file.
- `react-polymorph` is pinned at 1.0.4. It sits between Storybook and every rendered story through
  `StoryDecorator.tsx:25-30`, which wraps children in `ThemeProvider` with `SimpleSkins`,
  `SimpleDefaults` and `daedalusTheme` plus `themeOverrides`.
- `StoryDecorator` also uses `Children.map` and `React.cloneElement` at `:31-39`, so it depends on
  its children being elements rather than an array of arbitrary nodes. That is worth exercising
  because the 8.x decorator pipeline changed what a decorator receives.
- MobX is 5.15.7 and `mobx-react` 6.3.1, and `StoryDecorator` is itself an `@observer` class
  component, so the MobX path is already on the critical rendering route rather than only inside
  stories.
- The corpus is 258 registrations across 65 files, all still `storiesOf`. Nothing in this task
  changes that.
- The checks are green at `147e5af31`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook,i18n,stylelint}` all exit 0.

## Files Expected To Change

On the branch, tracking only:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-013.status` and
  the findings this task records
- `.agent/plans/storybook-modernization/task-plans/task-013.md`
- `.agent/plans/storybook-modernization/task-plans/task-013-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-013-impl-review.md`

Outside the repository, not committed and not merged:

- a scratch project holding the 8.6.x installation, the copied support modules and the hand-written
  CSF stories

No source file changes. The commit carries the pre-flight's findings, which is its whole output.

## Implementation Approach

1. Build the pre-flight as a standalone scratch project rather than a `git worktree`. The task entry
   says scratch worktree, and the requirement it expresses is that nothing reaches the branch. A
   plain directory satisfies that and avoids creating a worktree that would then need removing,
   which this environment cannot do without a hand-off. The deviation is recorded.
2. Pin the scratch project to this repository's versions for everything that is not Storybook:
   React 16.14.0, `react-dom` 16.14.0, `react-polymorph` 1.0.4, `mobx` 5.15.7, `mobx-react` 6.3.1,
   `typescript` 4.9.5, `swc-loader` 0.1.15, `@swc/core` 1.10.18, and the sass and css loader
   versions. A pre-flight against different versions of the things under test would prove nothing
   about this tree.
3. Copy the real `StoryDecorator.tsx`, `StoryProvider.tsx` and the theme modules they import out of
   the repository rather than writing stand-ins, so what is exercised is this repository's code.
4. Carry the `webpackFinal` body across verbatim and record any change it needs to build at 8.6.x,
   which is the fourth acceptance criterion.
5. Write CSF stories by hand covering the three cases the task names, and run `storybook build`.
   That establishes the builder, the SWC rule and the decorator settings.
6. Establish "renders" without a browser through `composeStories` from `@storybook/react` 8.6.x,
   which applies the project annotations, decorators and parameters to a story and returns a
   component. Rendering that under jsdom exercises the 8.6 preview pipeline rather than a
   hand-assembled approximation of it. Assert on the produced DOM: that the theme variables reach
   the tree, that a `react-polymorph` skin renders its markup, and that mutating an observable
   re-renders.
7. Test `withKnobs` at 8.0.1 by calling it as `withKnobs(story, context)` directly and recording the
   observed behaviour, whatever it is.
8. Settle the signature rule by observation: build both `(props) => ...` and `(_, props) => ...`
   stories at 8.6.x and record which argument carries what, then state the rule the conversion will
   follow.
9. Record everything in the task entry, including any kill-criterion trigger.
10. Land it as one signed commit carrying the plan documents and the tracking updates.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- Every pre-flight story renders in the preview with correct theming. There is no preview to look
  at, so this is answered by `composeStories` under jsdom asserting on the rendered DOM, plus a
  successful `storybook build`. The substitution and its limit are recorded rather than glossed.
- A MobX observable update re-renders a story. Evidence: an assertion that the DOM text changes after
  the observable is mutated inside `runInAction`.
- The build completes with the existing `swc-loader` rule rather than a re-derived compiler config.
  Evidence: the scratch `main.ts` carries the repository's rule verbatim and `storybook build`
  succeeds.
- Any deviation required from the current `webpackFinal` body is written down before the branch
  opens. Evidence: the diff between the repository's body and the scratch project's, recorded in the
  entry.
- The pre-flight records whether `withKnobs(story, context)` still works at 8.0.1, with the observed
  behaviour rather than an assumption. Evidence: the direct call and its result.

Added for this plan:

- The story-signature rule is settled by observation and written down, because `task-016` and the
  five hand-finish tranches all depend on it and "correcting" the shape that currently reads
  `undefined` would be a behaviour change rather than a fix.
- The four legacy-decorator settings are confirmed present in the scratch configuration and the MobX
  assertion is what would fail if one were dropped.

## Verification Plan

Already run for planning:

- Registry reachability, and the 8.6.18 peer ranges for `@storybook/react` and
  `@storybook/react-webpack5`.
- `@storybook/addon-knobs` version history and its 8.0.1 peer set, and confirmation that all four
  peered packages publish 8.6.18.
- The repository's pinned versions for React, MobX, `react-polymorph` and the compiler chain.
- The `webpackFinal` body and the four decorator settings, read at their lines.
- `StoryDecorator`'s `ThemeProvider` wrapping and its `Children.map`/`cloneElement` behaviour.

To run for the build:

- `yarn install` or `npm install` in the scratch project, recording what resolves.
- `storybook build` in the scratch project.
- `composeStories` under jsdom for each pre-flight story, with assertions on the DOM.
- The MobX mutation assertion.
- The `withKnobs(story, context)` call at 8.0.1.
- Both story signatures at 8.6.x, recording which argument carries the args and which the context.
- On the branch: `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}`, which must
  stay green because this task changes no source.

If `react-polymorph` misbehaves, the kill criterion fires: stop, record what was observed, and ask
before opening the branch. If `withKnobs(story, context)` no longer works, that is not a kill
criterion but it changes `task-016` and the tranche tasks, and it is reported before `task-014`
starts.

## Risks and Open Questions

- `composeStories` exercises the story pipeline, not the manager, the addon panels or the iframe
  bootstrap. A failure mode that only appears in a real browser would survive this pre-flight. That
  is the same blind spot the whole plan carries, recorded in locked decision 7, and this task
  narrows it rather than closing it.
- The scratch project is not the repository. It cannot reproduce interactions with the other 1500
  files under `source/`, only with the support modules copied into it. The things it is testing,
  `react-polymorph` under a new preview runtime and MobX under the decorator settings, do not depend
  on the rest of the tree.
- `@storybook/addon-knobs@8.0.1` is two years older than 8.6.18 and is the least maintained thing in
  the target set. If it fails here, the route through 8.6.x is in question and that is worth
  stopping for, even though the task entry does not list it as a kill criterion.
- Installing into a scratch project uses the public registry rather than the offline Nix cache, so
  the resolved tree may differ in transitive detail from what `task-015` will produce under Yarn 1
  with a lockfile. What is being tested is whether the code paths work, not whether a particular
  hoisting arrangement resolves; `task-015`'s own acceptance covers the latter.
- Nothing here can be rolled back, because nothing here lands.

## Required Docs, Research, and Tracking Updates

- Set `task-013.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Record in the entry: the observed `withKnobs` behaviour at 8.0.1, the settled signature rule, any
  `webpackFinal` deviation, and the fact that the addon is pinned at 6.4.0 today rather than the
  8.0.1 the compatibility analysis assumes.
- Record in the entry that "renders in the preview" was answered by `composeStories` under jsdom and
  a successful build, and what that does not cover.
- No PRD change unless the kill criterion fires.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-013-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-013-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- A scratch project outside the repository runs Storybook 8.6.18 with
  `@storybook/react-webpack5`, React 16.14.0, `react-polymorph` 1.0.4, MobX 5.15.7 and
  `@storybook/addon-knobs` 8.0.1, carrying this repository's `webpackFinal` body.
- `storybook build` succeeds. Ten render assertions pass through `composeStories` under jsdom.
- Nothing was merged. The commit carries the findings.

## Final Outcome

- `task-013` completed. `task-014` is unblocked. The kill criterion did not trigger.
- **One blocking deviation found, and it is the reason this task existed.** The repository's
  `webpackFinal` assigns `config.resolve` wholesale. At 8.6.x that discards the alias
  `@storybook/react-dom-shim`'s own preset installs, pointing the shim at its `react-16` build
  whenever `react-dom` is below 18. Without it the preview resolves `react-dom/client`, which React
  16.14.0 does not have, and the build fails with `SB_BUILDER-WEBPACK5_0002`. Spreading
  `config.resolve` instead of replacing it fixes it, verified here. `task-015` must carry this.
- `react-polymorph` renders correctly under the 8.6.18 pipeline: `Input` and `Button` through
  `StoryDecorator`'s `ThemeProvider`, with values, labels and theme class names. The full
  `StoryDecorator` and `StoryProvider` shell mounts and renders a story inside it.
- A MobX observable mutated in `runInAction` re-renders an `@observer` story, `count:0` to `count:1`,
  compiled with all four legacy-decorator settings.
- `withKnobs(story, context)` still works at 8.0.1 when called from inside a decorator, which is the
  corpus idiom at 22 sites. Called bare outside a render it throws, which is expected and is not how
  the corpus uses it.
- The signature rule is settled by measurement: at 8.6.18 the first render argument is the args
  object and the second is the story context.

## Self-Review

- The pre-flight paid for itself on its first build. The `config.resolve` assignment is invisible at
  6.4.22, fatal at 8.6.x, and would have surfaced inside the one landing that cannot be decomposed,
  with the branch already red and the cause looking like a React 16 incompatibility rather than a
  four-year-old line in `webpackFinal`.
- The first `withKnobs` result was a false negative and is recorded as one. Calling the function
  bare in Node is not the corpus idiom; the corpus calls it inside a decorator, where the hook
  context the addon needs exists. A verification that reproduces the wrong thing is worse than none,
  and the second attempt reproduced the real call site.
- Layering the repository's installed tree underneath the pre-flight's own was what made the full
  shell testable. Replicating the application's dependency set package by package was converging one
  error at a time and would not have finished.
- What `composeStories` does not cover is stated rather than left implied.
