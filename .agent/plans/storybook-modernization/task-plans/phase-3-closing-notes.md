# Phase 3 closing notes

Status: complete. The window closed at `task-025`.

## The recurring defect: an instrument that reports on something other than what it measures

Six times so far in this plan a tool has reported success, or reported a number, for something other
than the thing it appeared to be measuring. They are listed here because the pattern is more useful
than any one of them, and because each was cheap to catch once the question was asked and expensive
to catch afterwards.

1. **`yarn storybook:build` exited 0 on a hard build failure.** The 8.6 CLI's crash-report prompt
   resolves without a TTY, so the process exited cleanly after the build had failed. `--disable-telemetry`
   on both scripts makes the same tree exit 1. Until that was fixed, every green `storybook` check
   was unfalsifiable.
2. **A prettier check that matched no files reported success.** The copy being checked sat under a
   path `.prettierignore` excludes, and prettier reports success when it matches nothing. Checking
   through `--stdin-filepath` measures the file rather than the path.
3. **The formatter being checked was not the formatter being enforced.** `node_modules/.bin/prettier`
   is pinned at 2.1.2 and nothing runs it: `.eslintrc` extends `eslint-config-prettier`, which only
   switches formatting rules off. `nix fmt` runs prettier 3.6.2 and is the only formatter with
   authority over the tree. Several tasks were checked against the wrong one before this surfaced.
   It produced one divergence, because the two versions agree nearly everywhere, which is exactly why
   it went unnoticed.
4. **`storybook migrate` converted 1 of 64 files and reported `0 errors, 1 ok`.** The CLI relaunches
   itself through `child_process.spawn(..., { shell: true })`, so `--glob` is pathname-expanded by
   `/bin/sh` before the CLI parses arguments, and `/bin/sh` has no `**`. The pattern collapsed to one
   directory level, the first match became the glob, and the rest became ignored arguments. The
   number `1` is the tell, and it is only a tell if the count is read.
5. **A dry run at a different directory depth rehearsed something different.** The same glob under a
   shallower tree matched nothing in the shell, so the shell left it alone and the correct pattern
   reached the tool. The rehearsal passed for a reason that did not hold in the real run. Depth is
   not something anyone would have written down as a variable.
6. **The codemod chain's own report cannot see a dropped registration.** Measured directly: the chain
   reports `1 ok, 0 errors` on a file whose non-literal `.add()` label it discarded. This is why
   every tranche in this phase is measured by label set diffed against the baseline and never by exit
   status.

The two questions that catch this class:

- What would this instrument report if the work had not been done? If the answer is "the same
  thing", it is not an instrument.
- Is the thing being measured the thing that has authority? A formatter, a linter and a build can all
  be present, configured, and irrelevant.

## What the measurements rest on

The label set is the load-bearing measurement for the whole conversion. It is read three ways and the
three agree: the committed extractor over the `storiesOf` corpus before, an AST reading of the
converted files after, and `index.json` emitted by a real Storybook build after, which is Storybook's
own indexer rather than a model of it. 258 `title | label` pairs across 49 panels, identical in all
three, with per-file ordering identical too.

## A story can render nothing, in two directions

Locked decision 7 dropped the browser-driven render check because Playwright cannot run in the
offline sandbox, and recorded the consequence: a story could build and render nothing with every
check green. Thirteen live instances of that were in the corpus. Two of them are worth keeping as a
pair, because they show the failure has two directions and one blind spot covers both.

`news/IncidentOverlay.stories.tsx` read an argument nothing fills. Its decorator handed the incident
fixture to `story({ ... })`, which Storybook merges onto the story context after stripping `title`
among other keys, while the three stories read their first argument, which is `context.args`. All
three rendered an incident with no title, content, date or action.

`wallets/settings/WalletSettings.stories.tsx` read an argument filled with the wrong thing. It
spread the entire story context into `WalletSettingsScreen`, a component declaring one prop, so the
screen was handed a story id, a parameters object and a globals object as props alongside a locale
that only arrived because the wrapper was still passing it down.

One asks for something that is not there; the other takes everything that is. Neither is visible to
`compile`, to `lint`, or to `storybook`: both compile, both lint clean, both index, and both appear
in the sidebar under the right label with a component that is empty or wrong inside. The scan at
`story-args-audit.js` covers both, because both come down to the same question, which argument the
value is expected to arrive on.

## A check nobody runs is not green

`jest` was failing from the moment the manifest moved to 8.6.18, and four tasks went past before
anyone saw it. Nothing hid it. The window was being tracked by `compile`, `lint` and `storybook`,
because those were the three expected to move, and `jest` was simply not being run. It surfaced at
`task-025` only because that task runs everything the flake defines rather than the three under
observation.

The same shape appears twice more in this phase, smaller each time. Seven imports survived a file
split unused, invisible to `tsc` under this configuration and to a lint rule that is off. Two
`@ts-ignore` directives survived the change that made their subject disappear. In each case nothing
was watching that particular thing, and nothing said so.

## The count that went up

The empty-render scan reported eleven, then thirteen. Nothing regressed: the scan could not see a
story written as `export const Thing = SomeStory`, and two of the fourteen such bindings in the
corpus read a parameter. The true figure had always been thirteen.

Worth separating from a real regression, because from outside the two are indistinguishable. What
tells them apart is the mechanism, and only whoever is holding the instrument can produce it. The
rule that follows: when a count moves the wrong way, show why before continuing, and if the reason
cannot be shown, stop.

## Defects found in the repository, not in the plan

- **Undeclared transitive dependencies.** Written up in
  `.agent/findings/undeclared-transitive-dependencies.md`. `@types/webpack-env` and `os-browserify`
  were used but never declared, supplied only by a package the upgrade removed.
- **Two copies of webpack in one build.** `storybook/main.ts` built plugins from the pinned webpack
  at the top of `node_modules` while `@storybook/builder-webpack5` ran its own nested copy. Different
  copies mean different class objects, so a plugin from one registered on a compiler from the other
  produced 156 `Module parse failed` errors naming no file.
- **`yarn compile` does not catch a dangling relative import.** Confirmed with a probe import that
  `compile` passed and `storybook:build` rejected.
- **Jest cannot resolve a package `exports` map.** Jest 27 predates the feature, and Storybook 8
  publishes almost everything behind one. Fixed with a resolver rather than a list of path mappings,
  because each mapped path resolves to a bundle naming more of them.
- **Three decorators branch on values that do not do what they look like.** A wallets wrapper tests
  for two story names that have never existed under any wallets panel; a settings menu item points
  at `Terms of service` where the story is `Terms of Service`; a wallet navigation layout strips a
  `Wallets|` prefix from titles that have used ` / ` for years. All three predate this work and none
  was touched by it.

## What the window did

70 compiler errors at `task-015`, 0 at `task-022`. 258 sidebar registrations across 49 panels before
the conversion and the same 258 after, pair for pair, at every step. 13 stories rendering against
nothing at the start and none at the end. All eight checks green.

Twenty-two of the 272 baseline registrations differ from what is there now: fifteen removed by phase
1, one restaged deliberately, and the rest of the difference is panels and groups emptied by those
removals. Every one is enumerated in `task-023`.
