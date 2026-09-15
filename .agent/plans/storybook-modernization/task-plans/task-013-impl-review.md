Implementation: Iteration 1
Timestamp: 2026-09-15T06:12:40Z

Changes made:
- Built the pre-flight as a standalone scratch project outside the repository, holding Storybook
  8.6.18 with `@storybook/react-webpack5`, React 16.14.0, `react-polymorph` 1.0.4, MobX 5.15.7,
  `mobx-react` 6.3.1 and `@storybook/addon-knobs` 8.0.1, plus the compiler and loader versions this
  repository pins.
- Carried the repository's `webpackFinal` body across verbatim, extracted from `storybook/main.ts`
  rather than retyped.
- Wrote five hand-written CSF stories exercising `react-polymorph` through the real
  `StoryDecorator`, a MobX `@observer`, the corpus's `withKnobs(story, context)` idiom, both story
  signatures, and the full `StoryDecorator` plus `StoryProvider` shell.
- Recorded seven findings in the `task-013` entry and set its status to `completed`.

Files touched:
- On the branch: the tasks JSON and the three `task-013` plan documents. No source change.
- Outside the repository: the scratch pre-flight project, not committed.

Verification run:
- `npm install` resolved the whole tree, confirming 8.6.18 and React 16.14.0 coexist:
  `storybook` 8.6.18, `@storybook/react` 8.6.18, `@storybook/react-webpack5` 8.6.18,
  `@storybook/addon-knobs` 8.0.1, `react` 16.14.0, `react-polymorph` 1.0.4, `mobx` 5.15.7,
  `mobx-react` 6.3.1.
- `storybook build` failed on the first attempt with
  `SB_BUILDER-WEBPACK5_0002 (WebpackInvocationError): Module not found: Error: Can't resolve
  'react-dom/client' in node_modules/@storybook/react-dom-shim/dist`. Traced to
  `@storybook/react-dom-shim/dist/preset.js`, which exports its own `webpackFinal` setting
  `resolve.alias['@storybook/react-dom-shim'] = '@storybook/react-dom-shim/dist/react-16'` whenever
  `react-dom` is not 18 or 19. The repository's `webpackFinal` runs after it and assigns
  `config.resolve` wholesale, discarding the alias. Spreading `config.resolve` and merging
  `extensions` and `fallback` instead of replacing them makes the build succeed. This is the
  deviation the fourth acceptance criterion asks for, and `task-015` has to carry it.
- `storybook build` then succeeded, preview built. The only warning is
  `Can't resolve '@storybook/test'`, which is an optional peer of `@storybook/react` and a warning
  rather than an error.
- Ten render assertions through `composeStories` under jsdom, all passing: `composeStories` returned
  the stories; `react-polymorph` `Input` rendered an `<input>` through `ThemeProvider` carrying the
  story's value and a theme class name; `Button` rendered with its label; the MobX story rendered
  `count:0` and re-rendered to `count:1` after `runInAction`; and the full `StoryDecorator` plus
  `StoryProvider` shell mounted and rendered a story inside it.
- `withKnobs` at 8.0.1, tested twice. Called bare in Node it throws
  `Storybook preview hooks can only be called inside decorators and story functions`. Called from
  inside a decorator, which is the corpus idiom, it renders the wrapped story and the knobs return
  their defaults: rendered text `knob-default:true`, with the wrapper element wrapping the story.
- Both story signatures at 8.6.18: `(props) => ...` receives the args object, printed as `arg1={}`;
  `(_, props) => ...` receives the story context, printed with `argTypes`, `args`, `globals` and the
  rest.
- On the branch, `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` stay green,
  which is expected because this task changes no source.

Two corrections to my own work, recorded rather than quietly fixed:

- The first `withKnobs` test called the function bare, outside any story render, and recorded a
  failure. That was a false negative: the addon uses Storybook preview hooks internally, so it needs
  an active hook context, and the corpus calls it inside a decorator where that context exists. The
  test was rewritten to reproduce the real call site and then passed. A verification that reproduces
  the wrong thing is worse than no verification.
- The first attempt at the full shell tried to replicate the application's dependency set in the
  scratch project and was converging one `Module not found` at a time, four rounds in. Layering the
  repository's own installed tree beneath the pre-flight's in `resolve.modules`, so Storybook and
  React come from 8.6.x and everything else falls through, made the shell build and render at once.

Deviations from the approved plan:
- A plain scratch directory was used rather than a `git worktree`, as the plan said it would be:
  removing a worktree afterwards needs a deletion this environment cannot perform. Nothing reached
  the branch either way, which is what the task entry's wording requires.

User interaction is now required:
- No, but one finding changes `task-015`: the `config.resolve` deviation is mandatory, not optional.

Outcome: All five acceptance criteria met, the kill criterion did not trigger, and one blocking
deviation was found before the branch opened; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T06:17:55Z

Summary:
- Approved. The pre-flight found a defect that would have surfaced inside the irreversible window,
  disguised as something else, and it settled both of the questions the tranche tasks depend on.

Blocking findings:
- None.

Non-blocking observations:
- The `config.resolve` finding is the whole justification for this task existing. A four-year-old
  line that assigns rather than merges is invisible at 6.4.22, fatal at 8.6.x, and its symptom,
  `Can't resolve react-dom/client`, points at React 16 rather than at `webpackFinal`. Finding it
  with the branch green and one command to reproduce it is worth more than the eight hours the
  entry budgets.
- The `withKnobs` false negative is the more instructive of the two self-corrections. The first
  result was a real error message from a real library and would have been easy to write up as "the
  idiom no longer works at 8.0.1", which would have sent `task-016` and five tranche tasks down a
  rewrite that was not needed. What caught it was asking whether the test reproduced the call site,
  not whether the code threw.
- Measuring the signature behaviour rather than reasoning about it produced a rule that contradicts
  the intuitive fix. `(props) => props.currentTheme` reads args and gets `undefined`, and
  "correcting" it to read the context would change behaviour rather than restore it. Written down
  now, that saves each tranche from deciding it independently.
- Layering the repository's node_modules under the pre-flight's is a good technique to keep: it
  tests the repository's real modules against new framework packages without replicating a
  dependency set.
- The limits are stated. `composeStories` does not exercise the manager, the addon panels or the
  iframe bootstrap, and the entry says so rather than letting ten green assertions imply a browser
  was involved.

Approval bar:
- Met. `task-013` is complete and `task-014` is unblocked.

Decision: approved
