Implementation: Iteration 1
Timestamp: 2026-09-15T11:48:20Z

Changes made:
- Added `storybook/stories/_support/globals.ts` with `currentThemeOf`, `localeOf` and `osNameOf`.
  Each takes the story context and maps through `config.ts`, so a component gets the theme id and
  the locale code rather than the toolbar labels.
- Moved all 15 global reads in the tranche onto `(_args, context)` and a helper call: 3 in
  `common/Widgets`, 1 in `dapps/TransactionRequest`, 7 in `navigation/Sidebar`, 2 in
  `navigation/SidebarWalletsMenu`, 1 in `news/AlertsOverlay`, 1 in `news/AppUpdateOverlay`, 1 in
  `news/NewsFeed`.
- `news/IncidentOverlay.stories.tsx`: the incident fixture moved from the decorator's `story({ ... })`
  call to `args` on the meta, and the three render functions now name their first argument `args`.
- Removed `getInitialState` and its `getParams` helper from `_support/config.ts`. Nothing has called
  them since the toolbar addon went.
- The other five tranche files read no global and are untouched.

What the change rests on, measured rather than reasoned about:
- A probe composed a synthetic CSF module through `@storybook/react`'s own `composeStories` and
  printed both arguments a render function receives. The first is `context.args`. `context.globals`
  carries the toolbar selections. A decorator calling `story({ x })` merges `x` onto the context at
  top level, not into `args`.
- A second run added `args` to the synthetic meta and printed them arriving as the first argument
  intact, including a key named `title`.
- That second detail matters here. Storybook sanitises a `story({ ... })` update before merging it
  and strips `title` along with `id`, `name`, `parameters` and others. The `IncidentOverlay` fixture
  has a `title`, so even a story reading the right argument would have rendered an incident with no
  title. Moving the fixture to `args` removes both problems at once.

Verification:
- No `props.currentTheme`, `props.osName` or `props.locale` anywhere in the 13 files, nor any
  destructured equivalent.
- The only first-argument reads left in the tranche are the three `IncidentOverlay` stories, whose
  meta now declares `args`.
- Label set from `index.json` of a real build: 258 entries across 49 titles, identical to the
  pre-conversion set pair for pair.
- `compile` 19 errors, all `TS2339`, none in this tranche. `lint` exit 0. `storybook` exit 0.
- `@ts-ignore` in the tranche 49 to 48.

Two corrections during the work:

- The first check run reported seven `TS2307` for `../_support/globals`, a module sitting on disk.
  The `nix` checks build from the git tree, so a new file that has not been staged does not exist as
  far as they are concerned. Staging it cleared all seven. A check can be red for a reason that has
  nothing to do with the code it is checking, and this one names a missing module convincingly.
- `IncidentOverlay.stories.tsx` loses its single `@ts-ignore`, taking the tranche from 49 to 48
  against an acceptance criterion that says the count is unchanged. It suppressed the type error on
  the `story({ ... })` argument, and that call no longer exists, so it is the same category as the 28
  the conversion removed and audited. Recorded rather than replaced with a directive that would
  suppress nothing.

Deviations from the approved plan:
- None.

Outcome: The tranche reads its globals from context, three stories that rendered empty now render
their fixture, and the patterns for the remaining four are written down; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T11:55:05Z

Summary:
- Approved.

Blocking findings:
- None.

Non-blocking observations:
- The `IncidentOverlay` defect is the sharpest illustration so far of what a green build does not
  cover. `storybook` passes, `compile` says nothing, `lint` says nothing, the story renders, the
  panel appears in the sidebar with the right label, and the component inside it has no content. The
  only thing that finds it is asking what value actually arrives.
- Routing every global read through one module is worth more than the three functions in it. It
  gives `task-024` a single place to check, it stops the label-to-value mapping being restated per
  story, and it makes the "which argument" question answerable once rather than at each site.
- Six of the fifteen sites were reading an argument that does not carry what they asked for. That
  they were not obviously broken is the point: `intl[undefined]` and `isShelleyTestnetTheme(undefined)`
  both return something.
- Removing `getInitialState` here rather than carrying it through four more tranches is the right
  moment for it. It reimplemented persistence the platform now provides and had no caller left.

Approval bar:
- Met. `task-018` is complete and the remaining tranches have their pattern.

Decision: approved

Correction: Iteration 1
Timestamp: 2026-09-15T12:22:40Z

The check results above said `compile` reported 19 errors "none in this tranche". One of the 19 was
in this tranche: `notifications/Notifications.stories.tsx:170`, `Property 'storyName' does not exist
on type '() => Element'`. The claim was made from a grep whose output was read as though it were
empty when it was not, which is the same failure as trusting an exit status.

Fixed here rather than left for a later tranche, using the shape this tranche settled for a display
name: the `withState` call becomes a plain const and the export becomes
`{ render: WithActionsStory, name: 'With actions' }`. `withState` itself is untouched, so phase 4's
scope does not move. The label is unchanged, confirmed against `index.json`.

The same shape accounts for all 19: `csf-2-to-3` leaves `X.storyName` or `X.parameters` attached to
a const TypeScript has typed `() => Element`, and the remedy in every case is the CSF 3 object form.
Eighteen remain, and each belongs to a tranche that has not run yet.
