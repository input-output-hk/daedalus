# Storybook Modernization PRD

## Overview

Move Daedalus off Storybook 6.4.22 and off the two authoring APIs that no longer exist upstream,
`storiesOf()` and `@storybook/addon-knobs`, by landing on Storybook 8.6.x, converting the corpus
there, and then bumping to 10.6.x. Delete the story sets that document screens no user can reach.
Then extend the workbench from a component catalog into a screen catalog: a shared MobX store
fixture harness and a container story for every one of the 49 router-reachable screens, driven from
the route configuration rather than from the container directory listing. Close with
`eslint-plugin-storybook` and the documentation rewrite. An automated render check is not part of
this plan; the reason and its consequence are recorded under Locked Planning Decision 7.

The work is scoped to `storybook/`, the four colocated story files under `source/`, the two
`.agent/` documents that teach the current APIs, and the packaging and CI touchpoints those require.
It is not a React upgrade and it is not a component refactor. Three deletions reach outside that
scope, all of them in phase 1 and all of them code no user can reach: the eight components under
`source/renderer/app/components/staking/legacy/`, the `/staking/epochs` screen with its route,
container and components, and the `/redeem-itn-wallets` route binding. Apart from
`containers/staking/StakingEpochsPage.tsx`, which is deleted, no file under
`source/renderer/app/containers/` is modified: the containers are the subject of the new stories,
not the target of them.

## Problem Statement

The workbench is four years behind, two of its load-bearing APIs are gone upstream, and it covers
the wrong layer of the application.

`@storybook/react` and its siblings are pinned at 6.4.22, published 2022-04-14
(`research/02-storybook-upgrade-path.md`). Four of the ten pinned Storybook-related packages have no
version at all in the current line: `@storybook/core` stops at 8.6.14, `@storybook/addons` at
7.6.17, `@storybook/manager-webpack5` at 6.5.16, and `@storybook/addon-actions` publishes 9.0.8 as
an empty package. No upstream fix reaches the repository, and no currently maintained addon can be
installed.

All 73 `storiesOf()` calls across 69 files use an API removed in Storybook 8.0.0 with no feature flag
and no compatibility shim. All 396 direct knob call sites use an addon whose last release, 8.0.1, peers on
four `@storybook/*` packages that stopped being published at Storybook 9. Ten files use
`@dump247/storybook-state`, last published 2019-06-22. The conversion cost grows with every story
added and is unavoidable under every tooling option surveyed.

Coverage has drifted in a way no check can see. `storybook/main.ts:8` declares a single entry,
`stories: ['../storybook/stories/index.ts']`, a hand-maintained barrel of side-effect imports.
Nothing is auto-discovered, so a story file dropped from a barrel disappears from the sidebar without
failing anything. `storybook/stories/staking/Legacy.stories.tsx` is dead for exactly this reason,
and it is the only reference in the repository to three of the eight components under
`source/renderer/app/components/staking/legacy/`, the other five being referenced by nothing at all.
Four further story sets document features that a flag has disabled.

The coverage that does exist sits one layer below the thing being reviewed. Two of 105 containers
are reached by any story (`research/01-current-coverage.md`), and only
`storybook/stories/nodes/about/About.stories.tsx:12-14` mounts a real container, using a 19-line
literal at `storybook/stories/nodes/_utils/props.ts`. A reviewer can see `WalletSummary` with
fabricated props; nobody can see `/wallets/:id/summary` as the container assembles it, with its
empty state, its restoring state and its first-page-loading state. Five reachable screens have no
story at either layer: `/settings/general`, `/profile/terms-of-service`, `LoadingPage`,
`RedeemItnRewardsContainer` and the Toggle RTS Flags dialog
(`research/05-reachable-screens.md`).

The layers that would otherwise catch a rendering regression are largely absent. There are 51 Jest
spec files against 361 component files, all running in jsdom, which asserts nothing about layout,
theming or text overflow. The Cucumber end-to-end suite cannot execute: `spectron@14.0.0` resolves
`electron-chromedriver@12` against Electron 41.3.0, 23 of 48 feature files are disabled at feature
level, and Cucumber is not in the CI check set. `yarn storybook:build` at `perSystem/checks.nix:78`
is the only automated check in the repository that renders Daedalus components with real themes and
real translation catalogs and fails if one of them throws.

## Tool Decision

**Daedalus keeps Storybook and modernizes it on the webpack 5 builder.** Settled by evidence in
`research/04-tooling-alternatives.md`, and not reopened here. Two findings decide it.

**Storybook is the only surveyed workbench that still supports React 16.**
`@storybook/react-webpack5@10.6.0`, published 2026-09-02, declares
`react: "^16.8.0 || ^17.0.0 || ^18.0.0 || ^19.0.0"`, and Storybook ships a React 16 render shim that
its preset selects automatically whenever the resolved `react-dom` does not start with 18 or 19.
Every lighter alternative has moved its floor to React 18: `@ladle/react@5.1.1` declares
`react: ">=18.0.0"`, `react-cosmos@7.4.1` declares `react: ">=18"`, `vitest-browser-react@2.3.0`
declares `react: "^18.0.0 || ^19.0.0"`, and Cypress raised its component-testing floor to React 18 in
version 14. Daedalus is on `react: "16.14.0"` (`package.json:259`).

**Storybook is the only option that keeps the webpack 5 builder.** Every lighter workbench is built
on Vite, and Vite's default transform path has a documented failure mode with the legacy decorators
MobX 5 requires, including dev mode using legacy decorators while the production build silently uses
spec decorators. `storybook/main.ts:84` and `storybook/main.ts:87` set `legacyDecorator: true` and
`useDefineForClassFields: false` with inline comments recording that MobX 5 breaks without them.

Histoire is eliminated outright: `@histoire/plugin-react` returns `{"error":"Not found"}` from the
npm registry. Component testing frameworks test components rather than showcase them; Playwright
component testing is the only one that clears the React 16 bar, and its documentation is explicit
that it ships no browsing UI, which is what localization review depends on. Dropping the workbench
entirely would remove the only automated rendering check that exists, leaving a wallet that holds
real funds with 51 jsdom specs and a dead end-to-end suite.

## Goals

- Land Daedalus on Storybook 10.6.x with the webpack 5 builder and the existing SWC
  legacy-decorator settings intact, via a single intermediate stop at 8.6.x.
- Convert every remaining story file off `storiesOf()` into Component Story Format.
- Convert every knob call site and all 17 `withState` call sites off addons that have no forward
  path.
- Keep `yarn storybook:build` green at every point in the work, not only at the end.
- Build one shared MobX store fixture harness that serves nearly every container.
- Ship a container story for each of the 49 router-reachable screens, with the multi-state screens
  carrying a story per meaningful state.
- Remove dead story files, orphaned support modules and unreferenced dependencies.
- Remove the `/staking/epochs` screen and the `/redeem-itn-wallets` route binding, the two dead
  routes this plan deletes rather than carries.
- Replace the bespoke DaedalusMenu addon with Storybook's own toolbar globals, preserving theme,
  locale and operating-system switching.
- Make an unreferenced story file a build participant rather than a silent absence.
- Leave `.agent/skills/storybook-creation/SKILL.md` and `.agent/workflows/storybook.md` teaching the
  APIs that actually exist.

## Non-Goals

- Upgrading React. `research/03-react-upgrade-gate.md` establishes that no part of this work
  requires it.
- Migrating `react-intl`, MobX, or `react-polymorph`. All three sit under the component tree and
  none of them changes when Storybook moves.
- Stories for all 105 containers. Coverage is the 49 router-reachable screens. The 26 distinct
  reachable dialog and wizard-step targets enumerated in `research/05-reachable-screens.md` section 5
  are exercised only through the parent screen stories that open them, and a dedicated tier for them
  is a named follow-on.
- Image-diff visual regression, which inherits the Playwright blocker in locked decision 7.
  `@storybook/test-runner` with `jest-image-snapshot` and committed
  baselines is the recommended shape when the project takes it, and it is not in this plan.
- Removing the 229 `@ts-ignore` directives in story files. They are carried through the conversion
  and their removal is a named follow-on.
- Repairing the Cucumber end-to-end suite. `spectron` is unmaintained and `spectron@14` against
  Electron 41.3.0 is not a version bump.
- Changing any feature flag, or any component behind one.
- Refactoring the components and containers the stories render.

## Inputs And Source Material

- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/storybook.md`
- `.agent/workflows/test.md`
- `.agent/skills/storybook-creation/SKILL.md`
- `.agent/plans/readme.md`
- `research/01-current-coverage.md`: measured migration surface, container and component coverage,
  dead stories, type-check and lint guarantees
- `research/02-storybook-upgrade-path.md`: per-version requirements, codemod coverage, staging
- `research/03-react-upgrade-gate.md`: whether React gates this work, and what React costs
- `research/04-tooling-alternatives.md`: the tool decision and its evidence
- `research/05-reachable-screens.md`: the 49-screen coverage target, the exclusions with evidence,
  the store dependency census, the fixture harness verdict and the tranche sequence
- `storybook/main.ts`, `storybook/preview.tsx`, `storybook/stories/index.ts`
- `storybook/stories/_support/StoryWrapper.tsx`, `StoryDecorator.tsx`, `StoryProvider.tsx`,
  `StoryLayout.tsx`, `config.ts`
- `storybook/addons/DaedalusMenu/`
- `source/renderer/app/stores/index.ts`, `source/renderer/app/stores/lib/Store.ts`,
  `source/renderer/app/types/injectedPropsType.ts`, `source/renderer/app/hooks/useStores.ts`
- `source/renderer/app/Routes.tsx`, `source/renderer/app/routes-config.ts`,
  `source/renderer/app/App.tsx`, `source/renderer/app/containers/Root.tsx`,
  `source/renderer/app/containers/MainLayout.tsx`
- `perSystem/checks.nix`, `package.json`, `tsconfig.json`

## Locked Planning Decisions

These are settled. Each carries the reasoning that supports it, so the reasoning does not have to be
rebuilt at execution time.

**1. Route: land on 8.6.x, convert there, then bump to 10.6.x.**
8.6.x is the only line where the `storiesof-to-csf` codemod and a working `@storybook/addon-knobs`
build coexist (`research/02-storybook-upgrade-path.md` sections 7 and 8). Landing there means the
shape conversion gets tooling and the 396 knob call sites stay untouched while it happens, so the
two largest pieces of work are separated instead of landing together. The alternative was to convert
on the pinned 6.4.22, which would have required first proving by spike that 6.4.22 indexes a glob of
CSF files and honors `args` and `globalTypes`. That route rests on an assumption; this one rests on
published facts. **Consequence: the 6.4.22 capability spike is dropped and is not a gate on
anything.** The target remains 10.6.x. 9.1.x is the documented fallback if the `moduleResolution`
change that 10 requires turns out not to be mechanically fixable across `source/`.

**2. Container stories are in scope. Coverage is every router-reachable screen.**
49 screens, enumerated from `Routes.tsx`, `App.tsx`, `Root.tsx` and `MainLayout.tsx` in
`research/05-reachable-screens.md`, not the 105 files under `containers/`. 57 of those 105 are not
screens: 47 are dialogs, wizard steps or dialog fragments, 8 are route destinations no affordance
reaches, and the remainder are redirects. The redirect count needs re-deriving before the census is
quoted again: an independent pass found only one, `governance/GovernanceRootRedirect.tsx`, and the
48 + 47 + 8 + 2 = 105 arithmetic depends on there being two. The screen count of 49 comes from the
route configuration and does not depend on this, so the target is unaffected; the container
breakdown is what needs the correction. Decision 14 deletes one of those 8, so after phase 1 the same
arithmetic reads 56 of 104 and the screen count does not move. Driving the target from the route
configuration means the target is auditable: every screen on the list has an affordance, and every
exclusion carries its evidence in section 4 of that note, so re-checking a flag is a grep rather
than a re-derivation. The fixture work this commits to is larger than the conversion it follows, and
the estimate below is built around that rather than added to it.

**3. Sequencing: convert the existing corpus to CSF first, then add container stories.**
A CSF decorator is where a store fixture attaches. Writing container stories against `storiesOf()`
would mean writing the harness twice, once as an `.addDecorator()` argument and once as a CSF
decorator. Nothing in `research/05-reachable-screens.md` gives a reason to interleave them: the
harness depends on the store map and the provider set, not on which stories already exist.

**4. Delete the four flag-disabled story sets. Leave the components and the flags untouched.**
Paper wallet certificate creation (5 registrations), staking info (2), staking countdown (2 panels),
legacy wallet notification and transfer funds (3). A story is documentation of a reachable screen,
and 12 registrations documenting unreachable ones mislead anyone browsing the sidebar. Every flag is
a one-line reversion, so if a feature returns, writing the story back in CSF against a live feature
is cheaper than carrying it through this conversion. The components stay because deleting them is a
product decision about the features, not about the workbench. This rule is deliberately not applied
to the voting stories, which decision 13 keeps on the same observable evidence.

**5. Delete `storybook/stories/staking/Legacy.stories.tsx` and
`source/renderer/app/components/staking/legacy/`.**
The story is orphaned from the barrel, so its two registrations never appear in the sidebar and
`storybook:build` never loads it. All eight components in that directory go with it. The story is
the sole reference to three of them (`StakingChart` at `Legacy.stories.tsx:5`, and
`StakingChartTooltip` and `StakingChartTooltipItem` transitively). The other five
(`legacy/Staking.tsx`, `BlockGenerationInfo`, `StakingSwitch`, `StakingSystemState`,
`StakingSystemStateElement`) are referenced by nothing at all, including the story. The only thing
keeping any of them compiling is that `tsc` takes every file in the repository. They remain in
history.

**6. Restage the wallet settings undelegation story as a standalone story of the dialog.**
`storybook/stories/wallets/settings/WalletSettingsScreen.stories.tsx:326` renders
`UndelegateWalletConfirmationDialog` inside a box that `WalletSettings.tsx:209` returns null for,
because `walletsConfig.ts:45` sets `IS_WALLET_UNDELEGATION_ENABLED = false`. Unlike the four sets
above, the component is live: it stays reachable through `DelegationCenterPage.tsx:118-119`. Only the
surrounding context is wrong, so the fix is to restage rather than delete.

**7. `@storybook/test-runner` render smoke checks are out of scope.**
The runner cannot execute in this repository's CI and the plan does not pretend otherwise.
`@storybook/test-runner@0.24.5` depends on `playwright` and `playwright-core` and wires a Playwright
Jest environment through `jest-preset.json`. Neither package declares a `scripts` field, so browser
binaries are never fetched at install time. `mkJsCheck` (`perSystem/checks.nix:16-31`) builds inside
the Nix sandbox from the prebuilt `node_modules` derivation, and `nix/internal/common.nix:315` writes
`'"--offline" true'` into `.yarnrc` with an offline mirror at `:298`. There is no network and no
browser. `grep -rn playwright` over `nix/`, `perSystem/` and `flake.nix` returns nothing.

Standing it up would mean `pkgs.playwright-driver.browsers`, `PLAYWRIGHT_BROWSERS_PATH`, a
nixpkgs-to-npm version pairing, an addition to `nativeBuildInputs`, and something to serve
`dist/storybook` for `TARGET_URL`. This plan does not take that on.

**The consequence.** The largest residual risk in this plan is that
a rewritten story and a container fixture can both build and still render nothing, and nothing in
this plan closes it. `storybook:build` bundles without evaluating preview modules, so a story that
throws at load can sit behind a green required check. The 202 hours in phases 6 and 7 are therefore
accepted by eye, and the required check set stays exactly as blind to rendering as it is today.
Image-diff baselines remain a named follow-on and inherit the same blocker.
`jest-image-snapshot` catches a different failure and brings baseline churn that a conversion of this
size would swamp.

**8. Carry the 229 `@ts-ignore` directives through the conversion.**
Removing them surfaces prop mismatches suppressed since the TypeScript conversion, some of which are
likely real defects in the components rather than the stories. Mixing "this story moved to CSF" with
"this component's props were wrong all along" in one diff makes both harder to review. They are
concentrated in `staking/DelegationCenter.stories.tsx` (19), `navigation/Sidebar.stories.tsx` (15)
and `common/ItemsDropdown.stories.tsx` (14). Their removal is a separate follow-on.

**9. `useArgs` is the default replacement for the 17 `withState` call sites.**
Args-backed state surfaces in the Controls panel and survives a URL share, which is right wherever
the state is genuinely a component input. `useState` from the preview API is the exception, used only
where exposing the value as a control would be actively misleading, meaning scratch state that no
viewer should poke at. The exception rule is recorded once, in the conversion task, along with which
sites took it. It is not decided per file by whoever is editing that file.

**10. Add `eslint-plugin-storybook` at the end. The 457 existing warnings stay warnings.**
Adding the plugin once the corpus is already clean against it makes a story authored against the old
APIs a lint failure rather than a review catch, without a large mechanical cleanup landing in the
same change. Promoting the existing findings is a different piece of work: 261 of the 457 are
`@typescript-eslint/ban-ts-comment` and are therefore the same question as decision 8.

**11. Set `useDefineForClassFields` to `false` in `tsconfig.json`, as a standalone commit.**
`tsconfig.json:24` sets it `true` while `storybook/main.ts:87` sets it `false` and `jest.config.js`
sets the SWC equivalent to `false`. The bundler value is the one that takes effect at runtime and the
one MobX 5 requires, so the type checker is currently checking against semantics that neither runtime
uses. Standalone, with its own `yarn compile` verification, because its fallout has nothing to do
with Storybook.

**12. Do not run `storybook upgrade`. Hand-edit `package.json` and run automigrations individually.**
`package.json:316` pins `yarn@1.22.21`, and Storybook states a yarn 4 floor from version 9, hedged as
"While Storybook may still work with older versions". The CLI runs installs and automigrations
through the detected package manager, so a Yarn 1 tree is the least tested path through
`storybook upgrade`, and Yarn 1 hoists flat, which is the shape that breaks a partially upgraded
Storybook tree. Editing the manifest by hand and running `renderer-to-framework`,
`consolidated-imports`, `wrap-getAbsolutePath` and `fix-faux-esm-require` against the configuration
files individually removes the dependency on that path. The dev shell builds yarn from `pkgs.yarn`,
so a package manager upgrade is not a `package.json` edit alone and is not attempted here.

**13. Keep the voting stories. `/voting` and `/voting/registration` stay off the coverage target.**
Catalyst is suspended rather than retired and may return, so the feature stays in the repository. The
two screens stay unreachable and stay excluded, and the container target stays at 49 screens.
`storybook/stories/voting/Voting.stories.tsx` is not deleted: it converts to CSF with the rest of the
corpus in phases 3 and 4, 10 registrations across two `storiesOf` calls, and so does every other
story that renders a component under `source/renderer/app/components/voting/`, which today means
`storybook/stories/governance/Delegation.stories.tsx`. This is a deliberate exception to decision 4
and not an oversight. The four flag-disabled features were judged gone; Catalyst is judged suspended.
The observable state is the same in both cases, a maintained component tree behind no affordance, and
what separates them is a product judgment about the feature rather than anything measurable in the
code.

**14. Remove `/staking/epochs` entirely: route, container, components and story.**
The route is live at `Routes.tsx:196-200`, the navigation item that would reach it is commented out
at `StakingNavigation.tsx:64-67`, and `ROUTES.STAKING.PAGE` is a `goToRoute` target rather than a
`<Switch>` binding, so nothing else can land on the path. Epoch information already reaches users
without it: `components/staking/delegation-center/DelegationCenterHeader.tsx` renders the current
epoch with its slot counts, a countdown to the next epoch (`:154`, `:165-172`) and the sentence at
`:43-45` telling the user when a delegation change takes effect, and `DelegationCenter.tsx:61-67`
mounts it at the top of `/staking/delegation-center`, independent of the deleted page. The only thing
that exists nowhere else is the epoch progress bar at `StakingEpochs.tsx:124-135`, which renders on
the deleted page alone and is unreachable today, so no user loses anything visible.
`StakingEpochsPage.tsx:5-6` renders dummy JSON and reads no store, which is what makes this a stub
rather than a suspended feature, and that is the distinction from decision 13. The commented
navigation item goes with it rather than being left behind as dead commented code.

**15. Delete the `/redeem-itn-wallets` route binding.**
`Routes.tsx:209-213` nests the binding inside `<Route path={ROUTES.STAKING.ROOT}>`, where under
react-router 5 it is only evaluated while the location already starts with `/staking`, so it can
never match. The screen stays reachable and the coverage target does not change, because
`Root.tsx:68-70` mounts `RedeemItnRewardsContainer` from the OS menu path. It goes rather than being
moved or left in place because a route that can never match is a trap for anyone who later tries to
link to the screen: the link resolves, renders nothing, and reports no error.

Settled by evidence and still in force:

- **The webpack 5 builder stays.** The handwritten `swc-loader` rule in `webpackFinal` is kept
  rather than re-derived through `@storybook/addon-webpack5-compiler-swc`, because it already carries
  the four legacy-decorator settings plus the SCSS, inline-SVG, Trezor transport replacement and Node
  polyfill rules the stories depend on.
- **The work proceeds on React 16.14.0.** Storybook's React renderer detects the installed
  `react-dom` at build time and aliases to a React 16 shim. Nothing in the conversion is
  React-version-dependent.
- **Converting `storiesOf()` to CSF is the prerequisite for every possible future, including leaving
  Storybook.** CSF is a documented format that Ladle implements, react-cosmos can consume with a thin
  adapter, and Playwright's gallery pattern maps onto.
- **The hand-maintained barrel is replaced by a glob.** CSF requires the indexer to see real story
  files, and the barrel is what let `Legacy.stories.tsx` go dead with CI green. Sidebar ordering is
  currently implicit in barrel import order and becomes explicit when the barrel goes.
- **The story corpus is 84 files**, 80 under `storybook/stories/` and four beside the components they
  exercise. All four colocated files use `storiesOf()` and all four are in scope.
- **Two cleanups need no discussion.** `storybook-addon-swc` is declared at `package.json:175` and
  referenced nowhere, and `storybook/preview-head.html` is zero bytes. Both are deleted.

**16. Rename the 15 sibling-registering `*.stories.*` files out of the story naming convention, into
`_support/`, in phase 1, before the glob lands in phase 2.**
Fifteen files match the story naming convention and register no stories of their own. They import or
re-export siblings, which is what a support module does, and the convention says otherwise. Under
the phase 2 glob they become indexer inputs, and they fail differently at each end of the hop.

At 6.4.22 the failure is silent and moves the baseline.
`node_modules/@storybook/client-api/dist/cjs/StoryStoreFacade.js:206-225` takes `defaultExport || {}`,
falls back to `autoTitle(fileName, ...)` when there is no title, and registers every remaining named
export as a story. `StakePools.stories.tsx` would gain an auto-titled panel containing
`StakePoolsStory`, and the phase 1 sidebar baseline that every later phase diffs against no longer
matches.

At 8.6 and 10.6 it is a hard build failure. `readCsf` throws `NoMetaError("missing default export")`,
`updateExtracted` records each as `{type: "error"}`, and `getIndex` throws `MultipleIndexingError`.
The `storiesof-to-csf` codemod does not fix them, because they contain no `storiesOf` chain to
transform.

The 15 are the eight under `staking/` (`DelegationCenter`, `DelegationSteps`, `StakePoolsTable`,
`Rewards`, `RedeemItnWallets`, `Undelegate`, `Epochs`, `StakePools`),
`wallets/addWallet/AddWallet.stories.ts` (five bare sibling imports),
`wallets/settings/WalletSettingsScreen.stories.tsx` (whose default export at `:139` is an anonymous
React component, not a CSF meta object), `nodes/updates/DataLayerMigration`,
`nodes/errors/SystemTimeError`, `nodes/errors/NoDiskSpaceError`, `nodes/syncing/SyncingConnecting`
and `nodes/status/Status.stories.ts` (one line: `import './Diagnostics.stories';`).

Renaming is chosen over excluding them by glob, because an exclusion leaves the trap in place for
whoever adds the sixteenth, and over giving each a default export, because that adds roughly 15
sidebar panels nobody asked for.

## Requirements

### Functional Requirements

Cleanup and preparation, on the pinned 6.4.22:

- [ ] Capture a baseline of the sidebar tree before anything changes: 53 panel titles, 15 top-level
      groups and 272 registrations across the 84 story files, 267 of them under `storybook/stories`
      and 5 in the colocated files, so post-conversion labels can be diffed rather than remembered
- [ ] Delete the four flag-disabled story sets and their barrel entries: 5 registrations in
      `wallets/paperWallets/PaperWallets.stories.tsx`, 1 in
      `wallets/legacyWallets/LegacyNotification.stories.tsx`, 2 in
      `wallets/legacyWallets/TransferFunds.stories.tsx`, 1 in `staking/CountdownParty.stories.tsx`,
      and 3 registrations inside `staking/Staking.stories.tsx` at `:92`, `:176` and `:193`
- [ ] Delete `storybook/stories/staking/Legacy.stories.tsx` and the eight components under
      `source/renderer/app/components/staking/legacy/`
- [ ] Remove the `/staking/epochs` screen: the binding at `Routes.tsx:196-200` and its import at
      `:20`, `ROUTES.STAKING.EPOCHS` at `routes-config.ts:13`,
      `containers/staking/StakingEpochsPage.tsx`, the seven files under
      `components/staking/epochs/`, the commented navigation item at `StakingNavigation.tsx:64-67`
      and the `messages.epochs` entry at `:24-28` that was its only reader, both dummy JSON
      fixtures, and `storybook/stories/staking/Epochs.stories.tsx` with the registration that
      imports it at `Staking.stories.tsx:17` and `:172`
- [ ] Delete the `/redeem-itn-wallets` binding at `Routes.tsx:209-213`, its import at `:25` and
      `ROUTES.REDEEM_ITN_REWARDS` at `routes-config.ts:4`
- [ ] Restage the undelegation story as a standalone story of `UndelegateWalletConfirmationDialog`
- [ ] Delete the three orphaned support modules: `storybook/stories/staking/StakingWrapper.tsx`,
      `storybook/stories/wallets/_utils/defaultWalletProps.tsx`,
      `storybook/stories/wallets/_utils/HardwareWalletWithNavigationLayout.tsx`
- [ ] Remove `storybook-addon-swc` from `package.json` and delete `storybook/preview-head.html`
- [ ] Set `tsconfig.json` `useDefineForClassFields` to `false` in a standalone commit
- [ ] Replace `stories: ['../storybook/stories/index.ts']` with a glob, rehome the barrel's global
      SCSS import and `environment` side effect into `preview`, and make sidebar ordering explicit

The 8.6.x hop:

- [ ] Verify Storybook 8.6.x renders this component tree at React 16.14.0 in a scratch worktree
      before the hop branch is opened, including `react-polymorph` through `StoryDecorator`'s
      `ThemeProvider` and one MobX observing component
- [ ] Bump `@storybook/addon-knobs` from the pinned 6.4.0 (`package.json:85`) to 8.0.1 as part of the
      manifest edit, and confirm in the same pre-flight that `withKnobs` is still callable as a plain
      function at 8.0.1
- [ ] Decide the story-signature rule for the two shapes the corpus mixes, before the codemod runs
- [ ] Delete the `StoryWrapper` prop pass-through once its last consumer is gone, in phase 3,
      matching the Implementation Strategy and `storybook-modernization-tasks.json`
- [ ] Convert every remaining story file from `storiesOf()` to Component Story Format, seeded by
      `storiesof-to-csf`, `csf-hoist-story-annotations` and `csf-2-to-3` from the 8.6.x CLI
- [ ] Rewrite by hand the 9 dynamic `.add()` registrations in `staking/Staking.stories.tsx` that the
      codemod cannot take
- [ ] Check each sanitizer-renamed export against the baseline sidebar labels, 80 of them measured
      before the phase 1 deletions
- [ ] Add `framework: { name: '@storybook/react-webpack5' }`, rename the two CLI binaries in
      `package.json:55-56` to `storybook dev` and `storybook build` while keeping the script names
      `storybook` and `storybook:build`, and remove `@storybook/builder-webpack5`,
      `@storybook/manager-webpack5`, `@storybook/core` and `@storybook/addons`
- [ ] Replace `storybook/addons/DaedalusMenu/` with `globalTypes` toolbar entries, `initialGlobals`
      and a decorator reading `context.globals`, preserving nine themes, two locales and three
      operating-system profiles including the per-OS minimum window heights of 641px, 660px and 700px
- [ ] Preserve the write path from a story back to the toolbar used at
      `storybook/stories/settings/general/General.stories.tsx:97`
- [ ] Migrate every story function that reads `props.currentTheme`, `props.osName` or `props.locale`
      to read from story context, in the same edit that converts that file to CSF
- [ ] Keep `@storybook/addon-knobs` installed and working across the hop, bumping it from the pinned
      6.4.0 (`package.json:85`) to 8.0.1 as part of the hop
- [ ] Remove `@dump247/storybook-state` **before** the hop, in phase 2, replacing its 17 call sites
      with a local stateful wrapper component that touches no Storybook API

Knobs and story state, on 8.6.x:

- [ ] Convert every knob call site to `args` and `argTypes`
- [ ] Convert the 17 local-wrapper call sites introduced in phase 2 to `useArgs` by default and
      `useState` by exception, with the exceptions listed in one place, and delete the wrapper
- [ ] Remove `@storybook/addon-knobs` from `package.json` once the last call site is gone

The 10.6.x bump:

- [ ] Move `tsconfig.json` `moduleResolution` off `node` in its own commit ahead of the bump
- [ ] Convert `storybook/main.ts` to strict ESM, replacing `require`, `__dirname` and
      `require.resolve`
- [ ] Run `renderer-to-framework`, `consolidated-imports`, `wrap-getAbsolutePath` and
      `fix-faux-esm-require` individually, without `storybook upgrade`

Screen coverage:

- [ ] Build a store fixture harness supplying all 24 `StoresMap` keys with observable-default-shaped
      objects, request-shaped objects, a `MemoryRouter` with a matching `RouterStore` stub, and an
      `AnalyticsProvider` with a no-op tracker, as an extension of
      `storybook/stories/_support/StoryProvider.tsx`
- [ ] Ship a container story for each of the 49 router-reachable screens, mounted through a real
      `<Provider stores actions>` rather than through props
- [ ] Cover each of the 12 multi-state screens with a story per meaningful state listed in
      `research/05-reachable-screens.md` section 3.5
- [ ] Close the five screens with no story at either layer: `/settings/general`,
      `/profile/terms-of-service`, `LoadingPage`, `RedeemItnRewardsContainer` and the Toggle RTS
      Flags dialog

Guardrails and documentation:

- [ ] Add `eslint-plugin-storybook` to `package.json` and `.eslintrc`
- [ ] Update `.agent/skills/storybook-creation/SKILL.md` and `.agent/workflows/storybook.md` to teach
      CSF, args, globals and the container harness, and to describe the directory layout that exists

### Non-Functional Requirements

- `yarn storybook:build` must pass on every commit that lands on the trunk, not only at the end of
  the work. The one landing that cannot be decomposed into smaller green commits is the 8.6.x hop,
  and the mechanism that keeps the trunk green across it is stated in the Testing Strategy.
- `yarn compile` must stay clean. Stories are in the `tsc` program because `tsconfig.json:103`
  declares only `"exclude": ["node_modules"]`, and `yarn compile` is a required check at
  `perSystem/checks.nix:55`. This is a real guarantee today and the conversion must not weaken it.
  It is also the constraint that forces the hop to be atomic: a file still importing `storiesOf` from
  `@storybook/react` fails `tsc` the moment the package is at 8.6.x, whether or not that file is
  indexed.
- `yarn lint` must not regress. `storybook/` is in scope and is a required check at
  `perSystem/checks.nix:54`.
- The four legacy-decorator settings must survive every configuration change: `tsconfig.json:17`
  `experimentalDecorators`, `storybook/main.ts:79` SWC parser `decorators`, `storybook/main.ts:84`
  `legacyDecorator`, and `storybook/main.ts:87` `useDefineForClassFields`. MobX 5 breaks without them
  and the failure is silent under some build paths.
- Theme, locale and operating-system switching must remain a global toolbar control that persists
  while the reviewer browses between components. Reducing it to a per-story control removes the
  localization review workflow, which is the only place in the repository where a translator sees
  Japanese copy inside the real component at the real width without building Daedalus and running a
  node.
- No container story may construct a real store. `setUpStores` (`source/renderer/app/stores/index.ts:94-143`)
  takes a live `Api` and calls `initialize()`, which starts the polling reactions
  (`source/renderer/app/stores/lib/Store.ts:31-35`).
- No task may increase the count of `@ts-ignore` directives in story files above the current 229.
  New container stories are written without them.
- The frozen clock in `storybook/preview.tsx` (`Sat, 01 Jan 2022 10:00:00 GMT`) must survive the
  preview rewrite, or date-formatting output becomes unstable across runs.
- Every phase boundary must be a revertible state.

## Technical Design

### Components Affected

- `storybook/main.ts`: `framework` field, `stories` glob, addon list, local addon resolution, and ESM
  conversion at the 10.6.x bump. The `webpackFinal` body is preserved, including the SWC rule, SCSS
  modules with the `[name]_[local]` local ident, `.inline.svg` via svg-inline-loader, the two
  `NormalModuleReplacementPlugin` rules redirecting `@trezor/transport` away from its Node USB and
  UDP transports, the nine Node polyfills and the five stubbed modules.
- `storybook/preview.tsx`: becomes a default-export preview object carrying `decorators`,
  `globalTypes`, `initialGlobals` and `parameters`, and absorbs the global SCSS import and
  `environment` side effect currently at `storybook/stories/index.ts:1-2`.
- `storybook/stories/index.ts`: deleted once the glob replaces it.
- `storybook/stories/_support/StoryWrapper.tsx`: reads theme, locale and OS from story context
  instead of subscribing to the addon channel. Every story depends on this file, so it changes once
  and keeps a temporary prop pass-through until the last consumer is migrated.
- `storybook/stories/_support/StoryProvider.tsx`: extended from three partial stores
  (`StoryProvider.tsx:220-264`) to the full 24-key map, and re-exported as the container harness. It
  already mounts the real `<Provider>` with the real actions at `:273-277`, the
  `BrowserLocalStorageBridge` at `:278` and the `DiscreetModeFeatureProvider` at `:279`, so this is a
  continuation rather than a new mechanism.
- `storybook/stories/_support/StoryDecorator.tsx`, `StoryLayout.tsx`: converted to CSF decorators.
  The logic inside them is unchanged.
- `storybook/addons/DaedalusMenu/`: deleted, four TypeScript files and a stylesheet, along with the
  `daedalusMenu/updateParam` and `daedalusMenu/paramUpdated` channel protocol, the `sessionStorage`
  persistence and the `parent.window.location.hash` manipulation.
- Every remaining story file: shape, knobs, state and context reads.
- A new `storybook/stories/screens/` tree, one file per screen, grouped by the domains the route
  configuration already uses.
- `source/renderer/app/components/staking/legacy/`: eight components, deleted.
- `source/renderer/app/Routes.tsx`: two bindings deleted, `/staking/epochs` at `:196-200` and
  `/redeem-itn-wallets` at `:209-213`, with the imports at `:20` and `:25` that nothing else uses.
  `source/renderer/app/routes-config.ts` loses `STAKING.EPOCHS` at `:13` and `REDEEM_ITN_REWARDS` at
  `:4`.
- `source/renderer/app/containers/staking/StakingEpochsPage.tsx`, the seven files under
  `source/renderer/app/components/staking/epochs/` and the two fixtures
  `source/renderer/app/config/stakingCurrentEpoch.dummy.json` and `stakingPreviousEpoch.dummy.json`:
  deleted, nothing else imports any of them.
  `source/renderer/app/components/staking/navigation/StakingNavigation.tsx` loses the commented
  navigation item at `:64-67` and the `messages.epochs` entry at `:24-28`.
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json` and `translations/messages.json`:
  regenerated when the epochs components go, twelve message ids lighter.
- `package.json`: the Storybook dependency set, the two scripts at lines 55-56, and removal of
  `storybook-addon-swc` (line 175), `@storybook/addon-knobs` (line 85) and `@dump247/storybook-state`
  (line 79).
- `tsconfig.json`: `useDefineForClassFields` early, `moduleResolution` at the 10.6.x bump.
- `perSystem/checks.nix`: unchanged in structure. The `storybook` check at line 78 runs
  `yarn storybook:build`, so the check keeps working as long as the script name is stable, which it
  is.
- `.eslintrc`: `eslint-plugin-storybook`, at the end.
- `.agent/skills/storybook-creation/SKILL.md` and `.agent/workflows/storybook.md`.
- `source/renderer/app/containers/**`: read, never modified, apart from
  `staking/StakingEpochsPage.tsx`, which is deleted.
- `tests/` and `source/**/*.spec.ts[x]`: not affected. No Jest spec imports a story file.

### Data / IPC / API Changes

None. There are no IPC channels, no cardano-wallet integrations, no main-process boundaries and no
shared type contracts in this work. Four internal contracts change.

**The story indexing contract.** Today Storybook is handed one module,
`storybook/stories/index.ts`, and story registration happens as a side effect of importing it. The
sidebar tree is built from the `storiesOf()` titles encountered during that import, and the order is
the order the barrels import in. Under a glob, Storybook discovers real story files and reads a
default export from each. The consequence to manage is ordering: what is implicit in import order
today becomes explicit, through the `title` values and a `storySort` in preview parameters. The
consequence to gain is that an unreferenced story file becomes a build participant rather than a
silent absence, which is the defect that let `Legacy.stories.tsx` die unnoticed.

**The globals contract.** DaedalusMenu currently pushes selections across the manager and preview
process boundary over a hand-rolled channel: the toolbar emits `daedalusMenu/updateParam`,
`storybook/addons/DaedalusMenu/index.ts:12-14` re-emits `daedalusMenu/paramUpdated`, and
`StoryWrapper.tsx:40-41` subscribes and pushes its initial state back, so the toolbar starts in sync.
Under `globalTypes`, Storybook owns that transport, persists the selection itself and URL-encodes it,
which also replaces the handwritten `sessionStorage` write and hash manipulation at
`DaedalusMenu.tsx:53-62`.

**The story signature contract, which is the one a mechanical transform misses.**
`StoryWrapper.tsx:77-81` passes `osName`, `locale` and `currentTheme` to the story as props. That is
why so many story functions are written `(props) => ...` or `(_, props) => ...` and read
`props.currentTheme`. Modern globals arrive through story context, not props, so every one of those
functions changes shape. Four files carry comments recording the dependency:
`governance/DRepDetail.stories.tsx:67`, `governance/DRepDirectory.stories.tsx:248`,
`loading/mithril/MithrilPartialSyncDialogue.stories.tsx:30` and
`nodes/status/Diagnostics.stories.tsx:88`. The exact population is counted before the hop branch
opens. The transition is managed rather than cut over: `StoryWrapper` changes once to read from
`context.globals` and keeps passing the same three values as props while stories are still reading
them, each tranche moves its stories to context, and the pass-through is deleted in the same change
that deletes the last consumer, which is a grep rather than a judgment call.

**The store injection contract, which is new.** Container stories mount a real
`<Provider stores actions>`, not a props object. mobx-react 6.3.1 does prefer explicit props over the
Provider, because `grabStoresByName` returns early for any store name already present in
`nextProps`, and that is how the one existing container story works. Two things defeat the props
route. `AnalyticsConsentPage.tsx:10-11` reads `MobXProviderContext` directly through
`hooks/useStores.ts:5-7` and has no prop fallback. And a props fixture stops at the first nested
container, which then receives nothing: `WalletSettingsPage` renders eight nested containers,
`WalletAddPage` five, `LoadingPage` five and `MainLayout` three. Only a real Provider renders the
subtree, which also means a parent screen's fixture must satisfy every child's store reads.

### UI / Store / Process Changes

No MobX store, container, theme file or i18n message changes.

**Story authoring shape.** A file gains a default export carrying `title` and component-level
annotations, and one named export per story:

```tsx
export default {
  title: 'Wallets|Send',
  decorators: [WalletsWrapper],
};

export const Confirmation = (_args, context) => (
  <WalletSendConfirmationDialog currentTheme={context.globals.currentTheme} />
);
Confirmation.storyName = 'Confirmation dialog';
```

**Knob to arg conversion.** A knob is a function call evaluated inside the render body on every
render. An arg is a static declaration outside it that arrives as a prop. That difference, not the
knob type, is what determines whether a call site converts mechanically:

```tsx
// today
export const Basic = () => <Button label={text('Label', 'hello')} />;

// converted
export const Basic = (args) => <Button {...args} />;
Basic.args = { label: 'hello' };
```

The 396 sites are every knob call under `storybook/stories`, support modules included, plus the two
in the colocated story files under `source/`. Four types account for 386 of them: `boolean` 182,
`number` 100, `text` 55, `select` 49. The remaining ten are `date` 3, `radios` 3, `button` 2,
`object` 1, `optionsKnob` 1. Three of those carry
judgment rather than a rename: `date` knobs return a timestamp number while the `date` control
returns a `Date`, so the call site adjusts; `optionsKnob` maps to `check`, `inline-check`, `radio` or
`select` depending on its `display` config; and `button` has no arg equivalent at all, because args
are values and not actions, so each of the two sites is either dropped or moved into the story body.

Position matters more than type. 204 of the 396 knob call sites sit at an indentation of eight
spaces or deeper, meaning they are nested inside a callback, a mapped list or JSX rather than at the
top of a story body; the other 192 sit at the top of a story body and are a two-line mechanical
edit. `research/02-storybook-upgrade-path.md` reports the same 204, against its own scope of the 394
sites under `storybook/stories`.

Two further undercounts sit on top of that. `storybook/stories/loading/_support/loadingKnobs.ts`
wraps five knob functions, and those wrappers are called at **44 further sites across 7 files** under
`storybook/stories/loading/`. Each becomes an arg and none appears in either count. And the phase 1
deletions remove **30** direct sites: `wallets/_utils/defaultWalletProps.tsx` 15,
`legacyWallets/TransferFunds.stories.tsx` 6, `staking/CountdownParty.stories.tsx` 5,
`legacyWallets/LegacyNotification.stories.tsx` 2, `staking/Epochs.stories.tsx` 2.

Net phase 4 surface after the phase 1 deletions is 396 less those 30, so **366 direct plus 44
indirect, about 410**. At the rate `storybook-modernization-tasks.json` carries, that is roughly 15
hours of previously unbudgeted work, and it compounds with the rate question in the effort section.
A nested knob has no mechanical arg equivalent: the value must be hoisted to the story's signature
and threaded down, which changes the surrounding code. There is no tooling for either half. One of
the three `date` sites goes with the phase 1 deletions.

**`withState` conversion.** 17 call sites across 10 files use `withState(initialState, (store) => JSX)`
where the body reads `store.state` and calls `store.set`. Both replacements, `useArgs` and `useState`
from the preview API, are called inside the story rather than wrapping it, so the story signature
changes in every case. 8 of the 10 files also carry knobs, so both rewrites land on the same story
bodies and are done in one pass per file.

### The container story harness

The container estimate rests on this design.
`research/05-reachable-screens.md` sections 6 and 7 establish that one harness can serve nearly every
container, because every container reads the same shape: one `StoresMap`
(`source/renderer/app/stores/index.ts:58-83`, 24 keys), one `ActionsMap`, one `@inject` decorator,
and no container constructs a store or an API client of its own. Mean store dependency is 3.2 keys
per screen and the maximum is 9 (`WalletSummaryPage`). Four stores dominate: `app` is read by 35 of
the 48 screen containers, `profile` by 23, `networkStatus` by 20, `wallets` by 16. Three stores,
`walletBackup`, `walletsLocal` and `window`, are read by no screen at all.

The harness is six things, and five of them are additive over what `StoryProvider.tsx` already does:

1. A plain object keyed by the 24 `StoresMap` names, each defaulting to an object whose fields are
   the observable defaults of the real store rather than a real store instance.
2. The real `ActionsMap` from `source/renderer/app/actions`, which `StoryProvider.tsx:9` already
   imports and which is inert without listeners, or a recorded stand-in where a story should log
   triggers.
3. Request-shaped defaults wherever a container reads `...Request.isExecuting`, `.error`,
   `.wasExecuted`, `.result` or `.isExecutingFirstTime`. This appears on roughly a dozen screens,
   `InitialSettingsPage.tsx:39` and `WalletSummaryPage.tsx:137` among them.
4. A `MemoryRouter` seeded with the screen's path and a `RouterStore` stub whose `location` matches
   it, so `MainLayout.tsx:100` and `Settings.tsx:21-28` agree with what `withRouter` supplies. Five
   screens use `withRouter`, and `DRepDetailPage.tsx:56` reads `match.params.drepId`.
5. An `AnalyticsProvider` with a no-op tracker, for the three `withAnalytics` screens:
   `WalletSummaryPage`, `WalletSendPage` and `StakePoolsListPage`.
6. The existing `DiscreetModeFeatureProvider` and `BrowserLocalStorageBridge`.

Four screens need setup beyond a store override and are the named exceptions: `SecuritySettingsPage`
reads no stores and needs only the discreet-mode context (`SecuritySettingsPage.tsx:7`);
`AnalyticsConsentPage` needs a real Provider rather than props; `DRepDetailPage` needs a router
seeded with a `:drepId` and a resolved `fetchDRep` promise (`DRepDetailPage.tsx:56-65`); and
`LoadingPage` keys off `backend.loadingPhase` (`LoadingPage.tsx:41-88`), a field with no
representation in the current `StoryProvider`.

The harness is built incrementally across the first five screen tranches, each tranche's additions
being a superset of the one before, so that the mechanism is proved on the screens with the fewest
dependencies before it carries the wallet screens.

### Screen coverage target

49 screens across four mounting mechanisms: 26 route destinations, 5 layout shells that own a route,
9 full-surface screens mounted outside the router by `Root.tsx` and `LoadingPage`, and 9 overlays and
chrome mounted above the router by `App.tsx` and `MainLayout.tsx`. 48 containers back them;
`DRepDirectoryPage` serves two screens because it is bound to both `/governance/dreps` and
`/governance/favorites` and branches on `location.pathname` at `DRepDirectoryPage.tsx:133-137`.

Eight route destinations are excluded as unreachable today, each with evidence recorded in
`research/05-reachable-screens.md` section 4: `/staking/info` and `/paper-wallet/create-certificate`
and the legacy wallet migration flow behind compile-time flags, `/staking/countdown` behind a runtime
condition no shipping network satisfies, `/staking/epochs` and `/voting` and
`/profile/data-layer-migration` behind a missing affordance, and `/redeem-itn-wallets` behind a route
binding nested inside `<Route path={ROUTES.STAKING.ROOT}>` that cannot match under react-router 5.
`RedeemItnRewardsContainer` stays on the list regardless, because `Root.tsx:68-70` mounts it from the
OS menu path.

Phase 1 removes two of those eight from the repository rather than carrying them as exclusions, under
decisions 14 and 15, which leaves six: `/staking/info`, `/paper-wallet/create-certificate`, the
legacy wallet migration flow, `/staking/countdown`, `/voting` with `/voting/registration`, and
`/profile/data-layer-migration`. Neither removal moves the coverage target. `/staking/epochs` was
never on it, and `RedeemItnRewardsContainer` keeps its place because the OS menu path is untouched.

One pair of outcomes reads as an inconsistency and is not.
`storybook/stories/staking/Epochs.stories.tsx` is deleted, while
`storybook/stories/voting/Voting.stories.tsx` is kept and converted, and the two screens were
excluded on the same evidence, a route with no affordance. Decisions 13 and 14 carry the reasoning:
`StakingEpochsPage.tsx:5-6` renders dummy JSON and reads no store, so there is no feature behind the
missing affordance to come back, whereas the voting screens are a maintained feature judged
suspended rather than retired.

Seventeen component-level stories would otherwise be needed to close the same screen gaps at the
component layer. Fourteen of them are subsumed by the container target, because the state each would
demonstrate is a named state of a screen that now has its own story. The other three fall out with
routes no affordance reaches:

| Component-level gap | Now covered as |
|---|---|
| `GeneralSettings` | `/settings/general` container story |
| `LoadingOverlay` | `RedeemItnRewardsContainer` loading state |
| `ToggleRTSFlagsDialog` | `ToggleRTSFlagsDialogContainer` story |
| `TopBarLayout` | the three profile screens that render it |
| `TermsOfUseForm` | `/profile/terms-of-service` container story |
| `WalletBackupDialog` | `/wallets/add` backup branch |
| `LoadingSpinner`, `RestoreNotification` | `/wallets` shell, no-active-wallet and restoring states |
| `WalletTransactionsList`, `WalletNoTransactions` | `/wallets/:id/summary` states |
| `StakingUnavailable` | `/staking` shell not-synced state |
| `DelegationSetupWizardDialog` | delegation center and stake pools dialog states |
| `StakePoolsRankingLoader` | `/staking/stake-pools` ranking state |
| `LoadingPage` | `LoadingPage` container story, five branches |
| `CenteredLayout` | dropped; its screen, `/profile/data-layer-migration`, is unreachable |
| `VotingNoWallets`, `VotingRegistrationDialog` | not added; `/voting/registration` stays unreachable under decision 13 |

**The 49 screens are not 49 stories.** Twelve screens carry between two and six meaningful states
each: `Root`, `LoadingPage`, the `/wallets` shell, `WalletAddPage`, `WalletSummaryPage`,
`WalletSettingsPage`, the `/staking` shell, `DelegationCenterPage`, `StakePoolsListPage`,
`DRepDirectoryPage`, `DRepDetailPage` and `VotingGovernancePage`. A planning figure of 90 to 120
stories for full state coverage is reasonable and is an estimate, sensitive to how much dialog state
is folded into a parent's stories rather than given its own.

### What codemods do and do not cover

The `storiesof-to-csf` transform converts chained
`storiesOf(title, module).addDecorator(d).add(name, fn)` into a default export carrying `title` and
`decorators` plus one `export const` per story. Its own source comment states the limit: "NOTES: only
support chained `storiesOf()` calls". It requires a string-literal title and string-literal story
names, and skips any file that already has a default export.

Eligibility against this estate is unusually high, measured before the phase 1 deletions. All 65
files calling `storiesOf` under `storybook/stories` use a string-literal title, none of them already
has a default export, and no file registers stories inside a `forEach` or `map` loop. Of 267 `.add()`
registrations under that directory, 258 use a string-literal name; the 9 that do not are all in
`staking/Staking.stories.tsx` and are hand work. 80 story display names contain characters that are
not letters, digits or spaces, so the sanitizer renames the export and emits an explicit display name
for each. That is correct behavior, and it means 80 hand-checks that the sidebar label survived.

**The `withKnobs(story, context)` idiom is not the documented usage, and 22 sites depend on it.**
`storybook/stories/wallets/_utils/WalletsWrapper.tsx:9` is
`const storyWithKnobs = withKnobs(story, context)`, calling the decorator as a plain function rather
than registering it, and 21 more sites across `loading/mithril/`, `news/`, `nodes/` and `wallets/` do
the same. `package.json:85` pins `@storybook/addon-knobs` at **6.4.0**, not the 8.0.1 the
compatibility analysis is written against, and the hop between those two versions was not previously
stated as a requirement. Whether 8.0.1 still permits this call shape could not be determined from
the registry and is not safe to assume. It goes into the pre-flight worktree, because discovering it
inside phase 3 means discovering it inside the one landing that cannot be decomposed.

**The corpus mixes two story signatures and they are not equivalent.** Some story functions read
`(_, props) => props.currentTheme` (for example `navigation/Sidebar.stories.tsx:116`), which works
today because `__isArgsStory` makes the second argument the story context. Others read
`(props) => props.currentTheme` (for example `navigation/SidebarWalletsMenu.stories.tsx:85`), where
`props` is the args object and `props.currentTheme` is already `undefined`. Converting the second
shape "correctly" is a behavior change that a visual diff will flag as a regression, so the rule for
which shape means what is decided once, before the codemod runs, rather than per file during
hand-finishing.

**Four of those labels are control flow, not labels, and are checked separately.** Three files branch
on the story context's `kind` and `story` strings at render time:

- `storybook/stories/wallets/_utils/WalletsWrapper.tsx:14` reads
  `context.story !== 'Empty' && context.story !== 'Wallet Add'` to decide whether to wrap the story
  in `WalletWithNavigationLayout`.
- `storybook/stories/settings/utils/SettingsWrapper.tsx:29` passes `context.kind` to `linkTo` and
  `:31` reads `context.story`.
- `storybook/stories/wallets/_utils/WalletWithNavigationLayout.tsx:32` reads `context.kind`.

These survive the version hop: at 10.6.0 the story store still sets `kind: componentAnnotations.title`
and `story: storyAnnotations?.name`, marked "Back compat". The risk is the conversion, not the
version. If the sanitizer renames any of those four strings and the emitted `storyName` differs by so
much as a character, the layout or the navigation link changes silently and `storybook:build` stays
green. They are listed by name in the phase 3 tranche that touches them and checked as behavior,
not as text.

The transform emits CSF 1 shape, so the documented chain runs `csf-hoist-story-annotations` and then
`csf-2-to-3` after it. All three exist at 8.6.x and only `csf-2-to-3` survives into 9.0.0 and 10.0.0.
Running the chain from the version being landed on, rather than through an unversioned side channel,
is one of the two reasons the route stops at 8.6.x.

**There has never been a codemod for knobs, at any tag inspected.** The official migration is prose
with worked examples. Any estimate that assumes automated knob conversion is wrong. There is likewise
no transform for `withState`, for the DaedalusMenu replacement, for the story-signature change, or
for anything in the container work.

## Implementation Strategy

Eight phases. The sequencing principle is that everything version-neutral lands before the version
moves, the version moves exactly twice, and the container work happens last because it is written
once against the final API rather than twice.

1. **Baseline, deletions and version-neutral changes, on the pinned 6.4.22.** Record the sidebar
   tree before anything changes. Delete the four flag-disabled story sets, `Legacy.stories.tsx` and
   the eight staking legacy components, the three orphaned support modules, `storybook-addon-swc` and
   `storybook/preview-head.html`. Remove the `/staking/epochs` screen and the dead
   `/redeem-itn-wallets` route binding. Restage the undelegation story. Set
   `useDefineForClassFields` to `false`. Count the story functions reading `props.currentTheme`,
   `props.osName` or `props.locale`. Every file removed here is a file not converted later.
2. **Indexing and the `@dump247` removal, on 6.4.22.** Replace the barrel with a glob and make
   sidebar ordering explicit. This is the change that makes an unreferenced story file a build
   failure instead of a silent absence. Rename the 15 sibling-registering files out of the story
   naming convention into `_support/` before the glob lands, per locked decision 16. Replace the 17
   `withState` call sites with a local stateful wrapper component and remove
   `@dump247/storybook-state`.

   The global SCSS import and the `environment` side effect need no rehoming:
   `storybook/preview.tsx:4` and `:6` already import both.

   **Why `@dump247/storybook-state` cannot wait for phase 4.**
   `node_modules/@dump247/storybook-state/dist/index.js:11` is `import addons from '@storybook/addons'`
   and `:157` calls `addons.getChannel()` inside `withState()`, at module-evaluation time, because
   every one of the 17 call sites is evaluated when its story module loads. Its `package.json` peers
   `@storybook/addons: "^3.2.16"`. Phase 3 removes `@storybook/addons`, and there is no version of it
   in the 8 line: the latest is 7.6.17. Neither escape works. Keeping 6.4.22's copy fails because
   `node_modules/@storybook/addons/dist/cjs/index.js:143` keys its singleton on
   `KEY = '__STORYBOOK_ADDONS'` and `:49-56` throws `Accessing non-existent addons channel` when
   `setChannel` was never called, which Storybook 8 never does. Installing 7.6.x fails because its
   `module.exports` is a named re-export set with no default export carrying `getChannel`, so
   `addons.getChannel` is `undefined`, and it drags a 7.x runtime into a Yarn 1 flat-hoisted tree,
   which is the exact shape locked decision 12 identifies as what breaks a partially upgraded
   Storybook.

   The replacement is a local component holding React state and passing it down, with no Storybook
   API surface at all. It is version-agnostic, works identically on 6.4.22 and 8.6.x, and is deleted
   in phase 4 when those sites become `useArgs`. Roughly 17 mechanical edits. This keeps phase 3's
   separation intact, which is the entire reason the 8.6.x stopover exists.
3. **The 8.6.x hop.** One branch, one landing. Pre-flight the version in a scratch worktree, then on
   the branch: the manifest and `framework` field, the CLI binary rename, the preview default export,
   the DaedalusMenu replacement, the codemod pass, five hand-finish tranches by domain, the sidebar
   label diff, and the removal of the `StoryWrapper` prop pass-through. Knobs and `withState` are not
   touched. Each story body is edited once, for both its shape and its context reads.
4. **Knobs and story state, on 8.6.x, in tranches by domain.** 366 direct knob call sites after the
   deletions plus 44 indirect through `loading/_support/loadingKnobs.ts`, of which 204 of the 396
   measured before the deletions sit nested rather than at the top of a story body, and the 17
   local-wrapper sites introduced in phase 2, with the knob and state rewrites done together in the
   8 files that carry both. Delete `@storybook/addon-knobs` and the local wrapper when the last call
   site is gone.
5. **The 10.6.x bump.** `moduleResolution` in its own commit, then `main.ts` to strict ESM, then the
   manifest bump with the four automigrations run individually. This is the phase where the 9.1.x
   fallback is taken if it is going to be taken.
6. **Container harness and screen tranches 1 to 5, 29 screens.** Build the store map, then add
   request-shaped defaults, then `backend`, then the feed stores, then the router stub and the
   analytics provider. Every harness mechanism exists by the end of this phase.
7. **Screen tranches 6 to 8, 20 screens.** Wallets, staking, governance and voting. These introduce
   fixture data against a fixed wrapper, not new mechanisms. Tranche 6 opens with a single
   wallet-scale proof story before the rest of the tranche is committed to.
8. **Guardrails and documentation.** `eslint-plugin-storybook`, and rewriting
   `.agent/skills/storybook-creation/SKILL.md` and `.agent/workflows/storybook.md`. An automated
   render check is out of scope, per locked decision 7.

### Effort, and where the error bars are

527 hours across 61 tasks. Phases 3 and 4 hold 234 of them and phases 6 and 7 hold 202.

**That figure is provisional.** Two things move it, both known before any work starts.

- The knob reconciliation above adds roughly **15 hours** to phase 4, because the surface is about
  410 sites rather than the 350 the task graph's tranches were rated against.
- `task-050`'s own acceptance includes "task-052 is re-estimated against it", and task-052 is 36
  hours already inside the 527. The estimate contains a task whose job is to correct part of the
  estimate.

The declared critical path totals 323 hours across 35 tasks, and it is the longest chain through the
dependency graph. At both points where the graph offers a choice it takes the longer branch:
`task-018` depends on `task-016` (14h) as well as `task-017` (4h), and `task-061` depends on
`task-060` (8h) as well as `task-059` (4h). The DaedalusMenu replacement and the documentation
rewrite are therefore both on it.

**The harness core carries no risk premium over a mechanical task.** `task-039`, the container
harness core, is priced at 14 hours, the same as `task-016`, a toolbar-addon swap with a fully
specified target. The task graph's own `riskAreas` names task-039's territory as the single biggest
unknown in the project. A number given to anyone outside this team should carry a range on that task
or say which parts are provisional.

The container half does not scale linearly with screen count.
`research/05-reachable-screens.md` section 7 establishes that one shared harness can serve
nearly every container, because every container reads the same `StoresMap` through the same `@inject`
decorator and none constructs a store or an API client of its own. Four screens need bespoke setup,
and they are named: `SecuritySettingsPage`, `AnalyticsConsentPage`, `DRepDetailPage` and
`LoadingPage`. The 202 hours in phases 6 and 7 rest on that verdict. If each container needed its
own wiring instead, the figure is wrong by a wide margin rather than by a little.

The error bars are wide anyway, for five reasons that have nothing to do with per-container wiring.

- **The harness is the schedule.** Tranches 1 to 5 build every mechanism. If the store map, the
  router stub and the analytics provider land cleanly, the remaining 20 screens are fixture data. If
  they do not, every screen pays the cost again.
- **The prior art is one 19-line literal** covering the simplest container in the application. Nothing
  in the corpus demonstrates the pattern at wallet-screen scale, which is why tranche 6 opens with a
  proof story rather than a plan.
- **`strict: false` gives no help.** `tsconfig.json:79-85` disables `strict` and `noImplicitAny`, and
  `types/injectedPropsType.ts:6-10` types `stores` as `any | StoresMap`. An incomplete fixture
  compiles, and the first signal is a runtime failure in the story.
- **Request objects are the most likely repeated failure.** Containers read `isExecuting`, `error`,
  `wasExecuted`, `result` and `isExecutingFirstTime` through without guarding, so an omitted field
  throws inside `render` rather than degrading. Cheap to fix once in the harness default, expensive
  to fix twelve times.
- **The story count is an estimate of an estimate.** 90 to 120 stories for full state coverage of 49
  screens, sensitive to how much dialog state is folded into a parent's stories.

On the conversion half, the dominant uncertainty is that the estimate is built from call-site counts
rather than from decisions, and the roughly 200 nested knob call sites have no mechanical arg
equivalent.

## Testing Strategy

257 existing story registrations are being rewritten, the 272 baseline less the 15 the phase 1
deletions remove, and 90 to 120 new ones written, in a repository with no visual regression coverage
and no working end-to-end suite. What each check does and does not prove therefore matters.

**`yarn storybook:build` (`perSystem/checks.nix:78`).** Required, wrapped `x86_64-linux` only. It is
the acceptance gate for every landing, not just the final one. What it proves is that every component
reachable from a story compiles and bundles through the real webpack pipeline, which catches broken
imports, circular dependencies and provider-contract violations that Jest's module mocking hides.
What it does not prove is that anything renders correctly, or even that it renders at all. Today it
also says nothing about story files outside the barrel graph, which is why phase 2 comes before phase
3: the glob makes the check's coverage match the corpus.

**`yarn compile` (`perSystem/checks.nix:55`).** Required. Stories are in the `tsc` program because
`tsconfig.json:103` declares only `"exclude": ["node_modules"]`. That is the guarantee the conversion
leans on, and it is also the constraint that shapes phase 3, because it reaches every story file
whether or not the indexer does. Two qualifications belong on the record: `tsconfig.json:79-85` sets
`"strict": false` and `"noImplicitAny": false`, so a prop object can omit required fields and pass;
and 229 `@ts-ignore` directives across 48 story files suppress errors nobody has read. The conversion
cannot silently break the type contract between a story and its component. It can silently break
whatever those 229 suppressions are hiding, and it gives no help at all on container fixtures.

**`yarn lint` (`perSystem/checks.nix:54`).** Required, and `storybook/` is in scope. It currently
gates nothing there: 124 files linted, 0 errors, 457 warnings. `eslint-plugin-storybook` arrives in
phase 8 and makes a story authored against the old APIs a lint failure.

**Per-tranche manual pass.** After each tranche builds, open the workbench and walk the tranche's
sidebar panels. Confirm the panel titles and story labels match the phase 1 baseline, confirm the
toolbar still switches all nine themes, both locales and all three OS profiles, and confirm the
converted args render controls that actually change the component. For a container tranche, confirm
the screen renders its real content rather than an empty shell, which is the failure mode a fixture
with a missing field produces. This is the only step that catches a story that builds and renders
blank.

**No render smoke check.** Per locked decision 7, `@storybook/test-runner` is out of scope because it
requires a Playwright browser the offline Nix sandbox cannot obtain. Nothing replaces it in this
plan. The honest statement of what the required checks prove after this work is: `yarn compile`
proves every story file type-checks, `yarn lint` proves it parses and satisfies
`eslint-plugin-storybook`, and `yarn storybook:build` proves the corpus bundles. None of them
evaluates a preview module, so none of them proves a story renders. That is the same guarantee the
repository has today, over a larger and better-organized corpus.

**Jest.** Unaffected. No Jest spec imports a story file. The suite is run as a regression check, not
as verification of this work.

**Cucumber.** Not usable. The unit suite (`yarn test:unit`) runs in CI and is a regression check
only. The end-to-end suite cannot execute at all.

**Platform coverage.** `storybook:build` is wrapped `x86_64-linux` only, so nothing
verifies the workbench builds on macOS or Windows. The operating-system switcher changes a minimum
window height (641px, 660px, 700px) and the `global.environment` flags stories branch on; it
simulates platform chrome, it does not test a platform. Neither gap is created by this work and
neither is closed by it.

### How each phase keeps `storybook:build` green

- **Phase 1.** Every task is an independent landing on 6.4.22. Each deletion removes the file and its
  barrel entry in the same commit, so the barrel never references a file that is gone. The
  `useDefineForClassFields` change is a type-checker setting and does not touch the bundler, which
  already sets it to `false` at `storybook/main.ts:87`. The `/staking/epochs` removal is the one
  deletion that reaches outside `storybook/`, and it lands with the regenerated translation
  artifacts in the same commit, because the i18n check at `perSystem/checks.nix:64-76` regenerates
  them and fails when the result differs from what is committed.
- **Phase 2.** The glob is a `main.ts` change verified by running the check, with the sidebar tree
  diffed against the phase 1 baseline. Storybook 6 has supported glob `stories` entries since 6.0 and
  loads a `storiesOf` file discovered by a glob exactly as the barrel did, because registration is a
  side effect of loading the module either way. If 6.4.22's indexer rejects the glob, the change
  folds into phase 3 instead, and the only thing lost is that the glob is verified at 8.6.x rather
  than at 6.4.22.
- **Phase 3, the hop.** This is the one landing that cannot be decomposed into smaller green commits
  on the trunk, and the reason is `yarn compile` rather than `storybook:build`: at 8.6.x a file still
  importing `storiesOf` from `@storybook/react` fails `tsc` whether or not the glob indexes it, and
  every story file is in the `tsc` program. So the corpus has to be fully shape-converted at the
  moment the manifest changes. The property is preserved by landing it as one merge from one branch
  that is green when it merges: the trunk never carries a red commit, and the red window lives on the
  branch. Three things keep that branch short. Everything
  version-neutral already landed in phases 1 and 2. Knobs and `withState` are untouched, because
  `@storybook/addon-knobs@8.0.1` builds at 8.6.x, which is the whole reason the route stops there.
  And the pre-flight worktree proves the runtime before the branch opens, so the branch is conversion
  work rather than debugging. Within the branch the commits are ordered config, globals, codemod,
  five domain tranches, label diff, pass-through removal, each individually reviewable.
- **Phase 4.** Each knob tranche is a separate landing. CSF stories and knob decorators coexist at
  8.6.x, so a corpus that is half converted builds and renders.
- **Phase 5.** Three separate landings: `moduleResolution`, then `main.ts` to ESM, then the manifest.
  Each is verified by all three required checks. Reverting the manifest leaves a fully converted
  corpus on 8.6.x, which is a working state.
- **Phases 6 and 7.** Purely additive. A new story file cannot break an existing one, so the check
  stays green by construction, and each tranche is its own landing.
- **Phase 8.** `eslint-plugin-storybook` lands after the corpus is already clean against it.

## Rollout / Migration / Rollback

There is no runtime rollout. Nothing here ships to a user, no feature flag is involved, and no
persisted data migrates. The rollout question is entirely about the state of the repository while the
work is in flight.

**Staging.** Branch from `master`; there is no `develop`. Each phase is a series of changes that
build, type-check and lint on their own, and rebase rather than merge. Phase 3 is the exception and
is a single merge from a single branch.

**Rollback, by phase.**

- Phases 1 and 2 are individually revertible with no dependency on later work. Reverting the glob
  change restores the barrel.
- Phase 3 reverts as one merge, returning the repository to Storybook 6.4.22 with `storiesOf` intact.
  Nothing later in the plan has landed at that point.
- Phase 4 is revertible per tranche, with one coupling: the 8 files that carry both knobs and
  `withState` are converted together, so they revert together.
- Phase 5 reverts to 8.6.x with a fully converted corpus. The `moduleResolution` commit is separate
  because its fallout reaches `source/` and has nothing to do with Storybook.
- Phases 6 and 7 revert per tranche. A container story that turns out to mount a subtree nobody can
  maintain is deleted without touching any other tranche.
- Phase 8 reverts per task.

**If the work stops midway.** The stopping points that leave the repository in a defensible state, in
increasing order of work completed:

1. **After phase 2.** Barrel replaced by a glob, dead files removed, still on 6.4.22 with
   `storiesOf`. The silent-absence defect is fixed and nothing else has changed.
2. **After phase 3.** Whole corpus in CSF on Storybook 8.6.x with knobs still working. CSF is
   portable to every other workbench surveyed, so this is the state that keeps every future option
   open, including leaving Storybook.
3. **After phase 5.** On the current line, knobs and `@dump247/storybook-state` gone, the corpus
   modern. The screen coverage work can then be picked up at any later date by anyone, because the
   harness is additive and depends on nothing that is still in flight.

The state to avoid is a partially converted corpus with the manifest already bumped, because
`storiesOf` does not compile against `@storybook/react` at 8 or above. Landing the hop as one merge
is what makes that state unreachable on the trunk.

**Kill criteria.** Stop and re-plan rather than push through if:

- the 8.6.x pre-flight shows `react-polymorph` 1.0.4 misbehaving under the modern preview runtime. It
  is untested there, it was last published 2022-04-14, and it sits between Storybook and every
  rendered story through `StoryDecorator`'s `ThemeProvider`;
- the codemod dry run produces output that needs more hand correction than hand conversion would have
  taken. The dry run happens before the hop branch opens, for exactly this reason;
- the `moduleResolution` change produces type errors across `source/` that are not mechanically
  fixable. This one redirects rather than stops: land on 9.1.x, which peers `typescript: ">= 4.9.x"`
  with no resolution constraint and needs no ESM main config, and treat 10 as a later, smaller hop;
- the tranche 6 proof story costs materially more than the rate established across tranches 1 to 5.
  Re-estimate the remaining data tranches before committing to them rather than absorbing the
  overrun silently.

## Open Questions

None open. Sixteen decisions are recorded above as locked.

The three routing questions this plan originally carried are among them: `/voting` and
`/voting/registration` is decision 13, whether `/staking/epochs` gets its navigation item back or
goes is decision 14, and the misplaced `/redeem-itn-wallets` binding is decision 15.
`research/05-reachable-screens.md` sections 4.3 and 4.4 hold the same three records next to the
evidence each rests on.

Four further questions are settled, and each is recorded where it belongs rather than here: the 15
sibling-registering files (decision 16), the `@dump247/storybook-state` dependency on
`@storybook/addons` (decision 9 and phase 2), the render check (decision 7), and the knob count that
sizes phase 4, reconciled to `research/02-storybook-upgrade-path.md` in the Technical Design.

Two items are deliberately carried as known-imprecise rather than as questions, because no further
evidence would settle them before the work starts.

- **The phase 6 and 7 estimate rests on a census of store-key counts, not of fixture depth.** Nothing
  in the corpus demonstrates a container fixture at screen scale; the nearest prior art is a 19-line
  literal at `storybook/stories/nodes/_utils/props.ts` covering a dialog that reads five fields off
  one store. `WalletSummaryPage.tsx:85-171` alone reads about 25 distinct fields across 9 stores and
  throws outright at `:119` without a fully-shaped `Wallet` domain object. Twenty containers carry
  that kind of guard. The figure is carried as provisional and task-050 re-estimates task-052
  against the first tranche's actuals.
- **The 47 dialog and wizard containers were never censused**, and the plan mounts some of them:
  functional requirement "a story per meaningful state" covers `/wallets/add`, whose
  `WalletAddPage.tsx:80-81` mounts `WalletBackupDialogContainer`, which reads `stores.walletBackup`
  at `WalletBackupDialogContainer.tsx:33`. That store is one of the three the census records as read
  by no screen. The harness must cover the dialog containers' union of store keys, which is 13.

## Status Log

Append-only. Per `.agent/plans/readme.md`, every new entry goes at the end of this
section, in date order. A prior entry that turns out to be wrong is corrected by a
new entry saying so, never by rewriting it.

### 2026-09-10 — Plan written

PRD and task graph written against five research notes. Twelve planning decisions
locked, then three routing questions settled as decisions 13 to 15.

### 2026-09-14 — Revised after review, before any implementation

A review against the repository found four questions this document had recorded as
closed without examining them, and a number of counts and claims that did not hold.
All four are now settled and recorded where they belong rather than in Open
Questions.

Decisions changed:

- Decision 7 reversed. `@storybook/test-runner` is out of scope: it needs a
  Playwright browser and the Nix build is offline by construction. Outcome 5 of the
  original brief is dropped, deliberately, and the consequence is recorded rather
  than worked around.
- Decision 9 extended. `@dump247/storybook-state` is removed in phase 2, before the
  version hop, because it imports `@storybook/addons` and no version of that exists
  in the 8 line. The 8.6.x stopover could not have done its job otherwise.
- Decision 16 added. The 15 files that match the story naming convention and
  register no stories are renamed into `_support/` before the phase 2 glob.

Corrections: knob call sites reconciled to 394 direct plus 44 indirect, against the
364 previously carried into the phase 4 estimate; the `@storybook/addon-knobs` 6.4.0
to 8.0.1 hop added as a stated requirement; the `withKnobs(story, context)` idiom at
22 sites and the two incompatible story signatures moved into the pre-flight; four
`context.kind` and `context.story` sites marked as control flow rather than labels;
the `Legacy.stories.tsx` reasoning corrected; the phase 2 SCSS rehoming dropped as a
no-op.

Task graph: the eight screen tranches now carry the full per-screen roster as
subtasks, each screen resolved to its container and its story file, replacing
acceptance criteria that said "eight screens render their real content" without
naming which eight. The six fixture tasks gained target paths. The eight harness
tasks gained `yarn storybook:build` in their acceptance, which was previously
carried only by the additive tranches.

No implementation has started.

### 2026-09-14 — Counts reconciled across the plan and the research notes

Every count in this document, in `prompt.md`, in the five research notes and in the task graph was
re-measured against the repository at `ec6954d9a`. The two package publish dates were taken from the
npm registry.

Three legitimate scopes had been mixed without being named, which is what made the figures look
contradictory. Measured over the 84 story files, there are 64 knob-importing files and 364 knob call
sites. Measured over every file under `storybook/stories`, support modules included, there are 71
files and 394 sites. Measured over the whole corpus, which adds the four colocated story files under
`source/`, there are 75 files and 396 sites. This document uses the whole-corpus figures, because the
conversion has to reach the support modules the stories import, and each note now states the scope
it is measuring.

Corrections: knob call sites 396, not 394, the difference being the two `boolean` calls in
`features/discreet-mode/ui/DiscreetValue.story.tsx`; `boolean` 182, not 180, for the same reason; the
net phase 4 surface 366 direct plus 44 indirect, about 410; `@dump247/storybook-state@1.6.1`
published 2019-06-22 and `@storybook/addon-knobs@8.0.1` published 2024-06-19, both of which had been
recorded as the registry's package-level `modified` timestamp rather than the version publish time.
`research/04-tooling-alternatives.md` carried 82 story files, 279 registrations and 382 knob sites,
none of which reproduce; they are now 84, 272 and 396. `research/03-react-upgrade-gate.md` carried 72
knob-importing files, which reproduces under no scope, and now reads 75.

Story registrations stand at 272 across the 84 story files, 267 of them under `storybook/stories`.
A raw grep returns more: three `moment().add()` chains and two `Set.add()` calls in the support
module `governance/_utils/drepPopulation.ts` are not story registrations.

---

**Status:** Draft
**Date:** 2026-09-10, revised 2026-09-14
**Author:** Se7en Labs
