# Storybook Modernization: Planning Brief

## The request

Modernize Storybook in Daedalus. Five outcomes are asked for, in this order:

1. **Modernize the workbench.** The pinned Storybook packages are 6.4.22, published 2022-04-14.
   Bring the setup onto a version that still receives upstream fixes and can accept currently
   maintained addons.
2. **Ensure a story exists for every currently accessible screen.** A screen a user can reach in the
   running application should be visible in the workbench. Coverage means the screen as the
   container assembles it, not the components underneath it.
3. **Remove stories that are no longer used.** Stories for deleted components, orphaned story files,
   and stories documenting screens no user can reach.
4. **Verify everything still works.** Not only that the build passes at the end, but that the suite
   remains usable, and the required check remains green while the work is in progress.
5. **Leave an automated render check out of scope.** The workbench is the only
   thing in the repository that renders a component with its real theme and its real translation
   catalog, and nothing currently fails when one of them renders nothing. This plan does not close
   that gap. `@storybook/test-runner` was the intended closer, and it cannot run here: it requires a
   Playwright browser, `playwright` and `playwright-core` declare no `scripts` field, so binaries
   are never fetched at install, and the Nix build is offline by construction
   (`nix/internal/common.nix:315` writes `'"--offline" true'`). Standing it up means a
   `perSystem/checks.nix` change plus a nixpkgs-to-npm version pairing, which this plan does not
   take on. The container story tranches in phases 6 and 7 are therefore accepted by eye, and the
   required check set stays as blind to rendering as it is today.

The tool question was raised directly alongside the request: whether Storybook is still the right
tool for this codebase at all, or whether a lighter workbench, a component testing framework, or no
workbench would serve Daedalus better. That question was answered before the modernization plan
began, because the answer determines what the other outcomes are being built against.

## Constraint framing

The request lands against a codebase with several fixed points that the plan respects rather than
negotiates:

- **React is 16.14.0** (`package.json:259`), and `react-intl@2.9.0`, `react-polymorph@1.0.4` and
  `mobx@5.15.7` sit on top of it. `research/03-react-upgrade-gate.md` establishes that the React
  upgrade gates nothing here and is itself a much larger workstream.
- **Legacy decorators are mandatory.** MobX 5 breaks under TC39 stage 3 decorators. The settings that
  keep it working are spelled out in `tsconfig.json:17`, `storybook/main.ts:79`,
  `storybook/main.ts:84` and `storybook/main.ts:87`. Any builder or compiler change must carry all
  four across.
- **`yarn storybook:build` is a required CI check** (`perSystem/checks.nix:78`, wrapped
  `x86_64-linux` only). It is also the only automated check in the repository that renders components
  with their real themes and real translation catalogs. The plan must keep it green throughout the
  work, not only at the end.
- **The end-to-end suite cannot execute.** `spectron@14.0.0` resolves `electron-chromedriver@12`
  against Electron 41.3.0, and 23 of 48 feature files are disabled at feature level. There is no
  second net beneath the workbench to catch a rendering regression.
- **The stories are type-checked.** `tsconfig.json` declares no `include`, only
  `"exclude": ["node_modules"]` (`tsconfig.json:103`), so `yarn compile` covers every story file.
  This makes the conversion safer than it would otherwise be. It is also what forces the version hop
  to be a single landing, because a file importing `storiesOf` fails `tsc` the moment the package
  moves, whether or not the indexer sees that file.
- **Containers do not type-check their fixtures.** `types/injectedPropsType.ts:6-10` types `stores`
  as `any | StoresMap` and `tsconfig.json:79-85` disables `strict` and `noImplicitAny`, so an
  incomplete store fixture compiles and fails at runtime instead.

## Locked decisions

These are settled. `storybook-modernization-prd.md` carries each one with the evidence behind it,
under Locked Planning Decisions.

1. Land on Storybook 8.6.x, convert the corpus there with codemod assistance, then bump to 10.6.x.
   9.1.x is the documented fallback. The 6.4.22 capability spike is dropped and gates nothing.
2. Container stories are in scope, with coverage defined as every router-reachable screen driven from
   the route configuration, not all 105 containers.
3. Convert the existing corpus to CSF first, then add container stories.
4. Delete the four flag-disabled story sets. Leave the components and the flags untouched.
5. Delete `storybook/stories/staking/Legacy.stories.tsx` and the eight components under
   `source/renderer/app/components/staking/legacy/`.
6. Restage the wallet settings undelegation story rather than deleting it.
7. `@storybook/test-runner` render smoke checks are **out of scope**. The runner needs a Playwright
   browser the Nix sandbox cannot obtain. Image-diff baselines remain a named follow-on and inherit
   the same blocker.
8. Carry the 229 `@ts-ignore` directives through the conversion.
9. `useArgs` is the default replacement for `withState` **in phase 4**, with the exception rule
   recorded once. Separately, `@dump247/storybook-state` is removed in **phase 2**, before the
   version hop, by replacing its 17 call sites with a local stateful wrapper component that touches
   no Storybook API. It imports `@storybook/addons`
   (`node_modules/@dump247/storybook-state/dist/index.js:11`) and calls `addons.getChannel()` at
   `:157`, and no `@storybook/addons` exists in the 8 line, so it cannot survive the hop that
   phase 3 performs.
10. Add `eslint-plugin-storybook` at the end; the 457 existing warnings stay warnings.
11. Set `useDefineForClassFields` to `false` in `tsconfig.json` as a standalone commit.
12. Do not run `storybook upgrade`. Hand-edit `package.json` and run automigrations individually.
13. Keep the voting stories, and keep `/voting` and `/voting/registration` off the coverage target.
14. Remove `/staking/epochs` entirely: route, container, components and story.
15. Delete the `/redeem-itn-wallets` route binding, which is nested where it can never match.
16. Rename the 15 files that match the story naming convention but register no stories of their own
    out of that convention, into `_support/`, in phase 1, before the glob lands in phase 2. They are
    support modules that re-export or import siblings, not stories. Under the phase 2 glob they
    would become indexer inputs. At 6.4.22 `StoryStoreFacade` falls back to `autoTitle` and
    registers every named export, moving the sidebar baseline the whole conversion is diffed
    against. At 8.6 and 10.6 they are a hard `build-storybook` failure (`NoMetaError`, collected
    into `MultipleIndexingError`). The `storiesof-to-csf` codemod does not touch them, because they
    contain no `storiesOf` chain. The 15 are the eight under `staking/`
    (`DelegationCenter`, `DelegationSteps`, `StakePoolsTable`, `Rewards`, `RedeemItnWallets`,
    `Undelegate`, `Epochs`, `StakePools`), `wallets/addWallet/AddWallet.stories.ts`,
    `wallets/settings/WalletSettingsScreen.stories.tsx` (whose default export at `:139` is an
    anonymous React component, not a CSF meta object), `nodes/updates/DataLayerMigration`,
    `nodes/errors/SystemTimeError`, `nodes/errors/NoDiskSpaceError`, `nodes/syncing/SyncingConnecting`
    and `nodes/status/Status.stories.ts`.

## What the plan produces

A sequenced, costed plan that names the target tool and version with the evidence behind both. It
converts the story corpus off the APIs that no longer exist upstream without a red window on the
trunk, extends coverage from the component layer to the screen layer, removes what is dead, and
states plainly what happens if the upgrade stalls midway.

Nothing in the brief is left open. The three questions the evidence could not settle were routing
questions and are decisions 13 to 15. Four more are decisions 7, 9 and 16, and the knob count that
sizes phase 4, which is reconciled to `research/02-storybook-upgrade-path.md`.

## Supporting research

Five research notes were written before this plan and are the evidence base for it:

- `research/01-current-coverage.md`: the measured migration surface, container and component
  coverage, dead stories, and the type-check, lint and build guarantees that currently apply.
- `research/02-storybook-upgrade-path.md`: version requirements per Storybook line, what codemods
  cover and what they do not, and whether the upgrade can be staged.
- `research/03-react-upgrade-gate.md`: whether the React upgrade gates this work, and what the React
  upgrade itself would cost.
- `research/04-tooling-alternatives.md`: whether a component workbench is still the right tool, and
  whether Storybook is still the right workbench.
- `research/05-reachable-screens.md`: the 49 router-reachable screens enumerated from the route
  configuration, the eight exclusions with the evidence that puts each one outside the list, the
  store dependency census across the 48 screen containers, the verdict that one shared fixture
  harness can serve nearly all of them, and the tranche sequence the container work follows.
