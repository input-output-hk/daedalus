# Tooling alternatives: is a component workbench still the right tool, and is Storybook still the right workbench

Status: research complete
Date: 2026-09-10
Measured against: `feat/drep-discovery` at commit `3eaa4f17fac50a959057143904d488303fe4922b` (2026-08-27). Registry and repository facts fetched 2026-09-10.

## Summary

Stay on Storybook and modernize it. Two findings decide this, and neither depends on Storybook's popularity.

First, Storybook 10 is the only surveyed workbench that still supports React 16. `@storybook/react-webpack5@10.6.0` (published 2026-09-02) declares `react: "^16.8.0 || ^17.0.0 || ^18.0.0 || ^19.0.0"`. Every lighter alternative has moved its floor to React 18: Ladle 5.1.1 declares `react: ">=18.0.0"`, react-cosmos 7.4.1 declares `react: ">=18"`, `vitest-browser-react@2.3.0` declares `react: "^18.0.0 || ^19.0.0"`, and Cypress dropped React 16 and 17 in version 14. Daedalus is on react 16.14.0 (`package.json:259`). Choosing any of those turns a story rewrite into a story rewrite plus a React major upgrade.

Second, Storybook is the only option that keeps the webpack 5 builder. `@storybook/builder-webpack5@10.6.0` shipped the same day as the rest of the 10.6 line. Everything else in the lighter-workbench category is built on Vite, and Vite's default transform path has a documented failure mode with the legacy decorators that MobX 5 requires. `storybook/main.ts` sets `legacyDecorator: true` and `useDefineForClassFields: false` on swc-loader with a comment explaining that MobX 5 breaks without them.

Storybook is also not declining. It shipped 10.6.0 on 2026-09-02 and cut `11.0.0-alpha.0` the same day, and the repository absorbed 13,035 commits in the trailing 52 weeks.

The migration is still a real rewrite: `storiesOf()` was removed in Storybook 8 and the codemod that used to automate the conversion no longer ships. That cost is unavoidable under every option on the table, so it does not separate them.

## 1. What Daedalus actually needs, measured from the repository

### The context wiring

A story in this repository does not render a component. It renders a component inside four stacked layers of context, and every one of those layers is bespoke code in this repository.

| Layer | File | What it provides |
|---|---|---|
| Global state and toolbar | `storybook/addons/DaedalusMenu/` (5 files) | Theme, locale and OS pickers rendered into the manager toolbar, synchronized to `sessionStorage` and the URL hash |
| Theme, locale, window size | `storybook/stories/_support/StoryWrapper.tsx` | `ThemeManager`, `WindowSizeManager`, `IntlProvider` with `addLocaleData([...en, ...ja])`, and per-OS minimum window height |
| Component library theming | `storybook/stories/_support/StoryDecorator.tsx` | react-polymorph `ThemeProvider` with `SimpleSkins`, `SimpleDefaults` and the Daedalus `themeOverrides` |
| MobX stores and app shell | `storybook/stories/_support/StoryProvider.tsx`, `StoryLayout.tsx` | mobx-react `Provider` with a hand-built `stores` object, real `actions`, `BrowserLocalStorageBridge`, `DiscreetModeFeatureProvider`, plus a full sidebar and topbar shell |

Supporting fixtures sit alongside: `environment.ts` stubs the entire `Environment` type onto `global.environment`, `config.ts` enumerates nine themes, two locales and three operating systems, and `preview.tsx` freezes the clock with `timemachine` at `Sat, 01 Jan 2022 10:00:00 GMT` so date-formatting output is stable.

The webpack configuration in `storybook/main.ts` carries its own weight. It sets up SCSS modules with a `[name]_[local]` local ident, `.inline.svg` via svg-inline-loader, and WebAssembly experiments. Two `NormalModuleReplacementPlugin` rules redirect `@trezor/transport` away from its Node USB and UDP transports. Nine Node core modules get browser polyfills (`crypto`, `stream`, `buffer`, `os`, `path`, `http`, `https`, `url`, `process`), and five more are stubbed to `false` (`child_process`, `dgram`, `fs`, `usb`, `node-gyp-build`).

Any move to a different tool rebuilds all of it. Staying on Storybook rebuilds only the parts whose API changed.

### The surface being migrated

| Measurement | Count | How it was counted |
|---|---|---|
| Story files | 84 | 80 under `storybook/stories/`, 4 under `source/`, two of those named `.story.tsx` |
| Files calling `storiesOf()` directly | 69 | The other 15 export story functions consumed by a sibling, for example `Staking.stories.tsx:14` imports `StakePoolsStory` from `./StakePools.stories` |
| Individual stories (`.add()` calls) | 272 | Excludes three `moment().add()` chains that a raw grep picks up |
| `.addDecorator()` calls | 99 | 92 under `storybook/stories`, 7 in the colocated files |
| Files importing `@storybook/addon-knobs` | 75 | 71 under `storybook/stories`, 4 colocated |
| Knob call sites | 396 | 182 `boolean`, 100 `number`, 55 `text`, 49 `select`, 3 `date`, 3 `radios`, 2 `button`, 1 `object`, 1 `optionsKnob` |
| Files using `@dump247/storybook-state` | 10 | |
| Themes / locales / OS profiles | 9 / 2 / 3 | `storybook/stories/_support/config.ts` |

Registration runs through one entry point loaded for its side effects. `main.ts` sets `stories: ['../storybook/stories/index.ts']`, and that file imports around 40 story modules for their side effects. This is a `storiesOf()`-era pattern. CSF replaces it with glob discovery, so the entry file disappears rather than needing conversion.

### What the workbench is used for

`.agent/workflows/test.md:37` lists Storybook in the test-tooling table under "Visual component testing", and `.agent/workflows/frontend.md` makes `yarn storybook` the documented loop for component development. `.agent/skills/storybook-creation/SKILL.md` is a full authoring standard: naming conventions, directory layout, domain wrappers, and templates for the knobs and `withState` patterns.

Beyond development, three uses are load-bearing:

1. **A required CI artifact.** `perSystem/checks.nix:78` defines `storybook = mkJsCheck "daedalus-storybook-build" "yarn storybook:build"`. A build failure blocks the check. `check:all` in `package.json:17` ends with the same command.
2. **Localization review.** `IntlProvider` is wired with real `en-US.json` and `ja-JP.json` message catalogs and the DaedalusMenu locale switch flips between them live. This is the only place in the repository where a translator can see Japanese copy inside the real component at the real width without building and running the Electron app against a live node.
3. **Cross-platform visual review.** The OS switch changes minimum window height per platform, and `environment.ts` exposes `applyEnvironmentOs` so stories whose components branch on `global.environment.isWindows` stay in step. Contributors on one platform can review layout for the other two.

## 2. The safety net this decision sits inside

The workbench matters more here than it would in a typical React application, because the layers that would otherwise catch a visual regression are largely absent.

`source/` holds 361 non-spec, non-story component files under `source/renderer/app/components` and 51 colocated `.spec.ts`/`.spec.tsx` files in total, of which 25 use `@testing-library/react`. Component behavior coverage is therefore in the low tens of percent at best, and those tests run in jsdom, which asserts nothing about layout, theming or text overflow.

The Cucumber end-to-end suite cannot execute. `package.json:174` pins `spectron: "14.0.0"`, which resolves `electron-chromedriver@12` against the Electron 41.3.0 in `package.json:223`. Of 48 `.feature` files under `tests/`, 23 are disabled at feature level: 15 tagged `@wip`, 7 tagged `@skip` (three of those also `@API`), and one `@unit @skip`. Cucumber is not in the CI check set in `perSystem/checks.nix` either.

`storybook:build` is therefore the only automated check in CI that renders Daedalus components with their real themes and real translations and fails if one of them throws.

## 3. Storybook itself: is it in decline

The evidence says no.

| Signal | Value | Source |
|---|---|---|
| Latest release | 10.6.0, published 2026-09-02 | npm dist-tags for `@storybook/react` |
| Next major already open | `11.0.0-alpha.0`, published 2026-09-02 | Same |
| Stated cadence | Minor every eight weeks, major once a year | https://storybook.js.org/docs/releases/roadmap |
| Commits, trailing 52 weeks | 13,035 | GitHub participation stats for `storybookjs/storybook` |
| Last push to default branch | 2026-09-10 | GitHub API |
| Weekly npm downloads (`storybook`) | 11,239,174 | https://api.npmjs.org/downloads/point/last-week/storybook |
| Stars | 91,034 | GitHub API |

Governance and funding are concentrated but not precarious. Storybook is maintained by Chromatic, which sells a hosted visual testing service built on it (https://www.chromatic.com/company/about). The Storybook 10 announcement credits Netlify and CircleCI as additional supporters (https://storybook.js.org/blog/storybook-10/). Chromatic has raised roughly $10.5M and reported around $5.6M in revenue in 2025. This is a single-vendor open source project, which is a structural risk: if Chromatic fails, Storybook's release cadence would depend on whether the community picks it up. Against that, the license is MIT, at least six distinct human contributors landed commits in the last 30 days alongside automation, and 11.2M weekly downloads is a large enough installed base that a fork would find maintainers.

Direction of travel is toward testing and away from bulk. Storybook 9 and 10 folded the classic essential addons into the core package: `@storybook/addon-actions`, `addon-controls`, `addon-toolbars` and `addon-viewport` all stop at 9.0.8 (2025-06-10) and now exist as subpath exports of `storybook` itself (`storybook/actions`, `storybook/viewport`, `storybook/manager-api`, `storybook/preview-api`, `storybook/test`, `storybook/theming`). `@storybook/addon-essentials` stops at 8.6.14. Storybook 10's only breaking change is ESM-only distribution, which the release notes say cuts install size 29% on top of 50% saved in version 9.

For Daedalus that consolidation is good news, because the addon dependency list shrinks from nine packages to roughly two.

One caveat attaches to this. Storybook's React 16 peer range is permissive, but React 16 is not what the project's own test matrix exercises most. The peer declaration is a commitment, not a guarantee that every code path was tried at 16.14.0. This should be settled by a spike that boots Storybook 10 against a handful of converted stories before the bulk conversion starts, not by trusting the manifest.

## 4. Lighter component workbenches

All three fail on requirements that Daedalus cannot negotiate away in this piece of work.

| Tool | Latest | React floor | Builder | Custom addons | Verdict |
|---|---|---|---|---|---|
| Ladle | `@ladle/react@5.1.1`, npm 2025-11-04, repo push 2026-06-28 | `>=18.0.0` | Vite 6 only | "Ladle currently does not support third-party addons" | Blocked |
| Histoire | `1.0.0-beta.1`, 2026-01-07 | No React plugin exists | Vite only | Plugin API is Vue-shaped | Blocked |
| react-cosmos | `7.4.1`, 2026-08-31 | `>=18` | Webpack or Vite | Fixtures and decorators, no toolbar addon API | Blocked |

**Ladle** (https://github.com/tajo/ladle, https://ladle.dev/docs/addons) is the strongest of the three on its own terms: 2,982 stars, 167,719 weekly downloads, MIT, and it is fast because it drops the manager/preview iframe split. It fails Daedalus on three counts. Its React peer floor is 18. It is built around Vite, so the entire webpack configuration in `main.ts` would be rewritten as Vite plugins, including the Trezor transport replacements and the fourteen Node fallbacks. And its addon set is fixed and closed, so DaedalusMenu has no home. The theme, locale and OS switchers would become an in-story control panel instead of a toolbar, changing the review experience the translator and the platform reviewers rely on. Activity is also softening. The last npm release was ten months ago and the last repository push was 2026-06-28.

**Histoire** (https://github.com/histoire-dev/histoire) is eliminated outright. There is no React plugin. `@histoire/plugin-react` returns `{"error":"Not found"}` from the npm registry, and the published plugin packages are Vue, Vue 2, Svelte and Nuxt only. The project has also been in `1.0.0-beta` since 2026-01-07 with 203 open issues.

**react-cosmos** (https://github.com/react-cosmos/react-cosmos) is the healthiest of the three by maintenance signal: v7.4.1 shipped 2026-08-31, only 5 open issues, 8,685 stars. It supports webpack, unlike the other two. But its React peer floor is 18, and its model differs: fixtures with a decorator hierarchy by directory, no toolbar addon API, and no equivalent of Storybook's `globalTypes`. The theme, locale and OS switchers would become fixture-level inputs rather than a global toolbar, so switching locale would no longer persist while browsing between components. At 28,360 weekly downloads it is also a much smaller ecosystem to draw help from.

On the Vite question generally: even setting React versions aside, moving this codebase onto a Vite-based workbench means moving MobX 5's legacy decorators onto Vite's transform. That path is documented as fragile, including a failure mode where dev mode uses legacy decorators and the production build silently uses spec decorators, producing behavior that differs between the two (https://github.com/evanw/esbuild/issues/3301, https://mobx.js.org/enabling-decorators.html). Storybook's own Vite builder would inherit the same problem, which is a second reason to keep the webpack5 builder even while upgrading Storybook.

## 5. Component testing frameworks

These test components. They do not showcase them. Two of the four needs listed above are review needs, which no assertion framework covers.

| Need | Playwright CT | Cypress CT | Vitest browser mode |
|---|---|---|---|
| Isolated render with heavy context | Yes, via your own dev server | Yes | Yes |
| Browsable catalog for humans | Partial, see below | No | No |
| Live theme / locale / OS switching by a reviewer | No | No | No |
| Required CI artifact | A test run, not an artifact | A test run | A test run |
| Works at React 16.14.0 | Yes | No, dropped in Cypress 14 | No, `vitest-browser-react` needs 18+ |

**Playwright component testing** is the only one of the three that clears the React 16 bar. It became non-experimental in Playwright 1.62; the `@playwright/experimental-ct-react` packages were removed and `mount()` is now a documented built-in fixture of `@playwright/test` (https://playwright.dev/docs/test-components). It is now bundler-agnostic and framework-agnostic: it serves a "gallery" page from your own dev server exposing `window.mount()` and `window.unmount()`, so whatever your application can render, your stories can render. That means no React version constraint from Playwright's side.

But the gallery is not a workbench. The documentation is explicit that it ships no UI for browsing stories, and that an index page listing discovered stories is something you build yourself. There is no controls panel, no toolbar, no persistent global state across navigations. For a translator checking Japanese copy across thirty screens, "call `window.mount()` from the browser console" is not a workflow.

Playwright CT covers automated interaction and visual assertions on components. It does not cover the workbench question.

**Cypress component testing** is closed. Cypress 14 raised the component-testing React floor to 18.0.0 and the current release is Cypress 16 (https://www.cypress.io/releases/14-0-0). Staying on Cypress 13 to keep React 16 support would mean adopting a tool at a version that is two majors behind on day one.

**Vitest browser mode** went stable in Vitest 4 and now includes built-in visual regression assertions. It is closed for the same reason: rendering React in it requires `vitest-browser-react`, which declares `react: "^18.0.0 || ^19.0.0"`. The same constraint closes Storybook's own `@storybook/addon-vitest`, which peers on `@vitest/browser`. Even inside Storybook, the Vitest-based testing integration is unavailable at React 16, so the testing path for this repository is `@storybook/test-runner`. That limitation belongs in the plan.

## 6. Doing without a workbench

The proposition is: delete `storybook/`, delete the `storybook:build` CI check, rely on Jest plus testing-library for behavior and on the running Electron app for anything visual.

It removes 84 story files, 34KB of `_support` wiring across nine files, a 6KB bespoke addon, and a 142-line webpack configuration carrying Trezor stubs and Node polyfills. It also removes the slowest check in `check:all` and three stale dependencies: `@storybook/addon-knobs` (last published 2024-06-19), `@dump247/storybook-state` (last published 2019-06-22), and `storybook-addon-swc`, which is declared at `package.json:175` and referenced nowhere in the repository.

The cost is that it removes the only automated rendering check that exists. After deletion, the complete visual safety net for a wallet that holds real funds would be three things: 51 Jest spec files against 361 components, all in jsdom; an end-to-end suite that cannot execute; and a human remembering to open the Electron app. Verifying that a change did not break the Japanese layout on Windows in the dark-blue theme would require building Daedalus, running a node, navigating to the screen, and doing it again for each of the nine themes. Today it is three clicks.

There is also a second-order cost. `storybook:build` currently type-checks and bundles every component reachable from a story through the real webpack pipeline. It catches broken imports, circular dependencies and provider-contract violations that Jest's module mocking hides. Losing it removes an integration check, not just a picture.

This option would be defensible if the e2e suite worked. It does not, and repairing it is a separate and larger problem (spectron 14 against Electron 41 is not a version bump, spectron is unmaintained). Dropping the workbench while the e2e suite is dark would leave the project with essentially no rendering verification at all. That is the wrong direction for a wallet.

## 7. Visual regression

Nothing in the current setup does visual regression. Adding it is a separate decision from the workbench decision, but the workbench choice constrains it, so it belongs here.

| Option | Self-hostable | Health | Fit |
|---|---|---|---|
| `@storybook/test-runner` plus `jest-image-snapshot` | Yes, fully local | v0.24.5, peers `storybook: ^10 \|\| ^11` | Best fit |
| reg-suit | Yes, S3 or local storage | Pushed 2026-09-09, 1,292 stars | Good pairing for report and diff review |
| Chromatic | No, hosted service only | Actively developed | Caveat below |
| Lost Pixel | Yes, but archived | **Repository archived**, last push 2026-04-22 | Do not adopt |
| Loki | Yes | Last push 2024-10-12, 140 open issues | Stale |
| Argos | MIT codebase but SaaS-oriented | Pushed 2026-09-10 | Self-hosting not a supported path |

The recommendation for visual regression, when the project is ready for it, is `@storybook/test-runner` driving a statically built Storybook via `TARGET_URL`, taking screenshots in `postVisit` and comparing with `jest-image-snapshot`, with baselines committed to the repository. This is entirely self-hosted, adds no external service, runs against the artifact `storybook:build` already produces, and uses Playwright and Jest, both of which are already understood in this codebase. reg-suit can be layered on later if HTML diff reports and baseline management become the bottleneck.

Chromatic carries one caveat. It is built by the same people who maintain Storybook, so integration is seamless. It is also a paid hosted service that would send every component render of the wallet UI to a third party on every CI run, and it would become a dependency of the CI pipeline. The free tier is 5,000 snapshots per month, Chrome only. Daedalus is Apache 2.0 and Chromatic does run an open source sponsorship program (https://www.chromatic.com/docs/open-source/), but eligibility is aimed at design systems and component libraries rather than applications, so qualification is not assumable. Given a stated preference for minimizing external dependencies, and given that the self-hosted path costs a Playwright runner already present, Chromatic is not the recommendation.

Lost Pixel is still named as the open source answer by most 2026 comparison articles, but its GitHub repository is archived, and its last push was 2026-04-22.

## 8. Migration cost, both directions

The story rewrite is not a differentiator, because it is identical under every option. `storiesOf()` was removed in Storybook 8 (https://storybook.js.org/docs/8/migration-guide/from-older-version), Ladle and react-cosmos never supported it, and no component testing framework has an equivalent. All 84 files change no matter what.

The automation is also gone. The `storiesof-to-csf` transform is present in `@storybook/codemod@8.6.18` but absent from both `9.1.20` and `10.6.0`. The practical route is to run `npx @storybook/codemod@8.6.18 storiesof-to-csf` as a one-off conversion pass and then upgrade, rather than expecting the current CLI to offer it. That should be verified early, since the codemod is unmaintained and 272 `.add()` calls with 99 decorator attachments is a lot of surface for a transform to get right.

What does differ between the options is everything else.

| Work item | Storybook 10 | Ladle or react-cosmos |
|---|---|---|
| 84 story files to CSF | Required | Required |
| 396 knob calls to args/controls | Required | Required |
| 10 `withState` usages to hooks | Required | Required |
| React 16 to 18 upgrade | Not required | **Required first**, across 361 components and 105 containers |
| webpack config (`main.ts`, 142 lines) | Keep, port CommonJS to ESM | **Rewrite as Vite config** including Trezor replacements and 14 Node fallbacks |
| MobX 5 legacy decorators | Unchanged, swc-loader already configured | **At risk** on Vite's transform |
| DaedalusMenu addon | Port `@storybook/addons` to `storybook/manager-api`, or replace with `globalTypes` toolbars | **Ladle: no home. Cosmos: reshape as fixture inputs** |
| StoryWrapper / StoryDecorator / StoryProvider | Convert to CSF decorators, logic unchanged | Rebuild against a different decorator model |
| `perSystem/checks.nix` | Change `storybook:build` script body only | Replace the check |
| Addon dependency count | 9 packages to about 2 | New ecosystem |

The Storybook path also opens a simplification. DaedalusMenu exists because Storybook 6.4 had no first-class way to put a global switcher in the toolbar. Storybook has had one since 6.0 in `globalTypes` with `toolbar` annotations, read back in decorators through `context.globals` (https://storybook.js.org/docs/essentials/toolbars-and-globals). Theme, locale and OS are exactly that shape. Whether to port DaedalusMenu's five files to `storybook/manager-api` or delete them in favor of three `globalTypes` entries is a decision for the implementation plan. The second option removes the custom addon entirely, along with its `sessionStorage` and URL-hash synchronization, because Storybook persists globals itself.

Three smaller items on the Storybook path are cheap but easy to miss. The `start-storybook` and `build-storybook` binaries were replaced by `storybook dev` and `storybook build` in Storybook 7, so the two scripts at `package.json:55-56` change. Storybook 10 is ESM-only, so `main.ts`'s `module.exports` and `require()` calls become ESM. And `@storybook/addon-actions` imports across 64 files and `addon-knobs` imports across 75 become `storybook/actions` and args respectively. Node is not a constraint: the dev shell provides v22.23.1, and Storybook 10.6.0 declares no `engines` floor at all (9.1.20 declared `node >=20`).

## 9. Recommendation

**Stay on Storybook and modernize to Storybook 10 on the webpack5 builder.**

Concretely, and in this order:

1. Spike Storybook 10.6 with `@storybook/react-webpack5` against React 16.14.0 and five converted stories, one of which uses the full `StoryProvider` and `StoryLayout` shell, before committing to the bulk conversion. This is the one assumption in this recommendation that is not yet verified by running code.
2. Convert `storiesOf()` to CSF, seeding with `npx @storybook/codemod@8.6.18 storiesof-to-csf` and hand-finishing.
3. Replace knobs with args and controls, and the ten `@dump247/storybook-state` usages with local hooks in the story render.
4. Replace DaedalusMenu with `globalTypes` toolbar entries, keeping the switching behavior and dropping the custom addon and its storage synchronization.
5. Convert `main.ts` to ESM and the scripts to `storybook dev` / `storybook build`, keeping the webpack customizations as they are.
6. Drop `storybook-addon-swc` and `@dump247/storybook-state`, which become unreferenced.
7. Update `.agent/skills/storybook-creation/SKILL.md` and `.agent/workflows/storybook.md` in the same change. Both currently teach `storiesOf()`, and `.agent/workflows/storybook.md` additionally documents a `storybook/.storybook/` directory that does not exist and a `Category|Name` title separator that the skill file already contradicts.

Visual regression is a follow-on decision, not part of this work. When it is taken, the self-hosted `@storybook/test-runner` plus `jest-image-snapshot` path is the one that fits the project's stated preference for minimizing external dependencies.

## 10. The strongest argument against, and the answer

**The argument.** Storybook is the heaviest option by a wide margin, and the modernization keeps Daedalus on a stack that is aging in place: React 16, webpack, legacy decorators, SCSS modules, react-polymorph 1.0.4 which has not been published since 2022-04-14. Every alternative surveyed has moved to React 18 and Vite. Paying for an 84 file rewrite to land on the old stack means paying again later. A React 18 upgrade is coming whether this work happens or not, and at that point Ladle and Vitest browser mode both become available, so the story files would be touched a third time. Better to do React 18 first and then pick a modern tool once, rather than rewrite the stories twice.

**The answer.** The sequencing is right, but the conclusion does not follow, for one reason: the CSF story files are portable, and the workbench is not. CSF is a documented format that Ladle explicitly implements, that react-cosmos can consume with a thin adapter, and that Playwright's gallery pattern maps onto directly. Converting `storiesOf()` to CSF is the prerequisite for every possible future, including leaving Storybook. It is not work that gets thrown away in a later React 18 upgrade; the decorator layer would be revisited, but 272 story definitions written as CSF exports would carry across largely unchanged. Doing React 18 first inverts the risk. It means a major React upgrade across 361 components and 105 container components, carried out with the current safety net: 51 jsdom specs, a dead e2e suite, and a Storybook that cannot be upgraded past 6.4. Modernizing the workbench first is what makes the React 18 upgrade reviewable.

The stack aging is a real problem, and it should be tracked as its own piece of work rather than folded into this one. react-polymorph in particular is a single-consumer library last published on 2022-04-14. But it is a different decision from the workbench decision, and coupling them means neither one gets made.
