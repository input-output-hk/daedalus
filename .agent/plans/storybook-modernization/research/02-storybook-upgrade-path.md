# 02: Storybook 6.4.22 Upgrade Path

Research note. Measurements taken on 2026-09-10 against `feat/drep-discovery` at
`3eaa4f17fac50a959057143904d488303fe4922b`. Upstream versions checked against the npm registry
and the Storybook repository on the same date; `storybook@10.6.0` was the published latest
(`registry.npmjs.org/storybook`, `dist-tags.latest`, modified 2026-09-03).

## Headline finding

React 16.14.0 does not block any Storybook version, including 10.

`@storybook/react@10.6.0` declares `"react": "^16.8.0 || ^17.0.0 || ^18.0.0 || ^19.0.0"` as a peer
dependency, and Storybook 10 still ships a React 16 render shim that is selected automatically:
`code/lib/react-dom-shim/src/react-16.tsx` at tag `v10.0.0` calls `ReactDOM.render` and
`ReactDOM.unmountComponentAtNode`, and `code/lib/react-dom-shim/src/preset.ts` aliases
`@storybook/react-dom-shim` to that file whenever the resolved `react-dom` version does not start
with 18 or 19. The manager UI runs its own prebundled React 18 (`code/core/package.json` at
`v10.0.0` lists `react: ^18.2.0` as a devDependency, not a peer), so it is insulated from the
project's React entirely.

- https://registry.npmjs.org/@storybook/react (peerDependencies for 10.6.0)
- https://github.com/storybookjs/storybook/blob/v10.0.0/code/lib/react-dom-shim/src/preset.ts
- https://github.com/storybookjs/storybook/blob/v10.0.0/code/lib/react-dom-shim/src/react-16.tsx

What forces the rewrite is `storiesOf`, removed in Storybook 8.0.0, and `addon-knobs`, which has no
build for Storybook 9 or later. Those are independent of React. Treating the React 16 to 18 upgrade
as a prerequisite would sequence a large, risky change ahead of one that does not need it.

## What is installed and what it touches

From `package.json` at the measured commit:

| Package | Pinned | Registry latest | Last published | Note |
|---|---|---|---|---|
| `@storybook/react` | 6.4.22 | 10.6.0 | 2026-09-02 | Current |
| `@storybook/builder-webpack5` | 6.4.22 | 10.6.0 | 2026-09-02 | Current |
| `@storybook/addon-links` | 6.4.22 | 10.6.0 | 2026-09-02 | Current |
| `@storybook/core` | 6.4.22 | 8.6.14 | 2025-05-16 | Folded into `storybook` at 9 |
| `@storybook/addon-actions` | 6.4.22 | 9.0.8 | 2025-06-10 | Published as an empty package, "please don't use it anymore" |
| `@storybook/addons` | 6.4.22 | 7.6.17 | 2024-02-20 | Removed at 8 |
| `@storybook/manager-webpack5` | 6.4.22 | 6.5.16 | 2023-01-26 | Absorbed into the framework package at 7 |
| `@storybook/addon-knobs` | 6.4.0 | 8.0.1 | 2024-11-20 | Unusable from 9 |
| `@dump247/storybook-state` | 1.6.1 | 1.6.1 | 2022-06-12 | Unmaintained |
| `storybook-addon-swc` | 1.1.7 | 1.2.0 | 2023-04-18 | Declared but never registered |

Four of the ten pinned packages have no version at all in the target line. That is the upgrade in
one row: this is not nine packages moving from 6.4.22 to 10.6.0, it is four packages moving and six
being deleted or replaced.

Estate measured under `storybook/stories`:

| Measure | Count |
|---|---|
| `*.stories.ts`/`*.stories.tsx` files | 80 |
| All `.ts`/`.tsx` files (stories plus wrappers and fixtures) | 118 |
| Files containing a `storiesOf()` call | 65 |
| `storiesOf()` call sites | 69 |
| `.add()` story registrations | 267 |
| `.addDecorator()` calls | 92 |
| Files importing `@storybook/addon-knobs` | 71 |
| Knob call sites (counted only in files importing that symbol) | 394 |
| Files importing `@dump247/storybook-state` | 10 |
| `withState()` call sites | 17 |
| Files importing `@storybook/addon-actions` | 64 |
| Files importing `linkTo` from `@storybook/addon-links` | 6 |

Knob call sites by type: `boolean` 180, `number` 100, `text` 55, `select` 49, `date` 3, `radios` 3,
`button` 2, `object` 1, `optionsKnob` 1.

Two configuration details drive work later in this note. `storybook/main.ts` sets
`stories: ['../storybook/stories/index.ts']`, a single barrel of side-effect imports rather than a
glob, and `storybook/preview-head.html` is a zero-byte file. `storybook-addon-swc` appears in
`package.json` and nowhere else in the repository: `storybook/main.ts` pushes its own `swc-loader`
rule through `webpackFinal` and never registers the addon.

## 1. Version requirements per line

| | Storybook 8 (8.6.18 latest) | Storybook 9 (9.1.20 latest) | Storybook 10 (10.6.0 latest) |
|---|---|---|---|
| React (preview) | `^16.8.0 \|\| ^17 \|\| ^18 \|\| ^19.0.0-beta` | same | `^16.8.0 \|\| ^17 \|\| ^18 \|\| ^19` |
| React (manager) | prebundled React 18, not a peer | prebundled React 18 | prebundled React 18 |
| Node | 18 or above | 20 or above | 20.19+ or 22.12+ |
| TypeScript | `>= 4.2.x` | `>= 4.9.x` | `>= 4.9.x`, plus `moduleResolution` of `bundler`, `node16` or `nodenext` |
| webpack | 5, via `@storybook/react-webpack5` | 5, same | 5, same |
| Vite required | no | no | no |
| webpack5 builder | supported | supported | supported, published at 10.6.0 |
| Legacy decorators | unaffected by Storybook; the builder is compiler-agnostic from 8.0 | same | same |
| Package manager floor | not stated | npm 10+, yarn 4+, pnpm 9+ | same |

Sources: `MIGRATION.md` on `next`, sections "From version 8.x to 9.0.0" (TypeScript < 4.9, Node.js
< 20, package managers) and "From version 9.x to 10.0.0" (Node.js 20.19+ or 22.12+, `moduleResolution`,
ESM-only main config); `MIGRATION.md` "Dropping support for Node.js 16" for the Storybook 8 floor.
Peer ranges read from the npm registry documents for `@storybook/react` and
`@storybook/react-webpack5` at 8.6.18, 9.1.20 and 10.6.0.

- https://github.com/storybookjs/storybook/blob/next/MIGRATION.md
- https://storybook.js.org/docs/releases/migration-guide

Four consequences for this repository.

**Node is already fine.** `nix/internal/common.nix:220` builds the dev shell's Node from
`pkgs.nodejs_24`, above every floor listed.

**Yarn sits below the stated floor.** `package.json` pins `yarn@1.22.21`. Storybook 9 states a
minimum of yarn 4, hedged as "While Storybook may still work with older versions". The Storybook CLI
runs installs and automigrations through the detected package manager, so a Yarn 1 tree is the least
tested path through `storybook upgrade`. Yarn 1 also hoists flat, and a partially upgraded Storybook
tree is exactly the shape that breaks under flat hoisting.

**Storybook 10 forces a TypeScript decision.** The repo is on `typescript@4.9.5` with
`"moduleResolution": "node"` in `tsconfig.json`. Storybook 10 removed the `typesVersions` fields
that made `node` resolution work, so the config must move to `bundler`, `node16` or `nodenext`.
`bundler` was introduced in TypeScript 5.0, so on 4.9.5 the only options are `node16` or `nodenext`,
which change resolution semantics across the whole project. `yarn compile` is `tsc --noEmit` and
`tsconfig.json` carries no `include`, only `"exclude": ["node_modules"]`, so it type-checks
`storybook/` alongside `source/`. There is no way to scope this change to the Storybook directory.

- https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-0.html

**Legacy decorators are not a Storybook constraint.** From 8.0 the webpack5 builder dropped
its Babel dependency and became compiler-agnostic: "In Storybook 8.0, we have removed the
`@storybook/builder-webpack5` package's dependency on Babel." The compiler is now supplied by
`@storybook/addon-webpack5-compiler-swc` (latest 4.0.3, published 2026-03-25) or by a `swc` callback
in `main.ts`, and either way the `jsc.parser.decorators` and `jsc.transform.legacyDecorator` options
pass straight through. `storybook/main.ts:71` already configures `swc-loader` by hand with
`legacyDecorator: true` and `useDefineForClassFields: false`, and that rule survives the upgrade
unchanged because `webpackFinal` is still supported.

- https://storybook.js.org/docs/api/main-config/main-config-swc

## 2. storiesOf: when it was removed, and whether any bridge survives

`storiesOf` was deprecated in Storybook 7.5.0 and removed in 8.0.0. `MIGRATION.md` states it
plainly: "The `storiesOf` API has been removed in Storybook 8.0."

The replacement is Component Story Format. A file exports a default object carrying `title` and
component-level annotations, and one named export per story.

Three candidate bridges, all dead ends.

**The `storyStoreV6` feature flag.** `storiesOf` depends on the v6 story store. `MIGRATION.md`
records `storyStoreV7: true` as "the default and only option in Storybook 8", and lists
`storyStoreV6` alongside `storiesOf` in the 7.5.0 deprecation. There is no flag to restore it in 8
or later. This repository is on the v6 store today by default: `storybook/main.ts` sets no
`features` block, and Storybook 6.4 shipped v7 as opt-in only.

**`@storybook/preview-api` compatibility.** It has none. `code/core/src/preview-api/index.ts` at tag
`v10.0.0` exports the hooks API (`useArgs`, `useChannel`, `useGlobals`, `useState`, and the rest),
`makeDecorator`, and an `addons` export marked `@deprecated`. `storiesOf` is not among them.

**The `storiesof-to-csf` codemod.** It exists, but not in a version you would upgrade to. Listing
`code/lib/codemod/src/transforms` by tag shows `storiesof-to-csf.js` present at `v7.6.17`, `v8.0.0`
and `v8.6.14`, and absent at `v9.0.0` and `v10.0.0`. What it converts, and what it leaves, is
measured further down.

The only supported way to keep a `storiesOf`-shaped authoring API past 8 is to implement it yourself
through the `experimental_indexers` main-config API, which `MIGRATION.md` points at as the
alternative. That means owning a custom indexer against an API still labeled experimental, in
exchange for keeping an authoring style the ecosystem has abandoned. It is not a serious option.

- https://github.com/storybookjs/storybook/blob/next/MIGRATION.md (sections "Removal of `storiesOf`-API", "`storyStoreV6` and `storiesOf` is deprecated")
- https://github.com/storybookjs/storybook/blob/v10.0.0/code/core/src/preview-api/index.ts

## 3. addon-knobs: removal, replacement, and what actually converts

Knobs was deprecated in Storybook 6.3 in favor of Controls: "We are replacing
`@storybook/addon-knobs` with `@storybook/addon-controls`." The pinned 6.4.0 release carries an npm
`deprecated` field reading "deprecating @storybook/addon-knobs in favor of @storybook/addon-controls".
The addon moved out of the monorepo to `storybookjs/addon-knobs`, whose README opens "Storybook Addon
Knobs (deprecated)".

Deprecation is not where support ends. The last knobs release is 8.0.1, published
2024-11-20, and it peers on `@storybook/manager-api@^8.0.0`, `@storybook/theming@^8.0.0`,
`@storybook/components@^8.0.0` and `@storybook/core-events@^8.0.0`. Storybook 9 consolidated
all four of those into the `storybook` package under new paths (`storybook/manager-api`,
`storybook/theming`, and so on) and stopped publishing them. So knobs works on Storybook 8 and is
unusable from Storybook 9 onward. Storybook 8 is therefore the only version at which knobs can be
kept while the `storiesOf` conversion happens.

The replacement is args plus controls. Instead of calling a knob inside the render body, a story
declares `args` and reads them from its props, with `argTypes` describing the control. Storybook's
own migration notes give the shape:

```jsx
// knobs
export const Basic = () => <Button label={text('Label', 'hello')} />;

// args
export const Basic = (args) => <Button {...args} />;
Basic.args = { label: 'hello' };
```

**There is no codemod for knobs.** No transform named for knobs has ever existed in
`code/lib/codemod/src/transforms` at any tag inspected (7.6.17, 8.0.0, 8.6.14, 9.0.0, 10.0.0). The
official guidance is prose with worked examples, not tooling. Any estimate that assumes automated
knob conversion is wrong.

What converts mechanically and what does not, against the 394 measured call sites:

| Knob type | Sites | Control replacement | Mechanical? |
|---|---|---|---|
| `boolean(label, default)` | 180 | `control: 'boolean'`, `args: { x: default }` | Yes, when the call sits directly in a story's render body |
| `number(label, default)` | 100 | `control: 'number'`, or `{ type: 'range', min, max, step }` when knob options are given | Yes |
| `text(label, default)` | 55 | `control: 'text'` | Yes |
| `select(label, options, default)` | 49 | `control: 'select'` with `options` | Yes where `options` is a value the story can hoist. 10 of the 49 pass an inline object literal, the other 39 pass an identifier |
| `radios(label, options, default)` | 3 | `control: 'radio'` | Yes |
| `date(label, default)` | 3 | `control: 'date'`; the knob returns a timestamp number, the control returns a `Date`, so the call site needs adjusting | No, needs judgment |
| `object(label, default)` | 1 | `control: 'object'` | Yes |
| `optionsKnob(label, options, default, config)` | 1 | `control: 'check'`, `'inline-check'`, `'radio'` or `'select'` depending on the knob's `display` config | No, needs judgment |
| `button(label, handler)` | 2 | No equivalent. Args are values, not actions. Either drop the button or move the behavior into the story | No, needs judgment |

The type table understates the work, because position matters more than type. A knob is a
function call that can appear anywhere in a render body. An arg is a static declaration on the story
that arrives as a prop. Of the 394 call sites, 204 sit at an indentation of eight spaces or more,
meaning they are nested inside a callback, a mapped list, or JSX rather than at the top of a story
body. A nested knob has no mechanical arg equivalent: the arg must be hoisted to the story's
signature and threaded down, which changes the surrounding code.

A further 75 of the call sites, spread over 8 files, sit in files that also use `withState`. Those
files cannot be converted until the choice between `useArgs` and `useState` is settled, because both
rewrites land on the same story bodies.

So of 394 sites, 190 sit at the top of a story body and are a two-line mechanical edit, and 204 are
nested and need someone to read the story and decide where the value belongs. There is no tooling
for either half.

- https://github.com/storybookjs/storybook/blob/next/MIGRATION.md (section "Deprecated addon-knobs")
- https://github.com/storybookjs/addon-knobs
- https://github.com/storybookjs/storybook/blob/v8.6.14/code/addons/controls/README.md (section "How do I migrate from addon-knobs?")
- https://storybook.js.org/docs/essentials/controls

## 4. @dump247/storybook-state

Version 1.6.1 was published on 2022-06-12 and is still the latest. Four years without a release, and
the package predates the Storybook 7 architecture entirely. It is not going to gain Storybook 9 or 10
support.

It is used in 10 files across 17 `withState()` call sites. The pattern is
`withState(initialState, (store) => JSX)` where the story body reads `store.state` and calls
`store.set(...)`.

Two modern replacements, and the choice is not obvious.

`useArgs` from `storybook/preview-api` is the canonical one. It returns `[args, updateArgs]` inside
a story, and the state is the story's args, so it shows up in the Controls panel and survives a URL
share. This is the right target where the state is genuinely a component input, which covers most of
the measured uses (a selected wallet, a route, a toggled panel).

`useState` from `storybook/preview-api` is the closer mechanical analogue: local component state
inside a story, invisible to controls. It is the cheaper conversion and the right target where the
state is scratch state that no viewer should be poking at.

Both are exported from `code/core/src/preview-api/index.ts` at tag `v10.0.0`. Neither is a drop-in
for `withState`, because `withState` wraps the story in a higher-order component and both hooks are
called inside it, so the story signature changes in every case. 17 call sites, all handwritten, and
8 of the 10 files also carry knobs that must be rewritten in the same pass.

- https://github.com/storybookjs/storybook/blob/v10.0.0/code/core/src/preview-api/index.ts
- https://registry.npmjs.org/@dump247/storybook-state

## 5. storybook-addon-swc

Delete it now, independent of any upgrade. It is declared at `package.json:175` and referenced
nowhere else in the repository. `storybook/main.ts` configures `swc-loader` directly inside
`webpackFinal`, so the addon is not registered and does nothing. Its own peer range is
`webpack: ^4.0.0 || ^5.0.0` with no Storybook peer at all, and its last release was 1.2.0 on
2023-04-18.

For the upgraded configuration there are two supported ways to get SWC, and the existing hand-rolled
loader rule is one of them. Keeping the `webpackFinal` rule is the lower-risk choice, because it is
already carrying the `legacyDecorator` and `useDefineForClassFields` settings that MobX 5 depends on,
and because that rule also carries the SCSS, CSS, inline-SVG and asset rules the stories need. The
alternative, `@storybook/addon-webpack5-compiler-swc` plus a `swc` callback in `main.ts`, is the
documented path and is maintained (4.0.3, 2026-03-25), but it would mean re-deriving settings that
already work.

- https://registry.npmjs.org/storybook-addon-swc
- https://storybook.js.org/docs/api/main-config/main-config-swc

## 6. The custom addon API and DaedalusMenu

DaedalusMenu spans both sides of the Storybook process boundary through one import.
`storybook/addons/DaedalusMenu/register.tsx` runs in the manager and calls
`addons.register`, `addons.add` and `types.TOOL`. `storybook/addons/DaedalusMenu/index.ts` runs in
the preview and calls `addons.getChannel()`; it is imported by
`storybook/stories/_support/StoryWrapper.tsx` and `storybook/stories/settings/general/General.stories.tsx`.
Both import from `@storybook/addons`.

What changed, in order:

**Storybook 7** split `@storybook/addons` into `@storybook/preview-api` (preview side) and
`@storybook/manager-api` (manager side), keeping `@storybook/addons` as a deprecated shim.

**Storybook 8** removed the shim. `MIGRATION.md` lists `@storybook/addons` first under "Removed
deprecated shim packages". The npm registry agrees: the latest `@storybook/addons` is 7.6.17. The
same release rendered manager addons with React 18 and stopped passing a `key` prop to the `render`
function.

**Storybook 9** consolidated both halves into the `storybook` package: `@storybook/manager-api`
becomes `storybook/manager-api` and `@storybook/preview-api` becomes `storybook/preview-api`.

**Storybook 10** requires local addons to be fully resolved, so
`require.resolve('./addons/DaedalusMenu/register.tsx')` in `storybook/main.ts:13` becomes
`import.meta.resolve('./addons/DaedalusMenu/register.tsx')`, and `main.ts` itself must be valid ESM
with no `require`, `__dirname` or `__filename`. The addon migration guide also states that Storybook
10 "requires all addons to be built as ESM-only", which matters for published addons and is a
non-issue for a local one that the builder compiles.

The manager API the addon actually leans on survives. `setQueryParams` is still on the URL module at
`code/core/src/manager-api/modules/url.ts` in `v10.0.0`, alongside `getQueryParam`. So a
straight port is small: change two import paths, change one resolve call, and confirm the React 18
render path.

A straight port may be the wrong move. DaedalusMenu is a theme, locale and OS switcher that
predates a first-class Storybook feature covering the same ground. Modern Storybook declares
toolbar dropdowns as `globalTypes` in the preview config, seeds them with `initialGlobals`, and lets
a decorator read `context.globals`. No manager-side addon is needed:

```ts
// preview.ts
const preview = {
  globalTypes: {
    theme: { toolbar: { title: 'Theme', items: [...], dynamicTitle: true } },
  },
  initialGlobals: { theme: 'light' },
  decorators: [(Story, context) => <ThemeManager theme={themes[context.globals.theme]}><Story /></ThemeManager>],
};
```

That deletes `storybook/addons/DaedalusMenu/` entirely, along with the hand-rolled channel protocol
(`daedalusMenu/updateParam`, `daedalusMenu/paramUpdated`), the manual `sessionStorage` persistence,
and the `parent.window.location.hash` manipulation in `DaedalusMenu.tsx` that carries a `@ts-ignore`.
Globals are persisted and URL-encoded by Storybook itself. The one thing to check before committing
to it is `StoryWrapper.tsx`, which currently reconstructs the same three values through
`onReceiveParam` and holds them in component state; under globals it reads them from the decorator
context instead, which is simpler but is a real edit to a file every story depends on.

- https://github.com/storybookjs/storybook/blob/next/MIGRATION.md (sections "Removed deprecated shim packages", "Manager addons are now rendered with React 18", "Dropped support for legacy packages", "Local addons must be fully resolved")
- https://storybook.js.org/docs/addons/addon-migration-guide
- https://storybook.js.org/docs/essentials/toolbars-and-globals
- https://github.com/storybookjs/storybook/blob/v10.0.0/code/core/src/manager-api/modules/url.ts

## 7. Codemods and their real coverage

`npx storybook@latest upgrade` finds Storybook projects, bumps dependencies, and runs automigrations.
`npx storybook@latest automigrate` runs the automigrations alone. Both are documented in the
migration guide. The automigration set at `v10.0.0` is 20 fixes in
`code/lib/cli-storybook/src/automigrate/fixes`, and the ones that would touch this repository are
`renderer-to-framework` (rewrite `@storybook/react` imports and the `framework` field),
`consolidated-imports` (rewrite `@storybook/manager-api` and friends to their `storybook/*` paths),
`wrap-getAbsolutePath`, and `fix-faux-esm-require`. `remove-essentials` and `remove-addon-interactions`
do not apply, because neither addon is installed.

Automigrations operate on configuration and dependencies. They do not rewrite story bodies. Every
one of the 394 knob call sites and 17 `withState` call sites is outside their reach.

The codemods that touch story files, by tag:

| Codemod | v7.6.17 | v8.0.0 | v8.6.14 | v9.0.0 | v10.0.0 |
|---|---|---|---|---|---|
| `storiesof-to-csf` | present | present | present | removed | removed |
| `csf-hoist-story-annotations` | present | present | present | removed | removed |
| `csf-2-to-3` | present | present | present | present | present |
| `mdx-to-csf` | present | present | present | removed | removed |
| `upgrade-hierarchy-separators` | present | present | present | present | present |

So the documented three-step chain (`storiesof-to-csf`, then `csf-hoist-story-annotations`, then
`csf-2-to-3`) only exists in a Storybook 8 or 7 CLI. The tooling that converts 65 files of legacy
API does not ship in the version we would otherwise jump to.

What `storiesof-to-csf` actually does, read from
`code/lib/codemod/src/transforms/storiesof-to-csf.js` at `v8.6.14`:

It converts chained `storiesOf(title, module).addDecorator(d).add(name, fn)` into a default export
carrying `title` and `decorators`, plus one `export const` per story. It sanitizes story names into
identifiers and re-attaches the original display name. The source comment states the limit outright:
"NOTES: only support chained `storiesOf()` calls". It requires the `storiesOf` title to be a string
literal and each `.add()` name to be a string literal, and it skips any file that already has a
default export, logging "existing default export, SKIPPING".

Measured against this estate:

- 65 files contain a `storiesOf()` call; all 65 use a string-literal title.
- 0 of those 65 already have a default export, so none would be skipped.
- Of 267 `.add()` registrations, 258 use a string-literal name. The 9 that do not are all in
  `storybook/stories/staking/Staking.stories.tsx`.
- 0 files register stories inside a `forEach` or `map` loop.
- 80 story display names contain characters that are not letters, digits or spaces, so the codemod's
  sanitizer will rename the export and emit an explicit display name for each. That is correct
  behavior, but it means 80 hand-checks that the sidebar label survived.

The codemod emits CSF 1 shape (`Story.story = { name, parameters, decorators }`), which is why the
documented chain runs `csf-hoist-story-annotations` next to lift those onto `Story.storyName` and
friends, and `csf-2-to-3` last to reach the modern object form.

The split across the 80-file estate:

| Work | Covered by codemod | Hand work |
|---|---|---|
| `storiesOf` to CSF shape: title, decorators, story exports, names | 64 of 65 files fully, 1 file partly | Review all 65; rewrite the 9 dynamic registrations in `Staking.stories.tsx` |
| CSF 1 to CSF 3 annotation shape | yes, two further codemod passes | Spot-check |
| `main.ts`: `framework` field, builder removal, script rename, `stories` glob | automigrations cover framework and imports | The `stories` barrel has to be replaced by a glob by hand, and its side-effect imports rehomed into `preview` |
| `addon-knobs` to args and controls, 394 sites | none | all 394 |
| `withState` to `useArgs` or `useState`, 17 sites | none | all 17 |
| `addon-actions` import path change in 64 files | `consolidated-imports` automigration | Verify |
| DaedalusMenu port or replacement | none | all of it |

Codemods handle the file structure of 65 files and none of the 411 knob and state call sites inside
them. The shape conversion is covered; the semantic conversion, which is the bulk of the hours, is
not.

- https://storybook.js.org/docs/releases/migration-guide
- https://github.com/storybookjs/storybook/blob/v8.6.14/code/lib/codemod/src/transforms/storiesof-to-csf.js
- https://github.com/storybookjs/storybook/tree/next/code/lib/codemod

## 8. Can the upgrade be staged

Yes, and the intermediate is Storybook 8, not Storybook 9.

Storybook 8 is the last line where `addon-knobs` still has a working build (8.0.1, peering on
`@storybook/manager-api@^8.0.0`) and the last line whose CLI still ships `storiesof-to-csf` and
`csf-hoist-story-annotations`. That combination is the whole argument: at 8 you can convert 65 files
from `storiesOf` to CSF with tooling while leaving all 394 knob call sites untouched, because knobs
works fine as a decorator inside a CSF story. The two largest pieces of work get separated instead of
landing in one unreviewable change.

What landing on 8 costs, and none of it is throwaway:

- `main.ts` gains the mandatory `framework: { name: '@storybook/react-webpack5' }` field, and
  `@storybook/builder-webpack5`, `@storybook/manager-webpack5` and `@storybook/core` are removed.
- `package.json` scripts change from `start-storybook`/`build-storybook` to `storybook dev` and
  `storybook build`, since those binaries were removed in 7.0. `perSystem/checks.nix:78` runs
  `yarn storybook:build`, so the check keeps working as long as the script name is stable.
- The `stories` barrel becomes a glob and its side effects move into `preview`.
- `@storybook/addons` imports in DaedalusMenu split into `@storybook/preview-api` and
  `@storybook/manager-api`, and the manager render path moves to React 18.
- `preview.tsx` moves to a default-export `Preview` object.
- 65 files run through three codemods and get reviewed.

Every one of those is required for 9 and 10 as well, except that the 9 and 10 hop rewrites the two
`@storybook/*-api` imports again into `storybook/*` paths. That is a small, automigrated delta.

What jumping straight to 10 costs instead:

- The 65-file `storiesOf` conversion is done by hand, because the codemod is not in the 10 CLI. It
  can be run by installing `@storybook/codemod@8.6.14` and invoking `jscodeshift` directly, which is
  supported (the codemod README documents running transforms by hand) but is an unversioned side
  channel rather than a documented upgrade path.
- All 394 knob call sites must be converted in the same change, because knobs cannot load on 9 or 10.
- The TypeScript `moduleResolution` change lands at the same time, affecting `tsc --noEmit` across
  `source/` as well as `storybook/`.
- `main.ts` must become ESM in the same change, losing `require`, `__dirname` and the two
  `require.resolve` calls it currently uses.
- The Storybook build check is red for the entire duration, since the estate cannot be converted
  incrementally in one branch.

Against the intermediate: landing on 8 means the repository sits on a version that is two majors
behind on the day it lands, and the hop from 8 to 10 is a second round of review, a second CI-red
window, and a second set of import rewrites. If the whole conversion is going to be done in one
sustained effort anyway, the intermediate buys sequencing and not total effort.

A third option is cheaper than both. Land on 8 with the story conversion done, keep knobs, and then
treat the knobs-to-args conversion as ordinary incremental work spread across normal component
changes, upgrading to 10 once the knob count reaches zero. That turns the 394-site conversion from a
blocking project into a background one, at the price of staying on 8 for as long as it takes.

## 9. The decision table

| Target | React demanded | What it forces us to rewrite | What it buys |
|---|---|---|---|
| Stay on 6.4.22 | 16.14.0, current | Nothing | Nothing. The pinned Storybook packages were published 2022-04-14 and the pinned knobs release 2021-11-27, so no upstream fix reaches the repo, no modern addon can be installed, and the conversion cost grows with every story added |
| **8.6.x** | **16.14.0 unchanged.** Peer `^16.8.0 \|\| ^17 \|\| ^18 \|\| ^19.0.0-beta` | `storiesOf` to CSF in 65 files (codemod-assisted); `framework` field and builder packages in `main.ts`; CLI binary rename in two scripts; `stories` barrel to glob; `preview.tsx` to default export; DaedalusMenu imports split across preview and manager APIs; `@storybook/addons` gone. **Knobs stay. `withState` stays.** | The only version where the `storiesOf` codemod and a working knobs build coexist. Compiler-agnostic webpack5 builder. Keeps the largest single conversion (394 knob sites) out of this change |
| 9.1.x | 16.14.0 unchanged. Same peer range | Everything in the 8 row, plus all 394 knob sites to args and controls, plus 17 `withState` sites, plus `@storybook/*-api` imports rewritten to `storybook/*`, plus `@storybook/addon-actions` to `storybook/actions` in 64 files. Node 20+, TypeScript 4.9+ (already met) | 48% smaller install than 8. Controls, actions, viewport, toolbars and interactions folded into core, so the addon list shrinks to `@storybook/addon-links`. Built-in a11y and interaction testing. Story globals, which is what DaedalusMenu should become |
| 10.6.x | 16.14.0 unchanged. Peer `^16.8.0 \|\| ^17 \|\| ^18 \|\| ^19`, and a React 16 render shim still ships | Everything in the 9 row, plus `main.ts` to strict ESM (no `require`, no `__dirname`), `require.resolve` to `import.meta.resolve` for the local addon, and `tsconfig.json` `moduleResolution` off `node`, which on TypeScript 4.9.5 means `node16`/`nodenext` or a TypeScript 5 upgrade. Applies to `tsc --noEmit` over the whole repo | A further 29% install reduction over 9. Current line, so upstream fixes and addon compatibility land here. CSF Factories available. Module automocking on the webpack builder |

Storybook 8 is the only version at which the two hardest pieces of this migration, the `storiesOf`
conversion and the knobs conversion, can be separated.

## 10. What needs a decision

1. **Intermediate or direct.** Land on 8 with codemod-assisted story conversion and knobs intact, or
   convert stories and knobs together and land on 10. The evidence favors the intermediate; the cost
   is a second upgrade later.
2. **If the intermediate is chosen, how long we sit on 8.** Convert all 394 knob sites in a follow-up
   push, or drain them incrementally as components are touched and upgrade when the count hits zero.
3. **`useArgs` or `useState` for the 17 `withState` sites.** Args-backed state is the idiomatic
   answer and puts the values in the Controls panel; local hook state is the closer mechanical
   translation. The two give different results for the same story and the choice should be made once,
   not per file.
4. **Port DaedalusMenu or delete it.** Porting is a small import change. Replacing it with
   `globalTypes` toolbars deletes the addon, the channel protocol and the hash manipulation, but edits
   `StoryWrapper.tsx`, which every story depends on.
5. **TypeScript, at the point 10 is targeted.** `node16`/`nodenext` on TypeScript 4.9.5, or upgrade
   to TypeScript 5.x and use `bundler`. This is a repository-wide resolution change, not a Storybook
   one.
6. **Yarn.** Storybook 9 names yarn 4 as its floor while stating older versions may still work. Either
   accept Yarn 1 and expect the CLI's automigrations to be the least-tested path, or treat a package
   manager upgrade as a prerequisite. The Nix dev shell builds yarn from `pkgs.yarn`, so this is not a
   `package.json` edit alone.

Two cleanups are decided already and need no discussion. `storybook-addon-swc` is declared in
`package.json` and used nowhere, and `storybook/preview-head.html` is zero bytes. Both can go now.

## What this note does not establish

It does not attempt an hours estimate. The counts here are of call sites, not of decisions, and the
204 nested knob calls are the ones that will dominate the time.

It has not built anything. No Storybook version above 6.4.22 was installed, and no codemod was run
against this repository. The codemod coverage figures come from reading the `storiesof-to-csf`
transform source at `v8.6.14` and matching its stated preconditions against measured properties of
the estate. That is a prediction, not an observation. A codemod dry run on a scratch branch would
convert it into one.

It says nothing about visual output. Whether the converted stories still render correctly is a
separate question from whether they compile, and `yarn storybook:build` proves only the second. No
visual regression tooling is installed: `package.json` contains no Chromatic, Loki, Storyshots or
Percy dependency, so 267 converted stories have no automated check that they still look right. That
gap exists today, before any upgrade.

It does not cover `@storybook/addon-docs`, autodocs or MDX, because none are in use. The
`mdx-to-csf` codemod's removal at 9.0.0 is therefore not a constraint here.

It has not verified that `react-polymorph`, which every story reaches through `StoryDecorator`,
behaves identically under the modern preview. Nothing in the peer ranges suggests it would not, but
the component library sits between Storybook and every rendered story and was not exercised.
