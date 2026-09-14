# Is the React 16 to 18/19 upgrade a prerequisite for the Storybook work

Status: research complete
Date: 2026-09-10
Measured at: branch `feat/drep-discovery`, commit `3eaa4f17f`

## Verdict

The Storybook modernization is **not blocked** on React. It can proceed on React 16.14.0 all the
way to Storybook 10. Two things are given up by staying there, and neither is on the critical
path: concurrent-rendering behavior goes unexercised in Storybook, and `@storybook/addon-knobs`
has to be abandoned, which it does anyway at Storybook 9 whatever React version we are on.

The React upgrade is a separate, much larger workstream, and it is itself blocked. React 18
is reachable but expensive. React 19 is unreachable until `react-intl` is migrated off 2.9.0,
which touches 323 files, and until two abandoned dependencies are replaced or vendored:
`react-svg-inline`, whose newest ever release caps at React 16, and `react-custom-scrollbars`,
which caps at React 16 upstream and at React 18 in its community fork. Coupling the two
workstreams would convert a scoped Storybook rewrite into a whole-frontend migration.

Storybook's React renderer detects the installed `react-dom` version at build time and swaps its
rendering shim accordingly. From the published Storybook 9.1.20 artifact,
`@storybook/react-dom-shim@9.1.20/dist/preset.js`:

```js
let {version} = JSON.parse(await readFile(join(reactDom, "package.json"), "utf-8"));
return version.startsWith("18") || version.startsWith("19") || version.startsWith("0.0.0")
```

```js
webpackFinal = async (config, options) =>
  await getIsReactVersion18or19(options)
    ? config
    : {...config, resolve: {...config.resolve, alias: {...config.resolve?.alias,
        "@storybook/react-dom-shim": "@storybook/react-dom-shim/dist/react-16"}}}
```

The React 16 branch of that shim is a five-line file that calls `ReactDOM.render` and
`ReactDOM.unmountComponentAtNode`. React 16.14.0 falls into that branch automatically, with no
configuration. The same code is present unchanged on the Storybook `main` branch today, at
`code/lib/react-dom-shim/src/preset.ts`.

The peer ranges agree. From the npm registry:

| Package | React peer range |
|---|---|
| `@storybook/react@7.6.20` | `^16.8.0 \|\| ^17.0.0 \|\| ^18.0.0` |
| `@storybook/react@8.6.14` | `^16.8.0 \|\| ^17.0.0 \|\| ^18.0.0 \|\| ^19.0.0-beta` |
| `@storybook/react@9.1.20` | `^16.8.0 \|\| ^17.0.0 \|\| ^18.0.0 \|\| ^19.0.0-beta` |
| `@storybook/react@10.6.0` | `^16.8.0 \|\| ^17.0.0 \|\| ^18.0.0 \|\| ^19.0.0` |
| `@storybook/react-webpack5@10.6.0` | `^16.8.0 \|\| ^17.0.0 \|\| ^18.0.0 \|\| ^19.0.0` |

Storybook's own stated React floor is 16.3, set by its use of Emotion, recorded in Storybook's
`MIGRATION.md` under the heading "React 16.3+". We are at 16.14.0.

## 1. Class components versus function components

Counted over `source/renderer/app`, excluding `*.spec.tsx`:

```bash
find source/renderer/app -name "*.tsx" ! -name "*.spec.tsx" | wc -l                    # 485
find source/renderer/app -name "*.tsx" ! -name "*.spec.tsx" \
  | xargs grep -lE "extends\s+(React\.)?(Pure)?Component" | wc -l                      # 333
```

| Location | Class-component files | Total non-spec `.tsx` |
|---|---|---|
| `source/renderer/app/components` | 236 | 361 |
| `source/renderer/app/containers` | 95 | 105 |
| Elsewhere (`App.tsx`, `ThemeManager.tsx`, `features/`) | 3 | 19 |
| Total | 333 | 485 |

There are 334 `extends Component` occurrences across those 333 files, so one file declares two.

The remaining 152 files hold no class component. 144 of them contain JSX, 41 use hooks, one is
an `index.tsx` barrel. Treating "non-class file containing JSX" as the function-component
population gives roughly 144 function-component files against 333 class-component files, so the
codebase is about 70 percent class components by file.

Hook call sites across `source/renderer/app`, 49 files total:

```
useState 92 · useCallback 62 · useEffect 51 · useMemo 44 · useRef 25 · useContext 6 · useLayoutEffect 4
```

**Neither React 18 nor React 19 removes class components.** `React.Component`,
`React.PureComponent`, `setState`, `static defaultProps` on classes, and the standard lifecycle
methods all survive into React 19. The 333 class components are not, by themselves, a migration
cost. What breaks is narrower.

## 2. What React 18 and React 19 actually break here

Each item was grepped across `source`, `storybook` and `tests`.

| Pattern | Count | React 18 | React 19 |
|---|---|---|---|
| `ReactDOM.render` | 1 call site | Warns, app runs in React 17 mode | Removed |
| `static contextTypes` (legacy context) | 162 files | Supported | **Removed** |
| `getChildContext` (our code) | 1 site | Supported | **Removed** |
| String refs (`ref="..."`) | 0 | n/a | n/a |
| `this.refs` | 0 | n/a | n/a |
| `findDOMNode` | 0 | n/a | n/a |
| `UNSAFE_componentWillReceiveProps` | 1 site | Supported | Supported |
| Unprefixed `componentWillMount` / `WillUpdate` | 0 | n/a | n/a |
| `defaultProps` on a function component | 2 sites | Supported | **Removed** |
| `static defaultProps` on a class | 134 sites | Supported | Supported |
| `PropTypes` in our code | 0 | n/a | n/a |
| `react-test-renderer` | 0 | n/a | n/a |
| `enzyme` | 0 | n/a | n/a |
| `react-dom/test-utils` | 0 | n/a | n/a |
| `React.StrictMode` | 0 | n/a | n/a |
| `unstable_batchedUpdates` | 0 | n/a | n/a |

The React 19 removals are quoted from the React 19 upgrade guide, which lists legacy context
(`contextTypes` and `getChildContext`), string refs, `defaultProps` for function components,
`propTypes`, module pattern factories, `React.createFactory`, `ReactDOM.render`,
`ReactDOM.hydrate`, `unmountComponentAtNode`, `findDOMNode`, and `react-test-renderer/shallow`.
Legacy context is explicitly noted there as class-only and deprecated since 16.6, which is why it
still functions on React 18 and stops functioning on React 19.

Detail on the entries that matter.

**`ReactDOM.render`, one call site.** `source/renderer/app/index.tsx:3` imports `render` from
`react-dom` and calls it at `source/renderer/app/index.tsx:64`. On React 18 this keeps working but
prints "ReactDOM.render is no longer supported in React 18" and puts the whole app in React 17
compatibility mode, which means no automatic batching, no concurrent features, and no transitions.
On React 19 it is gone. Converting it to `createRoot` is a five-line change. The consequence is
that automatic batching then applies everywhere, and that behavior change cannot be found by grep.

**Legacy context, 162 files.** This is the largest single item and it is not our design choice.
Of the 162 files declaring `static contextTypes`, 154 declare exactly:

```ts
static contextTypes = {
  intl: intlShape.isRequired,
};
```

That is `react-intl` 2.9.0's delivery mechanism, not ours, and it is the reason the React 19 move
is gated on migrating `react-intl` off 2.9.0.

**`defaultProps` on function components, two sites in our code.**
`source/renderer/app/components/staking/stake-pools/StakePoolsList.tsx:167` assigns
`StakePoolsList.defaultProps` where `StakePoolsList` is `observer((props) => ...)` declared at
line 55, and `source/renderer/app/components/staking/widgets/PoolPopOver.tsx:100` does the same
for `export function PoolPopOver(props)` at line 18. Both would silently receive `undefined`
instead of their defaults on React 19. Both are trivial to convert to destructuring defaults.

**Automatic batching.** Not a grep result, a behavior change. Once `createRoot` is adopted,
state updates inside promises, `setTimeout`, and native event handlers are batched. The MobX
stores drive re-renders through `observer`, so most of this is absorbed by MobX, but any test or
component that assumed a synchronous DOM update after an `await` will need `flushSync` or a
rewrite. This cannot be enumerated by grep and has to be found by running the suite.

**Test tooling.** The Jest suite uses no `react-test-renderer` and no `enzyme`. It uses
`@testing-library/react` in 27 files across 70 spec files. The installed version is 12.1.2, whose
peer range is `react: "*"` but which internally renders through `ReactDOM.render`. React 18
requires `@testing-library/react` 13 or later; the current latest, 16.3.3, peers
`react: "^18.0.0 || ^19.0.0"`. So the Jest suite is a React 18 work item, but a contained one:
27 files, plus `tests/_utils/TestBed.tsx`.

**TypeScript types.** `@types/react` is pinned at 16.9.56 and `@types/react-dom` is not installed
at all. `@types/react` 18 removed the implicit `children` prop from `React.FC` and
`React.Component`. 51 sites in `source/renderer/app` declare `children` in a props type already,
but the ones that rely on the implicit declaration will surface as compile errors. A related
cleanup: 49 files still do `import type { Node } from 'react'`, a Flow leftover that only compiles
because each is preceded by a `@ts-ignore`.

## 3. react-intl 2.9.0 is the React 19 gate

`react-intl` 2.9.0 declares `"react": "^0.14.9 || ^15.0.0 || ^16.0.0"`
(`node_modules/react-intl/package.json`). Its implementation is built on the legacy context API.
From `node_modules/react-intl/lib/index.js`:

```
584:  InjectIntl.contextTypes = {
1055: IntlProvider.contextTypes = {
1058: IntlProvider.childContextTypes = {
1121: FormattedDate.contextTypes = {
1185: FormattedTime.contextTypes = {
1371: FormattedRelative.contextTypes = {
1440: FormattedNumber.contextTypes = {
1506: FormattedPlural.contextTypes = {
1670: FormattedMessage.contextTypes = {
1763: FormattedHTMLMessage.contextTypes = {
```

`IntlProvider` publishes the intl object through `getChildContext`, and every consumer reads it
through `contextTypes`. React 19 removes both. The failure mode is not a warning: `IntlProvider`
would publish nothing and every `injectIntl` consumer and every `Formatted*` component would
receive `undefined`.

So the compatibility answer splits:

- **React 18: compatible.** React 18 retains legacy context. `react-intl` 2.9.0 keeps working,
  with deprecation noise. Its one unprefixed `componentWillReceiveProps`
  (`node_modules/react-intl/lib/index.js:1314`) is in `FormattedRelative`, which this codebase
  never imports (0 hits). React 18 does not require touching react-intl.
- **React 19: incompatible, hard stop.** Legacy context is gone. The React upgrade past 18 is
  gated on the react-intl migration.

The size of that migration:

```bash
grep -rl "from 'react-intl'" source storybook   # 323 files (315 source, 8 storybook)
grep -rl "intlShape"          source storybook   # 205 files
grep -rl "FormattedHTMLMessage" source storybook # 75 files, 177 occurrences
grep -rl "defineMessages"     source             # 229 files
jq 'length' source/renderer/app/i18n/locales/en-US.json  # 1653 message keys
```

Those file counts are for the symbol appearing anywhere. Counting instead how many times each
symbol appears inside an `import { ... } from 'react-intl'` statement: `defineMessages` 221,
`intlShape` 191, `injectIntl` 63, `FormattedHTMLMessage` 60, `IntlProvider` 24,
`FormattedMessage` 11, `addLocaleData` 3.

Every one of those symbols except `defineMessages`, `injectIntl`, `IntlProvider` and
`FormattedMessage` was removed on the way to v3 or v4. `intlShape` and `addLocaleData` were removed
in v3 (react-intl was rewritten in TypeScript and dropped `prop-types`, exposing `IntlShape` as an
interface). `FormattedHTMLMessage` and `intl.formatHTMLMessage` were removed in v4, since
`FormattedMessage` gained embedded tag support. `IntlProvider`'s default `textComponent` changed
from `span` to `React.Fragment` in v3, which is a silent layout change across the app unless
`textComponent="span"` is set explicitly.

**The react-intl migration does not require React 18.** react-intl 6.8.9 peers
`react: "^16.6.0 || 17 || 18"` and `typescript: "^4.7 || 5"`. We are on React 16.14.0 and
TypeScript 4.9.5, so v6 is reachable from where we stand today, and it is simultaneously
React-18-ready. That makes the correct order: react-intl 2 to 6 first, on React 16, then React 18,
then React 19 with react-intl 7 or later. react-intl 7.1.x peers
`react: "16 || 17 || 18 || 19"` but also `typescript: "^5.6.0"`, so v7 additionally pulls the
TypeScript upgrade in. react-intl 8 and above require React 19.

Per-major React peer ranges, from the npm registry:

| react-intl | React peer |
|---|---|
| 3.12.1 | `^16.3.0` |
| 4.7.6 | `^16.3.0` |
| 5.25.1 | `^16.3.0 \|\| 17 \|\| 18` |
| 6.8.9 | `^16.6.0 \|\| 17 \|\| 18` |
| 7.1.14 | `16 \|\| 17 \|\| 18 \|\| 19` (TypeScript `^5.6.0`) |
| 8.2.0, 9.0.0 | `19` |
| 10.2.0 | `>=18.0.0` |

None of this touches Storybook. Storybook renders whatever `IntlProvider` the stories wrap
themselves in, currently `storybook/stories/_support/StoryWrapper.tsx:3`, which imports
`IntlProvider` alongside `addLocaleData`. Both move with the app.

## 4. MobX and mobx-react

Installed: `mobx@5.15.7`, `mobx-react@6.3.1`, `mobx-react-lite@2.2.2` (pinned by a `resolutions`
entry in `package.json`), `mobx-react-form@2.0.8`, `mobx-react-router@4.1.0`.

Installed peer ranges:

```
mobx-react@6.3.1        react: "^16.8.0 || 16.9.0-alpha.0"   mobx: "^5.15.4 || ^4.15.4"
mobx-react-lite@2.2.2   react: "^16.8.0"                     mobx: "^4.0.0 || ^5.0.0"
mobx-react-form@2.0.8   mobx: "^2.5.0 || ^3.0.0 || ^4.0.0 || ^5.0.0"
mobx-react-router@4.1.0 mobx: "^3.0.0 || ^4.0.0 || ^5.0.0"   react-router: "^4.0.0 || ^5.0.0"
```

The official mobx-react compatibility matrix:

| mobx-react | MobX | React | Added |
|---|---|---|---|
| v10 | 7.x | >=18 | MobX 7, React 18 strict mode |
| v9 | 6.x | >16.8 | React 18.2 strict mode |
| v7 | 6.x | >16.8 <18.2 | Hooks |
| v6 | 4.x / 5.x | >16.8 <17 | Hooks |

So mobx-react 6 is declared incompatible with React 17 and above. **React 18 requires mobx-react 9
at minimum, and mobx-react 9 requires MobX 6.** Registry check: `mobx-react@9.2.0` peers
`{"mobx": "^6.9.0", "react": "^16.8.0 || ^17 || ^18 || ^19"}`. The latest, 10.0.2, peers
`{"mobx": "^7.0.0", "react": "^18 || ^19"}`.

**Legacy decorators survive the MobX 6 move.** `@observer` and `@inject` are plain higher-order
components applied in decorator position; mobx-react 9 still supports them. They appear in 267
and 95 files respectively and need no change.

The work is in the state decorators. MobX 6 requires `makeObservable(this)` in the constructor of
every class that uses `@observable`, `@computed` or `@action`:

```
@observable  448 occurrences across 36 files
@action      282 occurrences across 26 files
@computed    137 occurrences across 26 files
```

36 constructors to touch, in 27 store files under `source/renderer/app/stores` plus form and
domain classes. MobX publishes `mobx-undecorate` (latest 1.3.1) as a codemod for exactly this
migration.

**mobx-react 9 does not require React 18.** Its React peer is `^16.8.0 || ^17 || ^18 || ^19`, the
same shape as react-intl 6. The MobX 6 migration can therefore land on React 16, before any React
version changes.

**Two satellite packages move with MobX.** `mobx-react-form@2.0.8` caps at MobX 5 and is imported
in 28 files, all through `ReactToolboxMobxForm`. The MobX 6 compatible line begins at 6.18.0
(`mobx: "^6.0.0"`); 7.0.0 widens back to `"^5.15.0 || ^6.0.0 || ^7.0.0"` but adds a `zod` peer.
`mobx-react-router@4.1.0` is worse: its current release, 6.1.0, peers
`{"mobx": "^6.3.2", "react-router": "^6.14.2"}`, so upgrading it forces React Router 5 to 6, a
routing rewrite. It is imported in 3 files, all of which use `RouterStore` and
`syncHistoryWithStore`.

**Concurrent rendering.** The mobx-react documentation states that automatic observable conversion
of `this.state` and `this.props` "is fundamentally incompatible with `StrictMode` in React 18.2 and
higher" and was removed. It advises against marking properties `@computed` in observer components
when they depend on `this.props` or `this.state`. This codebase uses no `React.StrictMode` (0 hits)
and holds `@computed` in 26 files, all stores rather than components, so the exposure is low. It is
still a behavior change that only a running app will confirm.

## 5. react-polymorph

`react-polymorph@1.0.4` from the npm registry, source at `input-output-hk/react-polymorph`.
Imported in 196 files; the heaviest entry points are `PopOver` (49 files), `Button` (49),
`Link` (37), `Input` (31), `Checkbox` (24), and `ThemeProvider` (10, including
`storybook/stories/_support/StoryDecorator.tsx:5`).

**Maintenance status: abandoned, and we do not control it.**

```bash
gh api repos/input-output-hk/react-polymorph --jq '{archived, pushed_at, default_branch}'
# {"archived":false,"default_branch":"develop","pushed_at":"2023-03-01T15:48:09Z"}
gh api "repos/input-output-hk/react-polymorph/commits?per_page=5"
# newest: 2022-04-14 "Merge pull request #213 ... release-1.0.4"
gh release list --repo input-output-hk/react-polymorph   # no releases
gh api repos/input-output-hk/react-polymorph --jq '.permissions'
# {"admin":false,"maintain":false,"pull":true,"push":false,"triage":false}
```

Last functional commit April 2022. No published releases. We have read access only, so a fix
upstream is not available to us; the options are a fork or a vendored copy.

**On React 18 it is not a hard blocker. On React 19 it is, in two specific places.**

Its own declared peer range is open-ended: `"react": ">=16.8.6", "react-dom": ">=16.8.6"`. Nothing
in the package caps at 16. Scanning `node_modules/react-polymorph/lib` for React 18/19 hazards:

```
findDOMNode 0 · componentWillReceiveProps 0 · componentWillMount 0 · contextTypes 0 · ReactDOM.render 0
```

Clean. It uses `createRef` (26), `forwardRef` (5), `useState` (10), `useEffect` (5),
`React.Children` (2), `cloneElement` (4). All of that is fine on 18 and 19.

The React 19 problem is `defaultProps` on function components. Three assignments exist:

```
node_modules/react-polymorph/lib/components/PopOver.js:47         PopOver.defaultProps = {...}
node_modules/react-polymorph/lib/components/PasswordInput.js:117  PasswordInput.defaultProps = {...}
node_modules/react-polymorph/lib/components/ThemeProvider.js:185  ThemeProvider.defaultProps = {...}
```

`ThemeProvider` is a class (`ThemeProvider.js:56`, `function (_Component)`), so React 19 keeps its
defaults. `PopOver` (`PopOver.js:26`, `function PopOver(props)`) and `PasswordInput`
(`PasswordInput.js:48`) are function components. Under React 19, `PopOver` would lose
`themeId: IDENTIFIERS.POP_OVER`, `theme: null`, `allowHTML: false` and `popperOptions: {}`, which
means the skin lookup `themeContext.skins[IDENTIFIERS.POP_OVER]` and the `composeTheme` call in its
body operate on `undefined`. That is a silent break in 49 files.

Its transitive React dependencies, all of which declare React 16 peers:

| Transitive | Version | React peer | Verdict |
|---|---|---|---|
| `create-react-context` | 0.3.0 | `^0.14 \|\| ^15 \|\| ^16` | Safe. `lib/index.js` exports `React.createContext \|\| polyfill`, so on 18/19 it delegates to the real API |
| `react-modal` | 3.1.12 | `^0.14 \|\| ^15 \|\| ^16` | Mostly safe. `Modal.js:49` guards `unstable_renderSubtreeIntoContainer` and `unmountComponentAtNode` behind `isReact16 = ReactDOM.createPortal !== undefined`, which is true on 18/19, so the removed APIs are unreachable. Retains 4 unprefixed `componentWillReceiveProps`, which warn but work through React 19. Current upstream 3.16.3 peers through `^19` |
| `react-scrollbars-custom` | 4.0.21 | `^16.0.0` | Clean of legacy APIs. Latest is 4.1.1, peers `>=16.0.0`, last published 2022 |
| `@tippyjs/react` | 4.2.1 | `>=16.8` | No cap |

None of them caps React 18 in practice. Clearing React 19 needs a fork that fixes two
`defaultProps` assignments, or local wrappers around `PopOver` and `PasswordInput`.

**For the Storybook work, react-polymorph is not a blocker.** It is wired
through `storybook/stories/_support/StoryDecorator.tsx`, which uses `ThemeProvider`, `SimpleSkins`
and `SimpleDefaults`; none of that is version-sensitive and none of it changes when Storybook
moves from 6 to 9.

## 6. Every other React-coupled dependency

Installed peer ranges against usage counts and the current upstream release.

| Package | Installed | Installed React peer | Files | Latest | Latest React peer | Verdict |
|---|---|---|---|---|---|---|
| `react-svg-inline` | 2.1.1 | `^0.14.9 \|\| ^15.3.0 \|\| ^16.0.0` | 107 | **2.1.1** | same | **Dead end.** 2.1.1 is the newest release, published 2022-06-26. 277 `SVGInline` call sites. No successor exists |
| `react-custom-scrollbars` | 4.2.1 | `^0.14 \|\| ^15 \|\| ^16` | 1 | **4.2.1** | same | **Dead end** upstream, but the community fork `react-custom-scrollbars-2@4.5.0` peers through `^18`. One import site, `SidebarWalletsMenu.tsx:6` |
| `react-virtualized` | 9.22.3 | `^15.3.0 \|\| ^16.0.0-alpha` | 5 | 9.22.6 | `^16.3 \|\| ^17 \|\| ^18 \|\| ^19` | Patch bump clears it |
| `recharts` | 1.8.5 | `^15 \|\| ^16` | 2 | 3.10.1 | `^16.8 \|\| ^17 \|\| ^18 \|\| ^19` | Two majors, breaking API |
| `react-table` | 7.7.0 | `^16.8.3 \|\| ^17.0.0-0` | 3 | 7.8.0 | `^16.8.3 \|\| ^17 \|\| ^18` | Patch bump clears it; still no React 19 |
| `react-copy-to-clipboard` | 5.0.2 | `^15.3.0 \|\| ^16` | 11 | 5.1.1 | `>=15.3.0` | Patch bump clears it |
| `react-lottie` | 1.2.3 | `^0.14.7 \|\| ^15 \|\| ^16` | 1 | 1.2.10 | `>=15.0.0` | Patch bump clears it |
| `react-datetime` | 3.0.4 | `^16.5.0` | 1 | 3.3.1 | `^16.5 \|\| ^17 \|\| ^18 \|\| ^19` | Minor bump clears it |
| `qrcode.react` | 1.0.0 | `^15.5.3 \|\| ^16` | 7 | 4.2.0 | `^16.8 \|\| ^17 \|\| ^18 \|\| ^19` | Three majors, API changed |
| `react-markdown` | 5.0.3 | `>=16` | 5 | 10.1.0 | `>=18` | Five majors. v6 dropped `allowDangerousHtml`, v7+ rewired plugins |
| `rc-slider` | 9.7.2 | `>=16.9.0` | 1 | 11.1.9 | `>=16.9.0` | No React cap at either end |
| `react-animate-height` | 2.0.23 | `>=15.6.2` | 1 | 3.2.4 | `>=16.8.0` | No React cap |
| `react-router` / `react-router-dom` | 5.2.0 | `>=15` | 14 | 8.3.1 / 7.18.3 | `>=19.2.7` / `>=18` | v5 has no React cap, so it does not block. Upgrading it is a rewrite and should not be attempted for React reasons |
| `react-syntax-highlighter` | 13.5.3 | `>= 0.14.0` | dev | 16.1.1 | `>= 0.14.0` | No cap |
| `@testing-library/react` | 12.1.2 | `*` | 27 | 16.3.3 | `^18 \|\| ^19` | Must move to 13+ for React 18. The v12 internals use `ReactDOM.render` |
| `@types/react` | 16.9.56 | n/a | all | 19.x | n/a | Must move with React. `@types/react-dom` is not currently installed at all |
| `react-refresh` | 0.11.0 | none | dev | 0.18.x | none | Fast Refresh needs a bump for React 18 |
| `prop-types` | 15.7.2 | n/a | 0 imports | 15.8.1 | n/a | Reached only transitively. The single textual hit is an eslint-disable comment at `ThemeManager.tsx:1` |

Three of these carry most of the cost. `react-svg-inline` has the widest reach and the least
recourse: 107 files, 277 call sites, a peer range capped at React 16, and 2.1.1 is both the
installed version and the newest ever published. It is a wrapper that inlines an SVG string into a
`span`, so a local replacement is small, but the import churn is 107 files. `react-markdown` and
`qrcode.react` each need a multi-major API migration. `recharts` 1 to 3 is a rewrite of two chart
components.

## 7. What the Storybook work gives up by staying on React 16

What continues to work on React 16:

- **Nothing in the Storybook 6 to 9 path requires React 18.** The `storiesOf` to CSF rewrite,
  the knobs to `args`/`argTypes` rewrite, the `@dump247/storybook-state` removal and the
  DaedalusMenu port are all React-version-independent work.
- **The DaedalusMenu addon runs on Storybook's React, not ours.** From Storybook's
  `MIGRATION.md`, "Manager addons are now rendered with React 18": the manager UI uses Storybook's
  own bundled React 18. So `storybook/addons/DaedalusMenu/register.tsx` renders against React 18
  regardless of what the app does. Its port is an API migration (`@storybook/addons` was split
  into `@storybook/preview-api` and `@storybook/manager-api` in SB7, then consolidated into
  `storybook/manager-api` and `storybook/preview-api` in SB9), not a React migration.
- **`@storybook/addon-docs` works.** Version 9.1.20 lists `react` and `react-dom` as its own
  dependencies rather than peers, so it renders docs with its own copy, and its `@types/react` peer
  accepts `^16.8.0`.
- **Play-function testing works.** `storybook/test` builds on `@testing-library/dom`, which
  operates at DOM level and is React-agnostic. `@storybook/addon-vitest@10.6.0` declares no React
  peer at all.
- **The SWC path survives.** `storybook-addon-swc@1.1.7` is abandoned (latest 1.2.0, last published
  2023-04-18) but `@storybook/addon-webpack5-compiler-swc@4.0.3` peers
  `storybook: "^9.0.0 || ^10.x"` and brings `@swc/core` plus `swc-loader`, so
  `jsc.transform.legacyDecorator: true` and `useDefineForClassFields: false` carry across. That
  matters because those two settings are load-bearing for MobX 5, and they are already spelled out
  with the same reasoning in `storybook/main.ts` and `jest.config.js`.
- **Node and TypeScript clear the bar.** Node is v22.23.1 against Storybook 9's `>=20`.
  TypeScript is 4.9.5 against the `typescript: ">= 4.9.x"` peer declared by both
  `@storybook/react@9.1.20` and `@storybook/react@10.6.0`, so it sits exactly at the floor for
  either. The first thing that forces a TypeScript bump is react-intl 7, which peers
  `typescript: "^5.6.0"`. Storybook 10 does add `@types/react` and `@types/react-dom` as peers, and
  `@types/react-dom` is not currently installed.

The two losses, neither on the critical path:

1. **Concurrent-rendering behavior is not exercised in Storybook.** Stories render through
   `ReactDOM.render`, so a component that misbehaves under concurrent rendering will not surface
   it in Storybook.
2. **`@storybook/addon-knobs` is a dead end regardless of React version.** Its newest release,
   8.0.1 (2024-06-19), peers `@storybook/*: "^8.0.0"`. There is no Storybook 9 build. So all 75
   knob-importing files must move to `args`/`argTypes` to reach Storybook 9, whatever React
   version we are on. That is 71 under `storybook/stories`, of which 64 are story files and the
   rest support modules, plus the 4 colocated story files under `source/`. That cost belongs to the Storybook plan, not to this question.

## 8. Sequencing

The dependency order that falls out of the evidence, with each step independently shippable:

1. **Storybook modernization, on React 16.14.0.** Unblocked today. Storybook 6.4.22 to 9.
   `storiesOf` to CSF across the 69 files that call it, knobs to controls across 75, remove
   `@dump247/storybook-state` from 10, port DaedalusMenu, swap `storybook-addon-swc` for
   `@storybook/addon-webpack5-compiler-swc`. `@storybook/manager-webpack5` disappears; the manager
   has been prebundled since Storybook 7. Keep `storybook:build` green in
   `perSystem/checks.nix:78` throughout.
2. **react-intl 2.9.0 to 6.8.9, still on React 16.** 323 files. Removes 154 `static contextTypes`
   declarations, replaces `intlShape` in 205 files, replaces `FormattedHTMLMessage` in 75, deletes
   `addLocaleData`. Set `textComponent="span"` on `IntlProvider` to hold layout. React 19 is gated
   on this step, and nothing gates this step.
3. **MobX 5 to 6 and mobx-react 6 to 9, still on React 16.** 36 constructors gain
   `makeObservable(this)`. `@observer` and `@inject` are untouched. Replace `mobx-react-router`
   (3 files) rather than upgrade it, since its current release forces React Router 5 to 6. Move
   `mobx-react-form` to the 6.19.x line.
4. **Replace the React-16-capped dependencies.** `react-svg-inline` (107 files, no successor),
   `react-custom-scrollbars` to `react-custom-scrollbars-2` (1 file), plus routine bumps for
   `react-virtualized`, `react-table`, `react-copy-to-clipboard`, `react-lottie`, `react-datetime`.
   Take `qrcode.react`, `react-markdown` and `recharts` as separate pieces of work.
5. **React 16.14.0 to 18.** `createRoot` at `source/renderer/app/index.tsx:64`,
   `@types/react` and `@types/react-dom` to 18, `@testing-library/react` to 13+ across 27 spec
   files, `react-refresh` bump, then hunt automatic-batching regressions by running the app.
6. **React 18 to 19**, only after step 2 has landed. Fix the two local function-component
   `defaultProps` sites, and fork or wrap react-polymorph's `PopOver` and `PasswordInput`.

Steps 2, 3 and 4 hold most of the React upgrade cost, and every one of them can start today
without changing the React version.

## What this does not capture

The counts here are static. Grep finds `static contextTypes` and `defaultProps`; it cannot find a
component that reads `this.context` without declaring `contextTypes`, or a runtime behavior that
only breaks under automatic batching. No part of this was validated by actually installing React 18
and running the app or the suite, so the estimates for steps 5 and 6 are lower bounds.

The `.tsx`-file counts treat one file as one component. Files that export several components are
counted once, so the true component totals are higher than 333 and 144.

Transitive React coupling was inspected for `react-polymorph` only. `@cardano-sdk/core`,
`@trezor/connect` and the hardware-wallet packages were not audited for bundled React
dependencies.

Storybook's React 16 support was verified by reading the published preset for 9.1.20 and the same
file on Storybook's `main`. It is not a documented guarantee, and Storybook could drop the
react-16 shim in a future major. A Storybook 11 upgrade should re-check the preset before
starting.

A separate divergence turned up while checking the SWC settings. `tsconfig.json` sets
`useDefineForClassFields: true`, while both `jest.config.js` and `storybook/main.ts` set the SWC
equivalent to `false`, each with a comment explaining that MobX 5 requires `false`. The type
checker and the two runtimes therefore disagree about class-field semantics. It does not affect
this analysis, and it will matter during the MobX 6 migration.

## Decisions for the project owner

1. Confirm that the Storybook modernization proceeds on React 16 and does not wait on the React
   upgrade.
2. Decide whether React 19 is a target at all, or whether React 18 is the destination. React 18
   costs roughly steps 3 through 5. React 19 additionally requires the full react-intl migration
   (step 2, 323 files) and a react-polymorph fork.
3. Decide the fate of `react-polymorph`. We hold read-only access to a repository with no commits
   since April 2022 and no published releases, and 196 files depend on it. The options are: stay on
   1.0.4 and cap at React 18; fork it; or replace it. A React 19 attempt cannot start until this
   is settled.
4. Decide how to handle `react-svg-inline`. 107 files, 277 call sites, no maintained successor, and
   a peer range that caps at React 16. A local replacement component is small; the churn is not.
5. Decide whether `mobx-react-router` (3 files) is replaced with a local router store rather than
   upgraded, since its only maintained release forces React Router 5 to 6.

## Sources

- React 19 upgrade guide, removed APIs: https://react.dev/blog/2024/04/25/react-19-upgrade-guide
- React 18 upgrade guide, deprecations and automatic batching: https://react.dev/blog/2022/03/08/react-18-upgrade-guide
- Storybook `react-dom-shim` preset, the React 16 aliasing logic: https://github.com/storybookjs/storybook/blob/main/code/lib/react-dom-shim/src/preset.ts
- The same preset as published, verified against the shipped artifact: https://unpkg.com/@storybook/react-dom-shim@9.1.20/dist/preset.js
- Storybook migration guide, `storiesOf` removal in 8.0 and manager addons on React 18: https://github.com/storybookjs/storybook/blob/v9.1.20/MIGRATION.md
- Storybook 8 migration overview: https://storybook.js.org/docs/8/migration-guide
- mobx-react compatibility matrix: https://github.com/mobxjs/mobx/blob/main/packages/mobx-react/README.md
- react-intl v2 to v3 upgrade guide: https://formatjs.io/docs/react-intl/upgrade-guide-3x/
- react-intl v3 to v4 upgrade guide, `FormattedHTMLMessage` removal: https://formatjs.io/docs/react-intl/upgrade-guide-4x/
- `mobx-undecorate` codemod: https://github.com/mobxjs/mobx/tree/main/packages/mobx-undecorate
- react-polymorph repository: https://github.com/input-output-hk/react-polymorph

Every peer-dependency range quoted above came from the npm registry
(`https://registry.npmjs.org/<package>`) or from the copy installed in `node_modules`, read on
2026-09-10.
