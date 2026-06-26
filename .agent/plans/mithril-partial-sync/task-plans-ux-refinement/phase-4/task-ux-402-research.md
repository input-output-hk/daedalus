# task-ux-402 — Research note

Durable findings (decisions, gotchas, evidence, residual gaps) recorded at completion
(code review approved 2026-06-26T11:20:22Z).

## Anchor reconciliations (every TASK_CONTEXT/PRD anchor drifted; components grew during task-ux-401)

Use the LIVE anchors below for any downstream task touching these shared components.

- `.activeCircle` active branch in `TopLevelIcon`: JSON `386-388` → **LIVE `MithrilStepIndicator.tsx:403-405`** (post-change spinner is L404-409).
- spec lock (`.activeCircle` not-null / `.iconSpinner` null): JSON `59-60` → **LIVE `MithrilStepIndicator.spec.tsx:61-62`** (test 38-64).
- bootstrap ticking effect: JSON `97-109` → **LIVE `MithrilProgressView.tsx:93-111`** (the proven `setInterval` path; deps `[bootstrapStartedAt, elapsedSecondsProp, status]`).
- `isStartingNode` reassurance block: JSON `142-163` → **LIVE `MithrilProgressView.tsx:145-166`** (`completionBlock`/`completionTitle`/`completionDetail`/`completionSpinner`).
- frozen `elapsedSeconds` overlay pass-through: `MithrilPartialSyncOverlay.tsx:93` → **CONFIRMED exact**.
- store mutation point: `MithrilPartialSyncStore._updateStatus` → **LIVE L146-166**; the `startedAt` anchor block landed at **L153-177**.
- bootstrap-proven anchor logic copied from: `MithrilBootstrapStore._updateStatus` **L128-137** (stamp on first working frame honoring backend elapsed; release on terminal→fresh-start / decision-cycle).

## Material scope reconciliation — the expected `MithrilStepIndicator.scss` edit was NOT needed

The tasks JSON listed `MithrilStepIndicator.scss` as a target path, implying a scss edit to animate the
active step. **It needed no edit:** `.iconSpinner` already existed (scss L71-77, identical structure to
the working `.subItemIconSpinner` L215-225; `loading-spin` is the global keyframe from
`@import '…/mixins/loading-spinner'`), AND the reduced-motion guard already names it
(`@media (prefers-reduced-motion: reduce) { .iconSpinner, .subItemIconSpinner { animation: none } }`,
scss L306-311). So the rotating active step is a **pure TSX swap** (static `.activeCircle` div →
`<SVGInline svg={spinnerIcon} className={classNames(styles.icon, styles.iconSpinner)} />`), and
reduced-motion is honored for free with no new animation class. The only scss edit was a new
`.reassurance` rule in `MithrilProgressView.scss` (not in the JSON list; the reassurance `<p>` lives in
that component).

## Renderer-timer-vs-frozen-elapsed decision (the core honesty fix)

The old partial-sync elapsed was a **backend-snapshot-stamped** number (`transferProgress.elapsedSeconds`)
passed straight through the overlay — it froze between status events (reporter saw it stick at 6:46) and
read 0:00 during the node stop. Replaced with a **renderer clock anchored to a store timestamp**, reusing
the bootstrap-proven seam end-to-end, with NO new IPC and NO synthetic figure (locked #8/#18):

- `MithrilPartialSyncStore.startedAt` (`@observable number | null`) is anchored in `_updateStatus`,
  mirroring `MithrilBootstrapStore.bootstrapStartedAt`: reset on non-working→working, stamp on the first
  working frame as `Date.now() - backendElapsed*1000` when a finite positive backend `elapsedSeconds`
  exists (so a re-attach to an in-flight op shows true elapsed) else `Date.now()`, and release only on
  `idle`. Kept non-null through terminal so the final overlay freezes its last value.
- Threaded store → `App.tsx` (`startedAt={mithrilPartialSync.startedAt}`, reactive via `@observer`; it
  only changes on status transitions so the per-second tick stays local to `MithrilProgressView` — no
  app-wide re-render each second) → overlay `startedAt` prop → `MithrilProgressView bootstrapStartedAt`.
- Dropped the frozen `elapsedSeconds={transferProgress?.elapsedSeconds}` overlay pass-through and the dead
  `App.tsx` `elapsedSeconds:` literal. The `elapsedSeconds` prop on `MithrilProgressView` and the
  `transferProgress.elapsedSeconds` contract field are KEPT (harmless) to avoid storybook/contract ripple
  — removing them is out of scope (storybook = task-ux-502).
- Elapsed is `Math.floor((Date.now() - startedAt)/1000)` computed in the renderer — advances from 0:00
  immediately and never freezes between events.

### `completed` added to TERMINAL_STATUSES — NOT inert for bootstrap; explicitly accepted (plan-review BLOCKER 2)

This was the load-bearing #11 finding. `completed` IS a `MithrilBootstrapStatus` and a `WORKING_STATUS`
(`MithrilBootstrap.tsx:67-76`), and bootstrap renders this SHARED `MithrilProgressView` for it with a
non-null `bootstrapStartedAt` (`MithrilBootstrap.tsx:190-204`; `MithrilBootstrapStore.ts:119-148` keeps
the anchor through `completed`, clearing only on decision-cycle / `preparing`-after-terminal). So **today
the elapsed clock ticks on the bootstrap completed frame; after this change it FREEZES** at its last
value. Accepted as correct, not a regression: a success/handoff frame must not climb an elapsed counter,
and `completed` is transient — `starting-node` (NOT terminal, same un-cleared anchor) immediately resumes
the live tick from the same `bootstrapStartedAt`. Proven by the new `MithrilProgressView.spec.tsx`
completed-freeze test (renders the exact shared component with `status:'completed'` + `bootstrapStartedAt`).
The plan's original "inert for bootstrap" claim was factually wrong and was corrected before approval.

## Reduced-motion approach

No JS toggle and no new class. The active top-level step reuses `styles.iconSpinner`, which the
pre-existing `@media (prefers-reduced-motion: reduce)` block (`MithrilStepIndicator.scss:306-311`) already
sets to `animation: none`. jsdom does not evaluate `@media`, so this is a code-review check, not a unit
assertion. KNOWN GAP (pre-existing, out of scope): the big start/stop block `completionSpinner` (its local
`spin` keyframe in `MithrilProgressView.scss`) is NOT reduced-motion-guarded — flagged for task-ux-601/502.
The task's reduced-motion requirement is specifically the top-level STEP spinner, which IS guarded.

## i18n keys added (5; first-class EN+JA, no `!!!` in runtime catalogs)

Defined in `MithrilBootstrap.messages.ts`; EN in `en-US.json`, JA in `ja-JP.json`; regenerated into
`defaultMessages.json` + `translations/messages.json`.

- `loading.mithrilBootstrap.progress.longPhaseReassurance` — EN "This can take several minutes — Daedalus is still working." / JA "数分かかることがあります。Daedalusは引き続き処理しています。"
- `loading.mithrilBootstrap.progress.nodeStoppingTitle` — EN "Stopping Cardano node..." / JA "Cardanoノードを停止しています..."
- `loading.mithrilBootstrap.progress.nodeStoppingDetail` — EN "Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes." / JA "Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。"
- `loading.mithrilPartialSync.progress.nodeStoppingTitle` — EN "Stopping Cardano node..." / JA "Cardanoノードを停止しています..."
- `loading.mithrilPartialSync.progress.nodeStoppingDetail` — EN "Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes." / JA "Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。"

Live i18n mechanism (re-confirmed): `defineMessages` (`!!!` defaults) → `yarn i18n:extract` writes ONLY
`translations/messages.json` → hand-edit key-sorted `en-US.json`/`ja-JP.json` → `yarn i18n:check`
(react-intl-translations-manager) regenerates `defaultMessages.json`. The bootstrap-scoped `nodeStopping*`
are the `MithrilProgressView` `|| intl.formatMessage(...)` fallbacks (mirroring `nodeStarting*`); the
partial-sync-scoped ones are the overlay overrides actually used today.

## Spec deviation (forced, justified — see impl-review)

The plan pinned `querySelector('.iconSpinner')` for the active-spinner assertions, but `svg-jest` /
`react-svg-inline` renders the `.svg` import as a bare `<svg></svg>` with NO className (true of the proven
sub-item spinner too — which is exactly why the old green test could only ever select the static
`.activeCircle` div and assert `.iconSpinner` null). The assertions were made truthful via observable DOM
instead: `.activeCircle` is gone and the active step's `.iconContainer` renders an `<svg>`; the
stopping-node test asserts `stepActive` + no `.pendingCircle`. The component change matches the plan
verbatim (`SVGInline` + `styles.iconSpinner`).

## Env workarounds (still active, repo-wide, NOT task regressions — carried from 301/303/304/401)

- Node v24 dart-sass breaks `yarn compile`'s `typedef:sass`/`precompile` glob → use
  `node_modules/.bin/tsc --noEmit -p .` as the authoritative TS gate (allow up to 600s). This task also
  hit a stale gitignored `MithrilProgressView.scss.d.ts` (missing the new `reassurance` key) → regenerate
  the single file with `typed-scss-modules <file>` (single-file invocation works; the full glob crashes),
  then tsc passes clean.
- jest css-modules transform can crash under the same defect → map `\.(scss|sass|css)$` to
  `identity-obj-proxy` via a CLI-only `--config`/`--moduleNameMapper` sidecar; DO NOT stage it. (The
  reviewer's run did not need it on the committed config; it remains a safe fallback.)

## Residual gaps / follow-ups

- task-ux-203 owns an optional backend emit of `stopping-node`; it stays renderer-optimistic
  (`START_PENDING_STATUS = 'stopping-node'`) here.
- task-ux-404 owns disabling Cancel during `stopping-node` and always-resync-after-cancel; this task left
  `disabled={isStartingNode}` / `hideAction` / `onCancel` wiring unchanged.
- task-ux-501 may prune the two likely-dead bootstrap-scoped `nodeStopping*` defaults.
- task-ux-601/502 own the holistic JA pass and the unguarded `completionSpinner` reduced-motion gap.
