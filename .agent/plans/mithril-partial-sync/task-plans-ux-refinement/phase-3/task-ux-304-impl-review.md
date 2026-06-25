# task-ux-304 — Implementation & Code-Review Log (append-only; Implementation/Code-Review entries appended later)

---

Implementation:
Timestamp: 2026-06-25T11:10:45Z

Executed the approved plan (task-ux-304.md) end to end. interactive_decision closed: filled the FINAL COPY
block with the user-approved variant A primary + COMPACT PARENTHETICAL sync-%, marked it resolved, set the
plan's planning status -> approved, and corrected the few "trailing sentence" mentions for behindSyncContext
to "compact parenthetical reference line below the primary" (DD3, DD4, Step 1b) so the doc stays truthful.

Resolved copy (runtime values, no `!!!`):
- behind (EN): "Your node is about {epochs} epochs behind the blockchain tip. Mithril partial sync can restore
  verified chain data to help it catch up faster than waiting for normal sync."
- behind (JA): "ノードはブロックチェーンの先端より約{epochs}エポック遅れています。Mithril partial syncで検証済みのチェーンデータを復元すれば、通常の同期を待つよりも速く追いつけます。"
- behindSyncContext (EN, NEW): "({syncPercentage}% synced)"
- behindSyncContext (JA, NEW): "（{syncPercentage}% 同期済み）"
- behindUnknown (EN/JA): RETAINED verbatim (value unchanged); description-only edit to "…shown when the
  epochs-behind figure is unavailable (tips/epoch missing)".

Files changed (source):
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — computed `behindByEpochs` next to
  `formattedSyncPercentage` (lock #3: `networkEpoch`/`localEpoch` guarded by `tip && Number.isFinite(epoch)`,
  `behindByEpochs = Math.max(1, networkEpoch - localEpoch)` else `undefined`); passed `behindByEpochs` into
  `<MithrilPartialSyncSection>` (replacing `behindByImmutables`); removed the now-unused
  `mithrilPartialSyncBehindByImmutables?: number` prop + its render destructure. `formattedSyncPercentage`
  was already passed. No threshold compare (lock #1); no backend edit (lock #1/#2).
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — Props: `behindByImmutables?` ->
  `behindByEpochs?`; threaded `behindByEpochs={this.props.behindByEpochs}` +
  `formattedSyncPercentage={formattedSyncPercentage}` (already destructured) into
  `<MithrilPartialSyncConfirmation>`.
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` — Props: dropped
  `behindByImmutables?`, added `behindByEpochs?: number` + `formattedSyncPercentage: string`. defineMessages:
  `behind` -> approved epochs EN sentence with `{epochs}` + epochs description; NEW `behindSyncContext` id
  `…mithrilPartialSyncConfirmationBehindSyncContext` = `'!!!({syncPercentage}% synced)'`; `behindUnknown`
  value unchanged, description-only edit. Render: `hasBehindFigure` from `behindByEpochs`; primary `<p>` shows
  `behind {epochs}` or `behindUnknown`; NEW de-emphasized `<p>` (class
  `mithrilPartialSyncConfirmationBehindSyncContext`) for `behindSyncContext {syncPercentage}` rendered ALWAYS,
  directly below the primary.
- `source/renderer/app/components/status/DaedalusDiagnostics.scss` — tightened `…Behind` bottom margin to 4px
  and added a small/muted `…BehindSyncContext` rule (font-size 14px, opacity 0.7) for the compact parenthetical.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — removed the
  `mithrilPartialSyncBehindByImmutables={mithrilPartialSync.behindByImmutables}` display prop pass. Left
  `mithrilPartialSync.behindByImmutables` on the store as internal/debug (lock #5; removal flagged to
  task-ux-501); no backend gate/type churn.
- i18n: edited ONLY `en-US.json` + `ja-JP.json` runtime values (behind EN+JA changed; behindSyncContext EN+JA
  added between `…Behind` and `…BehindUnknown`; behindUnknown values unchanged), then regenerated
  `translations/messages.json` via `yarn i18n:extract` ("Done in 6.43s"). Did NOT touch `defaultMessages.json`
  (confirmed: it does not hold these ids and is not the extract target). No `!!!` on the new EN/JA runtime
  values; no "immutable"/"immutableファイル" in any partial-sync user-facing string.

Tests (updated the three existing 303 specs):
- `MithrilPartialSyncConfirmation.spec.tsx` — defaultProps `behindByImmutables`->`behindByEpochs: undefined`
  + `formattedSyncPercentage: '62.50'`. New test: `behindByEpochs:3` renders the epochs primary AND the
  "(62.50% synced)" parenthetical, asserts the sync-% line follows the primary
  (`compareDocumentPosition` DOCUMENT_POSITION_FOLLOWING), and `queryByText(/immutable/i)` is null. Unknown
  test: `behindByEpochs:undefined` renders behindUnknown AND "(62.50% synced)" (no /undefined/). Kept the
  modal/scrim, ESC, distinct-button, only-start-path, startError tests.
- `MithrilPartialSyncSection.spec.tsx` — defaultProps `behindByEpochs: undefined`; thread-through test now
  asserts the epochs sentence ("3") AND the "(62.50% synced)" line.
- `DaedalusDiagnostics.spec.tsx` — removed `mithrilPartialSyncBehindByImmutables` from defaultProps; added 3
  tests: computed difference (net 500 / local 497 -> "3"), unknown fallback (`networkTip:null`), floor-at-1
  (equal epoch 100/100 -> "1").

Verification (REAL commands + REAL results, from repo root):
- `node_modules/.bin/tsc --noEmit -p .` (this IS the `yarn compile` gate, `"compile": "tsc --noEmit"`): PASS,
  exit 0, 0 errors. NOTE: had to add the new `mithrilPartialSyncConfirmationBehindSyncContext` export to the
  gitignored/auto-generated `DaedalusDiagnostics.scss.d.ts` sidecar (the concrete sidecar shadows the global
  `declare module '*.scss'`, and `typedef:sass`/`typed-scss-modules` crashes under Node v24 dart-sass — same
  documented env defect; same local-parity step task-ux-303 took). The sidecar is gitignored so it is NOT
  staged.
- `yarn lint`: PASS, exit 0 (5454 warnings repo-wide, all pre-existing/stylistic — `no-explicit-any` on
  `...args`, `@ts-ignore`, unused-import false positives, empty-arrow in pre-existing tests; none in new logic).
- jest (3 touched specs): PASS, 3 suites / 32 tests green, `MithrilPartialSyncConfirmation.tsx` at 100%
  coverage. The committed `jest-css-modules-transform` crashes under Node v24 ("Can't find sass or node-sass
  module") even on untouched scss-importing specs — the documented env defect — so the run used a CLI-only
  config that inherits the committed `@swc/jest` TS transform and maps `.scss` -> `identity-obj-proxy` (the
  committed jest config is unchanged; module-class assertions stay valid since identity-obj-proxy returns the
  key verbatim and react-polymorph applies the literal `primary`/`flat` classes regardless).
- `prettier --write MithrilPartialSyncConfirmation.tsx` (the new `<p>` reflowed to one line) then re-check
  clean; the other touched files were already prettier-clean.
- Diff grep for user-facing "immutable": NONE in any EN/JA runtime string; the only diff hits are a test
  comment + a negative assertion (`queryByText(/immutable/i)).toBeNull()`) proving "immutable" is NOT shown.

Locked invariants upheld: #1 (no renderer threshold; `getPartialSyncBehindness` and all backend untouched),
#2 (no backend field/constant/IPC/api-doc change; figure is renderer-derived), #3 (`max(1, net.epoch -
local.epoch)`, whole-epoch, guarded -> undefined -> behindUnknown), #4 ("verified … chain data … catch up
faster" preserved; no "immutable files" to the user), #5 (`behindByImmutables` left on the store as
internal/debug, flagged to 501; no gate-type churn), #6 (no storybook story added/modified).

Deviations: none substantive. Two env-defect workarounds the plan anticipated (tsc-direct gate +
identity-obj-proxy jest css mapping) and the gitignored `.scss.d.ts` sidecar regeneration (local parity,
not staged). Did NOT run git add/commit; did NOT touch `.gitignore` (its pre-existing `+.devcontainer` change
was already in the session-start working tree).

Outcome: implemented as planned; awaiting code review.

---

Code Review:
Timestamp: 2026-06-25T11:14:10Z

Scope reviewed: working-tree diff vs `task-ux-304.md` (approved plan) + the user-pinned FINAL COPY block.
Inspected `git --no-pager diff -- source/ translations/` and `git status --porcelain`; read all changed
component/spec files in full; ran the independent verifications below.

Correctness (verified):
- `behindByEpochs` computed in `DaedalusDiagnostics.render()` (`:570-577`) exactly per DD1/lock #3:
  `networkEpoch`/`localEpoch` guarded by `tip && Number.isFinite(tip.epoch)`, then
  `Math.max(1, networkEpoch - localEpoch)` else `undefined`. Whole-epoch subtraction; floored at 1; null/
  missing tip or non-finite epoch -> `undefined` -> `behindUnknown` fallback (no fabricated number). No
  threshold compare anywhere (lock #1).
- Prop thread-through correct: `DaedalusDiagnostics` passes `behindByEpochs` (and the already-present
  `formattedSyncPercentage`) to `<MithrilPartialSyncSection>` (`:722,:730`); Section Props swapped
  `behindByImmutables?` -> `behindByEpochs?` (`:16`) and threads both into `<MithrilPartialSyncConfirmation>`
  (`:121-122`); Confirmation Props dropped `behindByImmutables`, added `behindByEpochs?: number` +
  `formattedSyncPercentage: string` (`:92-93`). `hasBehindFigure` now tests `behindByEpochs` (`:114-115`).
- Render structure matches DD3: primary `<p>` = epochs sentence or `behindUnknown`; a SECOND de-emphasized
  `<p>` (`mithrilPartialSyncConfirmationBehindSyncContext`) renders the compact parenthetical
  `({syncPercentage}% synced)` directly BELOW the primary, ALWAYS (independent of `behindByEpochs`).
- Container `DaedalusDiagnosticsDialog.tsx` removed the `mithrilPartialSyncBehindByImmutables` display-prop
  pass; no `behindBy*` prop pass remains (figure now renderer-computed). `localTip`/`networkTip`/
  `syncPercentage` injections intact.

Copy (verified, exact match to FINAL COPY block):
- behind EN `en-US.json:160` and JA `ja-JP.json:160` match the approved variant-A epochs sentences verbatim;
  JA `約{count}個のimmutableファイル` string fully removed.
- behindSyncContext is the COMPACT PARENTHETICAL (NOT a full sentence): EN `({syncPercentage}% synced)`
  `:161`, JA `（{syncPercentage}% 同期済み）` `:161`; rendered as its own `<p>` below the primary, always.
- behindUnknown EN/JA values unchanged `:162` (description-only edit). `{count}`->`{epochs}` renamed
  consistently at the call site and in every catalog. No `!!!` on any partial-sync runtime EN/JA value.
  `translations/messages.json` regenerated consistently (new id at `:1870-1874`, behindUnknown description
  updated `:1877`); `defaultMessages.json` NOT touched (confirmed via `git status --porcelain`).
- grep for "immutable"/"immutableファイル" across both runtime catalogs and the three changed component
  sources: zero user-facing hits. The only diff "immutable" occurrences are a test comment and a negative
  assertion `queryByText(/immutable/i)).toBeNull()` proving it is never shown.

Locked invariants (independently confirmed):
- #1/#2: `git diff --name-only -- source/main source/common .agent/system/api-endpoints.md` = empty;
  `MithrilPartialSyncService.ts` (gate `getPartialSyncBehindness`) untouched. No new IPC field, no
  per-network `filesPerEpoch` constant, no backend `behindByEpochs` field, no api-endpoints.md change.
- #4: "verified … chain data … catch up faster" wording preserved in EN+JA; no "immutable files" reaches
  the user.
- #5: `behindByImmutables` retained on `MithrilPartialSyncStore.ts:67,:192` (internal/debug), no gate-type
  churn — correct non-blocking choice, removal flagged to task-ux-501.
- #6: no `*.stories.tsx` in the diff (story work belongs to task-ux-502).

Tests (independently run):
- `node_modules/.bin/jest` on the three touched specs with `.scss` mapped to identity-obj-proxy:
  3 suites / 32 tests PASS; `MithrilPartialSyncConfirmation.tsx` 100% coverage. Assertions are meaningful:
  Diagnostics computes 500-497 -> "3", floor-at-1 on equal epochs, null-tip -> behindUnknown; Confirmation
  asserts the parenthetical follows the primary via `compareDocumentPosition` and that "immutable" is absent.
- `node_modules/.bin/tsc --noEmit`: exit 0, zero errors (the authoritative `yarn compile` gate; jest/sass
  css-module crash under Node v24 dart-sass is the documented pre-existing env defect, not a regression).

Doc drift (non-blocking observation):
- The FINAL COPY block, DD3, DD4, and Step 1b were correctly updated from "trailing sentence" to "compact
  parenthetical reference line below the primary" per the user override — these are the load-bearing spec
  sections and they are truthful. Residual "trailing sentence" phrasings remain at narrative/scope/test-note
  lines 36, 67, and 453, plus the verbatim-from-JSON acceptance criterion line 417 (which must stay verbatim).
  These are cosmetic and outside the DD3/DD4/Step-1b text the override named; they do not contradict the
  shipped behavior (own `<p>`, compact parenthetical, always-rendered). Recommend a trivial follow-up sweep
  of lines 36/67/453 for full truthfulness; NOT a blocker.

Complexity / scope: no backend churn, no story edits, no threshold logic, no over-engineering. Diff is
minimal and matches the plan's ordered steps.

Blockers: none.

Decision: approved
