# task-ux-702a — Implementation Review Log (append-only)

> Append-only chronological transcript of the per-category implementation +
> code-review passes. Each entry carries a speaker label (Implementation |
> Code-Review), an ISO-8601 UTC `Timestamp:`, the category, a summary, the files
> touched, and the verification result. Append at end-of-file only; never
> rewrite history.

---

## Implementation

Timestamp: 2026-06-29T17:49:33Z
Speaker: Implementation
Category: CAT-A (Diagnostics availability & behind-ness-known timing — ISSUE-1 + ISSUE-2)

Summary:
Executed the CAT-A ordered steps on the shared working tree, renderer-only, no backend edits.

1. `MithrilPartialSyncStore.ts`: added the `_isRefreshingAvailability = false;` re-entrancy guard
   field (after `_isTornDown`); in `setup()` replaced the interval body so `_refreshAvailability()`
   now fires on **every** 30s tick (dropped the `if (this.isWorking)` gate) so the Diagnostics
   Mithril section self-corrects on first load without a reload (ISSUE-1); `AVAILABILITY_REFRESH_INTERVAL`
   left at `30_000`. In `_refreshAvailability` the early-return now also short-circuits on the guard,
   sets the guard `true` before the request, and clears it in a `finally` (prevents probe pile-up).
   `_applyAvailability` unchanged.
2. New `utils/mithrilBehindness.ts` exporting `isMithrilBehindnessKnown(localTip, networkTip)` =
   both tips present with finite epochs (`Number.isFinite(localTip?.epoch) && Number.isFinite(networkTip?.epoch)`).
   This is an availability signal only, NOT a renderer threshold (backend `isSignificantlyBehind`
   stays the sole offer signal — locked #4); surfaces no %/immutable values (D13 epochs-only).
3. `NetworkStatusStore.ts`: imported the helper and added `@computed get isBehindnessKnown(): boolean`
   (placed after `isEpochsInfoAvailable`, before `isVerifyingBlockchain`) delegating to the helper over
   the tracked `localTip`/`networkTip` observables, so consumers re-render the moment the network tip
   arrives. This is the load-bearing signal CAT-F AND-s into its `showMithrilPrompt` gate (do not
   re-derive a local `behindByEpochs !== undefined` — see the CAT-F contract).
4. Specs: rewrote the `MithrilPartialSyncStore.spec.ts` interval test (was "only while ... work is in
   flight") to assert refresh-while-idle — 1 call after setup, advance 30s → 2, advance 30s → 3 (each
   awaits the prior probe so the guard clears). New `mithrilBehindness.spec.ts` covers missing
   networkTip → false, missing localTip → false, non-finite/NaN/Infinity epoch → false, both finite →
   true (incl. equal epochs → true).
5. Flagged (no edit) the backend `MithrilController.getPartialSyncAvailability:167-174` coupling
   (static `isEnabled` gated behind the slow behind-ness probe) in `task-ux-702a-research.md` as an
   OPTIONAL backend-track hardening, out of CAT-A scope.

Locked-invariant adherence: honors #4 (no renderer threshold; read-only IPC; no auto-trigger),
D13/epochs-only + vocabulary (no user copy added; no %/immutable surfaced), #10 (kill switch keeps the
unconditional poll cheap), #11 (additive edits; bootstrap untouched). Amends no locked decision. No
i18n change (CAT-A adds no user copy).

Files:
- source/renderer/app/stores/MithrilPartialSyncStore.ts
- source/renderer/app/stores/MithrilPartialSyncStore.spec.ts
- source/renderer/app/utils/mithrilBehindness.ts (new)
- source/renderer/app/utils/mithrilBehindness.spec.ts (new)
- source/renderer/app/stores/NetworkStatusStore.ts
- .agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-702a-research.md (new; step-9 flag)

Verification:
- `yarn test:jest` on `mithrilBehindness.spec.ts` + `MithrilPartialSyncStore.spec.ts`: 2 suites / 29
  tests PASS; `mithrilBehindness.ts` at 100% coverage.
- `yarn compile` (tsc --noEmit, precompile regenerated `.scss.d.ts`): clean, no errors.
- `yarn eslint` on the 5 touched files: 0 errors. The only warnings are pre-existing
  (`NetworkStatusStore.ts` `@ts-ignore`/`no-explicit-any`; `MithrilPartialSyncStore.ts` line-1
  `action/computed/observable` "unused" decorator-import false positives — confirmed present on the
  HEAD-committed file via a temporary stash).
- Prettier: the 4 new/store files are clean under the project config; `NetworkStatusStore.ts` is
  flagged by the pinned prettier 2.1.2, but the same file fails identically at HEAD (verified by
  stashing my edit) — a pre-existing env mismatch (committed code formatted with newer prettier than
  the pinned 2.1.2), on pre-existing lines (148/518/551/715) I did not touch; my added import (~L15)
  and computed (~L872) are not in the prettier diff. Not a CAT-A regression.

---

## Code Review

Timestamp: 2026-06-29T17:54:49Z
Speaker: Code Review
Category: CAT-A (Diagnostics availability & behind-ness-known timing — ISSUE-1 + ISSUE-2)

Decision: APPROVED — no blockers.

Scope reviewed: working-tree diff for the five CAT-A files
(`MithrilPartialSyncStore.ts`/`.spec.ts`, `NetworkStatusStore.ts`,
`utils/mithrilBehindness.ts`/`.spec.ts`) plus the `.gitignore` line and the step-9
research flag. One broad adversarial pass.

Acceptance criteria — all met:
- Self-correct ≤30s without reload: `setup()` interval body now calls
  `_refreshAvailability()` on EVERY tick (the `if (this.isWorking)` gate is gone);
  `AVAILABILITY_REFRESH_INTERVAL` stays `30_000`. The new `_isRefreshingAvailability`
  re-entrancy guard (set before the request, cleared in `finally`) prevents probe
  pile-up. Verified.
- `isBehindnessKnown` true only once both tips carry finite epochs and re-renders
  consumers: `@computed get isBehindnessKnown` placed after `isEpochsInfoAvailable`
  / before `isVerifyingBlockchain`, delegates to the helper over the tracked
  `localTip`/`networkTip` observables. Helper = `Number.isFinite(localTip?.epoch) &&
  Number.isFinite(networkTip?.epoch)` — a boolean availability check, NOT a threshold.
- Tests: `yarn test:jest` on both spec files = 2 suites / 29 tests PASS;
  `mithrilBehindness.ts` 100% covered. Rewritten store spec asserts idle-refresh
  1→2→3 across ticks, awaiting each prior probe so the guard clears (timing verified
  sound — `setupStore()` only constructs, single explicit `setup()` → one interval).
  Helper spec covers all 4 plan cases + NaN/Infinity/equal-epochs/0-0 extras.
- `yarn compile` (tsc --noEmit, precompile regen of `.scss.d.ts`): clean.
- `yarn eslint` on the 5 files: 0 errors (48 warnings, all pre-existing
  `@ts-ignore`/`no-explicit-any` on untouched NetworkStatusStore lines).
- No backend file modified (git status: only the 5 renderer files + `.gitignore`);
  the optional `MithrilController.getPartialSyncAvailability:167-174` decoupling is
  flagged in `task-ux-702a-research.md` (NOT implemented). Confirmed.

Locked invariants honored:
- CAT-A no renderer threshold (#4): availability boolean only; backend
  `isSignificantlyBehind` untouched; read-only IPC, no auto-trigger. Doc comments
  state this explicitly.
- #11 bootstrap not regressed: additive edits only (one new field, interval-body
  swap, guard, one helper module, one computed); no bootstrap component touched.
- #10 kill-switch: unconditional poll stays cheap; UI gating untouched.
- D13/epochs-only + vocabulary: no user copy added; no %/immutable surfaced;
  "PartialSync" appears only in code identifiers (allowed).
- Amends no locked decision. No i18n change required or made (EN/JA N/A for CAT-A).
- No duplicate i18n id introduced (the shared D-702a-6 key is CAT-C/CAT-F's concern).

Prettier re-verification (independent): the 4 new/store files are prettier-clean;
`NetworkStatusStore.ts` is flagged by the pinned prettier 2.1.2 ONLY on pre-existing
untouched lines (148/518/551/715). Re-confirmed pre-existing by stashing the CAT-A
change and running prettier --check in-place at HEAD — it still fails identically.
Documented env mismatch (committed code formatted with newer prettier), not a CAT-A
regression. The added import/computed are absent from the prettier diff.

Notes (non-blocking, not CAT-A scope):
- `isBehindnessKnown` + `isMithrilBehindnessKnown` are currently unconsumed in the
  working tree; their sole consumer is CAT-F (a later category in the
  CAT-A→…→CAT-F sequence). Expected per the step-8 contract. Cross-category flag:
  CAT-F MUST AND `networkStatus.isBehindnessKnown` into `showMithrilPrompt` (NOT a
  local `behindByEpochs !== undefined` re-derivation) so this deliverable does not
  become orphaned dead code. Carry into CAT-F review.
- `.gitignore` gains `.devcontainer`; outside CAT-A's exact-files list but was
  already `M` in the working tree at task start (pre-existing env change). Harmless.
- Pre-existing behavior unchanged by CAT-A: an in-flight `_applyAvailability` can
  still resolve after teardown (the guard is orthogonal to `_isTornDown`); the
  teardown spec confirms no NEW ticks fire post-teardown. Not introduced here.

Blockers: none.

---

## Implementation

Timestamp: 2026-06-29T18:01:24Z
Speaker: Implementation
Category: CAT-C (Confirmation body simplify — ISSUE-4, D-702a-4)

Summary:
Executed the CAT-C ordered steps on the shared working tree, renderer + i18n only, no backend/scss edits.

1. Created the shared D-702a-6 module
   `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts`: one `defineMessages`
   with key `processSummary`, id **`daedalus.diagnostics.dialog.mithrilSyncProcessSummary`** (the single
   shared id CAT-F MUST import — do not redeclare), defaultMessage (prefixed `!!!`) = the canonical
   shutdown/restore/restart sentence, description noting it is shared by the confirmation modal and the
   proactive prompt. Follows the `ChainStorage.messages.ts` shared-module precedent
   (`Record<string, ReactIntlMessage>` + `export default messages`).
2. `MithrilPartialSyncConfirmation.tsx`: imported the shared module; KEPT `title`/`cancel`/`confirm`;
   DELETED `intro`, `success`, `stepStop`, `stepDownload`, `stepRestart`, `recovery`; reworded `behind`
   to `!!!Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to
   help your node sync faster.` and `behindUnknown` to `!!!Your node is behind the latest verified
   snapshot. Mithril Sync will restore verified chain data to help your node sync faster.`
3. Rewrote the render body to exactly two `<p>` + the error: subject paragraph (class
   `styles.mithrilPartialSyncConfirmationBehind`, behind/behindUnknown via the existing `hasBehindFigure`
   guard, `{epochs}` kept) and subtext paragraph (class **`styles.mithrilPartialSyncConfirmationRecovery`**
   — PINNED, not renamed, no new `...Subtext` class) rendering
   `mithrilSyncProcessSummaryMessages.processSummary`; then the unchanged `{startError ? <div
   className={styles.error}>}`. Removed the `<ol>` steps block. No `.scss` edit (CAT-D restyles the pinned
   recovery rule in place); reused only classes already present in `DaedalusDiagnostics.scss.d.ts`.
4. i18n: ran `yarn i18n:extract` (regenerated `translations/messages.json`) then `yarn i18n:check`
   (regenerated `defaultMessages.json`) — both clean minimal diffs: drop the 6 removed ids, update
   behind/behindUnknown, add the shared `mithrilSyncProcessSummary` descriptor.
5. Hand-finalized `en-US.json` + `ja-JP.json`: removed the 6 obsolete
   `...Confirmation{Intro,Recovery,StepDownload,StepRestart,StepStop,Success}` keys; set the reworded EN/JA
   `...ConfirmationBehind` / `...ConfirmationBehindUnknown`; ADDED
   `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` (EN canonical sentence; JA provisional per the
   601 glossary: Mithril同期 / 検証済みのチェーンデータ / Cardanoノード / 再起動 / シャットダウン). Verified
   no `!!!` leaked into the EN/JA catalogs and the 6 removed ids are absent from all four catalogs.
6. Spec `MithrilPartialSyncConfirmation.spec.tsx`: updated the behind test to the new combined string
   (3 epochs) keeping the no-`/% synced/` + no-`/immutable/` negatives; updated the behindUnknown test to
   the new fallback string; replaced the steps-wording test with one asserting the canonical subtext
   renders AND removed copy is absent (`queryByText(/Daedalus stops Cardano node/)` null,
   `queryByText(/downloads and verifies Mithril data/)` null). Modal/ESC/buttons/error/blocked tests
   unchanged.

Shared i18n key created (CAT-F imports this exact id): **daedalus.diagnostics.dialog.mithrilSyncProcessSummary**
(exported as `processSummary` from `MithrilSyncProcessSummary.messages.ts`). CAT-F drops its private
`mithrilProactivePromptConfirmBody` and consumes this module instead.

Locked-invariant adherence: honors #3 (confirm button is still the only start path), #4 (keeps backend
`behindByEpochs` + `hasBehindFigure`; no renderer threshold), D13/epochs-only (keeps `{epochs}` and the
`behindUnknown` fallback; no %/immutable), #11 (confirmation is partial-sync-only; the shared key is reused
by the prompt, not the bootstrap). Deliberate sanctioned amendment recorded: the re-touch of
`mithrilPartialSyncConfirmationBehind` (previously locked by 304/601, `tasks.json:617`) per D-702a-4 — NOT
a 304/601 regression. Locked vocabulary preserved ("Mithril Sync" only; no "partial sync" in user copy).

Files:
- source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts (new shared module)
- source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx
- source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- source/renderer/app/i18n/locales/defaultMessages.json (regenerated by i18n:check)
- translations/messages.json (regenerated by i18n:extract)

Verification:
- `yarn test:jest source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`:
  1 suite / 9 tests PASS; `MithrilPartialSyncConfirmation.tsx` + `MithrilSyncProcessSummary.messages.ts`
  both 100% coverage.
- `yarn compile` (tsc --noEmit, precompile regenerated `.scss.d.ts` via typedef:sass): clean, exit 0.
- `yarn eslint` on the new module + confirmation `.tsx`/`.spec.tsx`: 0 errors/0 warnings.
- Prettier: the 3 touched source files + all 4 i18n catalogs pass `prettier --check` under the pinned
  2.1.2 (the project-wide `yarn prettier:check` still reports pre-existing `tests/**` warnings unrelated to
  CAT-C). i18n catalog diffs verified minimal (only the CAT-C ids; no churn).

---

## Code Review

Timestamp: 2026-06-29T18:07:04Z
Speaker: Code Review
Category: CAT-C (Confirmation body simplify — ISSUE-4, D-702a-4)

Decision: APPROVED — no blockers.

Scope reviewed: the CAT-C working-tree diff — `MithrilSyncProcessSummary.messages.ts` (new),
`MithrilPartialSyncConfirmation.tsx`/`.spec.tsx`, and the four i18n catalogs
(`en-US.json`, `ja-JP.json`, `defaultMessages.json`, `translations/messages.json`). The
CAT-A renderer/store/util files in the same tree are out of CAT-C scope (already reviewed
above). One broad adversarial pass.

Acceptance criteria — all met:
- Body renders EXACTLY two `<p>` + the error: subject paragraph (class
  `mithrilPartialSyncConfirmationBehind`, behind/behindUnknown via the retained `hasBehindFigure`
  guard, `{epochs}` kept) and subtext paragraph (class `mithrilPartialSyncConfirmationRecovery`,
  rendering the shared `processSummary`), then `{startError ? <div className={styles.error}>}`.
  The old `<ol>` steps block is gone; the 6 removed copy blocks
  (Intro/Success/StepStop/StepDownload/StepRestart/Recovery) are absent from defineMessages and
  from all four catalogs (verified by grep — the only `...Recovery` residue is the CSS class name,
  which is correct per the pinned-class plan).
- Shared key exists exactly once: `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` is declared
  only in `MithrilSyncProcessSummary.messages.ts` and imported (not redeclared) by the confirmation;
  `i18n:check` reports "No duplicate ids found". Consumed by the confirmation now; the module carries a
  "do NOT redeclare — import this module" comment for CAT-F.
- Spec asserts the new strings + negatives: combined behind string (3 epochs) with no `/% synced/` and
  no `/immutable/`; new behindUnknown fallback; canonical subtext present AND removed copy absent
  (`queryByText(/Daedalus stops Cardano node/)` null, `/downloads and verifies Mithril data/` null).
  9 tests pass, meaningful.
- `i18n:check` exit 0, no duplicate ids, and no mithril key in the untranslated (`!!!`) report — i.e.
  EN and JA both carry real values for the three changed/added keys.
- `yarn compile` clean (the shared module's `Record<string, ReactIntlMessage>` + `intl.formatMessage`
  consumption typechecks; pattern matches the `ChainStorage.messages.ts` precedent). `yarn lint` on the
  three source files clean.

Locked invariants honored:
- #3 confirm-only start path: actions array still cancel + primary confirm; no second start path.
- #4 no renderer threshold: keeps backend `behindByEpochs` + the `hasBehindFigure` guard; computes no
  significance.
- D13 epochs-only: `{epochs}` figure retained, `behindUnknown` fallback retained; no %/immutable surfaced.
- Subtext class PINNED to `mithrilPartialSyncConfirmationRecovery` (present in `DaedalusDiagnostics.scss.d.ts`);
  not renamed, no `...Subtext` class introduced, no `.scss` edit — leaves CAT-D a clean in-place restyle target.
- #11 bootstrap not regressed: CAT-C touches only the partial-sync confirmation modal + the shared messages
  module + i18n; no bootstrap component/scss touched (git name-only confirms). The shared key is reused by
  the prompt, never the bootstrap.
- Sanctioned amendment: the re-touch of the 304/601-locked `mithrilPartialSyncConfirmationBehind` is
  authoritatively recorded in D-702a-4 (plan + decisions doc) and the Implementation entry above — NOT a
  304/601 regression.

Vocabulary: clean — "Mithril Sync" capitalized throughout; "Cardano node"; no "partial sync" in user copy;
no %/immutable. JA mirrors the 601 glossary (Mithril同期 / 検証済みのチェーンデータ / Cardanoノード /
再起動 / シャットダウン).

EN + JA: both updated for all three changed/added keys and the six removals; `defaultMessages.json` and
`translations/messages.json` regenerated consistently (descriptor for the new shared module placed under its
own `MithrilSyncProcessSummary.messages.ts` path). Key parity holds (1498 == 1498; no mithril key missing in
either locale).

Notes (non-blocking; cross-category / process):
- CAT-F handoff (later category): the canonical-sentence i18n key is consolidated here; CAT-F MUST import
  `MithrilSyncProcessSummary.messages.ts` and drop its private `mithrilProactivePromptConfirmBody` (still
  present and correctly untouched by CAT-C). Carry into CAT-F review.
- The unused-after-this-change scss class `mithrilPartialSyncConfirmationSteps` (the removed `<ol>`) is left
  in `.scss`/`.scss.d.ts`; pruning is explicitly CAT-D's call. Harmless to tsc.
- `mithrilPartialSyncRecommendation` / `mithrilPartialSyncButtonHintReady` still show pre-CAT-B copy in the
  catalogs — correct, those are CAT-B's edits later in the sequence, not CAT-C scope.
- Minor process nit (not a blocker): plan step 9's "record in research note" of the 304/601 override does not
  appear in `task-ux-702a-research.md`; the override is nonetheless authoritatively recorded in D-702a-4
  (plan + decisions) and the Implementation log, satisfying the "do not read as a regression" intent.

Blockers: none.

---

## Implementation

Timestamp: 2026-06-29T18:16:54Z
Speaker: Implementation
Category: CAT-B (Diagnostics recommendation → hover tooltip + reword — ISSUE-3, D-702a-3)

Summary:
Executed the CAT-B ordered steps on the shared working tree, renderer + i18n only, no backend/scss edits.

1. `MithrilPartialSyncRecommendation.tsx`: added
   `import { PopOver } from 'react-polymorph/lib/components/PopOver';` (path matches
   `DaedalusDiagnostics.tsx:9`). In `defineMessages`: reworded `recommendation` to the verbatim D-702a-3
   string `!!!If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified
   chain data to help speed up the sync.` and its description to "Tooltip copy shown on hover over the
   Mithril Sync button in diagnostics"; DELETED the `buttonHintReady` descriptor; KEPT `buttonLabel`/
   `buttonHintBlocked`.
2. Rewrote the render: wrapped the Mithril Sync `<button>` in `<PopOver maxWidth={280}
   content={<div className={styles.tooltipLabelWrapper}>{recommendation}</div>}>` (state-directory PopOver
   pattern); the blocked hint now renders only conditionally (`{isActionBlocked && <div
   className={styles.mithrilPartialSyncHint}>…buttonHintBlocked…</div>}`); DELETED the old always-visible
   `mithrilPartialSyncRecommendationCopy` `<div>` and the `hintMessage` ternary. Reused only classes already
   present in `DaedalusDiagnostics.scss.d.ts`; no `.scss` edit.
3. `MithrilPartialSyncSection.tsx`: confirmed by inspection — props unchanged, no edit needed.
4. i18n — EN: deleted `mithrilPartialSyncButtonHintReady`; set `mithrilPartialSyncRecommendation` to the EN
   tooltip string. JA (provisional, 601 glossary): deleted `buttonHintReady`; set recommendation to
   `Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、
   同期を高速化できます。`. `defaultMessages.json` + `translations/messages.json`: updated the recommendation
   `!!!` defaultMessage + description and removed the `buttonHintReady` descriptor in the
   `MithrilPartialSyncRecommendation.tsx` block; kept buttonLabel/buttonHintBlocked.
5. `MithrilPartialSyncSection.spec.tsx`: added a PopOver mock rendering BOTH children + content (so the
   hover-only tooltip copy is assertable); rewrote the three stale "back on recommendation" copy assertions
   (cancel/external-active/ESC) to robust view-state assertions (Mithril Sync button present + "Before
   Mithril Sync begins" heading absent); added a tooltip-coverage test (asserts the verbatim D-702a-3 copy
   is present and the removed ready-hint is absent) and a blocked-state test (button disabled + only the
   blocked hint).
6. `DaedalusDiagnostics.spec.tsx` (PopOver mocked children-only, so tooltip content is intentionally
   absent): rewrote the two recommendation tests to drop the now-lazy tooltip-content + ready-hint
   assertions and keep button-present + no-`/% synced/` + no-`/!!!/`; renamed them. The blocked-hint test
   stays green (buttonHintBlocked retained).

Cross-category cleanup (pre-existing CAT-C straggler, NOT a CAT-B behavior change): CAT-C reworded the
confirmation behind/behindUnknown copy but updated only `MithrilPartialSyncConfirmation.spec.tsx`, leaving
four confirmation-body assertions in CAT-B's two spec files (`MithrilPartialSyncSection.spec.tsx` ×1,
`DaedalusDiagnostics.spec.tsx` ×3) stale and RED before CAT-B began. Synced those four `getByText`
assertions to CAT-C's canonical strings ("Your node is about N epochs behind. Mithril Sync will restore
verified chain data to help your node sync faster." / behindUnknown fallback) so CAT-B's suites are green.
Mechanical copy sync only — no structural/behavioral change.

CAT-D handoff (flagged, no edit): `styles.tooltipLabelWrapper` is currently scss-nested under
`.layoutData .stateDirectoryPath`, so CAT-D must hoist/duplicate it for general tooltip use; the
`.mithrilPartialSyncRecommendationCopy` class is now unused in the `.tsx` (only the generated `.scss.d.ts`
references it) and CAT-D may prune it. CAT-B left all `.scss` untouched.

Locked-invariant adherence: copy/presentation only; amends no locked decision. Honors #3 (the button still
calls `onShowConfirmation` → the confirmation modal is the only start path; the tooltip is informational),
#4 (visibility still gated upstream by backend `isMithrilPartialSyncSignificantlyBehind`; CAT-B computes no
behind-ness/threshold), #10 (the `isMithrilPartialSyncEnabled` kill-switch gate untouched), #11 (no
bootstrap component/scss touched). Locked vocabulary preserved ("Mithril Sync"; "Cardano Node" per the
verbatim D-702a-3 string; no "partial sync" in user copy; no %/immutable — specs keep no-`/% synced/`).

Files:
- source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx
- source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx
- source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- source/renderer/app/i18n/locales/defaultMessages.json
- translations/messages.json

Verification:
- `yarn test:jest` on `MithrilPartialSyncSection.spec.tsx` + `DaedalusDiagnostics.spec.tsx`: 2 suites / 25
  tests PASS; `MithrilPartialSyncRecommendation.tsx` 100% coverage.
- `yarn compile` (tsc --noEmit, precompile regenerated `.scss.d.ts` via typedef:sass): clean, exit 0.
- `yarn i18n:check`: exit 0, "No duplicate ids found"; no mithril key in the untranslated (`!!!`) report
  (EN + JA both carry real values for the reworded recommendation key; the removed `buttonHintReady` is
  absent from all four catalogs).
- `yarn eslint` on the recommendation `.tsx` + the two specs: 0 errors. 9 warnings, all pre-existing
  (`no-explicit-any` on untouched `as any` defaultProps/fixture casts; two `no-empty-function` on the
  untouched unmount test).
- Prettier: the recommendation `.tsx` + all 4 i18n catalogs are clean under the pinned prettier 2.1.2. The
  two spec files are flagged by 2.1.2 ONLY on the multi-line `expect(screen.getByRole(...)).toBe*()` blocks
  — verified pre-existing: the HEAD versions of BOTH spec files fail pinned 2.1.2 identically on the same
  blocks (the documented env mismatch — committed code formatted with CI's newer prettier). My added lines
  deliberately match the surrounding committed multi-line style, so they are correct under CI's prettier.
  Not a CAT-B regression.

---

## Code Review

Timestamp: 2026-06-29T18:22:45Z
Speaker: Code Review
Category: CAT-B (Diagnostics recommendation → hover tooltip + reword — ISSUE-3, D-702a-3)

Decision: APPROVED — no blockers.

Scope reviewed: the CAT-B working-tree diff — `MithrilPartialSyncRecommendation.tsx`,
`MithrilPartialSyncSection.spec.tsx`, `DaedalusDiagnostics.spec.tsx`, and the four i18n
catalogs (`en-US.json`, `ja-JP.json`, `defaultMessages.json`, `translations/messages.json`,
recommendation/buttonHintReady descriptors only). CAT-A and CAT-C edits in the same tree are
out of CAT-B scope (already reviewed above). One broad adversarial pass.

Acceptance criteria — all met:
- Recommendation copy is now hover-only: the Mithril Sync `<button>` is wrapped in
  `<PopOver maxWidth={280} content={<div className={styles.tooltipLabelWrapper}>{recommendation}</div>}>`
  (matches the state-directory PopOver pattern at `DaedalusDiagnostics.tsx:9,673-685`). The old
  always-visible `mithrilPartialSyncRecommendationCopy` `<div>` and the `hintMessage` ready/blocked
  ternary are deleted. Recommendation EN copy is a BYTE-EXACT match to the verbatim D-702a-3 string
  ("If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain
  data to help speed up the sync."), confirmed by exact string compare.
- Ready-hint fully removed: `mithrilPartialSyncButtonHintReady` is absent from defineMessages and from
  ALL FOUR catalogs (grep count 0 across source/translations); no dangling reference anywhere.
- Blocked state: the hint now renders only conditionally `{isActionBlocked && <div
  className={styles.mithrilPartialSyncHint}>{buttonHintBlocked}</div>}`; button keeps
  `disabled={isActionBlocked}`. The new blocked-state spec asserts disabled button + only the blocked
  hint; the two pre-existing DaedalusDiagnostics "CTA disabled" tests stay green (buttonHintBlocked
  retained, value unchanged).
- Both specs pass: `yarn test:jest` on the two files = 2 suites / 25 tests PASS;
  `MithrilPartialSyncRecommendation.tsx` 100% coverage. Independently re-run.
- `yarn compile` (tsc --noEmit, precompile regen of `.scss.d.ts`): clean, exit 0 — all referenced
  classes (`tooltipLabelWrapper`, `mithrilPartialSyncHint`, `mithrilPartialSyncButton`, `layoutData`,
  `mithrilPartialSyncData`) are present in `DaedalusDiagnostics.scss.d.ts`.
- `yarn i18n:check`: exit 0, "No duplicate ids found"; no mithril key in the untranslated (`!!!`)
  report (EN + JA both carry real values for the reworded recommendation key).
- ESLint on the three source files: 0 errors (9 warnings, all pre-existing `no-explicit-any` fixture
  casts + `no-empty-function` on untouched lines).

Locked invariants honored:
- #3 (confirm-only start path): the button still calls `onShowConfirmation`; the PopOver is purely
  informational — no second start path introduced.
- CAT-A no renderer threshold / #4: CAT-B computes no behind-ness or significance; section visibility
  stays gated upstream by the backend `isMithrilPartialSyncSignificantlyBehind`
  (`MithrilPartialSyncSection.tsx` confirmed UNCHANGED — not in the working tree).
- #10 kill-switch: `isMithrilPartialSyncEnabled` gating untouched.
- #11 bootstrap not regressed: no bootstrap component or `.scss` touched; CAT-B made zero `.scss` edits
  (working tree has no scss change), so the committed `.scss.d.ts` already covers every referenced class.
- Cross-category boundaries (not CAT-B's surfaces, confirmed untouched by this category): CAT-E
  cancel-array wipe-trim, CAT-G finalize, and the shared-id reuse for CAT-F are all outside CAT-B's
  files and unaffected.

EN + JA: both updated for the one changed key (recommendation → tooltip copy, EN verbatim D-702a-3 /
JA provisional per the 601 glossary "Cardanoノードの同期に時間がかかりすぎると感じる場合は…同期を高速化できます。")
and the one removed key (buttonHintReady, EN+JA). `defaultMessages.json` + `translations/messages.json`
descriptors updated (defaultMessage + description → "Tooltip copy shown on hover…") and the
buttonHintReady descriptor block cleanly removed (valid JSON; i18n:check exit 0). buttonLabel
("Mithril Sync") and buttonHintBlocked retained.

Vocabulary: clean — "Mithril Sync" / "Mithril同期"; no "partial sync" in user copy; no %/immutable
(specs keep the no-`/% synced/` and no-`/!!!/` guards). "Cardano Node" (capital N) in the tooltip is the
locked, grill-approved verbatim D-702a-3 wording (decisions doc :68-70), NOT a vocabulary drift.

No duplicate i18n id: CAT-B introduces no new id; i18n:check confirms "No duplicate ids found". The
shared D-702a-6 key reuse is CAT-C/CAT-F's concern, not touched here.

Tests updated and meaningful: the three brittle "back on recommendation copy" assertions were replaced
with robust view-state assertions (button present + "Before Mithril Sync begins" heading absent); a
dedicated tooltip-coverage test asserts the verbatim copy + ready-hint absence; a blocked-state test
asserts disabled + only-blocked-hint. The DaedalusDiagnostics recommendation tests correctly drop the
now-lazy tooltip content (PopOver mocked children-only at spec :8-9) while keeping the sync-%/!!! guards.

Notes (non-blocking; cross-category / process):
- The tooltip-coverage assertion in `MithrilPartialSyncSection.spec.tsx` relies on a PopOver mock that
  force-renders BOTH children and content (so the hover-only copy is assertable in jsdom). This is the
  sanctioned plan-step-9 approach; it verifies the copy is wired as PopOver content, NOT real hover
  reveal — actual hover/contrast is deferred to operator visual validation (CAT-D/CAT-H). Informational.
- CAT-B synced four stale CAT-C confirmation-body `getByText` assertions left RED in its two spec files
  (Section ×1, Diagnostics ×3) to the already-landed CAT-C canonical strings ("Your node is about N
  epochs behind. Mithril Sync will restore verified chain data to help your node sync faster." /
  behindUnknown fallback). Verified those synced strings byte-match the landed CAT-C `en-US.json` values;
  this is a mechanical copy-sync (no CAT-B behavior change), transparently flagged in the Implementation
  log. Acceptable — CAT-C's spec-only update had left CAT-B's suites red.
- CAT-D handoff (flagged by CAT-B, no edit): `styles.tooltipLabelWrapper` is currently scss-nested under
  `.layoutData .stateDirectoryPath`, so CAT-D must hoist/duplicate it for general tooltip use;
  `.mithrilPartialSyncRecommendationCopy` is now unused in the `.tsx` (only `.scss`/`.scss.d.ts`
  reference it) and CAT-D may prune it. CAT-B correctly left all `.scss` untouched. Carry into CAT-D review.

Blockers: none.

---

## Implementation

Timestamp: 2026-06-29T18:31:20Z
Speaker: Implementation
Category: CAT-E (Cancelled (pre-cutover) recovery trim + button copy + subtext→body + right-align — ISSUE-6)

Summary:
Executed the CAT-E ordered steps on the shared working tree: backend cancel-array trim (ADR D-702a-2) +
renderer button copy / subtext→body / primary-on-right layout + specs + story.

1. Backend trim (both cancel-originated paths). In `MithrilPartialSyncService.ts`, replaced the two
   multi-line `allowedRecoveryActions: ['retry','restart-normal','wipe-and-full-sync']` blocks (the
   `status:'cancelled'` path and the cancel-cleanup `status:'failed'` path, now lines ~301 and ~311)
   with `allowedRecoveryActions: ['retry', 'restart-normal'],`. Did NOT touch the inline active-sync
   failure return at `_deriveAllowedRecoveryActions` (line 552: `return ['retry','restart-normal',
   'wipe-and-full-sync'];`) nor any `['wipe-and-full-sync']` line — the intentional asymmetry (active-sync
   *failure* stays 3-action; *cancelled* dialogue is 2-action) is preserved per the literal D-702a-2 citation.
2. Backend spec. In `MithrilPartialSyncService.spec.ts`, applied the same trim to the two cancel-path
   assertion blocks (now lines ~877 and ~901). Left the inline `['restart-normal','wipe-and-full-sync']`
   fixture (~915) and all `['wipe-and-full-sync']` assertions untouched.
3. Button copy. `MithrilBootstrap.messages.ts`: `partialSyncRetry` → `!!!Retry Mithril Sync (fast)`;
   `partialSyncRestartNormally` → `!!!Restart Node Sync (slow)`. Left `partialSyncWipeAndFullSync` and the
   CAT-G-owned `partialSyncContinue`/`partialSyncCompletedSubtitle` untouched.
4. i18n (EN+JA): `loading.mithrilPartialSync.error.retry` → EN `Retry Mithril Sync (fast)` / JA
   `Mithril同期を再試行（高速）`; `...error.restartNormally` → EN `Restart Node Sync (slow)` / JA
   `ノード同期を再起動（低速）` (provisional, full-width 高速/低速 convention per the 601 glossary). Updated
   `en-US.json`, `ja-JP.json`, `defaultMessages.json` (`!!!`-prefixed) and `translations/messages.json`.
   No keys added/removed; cancelled/wipe copy unchanged.
5. Subtext→body + primary-on-right (shared `MithrilErrorView.tsx`). Added `hintAsBody?: boolean;` to Props
   and destructured it; the hint now renders as `<p className={styles.hintBody}>` when `hintAsBody` else the
   existing `<div className={styles.errorHint}>`. Before the `.actions` map, compute `orderedActions =
   [...non-primary, ...primary]` (stable filter-partition) and map THAT, so the primary action renders
   last/rightmost. Bootstrap callers pass no `hintAsBody` (hint stays subtext).
6. Layout-only scss (single edit, `MithrilErrorView.scss`). `.actions` `justify-content: flex-start` →
   `flex-end`; added `.hintBody { color: var(--theme-mithril-body-text-color); font-size: 15px; line-height:
   1.5; margin-top: 8px; }` (reuses the existing body token — color governed by CAT-D's later audit). This
   is the ONLY scss CAT-E touches (layout, not color); CAT-D must not revert it. Regenerated
   `MithrilErrorView.scss.d.ts` via `yarn typedef:sass` (now exports `hintBody`).
7. Overlay wires hint-as-body. `MithrilPartialSyncOverlay.tsx`: added `hintAsBody={status === 'cancelled'}`
   on the `MithrilErrorView` usage. No other overlay change (CAT-G owns the completed branch).
8. Overlay spec. Replaced both `/restart normally/i` matchers with `/restart node sync/i`; rewrote the
   cancelled-recovery test to `canWipeAndFullSync:false`, removed the wipe click, added a
   `queryByRole('button',{name:/wipe chain data and do full mithril sync/i}).not.toBeInTheDocument()`
   negative and a DOM-order assertion `['Restart Node Sync (slow)','Retry Mithril Sync (fast)']`; in the
   cancelled-hint test added `expect(cancelledHint.tagName).toBe('P')`. Did NOT touch the completed test
   (CAT-G).
9. Story accuracy. `MithrilPartialSyncOverlay.stories.tsx`: removed `canWipeAndFullSync` from the
   'Cancelled' story (now shows only Retry+Restart). Left the two Failed stories.

Sanctioned ADR + intentional asymmetry: ADR D-702a-2 amendment applied (backend stops emitting
`wipe-and-full-sync` for the two cancel-originated arrays only). Post-cutover startup-failure wipe
(`mithrilPartialSyncNodeStartup.ts` `['wipe-and-full-sync']`) UNCHANGED; the active-sync failure 3-action
set (`_deriveAllowedRecoveryActions:552`) UNCHANGED — the cancelled(2)/failed(3) asymmetry is intentional
per the literal citation, flagged for review.

Shared-error-view note (#11, sanctioned visual-only): the `flex-end` + primary-last reorder also flips the
empty-chain bootstrap error view's footer (primary now on the right) — this cross-dialog alignment is
explicitly sanctioned by the decisions doc "Deferred / investigate" note ("if the footer/action layout is
shared with the bootstrap dialogs, align those too visual-only"). No logic/handler change; verified the
bootstrap spec stays green.

Locked-invariant adherence: honors #2/#4/#5 (only the backend-emitted array is trimmed; renderer/store
action logic renders strictly from `allowedRecoveryActions` — unchanged), #6 (`cancel()` still hard-rejects
post-cutover for installing/finalizing — guard spec green), #11 (shared `MithrilErrorView` reorder/right-align
is visual-only; bootstrap component logic untouched, bootstrap spec green). Locked vocabulary preserved
("Mithril Sync"/"Node Sync"; no "partial sync" in user copy; no %/immutable).

Files:
- source/main/mithril/MithrilPartialSyncService.ts
- source/main/mithril/MithrilPartialSyncService.spec.ts
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts
- source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss
- source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss.d.ts (regenerated)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- source/renderer/app/i18n/locales/defaultMessages.json
- translations/messages.json
- storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx

Verification:
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`: 1 suite / 38 tests PASS (cancel
  trim + post-cutover wipe-only + cancel-post-cutover-rejection guards all green).
- `yarn test:jest .../MithrilPartialSyncOverlay.spec.tsx`: 1 suite / 12 tests PASS (label swap, no-wipe,
  DOM order, hint-as-`<p>`, plus the locale-strings guard `loading.mithrilPartialSync.*` no-`!!!` test).
- `yarn test:jest .../MithrilBootstrap.spec.tsx`: 1 suite / 10 tests PASS (shared-error-view reorder does
  not regress the empty-chain bootstrap flow — #11).
- `yarn test:jest source/renderer/app/App.spec.tsx`: 1 suite / 2 tests PASS (overlay failed-state path).
- `yarn compile` (tsc --noEmit; precompile regenerated `.scss.d.ts` via typedef:sass): clean, exit 0
  (`hintBody` present in the regenerated `MithrilErrorView.scss.d.ts`).
- `yarn lint`: 0 errors in the touched files (only the repo-wide pre-existing storybook `@ts-ignore`/
  `no-explicit-any` warnings).
- `./node_modules/.bin/stylelint MithrilErrorView.scss`: CLEAN. (The repo-wide `yarn stylelint` glob trips
  on `SyncingConnectingMithrilPrompt.scss:9` `order/properties-alphabetical-order` — a PRE-EXISTING issue
  in a file CAT-E did not touch (git diff empty); it is a CAT-D/CAT-F scss surface, out of CAT-E scope.)

---

## Code Review — CAT-E (Cancelled recovery trim + button copy + subtext→body + right-align)

**Speaker:** Code Review
**Timestamp:** 2026-06-29 18:38 UTC

One adversarial pass over the CAT-E working-tree diff (backend service + spec, MithrilBootstrap.messages.ts,
MithrilErrorView.tsx/.scss, MithrilPartialSyncOverlay.tsx/.spec.tsx, the four i18n catalogs, Cancelled story)
against the CAT-E acceptance criteria (task-ux-702a.md:434-510) and ADR D-702a-2.

Acceptance criteria — ALL MET:
- Backend trim confined to the two PRE-CUTOVER cancel arrays inside `cancel()`: `status:'cancelled'`
  (MithrilPartialSyncService.ts:301) and the cancel-cleanup `status:'failed'` catch (:311), both now
  `['retry','restart-normal']`. The active-sync failure path `_deriveAllowedRecoveryActions` inline
  `return ['retry','restart-normal','wipe-and-full-sync']` (:552) is UNTOUCHED (intentional 3-action
  asymmetry per D-702a-2 literal citation); post-cutover `['wipe-and-full-sync']` (:241, :549) UNTOUCHED.
- Backend spec mirrors the trim on both cancel-path assertions (:877/:905 region); the inline 3-action +
  `['wipe-and-full-sync']` fixtures left intact.
- Button copy EN "Retry Mithril Sync (fast)" / "Restart Node Sync (slow)" applied across
  MithrilBootstrap.messages.ts (with `!!!`), en-US.json, defaultMessages.json, translations/messages.json;
  JA "Mithril同期を再試行（高速）" / "ノード同期を再起動（低速）" in ja-JP.json (full-width 高速/低速 per glossary).
- subtext→body: `hintAsBody?: boolean` prop on MithrilErrorView renders `<p className={styles.hintBody}>`
  (15px `--theme-mithril-body-text-color`, a pre-existing token) vs the prior `<div styles.errorHint>`
  (13px secondary); overlay passes `hintAsBody={status === 'cancelled'}`. New `.hintBody` rule is
  alphabetised; stylelint clean on this file.
- right-align/primary-last: `.actions justify-content: flex-end`; `orderedActions` = non-primary first then
  primary, so the primary renders last/rightmost. Cancelled = retry(primary)+restartNormally(secondary) →
  DOM order ["Restart Node Sync (slow)","Retry Mithril Sync (fast)"], exactly as the spec asserts.
- Overlay spec: cancelled-recovery test sets `canWipeAndFullSync:false`, asserts no wipe button + the
  DOM-order array; cancelled-hint test asserts `tagName === 'P'`; `/restart normally/`→`/restart node sync/`.
  Cancelled story drops `canWipeAndFullSync`.

Locked invariants — HONORED:
- #2/#5: only the backend-emitted array is trimmed; the renderer still renders strictly from
  `allowedRecoveryActions` via the unchanged can* booleans (overlay diff is +1 line, `hintAsBody`).
- #6: the trim lives in the cleanup branches only; `cancel()`'s post-cutover hard-reject guard is untouched
  and its spec stays green.
- #11: the shared MithrilErrorView reorder/right-align is visual-only and sanctioned (decisions.md:166-168);
  MithrilBootstrap.spec.tsx (10 tests) and the empty-chain bootstrap flow are unaffected — re-ran, green.
- CAT-G finalize path is NOT in this diff (CAT-G not yet applied; `partialSyncContinue` still referenced),
  so nothing here touches reset/staging-removal/marker-clear/folder-deletion. CAT-A renderer-threshold and
  bootstrap concerns N/A to CAT-E.
- Vocabulary clean: user copy uses "Mithril Sync"/"Node Sync"; "partialSync*" remains only in code/message
  identifiers. No %/immutable surfaced. No new/duplicate i18n id (CAT-E changes values only).

Verification re-run by reviewer:
- `yarn test:jest MithrilPartialSyncService.spec.ts MithrilPartialSyncOverlay.spec.tsx` → 2 suites / 50 PASS
  (includes the `loading.mithrilPartialSync.*` no-`!!!` EN+JA locale guard).
- `yarn test:jest MithrilBootstrap.spec.tsx` → 10 PASS (#11 not regressed by the shared-view reorder).
- `yarn compile` (tsc, precompile regenerated `.scss.d.ts`) → clean, exit 0.
- `stylelint MithrilErrorView.scss` → clean.

Notes (cross-category, NOT CAT-E blockers):
- Repo-wide `yarn stylelint` trips on `SyncingConnectingMithrilPrompt.scss:9`
  (`order/properties-alphabetical-order`) — a CAT-D/CAT-F scss surface, git-unmodified here; pre-existing,
  flag for CAT-D/CAT-F.
- CAT-D must NOT revert the `.actions flex-end` + `.hintBody` rule (already recorded in the plan's CAT-D
  coordination note).

**Blockers:** none.
**Decision:** APPROVED.

---

## Implementation

Timestamp: 2026-06-29T18:47:52Z
Speaker: Implementation
Category: CAT-F (Proactive prompt: known-behind gating + persist into Wallet Summary — ISSUE-7, D-702a-5)

Summary:
Executed the CAT-F ordered steps on the shared working tree, renderer + i18n only (no backend/scss). The
proactive Mithril prompt is hoisted off the loading screen into an app-level container so it persists across
loading -> Wallet Summary, gates on CAT-A's `networkStatus.isBehindnessKnown` (anti-flash), reuses CAT-C's
shared canonical sentence for the confirm view, and capitalizes the subtitle handoff note.

1. NEW `containers/loading/MithrilProactivePromptContainer.tsx` (`@inject('stores','actions') @observer`,
   `defaultProps={stores:null,actions:null}`). Ports the displayed-figure math VERBATIM from
   `SyncingConnectingPage` (`networkEpoch`/`localEpoch`/`behindByEpochs`) for the figure ONLY; the show-gate
   is `mithrilPartialSync.status==='idle' && isPartialSyncEnabled && isSignificantlyBehind &&
   networkStatus.isBehindnessKnown && !proactivePromptDismissedThisSession`. It consumes CAT-A's
   `isBehindnessKnown` computed DIRECTLY (NOT a local `behindByEpochs !== undefined` re-derivation) so that
   deliverable stays load-bearing. Returns `null` or `<SyncingConnectingMithrilPrompt behindByEpochs=...
   onStart={startPartialSync} onDismiss={dismissProactivePrompt} />`. Top-of-file doc comment explains the
   hoist (persist), the `status==='idle'` mutual-exclusion with the overlay, and the `isBehindnessKnown`
   anti-flash gate (CAT-A coupling).
2. `App.tsx`: imported the container and mounted `<MithrilProactivePromptContainer />` after the
   `mithrilPartialSync.shouldShowOverlay && <MithrilPartialSyncOverlay/>` block and before
   `<RTSFlagsRecommendationOverlayContainer/>` (sibling of `<Router>`, so it survives every route; gated to
   `status==='idle'` so it never co-renders with the overlay).
3. `SyncingConnecting.tsx`: removed the `SyncingConnectingMithrilPrompt` import, the four prompt props from
   `Props` (`showMithrilPrompt`/`mithrilBehindByEpochs`/`onStartMithrilSync`/`onDismissMithrilPrompt`), their
   destructure, and the JSX block (the prompt no longer mounts on the loading screen).
4. `SyncingConnectingPage.tsx`: removed `networkTip`/`localTip` from the `networkStatus` destructure, the
   `behindByEpochs`/`showMithrilPrompt` computations, `mithrilPartialSync` from the stores destructure (now
   unused), and the four `<SyncingConnecting>` props; left a comment pointing at the new app-level container.
5. `SyncingConnectingMithrilPrompt.tsx`: imported the shared CAT-C module
   (`../../status/MithrilSyncProcessSummary.messages`), DELETED the local `promptConfirmBody` descriptor, and
   in `renderConfirmView()` now renders `mithrilSyncProcessSummaryMessages.processSummary` (the exact shared
   id `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` — no duplicate declaration). Capitalized
   `promptHandoffNote` ("Mithril sync" -> "Mithril Sync"). Kept "Mithril Sync (fast)" as the primary/right
   action (no `autoFocus`); added a one-line comment that the visible-highlight contrast across themes is
   CAT-D's concern (CAT-F makes no scss change).
6. i18n (EN+JA): `en-US.json`/`ja-JP.json` — DELETED `mithrilProactivePromptConfirmBody`; capitalized
   `mithrilProactivePromptHandoffNote` (EN "...start the Mithril Sync..."; JA
   "...DiagnosticsからMithril同期を開始..."). Did NOT add the shared `mithrilSyncProcessSummary` key here (CAT-C
   owns it — already present EN+JA). Ran `yarn i18n:manage` to regenerate `defaultMessages.json` +
   `translations/messages.json`: the SyncingConnectingMithrilPrompt descriptor block drops the confirm-body
   descriptor and reflects the handoff-note change; no churn for the shared key.

Shared i18n key reuse (per CAT-C handoff): consumed the EXACT id
`daedalus.diagnostics.dialog.mithrilSyncProcessSummary` by importing `MithrilSyncProcessSummary.messages.ts`;
dropped the private `mithrilProactivePromptConfirmBody`. `i18n:check`: "No duplicate ids found".

Locked-invariant adherence: honors #3 (the choice button still only calls `showConfirmation`; `handleStart`
remains the SOLE `onStart` call site — confirmation precedes every start; no second start path), #4 (offer
still uses the backend `isSignificantlyBehind`; the new known-gate is a display-availability check, NOT a
renderer-computed threshold), #10 (the `isPartialSyncEnabled` kill-switch precondition kept in the gate),
#11 (during empty-chain bootstrap there is no synced local tip so `isBehindnessKnown` is false -> prompt
hidden; the optional `mithrilBootstrap.status==='idle'` guard was left out as the plan marks it optional and
the existing gates already suppress it — App.spec confirms the container renders nothing under bootstrap-like
stores). **D-702a-5 governs** the mount relocation; **D-702a-6 honored** via the shared canonical key. Locked
vocabulary preserved ("Mithril Sync" capitalized; "Standard Sync (slow)"; no "partial sync" in user copy;
epochs-only, no %/immutable).

Files:
- source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx (new)
- source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx (new)
- source/renderer/app/App.tsx
- source/renderer/app/components/loading/syncing-connecting/SyncingConnecting.tsx
- source/renderer/app/containers/loading/SyncingConnectingPage.tsx
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- source/renderer/app/i18n/locales/defaultMessages.json (regenerated by i18n:manage)
- translations/messages.json (regenerated by i18n:manage)

Verification:
- `yarn test:jest` on `MithrilProactivePromptContainer.spec.tsx` + `SyncingConnectingMithrilPrompt.spec.tsx`
  + `App.spec.tsx`: 3 suites / 18 tests PASS. New container at 100% coverage; `SyncingConnectingMithrilPrompt.tsx`
  100% statements. The container gating matrix asserts: all-pass renders "Your node is about 3 epochs behind.";
  non-idle status / disabled / not-significantly-behind / dismissed -> empty DOM; and the anti-flash case
  (`isBehindnessKnown:false` WITH finite tips present, so a local re-derivation would be truthy) -> empty DOM,
  proving the gate reads CAT-A's computed and not a local `behindByEpochs` re-derivation. The prompt spec adds
  the capitalized handoff-note assertion and the shared canonical confirm-body assertion (plus a negative for
  the removed private copy).
- `yarn compile` (tsc --noEmit; precompile regenerated `.scss.d.ts` via typedef:sass): clean, exit 0.
- `yarn i18n:check`: "No duplicate ids found"; no mithril key in the untranslated (`!!!`) report (EN+JA both
  carry real values; the removed `mithrilProactivePromptConfirmBody` is absent from all four catalogs).
- `yarn eslint` on the 7 touched source files: 0 errors. Warnings are the known repo-wide patterns: the
  `inject`/`observer` "defined but never used" DECORATOR-import false positives (identical on the committed
  `LoadingPage.tsx`/`MithrilBootstrapPage.tsx` — verified) and pre-existing `no-explicit-any`/empty-function
  spec-harness warnings. Not a CAT-F regression.
- Prettier: all touched files clean under the pinned 2.1.2 EXCEPT `SyncingConnectingPage.tsx`, which 2.1.2
  wants reformatted to the older multi-line-brace destructure. My single-line + break-after-`=` style is the
  NEWER CI-prettier convention the repo is actually formatted with (verified against committed
  `LoadingPage.tsx:36`, `TopBarContainer.tsx:27`, `UndelegateWalletDialogContainer.tsx:97` which use the same
  `} =` style that 2.1.2 rejects). Documented env mismatch, not a CAT-F regression.
- No e2e/cucumber reference to the removed props (`showMithrilPrompt`/`mithrilBehindByEpochs`/
  `onStartMithrilSync`/`onDismissMithrilPrompt`/`mithrilProactivePromptConfirmBody`/`ProactivePrompt`) — grep
  over `tests/` is empty.

CAT-D handoff (flagged, no edit): the "Mithril Sync (fast)" primary highlight contrast across the 10 themes is
CAT-D's (theme tokens / `SyncingConnectingMithrilPrompt.scss`); CAT-F made zero scss changes.

---

## Code Review — CAT-F (Proactive prompt: known-behind gating + persist into Wallet Summary — ISSUE-7 / D-702a-5)

Timestamp: 2026-06-29T18:55:14Z
Speaker: Code Review
Category: CAT-F (depends on CAT-A behind-ness-known signal; consumes CAT-C shared key)

Decision: APPROVED — no blockers.

Scope reviewed: working-tree diff for the CAT-F files
(`MithrilProactivePromptContainer.tsx`/`.spec.tsx` new, `App.tsx`,
`SyncingConnecting.tsx`, `SyncingConnectingPage.tsx`,
`SyncingConnectingMithrilPrompt.tsx`/`.spec.tsx`, EN/JA/defaultMessages/translations
i18n). One broad adversarial pass against the CAT-F acceptance criteria, the D-702a-5/-6
ADRs, and the locked safety boundaries.

Acceptance criteria — all met:
- Anti-flash + persist: the prompt is hoisted into a new app-level
  `MithrilProactivePromptContainer`, mounted in `App.tsx` as a sibling of `<Router>`
  (in the existing overlay region beside `MithrilPartialSyncOverlay` /
  `RTSFlagsRecommendationOverlayContainer`), so it survives the loading→Wallet Summary
  route swap. The old `SyncingConnecting.tsx` mount + the four prompt props + the
  `SyncingConnectingPage.tsx` gating computation are fully removed; no stale
  `showMithrilPrompt`/`mithrilBehindByEpochs`/`onStartMithrilSync`/`onDismissMithrilPrompt`
  reference remains anywhere under `source/renderer`.
- The gate is `status==='idle' && isPartialSyncEnabled && isSignificantlyBehind &&
  isBehindnessKnown && !proactivePromptDismissedThisSession` — exactly the plan formula.
  It reads CAT-A's `networkStatus.isBehindnessKnown` computed DIRECTLY (verified present at
  `NetworkStatusStore.ts:874`), NOT a local `behindByEpochs !== undefined` re-derivation;
  `behindByEpochs` is computed only for the displayed figure. CAT-A's deliverable stays
  load-bearing/non-orphaned.
- "Mithril Sync (fast)" remains the primary/right (default) action
  (`['primary', styles.actionButton, styles.primaryButton]`, rendered last); no `autoFocus`
  added; contrast deferred to CAT-D with an in-code comment. Subtitle/handoff note
  capitalized to "Mithril Sync".
- Confirm view now uses the shared canonical D-702a-6 sentence via
  `mithrilSyncProcessSummaryMessages.processSummary` (CAT-C's module); the private
  `mithrilProactivePromptConfirmBody` descriptor + key are deleted from the component and
  from all four catalogs.

Invariants verified:
- No duplicate i18n id: `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` is declared
  exactly once (CAT-C `MithrilSyncProcessSummary.messages.ts`) and imported by BOTH
  consumers (confirmation + prompt). CAT-F creates no shared key locally.
- #3 (confirmation precedes start): the choice "Mithril Sync (fast)" button only calls
  `showConfirmation` (reveals the confirm view); the single start site stays
  `handleStart`→`onStart`. No second start path.
- #4 (no renderer threshold): offer signal is still backend `isSignificantlyBehind`;
  `isBehindnessKnown` is a tip-availability boolean, not a renderer-computed significance
  threshold.
- #10 (kill switch): prompt self-gates on `isPartialSyncEnabled`.
- #11 (bootstrap not regressed): prompt gated on `status==='idle'`; the overlay shows only
  for non-idle display statuses — verified mutually exclusive at the type level
  (`MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` in `common/types/mithril-partial-sync.types.ts:86`
  explicitly filters out `'idle'`, so `shouldShowOverlay` is false whenever the prompt can
  render and vice-versa). No bootstrap component/scss touched.
- EN+JA both updated (handoff note capitalized in both; confirm-body key removed from both;
  shared key present once in each), plus `defaultMessages.json` + `translations/messages.json`.

Tests: new container spec is meaningful — the gating matrix covers all-pass (asserts the
displayed "Your node is about 3 epochs behind." copy, both choice buttons),
non-idle/disabled/not-significantly-behind/dismissed → empty DOM, and the anti-flash case
that keeps finite tips present while `isBehindnessKnown:false` → empty DOM, proving the gate
consumes CAT-A's computed rather than a local re-derivation. Prompt spec adds the capitalized
handoff + shared canonical confirm-body assertions plus a negative for the removed copy. Ran
`yarn test:jest` on both CAT-F specs: 2 suites / 16 tests PASS. `yarn compile`: clean (no
`error TS`). `yarn lint`: no errors in CAT-F files.

Notes (non-blocking, out of CAT-F's stated scope):
- Pre-existing hardcoded English fallback `'Unable to start Mithril sync.'`
  (`SyncingConnectingMithrilPrompt.tsx:137`) and the lowercase "standard sync" in the
  untouched `promptBodyBenefit` use lower-case vocabulary; not introduced or in-scope for
  CAT-F (plan step 6 capitalizes only `promptHandoffNote`). Flag for a vocabulary sweep.
- `MithrilProactivePromptContainer` injects `actions` but never reads it (plan specified
  `@inject('stores','actions')`); harmless, could drop the `actions` inject later.
- App-level visual positioning of the now-persistent prompt across the Wallet Summary page
  is a CAT-D / operator-visual-validation concern, not a code-review item.

---

## Implementation

Timestamp: 2026-06-29T19:01:56Z
Speaker: Implementation
Category: CAT-G (Completion auto-transition — auto-fire D9 finalize on timeout — ISSUE-8, ADR D-702a-1)

Summary:
Executed the CAT-G ordered steps on the shared working tree, renderer + i18n only (no backend/IPC/scss).
The completed Mithril-partial-sync overlay no longer renders a "Continue to Daedalus" button; it now shows
a loading-style hand-off (spinner + "Returning to Daedalus...") and AUTO-FIRES the existing finalize IPC on
a completed-timeout via the unchanged `store.dismissCompletedOverlay` (reset-to-idle + remove staging dir +
clear marker + folder deletion all preserved). The trigger swaps from a click to a timeout; the finalize
call is byte-for-byte unchanged.

1. `MithrilPartialSyncOverlay.tsx`: `import React, { useEffect } from 'react';`. After `PROGRESS_STATUSES`
   added `const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;` (with an ADR D-702a-1 doc comment). After the props
   destructure added an unconditional `useEffect` keyed on `[status, onDismissCompleted]` that, only while
   `status === 'completed'`, schedules `setTimeout(() => onDismissCompleted(), 4000)` and clears it on
   cleanup. `onDismissCompleted` (the stable MobX action → existing finalize IPC) is kept and now invoked by
   the timer.
2. In the `MithrilProgressView` invocation: `actionLabel` → always `MithrilBootstrapMessages.cancel`
   (dropped the completed→Continue ternary); added `'completed'` to the `hideAction` list (so no action
   button renders on the completed frame); `onAction` → always `onCancel`. Kept the completed subtitle
   ternary. Added `completedTransitionLabel={intl.formatMessage(MithrilBootstrapMessages.partialSyncCompletedTransition)}`.
3. `MithrilProgressView.tsx`: added optional `completedTransitionLabel?: string;` to Props + destructure;
   computed `isCompletedTransition = status === 'completed' && !!completedTransitionLabel`; after the
   `isStartingNode` block rendered a gated completion block (spinner `SVGInline` + `<h2>` caption) reusing
   the existing `.completionBlock`/`.completionSpinner`/`.completionTitle` classes (NO scss edit). Bootstrap
   never passes the prop (confirmed `MithrilBootstrap.tsx:192` does not set it), so its completed frame #11
   is byte-for-byte unchanged.
4. `MithrilBootstrap.messages.ts`: DELETED `partialSyncContinue`; reworded `partialSyncCompletedSubtitle`
   defaultMessage to `!!!Mithril Sync completed successfully.`; ADDED `partialSyncCompletedTransition` (id
   `loading.mithrilPartialSync.completed.transition`, defaultMessage `!!!Returning to Daedalus...`, spinner-
   caption description noting ADR D-702a-1).
5. i18n (EN+JA): `en-US.json`/`ja-JP.json` — DELETED `loading.mithrilPartialSync.completed.continue`; set
   `...completed.subtitle` (EN `Mithril Sync completed successfully.` / JA `Mithril同期が正常に完了しました。`);
   INSERTED `...completed.transition` (EN `Returning to Daedalus...` / JA `Daedalusに戻っています...`).
   Hand-edited `defaultMessages.json` + `translations/messages.json` to mirror (subtitle reword, delete the
   continue descriptor, add the transition descriptor). EN/JA key sets verified aligned; no `!!!` leaked.
6. `MithrilPartialSyncStore.ts`: updated the stale `dismissCompletedOverlay` comment to record that ADR
   D-702a-1 now invokes the unchanged finalize via the completed-overlay auto-timeout (not a Continue click).
   No logic change. Did NOT touch `App.tsx`/`App.spec.tsx` or the store spec (`dismissCompletedOverlay`
   behavior unchanged).
7. `MithrilPartialSyncOverlay.spec.tsx`: added `act` to the testing-library import; rewrote the completed
   test under scoped `jest.useFakeTimers()` (restored in `finally`) — asserts NO "Continue to Daedalus"
   button, "Returning to Daedalus..." present, `onDismissCompleted` not called before the timeout, then
   called exactly once after `advanceTimersByTime(4000)`. Kept the locale-strings parity guard. Did not
   touch CAT-E's cancelled-recovery tests.

Locked-invariant adherence: ADR D-702a-1 sanctioned amendment to locked #9/#16-as-amended-by-D9 — the
finalize IPC still fires (via the unchanged `store.dismissCompletedOverlay → mithrilPartialSyncFinalizeChannel`),
now triggered by the timeout; all cleanup semantics (folder deletion + marker clear + reset-to-idle) preserved.
Honors #11 (the `MithrilProgressView` spinner block is additive + gated on the new optional prop bootstrap
never passes; bootstrap completed frame #11 unchanged — bootstrap spec green), #2/#5 (error/recovery branch
untouched — CAT-E's `hintAsBody` and trimmed cancelled actions intact), #10 (no kill-switch change). No
backend/IPC-contract/scss change. The 601-touched `completed.subtitle` reword is the deliberate D-702a-1
override (not a regression). Locked vocabulary preserved ("Mithril Sync"; no "partial sync" in user copy;
no %/immutable).

Files:
- source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts
- source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx
- source/renderer/app/stores/MithrilPartialSyncStore.ts (comment only)
- source/renderer/app/i18n/locales/en-US.json
- source/renderer/app/i18n/locales/ja-JP.json
- source/renderer/app/i18n/locales/defaultMessages.json
- translations/messages.json

Verification:
- `yarn test:jest .../MithrilPartialSyncOverlay.spec.tsx`: 1 suite / 12 tests PASS (new fake-timer
  auto-finalize test + the `loading.mithrilPartialSync.*` no-`!!!` EN+JA locale guard).
- `yarn test:jest .../MithrilProgressView.spec.tsx .../MithrilBootstrap.spec.tsx`: 2 suites / 21 tests PASS
  (additive completion block does not regress the shared progress view; bootstrap completed frame #11
  unchanged).
- `yarn test:jest .../MithrilPartialSyncStore.spec.ts`: 1 suite / 25 tests PASS (comment-only change;
  `dismissCompletedOverlay flips it off and calls finalize once` green).
- `yarn compile` (tsc --noEmit; precompile regenerated `.scss.d.ts` via typedef:sass): clean, exit 0.
- ESLint on the 5 touched TS files: 0 errors (only pre-existing MobX decorator-import "unused" false
  positives + pre-existing `no-explicit-any`/unused-`arg` warnings — confirmed not introduced by CAT-G).
- Prettier: all touched files (5 source + 4 i18n catalogs) pass `prettier --check` under the pinned 2.1.2
  (ran `prettier --write` once on the spec to wrap the widened testing-library import).
- Grep confirms no active references to the removed `partialSyncContinue`/`completed.continue` remain
  (only explanatory comments + the spec's negative assertion).

---

## Code Review — CAT-G (Completion auto-transition — auto-fire D9 finalize on timeout — ISSUE-8 / ADR D-702a-1)

Timestamp: 2026-06-29T19:08:06Z
Speaker: Code Review
Category: CAT-G (renderer-only; amends locked #9/#16-as-amended-by-D9 per ADR D-702a-1)

Decision: APPROVED — no blockers.

Scope reviewed: the CAT-G working-tree diff — `MithrilPartialSyncOverlay.tsx`/`.spec.tsx`,
`MithrilProgressView.tsx`, `MithrilBootstrap.messages.ts`, `MithrilPartialSyncStore.ts`
(comment-only), and the four i18n catalogs (completed.subtitle / completed.transition /
completed.continue only). The CAT-A/C/B/E/F edits coexisting in the same tree are out of CAT-G
scope (already reviewed above). One broad adversarial pass against the CAT-G acceptance criteria
(task-ux-702a.md:685-689) and ADR D-702a-1.

Acceptance criteria — ALL MET:
- Completed overlay is now a loading-style hand-off: `MithrilProgressView` renders a gated
  `isCompletedTransition = status==='completed' && !!completedTransitionLabel` block (spinner
  `SVGInline` + `<h2>` caption) reusing the existing `.completionBlock/.completionSpinner/
  .completionTitle` classes (all present in `MithrilProgressView.scss` + `.scss.d.ts`; no scss
  edit). Overlay passes `completedTransitionLabel="Returning to Daedalus..."`.
- Auto-fire on timeout, exactly once, finalize unchanged: an unconditional `useEffect` keyed on
  `[status, onDismissCompleted]` schedules `setTimeout(() => onDismissCompleted(), 4000)` only
  while `status==='completed'` and clears it on cleanup. `onDismissCompleted` is wired in
  `App.tsx:119-121` as a DIRECT reference to the store's stable bound arrow action
  `mithrilPartialSync.dismissCompletedOverlay` (not an inline closure), so the dep array does NOT
  reset the timer on re-render — no double-fire / no never-fire. The spec proves it: no "Continue
  to Daedalus" button, "Returning to Daedalus..." present, `onDismissCompleted` NOT called before
  the linger, then called exactly once after `act(() => advanceTimersByTime(4000))` under scoped
  `jest.useFakeTimers()` (restored in `finally`).
- Continue button removed: `actionLabel` is now always `cancel`; `'completed'` added to the
  `hideAction` list (so no action button renders on the completed frame); `onAction` is always
  `onCancel`; the completed-subtitle ternary is kept. `partialSyncContinue` /
  `loading.mithrilPartialSync.completed.continue` are gone from defineMessages and all four
  catalogs (grep: only residue is explanatory comments + the spec negative assertion).
- Finalize semantics preserved: the store `dismissCompletedOverlay` change is COMMENT-ONLY (logic
  still flips `isCompletedOverlayDismissed` then `await mithrilPartialSyncFinalizeChannel.request()`);
  the backend `finalizeCompletedPartialSync()` (MithrilPartialSyncService.ts:351-362) is UNTOUCHED
  and still does `_resetToIdleStatus()` + `fs.remove(stagingRoot)` (staging/folder deletion) +
  `clearMithrilPartialSyncMarker()` + `_clearRuntimeWorkState()`. The trigger swaps click→timeout;
  the finalize call is byte-for-byte unchanged. No backend/IPC-contract change in CAT-G.

Locked invariants — HONORED:
- ADR D-702a-1 sanctioned amendment to #9/#16-as-amended-by-D9: finalize still fires (via the
  unchanged store action), now via the completed-overlay auto-timeout instead of a Continue click;
  the 601-touched `completed.subtitle` reword is the deliberate D-702a-1 override (not a regression).
- CAT-G finalize still fires reset+staging-removal+marker-clear+folder-deletion: VERIFIED at
  source (finalize method intact; store wiring comment-only).
- #11 bootstrap not regressed: the `completedTransitionLabel` prop is optional and the new
  `MithrilProgressView` completion block is gated on `!!completedTransitionLabel`; grep confirms
  ONLY `MithrilPartialSyncOverlay.tsx` passes it — `MithrilBootstrap.tsx` and storybook do NOT, so
  the bootstrap completed frame is byte-for-byte unchanged. MithrilProgressView + MithrilBootstrap
  specs re-run green.
- #2/#5 error/recovery branch untouched: CAT-E's `hintAsBody={status==='cancelled'}` and trimmed
  cancelled actions coexist cleanly in the same overlay/spec; both cancelled tests pass.
- #10 no kill-switch change. CAT-E wipe-trim still confined to the two pre-cutover cancel arrays
  with the renderer rendering strictly from `allowedRecoveryActions` (re-verified holds in the
  current tree); CAT-A no-renderer-threshold holds. No duplicate i18n id introduced (CAT-G's
  `completed.transition` is a private CAT-G key, not the shared D-702a-6 sentence; CAT-F's reuse of
  CAT-C's shared id is intact and was verified in the CAT-F review).

EN + JA: both updated — `completed.subtitle` reworded (EN "Mithril Sync completed successfully." /
JA "Mithril同期が正常に完了しました。"), `completed.transition` added (EN "Returning to Daedalus..." /
JA "Daedalusに戻っています..."), `completed.continue` removed. `defaultMessages.json` +
`translations/messages.json` regenerated consistently (transition descriptor carries the ADR
D-702a-1 note). EN/JA parity holds (1496 == 1496, no key missing in either locale).

Vocabulary: clean — "Mithril Sync" capitalized; "Daedalus"; no "partial sync" in user copy; no
%/immutable. JA per the 601 glossary (Mithril同期).

Verification re-run by reviewer:
- `yarn test:jest MithrilPartialSyncOverlay.spec.tsx MithrilProgressView.spec.tsx
  MithrilBootstrap.spec.tsx` → 3 suites / 33 tests PASS (fake-timer auto-finalize + locale
  no-`!!!` guard; #11 bootstrap/progress-view unaffected).
- `yarn i18n:check` → exit 0, "No duplicate ids found, great!", no mithril key in the
  untranslated (`!!!`) report.
- `yarn compile` (tsc --noEmit; precompile regen of `.scss.d.ts`) → clean, exit 0.
- `yarn eslint` on the 5 touched TS files → 0 errors (10 warnings, all pre-existing: MobX
  decorator-import false positives, `no-explicit-any`, unused `arg` in the intlShape context fn —
  none introduced by CAT-G).
- `prettier --check` on the 5 source files + EN/JA catalogs → all clean.

Notes (non-blocking; cross-category / process):
- The completion block renders spinner-above-title, whereas the starting/stopping blocks render
  title-above-spinner. This matches the plan step-7 ordering verbatim ("spinner `SVGInline` +
  `<h2>` label"); final visual polish/contrast of the completion spinner+title is CAT-D /
  operator-visual scope (later in sequence), not a CAT-G code-review item.
- `MithrilProgressView` coverage dipped only on the pre-existing `elapsedSeconds` lines (66,118-119),
  unrelated to the additive completion block. Harmless.
- CAT-D (theme tokens / `.scss`) and CAT-H (storybook) remain pending per the sequence
  (CAT-A→C→B→E→F→G→D→H); the completion spinner/title styling and any partial-progress story are
  expected follow-ups, not CAT-G gaps.

Blockers: none.

---

## Implementation

Timestamp: 2026-06-29T19:18:20Z
Speaker: Implementation
Category: CAT-D (Theme/contrast audit — owns all .scss + theme tokens — ISSUE-5, ISSUE-6(theme), ISSUE-9)

Summary:
Executed the CAT-D ordered steps on the shared working tree, scss-only (NO .tsx/store/IPC/i18n edits; no
.tsx restyle of the confirmation — restyled the live `mithrilPartialSyncConfirmationRecovery` rule IN
PLACE). Ran LAST (after CAT-A/C/B/E/F/G) so all class names are settled. Verified every seam against live
code before editing.

1. STEP 0 (env): regenerated the gitignored `.scss.d.ts` sidecars via `yarn typedef:sass`
   (`typed-scss-modules`); identity-obj-proxy jest sidecar present (specs green).
2. Prompt card (`SyncingConnectingMithrilPrompt.scss`) — migrated EVERY color off the report-issue/color
   families to `--theme-mithril-*`: `.component` bg → `--theme-mithril-card-background`, added
   `box-shadow: 0 12px 42px var(--theme-mithril-card-shadow)` + `border: 1px solid
   var(--theme-mithril-panel-border-color)`; `.body`/`.confirmBody` → `--theme-mithril-card-text-color`;
   `.benefit` → `--theme-mithril-body-text-color`; `.handoffNote` → `--theme-mithril-secondary-text-color`;
   `.error` → `--theme-mithril-error-text-color` (was `--theme-color-error`). Also reordered `.component`
   properties alphabetically, fixing the PRE-EXISTING `SyncingConnectingMithrilPrompt.scss:9`
   `order/properties-alphabetical-order` stylelint failure flagged by the CAT-E review (repo-wide
   `yarn stylelint` now exits 0).
3. Prompt button (STEP 3) — removed the `background-color: var(--theme-report-issue-button-background-color)`
   override from `.primaryButton` so the global `primary` ButtonSkin governs the highlight. KEPT the selector
   (CAT-F consumes `styles.primaryButton`) with a single harmless `min-width: 180px` declaration rather than
   an empty block, so the css-modules local is guaranteed to survive the sass→css-loader pipeline at runtime
   (typed-scss-modules + identity-obj-proxy already resolve it for tsc/jest).
4. Prompt positioning (STEP 4, judgement) — CAT-F hoisted the prompt into an app-level
   `MithrilProactivePromptContainer` mounted as a `<Router>` sibling in `App.tsx`, so it now persists into
   Wallet Summary, which renders an 84px `TopBar` (the loading screen renders none). Changed `.component`
   `top: 0` → `top: 84px` (kept `margin: 24px auto 0`) so the fixed card clears the Wallet Summary header in
   all themes instead of clipping it. Flagged for operator visual confirmation on both surfaces.
5. Confirmation modal (`DaedalusDiagnostics.scss`, STEP 5) — fixed white-on-white with STANDARD dialog
   tokens (the modal surface is `--rp-modal-bg-color`, white in cardano/yellow, NOT a mithril surface):
   `.mithrilPartialSyncConfirmationBehind` color `--theme-network-window-white-color` →
   `--theme-dialog-title-color`; `.mithrilPartialSyncConfirmationBody` added
   `color: var(--theme-dialog-text-color)`; restyled the EXISTING live
   `.mithrilPartialSyncConfirmationRecovery` rule (the subtext `<p>` CAT-C renders, class contract verbatim)
   IN PLACE to `{ color: var(--theme-dialog-text-color); font-family: var(--font-regular); font-size: 14px;
   line-height: 1.5; margin: 16px 0 0; opacity: 0.7; }` — no `...Subtext` class added, no rename, no .tsx
   edit.
6. Confirmation dead-code (STEP 6) — deleted ONLY the three genuinely-orphaned rules
   `.mithrilPartialSyncConfirmationSteps`, `.mithrilPartialSyncConfirmation`,
   `.mithrilPartialSyncConfirmationTitle` (grep confirmed zero `styles.*` references in any .tsx; the only
   residual `mithrilPartialSyncConfirmationTitle` token is the unrelated i18n id string). Did NOT delete
   `.mithrilPartialSyncConfirmationRecovery` (live, restyled above). Left `.mithrilPartialSyncConfirmationActions`/
   `CancelButton`/`RecommendationCopy` untouched — not on the plan's delete list (the latter is CAT-B's
   optional prune; harmless dead CSS, no contrast impact). Regenerated `.scss.d.ts` confirms the d.ts dropped
   exactly Steps/Confirmation/Title and KEPT Recovery.
7. Cancelled dialogue (`MithrilErrorView.scss`, STEP 7) — `.errorHint` color
   `--theme-mithril-secondary-text-color` → `--theme-mithril-body-text-color` (a brightening; both light on
   the dark mithril card, no contrast regression). CONFIRMED + KEPT CAT-E's `.actions justify-content:
   flex-end` and `.hintBody` rule (cancelled subtext already routes to `.hintBody` via CAT-E's
   `hintAsBody={status==='cancelled'}`); did not revert either.
8. Shared-footer parity (STEP 8, optional/review-gated) — applied `MithrilDecisionView.scss` `.actions`
   `flex-start` → `flex-end` for parity with CAT-E's already-right-aligned shared `MithrilErrorView`
   (sanctioned by decisions.md:166-168 "align shared bootstrap footers visual-only, gated by code review").
   Left `MithrilProgressView.scss` `.actions: center` per plan. Visual-only; MithrilBootstrap spec green
   (#11 not regressed). Flagged for explicit reviewer sign-off.
9. Theme-token audit (STEP 9, verify-first) — programmatically confirmed all 9 daedalus mithrilBootstrap
   blocks AND `createTheme.ts` carry the full 61 `--theme-mithril-*` tokens, and that every token I consume
   plus `--theme-dialog-title/text-color` is present in all 9 themes + createTheme.ts. Cross-theme contrast
   verified by token inspection: the mithril card pair is theme-tuned (white/yellow → light card + dark text;
   cardano/light-blue/dark → dark card + light text), so the previously-invisible prompt body text is
   readable in all 10 themes; and `--theme-dialog-*` contrasts `--rp-modal-bg-color` in every theme
   (white/yellow #2d2d2d-on-#fff; cardano/light-blue #5e6066-on-white/#fafbfc; dark-blue #e9f4fe-on-#263345),
   fixing white-on-white. EXPECTATION MET: zero token-value edits; no non-mithril token block touched.
10. Regen + verify (STEP 10) — regenerated `.scss.d.ts`; ran stylelint (4 files + repo-wide), `yarn compile`,
    and the consuming jest suites. Storybook visual walk is the deferred operator-visual gate.

Locked-invariant adherence: edits ONLY `.scss` (verify-only on theme tokens — no token-value change), so
#2/#3/#4/#5/#8/#9/#10 are untouched by construction (no copy/IPC/store/behind-ness change; no i18n key
added/changed). #11: STEP 7/8 touch the bootstrap-shared `MithrilErrorView`/`MithrilDecisionView` footers —
visual-only (right-align/brighten), bootstrap spec re-run green. Deliberate scoped amendment honored: the
prompt/cancelled surfaces consume `--theme-mithril-*` while the CONFIRMATION MODAL is intentionally restyled
with STANDARD `--theme-dialog-*` tokens (mithril card/white tokens would reintroduce white-on-white on the
white modal). No new `--theme-mithril-*` tokens added. Locked vocabulary untouched (scss-only, no user copy;
"partialSync*" persists only in class identifiers, allowed). Amends no locked decision (the two sanctioned
ADRs D-702a-1/-2 are CAT-G/CAT-E surfaces, preserved here).

Files (tracked):
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss
- source/renderer/app/components/status/DaedalusDiagnostics.scss
- source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss
- source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView.scss
- (gitignored, regenerated by typedef:sass: the corresponding `.scss.d.ts` sidecars — `*.scss.d.ts` is in
  .gitignore:141, not tracked)

Verification:
- `yarn typedef:sass`: regenerated the sidecars; `DaedalusDiagnostics.scss.d.ts` now drops
  `mithrilPartialSyncConfirmation`/`...Title`/`...Steps` and KEEPS `...Recovery`;
  `SyncingConnectingMithrilPrompt.scss.d.ts` keeps `primaryButton`; `MithrilErrorView.scss.d.ts` keeps both
  `errorHint` + `hintBody`.
- `yarn compile` (tsc --noEmit; precompile regenerated `.scss.d.ts`): clean, exit 0.
- stylelint on the 4 touched files: CLEAN. Repo-wide `yarn stylelint` ("**/*.scss"): exit 0 (the
  pre-existing `SyncingConnectingMithrilPrompt.scss:9` ordering failure flagged by CAT-E is now resolved).
- `yarn test:jest` on the 7 specs consuming the touched scss (MithrilPartialSyncConfirmation,
  MithrilPartialSyncSection, DaedalusDiagnostics, SyncingConnectingMithrilPrompt,
  MithrilProactivePromptContainer, MithrilPartialSyncOverlay, MithrilBootstrap): 7 suites / 72 tests PASS
  (classNames resolve via the identity-obj-proxy sidecar; class add/remove/restyle did not break any spec).
- eslint: N/A (scss-only edits; no .ts/.tsx touched).
- Deferred to operator: the Storybook cross-theme visual walk (prompt Known/Unknown, confirmation
  Known/Unknown/Start Error, cancelled overlay, progress overlay) + bootstrap-story regression spot-check —
  primary visual gate per the plan; contrast pre-verified by token inspection above.

---

## Code Review: CAT-D — Theme/contrast audit (owns all `.scss` + theme tokens)

- **Speaker:** Code Review
- **Timestamp:** 2026-06-29T19:24:58Z

One broad adversarial pass over the CAT-D working-tree diff. CAT-D is correctly confined to **4 `.scss`
files** (`SyncingConnectingMithrilPrompt.scss`, `DaedalusDiagnostics.scss`, `MithrilErrorView.scss`,
`MithrilDecisionView.scss`); **zero theme-token `.ts` edits** (`git status` shows no `themes/daedalus/*.ts`
or `createTheme.ts` change) — consistent with the plan's "no token-value edits expected" since no concrete
contrast failure forced one. No copy/IPC/store/i18n change.

Acceptance criteria — verified:
1. **Prompt card token migration (STEP 2/3):** every color moved off the report-issue family onto
   `--theme-mithril-*` exactly per plan — `.component` bg→card-background + `box-shadow ...card-shadow` +
   `border ...panel-border-color`; `.body`/`.confirmBody`→card-text; `.benefit`→body-text;
   `.handoffNote`→secondary-text; `.error`→error-text. `.primaryButton` report-issue background removed;
   selector KEPT (now `min-width:180px`, non-empty so stylelint's block-no-empty is satisfied) so CAT-F's
   `styles.primaryButton` reference (SyncingConnectingMithrilPrompt.tsx:178/210) stays valid. Positioning
   judgment `top:0→84px` (STEP 4) is the sanctioned anti-header-clip tweak for the app-level persist mount.
2. **Confirmation modal (STEP 5):** `...Behind`→`--theme-dialog-title-color`, `...Body` gains
   `--theme-dialog-text-color`, `...Recovery` restyled IN PLACE (dialog-text, font-regular, 14px, lh 1.5,
   margin 16px 0 0, opacity 0.7) — STANDARD `--theme-dialog-*` tokens per the deliberate scoped amendment
   (mithril card/white tokens would re-introduce white-on-white on the white modal). Class contract matches
   CAT-C verbatim: subject=`...Behind`, subtext=`...Recovery` (no `...Subtext`, no rename).
3. **Dead-code (STEP 6):** only `...ConfirmationSteps`, `...ConfirmationTitle`, bare `...Confirmation`
   deleted — confirmed genuinely orphaned (no `styles.*` reference anywhere in `source/renderer`).
   `...ConfirmationRecovery` KEPT and restyled (live subtext CAT-C renders at .tsx:102). Regenerated
   `DaedalusDiagnostics.scss.d.ts` drops the 3 and keeps Body/Behind/Recovery/Dialog → tsc stays consistent.
4. **Cancelled/shared (STEP 7/8):** `.errorHint`→`--theme-mithril-body-text-color`; CAT-E's `.hintBody`
   rule and `.actions: flex-end` PRESERVED (not reverted). `MithrilDecisionView.scss .actions`→`flex-end`
   (optional review-gated parity) applied.

Token integrity (highest-risk CAT-D check): all 9 referenced CSS variables
(`--theme-mithril-{card-background,panel-border-color,card-shadow,card-text-color,body-text-color,
secondary-text-color,error-text-color}`, `--theme-dialog-{title,text}-color`) exist in all 10 theme/util
`.ts` files — no dangling/empty custom property. `.scss.d.ts` sidecars regenerated and carry every
load-bearing class (`primaryButton`, `actionButton`, `hintBody`, `errorHint`). stylelint on the 4 files +
repo-wide: clean.

Locked-invariant + cross-category sweep:
- **#11 bootstrap:** CAT-D's shared-component edits (`MithrilErrorView.errorHint` brighten + `Mithril
  DecisionView/MithrilErrorView .actions: flex-end`) reach the bootstrap `MithrilBootstrap` consumers, but
  are **visual-only** (color token + `justify-content`) — no structural/behavioral change. Approved under
  the review gate. The 10-theme + bootstrap-story visual walk remains the operator's separate validation
  gate (interaction mode) and is not a code blocker.
- **CAT-A (note):** `isBehindnessKnown` delegates to `isMithrilBehindnessKnown` = `Number.isFinite(localTip
  ?.epoch) && Number.isFinite(networkTip?.epoch)` — pure presence, no renderer threshold. Honors #4.
- **CAT-E (note):** cancel-path arrays trimmed to `['retry','restart-normal']` (`:301`,`:311`); active-sync
  failure (`:552`) and post-cutover (`:241`,`:549`) keep `wipe-and-full-sync`; `mithrilPartialSyncNode
  Startup.ts` untouched; renderer still strict-from-list. Confined per D-702a-2.
- **CAT-G (note):** `MithrilPartialSyncOverlay` auto-fires `onDismissCompleted` (finalize) via 4000ms
  `setTimeout` gated on `status==='completed'`; "Continue to Daedalus" removed; finalize cleanup semantics
  unchanged (backend path untouched). Honors D-702a-1.
- **Shared i18n id (note):** `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` defined once in
  `MithrilSyncProcessSummary.messages.ts`, imported by both CAT-C confirmation and CAT-F prompt; EN+JA both
  present (en/ja:173). No duplicate-id extraction conflict.
- **Vocabulary:** scss carries no user copy; sole "MithrilPartialSync" occurrence is a code-identifier
  reference in a comment (allowed). CAT-D adds/changes no i18n, so EN+JA parity is N/A for this category.
- **Out-of-scope artifact (note):** the `.gitignore` `+.devcontainer` line is an env artifact, not a CAT-D
  edit; harmless, no action.

**Blockers:** none.

**Decision:** APPROVED. CAT-D meets all acceptance criteria, honors every locked invariant, and the
sanctioned shared-component visual edits do not regress bootstrap behavior. Remaining work is the operator's
cross-theme Storybook visual walk (human gate per interaction mode), not a code-review blocker.

---

## Implementation

Timestamp: 2026-06-29T19:32:07Z
Speaker: Implementation
Category: CAT-H (Storybook fidelity & reorg — ISSUE-10, ISSUE-11, ISSUE-12)

Summary:
Executed the CAT-H ordered steps on the shared working tree. Storybook-only: zero app
code/IPC/store/.scss/i18n edits. Verified every seam against live (post-CAT-B/CAT-F) code before
editing.

1. STEP 0 (read-first, re-verify post-CAT-B/CAT-F): confirmed the proactive-prompt prop signature
   is still `{ behindByEpochs?: number; onStart: () => Promise<void>; onDismiss: () => void }`
   (`SyncingConnectingMithrilPrompt.tsx:67-71`), so the moved story args (`proactivePromptBaseProps`
   with the Promise-returning `onStart`, `behindByEpochs={120}`) need no change. Confirmed the
   `MithrilStepIndicator.showBars` anchor is still id `step-3` (`DOWNLOAD_PROGRESS_ANCHOR_ID`,
   `MithrilStepIndicator.tsx:99,612-621`) and that `MithrilPartialSyncOverlay` passes
   `showDownloadProgressBar` + `transferProgress` through to it (`:167-172,212`). Confirmed the
   `MithrilPartialSyncRecommendation` component still exists for a clean delete.
2. Download-bar fix (STEP 2, `MithrilPartialSyncOverlay.stories.tsx`): replaced
   `getDownloadingProgressItems` with a 3-item array (`step-1`/`step-2` `completed`, `step-3`
   `active`, each `as const`). Root cause: the old fixture's active sub-item id was `download`,
   which is NOT the `step-3` anchor, so `showBars`'s anchor check (`activeSubItemId === step-3`)
   failed and `InlineProgressBar` was silently dropped. Labels still pulled from `intl` at render
   (`progressDiskCheck`/`progressCertificateChain`/`progressDownloadingSnapshot`).
3. New explicit partial-progress story (STEP 3): added
   `.add('Download Progress Bar (Partial)', ...)` after "Downloading File Count" with
   `status="downloading" filesDownloaded={6} filesTotal={9}` — the combined `InlineProgressBar`
   at ~63% (fallback snapshot weight) with the file-count snapshot counter.
4. Rename Status→Diagnostic (STEP 4, `Diagnostics.stories.tsx`): `storiesOf('Nodes / Status')` →
   `storiesOf('Nodes / Diagnostic')` and the confirmation sub-kind →
   `'Nodes / Diagnostic / Mithril Partial Sync Confirmation'`. File + `status/` dir unchanged.
5. Removed redundant only-text Recommendation stories (STEP 5): deleted the
   `'Nodes / Status / Mithril Partial Sync Recommendation'` block + the now-unused
   `MithrilPartialSyncRecommendation` import + `recommendationBaseProps`. JUDGMENT (flagged for
   reviewer per plan): real recommendation UI is preserved by the full-page `DaedalusDiagnostics`
   CTA stories (post-CAT-B the recommendation renders as the tooltip-on-button), so re-rendering the
   bare section here would only reproduce the text-block artifact.
6. Moved the dialogue view under Mithril (STEP 6): created
   `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` under kind
   `'Loading / Mithril / Mithril Partial Sync Dialogue'`, moving the proactive-prompt stories
   (Known Epochs Behind `behindByEpochs={120}`, Unknown Behind) with `proactivePromptBaseProps`
   copied verbatim; `StoryDecorator` only (full styled dialogue, not a frame).
7. Removed the moved block from `Diagnostics.stories.tsx` (STEP 7): deleted the
   `'Nodes / Status / Mithril Proactive Prompt'` block + the now-unused
   `SyncingConnectingMithrilPrompt` import + `proactivePromptBaseProps`.
8. Registered the new file (STEP 8): added `import './MithrilPartialSyncDialogue.stories';` to
   `storybook/stories/loading/mithril/index.ts`.
9. Icon investigation (STEP 9, report-only): documented the root cause in
   `task-ux-702a-research.md` — the folder vs component glyph is stock `@storybook/react@6.4.22`
   behavior (any kind segment that is a parent of a deeper kind gets the folder glyph), there is no
   per-story icon API in `storiesOf` and no sidebar/icon config in `storybook/main.ts` nor any
   `.storybook/manager.*`. The reorg deliberately keeps nested parent kinds (NODES→Diagnostic,
   LOADING→Mithril→Dialogue), so the parent folder glyphs are inherent; no trivial fix preserves the
   requested grouping. No code change.
10. Theme/contrast coordination (STEP 10): no `.scss`/token edit (CAT-D owns theming). Cross-theme
    Storybook visual spot-check of the touched stories is the deferred operator-visual gate.

Locked-invariant adherence: touches ZERO app code/IPC/store/.scss/i18n — cannot regress
backend-authoritative behavior. #11 honored (no bootstrap component/story modified; the download-bar
fix reuses the same `step-3` anchor convention the bootstrap preset already uses —
`mithrilFixtures.ts` `download-early`/`download-mid` presets). #5/#9/#16 untouched (recovery wiring,
Completed dismiss semantics are CAT-E/CAT-G). Locked vocabulary honored: no user copy added; the new
kind label "Mithril Partial Sync Dialogue" and the retained "Partial Sync Overlay"/code-identifier
"MithrilPartialSync*" are sanctioned (code identifiers + Storybook kind labels only). The two
sanctioned ADR amendments (D-702a-1/-2) live on CAT-G/CAT-E surfaces, not touched here.

Files (tracked):
- storybook/stories/nodes/status/Diagnostics.stories.tsx
- storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx
- storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx (new)
- storybook/stories/loading/mithril/index.ts
- .agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-702a-research.md (icon root cause)

Verification:
- `yarn eslint --ext .ts,.tsx` on the 4 touched story/index files: 0 errors (only the PRE-EXISTING
  `Diagnostics.stories.tsx:74` `as any` `no-explicit-any` warning on `baseProps`, untouched by CAT-H).
- `yarn compile` (tsc --noEmit; storybook is in tsconfig scope — only `node_modules` excluded;
  precompile regenerated `.scss.d.ts`): clean, exit 0 (Done in 19.84s).
- `yarn storybook:build` (authoritative gate — bundles every stories module incl. the new dialogue
  file): SUCCESS — "Output directory: dist/storybook", Done in 73.54s (only webpack asset-size WARNs,
  no errors).
- `yarn test:jest MithrilPartialSyncOverlay.spec.tsx` (existing spec that protects the `step-3`-active
  download-bar fixture shape — NOT modified per plan): 12/12 PASS.
- Node v24 env: scss-typing is not a factor (CAT-H edits no scss); `.scss.d.ts` regenerated by the
  compile precompile regardless. No regression masked by env quirks.

---

## Code Review — CAT-H (Storybook fidelity & reorg — ISSUE-10 / ISSUE-11 / ISSUE-12)

Speaker: Code Review
Timestamp: 2026-06-29T19:43:00Z
Category: CAT-H (storybook-only; no app code / IPC / store / .scss / i18n)

Decision: APPROVED — no blockers.

Scope reviewed: the CAT-H working-tree diff — `storybook/stories/nodes/status/Diagnostics.stories.tsx`,
`storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`,
`storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` (new),
`storybook/stories/loading/mithril/index.ts`, plus the icon root-cause note in
`task-ux-702a-research.md`. The CAT-E `canWipeAndFullSync` removal from the Cancelled story (same
overlay-stories file) is CAT-E's already-reviewed edit, not a CAT-H concern. One broad adversarial pass.

Acceptance criteria — all met:
- Sidebar reorg: `Diagnostics.stories.tsx` renames `storiesOf('Nodes / Status')` → `'Nodes / Diagnostic'`
  and the confirmation sub-kind → `'Nodes / Diagnostic / Mithril Partial Sync Confirmation'`; file +
  `status/` dir unchanged (verified). The new `MithrilPartialSyncDialogue.stories.tsx` registers kind
  `'Loading / Mithril / Mithril Partial Sync Dialogue'` with the two moved prompt stories (Known Epochs
  Behind `behindByEpochs={120}`, Unknown Behind), `StoryDecorator` only (full styled dialogue, no frame) —
  so the prompt renders as real UI, not a bare text block. `proactivePromptBaseProps` copied verbatim
  (Promise-returning `onStart`, `onDismiss`).
- Download-bar fix (ISSUE-12): `getDownloadingProgressItems` rewritten to a 3-item array with
  `step-1`/`step-2` `completed` and `step-3` `active` (each `as const`). Verified `step-3` IS
  `MithrilStepIndicator.DOWNLOAD_PROGRESS_ANCHOR_ID` (`MithrilStepIndicator.tsx:99`), and `showBars`
  (`:612-621`) fires when `activeSubItemId === DOWNLOAD_PROGRESS_ANCHOR_ID` — the old `download` id matched
  neither the anchor nor `stepId`/null, so the bar was silently dropped; the fix is causally correct. The
  three labels (`progressDiskCheck`/`progressCertificateChain`/`progressDownloadingSnapshot`) exist and are
  pulled from `intl` at render. New `.add('Download Progress Bar (Partial)', …)` story (`filesDownloaded=6`,
  `filesTotal=9`) added after "Downloading File Count" — both render the combined `InlineProgressBar` at a
  partial fill with the snapshot file counter, as required.
- Only-text views (ISSUE-11): the redundant `'Nodes / Status / Mithril Partial Sync Recommendation'` block
  + the now-unused `MithrilPartialSyncRecommendation` import + `recommendationBaseProps` are removed; real
  recommendation UI is preserved by the full-page `DaedalusDiagnostics` CTA stories (post-CAT-B the
  recommendation renders as the tooltip-on-button). This is the plan-sanctioned JUDGMENT (plan :855) —
  signed off here: re-rendering the bare section would only reproduce the text-block artifact.
- Icon investigation (ISSUE-12 side-q): root cause documented report-only in `task-ux-702a-research.md`
  (`## CAT-H` block) — folder vs component glyph is stock `@storybook/react@6.4.22` behaviour (any kind
  segment that parents a deeper kind gets the folder glyph; no per-story icon API in `storiesOf`, no
  sidebar/icon config). The reorg deliberately keeps nested parent kinds, so the folder glyphs are inherent;
  no trivial fix preserves the grouping. No code change — correct.
- STEP 0 contract re-verified independently: `SyncingConnectingMithrilPrompt` Props is still
  `{ behindByEpochs?: number; onStart: () => Promise<void>; onDismiss: () => void }`
  (`SyncingConnectingMithrilPrompt.tsx:67-71`), so the moved story args need no adjustment.

Locked invariants honored:
- Storybook-only, zero blast radius: `git status` confirms the only CAT-H-owned edits are the four
  storybook files (+ the research/impl-review docs). No app code, IPC, store, `.scss`, or i18n touched —
  cannot regress backend-authoritative behaviour.
- #11 bootstrap not regressed: no bootstrap component or bootstrap-story file modified
  (`MithrilDecisionView`/`MithrilProgressView`/`MithrilErrorView` stories untouched); the download-bar fix
  reuses the same `step-3` anchor convention the bootstrap presets already use. The existing
  `MithrilPartialSyncOverlay.spec.tsx` download fixture is `step-3` active (`:190`), consistent with the
  new story fixture — the spec was NOT modified by CAT-H (its edits are CAT-E/CAT-G).
- Cross-category boundaries unaffected and confirmed out of CAT-H's files: CAT-E pre-cutover cancel-array
  wipe-trim (renderer still strict-from-list), CAT-G finalize (reset + staging-removal + marker-clear +
  folder-deletion), CAT-A no-renderer-threshold — none are storybook surfaces; #5/#9/#16 dismiss semantics
  are CAT-E/CAT-G's, untouched here.
- Vocabulary: clean. No user-facing copy added (story labels pull existing component i18n at render).
  "Mithril Partial Sync Dialogue"/"Partial Sync Overlay"/"Mithril Partial Sync Confirmation" are Storybook
  kind labels, and "MithrilPartialSync*" are code identifiers — both explicitly sanctioned. `behindByEpochs`
  is the epochs-only figure (no %/immutable). No EN/JA change required or made (CAT-H is out of the i18n
  serialization chain) — correct. No new/duplicate i18n id (CAT-C's shared id untouched).
- Tests meaningful: no new spec files is correct (no storyshots harness); the existing overlay spec already
  guards the `step-3`-active fixture shape and is left unmodified per plan.

Verification re-run by reviewer (independent):
- `yarn storybook:build` (authoritative gate — bundles every stories module incl. the new dialogue file):
  SUCCESS, exit 0, "Output directory: dist/storybook, Done in 90.39s" (only webpack asset-size WARNs, no
  errors). Confirms the rename, the new registered file, and the rewritten fixtures all bundle.
- `eslint` on the four touched files: 0 errors; the lone warning is the pre-existing
  `Diagnostics.stories.tsx:74` `as any` on `baseProps`, untouched by CAT-H. No unused imports
  (`MithrilPartialSyncRecommendation`/`SyncingConnectingMithrilPrompt`/`recommendationBaseProps`/
  `proactivePromptBaseProps` all cleanly removed; remaining helpers `getActiveProgressItems`/
  `getStoppingProgressItems`/`getDownloadingProgressItems` all still referenced).

Notes (non-blocking; cross-category / informational):
- The new "Download Progress Bar (Partial)" and the existing "Downloading File Count" stories are both
  `status="downloading"` with file counts — intentional per plan acceptance (both must render the combined
  bar); not a duplicate to prune.
- Theme/contrast spot-check across the 9/10 theme tabs (plan STEP 10) is operator-visual and deferred to
  the CAT-D/CAT-H visual gate — outside what a code pass can assert; flagged, not a blocker.

Blockers: none.
Decision: APPROVED.

---

## Verification (whole-task)

Timestamp: 2026-06-29T19:50:53Z
Speaker: Verification
Category: ALL (CAT-A…CAT-H — whole-task task-ux-702a gate)

Scope: whole-task verification after all 8 categories landed and were per-category code-review APPROVED.
Ran, in order, on the shared working tree (Node v24): `yarn typedef:sass` → `yarn compile` (full tsc) →
`yarn lint` → `yarn stylelint` → `yarn test:jest` scoped to every mithril/partial-sync suite + the changed
non-mithril specs (`DaedalusDiagnostics.spec.tsx`, `App.spec.tsx`) found via `git status`. Regenerated the
`.scss.d.ts` up front per the documented Node-v24 env workflow so no stale-scss-types quirk could masquerade
as a regression.

Command results:
- `yarn typedef:sass`: CLEAN, exit 0 ("Done in 6.25s") — regenerated all `.scss.d.ts` incl. the CAT-D/E
  surfaces (`DaedalusDiagnostics`, `MithrilErrorView`, `MithrilDecisionView`, `SyncingConnectingMithrilPrompt`).
- `yarn compile` (`tsc --noEmit`, precompile re-ran typedef:sass): PASS, exit 0 ("Done in 22.52s"). No type
  errors across the full renderer+main tree.
- `yarn lint` (eslint): PASS, exit 0 ("Done in 43.05s"). 0 errors; 5467 warnings, all pre-existing
  (`@ts-ignore`/`no-explicit-any`/`no-empty-function` across storybook + untouched files — same classes the
  per-category logs already classified as pre-existing). No new lint error attributable to 702a.
- `yarn stylelint` (`**/*.scss`): CLEAN, exit 0 ("Done in 1.42s"). The `SyncingConnectingMithrilPrompt.scss:9`
  `order/properties-alphabetical-order` trip flagged as pre-existing in the CAT-E log is now resolved (CAT-D/F
  owned that scss surface) — full-glob stylelint is green.
- `yarn test:jest "[Mm]ithril" "DaedalusDiagnostics.spec" "app/App.spec"`: PASS, exit 0. Test Suites: 59
  passed / 59 total; Tests: 610 passed / 610 total; 0 failures. Coverage includes all 23 mithril/partial-sync
  suites (main: `MithrilPartialSyncService`, `mithrilPartialSyncNodeStartup`, `mithrilPartialSyncMarker`,
  `mithrilSnapshotMetadata`, `mithrilNetworkConfig`, `mithrilErrors`, `mithrilCommandRunner`,
  `MithrilBootstrapService`(+`.install`), `mithrilPartialSyncChannel`, `mithrilBootstrapChannel`; renderer:
  `MithrilPartialSyncOverlay`, `MithrilProgressView`, `MithrilStepIndicator`, `MithrilBootstrap`,
  `partialSyncErrorCopy`, `SyncingConnectingMithrilPrompt`, `MithrilPartialSyncConfirmation`,
  `MithrilPartialSyncSection`, `MithrilProactivePromptContainer`, `MithrilBootstrapStore`,
  `MithrilPartialSyncStore`, `mithrilBehindness`) plus the changed `DaedalusDiagnostics.spec.tsx` and
  `App.spec.tsx` (overlay/app-mount path) — all PASS. The broad testPathPattern also pulled adjacent
  chain-storage/wallet suites into the run; those are green too.

Real-vs-env classification:
- REAL regressions introduced by task-ux-702a: NONE. Every gate is green (4×exit 0 + jest 610/610).
- Documented Node-v24 scss-types/identity-obj-proxy env quirk: NOT encountered this run. The known quirk is a
  stale/missing `.scss.d.ts` (tsc) or unmapped scss module (jest); it was pre-empted by running
  `yarn typedef:sass` first (and `yarn compile`'s precompile re-runs it). identity-obj-proxy + typed-scss-modules
  confirmed present in `node_modules`. No tsc/jest failure traced to scss types or the identity-obj-proxy
  sidecar — so nothing required the "regen + re-run before blaming code" fallback.
- Pre-existing (NOT 702a, NOT the scss/jest env quirk): the 5467 eslint warnings are repo-baseline
  `@ts-ignore`/`no-explicit-any`/`no-empty-function`; the per-category prettier 2.1.2-vs-CI mismatch noted in
  the CAT-A/CAT-B logs is a separate formatting-env mismatch on untouched lines and is out of this gate's
  scope (prettier is not one of the five whole-task gates).

Verdict: GREEN. compile PASS, lint PASS, stylelint PASS, jest 59 suites/610 tests PASS. realRegressions: none.

---

## Scribe (finalization)

Timestamp: 2026-06-29T19:51:40Z
Speaker: Scribe
Category: ALL (task-ux-702a close-out)

task-ux-702a is **completed** (completedAt 2026-06-29). All 8 categories (CAT-A…CAT-H) landed sequentially
on the shared working tree in the collision-safe order `CAT-A → CAT-C → CAT-B → CAT-E → CAT-F → CAT-G →
CAT-D → CAT-H`; every per-category code-review pass above returned **APPROVED with zero blockers**, and the
whole-task verification entry is **GREEN** (`yarn compile` / `yarn lint` / `yarn stylelint` each exit 0;
`yarn test:jest` 59 suites / 610 tests pass, 0 failures). The 12 manual-assessment issues are remediated.
The two sanctioned ADR amendments held with all other safety boundaries intact: **D-702a-1** auto-fires the
D9 finalize on the completed-overlay timeout (click → timeout trigger swap only; reset-to-idle + staging-
folder removal + marker clear byte-for-byte unchanged), and **D-702a-2** trims `wipe-and-full-sync` from the
two pre-cutover cancel arrays only (post-cutover wipe-only and the active-sync 3-action failure set
unchanged; renderer still strict-from-`allowedRecoveryActions`; cancel still hard-rejects post-cutover). The
shared **D-702a-6** canonical sentence is one i18n id consumed by both the confirmation modal and the
proactive prompt, and CAT-A's `isBehindnessKnown` computed is load-bearing in CAT-F's anti-flash prompt gate
(not orphaned). EN + JA were updated for every changed string (JA provisional per the 601 glossary) and the
`task-ux-702a-copy-overview.md` deliverable captured 18 strings across 5 surfaces for the manual Japanese
translator (D-702a-7).

Doc/JSON updates applied at close-out: the plan `task-ux-702a.md` Status set to `completed`
(completedAt 2026-06-29) with an outcome summary; `task-ux-702a-research.md` gained the durable CAT-A
first-load + CAT-F unmount root causes and the two ADR-amendment cleanup-preservation notes (CAT-A ISSUE-2
probe-delay and the CAT-H sidebar-icon root cause were already recorded); the tasks JSON gained a
`task-ux-702a` entry (phase 7, `interactive_validation`, deps `[task-ux-503, task-ux-601, task-ux-701]`,
status `completed`), `task-ux-702` now depends on `task-ux-702a`, and `metadata.totalTasks` /
`summary.totalTasks` were bumped 20 → 21 (JSON re-parsed clean; 21 tasks across phases).

Remaining human-gated items (NOT code blockers, carried to `task-ux-702`): the operator cross-theme
Storybook visual walk for CAT-D/CAT-H across all 10 themes; the manual Japanese-translator review of the
provisional JA via the copy-overview; and the OPTIONAL backend `getPartialSyncAvailability` probe-delay
decoupling flagged for the backend-correctness track. With 702a complete, the `task-ux-702` deployment QA
gate is unblocked.

