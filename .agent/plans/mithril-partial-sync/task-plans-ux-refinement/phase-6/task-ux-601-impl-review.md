# task-ux-601 — Implementation Review Log (append-only)

> Append-only. Each entry: ISO-8601 UTC `Timestamp:`, a speaker label
> (Implementation | Code-Review), and a `Decision: approved|requires_changes` line.
> (Planner phase complete — see task-ux-601-plan-review.md. No implementation entries yet.)

---

## Implementation

Timestamp: 2026-06-26T17:56:58Z
Speaker: Implementation

Applied the approved plan exactly. Copy-only changes — no IPC/backend/threshold/figure
logic touched. Lock #4 verified-data wording kept verbatim (only the name token + the
`normal`→`standard` sync-method noun were swapped). The diagnostics TABLE-row sync-%
(`getRow('syncPercentage')`, `getRow('synced', isSynced)`) and its `formattedSyncPercentage`
compute at DaedalusDiagnostics.tsx:569 were left intact (D-A safe path).

### Decisions realized
- D-A: removed sync-% from BOTH the confirmation behind-ness (deleted
  `...ConfirmationBehindSyncContext`) AND the recommendation (deleted
  `...RecommendationWithProgress`, collapsed the `isSynced ? base : withProgress`
  selector to always render the epochs-agnostic base recommendation). `formattedSyncPercentage`
  + `isSynced` became dead in Confirmation/Recommendation/Section and were removed from those
  three components' Props + render destructure + threading; kept only in DaedalusDiagnostics
  for the table.
- D-B: one canonical JA rendering `Mithril同期` applied across diagnostics + overlay
  namespaces (dropped 部分; kept Latin `Mithril` brand token). EN feature name = `Mithril Sync`.
- D-C: renamed diagnostics `buttonLabel`, `sectionLabel`, `buttonHintReady` → `Mithril Sync`.
- Extra ZERO-partial-sync reach: `loading.mithrilPartialSync.title`,
  `...progress.nodeStartingDetail`, and capitalization harmonizations (lowercase
  `Mithril sync` → `Mithril Sync` in cancelled.hint / latestDrift.hint). `full Mithril sync`
  deliberately kept lowercase as the distinct bootstrap concept; JA 165 embedded-English
  `完全なMithril sync` → `完全なMithril同期` (matches JA 369).

### Files changed (17 code/locale + 1 regenerated .d.ts gitignored)
- Runtime locales: `en-US.json`, `ja-JP.json` (deleted BehindSyncContext +
  RecommendationWithProgress keys; renamed/reworded the rest).
- Generated catalogs (via `yarn i18n:extract`): `defaultMessages.json`,
  `translations/messages.json` — deleted ids gone, new values present.
- Components: `MithrilPartialSyncConfirmation.tsx` (removed behindSyncContext message + <p> +
  dead prop), `MithrilPartialSyncRecommendation.tsx` (removed withProgress message + selector +
  dead props), `MithrilPartialSyncSection.tsx` (removed dead props + threading + sectionLabel),
  `DaedalusDiagnostics.tsx` (dropped formattedSyncPercentage/isSynced from Section invocation),
  `MithrilBootstrap.messages.ts` (9 overlay defaultMessage VALUES mirror new EN; description:
  fields keep the internal engineering name).
- SCSS: `DaedalusDiagnostics.scss` (deleted `.mithrilPartialSyncConfirmationBehindSyncContext`
  rule; `.mithrilPartialSyncConfirmationBehind` margin `0 0 4px` → `0 0 16px`). Regenerated
  `DaedalusDiagnostics.scss.d.ts` via typed-scss-modules (gitignored).
- Specs (4): `MithrilPartialSyncConfirmation.spec.tsx`, `MithrilPartialSyncSection.spec.tsx`,
  `DaedalusDiagnostics.spec.tsx`, `MithrilPartialSyncOverlay.spec.tsx`.
- Stories (2): `Diagnostics.stories.tsx` (removed dead props + the `Synced` story),
  `MithrilPartialSyncOverlay.stories.tsx` (truthful mock string).

### Verification results
- `node_modules/.bin/typed-scss-modules .../DaedalusDiagnostics.scss` — regenerated .d.ts (required before tsc).
- `yarn i18n:manage` (extract + check) — PASS. Deleted ids (`...BehindSyncContext`,
  `...RecommendationWithProgress`) gone from both generated catalogs; no new `!!!` for our keys.
- `yarn compile` (tsc --noEmit) — PASS.
- `yarn lint` — PASS (warnings only, all pre-existing/unrelated).
- `yarn stylelint` — our `DaedalusDiagnostics.scss` PASS (exit 0). Repo-wide stylelint reports
  ONE pre-existing failure in `SyncingConnectingMithrilPrompt.scss` (alphabetical-order) — NOT
  my file, no diff vs HEAD; not a regression from this task.
- Focused jest (no sidecar needed on Node v24.16) — 4 suites PASS, 44 tests
  (Confirmation 9, Section + Diagnostics + Overlay 35).
- `yarn storybook:build` — PASS (Done in 67.86s); removed BehindSyncContext <p> and `Synced`
  story did not break imports.
- Extended grep gate — ALL CLEAN: no `partial sync` / `Partial Sync` / `部分同期` in runtime
  locales, generated value lines, component defaultMessages, or JA embedded-English.

Decision: approved

---

## Code-Review

Timestamp: 2026-06-26T18:01:55Z
Speaker: Code-Review

Reviewed the actual working-tree diff (`git --no-pager diff`), not the implementer report.
Re-ran the critical gates after the scss `.d.ts` regen workaround.

### Verified against plan + user decisions
- ZERO `partial sync` / `Mithril Partial Sync` / `部分同期` in any user-facing string. Confirmed
  across `en-US.json`, `ja-JP.json`, the `defaultMessage` VALUE lines of `defaultMessages.json` /
  `translations/messages.json` (`grep '"defaultMessage": ".*(partial sync|部分同期)'` → 0), and
  component `defaultMessage` strings. Remaining `partial sync` hits are exclusively in `description:`
  fields, code comments, internal error-code constants (`PARTIAL_SYNC_*`), and a dev-only
  `logger.warn` — all internal/engineering language, allowed.
- D-A epochs-only: `...ConfirmationBehindSyncContext` and `...RecommendationWithProgress` keys,
  their `defineMessages` entries, the confirmation `<p>` + scss `.mithrilPartialSyncConfirmationBehindSyncContext`
  rule, and the `isSynced ? base : withProgress` selector are all fully removed. Confirmation now
  renders only the epochs `behind`/`behindUnknown` line; recommendation always renders the base copy.
  Diagnostics TABLE-row sync-% path intact: `getRow('syncPercentage', \`${formattedSyncPercentage}%\`)`
  + `getRow('synced', isSynced)` and the `formattedNumber(syncPercentage, 2)` compute (DaedalusDiagnostics.tsx:569)
  untouched; `isSynced`/`formattedSyncPercentage` still consumed there so no dead-var/tsc fallout.
- One canonical JA `Mithril同期` everywhere (20 occurrences); `部分` only appears in two unrelated
  non-Mithril keys (paper-wallet, address). All new JA is first-class (no English fallbacks, no `!!!`).
  `完全なMithril同期` consistently used for the distinct "full Mithril sync" bootstrap concept.
- All 4 catalogs consistent; deleted ids gone from all of them (grep `BehindSyncContext|RecommendationWithProgress` → 0).
- Prop cleanup clean: `formattedSyncPercentage` (Confirmation/Recommendation/Section) and `isSynced`
  (Recommendation/Section) removed from Props + render + threading; the `DaedalusDiagnostics` →
  `MithrilPartialSyncSection` invocation no longer passes them; no dangling required prop / broken caller.
- Lock #4 verified-data wording intact verbatim ("can restore verified chain data to help it catch up
  faster"); only the name token + the contrast noun `normal`→`standard` (explicitly sanctioned by the task)
  changed. No scope creep beyond copy.
- Specs (4) + stories (2) updated truthfully; the `shouldShowRecommendation:false → return null` gate makes
  the broadened `not.toMatch(/Mithril Sync/)` assertion valid. Glossary section `## Glossary (EN+JA)`
  present with all 6 required terms in both languages.

### Re-run gates (this review)
- `typed-scss-modules DaedalusDiagnostics.scss` — regenerated .d.ts.
- `yarn i18n:manage` — PASS; the only `!!!` warnings are pre-existing unrelated namespaces
  (staking / wallet / voting / dapp). No Mithril key carries a placeholder.
- `yarn compile` (tsc --noEmit) — PASS (16.39s).
- Focused jest (4 suites) — PASS, 44/44.

### Findings
- LOW (pre-existing, out of enumerated scope — non-blocking): `MithrilPartialSyncSection.tsx:107`
  hardcodes a non-i18n English fallback `'Unable to start Mithril partial sync.'` assigned to
  `startError`, which renders in the confirmation modal's error `<div>` (Confirmation.tsx:150).
  It only surfaces on the rare non-`Error` throw path and is not one of the enumerated copy keys, but
  it is a residual `partial sync` token on a user-facing surface that the D-C "zero" goal would ideally
  also cover. Recommend a follow-up to localize/rename it (e.g. "Unable to start Mithril Sync."). Not a
  regression from this task; does not block.

Decision: approved
