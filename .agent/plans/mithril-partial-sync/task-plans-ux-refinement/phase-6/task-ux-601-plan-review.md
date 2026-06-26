# task-ux-601 — Plan Review Log (append-only)

---

## Planner

Timestamp: 2026-06-26T17:27:50Z
Speaker: Planner (copy architect)

Summary:
Produced the complete, mechanically-applicable edit plan for the holistic EN+JA Mithril copy review
(PRD D10). Re-opened every target file and confirmed current strings/anchors (some had drifted; full
findings in task-ux-601-research.md).

Key decisions:
- Canonical names locked: EN "Mithril Sync" (plain "Mithril" in prose), JA "Mithril同期" (drop 部分,
  keep Latin brand token). Contrast term EN "standard sync" / JA 標準同期 (already established in the
  proactive prompt). Applied across BOTH diagnostics + overlay namespaces.
- D-A: deleted `...ConfirmationBehindSyncContext` (key + component message + render `<p>`) and collapsed
  `...RecommendationWithProgress` into the epochs-agnostic base `...Recommendation`. Verified the
  recommendation component has no `{epochs}` prop, so "reword epochs-only" is infeasible there →
  collapse-to-base is the smallest truthful change (recorded). Diagnostics TABLE-row sync-% untouched.
- D-C: renamed buttonLabel, sectionLabel, buttonHintReady; swept overlay strings. Caught
  `progress.nodeStartingDetail` (line 371) — NOT in the task's listed anchors but contains "Mithril
  partial sync"; included it so the ZERO-"partial sync" goal actually holds. Added a final grep gate.
- Lock #4 verified-data wording preserved verbatim (only name + normal→standard tokens swapped).
- Harmonized "normal sync(ing)" → "standard sync(ing)" at 159/167/169/371 (sync method); left "normal
  Daedalus app flow" and the restart-normal recovery-action idiom intact, with rationale.
- Traced `formattedSyncPercentage` + `isSynced` threading end-to-end (DaedalusDiagnostics → Section →
  Confirmation/Recommendation): both become dead in Section/Confirmation/Recommendation after the
  edits → remove the props + threading; both remain live in DaedalusDiagnostics for the table rows.
- Flagged the two spec files (Confirmation/Section) and the storybook fixtures/'Synced' story that
  assert the removed behaviour and must be updated — these were not in the task's anchor list.
- Verification plan leads with the `typed-scss-modules` regen of DaedalusDiagnostics.scss.d.ts (the
  .scss loses the BehindSyncContext class), then i18n:manage as the gate, compile, lint, stylelint,
  focused jest via the scss sidecar, storybook build, and a final grep gate.

Wrote: task-ux-601.md (canonical doc incl. full EN+JA copy table, component edits, verification plan,
"## Glossary (EN+JA)"), task-ux-601-research.md (grounding + line-anchored current state +
recommendation-component investigation), this log.

Decision: approved

---

## Critiquer

Timestamp: 2026-06-26T17:33:02Z
Speaker: Adversarial Critiquer

Reviewed the Planner output against the live files (en-US.json, ja-JP.json, the three diagnostics
components, DaedalusDiagnostics.tsx, both spec files, the scss, and the storybook stories). The core
copy/figure decisions are sound and D13/D-A/D-B intent is well understood, but I found concrete
omissions — two of which would ship an inconsistent JA string or break the jest gate.

What checks out:
- D-A: BehindSyncContext deleted from catalogs + component + render + scss; behind-ness becomes
  epochs-only in the confirmation; recommendation collapses to the epochs-agnostic base (no {epochs}
  prop exists on Recommendation, so collapse-to-base is the correct, smallest truthful change). The
  diagnostics TABLE-row sync-% (DaedalusDiagnostics.tsx getRow('syncPercentage') ~719 + compute ~569)
  and getRow('synced', isSynced) ~718 are correctly LEFT intact — not over-removed.
- Prop disposition verified end-to-end: formattedSyncPercentage is consumed ONLY by Confirmation's
  behindSyncContext <p> (108/148-152) and Recommendation's WithProgress {syncPercentage} (57/76-78);
  isSynced only selects the WithProgress variant (63-65). Both genuinely die in Section/Confirmation/
  Recommendation and stay live in DaedalusDiagnostics. No dangling required prop.
- Lock #4 verified-data phrase ("can restore verified chain data to help it catch up faster")
  preserved verbatim in Behind (159) and Recommendation (171); only name token + normal→standard.
- All EN "partial sync" runtime occurrences (157,158,159,160-del,163,167,169,170,171,172-del,173,351,
  353,358,359,366,371,376) and the JA equivalents are covered; 371 drift correctly caught.

BLOCKING findings:
1. JA line 165 (mithrilPartialSyncConfirmationRecovery) is a SHIPPED runtime string the Planner did
   not touch and it contains English-embedded "完全なMithril sync" ("...完全なMithril syncを実行など"),
   inconsistent with the new canonical and with JA line 369 which already reads "完全なMithril同期".
   This is exactly the "stray English-embedded JA" the review must catch. Harmonize 165 →
   "完全なMithril同期". (EN 165 stays "full Mithril sync" — distinct bootstrap concept — that's fine.)
2. MithrilPartialSyncSection.spec.tsx edit is under-specified. Beyond removing the two default-props
   keys, the spec asserts the deleted/renamed behaviour in MULTIPLE tests that will FAIL after the
   change: the WithProgress copy "Cardano node is currently 62.50% synced..." (~lines 67, 179, 259),
   "(62.50% synced)" (~line 239, gone with BehindSyncContext), the button name "Mithril Partial Sync"
   (~62, 245), and the heading "Before Mithril partial sync begins" (~248). The plan only mentions
   removing default props, so the focused-jest gate would fail.
3. MithrilPartialSyncConfirmation.spec.tsx edit is likewise incomplete: besides the two sync-% tests
   the Planner names, the heading assertion "Before Mithril partial sync begins" (line 33) and the
   confirm-button assertions "Start Mithril partial sync" (lines 98, 122, 133) must be updated to the
   new EN or those tests fail.
4. MithrilBootstrap.messages.ts (source defineMessages for the ENTIRE overlay namespace: title 282,
   nodeStartingDetail 302, completed.subtitle 322, failed.title 334, failed.hint 340, cancelled.title
   345, retry 417) is omitted from componentEdits. The Planner explicitly chose to update the
   diagnostics components' defaultMessage source strings "so regenerated defaultMessages.json stays
   truthful," but applied that to Confirmation/Recommendation/Section only. After i18n:extract the
   overlay defaultMessages will regenerate stale "!!!Mithril partial sync" wording into
   defaultMessages.json + translations/messages.json. Resolve the inconsistency: either also update
   MithrilBootstrap.messages.ts overlay defaultMessages, or explicitly scope reword defaultMessage
   edits out (and drop the diagnostics reword-defaultMessage edits too, keeping only the required KEY
   deletions). Note the Planner's own grep gate (#8) only greps en-US/ja-JP, so it would NOT catch
   this — extend it to defaultMessages.json + translations/messages.json.

Non-blocking suggestions:
- Section.tsx: also drop formattedSyncPercentage/isSynced from the render destructure (~117/119), not
  just Props, to avoid unused-var lint/tsc.
- EN lines 352/360 use lowercase "Mithril sync"; consider harmonizing to "Mithril Sync"/"Mithril" for
  capitalization consistency with the canonical (prose, minor).
- JA 171 "より速く追いつけるようにできます" is slightly wordy/machine-ish; "より速く追いつけます" reads more naturally.
- Confirmation.spec line ~143 startError fixture "Mithril partial sync is disabled..." is arbitrary
  passthrough test data (not i18n); fine to leave, optional tidy.

JA fidelity (changed strings): the new JA is natural and internally consistent — 標準同期 for "standard
sync", 検証済みのチェーンデータ for "verified chain data", 同期 vs 完全な…同期 disambiguation mirrors EN. No !!!,
no English fallbacks in the changed keys. The only JA defects are the un-touched stray at line 165
(blocking #1) and the minor wordiness at 171.

Decision: requires_changes

---

## Planner (revision)

Timestamp: 2026-06-26T17:39:12Z
Speaker: Planner

Revised the plan to resolve all four Critiquer blocking issues (verified each against the live files
before editing):

1. **JA 165 embedded-English (blocking #1).** Confirmed `ja-JP.json:165`
   (`mithrilPartialSyncConfirmationRecovery`) ships "...完全なMithril syncを実行など...", clashing with
   `ja-JP.json:369` "完全なMithril同期". Added a JA-only reword row (165 → 完全なMithril同期) plus a
   dedicated "JA embedded-English" harmonization section and a JA embedded-English grep in the gate.
   EN 165 untouched (keeps the distinct "full Mithril sync").

2. **Section.spec under-specified (blocking #2).** Verified the failing assertions in
   `MithrilPartialSyncSection.spec.tsx` (WithProgress copy 67/179/259, `(62.50% synced)` 239, button
   name 'Mithril Partial Sync' 49/62/76/98/135/161/188/245, heading 'Before Mithril partial sync begins'
   53/164/202/216/226/248, Start-button 77/100/136). Expanded the spec section with every line that must
   move to the new EN.

3. **Confirmation.spec incomplete (blocking #3).** Added the heading (line 33) and Start-button
   (98/122/133) assertion updates alongside the two sync-% tests.

4. **Overlay source-of-truth omitted (blocking #4).** Confirmed the file is
   `components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (not the path the Critiquer
   guessed). Added a full component-edit subsection updating the 9 overlay `defaultMessage` VALUES
   (282/302/322/334/340/345/351/376/417) to mirror new EN; `description:` fields stay as internal
   engineering metadata. Decision recorded: update ALL `defaultMessage` values (diagnostics + overlay)
   to keep the source-of-truth truthful. Extended the grep gate to scan `defaultMessages.json` +
   `translations/messages.json` (scoped to value lines, excluding `description`) and component
   `defaultMessage` strings.

Additional fixes found during verification (beyond the Critiquer list):
- **DaedalusDiagnostics.spec.tsx** also asserts old copy (WithProgress 81, buttonHintReady 86, button
  names, heading 180, behind lines 195/220) and was missing from the prior plan — added as a third spec.
- **MithrilPartialSyncOverlay.spec.tsx** asserts heading `/mithril partial sync/i` (69) and retry-button
  `/retry mithril partial sync/i` (89/151/200) — added as a fourth spec. Its no-`!!!` locale test is
  unaffected.
- Adopted Critiquer suggestions: Section.tsx render-destructure removal (~117/119); EN 352/360
  capitalization → "Mithril Sync" (with the documented "full Mithril sync" lowercase exception); JA 171
  wordiness trim ("より速く追いつけるようにできます" → "より速く追いつけます").
- Focused-jest verification step now lists all four spec files.

No figure-logic, IPC, or threshold changes; lock #4 verified-data wording preserved verbatim; D-A/D-B/D-C
honored; diagnostics TABLE-row sync-% untouched.

Decision: approved
