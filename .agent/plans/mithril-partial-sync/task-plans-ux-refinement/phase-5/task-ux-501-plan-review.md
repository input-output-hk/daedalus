# task-ux-501 — Planning Review Log

Append-only chronological transcript. Each entry: speaker, ISO-8601 UTC timestamp, outcome.
Critiquer entries end with `Decision: approved | requires_changes`.

---

## Planner
Timestamp: <UTC>  (real clock unavailable in this environment — scribe to stamp the actual ISO-8601 UTC time)
Outcome: Plan drafted.

Verified grounding anchors A–H against live files (line numbers confirmed; see canonical doc's
"Verified live state" table):
- A/A': stale `buttonLabel` description confirmed at `defaultMessages.json:1927` and
  `MithrilPartialSyncRecommendation.tsx:25` → correct to a functional CTA description.
- B/C: hardcoded `Mithril Partial Sync` row label at `MithrilPartialSyncSection.tsx:136`; will add a
  `defineMessages` block (new key `mithrilPartialSyncSectionLabel`) and render via
  `intl.formatMessage`.
- Orphan: bare `mithrilPartialSyncButtonHint` has **zero** matches — already removed; only
  `...ButtonHintBlocked`/`...ButtonHintReady` exist (live + descriptions accurate). Recorded
  "no change needed".
- F: recovery copy reworded (EN+JA) to make explicit that retry/restart/wipe appear in the
  progress/error overlay **only on failure**; phrased as "available options" to respect the
  `allowedRecoveryActions` invariant.
- D/E: doc-only — `_isTornDown` guard (`MithrilPartialSyncStore.ts:72,90,140-142,150-152,196-198,
  212-214`) and `retry`=start reuse (`:238-241`) documented, no code change.
- G: ux-refinement phase-3 docs already cite the live 3-component trio; the actually-stale
  inline-`DaedalusDiagnostics.tsx` plans live in the original `task-plans/`, which the orchestrator
  prompt forbids editing. Reconciliation: re-baseline note lands in the phase-3 docs (already aligned)
  + the historical UX plan; `task-plans/` left immutable. Conflict + reconciliation to be recorded in
  the research note.
- H: `mithril-snapshot-ux.md` covers only empty-DB bootstrap; will add a brief partial-sync pointer
  section referencing the UX-refinement PRD (no duplication).

Discovered: `translations/messages.json` and `defaultMessages.json` are byte-identical except a
trailing newline (formatjs-extract output mirrored) — plan instructs in-place mirror edits to preserve
the no-trailing-newline and keep the diff minimal.

Plan written to the small-model-implementable bar (exact files, exact new key + EN/JA strings, exact
reworded confirmation copy EN+JA, explicit "no code change, doc only" for anchors D and E).
Awaiting Critiquer pass.

---

## Critiquer
Timestamp: 2026-06-26T13:30:12Z
Decision: requires_changes

Broad pass. The plan is well-grounded and well-scoped. Re-verified anchors against live files:
- A/A': stale `buttonLabel` description confirmed at `MithrilPartialSyncRecommendation.tsx:25`
  and `defaultMessages.json` — Step 1 fix correct.
- B: hardcoded `Mithril Partial Sync` row label confirmed at `MithrilPartialSyncSection.tsx:136`
  (inside `layoutHeader`, `intl` destructured at :114, current import `import { intlShape } from
  'react-intl'`) — Step 2 approach (add `defineMessages`, render `messages.sectionLabel`) correct.
- Orphan: bare `mithrilPartialSyncButtonHint` has zero matches; only `...ButtonHintBlocked`/
  `...ButtonHintReady` exist (live + accurate descriptions). "No change needed" confirmed.
- F: recovery descriptor confirmed at `MithrilPartialSyncConfirmation.tsx:68-74`.
- E: `retry`=start comment confirmed verbatim at `MithrilPartialSyncStore.ts:238-241`.
- Gap coverage complete: #20 (S1), #21 (S2), #25 (S3), #15-doc (S5), #24-doc (S6), #10 (S7),
  #27 (S8). Scope guard honored (no IPC/types/backend; no full copy audit; behind-ness keys and
  bootstrap untouched). Locked invariants safe — reworded recovery uses "available options",
  honoring the `allowedRecoveryActions` render contract.

BLOCKER (must fix before build):
1. EN reworded recovery copy is ungrammatical and mismatches the JA. "You don't choose now." is
   not correct English; the JA ("ここで選択する必要はありません" = "you don't *need* to choose
   here") carries the intended meaning. Fix the EN to "You don't need to choose now." (in both the
   source `defaultMessage` `!!!` string and the `en-US.json` runtime value). Since this is a
   copy-hygiene task whose acceptance criterion is clarity, the EN must be correct/unambiguous.

NON-BLOCKING NOTES (apply if cheap; do not expand scope):
2. Test-risk is overstated. The new row-label English text is unchanged ("Mithril Partial Sync"),
   `MithrilPartialSyncSection.spec.tsx` matches the *button* via `getByRole(... name: 'Mithril
   Partial Sync')` (from `buttonLabel`, not the row), and its negative assert at :190
   (`not.toMatch(/Mithril Partial Sync/)` when the recommendation is hidden) still passes because
   row + button disappear together. `MithrilPartialSyncConfirmation.spec.tsx` does not assert the
   recovery sentence. So no spec edits should actually be required — soften the Risk/Test sections
   from "update assertions" to "verify specs still pass (expected: no change needed)".
3. Confirm in Step 4 that `defaultMessages.json` is NOT regenerated by `yarn i18n:extract` (only
   `translations/messages.json` is) — the manual mirror in Step 4b is therefore necessary and the
   plan is correct; just make this explicit so the builder does not expect extract to touch it.
4. Pre-existing (do not fix here): the new section row label and the CTA button share identical
   EN text "Mithril Partial Sync". Acceptable; note it so task-ux-601 can disambiguate if desired.

Re-approval gate: fixing blocker #1 is sufficient. Notes 2-4 are optional polish.

---

## Planner (fix)
Timestamp: <UTC>  (real clock unavailable in this environment — scribe to stamp the actual ISO-8601 UTC time)
Outcome: Blocker resolved; cheap non-blocking notes applied. No scope change.

Single allowed fix pass on `task-ux-501.md`:
- BLOCKER #1 (fixed): reworded EN recovery copy from the ungrammatical "You don't choose now." to
  "You don't need to choose now." in **both** places the plan specifies the string — Step 3 source
  `defaultMessage` (`!!!`-prefixed, for `MithrilPartialSyncConfirmation.tsx`) and Step 4c
  `en-US.json` runtime value. Added an inline note that the corrected EN now matches the JA
  "ここで選択する必要はありません" (= "you don't *need* to choose here"). JA was already correct and
  is unchanged.
- Note 3 (applied): Step 4a now states explicitly that `yarn i18n:extract` rewrites **only**
  `translations/messages.json` and does **not** regenerate `defaultMessages.json`, so the Step 4b
  manual mirror is required (not optional).
- Note 2 (applied): softened the Risk and Verification/Test sections from "update assertions" to
  "verify specs still pass (expected: no change needed)", with the rationale (row English text
  unchanged; `Section.spec.tsx` matches the button via `getByRole` and its `:190` negative assert
  still holds; `Confirmation.spec.tsx` does not assert the recovery sentence).
- Note 4 (recorded, not fixed — out of scope): added a Risks line flagging that the new
  `mithrilPartialSyncSectionLabel` row label and the `buttonLabel` CTA share identical EN text
  "Mithril Partial Sync"; deferred to `task-ux-601`.

Scope unchanged: still hygiene/doc only — no new IPC channels, types, or backend logic; behind-ness
keys and bootstrap untouched; recovery copy still phrased as "available options" to honor the
`allowedRecoveryActions` render contract. Ready for build.
