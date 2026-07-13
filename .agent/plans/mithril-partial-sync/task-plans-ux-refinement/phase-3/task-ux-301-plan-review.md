# task-ux-301 — Plan Review Log (append-only)

---

Planner:
Timestamp: 2026-06-24T15:31:00Z

Drafted the canonical plan for task-ux-301 (consume the availability read model, gate all
partial-sync UI on `isEnabled`, gate the recommendation/CTA additionally on `isSignificantlyBehind`,
and re-arm the CTA by deriving `isActionBlocked` from `isWorking` instead of `isActive`).

Approach: read the format template (phase-1/task-ux-101), all five live targetPath files end-to-end,
the store + component specs, PRD D1/D3/D9-BUG1, and research-19 (blocker #1, gap #40, rows #15/#24).
Traced the live wiring chain: `MithrilPartialSyncStore` → `DaedalusDiagnosticsDialog` (container,
`:133-137`) → `DaedalusDiagnostics` (presentational, `isMithrilActionBlocked` at `:565-566`, Section
render at `:704-711`) → `MithrilPartialSyncSection` → `MithrilPartialSyncRecommendation`.

Key decisions (no ambiguity left for the implementer):
- DD1: store exposes availability; container reads it and passes booleans down as props (keeps
  `DaedalusDiagnostics` a props-only `@observer`, consistent with current wiring + its spec).
- DD2: two-level gate — `isEnabled` gates the WHOLE `<MithrilPartialSyncSection>` (lock #10);
  `isEnabled && isSignificantlyBehind` threaded as `shouldShowRecommendation` gates the
  recommendation/CTA only. Confirmation-in-progress branch left ungated (reachable only via the gated
  button).
- DD3: change the partial-sync term in `isMithrilActionBlocked` from `isActive` → `isWorking`, fed
  from the container; RENAME the prop `isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking`
  end-to-end for honest naming; preserve the `|| isMithrilBootstrapActive` serialized-lock half (#14).
- DD4: periodic refresh — the partial-sync store has NO existing polling (push-driven via
  `onReceive`), so mirror `NetworkStatusStore`'s `setInterval`/`clearInterval` pattern: one-shot
  `request()` in `setup()`, plus a 30s interval that refreshes ONLY while `isWorking`, cleared in
  `teardown()`.

Convergence honored: NO new IPC channel (reuses `mithrilPartialSyncAvailabilityChannel`), NO
threshold math (`behindByImmutables` stored verbatim), no overlay dismiss/finalize wiring (404), no
proactive prompt (302), no confirmation-modal work (303), no bootstrap progress component touches.

Anchor drift recorded: tasks JSON + research-19 gap #40 cite `isMithrilPartialSyncActiveStatus` at
`mithril-partial-sync.types.ts:92-94`; LIVE location is `:98-100` (`isWorking` helper is `:86-88`,
matches the brief). The live file is authoritative; all plan anchors verified against live code as of
2026-06-24. Also confirmed the renderer availability wrapper is already exported
(`mithrilPartialSyncAvailabilityChannel`, `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:53-56`)
and that no proactive-prompt component exists yet (302 correctly out of scope).

`_isTornDown` guard (#15): documented — the new `_refreshAvailability`/`_applyAvailability` are both
guard-inert after teardown; the `onReceive` status subscription stays guard-inert with NO unsubscribe
added; the new `setInterval` IS cleared in `teardown()`.

Verification mapped to the 4 JSON testCases (a–d) across the store + `DaedalusDiagnostics` +
`MithrilPartialSyncSection` + container specs, with the exact mock updates enumerated so the existing
suites compile after the prop rename. Exact commands given: `yarn compile`; `eslint` on the five
touched files; `yarn test:jest` on the four specs.

Planning status set to `draft`; build status `in_progress`. Handing to critique.

---

Critiquer:
Timestamp: 2026-06-24T15:38:22Z

One broad review pass. Read the canonical plan, the format template (phase-1/task-ux-101), PRD D1
(`:32-95`)/D3 (`:130-164`)/D9-BUG1 (`:402-416`), research-19 blocker #1 (row 128, §2 `:115-120`),
rows #15 (`:142`) + #24 (`:151`), and spot-checked EVERY live anchor the plan pins.

Anchor verification (all EXACT matches against live code as of this review):
- Type `MithrilPartialSyncAvailability` at `mithril-partial-sync.types.ts:56-60`; `isMithrilPartialSyncWorkingStatus`
  at `:86-88`; `isMithrilPartialSyncActiveStatus` (`status !== 'idle'`) at `:98-100` (drift from the
  cited `:92-94` correctly recorded). ✓
- Renderer wrapper `mithrilPartialSyncAvailabilityChannel` exists at
  `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:53-56` with `.request()`. ✓ (Note: a sibling
  `mithrilPartialSyncFinalizeChannel` at `:58-61` already exists — 404 territory; plan correctly does
  not touch it.)
- Store: imports `:2-6`/`:14-20`, `_isTornDown` `:61`, `setup()` `:63-70`, `teardown()` `:72-75`,
  `_updateStatus` ends `:147`, `syncStatus`/`_updateStatus` pairing `:117-147`. ✓
- Container `DaedalusDiagnosticsDialog.tsx:133-137`. ✓
- `DaedalusDiagnostics.tsx`: Props `:409-411`, destructure `:519-520`, `isMithrilActionBlocked`
  `:565-566`, Section render `:704-711`. ✓
- `MithrilPartialSyncSection.tsx`: Props `:10-17`, `componentDidUpdate` `:43-51`, render destructure
  `:101`, `isShowingConfirmation` branch `:105-114`, Recommendation element `:122-127`. ✓
- `MithrilPartialSyncRecommendation.tsx`: Props `:42-47`, render `:54-93`, button `disabled` `:81`. ✓
- `NetworkStatusStore.ts:443-459` setInterval/clearInterval pattern is real and analogous. ✓
- Spec anchors: store spec mock `:15-34`, fake timers `:6`/`:51`; `DaedalusDiagnostics.spec.tsx`
  `defaultProps` `:52-54`, active-CTA test `:105-114`; `DaedalusDiagnosticsDialog.spec.ts` mock `:38-42`,
  `withStatus` `:89-99`. ✓

Coverage: D1 hide-not-disable, D3 kill-switch (gate ALL UI on `isEnabled`), D9/BUG1 `isWorking` re-arm
(renderer half only; correctly leaves backend reset-to-idle + dismiss→finalize to 404), blocker #1
renderer half, #15 `_isTornDown` documentation (Step 6 + impl-review note), #24 retry=start (Risks) —
all covered. Scope boundaries clean: no new IPC channel, no threshold math (`behindByImmutables` stored
verbatim), no 302 proactive prompt (confirmed no such component exists), no 303 copy/confirmation work,
no 404 overlay/finalize, no bootstrap-progress touches. Locks #4/#10/#14 honored. The 4 JSON testCases
are mapped; testCase (c) is sound (terminal `completed`/`cancelled`/`failed` are NOT in
`MITHRIL_PARTIAL_SYNC_WORKING_STATUSES`, so `isWorking` is false → CTA re-arms). Interaction mode
`autonomous` is correct — no hidden product/UX decision; the gating + derivation rules are pre-decided
by D1/D3/D9. The approach is concise and small-model-implementable (exact files/symbols/anchors,
quoted seams, 13 mechanical steps).

Blockers (consolidated):

1. [major] Residual partial-sync header row when `isEnabled && !isSignificantlyBehind`. The plan
   places the visibility gate INSIDE `MithrilPartialSyncRecommendation` (Step 13 returns `null` when
   `!shouldShowRecommendation`), but the enclosing `MithrilPartialSyncSection.render()` (`:116-129`)
   still emits the `styles.layoutRow` wrapper + the `styles.layoutHeader` "Mithril Partial Sync:"
   label. The Section is gated only on `isEnabled` (Step 11), NOT on `isSignificantlyBehind`. Result:
   when the kill switch is on but the user is NOT significantly behind, a bare labeled "Mithril Partial
   Sync:" row renders with empty content near the tip — a dead/empty partial-sync surface that
   contradicts D1's explicit "there is never a dead control near the tip" (`prd:87-88`) and the D1
   "no offer" intent. testCase (b)(ii) as written would still pass (it only asserts the button + copy
   are absent), so the bug ships silently.
   Fix: gate the Section's own `layoutRow`/header too — e.g. in `MithrilPartialSyncSection.render()`
   return `null` (or skip the `layoutRow`) when `!shouldShowRecommendation && !isShowingConfirmation`,
   since the Section's only non-confirmation content IS the recommendation. Then strengthen testCase
   (b)(ii) to also assert the "Mithril Partial Sync" header text is absent
   (`screen.queryByText('Mithril Partial Sync')` null) when `isEnabled && !isSignificantlyBehind`.

2. [minor] `MithrilPartialSyncSection.spec.tsx` mock update not enumerated. The plan's "Tests to
   add/update" pre-req list enumerates mock updates for `MithrilPartialSyncStore.spec.ts`,
   `DaedalusDiagnostics.spec.tsx`, and `DaedalusDiagnosticsDialog.spec.ts`, but omits
   `MithrilPartialSyncSection.spec.tsx`, which has `isMithrilPartialSyncActive: false` in `defaultProps`
   (`:19`), a bare `isMithrilPartialSyncActive` prop at `:162`, and lacks the now-required
   `shouldShowRecommendation`. After the rename (Step 12) this suite fails to compile/render until
   updated. `yarn compile`/`yarn test:jest` would catch it, but enumerate it explicitly for the
   small-model implementer: rename `:19`/`:162` to `isMithrilPartialSyncWorking` and add
   `shouldShowRecommendation: true` to `defaultProps` so the existing render tests keep passing.

Everything else is correct and buildable. Fix blocker #1 (and ideally #2) and re-submit.

Decision: requires_changes

---

Planner:
Timestamp: 2026-06-24T15:42:22Z

Single allowed fix pass. Re-read the two live components the blockers touch
(`MithrilPartialSyncSection.tsx`, `MithrilPartialSyncRecommendation.tsx`) and
`MithrilPartialSyncSection.spec.tsx` end-to-end before revising; confirmed every cited anchor.

Blocker #1 [major] — residual empty "Mithril Partial Sync:" header row when `isEnabled &&
!isSignificantlyBehind`. Confirmed against live code: `MithrilPartialSyncSection.render()` (`:116-129`)
emits `styles.layoutRow` + the `styles.layoutHeader` "Mithril Partial Sync" label (+ a SEPARATE colon
text node from `globalMessages.punctuationColon`, `:120`) BEFORE delegating to
`MithrilPartialSyncRecommendation`, so a `null` inside the recommendation (the old Step 13) would have
left the labeled row behind — a dead surface near the tip, contradicting D1 (`prd:87-88`).
FIX APPLIED: moved the gate UP to the Section. Step 12b now adds, AFTER the existing
`isShowingConfirmation` early-return and BEFORE the `layoutRow` JSX, `if (!shouldShowRecommendation) {
return null; }`, so the Section renders NOTHING (no header, no row) for its only non-confirmation
content when the gate is off. Step 13 changed to NO-CHANGE: `MithrilPartialSyncRecommendation.tsx` is
no longer edited (no new prop, no null return) — gating at the Section is sufficient and is the
smaller change (one null-return, no redundant second gate). Propagated the consequence to Scope item
4, "Files expected to change" (items 4/5 — Recommendation downgraded to READ-ONLY), DD2 (added an
explicit empty-row NOTE), and the D1 locked-invariant line. Strengthened testCase (b)(ii): now asserts
the header is absent via `screen.queryByText(/Mithril Partial Sync/)` === null (REGEX, not exact
string — the header's normalized text content is "Mithril Partial Sync:" because the colon is a
separate text node, so an exact-string match would never match the header even when present); the
Section-level assertion now checks the WHOLE Section renders nothing (no header role/text) with
`shouldShowRecommendation={false}`.

Blocker #2 [minor] — `MithrilPartialSyncSection.spec.tsx` mock update not enumerated. Confirmed:
`defaultProps` has `isMithrilPartialSyncActive: false` (`:19`) and a bare `isMithrilPartialSyncActive`
prop (`:162`), and lacks `shouldShowRecommendation`; six existing tests click the "Mithril Partial
Sync" button so they need the recommendation row visible.
FIX APPLIED: added `MithrilPartialSyncSection.spec.tsx` to the enumerated pre-req mock-update list —
rename `isMithrilPartialSyncActive` at `:19` and `:162` to `isMithrilPartialSyncWorking` and add
`shouldShowRecommendation: true` to `defaultProps` so the existing render tests keep passing. Also
updated the "Prop rename surface" risk to name all THREE specs that reference the renamed prop
(`DaedalusDiagnostics.spec.tsx`, `MithrilPartialSyncSection.spec.tsx`, `DaedalusDiagnosticsDialog.spec.ts`).

No scope creep: still NO new IPC, NO threshold math, NO 302/303/404 work. The fix shrank the source
surface (Recommendation now untouched). All other steps, locks, and the verification command set are
unchanged. Re-submitting for critique.
