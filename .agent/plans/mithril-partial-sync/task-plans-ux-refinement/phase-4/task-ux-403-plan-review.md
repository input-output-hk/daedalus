# task-ux-403 — Plan Review Log

Append-only. Newest entries at EOF.

---

**Planner:** 2026-06-26T11:31:11Z

Authored `task-ux-403.md` (planning status `in_review`, build `in_progress`, interaction mode `autonomous`) to the small-model-implementable bar: exact files, ordered mechanical steps, LIVE line anchors, exact symbol/type/i18n-key names, the resolver shape, inline locked invariants, and per-testCase verification commands. Mirrors the structure of the shipped `task-ux-402.md`.

**Committed implementation approach (renderer-only, COPY-only):**
1. **Pure resolver** `resolvePartialSyncErrorCopy(status, error) -> { title, hint }` in a NEW module `partialSyncErrorCopy.ts`, returning `react-intl` `MessageDescriptor`s (no `intl` dependency → trivially unit-testable; the overlay formats). Resolution order **cancelled → code → stage → generic failed**. `cancelled` short-circuits first so the user-stop case always gets the calmer copy; **code beats stage** because `PARTIAL_SYNC_STAGED_DB_INVALID` is emitted at both `verifying` and `installing`, so the code is the precise signal. The stage tier reuses the code descriptors (downloading→download, verifying/installing→staged-db, converting→conversion) so a code-less failed frame still gets relevant copy with no extra i18n keys.
2. **5 bespoke code entries** (10 new message keys) + **rewrite the byte-identical cancelled hint** (gap #32) + reuse the existing generic `partialSyncFailed*`/`partialSyncCancelledTitle` keys.
3. **Wire into `MithrilPartialSyncOverlay`** only — replace the binary title/hint at `MithrilPartialSyncOverlay.tsx:129-138` with `intl.formatMessage(errorCopy.title/hint)`. `MithrilErrorView` needs **no edit** (it already honors explicit title/hint, short-circuiting its bootstrap-keyed `ERROR_COPY_BY_STAGE`).
4. **Add `MithrilPartialSyncErrorCode` union** (the exact 5 LIVE strings) to `mithril-partial-sync.types.ts` so `COPY_BY_CODE` is completeness-checked.

**Live-code verification done:** all 5 codes + their emitted stages confirmed against `MithrilPartialSyncService.ts` (latest-drift :197-201 stage `preparing`; download :479-485 stage `downloading`/`verifying`; conversion :497-501 stage `converting`), `mithrilPartialSyncPreflight.ts:152` (no-certified-range, stage `preparing`), `mithrilPartialSyncStaging.ts` (staged-db-invalid, stage `verifying` AND `installing`). Gap #32 confirmed: `cancelled.hint` == `failed.hint` byte-identical in `MithrilBootstrap.messages.ts:340-341/350-353`, `en-US.json:351/353`, `ja-JP.json:351/353`. Overlay binary block confirmed at `MithrilPartialSyncOverlay.tsx:129-138`; `MithrilErrorView.tsx:88-94` honors explicit title/hint.

**PARTIAL_SYNC_ prefix discrepancy — RESOLVED in favor of LIVE.** Tasks JSON lists `DOWNLOAD_COMMAND_FAILED`/`CONVERSION_FAILED` without the prefix; the LIVE codes carry `PARTIAL_SYNC_`. The map is keyed on the exact LIVE strings (`PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED`, `PARTIAL_SYNC_CONVERSION_FAILED`). Recorded in `task-ux-403-research.md`.

**Locked invariants stated inline:** #18 (resolver maps structured code/stage → localized descriptors; primary copy is never `error.message`; the pre-existing secondary `<p>{error.message}</p>` is unchanged diagnostic surface, out of scope), #4 (verified-chain-data wording in the bespoke hints), #34 (one-sentence cause + one "how to continue" pointer), #17 (latest-drift = retriable "snapshot advanced while preparing; retry", not a silent reset), #5 (only title/hint change; `actions[]` from `allowedRecoveryActions` untouched; hints say "choose how to continue below" and never name a button except retry-for-latest-drift), no-bootstrap-regress (resolver called only by the overlay; `MithrilErrorView`/`ERROR_COPY_BY_STAGE` untouched).

**i18n process mirrors task-ux-402 exactly:** `defineMessages` `!!!` defaults → `yarn i18n:extract` (writes only `translations/messages.json`) → hand-edit `en-US.json`/`ja-JP.json` first-class (no `!!!`) → `yarn i18n:check` regenerates `defaultMessages.json` (stage it). First-class EN + JA provided for all 10 new keys and the rewritten cancelled hint.

**Interaction-mode classification: `autonomous`, `requiresUserDecision = false`.** This is functional failure/recovery copy fully governed by the locked wording rules (#4/#34/#18/#17) and PRD D5b/D5e/D5#17 — not benefit-framing marketing copy. No blocking decision exists. Two non-blocking residuals recorded as open questions: (a) the locked "Mithril sync" vs "partial sync" vocab note — new copy uses "Mithril sync", existing generic titles keep "partial sync" pending the holistic task-ux-601 pass; (b) gap #12's confirmation-surface start-rejection raw string is a different surface out of this task's targetPaths — closed here only for the overlay's primary copy. Neither gates the task.

**Tests planned:** new pure `partialSyncErrorCopy.spec.ts` (5-code `it.each`, unknown-code fallback, stage-tier, distinct cancelled, latest-drift retriable hint, no-raw-message) — runs WITHOUT the scss sidecar; plus 2 render tests in `MithrilPartialSyncOverlay.spec.tsx` (bespoke code copy + raw-message-not-the-heading; cancelled calmer hint distinct from failed). The existing placeholder-marker locale test auto-covers the new keys.

---

**Planner (resumed):** 2026-06-26T11:31:11Z

Re-validated `task-ux-403.md` against the small-model-implementable bar and re-verified every load-bearing LIVE anchor in the working tree; the plan is accurate and complete, so it is **kept as authored** (no rewrite, no material correction needed).

Re-verified LIVE:
- `MithrilPartialSyncOverlay.tsx:129-138` — the binary `status === 'cancelled' ? partialSyncCancelledTitle/Hint : partialSyncFailedTitle/Hint` block on `<MithrilErrorView>` is exactly as the plan pins it; this is the Step-4c replacement site. The overlay already carries `startedAt?`/`bootstrapStartedAt={startedAt}` from task-ux-402 (no conflict; the resolver wiring is orthogonal). It already imports `MithrilBootstrapMessages` and types `error?: MithrilPartialSyncError | null` (L24) — both prerequisites the resolver and Step-2 message additions rely on.
- `mithril-partial-sync.types.ts` — `MithrilPartialSyncErrorStage` ends L30, `MithrilPartialSyncError` is L32-37 with broad `code?: string` (L34). The Step-1 `MithrilPartialSyncErrorCode` union insertion point (after L30, before L32) and the "do NOT narrow `code`" instruction are correct.
- `MithrilBootstrap.messages.ts` — `partialSyncFailedHint` (L337-342) and `partialSyncCancelledHint` (L348-353) are byte-identical (gap #32 confirmed live); titles already differ (`…failed` L334 vs `…was cancelled` L345). Step 2a rewrites ONLY the cancelled hint; Step 2b adds the 10 new keys after it. Existing generic/recovery keys (L332-371) intact for resolver reuse.

Confirmed unchanged from the original Planner entry: resolution order (cancelled → code → stage → generic), code-beats-stage rationale (`PARTIAL_SYNC_STAGED_DB_INVALID` emitted at both `verifying` and `installing`), the PARTIAL_SYNC_ prefix discrepancy resolved in favor of LIVE codes, locks #4/#5/#17/#18/#34 stated inline, the i18n process mirroring task-ux-402, and the Node v24 env workarounds (this task touches NO scss, and the new `partialSyncErrorCopy.spec.ts` imports no scss so needs no jest sidecar). **Interaction mode reaffirmed: `autonomous`, `requiresUserDecision = false`** — no blocking decision the locked wording rules + PRD D5b/D5e/D5#17 cannot resolve.

Decision: approved

---

**Critiquer:** 2026-06-26T11:31:11Z

One broad adversarial pass over `task-ux-403.md` against the tasks-JSON `task-ux-403` object (acceptance/testCases/implementationNotes), PRD D5b/D5e/D5#17, research-19 gaps #6/#12/#17/#32, and the locked invariants. Re-verified every load-bearing anchor in the working tree. **The plan is correct, complete, and small-model-implementable — approved.** No blocking defects found.

**Independently re-verified LIVE (not just trusted):**
- **Latest-drift retry IS allowed (the one genuinely risky claim).** The plan's latest-drift hint is the ONLY hint that names a specific button ("Retry Mithril sync"), so lock #5 requires that `retry` actually be in `allowedRecoveryActions` for `PARTIAL_SYNC_LATEST_DRIFT`. Confirmed: `MithrilPartialSyncService.ts:548-560` `_deriveAllowedRecoveryActions` returns `['retry','restart-normal','wipe-and-full-sync']` for every stage NOT in `['installing','finalizing','starting-node']`; LATEST_DRIFT is thrown with stage `preparing` (`:197-201`), so retry renders. The plan's risk note is accurate and the hint does not over-promise. All other bespoke hints use generic "choose how to continue below" — lock #5 honored.
- **Gap #32 byte-identical confirmed** in both runtime catalogs: `en-US.json:351`==`:353`, `ja-JP.json:351`==`:353`, and `MithrilBootstrap.messages.ts:337-342`/`348-353`. The rewritten cancelled hint + the resolver's `cancelled` short-circuit make them distinct (D5e).
- **Overlay short-circuit + no-edit-to-MithrilErrorView confirmed.** `MithrilPartialSyncOverlay.tsx:129-138` is the exact binary block; `MithrilErrorView.tsx:88-94,113-117` renders the explicit `title` in an `<h1>` and `hint` in a `<div>`, only consulting the bootstrap-keyed `ERROR_COPY_BY_STAGE` when `hint===undefined`. The negative overlay assertion (`queryByRole('heading', {name:/mithril-client json/i})`) is safe: the raw `error.message` is rendered only in a `<p>` (`:116`) and inside `CollapsibleSection` (verified to emit no `<h*>`/`role=heading`), never a heading. The "ships polished" locale test (`MithrilPartialSyncOverlay.spec.tsx:149-160`) filters `loading.mithrilPartialSync.*`, so it auto-guards all 10 new `error.*` keys in EN **and** JA.
- **PARTIAL_SYNC_ prefix discrepancy** correctly reconciled to the LIVE codes and flagged for the research note.
- **Import path / types correctness:** the new `partialSyncErrorCopy.ts` sits in the same dir as the overlay, so the 5-level `../../../../../common/types/...` import matches the overlay's own; `Record<MithrilPartialSyncErrorCode, ...>` gives compile-time 5-code completeness; `MithrilPartialSyncError.code` stays the broad `code?: string` (`:34`) so the contract isn't narrowed.

**Gap/PRD coverage:** all 5 LIVE codes have bespoke EN+JA copy; LATEST_DRIFT is retriable (#17); cancelled is distinct and calmer (#32/D5e); the resolver returns localized descriptors only, so primary copy is never `error.message` (#18); the error-copy map lives in a pure, intl-free, unit-tested seam keyed code→stage→generic (gap #6). **Gap #12 is explicitly and defensibly scoped out:** the start-rejection raw fallback lives in `MithrilPartialSyncStore.toStartError` ("Unable to start Mithril partial sync.") surfaced via `MithrilPartialSyncSection.tsx` — a different surface, none of whose files are in this task's targetPaths — and #12 is closed here only for the overlay's primary copy. The tasks-JSON implementationNote naming #12 is in tension with the JSON targetPaths; the plan records this as a non-blocking open question, which the critique checklist permits.

**Minor, non-blocking polish (do NOT gate):**
1. **i18n interleaving hazard for a small model.** Step 5b/5c present the 10 new EN/JA strings as a *contiguous* block, but in the key-sorted catalogs `failed.hint`/`failed.title` must stay BETWEEN `downloadFailed.title` and `latestDrift.hint`, and `restartNormally`/`retry` BETWEEN `noCertifiedRange.title` and `stagedDbInvalid.hint`. A literal contiguous paste would misorder them. Mitigated by the plan's "alphabetical id order among the keys" instruction + `yarn i18n:check` normalization (the proven task-ux-402 mechanism) and the fact that order affects neither tsc/jest nor the polished-marker test — but an explicit "do not move the existing `failed.*`/`restartNormally`/`retry` keys; interleave around them" line would remove all doubt.
2. **Stage-tier coverage gap in the helper spec.** The spec exercises `converting→conversionFailed` but not `installing→stagedDbInvalid`; adding one `it` would fully pin the stage map. Optional.
3. **Trivial anchor drift:** the polished-marker test is `MithrilPartialSyncOverlay.spec.tsx:149-160` (plan says 149-159). Cosmetic.

Decision: approved

---
