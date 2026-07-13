# task-ux-403 — Implementation Review

## Implementation: 2026-06-26T11:31:11Z

Speaker: Implementer

### Summary
Added a partial-sync error-copy layer so each failure shows stage/code-specific copy, gave
`cancelled` distinct calmer copy than `failed`, and made `PARTIAL_SYNC_LATEST_DRIFT` a specific
retriable message. Renderer-only; changes **only** `title`/`hint` copy — which recovery actions render
is still driven strictly by `allowedRecoveryActions` (lock #5, untouched).

1. **Error-code union (contract).** Added `MithrilPartialSyncErrorCode` (the exact 5 LIVE code strings,
   incl. the `PARTIAL_SYNC_` prefix on `DOWNLOAD_COMMAND_FAILED`/`CONVERSION_FAILED`) to
   `source/common/types/mithril-partial-sync.types.ts`. `MithrilPartialSyncError.code` stays broad
   `code?: string`.
2. **Pure resolver.** New `partialSyncErrorCopy.ts` — `resolvePartialSyncErrorCopy(status, error)`
   returns `{ title, hint }` `MessageDescriptor`s. Resolution order: `cancelled` short-circuit → exact
   `error.code` (`COPY_BY_CODE`, 5 codes) → `error.stage` (`COPY_BY_STAGE`: downloading/verifying/
   converting/installing, reusing the code descriptors, no extra i18n keys) → generic `FAILED`. Code
   beats stage because `PARTIAL_SYNC_STAGED_DB_INVALID` is emitted at both `verifying` and `installing`.
   No `intl` dependency → trivially unit-testable.
3. **Bespoke copy (EN+JA, first-class).** 10 new messages (5 title + 5 hint) in
   `MithrilBootstrap.messages.ts` for the 5 codes; `latest-drift` hint is retriable ("Retry Mithril
   sync … your chain data was not changed", #17). Hints say "choose how to continue below" (generic),
   never naming a button that may not render — except latest-drift, which names retry (guaranteed
   allowed for that pre-cutover error).
4. **gap #32 fix.** Rewrote the byte-identical `partialSyncCancelledHint` default to calmer/distinct
   copy ("Mithril sync was stopped before it finished. Your existing chain data is unchanged — choose
   how to continue below."). Titles already differed; left them.
5. **Overlay wiring.** `MithrilPartialSyncOverlay.tsx` now computes
   `const errorCopy = resolvePartialSyncErrorCopy(status, error)` and passes
   `title={intl.formatMessage(errorCopy.title)}` / `hint={intl.formatMessage(errorCopy.hint)}` into
   `<MithrilErrorView>`, replacing the binary cancelled-vs-failed block. The `actions[]` array,
   `onWipeRetry`/`onDecline` (task-ux-404 owns removal), `error`, `onOpenExternalLink` are unchanged.
   `MithrilErrorView` needed **no edit** — its explicit title/hint short-circuit the bootstrap-keyed
   `ERROR_COPY_BY_STAGE` fallback, so the empty-DB bootstrap flow is untouched.
6. **i18n catalogs.** `yarn i18n:extract` → `translations/messages.json`; hand-edited `en-US.json` +
   `ja-JP.json` (changed cancelled hint + 10 first-class strings each, alphabetical id order, no `!!!`);
   `yarn i18n:check` regenerated `defaultMessages.json` (gained the 10 ids + changed cancelled default).
7. **Tests.** New pure-helper spec `partialSyncErrorCopy.spec.ts` (5-code map, latest-drift retriable
   hint, unknown→generic, stage tier, code-beats-stage, cancelled distinct+short-circuit, never returns
   `error.message`). Added 2 render tests to `MithrilPartialSyncOverlay.spec.tsx` (bespoke copy for a
   mapped code with the raw JSON never promoted to the level-1 title; cancelled calmer hint distinct
   from failed). The existing "no `!!!` placeholder markers" locale guard auto-covers the 10 new keys.

### Files changed
- `source/common/types/mithril-partial-sync.types.ts` — `MithrilPartialSyncErrorCode` union.
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` — NEW resolver.
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts` — NEW spec.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — resolver
  import + `errorCopy` + title/hint wiring.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` — 2 new
  render tests + a `react-polymorph/lib/components/Link` jest.mock stub (see deviation).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` — rewrote
  `partialSyncCancelledHint`; added 10 messages.
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json` — changed cancelled hint + 10 strings.
- `source/renderer/app/i18n/locales/defaultMessages.json`, `translations/messages.json` — regenerated.

### Verification commands and ACTUAL results
- `node_modules/.bin/tsc --noEmit -p .` → **EXIT 0** (PASS). No scss touched; no `.scss.d.ts` regen
  needed.
- `yarn i18n:extract` → Done in 6.47s; `translations/messages.json` gained the 10 ids (grep = 10).
- `yarn i18n:check` → Done in 0.86s; `defaultMessages.json` gained the 10 ids (grep -c = **10**) + the
  changed cancelled default.
- Focused jest (committed config — `partialSyncErrorCopy.spec.ts` imports no scss; the overlay spec uses
  role/text selectors so no identity-obj-proxy sidecar was needed):
  `partialSyncErrorCopy.spec.ts` + `MithrilPartialSyncOverlay.spec.tsx` → **2 suites / 21 tests PASS,
  EXIT 0**.
- Full `mithril-bootstrap/` suite (no-bootstrap-regression guard) → **5 suites / 56 tests PASS, EXIT 0**.
- `eslint` on the 6 touched source/spec files → **0 errors, 4 warnings** (all pre-existing patterns:
  the overlay's `arg`/`as any` props, and the spec's `as any` test-helper that mirrors the plan's exact
  helper signature).
- `prettier --check` on touched ts/tsx/json → **clean** (reformatted `partialSyncErrorCopy.ts` once for
  the generic-record line wrapping, then clean).
- gap #32 proof: `error.cancelled.hint` ≠ `error.failed.hint` in **both** en-US and ja-JP (verified by
  grep). No NEW `!!!` on any `mithrilPartialSync.error` key in either catalog.

### Deviations from the plan
- **Overlay spec needs a `react-polymorph/lib/components/Link` mock (forced, justified).** The plan's
  Step 6b render test passes a real `error.message` (`{"raw":"mithril-client json"}`), which makes
  `MithrilErrorView` render the collapsible technical-details section
  (`MithrilErrorView.tsx:128-153` via `CollapsibleSection`, which renders a react-polymorph `Link`).
  react-polymorph's `Link` resolves to `undefined` without a skin/theme context in this jest env and
  crashes the whole render ("Element type is invalid … Check the render method of `Link`"). No existing
  spec rendered an error WITH a message (the bootstrap spec only uses `error: null`). Mirrored the
  existing `DaedalusDiagnostics.spec.tsx:8` `jest.mock('react-polymorph/lib/components/PopOver', …)`
  pattern and stubbed `Link` to a plain `<a>`. This is test-env plumbing only; the component path is
  unchanged.
- **"raw message never a heading" assertion refined.** The raw `error.message` is rendered by the
  pre-existing `CollapsibleSection` as its `<h2>` detail header, so a blanket "no heading with the raw
  text" assertion is false. Refined to the task's actual intent: the **primary (level-1)** error
  heading is the bespoke localized title, and there is **no level-1 heading** containing the raw JSON —
  i.e. the raw string is never promoted to the primary error copy (lock #18). The raw message remaining
  as secondary diagnostic detail is pre-existing, shared-with-bootstrap surface, out of scope.
- No other deviations. `MithrilErrorView.tsx` was verified (no edit), and `onWipeRetry`/`onDecline`
  remain wired (task-ux-404 owns their removal).

### Locked-invariant check
- **#5 (actions strictly from `allowedRecoveryActions`):** the `actions[]` array
  (`canRetry`/`canRestartNormally`/`canWipeAndFullSync`) is byte-unchanged; only `title`/`hint` changed.
  The existing "renders recovery actions only from allowed backend actions" test stays green.
- **#18 (no raw JSON as primary copy):** the resolver always returns localized `MessageDescriptor`s;
  the helper spec asserts `title`/`hint` never contain `error.message`, and the overlay test asserts the
  raw JSON is never the level-1 title.
- **#17 (latest-drift retriable):** `PARTIAL_SYNC_LATEST_DRIFT` → bespoke retriable hint; not a reset.
- **#4 ("verified … chain data"):** staged-db-invalid and latest-drift hints use "verified … chain
  data"/"verified snapshot"; no synthetic behind-ness/sync-% figures.
- **#34 (keep copy simple):** one cause sentence + one "how to continue" pointer; no ancillary nuance.
- **No bootstrap regression:** resolver imported only by the overlay; `MithrilErrorView` +
  `ERROR_COPY_BY_STAGE` untouched; full `mithril-bootstrap/` suite green.

### Residuals (non-blocking, owned elsewhere)
- Vocabulary holistic "Mithril sync" rename across the existing generic titles/subtitle → task-ux-601.
- Confirmation-surface start-rejection raw string → different surface, not this task's targetPaths.
- Dead `onWipeRetry`/`onDecline` props → task-ux-404.

## Code Review: 2026-06-26T11:31:11Z

Speaker: Code-Reviewer (iteration 1, broad pass over the diff)

### What I reviewed
`git diff` + untracked new files for task-ux-403, against the approved plan
(`task-ux-403.md`) and the locked invariants. Re-ran every verification gate myself rather
than trusting the implementer's claims.

### Correctness — error-copy map
- **Keying:** `COPY_BY_CODE` (`partialSyncErrorCopy.ts:46-55`) is keyed on a
  `Record<MithrilPartialSyncErrorCode, …>`, so all 5 LIVE codes are mapped AND the union forces
  exhaustiveness — drop one and tsc fails. All 5 carry the `PARTIAL_SYNC_` prefix, matching the LIVE
  backend strings (verified the union in `mithril-partial-sync.types.ts:32-37`), NOT the un-prefixed
  tasks-JSON names. Discrepancy resolved in favor of live repo, as required.
- **Resolution order:** `cancelled` short-circuit → `error.code` → `error.stage` → generic `FAILED`
  (`:70-88`). Code beats stage — correct, since `PARTIAL_SYNC_STAGED_DB_INVALID` is emitted at both
  `verifying` and `installing`. Unknown code with no mapped stage → generic `FAILED`. The `cancelled`
  short-circuit precedes code/stage so a stray `error` riding along still yields calmer copy.
- **LATEST_DRIFT retriable (#17):** maps to a bespoke hint that says retry + "your chain data was not
  changed" — retriable, not a silent reset. It is the ONLY hint that names an action ("Retry Mithril
  sync"), which is sound because #17 guarantees retry is allowed for this pre-cutover error; every
  other hint stays generic ("choose how to continue below"), so no hint over-promises a button that
  `allowedRecoveryActions` may not render.
- **cancelled ≠ failed:** en-US.json:351 vs :357 and ja-JP.json:351 vs :357 are genuinely DIFFERENT
  strings (not byte-identical) — gap #32 closed in BOTH catalogs. Titles already differed.

### Locked-invariant regressions — none found
- **#5 (actions strictly from `allowedRecoveryActions`):** `git diff` of the overlay shows ZERO
  changed lines touching `canRetry`/`canRestartNormally`/`canWipeAndFullSync`/`actions=`/`onWipeRetry`/
  `onDecline`; only the `title`/`hint` props swapped to `errorCopy.*`. Action rendering byte-unchanged.
- **#18 (no raw JSON as primary copy):** resolver always returns localized `MessageDescriptor`s; helper
  spec asserts neither title nor hint contains the raw message, and the overlay render test asserts the
  raw JSON is never the level-1 heading. Pre-existing secondary `<p>{error.message}</p>` / collapsible
  detail is out of scope and shared with bootstrap.
- **#4 / #34:** "verified … chain data" wording preserved (staged-db-invalid, latest-drift); copy is
  one cause sentence + one continue pointer, no IOG-key/ancillary nuance.
- **No bootstrap regression:** the resolver is imported ONLY by `MithrilPartialSyncOverlay`;
  `MithrilErrorView` + its bootstrap-keyed `ERROR_COPY_BY_STAGE` are untouched. Full `mithril-bootstrap/`
  suite (incl. bootstrap specs) stays green.

### i18n integrity
10 new ids present in `MithrilBootstrap.messages.ts`, `defaultMessages.json`, `en-US.json`, `ja-JP.json`,
and `translations/messages.json` (grep -c = 10 in each runtime catalog). JA is first-class translation,
no English placeholders, no `!!!` markers on any new `mithrilPartialSync.error` key in either catalog.

### Tests
Helper spec asserts each of the 5 codes → bespoke title id, latest-drift retriable hint id,
unknown→generic, stage-tier, code-beats-stage, cancelled distinct + short-circuit, and never-raw-message
— real id/`defaultMessage` assertions, not vacuous. Overlay spec adds bespoke-copy-for-mapped-code (raw
JSON never the level-1 title) and cancelled-calmer-distinct render tests. The `react-polymorph` `Link`
jest.mock and the level-1 heading refinement are justified test-env deviations (mirrors
`DaedalusDiagnostics.spec.tsx`; the raw message legitimately renders as a CollapsibleSection `<h2>`
diagnostic header, so the lock #18 guarantee is correctly scoped to the primary/level-1 heading).

### Verification truthfulness (re-run by me)
- `tsc --noEmit -p .` → **EXIT 0**.
- focused jest (`partialSyncErrorCopy.spec.ts` + `MithrilPartialSyncOverlay.spec.tsx`) → **2 suites /
  21 tests PASS**; `partialSyncErrorCopy.ts` 100% coverage.
- full `mithril-bootstrap/` suite → **5 suites / 56 tests PASS** (no bootstrap regression).
- eslint on the 5 touched source/spec files → **0 errors, 4 warnings** (all pre-existing patterns:
  the overlay's `arg`/`error as any`, the spec's `as any` test helper).
- gap #32 distinctness + 10-key presence + no-`!!!` all reproduced. No Node v24 scss workaround needed
  (this task touches no scss, as the plan predicted).

### Defects
None found. Implementation matches the approved plan exactly; all implementer claims reproduced.

Decision: approved
