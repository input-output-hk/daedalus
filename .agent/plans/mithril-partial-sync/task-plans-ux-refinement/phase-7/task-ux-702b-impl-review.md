# task-ux-702b — Implementation Review (702a code-review remediation)

> Implementation + code-review log for the 8 categories (CAT-A…CAT-H) of task-ux-702b.
> Plan: `task-ux-702b.md`. Decisions: `task-ux-702b-decisions.md` (D-702b-0..10).
> Per-category implementation docs: `task-ux-702b-cat-{a..h}.md`.
> Implemented sequentially on the shared working tree in the collision-safe order
> **CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G**, each followed by a
> compile + touched-spec + per-category code-review pass. All 16 actionable findings
> (the 14 from the 702a review + grill-added #15 and #16) plus the 1 refuted non-bug are
> accounted for.

---

## Implementation

Ordered A–H (implemented order is B→A→C→D→E→H→F→G; see the sequencing rationale in the plan).
Line/anchor citations are from the live working tree.

### CAT-A — proactive-prompt lifecycle (#3, #4, #10, #14, #16)
Files: `MithrilProactivePromptContainer.tsx`, `MithrilPartialSyncStore.ts`,
`common/types/mithril-partial-sync.types.ts`, `MithrilProactivePromptContainer.spec.tsx`,
`MithrilPartialSyncStore.spec.ts`.
- Container: dropped the dead `actions` inject — `@inject('stores','actions')` → `@inject('stores')`,
  removed `actions` from `defaultProps`, switched `Props` from `InjectedProps` → `InjectedStoresProps`
  (#10). Rewrote `render()` so the cheap boolean gate `isGated` short-circuits **first** and
  `computeBehindByEpochs` is computed only after the early return (#14 observer-perf).
- Gate (D-702b-1 / #16 / #4): added `isConnected` (node-loaded / past-verifying), broadened the
  anti-flash known-term to `(isBehindnessKnown || certifiedKnown)` where
  `certifiedKnown = finite(localTip.epoch) && finite(certifiedEpoch)`, AND-ed in
  `!mithrilAttemptStartedThisSession` (re-pop guard), and added a second-stage near-tip hide
  `if (computeBehindByEpochs(...) === undefined) return null`. Kept the app-level mount + full
  cross-screen persistence (REVERSES the review's route-scoping); refreshed the top-of-file doc comment.
- Store: added `@observable mithrilAttemptStartedThisSession = false`, set `true` as the first
  statement of `startPartialSync` (before the optimistic `_updateStatus`); populated
  `this.certifiedEpoch = availability.certifiedEpoch` in `_applyAvailability` (#16).
- Types: added optional `certifiedEpoch?: number | null` to `MithrilPartialSyncAvailability` (consumer
  owns the optional field so the renderer type-checks before CAT-H produces the value).

### CAT-B — `computeBehindByEpochs`; ≤0 ⇒ undefined; beacon fallback (#11, #7, #16)
Files: `utils/mithrilBehindness.ts`, `utils/mithrilBehindness.spec.ts`, `DaedalusDiagnostics.tsx`,
`containers/status/DaedalusDiagnosticsDialog.tsx`, `MithrilProactivePromptContainer.tsx`,
`MithrilPartialSyncStore.ts` (declaration only).
- Added `computeBehindByEpochs(localTip, networkTip, certifiedEpoch?)` returning `undefined` at gap ≤ 0
  (replacing the old `Math.max(1, …)` clamp that rendered the misleading "about 1 epochs behind", #7);
  hybrid anchor prefers `networkTip.epoch` when finite, else `certifiedEpoch` (#16). `isMithrilBehindnessKnown`
  left **byte-identical** (gate-vs-display decoupling, D-702b-2/§4).
- Switched both call sites (`DaedalusDiagnostics.tsx`, `MithrilProactivePromptContainer.tsx`) to the util
  and deleted the duplicated `networkEpoch`/`localEpoch` intermediates (#11). Added a
  `certifiedEpoch?: number | null` prop to `DaedalusDiagnostics` and wired it through
  `DaedalusDiagnosticsDialog` (`certifiedEpoch={mithrilPartialSync.certifiedEpoch}`).
- Declared the `@observable certifiedEpoch: number | null | undefined = undefined` store observable
  (CAT-B is the first of the four store editors; populated later by CAT-A).

### CAT-C — order-agnostic error view; overlay-owned ordering; decision-view; bootstrap default; doc note (#1, #5)
Files: `MithrilErrorView.tsx`, `MithrilErrorView.scss`, `MithrilDecisionView.tsx`,
`MithrilPartialSyncOverlay.tsx`, specs, `task-ux-702a-impl-review.md`.
- `MithrilErrorView` is now render-order-agnostic: removed the `orderedActions` primary-last filter,
  renders `resolvedActions` verbatim in caller order. Added an opt-in `rightAlignActions?: boolean` prop →
  `.actionsRightAligned` modifier; `.actions` changed from a global `flex-end` to `flex-start` (right-align
  is now opt-in). This restores the empty-chain bootstrap default (no `actions` prop ⇒ primary-first,
  left-aligned `[Wipe chain & retry, Decline]`), fixing the silent destructive-button reorder (#1).
- Ordering (secondary-first / primary-last) + `rightAlignActions` were pushed into the
  `MithrilPartialSyncOverlay` caller only (`orderedErrorActions` is a stable reorder of the **same** action
  set — never filters/drops/adds; `allowedRecoveryActions` still owns membership).
- `MithrilDecisionView` DOM order fixed: decline (secondary) before accept (primary), keeping `flex-end` ⇒
  primary on the right (#5), matching the corrected error view and the `widgets/Dialog.tsx` convention.
- Corrected the inaccurate "sanctioned visual-only … no logic/handler change" note in
  `task-ux-702a-impl-review.md:533-537` (see Deliverable 5 below).

### CAT-D — completion finalize renderer-only robustness (#2)
Files: `MithrilPartialSyncStore.ts`, `MithrilPartialSyncOverlay.tsx`, specs.
- `dismissCompletedOverlay`: now `await`s `mithrilPartialSyncFinalizeChannel.request()` **before** flipping
  `isCompletedOverlayDismissed`; flips only on success (wrapped in `runInAction(...)` — see Code Review);
  catches + logs on failure with no blind flip; `finally { await this.syncStatus() }` resyncs the true
  backend state on every outcome.
- `MithrilPartialSyncOverlay` completed auto-dismiss `setTimeout`: wraps `onDismissCompleted()` in
  `Promise.resolve(...).catch(() => {})` so a finalize rejection can never surface as an unhandled
  rejection; widened the prop type to `onDismissCompleted(): void | Promise<void>`.
- No backend change (today's service runs `_resetToIdleStatus()` before `fs.remove`, so a finalize failure
  ends at `idle`; the staging/`.lock` leak remains renderer-unreachable and deferred to the out-of-scope
  backend reorder, per D-702b-4 corrected framing).

### CAT-E — availability poll: anti-pin bound + known-stable back-off (#6, #9)
Files: `MithrilPartialSyncStore.ts`, `MithrilPartialSyncStore.spec.ts`.
- #6 anti-pin: the availability `request()` is now raced against a 10s timeout
  (`AVAILABILITY_REQUEST_TIMEOUT_MS`) whose timer is **always** cleared in `finally`, so a wedged main
  process can no longer pin `_isRefreshingAvailability = true`.
- #9 back-off: extracted `_armAvailabilityInterval(intervalMs)` (clears any existing interval first);
  added `_isAvailabilityStable()` (`!isPartialSyncEnabled || !isSignificantlyBehind`); slows the poll to
  5 min (`AVAILABILITY_REFRESH_BACKOFF_INTERVAL`) only after `STABLE_READS_BEFORE_BACKOFF = 2` consecutive
  stable reads (ISSUE-1 guard via `_consecutiveStableReads`), and re-arms the fast 30 s cadence on the
  first unstable read after a back-off. Idle poll kept (the `isWorking` guard is NOT restored).

### CAT-F — shared `<CompletionBlock>`; remove dead `.primaryButton` CSS (#12, #13)
Files: `MithrilProgressView.tsx`, `SyncingConnectingMithrilPrompt.tsx`, `SyncingConnectingMithrilPrompt.scss`.
- #12: extracted a shared `CompletionBlock({ title, detail?, spinnerPosition })` helper and replaced the
  three near-identical stopping/starting/completed-transition blocks (completed uses
  `spinnerPosition="top"` and no detail), preserving `role=status`/`aria-live=polite`/`aria-atomic=true`
  + the spinner. Call sites keep their own `prop || intl.formatMessage(...)` resolution.
- #13: deleted the dead `.primaryButton` scss rule and the two `styles.primaryButton` `classNames` refs
  (now `['primary', styles.actionButton]`); rendered width unchanged (`.actionButton` already provides
  `min-width:180px`).

### CAT-G — live wipe-and-full-sync render + handler test (#8)
Files: `MithrilPartialSyncOverlay.spec.tsx`.
- Added a test on the **real** overlay driven from `status:'failed'` with
  `canRetry:false / canRestartNormally:false / canWipeAndFullSync:true` (wipe resolves to `primary`):
  asserts the wipe button renders, clicks it, and asserts `onWipeAndFullSync` fired once. Driven from
  `failed`, not `cancelled` (D-702a-2 removed wipe from the pre-cutover cancelled dialogue).

### CAT-H — backend probe-cost cache + beacon-epoch production (#15, #16, backend-only)
Files: `MithrilPartialSyncService.ts`, `mithrilSnapshotMetadata.ts`,
`MithrilPartialSyncService.spec.ts`, `mithrilSnapshotMetadata.spec.ts`, `mithrilPartialSyncChannel.spec.ts`.
- #15: added `_localImmutableCache` + `_getCachedLocalImmutableNumber()` under the existing 5-min
  `PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS` — a cache hit now skips **both** `getManagedChainPath()` (which
  transitively forks `checkDiskSpace`) and `resolveLocalImmutableNumber()` (the `immutable/` readdir).
  Added `_invalidateBehindnessCaches()` (nulls both caches) called at `start()` and `cancel()` entry and
  inside `_resetToIdleStatus()` (covers restart-normal/wipe/finalize-wipe/finalize-completed). The
  behind-ness result is still computed fresh each call (`latest − cachedLocal`); conservative (a stale-low
  local read reads as MORE behind, never a premature not-behind ⇒ no ISSUE-1 regression).
- #16: added `extractCertifiedEpoch(raw)` (multi-path, undefined-safe; `['beacon','epoch']` first), added
  `certifiedEpoch: number | null` to `ResolvedLatestSnapshot` set in `normalizeResolvedLatestSnapshot`,
  carried `epoch` on `_latestCertifiedImmutableCache`, added `_getCachedCertifiedEpoch()`, and appended
  `certifiedEpoch` to both the success and gap≤0 returns of `getPartialSyncBehindness` via a conditional
  spread `...(certifiedEpoch != null ? { certifiedEpoch } : {})` (keeping `behindByImmutables: gap` as the
  unconditional literal so the existing `toEqual` cases stay green). `MithrilController.getPartialSyncAvailability`
  auto-forwards via `{ isEnabled, ...behindness }` (verify-only, no edit — confirmed).

### Refuted non-bug (out of scope, D-702b-0)
The recommendation tooltip on a disabled button is covered by the always-visible `buttonHintBlocked`; the
optional `<span>`-wrapper polish was not required and was not done.

---

## Code Review

Per-category code-review passes (orchestrator inputs): **CAT-B, CAT-A, CAT-C, CAT-E, CAT-H, CAT-F, CAT-G —
review = approved, 0 iterations; CAT-D — review = approved, 1 iteration.** No unresolved category reviews.

- **CAT-B (approved, 0 iters):** verified the gate-vs-display decoupling held — `isMithrilBehindnessKnown`
  byte-identical, the ≤0 ⇒ undefined and the hybrid `networkTip.epoch ?? certifiedEpoch` anchor live only
  in `computeBehindByEpochs`; both call sites consume the util with the intermediates removed; undefined
  `certifiedEpoch` degrades to the networkTip-only figure (no regression).
- **CAT-A (approved, 0 iters):** confirmed reviewer ruling **F2** (KEEP BOTH gate stages) is intact — the
  cheap `(isBehindnessKnown || certifiedKnown)` boolean short-circuits before `computeBehindByEpochs`, and
  the near-tip `=== undefined` early-return is separate; the dead `actions` inject is gone; the re-pop
  guard is session-scoped and never reset; persistence/app-level mount preserved.
- **CAT-C (approved, 0 iters):** confirmed the error view renders caller order verbatim, the bootstrap
  default is restored (primary-first/left), right-align is opt-in, the overlay reorder is a stable reorder
  of the **same** set (locked invariant #2 — membership still owned by `allowedRecoveryActions`), and the
  decision view DOM order matches.
- **CAT-D (approved, 1 iteration — blocker resolved):** the single review iteration surfaced a MobX
  **strict-mode** blocker — under `enforceActions: 'observed'` (`source/renderer/app/index.tsx:30`) the
  post-`await` mutation `this.isCompletedOverlayDismissed = true` resumes outside the enclosing `@action`'s
  synchronous span, so it would throw and be swallowed by the surrounding `catch`, leaving the dismiss flag
  stuck `false` (the success overlay never hides). **Resolved** by wrapping the success-path flip in
  `runInAction('MithrilPartialSyncStore: dismiss completed overlay', …)`, mirroring the store's existing
  post-await action pattern. The await-before-flip, no-blind-flip-on-failure, `finally`-resync, and the
  overlay timeout `.catch` were all confirmed.
- **CAT-E (approved, 0 iters):** confirmed the request timeout always clears its timer, the back-off
  requires 2 consecutive stable reads (ISSUE-1 guard), re-arms fast on the first unstable read, and never
  stops the poll; idle consumers preserved.
- **CAT-H (approved, 0 iters):** confirmed both caches are TTL-bounded and invalidated on every lifecycle
  transition, a hit skips both expensive inputs, the behind-ness verdict per (latest, local) pair is
  unchanged, and the `!= null` conditional spread keeps the four existing `toEqual` cases green (Jest
  ignores `undefined` but not `null`; `_getCachedCertifiedEpoch()` returns `null`).
- **CAT-F (approved, 0 iters):** confirmed the three blocks render identically via `CompletionBlock`
  (a11y attributes + spinner preserved, completed = spinner-top/no-detail) and the dead CSS + refs are
  gone with unchanged button width.
- **CAT-G (approved, 0 iters):** confirmed the live overlay test drives the real component from `failed`,
  renders the wipe button, and asserts `onWipeAndFullSync` fired once.

---

## Whole-task verification (initial verdict: FAIL → PASS after orchestrator closure)

- `yarn compile` — **PASS** (tsc --noEmit exit 0, 0 `error TS`).
- `yarn lint` — **PASS** (exit 0; 5469 warnings are the pre-existing repo-wide baseline, 0 errors).
- `yarn stylelint` — **PASS** (exit 0, no violations).
- `yarn prettier:check` — **FAIL (baseline + 2 task-introduced nits).** 232 files flagged; 226 are
  untouched by this task — the repo is pervasively prettier-non-compliant at HEAD/develop under prettier
  2.1.2, so this gate does not pass on a pristine checkout (fundamentally environmental/baseline). Of the
  6 task-modified files flagged, 4 were already non-compliant at HEAD; **2 were clean at HEAD and are new
  task-introduced nits** (cosmetic line-wrap only): `source/main/mithril/mithrilSnapshotMetadata.spec.ts`
  (lines 57-59) and `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx`
  (line 265).
- `yarn test:jest` — **FAIL (1 real task-attributable test failure).** Full suite: 59 suites / 647 tests
  → 58 suites & 646 tests pass; **1 suite & 1 test fail**:
  `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx:209-213`, test
  "floors the epoch difference at 1 when the tips share an epoch". CAT-B intentionally replaced the old
  `Math.max(1, …)` clamp in `DaedalusDiagnostics.tsx` with `computeBehindByEpochs()` (returns `undefined`
  at a 0-epoch gap ⇒ the component now renders the fallback "behind the latest verified snapshot" line,
  the desired #7 fix), but this sibling spec was **not** updated and still asserts the obsolete clamped
  "about 1 epochs behind" copy. The product behavior is correct; the spec is stale. The behavior change
  itself is covered by the updated `mithrilBehindness.spec.ts` (passes).

### Orchestrator closure pass (2026-06-30)

The orchestrator applied the two **test/format-side** fixes the verify subagent flagged (no product/source
change) and re-ran the gate:

1. **Jest stale spec — fixed.** Updated `DaedalusDiagnostics.spec.tsx` "floors the epoch difference at 1 …"
   to assert the new fallback line ("Your node is behind the latest verified snapshot. …") and renamed it
   "shows the unknown behind-ness line when the tips share an epoch (no misleading floor-to-1)". This is the
   sibling spec the CAT-B grounding omitted; the behavior was already covered by `mithrilBehindness.spec.ts`.
2. **Prettier nits — formatted.** Ran `prettier --write` on `mithrilSnapshotMetadata.spec.ts` and
   `MithrilBootstrap.spec.tsx` (plus the re-edited `DaedalusDiagnostics.spec.tsx`); all task-touched files
   now pass `prettier --check`.

**Post-closure gate (re-run): PASS** — `yarn compile` PASS · `yarn lint` PASS (0 errors) · `yarn stylelint`
PASS · `yarn test:jest` **647/647 PASS (59 suites)** · `prettier:check` clean on all task-touched files (the
residual repo-wide prettier FAIL is the pre-existing baseline; this task adds zero new violations).

---

## Task-state decision (tasks.json) — COMPLETED

`task-ux-702b` is set **`status: "completed"`, `completedAt: "2026-06-30"`** — the whole-task verification
verdict is **PASS** after the orchestrator closure pass above, and all 8 per-category code reviews are
**APPROVED** with no unresolved reviews.

Remaining work is the CAT-A/CAT-C/CAT-H **verify-only operator QA** items (the D-702b-1
fixed-banner-over-Settings/Staking/Voting caveat, the D-702b-5 empty-chain bootstrap dialog, and the
D-702b-10 operator `cardano-db snapshot show latest --json` beacon-epoch key confirmation). These require a
running app / live aggregator, are **NOT build gates** per the decisions, and are reported to the operator
as post-landing follow-ups.
