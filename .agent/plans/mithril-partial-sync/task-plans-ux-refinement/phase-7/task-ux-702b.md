# task-ux-702b — Mithril partial-sync UX cleanup (702a code-review remediation)

> Canonical task plan + single source of truth for the **14 actionable findings (#1–#14)** of the
> **task-ux-702a code review** (plus 1 refuted non-bug). Authoritative inputs: `task-ux-702b-decisions.md` (locked decisions
> D-702b-0..9, the CAT-A…CAT-H category map, sequencing), `task-ux-702a.md` (predecessor plan, whose
> structure this mirrors), `task-ux-702a-decisions.md` (predecessor locks / cross-refs),
> `prompt-ux-refinement.md` (process rules, locked safety boundaries). All citations re-verified
> against live code while writing this plan; any drift from the decisions-doc grounding is called out
> in **§ Grounding pins & drift**.
>
> Review logs: `task-ux-702b-plan-review.md` (the grill + grounding re-verification that produced this
> revision — done), `task-ux-702b-impl-review.md`, `task-ux-702b-research.md` (as needed).

## Execution status — EXECUTED (finalized 2026-06-30)

**All 8 categories (CAT-A…CAT-H) implemented** in the collision-safe order
`CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`; **all 8 per-category code reviews
APPROVED** (CAT-D took 1 iteration — a MobX strict-mode `runInAction` blocker, resolved; all others 0
iterations). All 16 actionable findings (the 14 from the 702a review + grill-added #15 and #16) are
remediated; the refuted tooltip stayed out of scope. Source changes span 23 files (see
`task-ux-702b-impl-review.md` for the per-category Implementation + Code Review log and
`task-ux-702b-research.md` for durable findings).

**Whole-task verification verdict: PASS (gate green) after the orchestrator's closure pass.** The
whole-task verify subagent first reported FAIL on two **test/format-side only** items (no product-code
change required); the orchestrator then applied the minimal closure fixes below and re-ran the gate:
1. **Jest (1 stale sibling spec, now fixed):** `DaedalusDiagnostics.spec.tsx` "floors the epoch difference
   at 1 when the tips share an epoch" asserted the obsolete `Math.max(1,…)` copy that CAT-B intentionally
   removed (it swapped the clamp for `computeBehindByEpochs`, which returns `undefined` at a 0-epoch gap ⇒
   the unknown-behind-ness fallback copy). The grounding brief's CAT-B spec list omitted this sibling, so
   the CAT-B implementer left it stale. The orchestrator updated the case to assert the new fallback line
   and renamed it "shows the unknown behind-ness line when the tips share an epoch (no misleading
   floor-to-1)". The companion `mithrilBehindness.spec.ts` already covered the new behavior.
2. **Prettier (2 task-introduced nits, now formatted):** `mithrilSnapshotMetadata.spec.ts` and
   `MithrilBootstrap.spec.tsx` were prettier-clean at HEAD but newly flagged — formatted with
   `prettier --write`. The residual repo-wide prettier:check FAIL is the pre-existing baseline (226/232
   flagged files untouched by this task); this task now introduces **zero** new prettier violations.

**Post-closure gate:** `yarn compile` PASS · `yarn lint` PASS (0 errors) · `yarn stylelint` PASS ·
`yarn test:jest` **647/647 PASS (59 suites)** · `prettier:check` clean on all task-touched files.

**Tasks-JSON state: `completed` / `completedAt: 2026-06-30`.** Set by the orchestrator on the green verdict
with all 8 category reviews approved and unresolved-free. Remaining work is the CAT-A/CAT-C/CAT-H
**verify-only operator QA** items (fixed-banner-over-nav on Settings/Staking/Voting, empty-chain bootstrap
dialog, live beacon-epoch key on the pinned-fork aggregator) — these require a running app / live
aggregator, are NOT build gates, and are reported to the operator as follow-ups. Plan retained (not
deleted) as the source of truth.

## Per-category implementation docs (decomposition — implement from these)

This canonical plan has been decomposed into **eight self-contained per-category docs**. Each is
implementable on its own to the small-model bar **without** reading this full plan; this master stays the
single source of truth and the index. Implement in the collision-safe order below, each followed by
`yarn compile` + the touched specs + a per-category code-review pass.

| Order | Doc | Category (scope) | Findings |
|---|---|---|---|
| 1 | [`task-ux-702b-cat-b.md`](./task-ux-702b-cat-b.md) | CAT-B — extract `computeBehindByEpochs` (≤ 0 ⇒ undefined) + beacon-epoch fallback | #11, #7, #16 |
| 2 | [`task-ux-702b-cat-a.md`](./task-ux-702b-cat-a.md) | CAT-A — proactive-prompt lifecycle (node-loaded trigger, near-tip hide, re-pop guard, perf reorder, drop dead inject, beacon gate) | #3, #4, #10, #14, #16 |
| 3 | [`task-ux-702b-cat-c.md`](./task-ux-702b-cat-c.md) | CAT-C — order-agnostic shared error view; overlay-caller ordering; fix decision view; restore bootstrap default; correct doc note | #1, #5 |
| 4 | [`task-ux-702b-cat-d.md`](./task-ux-702b-cat-d.md) | CAT-D — completion finalize renderer-only robustness | #2 |
| 5 | [`task-ux-702b-cat-e.md`](./task-ux-702b-cat-e.md) | CAT-E — availability poll: anti-pin request bound + known-stable back-off (keep idle poll) | #6, #9 |
| 6 | [`task-ux-702b-cat-h.md`](./task-ux-702b-cat-h.md) | CAT-H — backend probe-cost cache + beacon-epoch production (backend-only) | #15, #16 |
| 7 | [`task-ux-702b-cat-f.md`](./task-ux-702b-cat-f.md) | CAT-F — shared `<CompletionBlock>`; remove dead `.primaryButton` CSS | #12, #13 |
| 8 | [`task-ux-702b-cat-g.md`](./task-ux-702b-cat-g.md) | CAT-G — live wipe-and-full-sync render + handler test | #8 |

**#16 spans CAT-B + CAT-A + CAT-H** (consumer util fallback + container gate/figure + backend beacon
producer); its producer/consumer split is order-independent by safe degradation (absent `certifiedEpoch`
⇒ networkTip-only = today). The per-category docs carry the **plan-review corrections** applied this
revision (CAT-D failure-path framing; CAT-H operator beacon-key pre-step + conditional-result; etc.) — see
[`task-ux-702b-plan-review.md`](./task-ux-702b-plan-review.md).

## Task id + title
- **Id:** task-ux-702b
- **Title:** Mithril partial-sync UX cleanup (702a code-review remediation)

## Interaction mode (AUTHORITATIVE)
`interactive_validation`. The decisions are already locked in `task-ux-702b-decisions.md`; this round
is mechanical remediation. The human-in-the-loop surface that keeps it non-autonomous is: (1) the
per-category code-review pass after each CAT lands, and (2) operator visual validation of the two
**verify-only** items (the D-702b-1 fixed-banner-over-Settings/Staking/Voting caveat and the D-702b-5
empty-chain bootstrap dialog). Do **not** relabel this autonomous — phase-7 tasks require operator-run
validation. The decisions behind each edit are not re-opened.

## Why now
`task-ux-702` (the deployment QA gate) is held `pending` and is gated behind 702a (now complete) →
**702b**. The xhigh code review of the uncommitted 702a working tree surfaced **14 actionable
findings (#1–#14)** (7 correctness/behavior, 1 test gap, 6 cleanups) plus **1 refuted non-bug** (the
recommendation tooltip on a disabled button). 702b remediates the 14 before the deployment gate can
re-run. The tasks JSON gains `task-ux-702b`; `task-ux-702` depends on it (chain: `…702a → …702b →
…702`).

## Scope
Remediate the 14 actionable findings (#1–#14) **plus the grill-added #15 (CAT-H, 2026-06-30) and #16
(folds into CAT-B/CAT-A/CAT-H, 2026-06-30)**, grouped into **8 implementation categories (CAT-A…CAT-H)**,
one code subagent per category, implemented sequentially on the shared working tree in the collision-safe
order **CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G**, each followed by a
code-review/compile pass. (#16 adds **no new category** — it revises the data source of three already-listed
categories; its producer/consumer split is order-independent by safe degradation — see D-702b-10.)

## Non-goals (carried + new)
- **No backend reorder for the finalize path (D-702b-4).** The renderer-only robustness fix is chosen;
  `MithrilPartialSyncService.finalizeCompletedPartialSync` (`MithrilPartialSyncService.ts:351-362`) is
  **not** touched (its `_resetToIdleStatus()`-before-`fs.remove` order stays).
- **Do NOT route-scope the proactive prompt (D-702b-1 REVERSES the review).** Keep the app-level mount
  and full cross-screen persistence; do not move it back inside `SyncingConnecting`.
- **Do NOT restore the `isWorking` guard on the availability poll (D-702b-6).** Its removal was
  intentional (idle-time consumers).
- **Do NOT modify `isMithrilBehindnessKnown` (D-702b-2 decoupling invariant).** The `≤ 0 ⇒ undefined`
  logic lives only in the new display-only `computeBehindByEpochs`.
- **Do NOT re-align the initial Mithril bootstrap dialog (D-702b-5).** Making the shared error view
  order-agnostic already protects the bootstrap default; any deliberate bootstrap realignment is a
  separate, code-reviewed decision — here it is **verify-only**.
- No i18n key changes (no copy edits in any category; CAT-F #12 keeps each call site's
  `prop || intl.formatMessage(...)` resolution).
- No change to the Boundary A/B/C recovery model, the `allowedRecoveryActions`-only render contract, or
  the backend `isSignificantlyBehind` offer signal.

## Dependencies
- **Depends on `task-ux-702a`** (this is its code-review cleanup round).
- **Blocks `task-ux-702`** (the deployment QA gate; update its deps to include `task-ux-702b`).

---

## Finding → Category traceability (nothing dropped)

| # | Finding (short) | Decision | CAT | Severity |
|---|---|---|---|---|
| #1 | Shared `MithrilErrorView.orderedActions` "primary-last" filter silently moved the destructive **"Wipe chain & retry"** primary to the far right of the empty-chain bootstrap default (no `actions` prop) | D-702b-5 | **CAT-C** | High |
| #2 | Completed-overlay finalize is fire-and-forget (no `.catch`) and `dismissCompletedOverlay` flips `isCompletedOverlayDismissed=true` **before** awaiting finalize → unhandled rejection + staging-dir/`.lock` leak | D-702b-4 | **CAT-D** | High |
| #3 | Proactive prompt flashes during connecting / "verifying blockchain", and shows the misleading near-tip offer; trigger/persist/near-tip-hide must be fixed (review's route-scoping is **reversed**) | D-702b-1 | **CAT-A** | High |
| #4 | Prompt re-pops after a Mithril attempt ends (completed/cancelled/failed/restart-normal) because only the dismiss flag — written nowhere on those paths — gates it | D-702b-3 | **CAT-A** | High |
| #5 | `MithrilDecisionView.scss:75` flipped to `flex-end` but its `.tsx` DOM order (primary first) was not, leaving its primary on the **left** (mirror image of the error view) | D-702b-5 | **CAT-C** | Med |
| #6 | Availability re-entrancy pin: `_isRefreshingAvailability` clears only in `finally` and `channel.request()` has no timeout → a wedged main process pins the guard `true` forever | D-702b-6 | **CAT-E** | High |
| #7 | Misleading "about 1 epochs behind" from `Math.max(1, …)` clamp at diff ≤ 0 | D-702b-2 | **CAT-B** | Med |
| #8 | No live test exercises the real overlay's `canWipeAndFullSync:true` render + `onWipeAndFullSync` handler (the one positive test was rewritten to assert absence) | D-702b-8 | **CAT-G** | Test gap |
| #9 | The 30s availability poll lost its `isWorking` gate (correct) but now fires every 30s for the whole session even after the node is fully synced | D-702b-6 | **CAT-E** | Cleanup |
| #10 | Dead `actions` inject in `MithrilProactivePromptContainer` (`@inject('stores','actions')`, `actions` unused) | D-702b-1 (CAT-A bundle) | **CAT-A** | Cleanup |
| #11 | Byte-identical behind-by-epochs math duplicated in `MithrilProactivePromptContainer.tsx` and `DaedalusDiagnostics.tsx` | D-702b-2 | **CAT-B** | Cleanup |
| #12 | Three near-identical completion blocks in `MithrilProgressView.tsx` (stopping/starting/completed) | D-702b-7 | **CAT-F** | Cleanup |
| #13 | Dead `.primaryButton` CSS rule + its two `styles.primaryButton` refs in `SyncingConnectingMithrilPrompt` | D-702b-7 | **CAT-F** | Cleanup |
| #14 | Observer-perf: the container reads the tip observables and computes the figure **above** the gate (figure computed even when the prompt is hidden) | D-702b-1 (CAT-A bundle) | **CAT-A** | Cleanup |
| #15 | **(grill-added 2026-06-30 — NOT from the 702a review)** Per-probe backend CPU cost: `getPartialSyncBehindness` (the 30s availability poll, when enabled) **forks `checkDiskSpace`** (df / PowerShell `Get-CimInstance`) via `getManagedChainPath → getConfig → getDefaultStorageConfig` **and** re-reads the whole `immutable/` dir (`resolveLocalImmutableNumber → fs.readdir` + ~5 `stat`/`access`) on **every** tick — only the aggregator (`_getCachedLatestCertifiedImmutableNumber`) is cached. CAT-E's renderer-only cadence back-off reduces frequency once not-behind but **never the per-call cost**, and is gated **off while the node is behind** (the catch-up window the user feels). The 702a review under-traced #9's cost; #15 fixes the actual CPU driver in the backend. | D-702b-9 | **CAT-H** | Perf/High |
| #16 | **(grill-added 2026-06-30 — NOT from the 702a review; "epoch visibility" signalling defect)** The behind-ness **gate** (`isMithrilBehindnessKnown` = finite `networkTip.epoch`) and the displayed epochs figure both anchor on cardano-wallet's `networkTip.epoch`, which resolves **only near the end of sync** (PastHorizon: "now" is beyond the era-history safe zone until the local tip nears the current era — `types.ts:48-78` types `network_tip.epoch_number` optional vs `node_tip.epoch_number` required). So during early/mid sync — exactly when the user is most behind and Mithril helps most — the prompt is **suppressed** and Diagnostics shows "unknown." The backend already sidesteps this via the **immutable beacon** (horizon-free). Fix: **re-anchor on the beacon's certified epoch (hybrid: prefer `networkTip.epoch` when finite, else `certifiedEpoch`)** for both the gate and the display. Plumb `certifiedEpoch` from the probe → availability payload → store → container. Multi-path/undefined-safe ⇒ degrades to today when absent. | D-702b-10 | **CAT-B + CAT-A + CAT-H** | High |
| — | **Refuted non-bug:** recommendation tooltip on a *disabled* button. `buttonHintBlocked` ("Unavailable while Mithril work is already active.") already covers the disabled state; the hover pitch only matters when the button is enabled. **Out of scope.** Optional one-line `<span>`-wrapper polish for consistency with `MithrilProgressView.tsx:248-254` is **not required**. | D-702b-0 | none | — |

**Numbering note (RESOLVED — reviewer ruling F1):** Standardize on **14 actionable findings (#1–#14)
+ 1 refuted non-bug**. An earlier draft of the D-702b-0 prose said "13 actionable findings" and called
the refuted tooltip "the 14th"; that wording has since been **corrected in the decisions doc** —
`task-ux-702b-decisions.md:11-14` now reads "14 actionable findings (#1–#14)" and treats the refuted
tooltip as a **separate** non-bug, **not** "the 14th" finding. The decisions-doc **category map**
(`task-ux-702b-decisions.md:370-396`, table at `:372-381`) enumerates the 14 actionable findings
**#1–#14** (it counts the two CAT-A cleanups #10 drop-dead-inject and #14 observer-perf reorder
separately). **This plan implements exactly the category map's groupings; treat the finding DESCRIPTIONS
above (not the bare numbers) as authoritative** when reading the original review.

**Grill-added #15 (2026-06-30):** the `grill-with-docs` CPU-cost re-validation surfaced a **15th**
actionable finding — the per-probe backend cost (**CAT-H / D-702b-9**). It is **NOT** part of the
original 702a review's 14; it remediates a cost that review under-traced when it tagged #9 as
"Cleanup / wasteful polling." So 702b now closes **14 (from the 702a review) + 1 (grill-added #15) =
15 actionable findings + 1 refuted non-bug**. CAT-H is backend-only and collision-free with CAT-A…G.

**Grill-added #16 (2026-06-30):** a second `grill-with-docs` session (the "epoch visibility" signalling
defect) surfaced a **16th** actionable finding (**D-702b-10**) — the behind-ness gate + display anchor on
cardano-wallet's `networkTip.epoch`, which resolves only near end-of-sync, so the prompt is suppressed
during the early/mid window when Mithril helps most. It **folds into the unbuilt CAT-B (util fallback
param), CAT-A (store field + container gate/figure), and CAT-H (backend beacon-epoch extraction + IPC
payload)** — **no new category**. So 702b now closes **14 (702a review) + 1 (#15) + 1 (#16) = 16
actionable findings + 1 refuted non-bug**.

---

## Locked decisions (restated; do not re-litigate — see `task-ux-702b-decisions.md`)
- **D-702b-1 (CAT-A) — proactive prompt: KEEP cross-screen persistence; fix trigger + near-tip hide.**
  REVERSES the review's route-scoping. (a) trigger only once the node is loaded (connected +
  behind-ness known, past verifying); (b) persist on all screens until the user chooses this session;
  (c) near-tip hide. Plus the fixed-banner visual caveat (verify-only).
- **D-702b-2 (CAT-B) — extract `computeBehindByEpochs` (≤ 0 ⇒ undefined), display-only.** The gate
  `isMithrilBehindnessKnown` stays clamp-free and unchanged. **Gate vs display are decoupled.**
- **D-702b-3 (CAT-A) — separate re-pop guard `mithrilAttemptStartedThisSession`.** Keep
  `proactivePromptDismissedThisSession` single-purpose.
- **D-702b-4 (CAT-D) — renderer-only finalize fix:** flip `isCompletedOverlayDismissed` **only on
  finalize success**; catch + always resync (`finally`); `.catch` the overlay timeout. No backend change.
  **Deliverables are no-unhandled-rejection + no-premature-optimistic-hide-on-success; on failure the
  overlay hides via the idle resync and the staging/`.lock` leak stays deferred to the out-of-scope
  backend reorder** (see D-702b-4 correction).
- **D-702b-5 (CAT-C) — order-agnostic shared error view:** ordering + opt-in right-align move to the
  partial-sync overlay caller; **restore the bootstrap default**; fix `MithrilDecisionView` DOM order;
  the shared `.scss` `flex-end` becomes opt-in/scoped; correct the inaccurate "visual-only" note in
  `task-ux-702a-impl-review.md`.
- **D-702b-6 (CAT-E) — keep the idle poll; bound the request (anti-pin); add a known-stable back-off.**
  Do NOT restore the `isWorking` guard.
- **D-702b-7 (CAT-F) — shared `<CompletionBlock>`; remove dead `.primaryButton` CSS + refs.**
- **D-702b-8 (CAT-G) — add a live wipe-and-full-sync render+handler test (drive from `failed`).**
- **D-702b-9 (CAT-H) — cache the two expensive behind-ness *inputs* under one TTL; invalidate on
  lifecycle transitions.** Add `_getCachedLocalImmutableNumber()` mirroring the existing
  `_getCachedLatestCertifiedImmutableNumber()`: on a cache hit, **skip both** `getManagedChainPath()`
  (which forks `checkDiskSpace`) **and** `resolveLocalImmutableNumber()` (the `immutable/` readdir).
  Reuse the existing 5-min `PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS`. The behind-ness **result** stays
  computed fresh each call (`cachedLatest − cachedLocal`); staleness is bounded by the same TTL the
  aggregator cache already accepts. Invalidate **both** caches on every lifecycle transition
  (`start`/`cancel`/`restart-normal`/`wipe`/`finalize-wipe`/`finalize-completed`) so post-cutover
  behind-ness is fresh. **Backend-only (`MithrilPartialSyncService.ts`); complements CAT-E** (CAT-E =
  poll *frequency*, CAT-H = per-probe *cost*). No ISSUE-1 regression: a stale-low local read makes the
  node look **more** behind (conservative), never prematurely not-behind.

---

## Grounding pins & drift (re-verified against live code 2026-06-30)

**D-702b-1 flag pin — "Cardano node loaded / behind-ness known, past the early verifying phase":**
- **`networkStatus.isConnected`** (`NetworkStatusStore.ts:836-839` = `isNodeResponding &&
  isNodeSyncing`) is the **node-loaded** signal. It also covers "past the early verifying phase"
  because `isVerifyingBlockchain` (`:883-889`) is defined as `!this.isConnected && …`, so
  `isConnected === true ⇒ isVerifyingBlockchain === false`. **This is the new gate term to ADD.**
- **`networkStatus.isBehindnessKnown`** (`:873-881` = `isMithrilBehindnessKnown(localTip,
  networkTip)`) is the existing reactive **behind-ness-known** anti-flash signal (both tips finite
  epochs). It **stays** in the gate, unchanged.
- The current container gate (`MithrilProactivePromptContainer.tsx:54-59`) checks `status==='idle' &&
  isPartialSyncEnabled && isSignificantlyBehind && isBehindnessKnown &&
  !proactivePromptDismissedThisSession` — it does **NOT** yet check `isConnected`, has **no** near-tip
  hide, and has **no** re-pop guard. CAT-A adds all three.

**Confirmed anchors (live):**
- `App.tsx:130` — `<MithrilProactivePromptContainer />` is a sibling of `<Router>` (`:83-85`); overlay
  block `:98-125`. No route awareness (correct per D-702b-1).
- `MithrilProactivePromptContainer.tsx` — `@inject('stores','actions')` `:31` (actions unused, #10);
  tip reads + `Math.max(1, networkEpoch-localEpoch)` `:45-52` ABOVE the gate (#14); gate `:54-59`.
- `MithrilPartialSyncStore.ts` — `proactivePromptDismissedThisSession` declared `:67`, sole writer
  `dismissProactivePrompt` `:243-246`; `dismissCompletedOverlay` `:229-241` flips
  `isCompletedOverlayDismissed=true` `:239` BEFORE `await mithrilPartialSyncFinalizeChannel.request()`
  `:240`, no try/catch; availability interval `:89-91` (no `isWorking` gate); `_refreshAvailability`
  `:200-216` (guard cleared only in `finally` `:213-215`; `request()` `:207` has no timeout);
  `AVAILABILITY_REFRESH_INTERVAL=30_000` `:36`; `startPartialSync` `:250-279`.
- `MithrilPartialSyncOverlay.tsx` — completed auto-dismiss `useEffect` `:84-91`
  (`setTimeout(() => onDismissCompleted(), COMPLETED_AUTO_DISMISS_DELAY_MS)` `:58`, no catch);
  `errorActions` built `:101-151` (recovery actions are **primary-FIRST** in the array); wipe branch is
  part of `recoveryActions` `:124-137`; `MithrilErrorView` rendered `:216-223` with `actions=
  {errorActions}`, `hintAsBody={status==='cancelled'}`.
- `MithrilErrorView.tsx` — default `resolvedActions` `:99-110` (primary first: `wipeAndRetry`,
  `decline`); `orderedActions` "primary-last" filter `:111-116`; render maps `orderedActions`
  `:168-182`; `hintAsBody` already wired `:23/:87/:126-130`. Callers: `MithrilBootstrap.tsx:207-213`
  (NO `actions` prop → default) and the overlay `:216-223` (passes `actions`).
- `MithrilErrorView.scss:75-79` — `.actions { … justify-content: flex-end; }` (currently **global**).
- `MithrilDecisionView.tsx:97-112` — `primaryAction` (`accept`) BEFORE `secondaryAction` (`decline`),
  no reorder; `MithrilDecisionView.scss:72-76` `.actions { … justify-content: flex-end; }`.
- App convention owner: `widgets/Dialog.tsx` + `Dialog.scss` — caller lists **primary LAST → right**;
  example `WalletSettingsRemoveConfirmationDialog.tsx`.
- `utils/mithrilBehindness.ts` — currently exports only `isMithrilBehindnessKnown` (`:18-22`); spec
  covers the predicate only. Duplicated math copy B at `DaedalusDiagnostics.tsx:570-577`; figure flows
  `DaedalusDiagnostics:728 → MithrilPartialSyncSection → MithrilPartialSyncConfirmation` (consumer
  fallback `hasBehindFigure` at `MithrilPartialSyncConfirmation.tsx:67-99`); the prompt consumer
  fallback is `SyncingConnectingMithrilPrompt.tsx:146-154`.
- `MithrilProgressView.tsx` — completion blocks at `:174` (stopping), `:197` (starting), `:220`
  (completed-transition); shared `div.completionBlock` + `role=status`/`aria-live=polite`/
  `aria-atomic=true` + `SVGInline` spinner (`styles.completionSpinner`). Stopping/starting render
  `<h2 completionTitle>` → `<p completionDetail>` → spinner; the completed block renders spinner FIRST,
  then `<h2 completionTitle>`, and **no** detail `<p>`. Gate `isCompletedTransition` `:108-109`.
- `SyncingConnectingMithrilPrompt.scss` — `.actionButton` `:57-60` (`min-width:180px`), dead
  `.primaryButton` `:62-68` (only `min-width:180px`); `.tsx` references `styles.primaryButton` in two
  `classNames([...])` at `:174-179` (choice view "Mithril Sync (fast)") and `:206-211` (confirm view
  "Start now"), always co-applied with `'primary'` + `styles.actionButton`.
- `MithrilPartialSyncOverlay.spec.tsx` — `renderComponent(overrides)` helper `:40-67` defaults
  `canWipeAndFullSync:false`; the cancelled test asserts wipe ABSENT; no remaining real-component test
  drives `canWipeAndFullSync:true`. `act` already imported `:4`.
- `MithrilProactivePromptContainer.spec.tsx` — `renderContainer({networkStatus, mithrilPartialSync})`
  helper `:45-53` with default `networkStatus` mock (`networkTip`, `localTip`, `isBehindnessKnown:
  true`) and default `mithrilPartialSync` (`status:'idle'`, …). **No `isConnected` in the mock yet** —
  CAT-A must add it.
- `task-ux-702a-impl-review.md:533-537` — the inaccurate "Shared-error-view note (#11, sanctioned
  visual-only)" claiming "No logic/handler change." This is the note D-702b-5 corrects.
- Backend finalize: `MithrilPartialSyncService.ts:351-362` `finalizeCompletedPartialSync` —
  `_resetToIdleStatus()` `:358` runs BEFORE `await fs.remove(stagingRoot)` `:359`, so on a finalize
  failure the backend ends at **idle** (the D-702b-4 grounding refinement — the damage is the unhandled
  rejection + staging/`.lock` leak, not a stuck `completed`). `_getStagingRootPath` `:601-611`. **Out of
  scope** (no backend change).
- **CAT-H probe-cost path (grill-verified live 2026-06-30):** the availability poll runs
  `MithrilProactivePromptContainer`/store `_refreshAvailability` →
  `mithrilPartialSyncAvailabilityChannel` → `MithrilController.getPartialSyncAvailability()`
  (`MithrilController.ts:167-174`; short-circuits to `{isEnabled:false, isSignificantlyBehind:false}`
  at `:169-171` when disabled — **no probe cost when disabled**) → `getPartialSyncBehindness()`
  (`MithrilPartialSyncService.ts:669-698`). Per probe (when enabled): (1)
  `_getCachedLatestCertifiedImmutableNumber()` `:674` — **already 5-min cached** via
  `_latestCertifiedImmutableCache` (`:111-114`, `:645-660`); (2) `getManagedChainPath()` `:675-676` →
  `chainStorageManager.getConfig()` → `getDefaultStorageConfig()` →
  **`checkDiskSpace(diskCheckPath)`** (`chainStorageManagerConfig.ts:23`, full path
  `source/main/utils/chainStorageManagerConfig.ts`) — **UNCACHED, forks
  df/PowerShell every probe** (`getManagedChainPath` only needs `config.customPath` and discards the
  `free`-bytes figure, so the fork is pure waste); (3) `resolveLocalImmutableNumber()` `:677-680` →
  `fs.readdir(immutable/)` + ~5 `stat`/`access` (`mithrilPartialSyncPreflight.ts:63-141`) — **UNCACHED
  every probe**. The TTL const + comment that says "**only the aggregator query is cached**" is at
  `:72`. **NOTE: `checkDiskSpace` reaches the probe path via `getManagedChainPath → getConfig`, NOT via
  the `start()` preflight `_assertSufficientDiskSpace` (`:786-815`, called once at `:173`)** — easy to
  miss by grepping `checkDiskSpace` call sites alone. Existing aggregator-cache spec to mirror:
  `MithrilPartialSyncService.spec.ts:1271-1292` (`Date.now` spy + `resolveLatestSnapshotMetadata`
  call-count); `check-disk-space` mocked `:12`, `getManagedChainPath` spied + `readdirMock` driven in
  `stubLocalImmutableNumber` (`:1208-1219`). New symbols `_localImmutableCache`,
  `_getCachedLocalImmutableNumber`, `_invalidateBehindnessCaches` confirmed absent from the tree.
- **#16 beacon-epoch path (grill-verified live 2026-06-30):** (1) `extractLatestCertifiedImmutableNumber`
  (`mithrilSnapshotMetadata.ts:66-88`) and **every** fixture
  (`mithrilSnapshotMetadata.spec.ts:28-31`, `MithrilPartialSyncService.spec.ts:149-150/176-184`) read
  **only** `immutable_file_number` — **no epoch is extracted or plumbed today**; `ResolvedLatestSnapshot`
  / `MithrilSnapshotItem` carry no epoch. (2) The backend probe works **entirely in immutable files**
  (`getPartialSyncBehindness:669-698` → `gap = latest − local`, `behindByImmutables`); the `20` at
  `MithrilPartialSyncService.ts:71` is the **offer threshold** for `isSignificantlyBehind`, **not** a
  display divisor (corrects the originating write-up's "÷20 heuristic" framing). (3)
  `source/renderer/app/api/network/types.ts:48-78`:
  `node_tip.epoch_number` **required** number vs `network_tip.epoch_number: number | null | undefined` —
  the horizon asymmetry that suppresses the gate early. (4) `MithrilController.getPartialSyncAvailability`
  (`source/main/mithril/MithrilController.ts:167-174`) returns `{ isEnabled, ...behindness }` (`:173`) ⇒
  a `certifiedEpoch` on the behind-ness result **auto-forwards** (no controller edit). (5)
  `MithrilPartialSyncStore._applyAvailability` (`:219-227`) sets `isPartialSyncEnabled`/
  `isSignificantlyBehind`/`behindByImmutables` from the payload — `certifiedEpoch` rides the same path.
  (6) `DaedalusDiagnosticsDialog.tsx:133-145` already injects `mithrilPartialSync` + passes
  `isSignificantlyBehind`/`localTip`/`networkTip` ⇒ `certifiedEpoch={mithrilPartialSync.certifiedEpoch}`
  is a one-liner. (7) Web-confirmed: current Mithril `CardanoDbBeacon` carries `{ epoch,
  immutable_file_number }` (deprecated `network` field removed) — but the repo pins a **fork branch**
  (`flake.nix:23`), so the exact key is a **verify-only** item. New symbols `extractCertifiedEpoch`,
  `certifiedEpoch` (store observable), `certifiedKnown` confirmed absent from the tree.

**Drift found:** none material. Line numbers match the decisions-doc grounding within ±2 (the
decisions doc cited `_refreshAvailability:199-216`; live is `:200-216`; `MithrilDecisionView` DOM at
`:97-112` matches; `_deriveAllowedRecoveryActions` is at `:552` in the impl-review vs `:560` in the
702a plan — a pre-existing 702a doc discrepancy, **not** in 702b scope). New symbols
`computeBehindByEpochs` and `mithrilAttemptStartedThisSession` confirmed absent from the tree.

---

## Sequencing (collision-safe)

```
CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G
```
each followed by `yarn compile` + the touched specs + a per-category code-review pass.

**Store-file serialization rationale** — **CAT-B, CAT-A, CAT-D, CAT-E** all edit
`MithrilPartialSyncStore.ts`, so they are serialized **B → A → D → E** (each followed by a compile/review
pass) to avoid edit collisions:
- CAT-B: declares the `@observable certifiedEpoch` (default `undefined`) — a one-line add, the first of
  the four store editors.
- CAT-A: adds the `mithrilAttemptStartedThisSession` observable + sets it in `startPartialSync`.
- CAT-D: rewrites `dismissCompletedOverlay`.
- CAT-E: rewrites `_refreshAvailability` (timeout) + adds the back-off.

**Other couplings:**
- **CAT-B before CAT-A** — both edit `MithrilProactivePromptContainer.tsx`. CAT-B switches the
  container's display math to `computeBehindByEpochs` (and switches `DaedalusDiagnostics.tsx`); CAT-A
  then reorders the render so the gate short-circuits first and consumes the `computeBehindByEpochs`
  result for the near-tip hide. (Decisions doc: "CAT-B before CAT-A — the near-tip gate consumes
  `computeBehindByEpochs`.")
- **CAT-C before CAT-D** — both edit `MithrilPartialSyncOverlay.tsx`. CAT-C reworks the `errorActions`
  ordering/right-align region (`:101-151/:216-223`); CAT-D touches the completed-overlay
  `useEffect`/timeout region (`:84-91`). Serialize C then D (distinct regions, but same file).
- **Shared spec `MithrilPartialSyncOverlay.spec.tsx`** is touched by **CAT-C** (cancelled DOM-order
  assertion), **CAT-D** (completed auto-dismiss / no-unhandled-rejection), and **CAT-G** (new live wipe
  test). The serialized order **C → D → G** has each category touch its own distinct test case in turn
  with a compile/review pass between, so there is no real collision on this spec.
- **CAT-F** touches `MithrilProgressView.tsx` + `SyncingConnectingMithrilPrompt.scss/.tsx` — no overlap
  with A–E. **CAT-G** touches only `MithrilPartialSyncOverlay.spec.tsx` — runs last.
- **CAT-H is backend-only** (`MithrilPartialSyncService.ts` + `.spec.ts`) — **zero overlap** with the
  renderer files touched by CAT-A…G (none of A…G edit the backend service), so it has no collision and
  could run anywhere. It is placed **right after CAT-E** to keep the two poll-cost fixes adjacent
  (CAT-E = frequency, CAT-H = per-probe cost) and reviewable together.

**Per-step verify commands** (Node v24 caveat applies to every step — see § Risks):
1. **CAT-B:** regen `.scss.d.ts` (none changed here, but ensure the sidecar is present so the new util
   import type-checks); `yarn test:jest source/renderer/app/utils/mithrilBehindness.spec.ts`;
   `yarn test:jest source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`;
   `yarn compile`; `yarn lint`.
2. **CAT-A:** `yarn test:jest
   source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx`;
   `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`; `yarn compile`;
   `yarn lint`.
3. **CAT-C:** `yarn test:jest .../MithrilPartialSyncOverlay.spec.tsx`; the bootstrap error-view spec;
   `MithrilDecisionView` spec (if present); `yarn compile`; `yarn lint`. **Regen the
   `MithrilErrorView.scss.d.ts`** (a new opt-in modifier class is added).
4. **CAT-D:** `yarn test:jest .../MithrilPartialSyncStore.spec.ts`; `yarn test:jest
   .../MithrilPartialSyncOverlay.spec.tsx`; `yarn compile`; `yarn lint`.
5. **CAT-E:** `yarn test:jest .../MithrilPartialSyncStore.spec.ts`; `yarn compile`; `yarn lint`.
6. **CAT-H:** `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`; **#16:**
   `yarn test:jest source/main/mithril/mithrilSnapshotMetadata.spec.ts` and
   `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` (payload forward); `yarn compile`;
   `yarn lint`. (Backend; no `.scss.d.ts` regen needed.)
7. **CAT-F:** **regen `SyncingConnectingMithrilPrompt.scss.d.ts`** (the `.primaryButton` class is
   removed); `yarn test:jest` on the prompt + progress-view specs; `yarn compile`; `yarn lint`;
   `yarn prettier:check`.
8. **CAT-G:** `yarn test:jest .../MithrilPartialSyncOverlay.spec.tsx`; `yarn compile`; `yarn lint`.

Final whole-task gate: `yarn compile`, `yarn lint`, `yarn prettier:check`, `yarn stylelint`,
`yarn test:jest` over the mithril/partial-sync suites (incl. **`MithrilPartialSyncService.spec.ts`**
for CAT-H) + `App.spec`.

---

## CAT-B — Extract `computeBehindByEpochs`; treat ≤ 0 as undefined; beacon-epoch fallback (Findings #11, #7, #16)

**Closes:** #11 (duplicated epoch math), #7 (misleading "about 1 epochs behind"), **#16 (D-702b-10 — the
`certifiedEpoch` fallback half: prefer `networkTip.epoch` when finite, else `certifiedEpoch`).**

**Exact files:**
- `source/renderer/app/utils/mithrilBehindness.ts`
- `source/renderer/app/utils/mithrilBehindness.spec.ts`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` (**#16** — pass
  `certifiedEpoch={mithrilPartialSync.certifiedEpoch}` next to `localTip`/`networkTip` at `:144-145`)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (**#16** — declare the `certifiedEpoch`
  observable, default `undefined`; populated by CAT-A)

**Edits checklist:**
- [ ] `mithrilBehindness.ts`: add `export const computeBehindByEpochs = (localTip, networkTip,
  certifiedEpoch?: number | null | undefined): number | undefined`. Logic (**hybrid, D-702b-10**): if
  `!Number.isFinite(localTip?.epoch)` → `undefined`; pick the **network-side anchor** =
  `Number.isFinite(networkTip?.epoch) ? networkTip.epoch : (Number.isFinite(certifiedEpoch) ?
  certifiedEpoch : undefined)`; if the anchor is `undefined` → `undefined`; else `diff = anchor −
  localTip.epoch`; return `diff > 0 ? diff : undefined`. **DISPLAY-ONLY.** Doc comment: returns
  `undefined` for diff ≤ 0 (node level/ahead) and that it is NOT the gate (the gate is the unchanged
  `isMithrilBehindnessKnown`); **`certifiedEpoch` (the Mithril certified-beacon epoch) is the early-sync
  fallback for the late-resolving `networkTip.epoch` — when omitted/undefined the helper behaves exactly
  as the networkTip-only version (no regression).**
- [ ] **`isMithrilBehindnessKnown` (`:18-22`) stays BYTE-IDENTICAL (D-702b-10 §4 / D-702b-2).** Do NOT add
  `certifiedEpoch` to it — it remains the **networkTip** sub-signal. The certified-known OR is composed at
  the **container** (CAT-A), not baked into this util, preserving the gate-vs-display decoupling.
- [ ] **Do NOT modify `isMithrilBehindnessKnown`** (`:18-22`) — it stays clamp-free, returns `true` at
  equal epochs (decoupling invariant).
- [ ] `mithrilBehindness.spec.ts`: add cases for `computeBehindByEpochs`: both tips finite & diff > 1
  → that diff; diff == 1 → 1; diff == 0 → `undefined`; diff < 0 → `undefined`; missing/ non-finite tip
  → `undefined`. **#16 hybrid-fallback cases:** `networkTip` null/non-finite **but** `certifiedEpoch`
  finite & `> localTip.epoch` → that diff (early-sync path); `networkTip` finite → result uses
  `networkTip.epoch` and **ignores** `certifiedEpoch` even when they differ (prefer-networkTip); both
  `networkTip` and `certifiedEpoch` absent → `undefined`; `certifiedEpoch` finite but `≤ localTip.epoch`
  → `undefined`; `localTip.epoch` non-finite (regardless of either anchor) → `undefined`. Keep the
  existing `isMithrilBehindnessKnown` cases unchanged (it still takes only `localTip`/`networkTip`).
- [ ] **Declare the store observable here (compile-order, D-702b-10).** CAT-B runs **before** CAT-A but
  both call sites below now read `mithrilPartialSync.certifiedEpoch`, so CAT-B adds `@observable
  certifiedEpoch: number | null | undefined = undefined;` to `MithrilPartialSyncStore.ts` (declaration
  only, default `undefined`). **CAT-A populates it** (in `_applyAvailability`) and adds the `certifiedKnown`
  OR to the container gate; **CAT-H produces it** in the backend probe + IPC payload. Until A+H land it
  stays `undefined` ⇒ `computeBehindByEpochs` degrades to networkTip-only (no regression). (CAT-B is the
  first of the four MithrilPartialSyncStore editors `B → A → D → E`, so this one-line add is collision-safe.)
- [ ] `DaedalusDiagnostics.tsx:570-577`: replace the `networkEpoch`/`localEpoch`/`behindByEpochs =
  Math.max(1, …)` block with `const behindByEpochs = computeBehindByEpochs(localTip, networkTip,
  certifiedEpoch);` (add the import; add a `certifiedEpoch?: number | null` prop). Delete the two
  intermediates. Wire the prop in `DaedalusDiagnosticsDialog.tsx` (`:144-145`, next to `localTip`/
  `networkTip`): `certifiedEpoch={mithrilPartialSync.certifiedEpoch}` (the sibling Mithril prop there is
  named `isMithrilPartialSyncSignificantlyBehind` at `:135-137`; add `certifiedEpoch` next to the existing
  `localTip={localTip}` `:144` / `networkTip={networkTip}` `:145`). The downstream consumer
  (`MithrilPartialSyncConfirmation.tsx:67-99`) already renders `behindUnknown` when `behindByEpochs` is
  undefined, so the confirmation modal's near-tip "1 epochs" is fixed too. **NOTE:** only the Mithril
  behind-by-epochs *figure* (`:574-577`) changes; the raw network-tip/local-tip diagnostic dump rows
  (`:742-779`) are untouched (they legitimately show "—" when `networkTip` is null).
- [ ] `MithrilProactivePromptContainer.tsx:45-52`: replace the `Math.max(1, …)` display block with
  `const behindByEpochs = computeBehindByEpochs(localTip, networkTip,
  mithrilPartialSync.certifiedEpoch);` (import added). **Leave the block above the gate for now — CAT-A
  reorders it.** The prompt consumer fallback (`SyncingConnectingMithrilPrompt.tsx:146-156`, message key
  `promptBodyUnknown` — not literally "behindUnknown") renders the unknown-behind fallback when undefined.

**New symbols:**
- `computeBehindByEpochs(localTip: TipInfo | null | undefined, networkTip: TipInfo | null | undefined,
  certifiedEpoch?: number | null | undefined): number | undefined` — exported from
  `utils/mithrilBehindness.ts` (**#16** adds the third param).
- `certifiedEpoch: number | null | undefined` (`@observable`, default `undefined`) on
  `MithrilPartialSyncStore` — **declared** here, **populated** in CAT-A.
- `DaedalusDiagnostics` prop `certifiedEpoch?: number | null` (**#16**).

**Acceptance:**
- Both call sites use the util; the `networkEpoch`/`localEpoch` intermediates are gone.
- `computeBehindByEpochs` returns `undefined` at diff ≤ 0; `isMithrilBehindnessKnown` is byte-for-byte
  unchanged. The confirmation modal and prompt show the `behindUnknown` fallback at the tip instead of
  "about 1 epochs behind."
- **#16:** with `networkTip` null but a finite `certifiedEpoch > localTip.epoch`, the util returns the
  certified diff (the figure is available early); with `networkTip` finite it prefers `networkTip.epoch`.
  When `certifiedEpoch` is `undefined` (the store field not yet populated by CAT-A/produced by CAT-H) the
  util behaves exactly as the networkTip-only version (no regression). The store observable exists and
  defaults `undefined`; `tsc` is clean at the end of CAT-B (no dangling reference to an unpopulated field).
- `mithrilBehindness.spec.ts` passes; `tsc`/`lint` clean.

**Tests:** new `computeBehindByEpochs` cases in `mithrilBehindness.spec.ts`; the existing
`MithrilPartialSyncConfirmation.spec.tsx` must still pass (undefined → `behindUnknown`).

---

## CAT-A — Proactive-prompt lifecycle: node-loaded trigger, near-tip hide, re-pop guard, perf reorder, drop dead inject, beacon-epoch gate (Findings #3, #4, #10, #14, #16)

**Closes:** #3 (D-702b-1 trigger/persist/near-tip), #4 (D-702b-3 re-pop guard), #10 (dead `actions`
inject), #14 (observer-perf reorder), **#16 (D-702b-10 — populate `certifiedEpoch` in `_applyAvailability`;
broaden the gate's anti-flash known-term to `(isBehindnessKnown || certifiedKnown)`; feed `certifiedEpoch`
to `computeBehindByEpochs`).** **Keeps the app-level mount + cross-screen persistence (REVERSES the
review's route-scoping).**

**Exact files:**
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (re-pop guard + **#16** `certifiedEpoch`
  population in `_applyAvailability`)
- `source/common/types/mithril-partial-sync.types.ts` (**#16** — add optional `certifiedEpoch?: number |
  null` to the `MithrilPartialSyncAvailability` payload type; CAT-H supplies the value)
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx`
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx`
- `source/renderer/app/App.tsx` (verify-only — the mount already exists at `:130`; **no edit expected**
  unless the comment at `:126-129` needs a one-line update to mention the node-loaded gate)
- `NetworkStatusStore.ts` (verify-only — `isConnected`/`isBehindnessKnown` already exist; **no edit**)

**Edits checklist:**
- [ ] **Store re-pop guard (D-702b-3).** In `MithrilPartialSyncStore.ts`, after
  `proactivePromptDismissedThisSession` (`:67`) add `@observable mithrilAttemptStartedThisSession =
  false;`. In `startPartialSync` (`:250-279`), set `this.mithrilAttemptStartedThisSession = true;`
  **before the optimistic `_updateStatus({ status: START_PENDING_STATUS, … })` call** (which is an
  object-literal snapshot, not a bare constant; the literal first statement in `startPartialSync` is
  `let startError: unknown;`, so insert the flag set right after that `let` and before the `_updateStatus`
  call). Session-scoped,
  in-memory, **never reset** (no idle reset). **Keep `proactivePromptDismissedThisSession`
  single-purpose** (still written only by `dismissProactivePrompt`).
- [ ] **Populate `certifiedEpoch` from the availability payload (#16, D-702b-10).** The observable is
  **declared in CAT-B** (`@observable certifiedEpoch: number | null | undefined = undefined;`); here CAT-A
  (1) adds `certifiedEpoch?: number | null` to the `MithrilPartialSyncAvailability` IPC payload type
  (`source/common/types/mithril-partial-sync.types.ts` — **optional**, so the renderer type-checks before
  CAT-H's backend production lands), and (2) sets `this.certifiedEpoch = availability.certifiedEpoch;` in
  `_applyAvailability` (`:219-227`, next to `behindByImmutables` `:226`). **Compile-order:** the consumer
  owns the *optional type field* (added here) so the read type-checks pre-CAT-H; CAT-H supplies the
  *value*. Until CAT-H lands the field is `undefined` ⇒ the gate's `certifiedKnown` is false and the figure
  degrades to networkTip-only (no regression).
- [ ] **Drop dead `actions` inject (#10).** In `MithrilProactivePromptContainer.tsx`, change
  `@inject('stores', 'actions')` (`:31`) → `@inject('stores')`; remove `actions: null` from
  `defaultProps` (`:34-37`); narrow `Props` so it no longer requires `actions` (e.g. a `{ stores:
  StoresMap }` shape, or keep `InjectedProps` but stop referencing `actions`). The container never used
  `actions`.
- [ ] **Gate + near-tip hide + perf reorder (#3, #14).** Rewrite `render()` so the **cheap boolean
  gate short-circuits first** and the figure is computed **after** the early return (#14), then a
  **second-stage near-tip hide** uses the CAT-B display helper:
  ```tsx
  render() {
    const { networkStatus, mithrilPartialSync } = this.props.stores;
    const { localTip, networkTip, isConnected, isBehindnessKnown } = networkStatus;
    const { certifiedEpoch } = mithrilPartialSync; // #16 (D-702b-10): early-sync beacon anchor

    // #16: combined known-ness = local epoch finite AND (live network tip finite OR certified epoch
    // finite). The named NetworkStatusStore.isBehindnessKnown stays the networkTip sub-signal; the
    // certified OR is composed HERE (keeps the util/gate decoupling — D-702b-2/§4 of D-702b-10).
    const certifiedKnown =
      Number.isFinite(localTip?.epoch) && Number.isFinite(certifiedEpoch);

    const isGated =
      mithrilPartialSync.status === 'idle' &&
      mithrilPartialSync.isPartialSyncEnabled &&
      mithrilPartialSync.isSignificantlyBehind &&   // backend offer signal (near-tip ⇒ false)
      isConnected &&                                 // D-702b-1(a): node loaded, past verifying
      (isBehindnessKnown || certifiedKnown) &&       // #16: anti-flash known-gate, now beacon-aware
      !mithrilPartialSync.mithrilAttemptStartedThisSession && // D-702b-3 re-pop guard
      !mithrilPartialSync.proactivePromptDismissedThisSession;

    if (!isGated) return null;

    // Display figure + near-tip hide (D-702b-1(c)): undefined when local ≥ chosen anchor.
    // #16: hybrid anchor — prefer networkTip.epoch when finite, else certifiedEpoch.
    const behindByEpochs = computeBehindByEpochs(localTip, networkTip, certifiedEpoch);
    if (behindByEpochs === undefined) return null;

    return (
      <SyncingConnectingMithrilPrompt
        behindByEpochs={behindByEpochs}
        onStart={mithrilPartialSync.startPartialSync}
        onDismiss={mithrilPartialSync.dismissProactivePrompt}
      />
    );
  }
  ```
  - **`isConnected`** is the node-loaded / past-verifying pin (`NetworkStatusStore.ts:836-839`;
    `isVerifyingBlockchain` is `!isConnected && …`, so `isConnected` implies past-verifying).
  - **`isBehindnessKnown`** stays as the cheap, named anti-flash short-circuit (it tracks the tip
    observables so the observer re-renders when the network tip arrives, and it short-circuits BEFORE
    `computeBehindByEpochs` is evaluated — satisfying #14).
  - **`behindByEpochs === undefined ⇒ return null`** is the near-tip hide tied to `computeBehindByEpochs
    ≤ 0` (D-702b-1(c)). With the backend `isSignificantlyBehind` also flipping false near tip (kept fresh
    by the CAT-E always-on poll), the prompt dismisses automatically once the node catches up, AND the
    misleading "about 1 epochs behind" is eliminated at the source.
  - Update the top-of-file comment to also state the **node-loaded (`isConnected`)** trigger and the
    **near-tip hide**; keep the persistence + mutual-exclusion narrative.

**New symbols:**
- `mithrilAttemptStartedThisSession: boolean` (`@observable`, default `false`) on
  `MithrilPartialSyncStore`; set `true` in `startPartialSync`.
- **#16:** `MithrilPartialSyncAvailability.certifiedEpoch?: number | null` (IPC payload type); the
  `certifiedEpoch` observable populated in `_applyAvailability`; local `certifiedKnown` in the container
  `render()`.

**Decoupling / anti-regression invariants (must hold):**
- `isMithrilBehindnessKnown` / `NetworkStatusStore.isBehindnessKnown` are **unchanged** and stay
  clamp-free (true at equal epochs). The near-tip ≤ 0 logic lives only in `computeBehindByEpochs`
  (display) and is consumed in the **container** render, never baked into the gate computed.
- **#16 (D-702b-10):** the container's anti-flash known-term broadens from `isBehindnessKnown` to
  `(isBehindnessKnown || certifiedKnown)`, where `certifiedKnown = finite(localTip.epoch) &&
  finite(certifiedEpoch)`. The **named** util `isMithrilBehindnessKnown(localTip, networkTip)` and
  `NetworkStatusStore.isBehindnessKnown` remain byte-identical (still the networkTip sub-signal); the OR is
  composed only in the container, so the gate-vs-display decoupling (D-702b-2) holds. The display helper's
  hybrid anchor (`networkTip.epoch ?? certifiedEpoch`) lives only in `computeBehindByEpochs`.
- **KEEP BOTH `isBehindnessKnown` AND the near-tip `behindByEpochs === undefined` early-return — do
  NOT collapse them (reviewer ruling F2).** Although `behindByEpochs === undefined` logically subsumes
  `isBehindnessKnown` (both require finite tips), the two-stage structure is **required, not optional**:
  (1) collapsing breaks the existing anti-flash test at `MithrilProactivePromptContainer.spec.tsx:98-110`,
  which sets `isBehindnessKnown:false` with **finite** tips (100/97) and expects no render — a lone
  `undefined` check would render there (97 < 100 ⇒ diff 3); (2) it violates the D-702b-2 gate-vs-display
  decoupling invariant (the gate must not depend on the display helper); (3) it loses the #14 perf
  short-circuit (`isBehindnessKnown` is the cheap named boolean that returns BEFORE `computeBehindByEpochs`
  is evaluated). `isBehindnessKnown` is the named anti-flash short-circuit; the `undefined` early-return is
  the near-tip display-availability hide. Both stay.
  - **#16 leaves F2 intact.** The first stage becomes `(isBehindnessKnown || certifiedKnown)` — still a
    cheap boolean short-circuit before `computeBehindByEpochs`. The `:98-110` mock keeps `certifiedEpoch`
    **undefined** ⇒ `certifiedKnown` false ⇒ the OR is false ⇒ still no render; and a *lone* `undefined`
    check would STILL render there (the figure prefers the finite `networkTip` ⇒ `100 − 97 = 3`, defined),
    so both stages remain required. (Add `certifiedEpoch: undefined` to the default mock — see Tests.)

**Acceptance:**
- The prompt does **not** flash during connecting / "verifying blockchain"; it appears only once
  `isConnected && (isBehindnessKnown || certifiedKnown)` AND `computeBehindByEpochs > 0`; it persists
  across every route (app-level mount unchanged) until the user picks Standard or Mithril this session.
- **#16:** during early/mid sync — `networkTip` not yet resolved but a finite `certifiedEpoch >
  localTip.epoch` — the prompt now **appears** (with the certified-anchored figure) instead of staying
  suppressed; this is the core defect fixed. When `networkTip` later resolves the figure may tick **up**
  by the certified-frontier lag (commonly ~1 epoch, occasionally more); never down — accepted, not clamped.
- After any Mithril attempt begins this session, the prompt never re-offers regardless of the terminal
  outcome (completed/cancelled/failed/restart-normal).
- The prompt hides automatically once the node reaches the tip (near-tip hide + backend signal).
- The container no longer injects `actions`; the figure is computed only when the prompt renders.

**Tests (`MithrilProactivePromptContainer.spec.tsx`):** extend the `renderContainer` default
`networkStatus` mock to add `isConnected: true`, and the default `mithrilPartialSync` mock to add
`mithrilAttemptStartedThisSession: false` **and `certifiedEpoch: undefined` (#16)** so the existing
networkTip-based cases (incl. the `:98-110` anti-flash case) are unaffected. **Also drop `actions={{} as
any}` from the `renderContainer` render helper (`:48-52`):** CAT-A narrows the container's inject to
`@inject('stores')` and drops `actions` from `Props`, so the spec must stop passing `actions` to the
rendered container — confirm with `yarn compile`. Add cases:
- `isConnected: false` → renders nothing (node-loaded gate).
- near-tip: `localTip`/`networkTip` with diff ≤ 0 (e.g. equal epochs) → renders nothing (near-tip
  hide), even though `isBehindnessKnown` is true.
- `mithrilAttemptStartedThisSession: true` → renders nothing (re-pop guard).
- **#16 early-sync render:** `isConnected: true`, `isSignificantlyBehind: true`, `networkTip: null`
  (so `isBehindnessKnown: false`), `localTip.epoch` finite, **`certifiedEpoch` finite and >
  `localTip.epoch`** → the prompt **renders** with the certified-anchored figure (the defect-fix case
  that previously stayed suppressed).
- **#16 prefer-networkTip:** both `networkTip.epoch` and `certifiedEpoch` finite but different → the
  figure uses `networkTip.epoch` (assert the rendered behind-by value reflects the networkTip diff, not
  the certified diff).
- keep the existing all-pass / non-idle / disabled / not-significantly-behind / `isBehindnessKnown:
  false` (now with `certifiedEpoch: undefined`) / dismissed cases green.
Add store-spec cases (`MithrilPartialSyncStore.spec.ts`): `startPartialSync` sets
`mithrilAttemptStartedThisSession = true`; **#16: `_applyAvailability` with a payload carrying
`certifiedEpoch` sets the observable, and a payload without it leaves `certifiedEpoch` `undefined`.**

---

## CAT-C — Order-agnostic shared error view; ordering + opt-in right-align in the overlay caller; fix decision view; restore bootstrap default; correct the doc note (Findings #1, #5)

**Closes:** #1 (destructive bootstrap primary silently reordered), #5 (decision-view mirror-image).

**Exact files:**
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss` (+ regen `.d.ts`)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx` (verify-only — keep
  the default untouched)
- specs: `MithrilPartialSyncOverlay.spec.tsx`, the bootstrap `MithrilErrorView`/`MithrilDecisionView`
  spec(s) if present
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-702a-impl-review.md`

**Edits checklist:**
- [ ] **Make `MithrilErrorView` render-order-agnostic.** In `MithrilErrorView.tsx`: delete the
  `orderedActions` filter (`:111-116`); map `resolvedActions` **verbatim** in the `.actions` block
  (`:168-182`). The default `resolvedActions` (`:99-110`, `[wipeAndRetry(primary), decline(secondary)]`)
  is rendered in caller order — primary first — so the bootstrap default returns to **primary-on-left**.
- [ ] **Add opt-in right-alignment.** Add a Prop `rightAlignActions?: boolean` (or `actionsAlignment?:
  'start' | 'end'`). Apply it as a className modifier on the `.actions` container, e.g.
  `className={classNames([styles.actions, rightAlignActions && styles.actionsRightAligned])}` (import
  `classnames`). Default (no prop) = left-aligned.
- [ ] **Scope the `.scss` `flex-end` (D-702b-5(3)).** In `MithrilErrorView.scss`: change `.actions`
  `justify-content: flex-end` → `flex-start` (`:75-79`); add an opt-in modifier
  `.actionsRightAligned { justify-content: flex-end; }`. **Keep `.hintBody` (`:31-36`) untouched** (it is
  CAT-E's live cancelled-hint class). **Regen `MithrilErrorView.scss.d.ts`** (new class).
- [ ] **Overlay caller owns ordering + right-align.** In `MithrilPartialSyncOverlay.tsx`: build
  `errorActions` (currently primary-first, `:101-151`) into display order **secondary-first,
  primary-last** before passing it down — move the old `orderedActions` filter logic here (sort
  non-primary then primary), OR construct the array in that order. Pass `rightAlignActions` (or
  `actionsAlignment="end"`) on the `MithrilErrorView` usage (`:216-223`). This keeps the cancelled/error
  overlay **primary-on-the-right** (per manual-assessment D-702a-2). Do **not** touch the completed
  branch (CAT-D owns it). **GUARDRAIL (locked invariant #2):** this is a **stable reorder** of the SAME
  action set (secondary-first, primary-last) — it must NOT filter, drop, or add any action;
  `allowedRecoveryActions` still determines membership. Reorder by partitioning into non-primary then
  primary (preserving each group's relative order), exactly as the old `orderedActions` did — only the
  location moves (to the caller).
- [ ] **Restore the bootstrap default (verify).** `MithrilBootstrap.tsx:207-213` renders
  `MithrilErrorView` with **no** `actions` and **no** `rightAlignActions` → default
  `[Wipe chain & retry (primary), Decline]`, left-aligned, primary-first — exactly as before 702a. **No
  edit** beyond confirming this.
- [ ] **Fix `MithrilDecisionView` DOM order (#5).** In `MithrilDecisionView.tsx:97-112`, swap the two
  `<Button>`s so `secondaryAction` (decline) is rendered **before** `primaryAction` (accept). Keep
  `MithrilDecisionView.scss:72-76` `flex-end` ⇒ primary renders on the right, matching the corrected
  error view + the `widgets/Dialog.tsx` convention (caller lists primary last → right).
- [ ] **Correct the doc note.** In `task-ux-702a-impl-review.md:533-537`, correct the "Shared-error-view
  note (#11, sanctioned visual-only … No logic/handler change)" claim: the 702a `orderedActions` change
  was a **real DOM child reorder** of a destructive action in the bootstrap default that moved
  "Wipe chain & retry" to the far right — **not** visual-only, and it contradicted
  `task-ux-702a-decisions.md:167-168` ("visual-only … do not regress the empty-chain bootstrap flow").
  Note the remediation: 702b makes the error view order-agnostic and restores the bootstrap default.

**New symbols:**
- `MithrilErrorView` Prop `rightAlignActions?: boolean` (or `actionsAlignment?: 'start' | 'end'`).
- `MithrilErrorView.scss` class `.actionsRightAligned`.

**Acceptance:**
- Empty-chain bootstrap error dialog: `[Wipe chain & retry (primary), Decline]`, **left-aligned,
  primary-first** (restored).
- Partial-sync overlay error/cancelled dialog: secondary-first, **primary on the right**, right-aligned
  (opt-in).
- `MithrilDecisionView`: `[Decline (secondary), Accept (primary)]` in the DOM, primary on the right.
- The 702a impl-review note is corrected.

**Tests:** the overlay cancelled/error spec keeps its DOM-order assertion (`['Restart Node Sync
(slow)','Retry Mithril Sync (fast)']`) green; add/adjust a bootstrap error-view assertion that the
default renders the wipe primary **first** (left); `MithrilDecisionView` spec (if present) asserts
decline-before-accept DOM order. No i18n change.

---

## CAT-D — Completion finalize: renderer-only robustness (Finding #2)

**Closes:** #2 (fire-and-forget finalize + optimistic hide → unhandled rejection + staging leak).
**Renderer-only; NO backend change.**

**Exact files:**
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- specs: `MithrilPartialSyncStore.spec.ts`, `MithrilPartialSyncOverlay.spec.tsx`

**Edits checklist:**
- [ ] **`dismissCompletedOverlay` — await before hide + catch + resync.** Rewrite `:229-241` to await
  the finalize **before** flipping `isCompletedOverlayDismissed`, and resync on any outcome, reusing the
  store's `try { … } finally { await this.syncStatus(); }` pattern (as in
  `cancelPartialSync`/`startPartialSync`):
  ```ts
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') return;
    try {
      await mithrilPartialSyncFinalizeChannel.request();
      this.isCompletedOverlayDismissed = true; // flip ONLY on success
    } catch (error) {
      logger.warn('MithrilPartialSyncStore: failed to finalize completed overlay', { error });
      // keep the overlay up; the resync below reflects the true backend state
    } finally {
      await this.syncStatus();
    }
  };
  ```
  Note in the code comment: the backend `_resetToIdleStatus()` runs before `fs.remove`, so on failure
  the backend is already idle and `syncStatus()` will pull `idle` (overlay hides via status); the
  renderer no longer **optimistically** hides nor leaks an unhandled rejection, and if a future backend
  reorder leaves status at `completed` on failure, the overlay correctly persists (no blind flip).
- [ ] **`.catch` the overlay timeout (no fire-and-forget).** In `MithrilPartialSyncOverlay.tsx:84-91`,
  wrap the `onDismissCompleted()` call so a finalize rejection can never become an unhandled rejection:
  ```ts
  const timer = setTimeout(() => {
    Promise.resolve(onDismissCompleted()).catch(() => {});
  }, COMPLETED_AUTO_DISMISS_DELAY_MS);
  ```
  Widen the `onDismissCompleted` Prop type to `(): void | Promise<void>` so the `Promise.resolve(...)`
  wrap type-checks. Keep `COMPLETED_AUTO_DISMISS_DELAY_MS`, the `status !== 'completed'` guard, and the
  `clearTimeout` cleanup.

**Acceptance:**
- On finalize success: `isCompletedOverlayDismissed` flips **after** the awaited finalize, `syncStatus()`
  runs, overlay hides cleanly (no premature optimistic hide).
- On finalize failure: `isCompletedOverlayDismissed` **stays `false`** and there is **no unhandled promise
  rejection**; `syncStatus()` resyncs the true backend state. **Note (plan-review correction):** today's
  backend resets to `idle` *before* the failed `fs.remove`, so that resync pulls `idle` and the overlay
  **hides anyway** — this change does **NOT** keep the overlay up and does **NOT** fix the staging/`.lock`
  leak (deferred to the out-of-scope backend reorder). The two genuine deliverables are: no unhandled
  rejection + no premature optimistic hide on success.
- No backend file is modified.

**Tests:**
- `MithrilPartialSyncStore.spec.ts`: finalize-success path (flag flips, `syncStatus` called) and a
  finalize-rejection path (flag stays false, no unhandled rejection, `syncStatus` called). Mock the
  finalize channel to resolve / reject.
- **Existing store-spec test must stay green** (`MithrilPartialSyncStore.spec.ts:~639`, "live completed
  push keeps overlay shown; `dismissCompletedOverlay` flips it off and calls finalize once"): after this
  rewrite the flag flips **after** the awaited finalize (not before), and the new `finally { await
  this.syncStatus(); }` issues an **extra** status-channel request on the finalize path. Ensure that
  test's status-channel mock **tolerates** the added finalize-path `syncStatus()` call (resolves a status
  response) and that its finalize-called-once assertion still holds.
- `MithrilPartialSyncOverlay.spec.tsx`: the completed auto-dismiss test (fake timers) asserts the
  timeout-fired `onDismissCompleted` is invoked and that a rejecting `onDismissCompleted` does not throw
  out of the timer (no unhandled rejection).
- **Failure-path resync (plan-review):** mock the finalize channel to reject AND the status channel to
  resolve an `idle` snapshot; assert `dismissCompletedOverlay` does not throw (no unhandled rejection),
  `isCompletedOverlayDismissed` stays `false`, `syncStatus()` is called, and the resulting
  `shouldShowOverlay === false` (overlay hides because the backend is idle — documents the real behavior
  so a later reviewer does not "fix" the test toward a keep-overlay-up intent that today's backend cannot
  deliver).

---

## CAT-E — Availability poll: bound the request (anti-pin) + known-stable back-off; keep the idle poll (Findings #6, #9)

**Closes:** #6 (re-entrancy pin — no request timeout), #9 (poll runs all session). **Do NOT restore the
`isWorking` guard** (idle-time consumers depend on the unconditional poll).

**Scope split with CAT-H (read first).** CAT-E addresses **how often** the poll fires (the cadence
back-off) and the re-entrancy pin. It is **renderer-only** and does **NOT** reduce the **per-probe
cost**. The per-probe cost — each tick forking `checkDiskSpace` (df/PowerShell) + re-reading the
`immutable/` dir — is **#15 / CAT-H / D-702b-9** (backend). This matters because the back-off here is
**gated off while the node is behind** (the catch-up window): `_isAvailabilityStable()` is
`!isPartialSyncEnabled || !isSignificantlyBehind`, so while `isSignificantlyBehind` is true the poll
stays at 30s and every tick pays the full backend cost — which is exactly why CAT-H is required and not
optional. Do not re-scope CAT-E to the backend; the per-call cost is owned by CAT-H.

**Exact files:**
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

**Edits checklist:**
- [ ] **Bound `request()` (#6).** Add `const AVAILABILITY_REQUEST_TIMEOUT_MS = 10_000;` near
  `AVAILABILITY_REFRESH_INTERVAL` (`:36`). In `_refreshAvailability` (`:200-216`), wrap the
  `mithrilPartialSyncAvailabilityChannel.request()` (`:207`) in a timeout so a wedged main process can
  never keep `_isRefreshingAvailability` pinned `true`. Implement a local helper that races the request
  against a timeout-reject **and clears the timer** to avoid a dangling timeout:
  ```ts
  let timeoutId: ReturnType<typeof setTimeout> | undefined;
  const withTimeout = new Promise<never>((_, reject) => {
    timeoutId = setTimeout(
      () => reject(new Error('availability request timed out')),
      AVAILABILITY_REQUEST_TIMEOUT_MS
    );
  });
  try {
    const availability = await Promise.race([
      mithrilPartialSyncAvailabilityChannel.request(),
      withTimeout,
    ]);
    this._applyAvailability(availability);
  } catch (error) { /* existing logger.warn */ }
  finally {
    if (timeoutId) clearTimeout(timeoutId);
    this._isRefreshingAvailability = false;
  }
  ```
  (A generation-token reset is the sanctioned alternative; the `Promise.race` timeout is recommended —
  simpler and fake-timer testable. Keep it local to the store.)
- [ ] **Known-stable back-off (#9) — gated so it CANNOT engage on the premature first read (HIGH-1).**
  Add `const AVAILABILITY_REFRESH_BACKOFF_INTERVAL = 300_000;` (5 min) and
  `const STABLE_READS_BEFORE_BACKOFF = 2;`, plus a private counter `_consecutiveStableReads = 0`. The
  stability predicate stays `_isAvailabilityStable() => !this.isPartialSyncEnabled ||
  !this.isSignificantlyBehind` (disabled ⇒ never available; enabled-and-not-behind ⇒ caught up), **but a
  single stable read MUST NOT slow the poll.** **Why (ISSUE-1, documented verbatim at
  `MithrilPartialSyncStore.ts` setup ~:83-91):** a probe that lands before the backend behind-ness
  settles reports `isSignificantlyBehind:false` while the node IS behind, so `enabled && !behind ⇒
  stable`; backing off on that transient first read would slow the poll to the 5-min cadence **before**
  the probe settles and re-surface the exact ISSUE-1 regression (Diagnostics/prompt show "not behind /
  no offer" for minutes). Gate the back-off behind a **settled signal — require ≥
  `STABLE_READS_BEFORE_BACKOFF` (2) CONSECUTIVE stable reads before slowing.** After `_applyAvailability`,
  evaluate `_isAvailabilityStable()`:
  - **stable read:** increment `_consecutiveStableReads`; only when it reaches `STABLE_READS_BEFORE_BACKOFF`
    **and** the poll is currently fast, **slow** the poll (re-arm at `AVAILABILITY_REFRESH_BACKOFF_INTERVAL`).
    The premature first stable read leaves the counter at **1** — the poll stays at 30s, the fast poll keeps
    running and self-corrects (ISSUE-1 preserved).
  - **unstable read:** reset `_consecutiveStableReads = 0` and **ALWAYS re-arm the fast 30s interval** (a
    fall-behind after back-off returns to 30s immediately; re-arming an already-fast interval is a harmless
    no-op re-arm).
  Factor the interval (re)arm into a small `_armAvailabilityInterval(intervalMs)` helper used by `setup()`
  and the back-off; it clears any existing `_availabilityRefreshInterval` before arming the new cadence.
  **SLOW, never STOP** — a stopped poll would strand the idle consumers (the proactive prompt and the
  Diagnostics dialog). **Preserve the first-load self-correct (ISSUE-1):** the initial `_refreshAvailability()`
  in `setup()` and the fast 30s interval until ≥2 consecutive stable reads are unchanged. Update `teardown()`
  only if the helper changes the field it clears (`_availabilityRefreshInterval`).
- [ ] **Keep the idle poll.** The poll still fires regardless of `isWorking` — the proactive prompt
  (`status==='idle'` gate) and the Diagnostics dialog remain served.

**New symbols:**
- consts `AVAILABILITY_REQUEST_TIMEOUT_MS`, `AVAILABILITY_REFRESH_BACKOFF_INTERVAL`,
  `STABLE_READS_BEFORE_BACKOFF` (= 2).
- private counter `_consecutiveStableReads` (default 0; reset to 0 on every unstable read).
- private helpers `_isAvailabilityStable()` and `_armAvailabilityInterval(intervalMs)`.

**Acceptance:**
- A never-settling availability `request()` no longer pins `_isRefreshingAvailability`; after the
  timeout the guard clears and the next tick can refresh.
- The back-off **cannot** engage on a premature first not-behind read: a single stable read leaves the
  poll at 30s (counter at 1); the poll only slows after **≥2 consecutive** stable reads, so the ISSUE-1
  first-load self-correct is preserved.
- Once availability is settled-stable (disabled, or enabled-and-not-behind for ≥2 consecutive reads) the
  poll backs off to the slower interval; on the **first** unstable read (node falls behind again) it
  resets the counter and re-arms to 30s immediately. The poll **slows, never stops**, so idle consumers
  (the proactive prompt and the Diagnostics dialog) keep updating.

**Tests (`MithrilPartialSyncStore.spec.ts`):**
- Inject a never-settling request; advance fake timers past `AVAILABILITY_REQUEST_TIMEOUT_MS`; assert
  the guard cleared and a subsequent tick issues a fresh request.
- Assert the existing idle-refresh test (refresh fires while idle) still passes (the `isWorking` guard
  stays removed).
- **(HIGH-1 i) a premature not-behind read does NOT slow the poll:** a single stable read (enabled &&
  `isSignificantlyBehind:false`) leaves the interval at 30s — advance one back-off worth of fake time and
  assert the poll still fired at the 30s cadence (counter at 1, not yet ≥2). Only a SECOND consecutive
  stable read re-arms the interval at `AVAILABILITY_REFRESH_BACKOFF_INTERVAL`.
- **(HIGH-1 ii) a later fall-behind re-arms to 30s:** after the poll has backed off (≥2 consecutive
  stable reads), feed an unstable read (enabled && `isSignificantlyBehind:true`) and assert the counter
  resets and the interval returns to the fast 30s cadence.
- (Keep the teardown-clears-interval test green.)

---

## CAT-H — Backend probe-cost cache + beacon-epoch production (Findings #15, #16)

**Closes:** #15 (grill-added 2026-06-30 — the probe-cost cache) **and the backend/producer half of #16
(D-702b-10): extract `beacon.epoch` (multi-path, undefined-safe) and surface it as `certifiedEpoch` on the
behind-ness result + the `getPartialSyncAvailability` payload, so the renderer (CAT-B/CAT-A) can anchor the
early-sync behind-by-epochs figure on the horizon-free beacon.** Each enabled availability probe forks
`checkDiskSpace` (df/PowerShell) **and** re-reads the whole `immutable/` dir, both uncached. **Backend-only;
complements CAT-E** (CAT-E = poll frequency; CAT-H = per-probe cost). This is the only fix that lowers the
per-tick CPU in **both** states (behind and caught-up), including the catch-up window where CAT-E's back-off
is gated off. **#16 rides the same beacon the probe already resolves — nearly free, and consistent with the
immutable number (same beacon → same frontier).**

**Why this is the right fix (grounded):** `getPartialSyncBehindness`
(`MithrilPartialSyncService.ts:669-698`) already caches the aggregator
(`_getCachedLatestCertifiedImmutableNumber`, `:645-660`, 5-min TTL) but recomputes the two **local**
inputs every call: `getManagedChainPath()` (`:675-676`; the pure helper at
`source/main/utils/chainStoragePathResolver.ts:14-20`, invoked via `chainStorageManager.ts:194-197` — the
`getConfig()` it calls is what forks `checkDiskSpace`) transitively forks `checkDiskSpace`
(`chainStorageManagerConfig.ts:23`, full path `source/main/utils/chainStorageManagerConfig.ts`, via
`getConfig → getDefaultStorageConfig`) even though it only uses
`config.customPath` and discards the `free`-bytes figure; and `resolveLocalImmutableNumber()`
(`:677-680`) does a full `fs.readdir(immutable/)` + ~5 `stat`/`access`
(`mithrilPartialSyncPreflight.ts:63-141`). Mirroring the existing aggregator cache for the **local**
read deduplicates **both** on a cache hit (on a hit we never resolve the path, so the fork never
happens).

**Exact files:**
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/mithrilSnapshotMetadata.ts` (**#16** — `extractCertifiedEpoch` multi-path
  extractor; `certifiedEpoch` on `ResolvedLatestSnapshot`; set it in `normalizeResolvedLatestSnapshot`)
- `source/main/mithril/mithrilSnapshotMetadata.spec.ts` (**#16** — extractor cases)
- `source/main/mithril/MithrilController.ts` (**#16 — VERIFY-ONLY, no edit.** `getPartialSyncAvailability`
  `:167-174` already returns `{ isEnabled, ...behindness }` `:173`, so a `certifiedEpoch` on the
  behind-ness result **auto-forwards** via the spread; the disabled short-circuit `:169-171` returns no
  epoch. Confirm the spread carries it; no code change.)

**Edits checklist:**
- [ ] **PRE-STEP (operator gate — do before writing the extractor).** Run `cardano-db snapshot show
  latest --json` (or hit the aggregator REST snapshot endpoint) against the **pinned-fork** aggregator
  (`flake.nix:23`) and record the exact beacon epoch key (`beacon.epoch` vs `beacon.epoch_number` vs
  `cardano_db_beacon.epoch`). Write `extractCertifiedEpoch`'s `explicitPaths` with the confirmed key
  FIRST. This converts #16 from "ship inert and hope" to "extractor written against the verified shape."
  If the operator cannot run it before implementation, implement with the multi-path default BUT mark #16
  **not done** until the live key is confirmed (the early-sync fix is silently inert until then).
- [ ] **Add `_localImmutableCache` + `_getCachedLocalImmutableNumber()`.** Next to
  `_latestCertifiedImmutableCache` (`:111-114`) add `_localImmutableCache: { value: number; fetchedAt:
  number } | null = null;`. Add a private `async _getCachedLocalImmutableNumber(): Promise<number>`
  that mirrors `_getCachedLatestCertifiedImmutableNumber` (`:645-660`): on a fresh hit (within
  `PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS`) return `this._localImmutableCache.value`; on a miss resolve
  `getManagedChainPath()` → `resolveLocalImmutableNumber()` (the **only** place these run), store
  `{ value, fetchedAt: now }`, return. **Reuse the existing 5-min TTL const (`:72`)** — do not add a new
  interval const.
- [ ] **Switch `getPartialSyncBehindness` to the cached local read.** In the `try` block (`:673-689`)
  replace the inline `getManagedChainPath()` + `resolveLocalImmutableNumber()` (`:675-680`) with `const
  localImmutableNumber = await this._getCachedLocalImmutableNumber();`. Leave the **gap/threshold logic**
  (`gap = latest - localImmutableNumber`, the `gap <= 0` guard `:682-685`, the threshold compare `:687`,
  the degrade-to-not-behind `catch` `:690-697`) **byte-for-byte unchanged** — the result is still computed
  fresh from `cachedLatest − cachedLocal` each call. A cache **miss** still throws into the same
  `catch` (degrade to not-behind), so error behavior is preserved. **Scope note:** "byte-for-byte
  unchanged" is the gap/threshold *computation*, not the return literals — the #16 step below *appends* an
  optional `certifiedEpoch` to the **success** (`:686-689`) and **`gap <= 0`** (`:682-685`) returns,
  keeping `behindByImmutables: gap` as the existing unconditional literal (the `catch` return stays
  literal).
- [ ] **Invalidate both caches on lifecycle transitions.** Add a private
  `_invalidateBehindnessCaches(): void { this._latestCertifiedImmutableCache = null;
  this._localImmutableCache = null; }`. Call it at every point where the local immutable position (or
  the offer) can change so the next probe is fresh: at the **start** of `start()` (`:133`, after the
  in-progress guard) and `cancel()` (`:264`), and inside `_resetToIdleStatus()` (`:576-587`) — which
  already runs on `restartNormal`/`wipeAndFullSync`/`finalizeWipeAndFullSync`/
  `finalizeCompletedPartialSync`. (Folding it into `_resetToIdleStatus()` covers all four reset paths
  in one place; `start()`/`cancel()` get explicit calls because they do **not** route through it.)
  **Rationale:** invalidating on completion is what makes the proactive prompt + Diagnostics flip to
  "caught up" promptly after a cutover instead of waiting out the 5-min TTL.
- [ ] **(#16) Extract the beacon epoch (multi-path, undefined-safe).** In `mithrilSnapshotMetadata.ts`,
  add `export const extractCertifiedEpoch = (raw): number | null` mirroring
  `extractLatestCertifiedImmutableNumber` (`:66-88`) — reuse `getNestedValue` + `toPositiveInteger`, walk
  `explicitPaths`: `['beacon','epoch']`, `['cardano_db_beacon','epoch']`, `['beacon','epoch_number']`,
  `['cardanoDbBeacon','epochNumber']`, `['epoch']` → first hit wins, else `null`. Add `certifiedEpoch:
  number | null` to `ResolvedLatestSnapshot` and set it in `normalizeResolvedLatestSnapshot` (`:124-146`).
  **Do NOT gate the existing `latestCertifiedImmutableNumber == null ⇒ return null` on the epoch** — a
  present immutable number with a `null` epoch must still resolve (the immutable path stays the offer
  signal; `certifiedEpoch` is best-effort/optional).
- [ ] **(#16) Carry the epoch on the aggregator cache + return it from the probe.** Extend
  `_latestCertifiedImmutableCache` (`:111-114`) to `{ value: number; epoch: number | null; fetchedAt:
  number }`; `_getCachedLatestCertifiedImmutableNumber` (`:645-660`) stores `snapshot.certifiedEpoch` too
  (keep its return type a `number` for the immutable value; expose the epoch via a sibling `private
  _getCachedCertifiedEpoch(): number | null` (do NOT change `_getCachedLatestCertifiedImmutableNumber`'s
  `number` return). Both `_show…`/`_list…` snapshot paths funnel through the single
  `normalizeResolvedLatestSnapshot`, so setting `certifiedEpoch` there covers every
  `ResolvedLatestSnapshot` construction). In `getPartialSyncBehindness` (`:669-698`) add
  `certifiedEpoch` to the **success** and `gap <= 0` returns via a conditional spread, **keeping
  `behindByImmutables` as the existing unconditional literal** (the live success return at `:686-689` sets
  `behindByImmutables: gap` unconditionally — `gap > 0` there — so do NOT refactor it into a spread):
  `{ isSignificantlyBehind: gap >= …, behindByImmutables: gap, ...(certifiedEpoch != null ? { certifiedEpoch
  } : {}) }` and `{ isSignificantlyBehind: false, ...(certifiedEpoch != null ? { certifiedEpoch } : {}) }`.
  Leave the degrade-to-not-behind `catch` unchanged (⇒ `certifiedEpoch` undefined there, which the renderer
  treats as "no early anchor"). The same cache invalidation (`_invalidateBehindnessCaches`, above) already
  refreshes the epoch on lifecycle transitions. **Use the `!= null` guard, NOT an unconditional add:**
  `_getCachedCertifiedEpoch()` returns **`null`** (not `undefined`) when the beacon has no epoch, and Jest
  `toEqual` ignores `undefined` keys but **not** `null` ones, so an unconditional `certifiedEpoch: null`
  would break the exact-match cases. Of the four existing `getPartialSyncBehindness` cases only **two** carry
  `behindByImmutables` — `:1228` `{ isSignificantlyBehind: true, behindByImmutables: 20 }` and `:1241`
  `{ isSignificantlyBehind: false, behindByImmutables: 5 }`; the other two assert
  `{ isSignificantlyBehind: false }` only — `:1254` (gap ≤ 0) and `:1266` (catch). Their fixtures carry no
  beacon epoch ⇒ the conditional spread omits the key ⇒ all four stay green. (`behindByImmutables?` is
  optional only at the **type** level `:670-672`; the service emits it unconditionally on the success path —
  don't conflate type-optionality with the runtime return.)
- [ ] **(#16) Forward = free; verify MithrilController.** `getPartialSyncAvailability` `:173` returns
  `{ isEnabled, ...behindness }`, so `certifiedEpoch` rides the spread automatically — **no edit**, just
  confirm. The renderer payload type field (`MithrilPartialSyncAvailability.certifiedEpoch?`) is added by
  **CAT-A** (consumer-owns-optional-type, compile-order); CAT-H supplies the value.

**New symbols:**
- field `_localImmutableCache: { value: number; fetchedAt: number } | null` (default `null`).
- private `_getCachedLocalImmutableNumber(): Promise<number>`.
- private `_invalidateBehindnessCaches(): void`.
- **#16:** `extractCertifiedEpoch(raw): number | null` (exported, `mithrilSnapshotMetadata.ts`);
  `certifiedEpoch: number | null` on `ResolvedLatestSnapshot`; `epoch` on `_latestCertifiedImmutableCache`;
  `certifiedEpoch?` on the `getPartialSyncBehindness` return.

**Decoupling / anti-regression invariants (must hold):**
- **No ISSUE-1 regression.** A stale-low cached local read makes `gap = latest − local` **larger** ⇒
  reads as **more** behind (conservative), never as a premature not-behind. The premature-not-behind
  failure mode ISSUE-1 guards against comes from an unsettled aggregator/node and is unchanged here;
  CAT-E's ≥2-consecutive-stable-reads back-off gate is unaffected (CAT-H caches inputs, not the
  stable/unstable verdict).
- **Aggregator cache semantics preserved.** `_getCachedLatestCertifiedImmutableNumber` keeps its TTL;
  CAT-H only *adds* invalidation to it (a strict freshness improvement at lifecycle transitions).
- **No backend behavior change to the offer signal.** `isSignificantlyBehind` / `behindByImmutables`
  values are identical to today for any given (latest, local) pair; only the I/O to obtain `local` is
  deduplicated.
- **Disabled path untouched.** `MithrilController.getPartialSyncAvailability` still short-circuits
  before `getPartialSyncBehindness` when `isEnabled` is false (`MithrilController.ts:169-171`).
- **Normal-sync freshness tradeoff (see D-702b-9 correction):** with no lifecycle transition the local
  cache is refreshed only by the 5-min TTL, so the offer signal can read stale-high (conservative
  over-offer) for up to ~5 min during plain genesis sync; the prompt is protected by the renderer
  near-tip hide, Diagnostics is not. Accepted; documented.

**Acceptance:**
- Within the 5-min TTL, repeated `getPartialSyncBehindness()` calls resolve `getManagedChainPath()` and
  `resolveLocalImmutableNumber()` (hence `checkDiskSpace` + `fs.readdir`) **once**, not per call; after
  the TTL expires the next call re-resolves them once.
- After any lifecycle transition (`start`/`cancel`/`restart-normal`/`wipe`/`finalize-wipe`/
  `finalize-completed`) the next probe re-resolves **both** local and aggregator inputs (caches
  invalidated), so post-cutover behind-ness is fresh without waiting out the TTL.
- The behind-ness verdict for any (latest, local) pair is unchanged vs. today; the existing
  `getPartialSyncBehindness` spec cases (`MithrilPartialSyncService.spec.ts:1221-1269`) stay green.
- **#16:** when the beacon carries an epoch, `getPartialSyncBehindness` returns it as `certifiedEpoch`
  and `getPartialSyncAvailability` forwards it (spread); when the beacon has **no** epoch field (the
  pinned-fork risk), `certifiedEpoch` is `null`/absent and every existing behavior is byte-for-byte
  unchanged (the renderer degrades to networkTip-only — no regression). The immutable offer signal is
  unaffected by epoch presence/absence.

**Tests (`MithrilPartialSyncService.spec.ts`):** mirror the existing aggregator-cache test
(`:1271-1292`) — `Date.now` spy + the `stubLocalImmutableNumber` helper (`:1208-1219`, which spies
`getManagedChainPath` and drives `readdirMock`):
- **local-read caching:** spy `getManagedChainPath` (and/or assert `readdirMock` call count); two
  `getPartialSyncBehindness()` calls within the TTL ⇒ the spy fired **once**; a third call past the TTL
  ⇒ fired **twice**.
- **invalidation:** after a cached read, drive a lifecycle transition that resets to idle (e.g.
  `finalizeCompletedPartialSync()` with the `fs-extra`/marker mocks already in the suite, or call
  `restartNormal()` from an allowed boundary) and assert the next `getPartialSyncBehindness()` re-runs
  the local read (spy/`readdirMock` count increments without advancing `Date.now`).
- keep the existing `getPartialSyncBehindness` cases (`:1221-1292`) and the aggregator-cache test green.
- **#16 extractor (`mithrilSnapshotMetadata.spec.ts`):** `extractCertifiedEpoch` returns the epoch from
  each `explicitPaths` location. Mirror the **real** existing immutable fixture at `:30-32` —
  `cardano_db_beacon: { immutable_file_number: '25' }` (key `cardano_db_beacon`, string value) — i.e. add an
  epoch to that same shape, e.g. `cardano_db_beacon: { epoch: 320, immutable_file_number: '25' }`. Parse
  string epochs via `toPositiveInteger`, and return `null` when no epoch key is present (the
  pinned-fork-absent case) while `extractLatestCertifiedImmutableNumber` still returns the immutable number
  from the same payload.
- **#16 probe returns epoch:** a `getPartialSyncBehindness()` over a beacon with `{ epoch, immutable_file_
  number }` returns `certifiedEpoch` on the success result; a beacon without an epoch returns
  `certifiedEpoch: null`/absent with the immutable verdict unchanged.

---

## CAT-F — Shared `<CompletionBlock>`; remove dead `.primaryButton` CSS (Findings #12, #13)

**Closes:** #12 (three near-identical completion blocks), #13 (dead `.primaryButton`). **No i18n
changes.**

**Exact files:**
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss`
  (+ regen `.d.ts`)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`

**Edits checklist:**
- [ ] **Extract `CompletionBlock` (#12).** In `MithrilProgressView.tsx`, add a small local helper
  component (or function returning JSX) `CompletionBlock` with props `{ title: string; detail?: string;
  spinnerPosition?: 'top' | 'bottom' }` (default `'bottom'`). It renders the shared wrapper
  `div.completionBlock` + `role="status"` + `aria-live="polite"` + `aria-atomic="true"`, the `SVGInline`
  spinner (`styles.completionSpinner`, `aria-hidden`), `<h2 className={styles.completionTitle}>{title}`,
  and — only when `detail` is provided — `<p className={styles.completionDetail}>{detail}</p>`. When
  `spinnerPosition==='top'` the spinner renders **before** the title (and there is no detail); otherwise
  title → detail → spinner (matching the current stopping/starting layout). Replace the three inline
  blocks:
  - stopping (`:174-195`): `<CompletionBlock title={stoppingNodeTitle || intl.formatMessage(
    messages.nodeStoppingTitle)} detail={stoppingNodeDetail || intl.formatMessage(
    messages.nodeStoppingDetail)} />` (spinner bottom).
  - starting (`:197-218`): same shape with the `nodeStarting*` resolutions (spinner bottom).
  - completed-transition (`:220-234`): `<CompletionBlock title={completedTransitionLabel}
    spinnerPosition="top" />` (no detail).
  **Call sites keep their own `prop || intl.formatMessage(...)` resolution.** No scss change in this
  file (reuse `.completionBlock`/`.completionSpinner`/`.completionTitle`/`.completionDetail`). Bootstrap
  never passes `completedTransitionLabel`, so its completed frame is byte-for-byte unchanged (#12).
- [ ] **Remove dead `.primaryButton` (#13).** In `SyncingConnectingMithrilPrompt.scss`, delete the
  `.primaryButton` rule (`:62-68`). In `SyncingConnectingMithrilPrompt.tsx`, drop `styles.primaryButton`
  from both `classNames([...])` arrays (`:174-179` and `:206-211`) → `classNames(['primary',
  styles.actionButton])`. Rendered width is unchanged (`.actionButton` already sets `min-width:180px`).
  **Regen `SyncingConnectingMithrilPrompt.scss.d.ts`.** **WARNING:** an UNRELATED `.primaryButton` class
  exists in `NotificationActions.scss`/`.tsx` (a different component) — do **NOT** touch it. Only remove
  the `.primaryButton` in `SyncingConnectingMithrilPrompt.scss:66-68` (+ its comment `:62-65`) and the two
  `styles.primaryButton` refs in `SyncingConnectingMithrilPrompt.tsx:178`/`:210`. Grep confirmed no
  test/story references the SyncingConnecting one.

**New symbols:** local `CompletionBlock` component in `MithrilProgressView.tsx` (not exported).

**Acceptance:**
- The three completion blocks render identically to before (same DOM/ARIA/spinner position) via the
  shared helper; bootstrap's completed frame is unchanged.
- `.primaryButton` is gone from the scss + `.d.ts` + both `classNames` calls; the two buttons render at
  the same width; no test/story referenced `.primaryButton` (grep-verified).

**Tests:** existing `MithrilProgressView`/`MithrilStepIndicator` specs stay green; the prompt spec stays
green (button widths unchanged). `yarn compile` after the `.d.ts` regen.

---

## CAT-G — Live wipe-and-full-sync render + handler test (Finding #8)

**Closes:** #8 (the real-overlay wipe branch + `onWipeAndFullSync` wiring is uncovered).

**Exact files:**
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

**Edits checklist:**
- [ ] Add a spec that renders the **real** `MithrilPartialSyncOverlay` with a **post-cutover** status
  where wipe is legitimately offered (drive from `failed`, NOT `cancelled` — D-702a-2 removed wipe from
  the pre-cutover cancelled dialogue):
  `renderComponent({ status: 'failed', canRetry: false, canRestartNormally: false, canWipeAndFullSync:
  true, onWipeAndFullSync })` ⇒ the wipe action resolves to `variant: 'primary'`.
  - Assert the wipe button is present (query by its label, e.g. `/wipe chain data and do full mithril
    sync/i`).
  - `fireEvent.click` it and assert the injected `onWipeAndFullSync` fired **exactly once**.
- [ ] Keep the existing cancelled test (asserts wipe ABSENT with `canWipeAndFullSync:false`) green.

**Acceptance:** the live wipe render branch (`MithrilPartialSyncOverlay.tsx:124-137`) + its
`onWipeAndFullSync` wiring are covered by a passing test driven from `failed`.

**Tests:** the new case + the full existing overlay spec pass.

---

## Risks / verify-only

- **D-702b-1 fixed-banner visual caveat (verify-only).** The prompt is a `position:fixed; top:84px;
  z-index:10` banner (`SyncingConnectingMithrilPrompt.scss:1-16`) that now persists over
  Settings/Staking/Voting/Wallet sub-pages. **Operator must verify it does not obscure the top
  navigation or page content** on those routes; adjust positioning/offset **only if it collides** (no
  blind change). This is the one residual risk of the "persist everywhere" decision.
- **D-702b-5 initial-bootstrap-dialog realignment (verify-only).** Whether the *initial Mithril
  bootstrap process* dialogs should also adopt primary-on-right is a **separate, code-reviewed
  decision**. Making the error view order-agnostic already protects the bootstrap default
  (left-aligned, primary-first). Do NOT blind-align the bootstrap.
- **Gate-vs-display decoupling invariant (must not regress).** `isMithrilBehindnessKnown` /
  `NetworkStatusStore.isBehindnessKnown` stay clamp-free and unchanged; the `≤ 0 ⇒ undefined` logic is
  display-only (`computeBehindByEpochs`) and consumed in the container render, never in the gate
  computed. Backend `isSignificantlyBehind` remains the sole offer signal. **#16 (D-702b-10) extends but
  does not break this:** the named util stays byte-identical; the container's known-term becomes
  `(isBehindnessKnown || certifiedKnown)` and the figure helper gains a `certifiedEpoch?` fallback param —
  both composed only at the consumer.
- **#16 beacon-epoch field presence (verify-only, D-702b-10 §5).** We confirmed *current* Mithril's
  `CardanoDbBeacon` carries `{ epoch, immutable_file_number }`, but the repo is pinned to a fork branch
  (`flake.nix:23` → `mithril/sl/fix-mismatch-rust-versions`) and no in-repo fixture proves the key on this
  aggregator. **Operator runs `cardano-db snapshot show latest --json` (or the aggregator REST snapshot
  endpoint) against the pinned aggregator and confirms an `epoch` is present** in the beacon (and which
  key — `beacon.epoch` vs `beacon.epoch_number` vs `cardano_db_beacon.epoch`). The extractor is
  **multi-path + undefined-safe**, so if the field is absent the figure degrades to networkTip-only
  (= today, no regression) — but then the early-sync fix is **silently inert**, so this verify is what
  tells us the fix is actually live. If the key differs from the assumed paths, add it to
  `extractCertifiedEpoch`'s `explicitPaths`. **No blind change; verify then adjust the path list if
  needed.** **Sequencing (plan-review):** run this check **before/at the start of CAT-H** so the
  extractor's `explicitPaths` are written against the confirmed key — do not defer it to
  post-implementation, or the High-severity #16 fix can land silently inert with no failing test. CAT-H
  additionally adds a `mithrilSnapshotMetadata.spec.ts` fixture using the known-good production beacon
  shape (the same object the immutable extractor's `:28-31` fixture uses) with an `epoch` added, so the
  extractor is covered by a test reflecting the real shape.
- **CAT-A gate redundancy — RESOLVED (reviewer ruling F2): KEEP BOTH.** `behindByEpochs === undefined`
  (near-tip hide) logically subsumes `isBehindnessKnown` (both need finite tips), but the plan keeps both
  and does NOT collapse: collapsing breaks the existing anti-flash test
  (`MithrilProactivePromptContainer.spec.tsx:98-110`, `isBehindnessKnown:false` with FINITE tips 100/97),
  violates the D-702b-2 gate-vs-display decoupling invariant, and loses the #14 perf short-circuit (the
  cheap named boolean returns before `computeBehindByEpochs` is evaluated). Not an open question.
- **CAT-E back-off policy — RESOLVED (reviewer ruling F3): SLOW + re-arm, gated on a settled signal.**
  Once **settled**-stable (disabled, or enabled-and-not-behind for ≥2 consecutive reads) the poll
  **slows** (back-off to 5 min) and re-arms to 30s on the first unstable read; it **never stops** (a stop
  would strand the idle consumers). The back-off is gated behind ≥2 consecutive stable reads so it cannot
  engage on the premature first not-behind read and re-surface ISSUE-1 (see CAT-E). Not an open question.
- **Renderer verify-env caveat (every category).** Node v24 needs `typed-scss-modules` to regen the
  `.scss.d.ts` for `tsc`, plus a gitignored `identity-obj-proxy` jest sidecar. Apply **before** treating
  any `tsc`/`jest` failure as a regression — especially CAT-C (`MithrilErrorView.scss.d.ts` gains
  `actionsRightAligned`), CAT-F (`SyncingConnectingMithrilPrompt.scss.d.ts` loses `primaryButton`), and
  the new `mithrilBehindness` util import. See memory `mithril-ux-renderer-verify-env`.

## Verification plan (whole task)
Per-category commands (§ Sequencing), re-confirmed at the end: `yarn test:jest` (touched specs),
`yarn compile`, `yarn lint`, `yarn prettier:check`, `yarn stylelint`, and the renderer scss-`.d.ts`
regen + identity-obj-proxy sidecar before any tsc/jest run. Manual: operator verifies the two
verify-only items.

## Tasks-JSON entry (added)
Append `task-ux-702b` to phase-7's `tasks` array **between** `task-ux-702a` and `task-ux-702`
(`status: pending`, `interactionMode: interactive_validation`, `dependencies: ["task-ux-702a"]`,
`priority: high`, `estimatedHours: 10` — bumped 6 → 8 when CAT-H/#15 was added, then 8 → 10 for #16).
Update `task-ux-702.dependencies` to include `task-ux-702b`. Update metadata (`totalTasks 21 → 22`,
`version → 1.5.0`, `updated → 2026-06-30`) and the `summary` block (`totalTasks 21 → 22`,
`estimatedTotalHours 90 → 100` (+10 for 702b), insert `task-ux-702b` into `criticalPath` between
`task-ux-702a` and `task-ux-702`). **Applied 2026-06-30:** entry present; description/targetPaths/
implementationNotes/testCases/acceptance refreshed for CAT-H, `estimatedHours 6 → 8`,
`estimatedTotalHours 96 → 98`, backend `MithrilPartialSyncService.ts`/`.spec.ts` added to `targetPaths`.
**#16 re-apply APPLIED 2026-06-30 (plan-review consolidation):** bumped
`task-ux-702b.estimatedHours 8 → 10` (+2 for the beacon-epoch re-sourcing spanning CAT-B/CAT-A/CAT-H) and
`summary.estimatedTotalHours 98 → 100`; added `source/main/mithril/mithrilSnapshotMetadata.ts` (+`.spec.ts`),
`source/common/types/mithril-partial-sync.types.ts`,
`source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`,
`source/main/ipc/mithrilPartialSyncChannel.spec.ts`, and `source/main/mithril/MithrilController.ts`
(verify-only) to `targetPaths`; extended `description`/`implementationNotes`/`testCases`/`acceptance` for
#16 / D-702b-10 (hybrid beacon-epoch anchor, gate OR, multi-path extractor, operator beacon-epoch verify).
Dependencies unchanged.

## Review-log paths
- Planning: `task-ux-702b-plan-review.md`
- Implementation: `task-ux-702b-impl-review.md`

## Status
- Planning status: `completed`
- Build status: `pending`
- Status: `pending`
