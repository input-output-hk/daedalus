# task-ux-702b — Grill Decision Record (702a code-review cleanup round)

> Authoritative decision log produced by a `grill-with-docs` session over the
> **task-ux-702a code review** (xhigh workflow review of the uncommitted 702a working tree).
> Source findings: the verified code-review output (14 actionable findings + 1 refuted).
> Grounded by four read-only exploration subagents; all citations verified against live code.
> Predecessor: `task-ux-702a-decisions.md`. Process rules: `prompt-ux-refinement.md`.

## Scope decision (D-702b-0)

`task-ux-702b` is a **net-new phase-7 cleanup task** that remediates the **14 actionable findings
(#1–#14)** from the task-ux-702a code review. A separate, already-refuted finding — the
recommendation-tooltip-on-disabled-button — was **verified as a non-bug** (`buttonHintBlocked`
already covers the disabled state) and is **out of scope**, except an optional one-line
`<span>`-wrapper consistency polish.

**Decision (grill Q1): do ALL findings in one round** — 7 correctness/behavior (#1–#7) + 1 test gap
(#8) + 6 cleanups (#9–#14). Rationale: the cleanups are cheap and several couple tightly with the
correctness fixes (#7 lives *inside* the #11 util extraction; #10/#14 sit in the same container as
#3/#4), so splitting them across rounds would fracture shared files.

`task-ux-702b` gets its own four-file set (`task-ux-702b.md`, `-plan-review.md`, `-impl-review.md`,
`-research.md` as needed) and a tasks-JSON entry. **`task-ux-702` (deployment QA gate) stays gated
behind 702b** (depends-on chain: `…702a` → `…702b` → `…702`).

The 14 findings are grouped into **7 implementation categories (CAT-A…CAT-G)**, one code subagent
each, implemented sequentially on the shared working tree (collision-safe order in the category map).
**Update 2026-06-30:** a `grill-with-docs` CPU-cost re-validation added a **15th** finding (#15) and an
**8th** category (**CAT-H / D-702b-9**, backend-only). A **second** `grill-with-docs` session the same
day (the "epoch visibility" signalling defect) added a **16th** finding (**#16 / D-702b-10**) that
**folds into the existing CAT-B/CAT-A/CAT-H — no new category** — so 702b now closes **16 actionable
findings across CAT-A…CAT-H** (the original 14 + grill-added #15 + grill-added #16).

---

## Locked decisions from the grill

### D-702b-1 — Proactive prompt scope: KEEP cross-screen persistence; fix trigger + near-tip hide (Findings #3, #7-gate) — **REVERSES the review's route-scoping recommendation**
The code review flagged the app-level mount (`App.tsx:130`, `position:fixed` banner) as a bug to be
solved by **scoping the prompt to the Wallet Summary route**. **Grill decision (Q2) overrides this:**

- **Keep the app-level mount and FULL cross-screen persistence.** The prompt must **persist on all
  screens** until the user makes a choice this session — *not* be route-scoped. Do **not** move it
  back inside `SyncingConnecting` (that unmount was the original ISSUE-7 / D-702a-5 bug).
- **Trigger timing:** show the prompt only **once the Cardano node is loaded** — i.e. node
  connected and behind-ness **known** (network tip available) — **not during** the early
  connecting / "verifying blockchain" checks. (Pin the exact `NetworkStatusStore` flag during
  planning: `isConnected` + behind-ness-known, distinct from `isNodeInSync`.)
- **Near-tip suppression:** **re-confirm behind-ness before prompting; if the chain is near the tip
  (node effectively caught up), hide.** This makes behind-ness a real **gate**, not just a display
  string — it eliminates the misleading "about 1 epochs behind" (#7) at the source and dismisses the
  prompt automatically once the node catches up. Tie to the backend `isSignificantlyBehind` signal
  **and** the `computeBehindByEpochs` ≤ 0 case (D-702b-2): near-tip ⇒ no offer.
- **Visual caveat (verify, no blind change):** a `position:fixed; top:84px; z-index:10` banner that
  now persists over Settings/Staking/Voting/Wallet sub-pages must **not obscure top navigation or
  page content**. Verify placement across those routes; adjust positioning/offset only if it
  collides. (This is the one residual risk of the "persist everywhere" decision.)
- Category: **CAT-A**.

### D-702b-2 — Behind-ness epochs: extract shared util; treat ≤ 0 as unknown (Findings #11, #7) — couples D-702b-1
- **Extract `computeBehindByEpochs(localTip, networkTip)`** into `utils/mithrilBehindness.ts`
  (next to `isMithrilBehindnessKnown`, reusing that predicate's finiteness check). The epoch math
  is currently **byte-identical** in two places — `MithrilProactivePromptContainer.tsx:45-52` and
  `DaedalusDiagnostics.tsx:570-577` — both feeding the user-facing `{epochs}` figure; switch **both**
  call sites to the util and delete the `networkEpoch`/`localEpoch` intermediates.
- **Display semantics:** the helper returns **`undefined` when `networkEpoch - localEpoch ≤ 0`**
  (node level/ahead) instead of `Math.max(1, …)` clamping to 1. Every consumer already has an
  "unknown"/fallback branch (`SyncingConnectingMithrilPrompt.tsx:146-156`,
  `MithrilPartialSyncConfirmation.tsx:68`), so this is safe and fixes the "about 1 epochs behind"
  display and the "1 epochs" plural nit.
- **Decoupling invariant (do NOT regress):** the **gate** `isMithrilBehindnessKnown` (the anti-flash
  availability signal) stays **clamp-free and unchanged** — it returns `true` at equal epochs by
  design. The ≤ 0 ⇒ undefined logic lives **only** in the display-only `computeBehindByEpochs`. Gate
  vs. display are wired to different things; backend `isSignificantlyBehind` remains the sole offer
  signal. (The near-tip *hide* in D-702b-1 is enforced by the gate + backend signal, consistent with
  the display returning undefined.)
- Category: **CAT-B** (implement before/with CAT-A so the gate can consume it).

### D-702b-3 — Re-pop after a Mithril attempt: separate session guard, keep dismiss flag single-purpose (Finding #4) — grill Q3
`proactivePromptDismissedThisSession` is written in exactly one place (the prompt's own "Standard
Sync" button) and **no** recovery/terminal path sets it, so after the user starts Mithril and it
ends (completed / cancelled / failed / restart-normal recovery) status returns to `idle` and the
prompt **re-pops**, re-offering what the user already acted on.

- **Decision (Q3 — "fix re-pop differently"):** **do not overload the dismiss flag.** Keep
  `proactivePromptDismissedThisSession` meaning exactly *"user clicked Standard Sync on the prompt."*
- Add a **separate session-scoped guard** (e.g. `mithrilAttemptStartedThisSession`) set when a
  Mithril partial-sync attempt begins (`startPartialSync`) and **AND `!`it into the prompt gate**, so
  once an attempt has run this session the prompt does not re-offer regardless of the terminal
  outcome. This cleanly separates *"user dismissed"* from *"an attempt already happened."*
- Category: **CAT-A** (store flag + gate).

### D-702b-4 — Completion finalize: renderer-only robust fix (Finding #2) — grill Q4
The completed-overlay auto-dismiss (`MithrilPartialSyncOverlay.tsx:84-91`) fire-and-forgets the async
finalize (no `.catch`), and `dismissCompletedOverlay` flips `isCompletedOverlayDismissed = true`
(hides the overlay) **before** awaiting the finalize IPC, with no try/catch.

- **Grounding refinement:** the backend runs `_resetToIdleStatus()` *before* `fs.remove(stagingRoot)`
  and the marker read cannot reject, so on a finalize failure the backend ends at **`idle`** (not
  stuck `completed` as the review implied). The real damage is an **unhandled promise rejection** +
  the **staging dir and `.lock` marker leak** with no retry path.
- **Decision (Q4 — renderer-only, robust):** in `dismissCompletedOverlay`, **flip
  `isCompletedOverlayDismissed` only on finalize success**; wrap the finalize in `try { await finalize;
  flip } catch { logger.warn } finally { await this.syncStatus(); }`, reusing the store's existing resync
  pattern (as in `cancelPartialSync`/`startPartialSync`). The `setTimeout` callback must not be
  fire-and-forget — add a `.catch`. **No main-process change** (the backend cleanup-before-reset reorder
  is explicitly **out of scope** for 702b).
- **What this DOES and does NOT deliver (CORRECTED 2026-06-30 by plan review).** Because the backend
  already reset to `idle` *before* the failed `fs.remove`, the `finally { await this.syncStatus() }`
  pulls `idle`; `idle` is not an overlay status, so on failure **the overlay hides anyway**
  (status-driven). This change therefore does **NOT** keep the overlay up for a retry and does **NOT**
  remediate the staging-dir/`.lock` leak (that leak is a backend `fs.remove` failure the renderer cannot
  reach). Its two genuine deliverables are: (1) **no unhandled promise rejection** (awaited finalize + the
  timeout `.catch`); (2) **no premature *optimistic* hide on the success path** (the flag flips only after
  finalize resolves). The staging/marker-leak remediation is **deferred** to the out-of-scope backend
  cleanup-before-reset reorder. Future-proofing: if that backend reorder later leaves `status==='completed'`
  on failure, the no-blind-flip + resync would correctly keep the overlay up — but with today's backend it
  hides.
- Category: **CAT-D**.

### D-702b-5 — Dialog button ordering: make the shared error view order-agnostic; scope ordering to the partial-sync caller; fix the decision view (Findings #1, #5) — CORRECTS the 702a impl-review doc
`MithrilErrorView.tsx:111-116` added an `orderedActions` "primary-last" filter to the **shared**
default branch. `MithrilBootstrap.tsx:207-213` renders that view with **no `actions` prop**, so the
filter silently moved the **destructive "Wipe chain & retry" primary to the far right** of the
empty-chain bootstrap dialog. `MithrilDecisionView.scss:75` was flipped to `flex-end` but its `.tsx`
DOM order was not, leaving its primary on the **left** — the mirror image of the error view.

- **The 702a impl-review's "sanctioned visual-only — no logic/handler change" label
  (`task-ux-702a-impl-review.md:533`) is inaccurate** and contradicts the 702a decision record's own
  guard (`task-ux-702a-decisions.md:167-168`: realignment must be *"visual-only … do not regress the
  empty-chain bootstrap flow"*). The `orderedActions` change is a real **DOM child reorder** of a
  destructive action in the bootstrap default. **Correct that doc note** as part of this task.
- **Decision:**
  1. **Make `MithrilErrorView` render-order-agnostic** — remove `orderedActions`, render the
     caller-supplied `actions` order verbatim (like the shared `widgets/Dialog.tsx`, which owns the
     app convention: caller lists **primary last → right**, `Dialog.scss:79-94`).
  2. **Push ordering + opt-in right-alignment into the partial-sync overlay caller only**
     (`MithrilPartialSyncOverlay.tsx` builds `errorActions` already in display order — secondary
     first, primary last — and opts into `flex-end`, e.g. via an alignment prop/modifier). This
     keeps the cancelled/error overlay **primary-on-the-right** per manual-assessment D-702a-2.
  3. **Restore the bootstrap default untouched** — `MithrilBootstrap.tsx` keeps
     `[Wipe chain & retry (primary), Decline]`, left-aligned, primary-first, as before 702a. (The
     shared `.scss .actions` `flex-end` must therefore be **scoped/opted-in**, not global.)
  4. **Fix `MithrilDecisionView`** to agree: reorder its DOM so `secondaryAction` precedes
     `primaryAction` (keeping `flex-end` ⇒ primary on the right), matching the corrected error view.
- **Out of scope / verify-only:** whether the *initial Mithril bootstrap process* dialogs should also
  adopt primary-on-right (manual-assessment side-question) is a **verification item, not a blind
  change** — making the error view order-agnostic already protects the bootstrap default; any
  deliberate bootstrap realignment is a separate, code-reviewed decision.
- Category: **CAT-C**.

### D-702b-6 — Availability polling: keep the idle poll, fix the re-entrancy pin, add a known-stable back-off (Findings #9, #6) — REFRAMES the review's "restore the guard"
- **#9 grounding:** removing `if (this.isWorking)` from the 30s poll was **intentional and correct** —
  the proactive prompt (`status==='idle'` gate) and the Diagnostics dialog are **idle-time**
  consumers of `isSignificantlyBehind` that the old `isWorking`-only poll could never serve. **Do NOT
  restore the guard.** The only residual concern is lifetime: it keeps firing every 30s for the whole
  session even after the node is fully synced. **Decision: add a light back-off** — **slow** (not
  stop) the poll once availability is **known-stable** (enabled-and-not-behind, or disabled), then
  **re-arm to 30s on the first unstable read** so a node that later falls behind is re-detected. Stop
  is rejected: it would strand the idle consumers (the proactive prompt + Diagnostics never refresh
  `isSignificantlyBehind`). Low priority; must not break the idle consumers.
  - **ISSUE-1 guard (mandatory — from plan review):** the back-off must **not engage on the premature
    first not-behind read.** A probe that lands before the backend behind-ness settles reports
    `isSignificantlyBehind: false` while the node *is* behind; a naive `enabled && !behind ⇒ stable`
    predicate would slow the poll to the back-off interval **before** the probe settles, hiding the
    offer / showing "not behind" for minutes — the exact ISSUE-1 regression (see the verbatim ISSUE-1
    rationale at `MithrilPartialSyncStore.ts` setup, ~lines 83-91). Require a **settled signal before
    slowing**: e.g. **≥ 2 consecutive stable reads**, or back off immediately **only** on the
    `disabled` case and gate the *not-behind* back-off behind having observed at least one
    `isSignificantlyBehind === true` (or an N-tick grace). Always re-arm fast on the first unstable
    read. **Cover with a spec:** a premature not-behind read must NOT slow the poll; a later
    fall-behind must re-arm to 30s.
- **#6 (re-entrancy pin):** `_isRefreshingAvailability` clears only in `finally`, and
  `channel.request()` has **no timeout** (no in-repo precedent for bounding `request()`), so a wedged
  main process pins the guard `true` forever and kills all future refreshes. **Decision: bound the
  request** — wrap the availability `request()` in a timeout (e.g. `Promise.race` with a sane limit)
  or a generation-token reset so the guard cannot stick. This introduces a small new pattern; keep it
  local to `MithrilPartialSyncStore` and covered by a spec (inject a never-settling request).
- **Scope note (added 2026-06-30):** D-702b-6/CAT-E addresses poll **frequency** only and is
  **renderer-only**. The **per-probe backend cost** of #9 (each tick forking `checkDiskSpace` +
  re-reading `immutable/`) is a separate concern owned by **D-702b-9 / CAT-H / #15** — see below.
  CAT-E's back-off is gated **off while behind**, so the per-call cost matters most in the catch-up
  window and only CAT-H reduces it.
- Category: **CAT-E**.

### D-702b-7 — Simplification: shared CompletionBlock; remove dead CSS (Findings #12, #13)
- **#12:** extract a single **`<CompletionBlock title detail? spinnerPosition>`** helper for the three
  near-identical blocks in `MithrilProgressView.tsx` (stopping-node :174, starting-node :197, new
  completed-transition :220). They share the `div.completionBlock` + `role=status`/`aria-live=polite`/
  `aria-atomic=true` wrapper and the spinner; they differ only in **spinner position** (top for the
  completed block) and **presence of the detail `<p>`** (omitted in the completed block). Call sites
  keep their own `prop || intl.formatMessage(...)` title/detail resolution. **No i18n key changes.**
- **#13:** delete the dead `.primaryButton` rule (`SyncingConnectingMithrilPrompt.scss:62-68`, only
  sets `min-width:180px` that co-applied `.actionButton` already provides) and drop the two
  `styles.primaryButton` entries from the `classNames([...])` calls in
  `SyncingConnectingMithrilPrompt.tsx` (always co-applied with `.actionButton`; grep-verified no
  test/story refs). Regenerate the `.scss.d.ts`. Rendered width unchanged.
- Category: **CAT-F**.

### D-702b-8 — Test coverage: live wipe-and-full-sync branch (Finding #8)
The only positive `canWipeAndFullSync:true` test on the **real** `MithrilPartialSyncOverlay` was
rewritten to assert the button's **absence**; all remaining overlay tests pass `false`, and
`App.spec` exercises only a **mocked** overlay. The live wipe-button render branch
(`MithrilPartialSyncOverlay.tsx:124-137`) + its `onWipeAndFullSync` wiring is **uncovered**.

- **Decision:** add a spec that renders the real overlay with a **post-cutover** status where wipe is
  legitimately offered (e.g. `status:'failed', canRetry:false, canRestartNormally:false,
  canWipeAndFullSync:true` ⇒ variant resolves to `primary`), asserts the button is present, clicks it,
  and asserts `onWipeAndFullSync` fired once. (Drive from `failed`, **not** `cancelled` — D-702a-2
  removed wipe from the pre-cutover cancelled dialogue.)
- Category: **CAT-G**.

### D-702b-9 — Backend probe-cost cache: dedupe the per-probe `checkDiskSpace` fork + `immutable/` readdir (Finding #15, grill-added 2026-06-30)
**Added by a `grill-with-docs` CPU-cost re-validation of finding #9, NOT in the original 702a review.**
The review tagged #9 as "Cleanup / wasteful polling," and D-702b-6 (CAT-E) remediates it as a **poll
frequency** problem (cadence back-off). Re-tracing the cost shows the back-off does **not** touch the
actual CPU driver, which is the **per-probe cost** of the backend behind-ness probe:

- **Grounded cost (live 2026-06-30).** The 30s availability poll →
  `MithrilController.getPartialSyncAvailability` (`:167-174`; short-circuits when disabled `:169-171`)
  → `MithrilPartialSyncService.getPartialSyncBehindness` (`:669-698`). Per probe **when enabled**: the
  aggregator is **already** 5-min cached (`_getCachedLatestCertifiedImmutableNumber`, `:645-660`,
  `:111-114`; TTL `:72` whose comment literally says "**only the aggregator query is cached**"), but
  the two **local** inputs run every call: (1) `getManagedChainPath()` (`:675-676`) transitively
  **forks `checkDiskSpace`** (df/PowerShell) via `getConfig → getDefaultStorageConfig`
  (`chainStorageManagerConfig.ts:23`) even though it only needs `config.customPath` and discards the
  `free` figure; (2) `resolveLocalImmutableNumber()` (`:677-680`) does a full `fs.readdir(immutable/)`
  + ~5 `stat`/`access` (`mithrilPartialSyncPreflight.ts:63-141`). **`checkDiskSpace` reaches the probe
  via `getManagedChainPath → getConfig`, NOT the `start()` preflight `_assertSufficientDiskSpace`** —
  easy to miss by grepping call sites alone (this is what the 702a review missed).
- **Why CAT-E is insufficient.** CAT-E is renderer-only and reduces **frequency** only when not-behind;
  its back-off predicate (`!isPartialSyncEnabled || !isSignificantlyBehind`) is **gated off while the
  node is behind**, so during the catch-up window — exactly when the user watches it and feels the CPU
  — the poll stays at 30s and pays the full backend cost every tick. The cadence back-off cannot lower
  per-call cost in any state.
- **Decision (grill Q1–Q3, 2026-06-30):**
  1. **Add a backend per-probe cache** (Q1: yes).
  2. **New category CAT-H + this decision D-702b-9** (Q2) — keep CAT-E/D-702b-6's renderer-only lock
     intact; CAT-H is backend-only and collision-free with CAT-A…G; reframe #9's cost as **Perf/High**.
  3. **Cache the two expensive inputs behind one TTL** (Q3): add `_localImmutableCache` +
     `_getCachedLocalImmutableNumber()` mirroring the aggregator cache; on a hit, **skip both**
     `getManagedChainPath` (no fork) and `resolveLocalImmutableNumber` (no readdir). **Reuse the
     existing 5-min `PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS`.** The behind-ness **result** stays computed
     fresh each call (`cachedLatest − cachedLocal`). Add `_invalidateBehindnessCaches()` (nulls **both**
     caches) at `start()`/`cancel()` entry and inside `_resetToIdleStatus()` (covers
     restart-normal/wipe/finalize-wipe/finalize-completed) so post-cutover behind-ness is fresh.
- **No ISSUE-1 regression.** A stale-low cached local read makes `gap = latest − local` **larger** ⇒
  reads as **more** behind (conservative), never as a premature not-behind; CAT-E's
  ≥2-consecutive-stable-reads gate is unaffected (CAT-H caches inputs, not the stable/unstable verdict).
- **Freshness tradeoff during NORMAL sync (no lifecycle transition) — documented, accepted.** The
  invalidation list covers Mithril *lifecycle* transitions only. During ordinary genesis sync with no
  Mithril attempt, the node advances `immutable/` continuously with **no** lifecycle transition, so
  `_localImmutableCache` is refreshed only by the 5-min TTL. Consequence: `gap = latest − cachedLocal`
  reads **stale-high**, so `isSignificantlyBehind` can stay `true` for up to ~5 min after the node
  actually drops below threshold. This is **conservative** (over-offers; never a premature not-behind,
  so no ISSUE-1) but is a responsiveness regression from today's 30s freshness. The proactive **prompt**
  is additionally protected by the renderer near-tip hide (`computeBehindByEpochs` over live
  `localTip`/`networkTip` flips promptly), so it hides on time regardless; **Diagnostics** has no second
  gate and may show "Mithril available" for up to ~5 min after catch-up. Accepted as the cost of removing
  the per-tick fork; revisit only if the lingering badge is reported.
- **Rejected alternatives:** (a) **fold into CAT-E / reopen D-702b-6** — muddies the locked
  renderer-only decision; (b) **cache the whole behind-ness result** — a 5-min frozen result would
  serve a premature not-behind for 5 min, amplifying ISSUE-1 and fighting CAT-E's back-off gate; (c)
  **only cache the path (kill checkDiskSpace), leave readdir uncached** — leaves the Linux/Mac readdir
  cost every 30s.
- Category: **CAT-H** (backend-only; `MithrilPartialSyncService.ts` + `.spec.ts`).

### D-702b-10 — Behind-ness epoch SOURCE: re-anchor on the Mithril certified-beacon epoch (hybrid); gate + display (Finding #16, grill-added 2026-06-30)
**Added by a `grill-with-docs` session on the "epoch visibility" signalling defect, NOT in the original
702a review.** This is the data-source counterpart to D-702b-2 (which fixed the *clamp*, not the
*source*). The behind-ness figure is suppressed during early/mid sync even though that is exactly when
the user is most behind and Mithril would help most.

**The three "tips" (do not conflate — this is the canonical disambiguation):**

| Quantity | Source | Resolves |
|---|---|---|
| sync-% / node-tip *time* | cardano-wallet `sync_progress` (ratio `nodeTipTime / nowTime`) | from the start |
| network-tip **epoch** (`network_tip.epoch_number`) | cardano-wallet `/network/information` | **only near the end** |
| network frontier as **immutable-file number** + **epoch** | Mithril aggregator **beacon** | from the start |

- **Why `network_tip.epoch_number` resolves late (grounded, `types.ts:48-78`).** `node_tip.epoch_number`
  is a **required** `number` (local tip is inside the era-history horizon, always placeable); but
  `network_tip` is **optional** with `epoch_number: number | null | undefined`. cardano-wallet derives
  `network_tip` by projecting wall-clock "now" through the node's era history, which only resolves slots
  up to the safe-zone horizon just past the **local** tip; early in sync "now" is many hard-forks beyond
  that horizon → `network_tip: null` until the node advances into the current era. Sync-% still works the
  whole time because it is a ratio of *times*, not an epoch placement. **The current gate
  `isMithrilBehindnessKnown` (`mithrilBehindness.ts:18-22`) requires finite `networkTip.epoch`, so it is
  `false` for most of sync — that is the visibility gap.**
- **The backend probe already sidesteps the horizon** (`getPartialSyncBehindness:669-698`): it works in
  **immutable files** (`gap = latest − local`) using the aggregator beacon
  (`extractLatestCertifiedImmutableNumber`, `mithrilSnapshotMetadata.ts:66-88`) and the on-disk
  `immutable/` counter (`resolveLocalImmutableNumber`) — both horizon-free integers available from the
  first moment.

**Contradiction corrected (write-up vs. live code).** The originating write-up framed the fix as
"stop using the `// ≈ 20 files/epoch` heuristic." **There is no display ÷20 heuristic.** The `20` at
`MithrilPartialSyncService.ts:71` is the **offer threshold** for the `isSignificantlyBehind` boolean
(`gap >= threshold`), **never** a display divisor. The displayed "behind by N epochs" is computed in the
renderer as `networkTip.epoch − localTip.epoch` (the to-be-extracted `computeBehindByEpochs`, D-702b-2),
with **no** file→epoch division anywhere. So the true fix is not "drop ÷20" — it is **"swap the late
network-side anchor (`networkTip.epoch`) for the early one (the beacon's certified epoch)."** Same
destination; the recorded decision is framed to match the code.

**Decisions (grill Q1–Q5, 2026-06-30):**
1. **Re-source gate AND display (Q1).** Re-pointing only the display string would NOT un-suppress the
   prompt, because the **gate** (`isBehindnessKnown` = finite `networkTip.epoch`) is what hides it. The
   beacon-epoch anchor must reach the gate too.
2. **Hybrid semantic (Q2): prefer `networkTip.epoch` when finite, else `certifiedEpoch`.** The Mithril
   certified frontier *lags* the live tip (`certifiedEpoch ≈ current or previous epoch`), and
   `networkTip.epoch ≥ certifiedEpoch` always. So: near the tip, where the ~1-epoch certified lag is most
   visible, the resolved `networkTip.epoch` is used (accurate to the live tip, near-tip-hide unchanged);
   early in sync, where `networkTip` is null and the certified lag is negligible against the huge gap,
   `certifiedEpoch` is used (figure available from the start). **Accepted consequence:** when
   `networkTip` finally resolves mid-sync the figure can tick **up** by the certified-frontier lag —
   commonly ~1 epoch, occasionally more (never down — the live tip is ≥ the certified frontier); truthful
   and rare, so **not clamped** to monotonic.
3. **Plumbing (Q3): probe payload + combine in the container.** Extract `beacon.epoch` in
   `getPartialSyncBehindness` alongside the immutable number (**same beacon → consistent**, nearly free —
   the probe already resolves the snapshot), return `certifiedEpoch?` on the behind-ness result, surface
   it through `MithrilController.getPartialSyncAvailability` → `MithrilPartialSyncStore` (the existing 30s
   poll CAT-E/CAT-H already touch). The **container** (`MithrilProactivePromptContainer`, which injects
   BOTH stores) combines `networkStatus.{localTip, networkTip}` + `mithrilPartialSync.certifiedEpoch`.
   **`isMithrilBehindnessKnown(localTip, networkTip)` stays byte-identical** as the networkTip
   sub-signal; the certified OR/fallback happens at the consumer. *Rejected:* (a) compute the figure in
   the backend — the backend has `localImmutableNumber` but NOT `localTip.epoch`, and deriving a local
   epoch from a local immutable-file number needs the era schedule (the same PastHorizon dependency this
   fix avoids), so the figure MUST be renderer-computed and only `certifiedEpoch` can come from main;
   (b) push `certifiedEpoch` into `NetworkStatusStore` — couples the generic network store to a
   Mithril-specific quantity with no natural source there.
4. **Fold into the unbuilt CAT-B / CAT-A / CAT-H (Q4)** as Finding #16 + this decision — **no new
   category, no build-then-rewrite.** CAT-H also extracts+returns `beacon.epoch`; CAT-B's
   `computeBehindByEpochs` gains the `certifiedEpoch?` fallback param; CAT-A's container gate ORs in
   `certifiedKnown` and reads `mithrilPartialSync.certifiedEpoch` (and owns the store observable +
   `_applyAvailability` epoch read). **D-702b-2 is PRESERVED and extended, not broken:** the named util
   stays clamp-free/unchanged; the ≤0⇒undefined logic stays display-only; only the container's
   composition gains an OR'd certified-known term and the figure helper gains a fallback param.
5. **Defensive-degrade + operator verify (Q5).** We confirmed *current* Mithril's `CardanoDbBeacon`
   carries `{ epoch, immutable_file_number }` (the deprecated `network` field was recently removed), but
   the repo is pinned to a **fork branch** (`flake.nix:23` → `mithril/sl/fix-mismatch-rust-versions`) and
   no in-repo fixture proves the key on this aggregator. So extract `beacon.epoch` via **multi-path**
   (mirror `extractLatestCertifiedImmutableNumber`'s `explicitPaths`, reuse `getNestedValue` +
   `toPositiveInteger`): `['beacon','epoch']`, `['cardano_db_beacon','epoch']`, `['beacon','epoch_number']`,
   `['epoch']`, … → `undefined` if absent. **When `certifiedEpoch` is undefined the hybrid falls back to
   networkTip-only = EXACTLY today's behavior (no regression); the early-sync fix simply stays inert until
   the field is present.** Add a **verify-only** item: operator runs `cardano-db snapshot show latest
   --json` against the pinned aggregator to confirm the field IS present (so the fix is live, not silently
   degraded).

**Sequencing note (safe-degradation makes #16 order-independent).** CAT-H *produces* `certifiedEpoch`;
CAT-B/CAT-A *consume* it. Because absent `certifiedEpoch` degrades to today's behavior, the producer and
consumers can land in **any** order without regression — the existing collision-safe order
(`CAT-B → CAT-A → … → CAT-H`) is unchanged. Each category remains independently testable (the util test
passes `certifiedEpoch` directly; the container test mocks `mithrilPartialSync.certifiedEpoch`; the
service test asserts the probe returns it). The fix is end-to-end live only once all three land.

**No ISSUE-1 / D13 regression.** Behind-ness stays **epochs-only** (no %/immutable surfaced to the user —
`[[ux-copy-cardano-vocabulary]]`). A stale-low cached `certifiedEpoch` (CAT-H 5-min TTL) makes the
hybrid figure *smaller* near the tip → at worst an earlier near-tip hide once the local tip has actually
reached the certified frontier (nothing left for Mithril to restore — correct), never a premature
"not behind" while genuinely behind. The offer signal (`isSignificantlyBehind`, immutable gap) is
untouched.

- Category: folds into **CAT-B** (util) + **CAT-A** (store field + container gate/figure) + **CAT-H**
  (backend beacon-epoch extraction + IPC payload).

---

## Category map (one code subagent each)

| Cat | Findings | Scope | Primary files |
|---|---|---|---|
| **CAT-A** | #3, #4, #10, #14, **#16** | Proactive-prompt lifecycle: node-loaded trigger, cross-screen persistence, near-tip hide, separate re-pop guard, observer-perf reorder, drop dead `actions` inject; **#16: add `certifiedEpoch` store observable + `_applyAvailability` read; container gate ORs in `certifiedKnown` and the figure prefers `networkTip.epoch` else `certifiedEpoch`** | `MithrilProactivePromptContainer.tsx`, `App.tsx`, `MithrilPartialSyncStore.ts`, `NetworkStatusStore.ts`, `MithrilProactivePromptContainer.spec.tsx` |
| **CAT-B** | #11, #7, **#16** | Extract `computeBehindByEpochs` (≤0 ⇒ undefined, plural fix); switch both call sites; gate decoupling invariant; **#16: add the `certifiedEpoch?` fallback param (prefer `networkTip.epoch`, else `certifiedEpoch`)** | `utils/mithrilBehindness.ts`/`.spec.ts`, `MithrilProactivePromptContainer.tsx`, `DaedalusDiagnostics.tsx` |
| **CAT-C** | #1, #5 | Make `MithrilErrorView` order-agnostic; overlay caller owns ordering + opt-in right-align; restore bootstrap default; fix decision-view DOM order; correct impl-review doc | `MithrilErrorView.tsx`/`.scss`, `MithrilDecisionView.tsx`/`.scss`, `MithrilPartialSyncOverlay.tsx`, specs, `task-ux-702a-impl-review.md` |
| **CAT-D** | #2 | Completion finalize renderer-only robustness: await-before-hide, catch, resync on failure | `MithrilPartialSyncStore.ts`, `MithrilPartialSyncOverlay.tsx`, specs |
| **CAT-E** | #6, #9 | Availability poll: bound the request (anti-pin), known-stable back-off; keep idle poll | `MithrilPartialSyncStore.ts`, `MithrilPartialSyncStore.spec.ts` |
| **CAT-F** | #12, #13 | Shared `<CompletionBlock>` helper; remove dead `.primaryButton` CSS + refs | `MithrilProgressView.tsx`, `SyncingConnectingMithrilPrompt.scss`/`.tsx` |
| **CAT-G** | #8 | Add live wipe-and-full-sync render+handler test (drive from `failed`) | `MithrilPartialSyncOverlay.spec.tsx` |
| **CAT-H** | #15, **#16** | **(grill-added)** Backend probe-cost cache: `_getCachedLocalImmutableNumber` (5-min TTL) dedupes the per-probe `checkDiskSpace` fork + `immutable/` readdir; invalidate both behind-ness caches on lifecycle transitions. Complements CAT-E (frequency vs per-call cost). **#16: extract `beacon.epoch` (multi-path, undefined-safe) alongside the immutable number, carry it on the aggregator cache + `ResolvedLatestSnapshot`, return `certifiedEpoch?` on the behind-ness result, and add it to the `getPartialSyncAvailability` IPC payload.** Backend-only ⇒ collision-free | `MithrilPartialSyncService.ts`, `mithrilSnapshotMetadata.ts`, `MithrilController.ts`, IPC/common types, `*.spec.ts` |

**Known couplings / sequencing (collision-safe order):**
- **CAT-B before CAT-A** — the near-tip gate (D-702b-1) consumes `computeBehindByEpochs` (D-702b-2).
- **CAT-A, CAT-D, CAT-E all touch `MithrilPartialSyncStore.ts`** → serialize them (A → D → E), each
  followed by a code-review/compile pass, to avoid edit collisions.
- **CAT-C and CAT-D both touch `MithrilPartialSyncOverlay.tsx`** → serialize (C before D, or coordinate
  the `errorActions` vs `dismissCompletedOverlay` regions).
- **CAT-H is backend-only** (`MithrilPartialSyncService.ts` + `.spec.ts`) — **no overlap** with the
  renderer files in CAT-A…G, so no collision; placed after CAT-E to keep the two poll-cost fixes
  (frequency + per-call cost) adjacent.
- **#16 producer/consumer is order-independent (D-702b-10).** CAT-H *produces* `certifiedEpoch` (backend
  beacon extraction + IPC payload); CAT-B/CAT-A *consume* it (util fallback param + container gate/figure).
  Because an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no
  regression), the producer and consumers may land in any order — the collision-safe order below is
  unchanged. End-to-end behavior is live only once all three land.
- **Suggested order:** CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G, each followed by
  `yarn compile` + the touched specs.

---

## Environment caveats (brief implementers)
- **Renderer + scss verify env:** Node v24 needs `typed-scss-modules` to regen the `.scss.d.ts` for
  `tsc`, plus a gitignored `identity-obj-proxy` jest sidecar. Apply before treating tsc/jest fails as
  regressions (esp. CAT-F scss work and the `mithrilBehindness` util import). See memory
  `mithril-ux-renderer-verify-env`.

## Out of scope / non-bugs (no silent caps)
- **Recommendation tooltip on a disabled button** (review's refuted finding): **verified non-bug** —
  `buttonHintBlocked` ("Unavailable while Mithril work is already active.") is an always-visible hint
  that covers the `isActionBlocked` state; the hover pitch is only meaningful when the button is
  enabled (and hover works). Optional one-line `<span>`-wrapper polish for consistency with
  `MithrilProgressView.tsx:248`, not required.
- **Backend cleanup-before-reset reorder** for the finalize path (D-702b-4): deferred; renderer-only
  fix chosen.
- **Initial Mithril bootstrap dialog realignment** (D-702b-5): verify-only, separate decision.

## Severity (orchestrator-assigned; review marked none)
Highest: **#3/#4** (prompt shows wrong / re-offers — user-facing behavior), **#16** (behind-ness figure +
prompt suppressed during early/mid sync — exactly when the user is most behind and Mithril helps most —
because the gate waits on cardano-wallet's late `networkTip.epoch`; **High/user-facing**, the root-cause
companion to #7's clamp fix) and **#1** (destructive bootstrap button silently reordered — regresses a
flow the 702a doc said not to touch). Then **#2**
(staging-dir leak + unhandled rejection), **#6** (poll can pin and stop self-correcting), **#15**
(per-probe `checkDiskSpace` fork + `immutable/` readdir every 30s — the actual CPU driver behind the
poll; **Perf/High**), **#7** (misleading epochs figure). Then cleanups **#10/#11/#12/#13/#14** and the
**#8** test gap. **#9 reclassified (2026-06-30):** the review's "Cleanup" tag undersized it — the
*frequency* half is the CAT-E cleanup, but the *per-probe cost* half is the **Perf/High #15 / CAT-H**.
