# Mithril Partial Sync — UX Refinement PRD (Phase 2)

> **Status:** Draft for review — grilling round 5 complete (behind-ness *figure source* changed to the
> exact **node-tip epoch difference** computed in the renderer — D12 supersedes D11's backend
> immutable→epochs derivation; the gate and the epochs+sync-% framing are unchanged), 2026-06-25.
> Round 4: behind-ness display refined to Cardano-native **epochs + sync-%** via D11, 2026-06-24. Round 3: success-finalization seam,
> proactive-prompt deep-link handoff, and `onReceive` teardown reconciled; issue #10 comment 4623095092
> audited & integrated.
> **Supersedes for UX:** the UX sections of `mithril-partial-sync-prd.md` where this doc decides
> a question differently. The original PRD remains the source of truth for backend orchestration,
> restore-safety posture, and the boundary recovery model.
> **Tracks:** GitHub issue #10 "Mithril Snapshot UX Design — Phase 2".
> **Research base:** `.agent/plans/mithril-partial-sync/research/19-ux-refinement-state-and-gaps.md`.

## Purpose

The Mithril partial sync feature is already implemented end-to-end on
`feat/mithril-partial-sync-ux-refinement` (backend → IPC → store → overlay). This PRD does **not**
re-implement it. It **refines the UX** to (a) satisfy issue #10's actual intent — prompt the user
when they are significantly behind the tip — and (b) close the remaining issues enumerated in the
research dossier (35 gaps, severity blocker → polish) **and the 13 follow-up items raised in issue #10
comment 4623095092** (see the *Issue #10 Comment Coverage Map* below).

This is a living document. Decisions are recorded in the **Decision Log** as they crystallize
during grilling; the full PRD body (requirements, technical design, testing, rollout) is assembled
once the load-bearing decisions are locked.

---

## Decision Log

Each entry: the decision, the options considered, the rationale, and the locks/constraints it
touches. `D#` ids are referenced from the requirements once written.

### D1 — Issue #10 trigger model: backend-gated manual trigger **+** proactive session-dismissible prompt

**Decision.** Implement a hybrid trigger:

1. **Backend-owned "significantly behind" signal.** The main process computes whether the user is
   significantly behind and emits it to the renderer (renderer never computes the threshold —
   honors the locked "no renderer threshold" decision). Metric/threshold defined in **D2**.
2. **Diagnostics manual trigger, gated on the signal.** The existing DaedalusDiagnostics
   recommendation + Start CTA is shown/enabled only when the backend signal says the user is
   significantly behind (and the feature is enabled — see D3). Still 100% manual; no auto-start
   (honors the locked "no auto-trigger" decision).
3. **Proactive loading-screen prompt (NEW surface).** When the user is significantly behind during
   sync, a dismissible prompt appears on the loading/sync screen, guiding them to Mithril partial
   sync as a faster catch-up path. It routes the user into the **same** Diagnostics-owned
   confirmation flow (no separate start path).
   - **Session-scoped dismissal:** if the user dismisses the prompt, it must **not** appear again
     for the remainder of the app session (in-memory, not persisted across restarts).
   - **Dismissal/handoff copy:** the prompt tells the user they can still trigger partial sync
     manually later from the Diagnostics page, which will guide them through catching up.
   - **Deep-link handoff (mechanism, resolved 2026-06-19).** "Review" must open Diagnostics **already in
     the partial-sync confirmation modal**. Today `openDaedalusDiagnosticsDialog` takes no argument and
     the confirmation is **local component state** in `MithrilPartialSyncSection`, so the deep-link has
     no seam. Resolution: give `openDaedalusDiagnosticsDialog` an **optional payload**
     (`{ showMithrilConfirmation: true }`); `DaedalusDiagnostics` threads a `showConfirmationOnOpen` prop
     to `MithrilPartialSyncSection`, which opens the confirmation modal on mount (respecting
     `isActionBlocked`). Minimal and localized; still **no separate start path** — confirmation precedes
     start (lock #3).

**Options considered.**
- *(chosen)* Backend-gated diagnostics trigger **plus** proactive prompt.
- Backend-gated diagnostics trigger only (no proactive surface) — under-delivers on "prompt".
- Accept the current always-on passive recommendation as closure — rejected; ignores the
  "significantly behind" condition entirely.

**Locks touched / exception granted.**
- ✅ Honors **"no auto-trigger"** — the prompt never starts partial sync; it only surfaces the
  option and routes to confirmation.
- ✅ Honors **"no renderer threshold/heuristic"** — the threshold is backend-owned (D2).
- ⚠️ **Documented exception to "entry point = DaedalusDiagnostics only"** (original PRD locked
  decision; `mithril-partial-sync-prd.md:60`): the proactive prompt is a *second discovery
  surface*, but it is **not** a second *start* path — it deep-links into the single
  Diagnostics-owned confirmation → start flow. The locked invariant that **confirmation precedes
  start** (task-302) is preserved. This exception is approved by explicit user decision on
  2026-06-19.

**Rationale.** Issue #10 asks to *prompt the user when significantly behind*. A passive,
always-on recommendation buried in Diagnostics satisfies neither the "prompt" nor the
"significantly behind" half. Gating on a backend signal makes the recommendation honest and
keeps the renderer dumb; the proactive prompt delivers the actual "prompt" while still funneling
through the one safe, confirmed start path. Session-scoped dismissal respects users who decline
without nagging them, while leaving the manual Diagnostics path always available.

**Issue #10 comment sign-off (UX4, 2026-06-19).** Comment 4623095092 asked to *"simply [disable] the
partial sync option if sync status is closer than a certain percentage (< 5%)."* That literal ask is
**superseded by explicit user decision** on two axes: (1) **disable → hide** — when the user is *not*
significantly behind, the option is *hidden*, not shown greyed-out (D3), so there is never a dead
control near the tip; (2) **sync-% → certified-immutable gap** — the gate is the backend D2 gap, not a
renderer sync-percentage threshold, because a raw sync-% can fire even when Mithril has *no* certified
range to help (truthfulness rationale, D2; honors the locked "no renderer threshold"). The polarity is
inverted (surface-when-far-behind rather than suppress-when-near-tip) but the near-tip effect is
identical — no offer — while being driven by a truthful backend signal.

**Display refinement (D11, 2026-06-24).** The *gate* stays the certified-immutable gap (above); the
*user-facing figure* is now **epochs** (derived from that same gap) followed by a **sync-%** sentence —
immutable files are never shown to the user. See **D11**.

**Open dependents:** D2 (what "significantly behind" means + threshold), D3 (kill-switch gating &
default), and the proactive prompt's exact surface/visual treatment.

### D2 — "Significantly behind" = backend-computed **certified-immutable gap**

**Decision.** The backend computes the behind-ness signal as the **certified-immutable gap**:

```
localImm  = highest parseable immutable file number on disk      (cheap local FS read)
latestImm = latest certified Mithril snapshot immutable number   (Mithril aggregator, cached)
gap       = latestImm - localImm
isSignificantlyBehind = gap >= THRESHOLD_IMMUTABLES
```

- The signal fires **only when a real certified range exists to restore**, so the user is never
  nudged toward a partial sync that would dead-end in `PARTIAL_SYNC_NO_CERTIFIED_RANGE`. This is
  strictly more truthful than node-tip distance (which can fire when Mithril cannot help).
- Reuses the backend's **existing** latest-snapshot resolution (the same resolver used at start),
  so no new restore machinery — one Mithril aggregator query, **cached** and refreshed periodically
  while the user is syncing (not a tight poll).
- `THRESHOLD_IMMUTABLES` is **backend-owned and tunable via launcher config**. Default: behind by
  **≈ ≥ 1 epoch-equivalent of immutable files** (~5 days on mainnet) — conservative to avoid
  over-prompting; calibrate down during QA.
- The renderer never computes or sees the threshold; it receives only the boolean
  `isSignificantlyBehind` (and optionally the raw `gap` for copy — see confirmation richness, TBD).

**Options considered.** Certified-immutable gap *(chosen)*; node-tip distance (cheap but can
dead-end); both combined (most precise, most plumbing).

**Locks touched.** ✅ Honors "no renderer threshold" (backend owns it). Reuses the certified-range
resolver already governed by the staged-restore safety posture.

**Display note (D11, 2026-06-24).** The gate stays immutable-based and backend-owned; only the
*user-facing figure* changes. The displayed figure is **epochs behind** (derived from this same
certified-immutable gap) plus a **sync-%** context sentence — immutable files are an internal unit and
are never shown to the user. Deriving the displayed figure from the gate's own quantity keeps it
consistent with the offer. See **D11**.

**Open dependents:** delivery mechanism for the signal (status snapshot vs a dedicated
behind-ness/availability query — see D3/technical design); the user-facing-figure question is
**resolved by D11** (epochs + sync-%).

### D3 — Expose the kill switch to the renderer via an availability read model; branch enables it for QA, production default deferred

**Decision.**
- **Expose `mithrilPartialSyncEnabled` to the renderer** (closes blocker #1). Introduce a single
  backend-owned **availability read model** that carries both the kill-switch state and the D2
  behind-ness signal:
  ```ts
  type MithrilPartialSyncAvailability = {
    isEnabled: boolean;              // from launcherConfig.mithrilPartialSyncEnabled
    isSignificantlyBehind: boolean;  // D2 certified-immutable-gap signal
    behindByImmutables?: number;     // raw gap — internal GATE BASIS only (never shown to users; renderer never thresholds it)
    // D12: NO behindByEpochs field — the user-facing epochs figure is the renderer node-tip difference (max(1, networkTip.epoch − localTip.epoch)), not a backend field.
  };
  ```
  Delivered on a **dedicated channel** (one-shot query at store setup + periodic refresh while
  syncing), kept **separate** from the in-flight `MithrilPartialSyncStatusSnapshot` (which is about
  an *operation*, not static availability). The behind-ness check is the natural home for the D2
  cached Mithril query.
- **Gate ALL partial-sync UI** on `isEnabled`: when the kill switch is off, the diagnostics
  recommendation/CTA **and** the proactive prompt are fully hidden — no enabled-looking button that
  throws a raw rejection string (the blocker-#1 failure mode). The manual trigger and the proactive
  prompt are additionally gated on `isSignificantlyBehind` (per D1/D2).
- **Rollout context (from user, 2026-06-19):** the nix default was set OFF only so the initial
  backend+UI could merge to master safely. **This branch (`feat/mithril-partial-sync-ux-refinement`)
  sets `mithrilPartialSyncEnabled = true` for full manual testing.** The branch will not merge to
  master / go live until all refinement features + fixes land and partial sync is
  deployment-ready. The **production release default (and any network-scoping) is deferred** to that
  readiness gate; the kill switch remains the rollout lever, flippable via launcher config without
  an app release.

**Locks touched.** Honors "kill switch is the rollout lever" (PRD:340); adds the missing renderer
visibility the original PRD's blocker note implied was required.

**Open dependents:** none blocking; exact channel name + refresh cadence are technical-design
details.

### D4 — Honest, **consistent** progress for BOTH partial sync and bootstrap (file-count based)

**Decision.** Adopt the **honest hybrid** progress presentation and apply it to **both** the partial
sync overlay **and** the Mithril full-snapshot bootstrap overlay so they are consistent (explicit
user directive, 2026-06-19 — scope expansion beyond partial sync alone).

- **Download phase = file-count progress, truthfully.** Drive the download bar from
  `filesDownloaded / filesTotal` and label it as files (e.g., "Restoring verified data — 142 / 980
  files"). Real total **size** from snapshot metadata (`snapshot.size`) may be shown as **static
  context** ("≈ 9.7 GB total") where available, but the **bar fill and the running readout are
  file-based**, not synthetic bytes.
- **Stop both current misbehaviors:**
  - Bootstrap today shows a *synthetic* byte readout: `bytesDownloaded =
    (filesDownloaded / filesTotal) * snapshot.size` (`MithrilBootstrapStore.ts:79-97`). **Factual
    correction (2026-06-19 audit):** this synthetic value and the real `snapshotSize` both render via
    `formattedNumber` as **plain numbers** on the combined download line
    (`MithrilStepIndicator.tsx:196-198`) — they are *not* "formatted as GB"; only the *ancillary* bytes
    use the GB formatter. It is still the "converted file→byte format" the user flagged: an
    interpolation assuming uniform file sizes. Replace with the file-count presentation.
  - Partial sync today passes **raw file counts into the byte-named props**
    (`MithrilPartialSyncOverlay.tsx:87-88` → `bytesDownloaded={filesDownloaded}`,
    `snapshotSize={filesTotal}`), so `formatSnapshotCount` renders counts through a **byte
    formatter** — actually broken. Fix to true file-count display.
  - Resolves the research **note-15 contradiction** (it said "keep the byte footer disabled until
    truthful telemetry") by making the readout **truthful (files)** rather than deleting it.
- **Long non-download phases get active reassurance.** For `verifying`/`converting`/`installing`/
  `finalizing` (and bootstrap's equivalents), show an **active/indeterminate indicator + elapsed
  time + clear phase label + "this can take several minutes"** so no phase ever looks frozen
  (closes the task-401 "looks stalled" residual, gap #5). The `MithrilStepIndicator` waterfall
  already names these phases; this adds activity affordance, not new IPC throughput math.
- **Shared component, single semantic.** `MithrilProgressView` / `MithrilStepIndicator` are shared
  by both flows; unifying them is the natural, low-surface way to deliver consistency. The
  unification must **not** introduce synthetic throughput / remaining-time / overall-% over IPC
  (locked invariant #18).
- **Active step circle must animate (UX7 — issue #10 comment).** The top-level active step renders a
  *static* dot today (`MithrilStepIndicator.tsx:386-388` `.activeCircle`; no animation in `.scss:79-84`,
  and a unit test even locks it — `MithrilStepIndicator.spec.tsx:59-60` asserts `.iconSpinner` is null).
  Render the **existing** `.iconSpinner` / `loading-spin` for the active top-level step so the circle
  visibly rotates, in **both** flows via the shared component (reduced-motion already handled,
  `.scss:299-311`). **Update `MithrilStepIndicator.spec.tsx:59-60`** to expect the spinner. This is the
  "progress circles for each step are not rotating" complaint, which the reporter also saw on the
  initial Mithril sync — fixing it in the shared component covers both.
- **Elapsed must tick continuously (UX6 / UX5a — issue #10 comment).** Partial-sync elapsed is
  backend-snapshot-driven only (stamped inside `_updateStatus`), so it **freezes between status events**
  (the reporter saw it stick at 6:46, and read 0:00 during node-stop). Drive the elapsed readout from a
  **renderer-side 1 s timer anchored to a backend start timestamp**, reusing the bootstrap-proven path:
  add `startedAt` to `MithrilPartialSyncStore`, pass it as `bootstrapStartedAt`
  (`MithrilProgressView.tsx:97-109`), and **stop passing the frozen `elapsedSeconds` prop**
  (`MithrilPartialSyncOverlay.tsx:93`). No new IPC; honors invariant #18 (no synthetic throughput).
  Without this, D4's own reassurance (elapsed + active indicator) would itself look frozen.

**Options considered.** Honest hybrid *(chosen)*; byte-accurate primary bar (rejected — the primary
cardano-db download reports file counts, not bytes, so a "real byte" bar would itself be
synthetic); relabel-only (rejected — leaves the looks-stalled phase issue open).

**Cross-feature impact.** Touches the **bootstrap** flow's progress display and its tests/stories.
Must verify no regression to bootstrap's success path. Bootstrap keeps its real `snapshot.size`
metadata for the static "total size" context; only the *progress bar semantic* changes from
synthetic-bytes to file-counts.

**Issue #10 comment sign-off (UX8, 2026-06-19).** Comment 4623095092 asked partial sync to show an
*estimated snapshot size* like the initial Mithril sync. D4 **supersedes that with explicit user
sign-off**: both flows show **honest file counts** as the primary bar/readout, and real `snapshot.size`
appears **only as static "≈ N GB total" context** where available. A *moving size bar* for partial sync
would require synthesizing bytes from file ratios (the partial-sync transfer type carries no real byte
total over IPC) — precisely the synthetic pattern banned by invariant #18 and the one D4 removes from
bootstrap. "Size" therefore means static context, never a moving bar.

**Open dependents:** whether partial sync's snapshot metadata carries a real total size for the
"≈ N GB total" context line (if not cheaply available, omit it and show file counts only).

### D6 — Confirmation is a proper decision-style **modal** with behind-ness context

**Decision.** Replace the in-place prose row-swap with a proper **modal** confirmation:
- **Behind-ness context line (epochs + sync-%, D11/D12).** A two-sentence, Cardano-native line: the primary
  sentence is the renderer-computed node-tip epoch difference (`max(1, networkTip.epoch − localTip.epoch)`, D12) (e.g. "Your node is about N epochs
  behind the blockchain tip."), followed by a **separate** sentence with the current sync percentage
  (e.g. "Cardano node is currently X% synced.") — never interpolated mid-sentence. Immutable files are an
  internal unit and are **not** shown to the user; node-tip distance ≥ the certified gap, so the figure can
  never show fewer epochs than the offer implies (D12), and the copy conveys the benefit of partial
  sync versus waiting. Renderer still does no thresholding. See **D11**.
- **What-happens steps:** (1) stop Cardano node, (2) download & verify Mithril data, (3) restart
  node automatically — and the recovery note (retry / restart normally / wipe & full sync).
  Confirmation copy must keep the locked wording that the data is **verified** (lock #4).
- **Modal treatment:** scrim + focus trap; **ESC / scrim-click = "Back to diagnostics"** (not close
  the entire diagnostics dialog — fixes gap #23). Closes gaps #22 (in-place swap) and #23.
- **Button hierarchy:** primary **"Start Mithril partial sync"** vs secondary **"Back to
  diagnostics"** — distinct styles (fixes gap #14, the shared-cancel-style bug).
- **Stays lighter than bootstrap:** **no** snapshot-selection and **no** storage-location picker
  (locked non-goals). Reuse bootstrap decision-view **styling** for visual consistency, not its
  snapshot-metadata grid.

**Options considered.** Decision-style modal with behind-ness *(chosen)*; minimal inline fix
(rejected — stays visually divergent); full bootstrap-style metadata grid (rejected — needs
digest/created/version plumbing that adds little for a recovery action).

**Locks touched.** Preserves "confirmation precedes start" (#3) and "verified data" wording (#4);
respects no-snapshot-selection / no-storage-picker non-goals.

**Open dependents:** none blocking.

### D5 — Errors & recovery: native dialog owns startup-interrupted recovery; in-session overlay gets stage-aware copy, a safe close-fallback, and no false Cancel

**Decision.** Resolve the error/recovery cluster as follows:

**(a) Startup-interrupted recovery → single native dialog (gap #7).** When startup detects an
unsafe interrupted cutover (`mithrilPartialSyncNodeStartup.handleInterruptedRecovery`, marker not
`installed-awaiting-node-start` / not `node-start-verified`), the **native Electron dialog is the
single authoritative recovery surface**; **remove the redundant React `failed` emission** on that
startup path (`mithrilPartialSyncNodeStartup.ts:69-90`). Honors the lock that startup-owned recovery
"must not depend on diagnostics UI or a running node," works before the renderer is ready, is
correctly blocking, and includes Quit. **In-session failures are unaffected** — they keep using the
React overlay.

**(b) Stage/code-specific failure copy (gap #6).** Add a **partial-sync** error-copy map keyed by
`error.code` / `MithrilPartialSyncErrorStage` and feed its title/hint into the overlay (the overlay
already always passes explicit title+hint, which currently short-circuits the bootstrap-keyed
`ERROR_COPY_BY_STAGE`). Bespoke, actionable copy for at least:
`PARTIAL_SYNC_NO_CERTIFIED_RANGE`, `PARTIAL_SYNC_LATEST_DRIFT`, `PARTIAL_SYNC_STAGED_DB_INVALID`,
`DOWNLOAD_COMMAND_FAILED`, `CONVERSION_FAILED`; everything else falls back to the generic
failed/cancelled copy. The backend already emits these codes + a correctly-typed stage.

**(c) No Cancel after cutover (gap #9, lock #6).** Extend the overlay's `hideAction` so Cancel is
hidden for **all** post-cutover phases (`installing`, `finalizing`, `starting-node`), not just
`starting-node` — the backend rejects cancel there, so the button must not appear. Pre-cutover
phases keep Cancel.

**(d) Safe close-fallback / no dead-end (gaps #8, #31).** The recovery actions ARE the exits:
pre-cutover, `restart-normal` is the "continue on the existing DB" escape; post-cutover, only
`wipe-and-full-sync` (no dismiss — unsafe to ignore). Add a **defensive Quit fallback** rendered
**only if** `allowedRecoveryActions` is ever empty, so a failure can never become an unclosable
dead-end. Do **not** add a generic "close & ignore" to post-cutover failures (would bypass the
safety boundary).

**(e) Cancelled ≠ failed copy (gap #32).** Give `cancelled` distinct, calmer copy (user chose to
stop) vs `failed` (something went wrong); they are byte-identical today.

**(f) Cancel during `stopping-node` is a silent no-op — fix it (UX5c — issue #10 comment).** Today
clicking Cancel during the multi-minute node-stop window does **nothing**: `service.cancel`
early-returns while `_activeWorkDir`/`_currentProcess` are still null
(`MithrilPartialSyncService.ts:241-247`) and the store never resyncs, so the button looks dead (the
reporter clicked Cancel during node-stop and it didn't respond). This is **distinct from gap #9**, which
is the post-cutover cancel *throw*. Resolution: **disable Cancel during `stopping-node`** with an
explanatory tooltip ("Cancellation available once the node has stopped"), and **always call
`syncStatus()` after any cancel** so the UI never sticks on the optimistic frame. True
abort-during-stop (a cancel flag the coordinator honors before the node-stop handler) is deferred to the
backend-correctness track (D7).

**Latest-drift UX (gap #17):** `PARTIAL_SYNC_LATEST_DRIFT` is a pre-cutover (`preparing`) failure,
so it surfaces as a **specific, retriable** error via (b) — "the verified snapshot advanced while
preparing; please retry" — not a silent reset.

**Locks touched.** Honors "startup-owned recovery independent of diagnostics/running node,"
"cancellation forbidden after cutover" (#6), and "render recovery strictly from
`allowedRecoveryActions`" (#5).

**Open dependents:** backend must ensure cancellation/failure leave staged artifacts cleaned so
`restart-normal` is genuinely safe (gap #16 — backend-correctness, see scope decision D7).

### D7 — Scope boundary: UX + UX-enabling backend; backend-correctness & full QA are referenced deployment gates

**Decision.** This PRD **fully specifies**:
- All UX changes (D1, D4, D5, D6) + the proactive prompt + bootstrap progress consistency.
- The **UX-enabling backend additions**: the behind-ness availability signal (D2), kill-switch
  exposure (D3), file-count progress unification (D4), structured-error→copy mapping (D5b),
  hide-cancel-post-cutover (D5c), and native startup-dialog ownership (D5a).

This PRD **references — but does not re-specify** — the **backend-correctness de-risking track** as
a required deployment-readiness gate:
- Cancellation/failure must leave staged artifacts cleaned so `restart-normal` is safe (gap #16).
- **Staging placement & disk-space preflight (BUG3 — issue #10 comment; user decision 2026-06-19).**
  Partial-sync staging is hardcoded to the **top-level Daedalus state directory**
  (`path.join(stateDirectoryPath, 'mithril-partial-sync')`, `MithrilPartialSyncService.ts:567-569`),
  which **contradicts the backend PRD's own locked text** (`mithril-partial-sync-prd.md:250`: stage
  "under the resolved Mithril work directory"). With a relocated/symlinked chain this fills the **wrong
  volume** and turns cutover into a cross-device copy (~2× transient destination usage). **Fix:** derive
  staging from the **resolved chain work directory** (sibling of the managed chain) so it honors PRD:250
  and cutover is an intra-volume rename, plus add a **disk-space preflight**. **No user storage
  picker** — the "no storage-location picker" non-goal is about *user snapshot pickers*, not the
  engineering placement of scratch space (the docs previously conflated these).
- Latest-snapshot drift backend handling (gap #17) — UX is covered (retriable error, D5); backend
  behavior is correctness work.
- Cutover / immutable-merge correctness re-validation (gaps #18/#19; the immutable-merge fix landed
  in task-401 — re-validate, don't assume).
- The **full manual QA matrix** (all 3 networks × default + custom storage × recovery paths ×
  packaged builds) is the gate before this branch merges to master / ships (per user, 2026-06-19).

**Rationale.** Keeps the PRD coherent as a UX document while making the deployment gates explicit.
The branch will not merge/deploy until both the UX work here **and** the referenced backend +
QA gates are complete.

### D8 — Hygiene, lifecycle, and coverage (batched; clear-cut, recorded without separate grilling)

These have unambiguous resolutions and are recorded as decisions, not open questions:

- **#15 `onReceive` teardown handler (resolved 2026-06-19 — guard, not unsubscribe).** The
  status-channel handler registered in `setup()` is **never removed** on `teardown()`. A true
  unsubscribe is **not cheaply implementable**: `IpcChannel.onReceive` returns `void`, the `IpcReceiver`
  type exposes only `on`/`once` (no `removeListener`), and `onReceive` registers an **anonymous wrapper**
  whose reference is discarded — so nothing can remove it without invasive shared-abstraction surgery (a
  base class used by **every** channel, with zero removal precedent in the renderer). The **existing
  `_isTornDown` guard already neutralizes the correctness risk**: both `_updateStatus` and `syncStatus`
  early-return once `_isTornDown` is set, so the retained handler is **inert**. **Decision:** keep and
  rely on the `_isTornDown` guard; the residual is a single benign listener retained on a singleton
  channel, functionally a no-op. Do **not** undertake IPC-abstraction changes for it. (This supersedes
  the earlier "must unsubscribe" framing, which over-committed to a fix the abstraction cannot support.)
- **#24 retry channel** — **keep `retry` as a re-invocation of the start path** (no new channel);
  reuses the existing seam per the elegance policy. The overlay/store already wire `onRetry →
  startPartialSync`; document the semantics so it reads intentionally.
- **#19/`stopping-node` — populated, in-dialogue node-stop frame (EXPANDED per issue #10 comment,
  UX5a/UX5b).** Keep `stopping-node` as the optimistic start-pending status, but the dialogue must not
  look dead while the node stops (the reporter saw node-stopping only behind the *opaque backdrop*,
  with elapsed `0:00` and greyed steps, for a couple of minutes). Require: (1) an **active
  `stopping-node` step/affordance** in the waterfall (not a greyed placeholder); (2) an **in-dialogue
  reassurance block** mirroring the `isStartingNode` block (`MithrilProgressView.tsx:142-163`) — e.g.
  "Stopping Cardano node… this can take a couple of minutes"; (3) the **ticking elapsed** from the D4
  timer fix so `0:00` advances immediately. Optionally have the backend **emit** `stopping-node` when it
  actually stops the node so the status is truthful rather than purely renderer-optimistic (polish). The
  cancel-during-stop no-op is handled in **D5(f)**.
- **#20 stale i18n** — delete the orphaned `mithrilPartialSyncButtonHint` key from all catalogs;
  correct the "Disabled placeholder CTA" descriptions on `buttonLabel`/hints.
- **#21 hardcoded label** — i18n the diagnostics row label "Mithril Partial Sync"
  (`MithrilPartialSyncSection.tsx:117-121`).
- **#33 dead props** — remove the unused `onWipeRetry`/`onDecline` bootstrap-reuse props from the
  partial-sync overlay path (it always passes an explicit `actions[]`).
- **#25 hand-off copy** — confirmation copy that promises retry/restart/wipe should make clear those
  appear if the attempt fails (they live in the progress/error overlay), not in the confirmation.
- **#34 ancillary disclosure** — **keep user copy simple** ("verified Mithril data"); do **not**
  surface IOG-key / no-ancillary trust nuance in the UI (engineering note only). Revisit only if QA
  shows a need.
- **#13 broken story** — fix the "Partial Sync Confirmation" Storybook story (it sets a
  non-existent state key, so confirmation never renders).
- **#28 / #35 storybook coverage** — add isolated stories for the recommendation, the new
  confirmation **modal**, the **proactive prompt**, all error stages (downloading/converting/
  installing/finalizing) with the full retry+restart+wipe layout, cancelled-vs-failed, and the
  **unified bootstrap + partial-sync file-count progress** states; reuse the locale-at-render-time
  fixture seams.
- **#10 doc re-baseline** — re-baseline references to the live 3-component structure
  (`MithrilPartialSyncSection` / `Recommendation` / `Confirmation`); the 300-series task plans cite
  the pre-refactor inline paths.
- **#30 handoff test** — add direct live-injection coverage for the diagnostics → overlay handoff.

### D9 — Success-path finalization: reset-to-idle + staging cleanup (BUG1 + BUG2 — issue #10 comment)

**Decision (amends locked decision #16).** On a **verified** partial-sync success the backend must
**finalize** the operation — re-arm the CTA, clean staging, and keep the durable marker truthful —
rather than leaving it parked in `completed`:

- **Re-arm the CTA (BUG1).** Today, after a successful sync, the diagnostics option stays disabled with
  *"Unavailable while Mithril work is already active"* until app restart, because `status` stays
  `completed` and `isActive = (status !== 'idle')` (`mithril-partial-sync.types.ts:92-94`), so
  `isActionBlocked` never clears. Fix on **both** sides (user decision, 2026-06-19): (a) the **renderer
  derives `isActionBlocked` from `isWorking`** (in-flight phases) rather than `isActive`, so a terminal
  `completed`/`cancelled`/`failed` never blocks the CTA — this re-arms it immediately; and (b) the
  **backend resets status to `idle`** as the last finalization step (`_resetToIdleStatus`, which already
  exists for the cancel/restart/wipe paths) so it stops reporting `completed` as active to any other
  reader.
- **Clean staging on success (BUG2).** The `mithril-partial-sync` staging folder is left on disk after a
  successful run — `_cleanupPartialSyncArtifacts()` / `fs.remove(stagingRoot)` runs only on cancel /
  restart-normal / wipe, never on success (install removes only the staged DB dir; finalize only clears
  the marker). Fold the staging removal into the **same** verified-success finalization path.
- **Activate the `node-start-verified` marker state (Option A — fixes a dead-code / model-fidelity
  bug).** The marker is a three-state machine — `cutover-in-progress` | `installed-awaiting-node-start`
  | `node-start-verified` (`mithrilPartialSyncMarker.ts:6-9`) — but **`node-start-verified` is never
  written**: the only writes are `cutover-in-progress` (`MithrilPartialSyncService.ts:205`) and
  `installed-awaiting-node-start` (`:209`), and `finalizeInstalledNodeStart` jumps straight from C1 to
  *clearing* the marker (`mithrilPartialSyncNodeStartup.ts:165`). That makes its startup-recovery branch
  (`mithrilPartialSyncNodeStartup.ts:60-63`) **dead code** and leaves the backend PRD's **Boundary C2**
  (`mithril-partial-sync-prd.md:187-194` — "resume normal startup; allow `restart-normal`")
  unimplemented: an interruption in the verify→clear window is handled as the heavier **C1 re-drive**
  instead of a clean normal boot. **Fix (user decision, 2026-06-19 — Option A):** in
  `finalizeInstalledNodeStart`, after the node is proven `RUNNING` (`:159-163`),
  **`writeMithrilPartialSyncMarker('node-start-verified', …)`**, and clear the marker only as the final
  finalization step — so an interrupted-after-verification run **resumes a normal boot** (the `:60-63`
  branch goes live). Low-severity (C1 re-drive is already safe/idempotent — no data-safety hole), but it
  makes the documented boundary model truthful. *(Option B — deleting the unused state — was rejected in
  favour of realising C2.)*
- **Ordered finalization sequence.** Implement the above as one sequence:
  (1) node proven `RUNNING` → (2) stamp marker `node-start-verified` → (3) emit `completed` (success
  overlay shows; the CTA re-arms now via the `isWorking` guard, while the renderer holds the success
  screen via its dismiss flag, honoring lock #16) → (4) on the user's **explicit dismiss** ("Continue to
  Daedalus"), the backend runs `_resetToIdleStatus` **+** `fs.remove(stagingRoot)` **+**
  `clearMithrilPartialSyncMarker()`. Steps (1)–(3) run in the main-process startup path
  (`finalizeInstalledNodeStart`); step (4) runs in `MithrilPartialSyncService`. The marker therefore
  reads `node-start-verified` (Boundary C2) for the whole completed-but-not-dismissed window, and the
  reset + cleanup + marker-clear happen exactly once.
- **Dismiss → backend finalize seam (resolved 2026-06-19).** Steps (1)–(3) and step (4) live in
  different processes/objects, and today `dismissCompletedOverlay` is a **renderer-only flag flip** with
  no backend hook — so the dismiss-driven finalization in step (4) has **no wiring**. Add a **dedicated
  finalize IPC request channel** (a fifth partial-sync action channel alongside start / cancel /
  restart-normal / wipe). `dismissCompletedOverlay` invokes it; `MithrilController` routes it to
  `MithrilPartialSyncService`, which performs the step-(4) reset + staging removal + marker clear
  **idempotently** (safe even when already idle). This is genuinely new behaviour with no existing seam
  to reuse, so a new channel is warranted — it does **not** contradict the reuse policy that kept `retry`
  on the start path (there is simply no finalize seam to reuse). **Why dismiss-driven, not at verified
  success:** an immediate `_resetToIdleStatus` would push `idle`, and because the renderer derives
  `shouldShowOverlay` from the live `status` (and `idle` is not an overlay status), the success screen
  would vanish before the user sees it — violating lock #16. Deferring the reset to dismiss preserves the
  success screen. *(This also corrects an internal inconsistency in the earlier Technical Design, which
  described reset-to-idle + staging removal happening "on verified node-start success"; the authoritative
  trigger is the dismiss-driven finalize.)*
- **Never-dismissed / crash safety.** If the user closes the app on the success screen without
  dismissing, the finalize channel never fires. The marker is then `node-start-verified`, so on next
  launch `handleInterruptedRecovery`'s C2 branch (`mithrilPartialSyncNodeStartup.ts:60-63`) clears the
  marker and resumes a normal boot — **extend that branch to also `fs.remove(stagingRoot)`** so leftover
  staging is reclaimed in the close-without-dismiss / crash case (closes the orphan-staging edge a
  research note flagged on the completed-but-not-dismissed window).
- **Amends locked decision #16.** Research lock #16 currently reads "backend does NOT reset to idle on
  success; explicit renderer dismiss." It is **amended** to: the overlay still follows the live
  lifecycle through `completed` and still requires an explicit user dismiss, **but on that dismiss the
  backend (via the finalize channel above) resets to `idle`, removes staging, and clears the marker**,
  returning the feature to a clean, re-runnable state without an app restart.

**Options considered.** Both backend reset + renderer guard, with Option A realising Boundary C2
*(chosen — most robust + truthful marker model)*; renderer-only (keeps #16 intact, but the backend still
reports `completed` as active and does not clean staging); backend-only (no defense-in-depth); deleting
the dead `node-start-verified` state (Option B — rejected, gives up the lighter C2 normal-boot
recovery).

**Locks touched.** ⚠️ **Amends locked decision #16** (research §5) by explicit user decision,
2026-06-19. Preserves "overlay follows lifecycle through `completed`" and "explicit dismiss"; adds the
post-dismiss reset + staging cleanup + the truthful `node-start-verified` marker (Boundary C2).

**Open dependents:** UX9 (a transient overlay error seen once in the nix dev environment on
Linux/Preprod) is **plausibly a symptom** of leftover staging or a stale marker on next launch, and is
expected to be mitigated by this cleanup; it is otherwise tracked as a triage note under D5b.

### D10 — Holistic EN + JA copy review & polish pass (UX1 — issue #10 comment)

**Decision.** After the copy-touching decisions land (D1 prompt, D5b error map, D5e cancelled-vs-failed,
D6 confirmation modal, D8 i18n hygiene), run a **single holistic English + Japanese copy review** over
**every** `mithrilPartialSync*` string (diagnostics + overlay + prompt) for tone, consistency, and
translation fidelity. Specifically:

- **Correct the research overstatement** that "ja-JP fully translated": the diagnostics keys still
  carry the English string "Mithril partial sync" (`ja-JP.json:158-167`) while the overlay keys use
  「部分同期」 (`ja-JP.json:332-343`). Pick **one canonical ja-JP rendering of "partial sync"** and apply it
  across both namespaces.
- The new D1/D5/D6/D11 copy **materially expands the JA translation burden** (behind-ness line — now
  epochs + sync-%, proactive prompt, stage/code error copy, cancelled-vs-failed) — these need
  first-class JA, not English fallbacks.
- Add a short **EN + JA glossary** for the canonical terms (partial sync, verified data, snapshot,
  cutover, **epochs behind**, **sync percentage**); the existing research glossary (research §7) is
  EN-only.

**Sequencing.** This pass runs **last**, once the copy-producing changes are merged, so it reviews final
strings rather than chasing churn.

**Locks touched.** None; preserves the "verified data" wording lock (#4) and "keep copy simple" (#34).

### D11 — User-facing behind-ness figure: **epochs** (backend-derived from the immutable gap) **+** sync-% context sentence; gate unchanged

> **Partially superseded by D12 (2026-06-25).** The figure *source* is now the exact **node-tip epoch
> difference** computed in the renderer (`max(1, networkTip.epoch − localTip.epoch)`), **not** a backend
> immutable→epochs conversion. D11's gate decision, the epochs-lead + sync-% trailing-sentence framing,
> the never-show-immutable-files rule, and the value-proposition framing all **stand**; only the
> derivation of the number changes. The `behindByEpochs` availability field and the per-network
> `filesPerEpoch` constant described below are **dropped** — see D12.

**Decision (2026-06-24).** Replace the "N immutable files behind" user-facing copy with Cardano-native
language **without changing the behind-ness gate**:

- **Gate unchanged (D2 reaffirmed).** `isSignificantlyBehind` and the offer-or-hide decision remain
  backend-owned and computed from the **certified-immutable gap** (`behindByImmutables`, threshold ≈ 1
  epoch-equivalent). Immutable files are an *internal/engineering* unit and are **never shown to the
  user**. The locked "no renderer-computed threshold" and the D2 truthfulness rationale (offer only when
  a real certified range exists) are preserved.
- **Primary figure = epochs behind, derived from the SAME gap.** The backend converts the gate's own
  immutable gap into an approximate **epochs-behind** figure (`behindByEpochs`) and adds it to the
  `MithrilPartialSyncAvailability` read model. Deriving the displayed figure from the gate's own quantity
  guarantees the user-facing number is **accurately connected to the immutable-based gating** (explicit
  user requirement, 2026-06-24) — it can never disagree with the offer (when the option is shown,
  `behindByImmutables ≥ ~1 epoch`, so epochs ≥ 1). Conversion uses a **per-network files-per-epoch**
  factor (mainnet/preprod ≈ 20 files/epoch = 432,000 slots/epoch ÷ ~21,600-slot immutable chunk;
  preview ≈ 4 = 86,400 slots/epoch). The factor is a **per-network constant keyed off
  `environment.network`** (mirroring the existing `mithrilNetworkConfig.ts` lookup) — **not** derived
  from a live epoch length, which exists only in the renderer's `NetworkStatusStore`, never in the main
  process; an unknown network defaults to 20 (mainnet-equivalent, no throw). The figure is approximate by
  design, so copy says "about"; if an operator tunes the gate threshold below one epoch's worth of files,
  `behindByEpochs` (floored at 1) can round up to "about 1" — benign given the "about" hedge.
- **Secondary context = sync %, as a SEPARATE following sentence.** After the epochs sentence, show the
  node's current sync percentage (`NetworkStatusStore.syncPercentage`, already plumbed into the
  diagnostics recommendation via `mithrilPartialSyncRecommendationWithProgress`). Per explicit user
  directive (2026-06-24) the percentage is its **own trailing sentence** — never interpolated mid-sentence.
  Sync-% is supporting color only; it is **not** the gate and **not** the primary figure (a near-100%
  sync-% must never undercut the "significantly behind" message — precisely why D2 rejected sync-% as the
  gate).
- **Value-proposition framing (explicit user requirement, 2026-06-24).** The copy must convey the
  **time/effort benefit of running partial sync versus simply waiting for normal sync to catch up** — the
  figure exists to show the user they are far enough behind that the recovery flow is worth it. Keep the
  locked "verified … chain data … catch up faster" wording (#4). The gate threshold default (≈ 1 epoch ≈
  ~5 days on mainnet) already corresponds to a meaningful benefit; calibrate so the offer never appears
  when waiting would be just as fast.
- **Unknown-figure fallback retained.** When `behindByEpochs` is unavailable, fall back to the generic
  "Your node is behind the latest verified snapshot." line (no fabricated number).
- **Canonical behind-ness *figure* copy = confirmation modal + proactive prompt.** The epochs-first +
  sync-% framing is the single behind-ness *figure* copy used by the confirmation modal (**revises the
  copy shipped by the completed task-ux-303**) **and** the proactive prompt (task-ux-302), so the nudge
  and the decision read consistently. The **diagnostics recommendation** row
  (`mithrilPartialSyncRecommendation` / `…WithProgress`) already leads with **sync-%** (common Cardano
  vocabulary, *no* "immutable files" language) — it is **not** the source of the user's complaint and
  stays a lighter teaser; adopting the epochs lead there too is **optional polish deferred to the
  holistic copy pass (task-ux-601)**, not required by task-ux-304.

**Options considered.** Epochs (backend-derived from the immutable gap) + sync-% context *(chosen)*;
sync-% gap alone (rejected — near-tip sync-% contradicts "significantly behind"); raw immutable-file
count (rejected — not user vocabulary; this is the issue being fixed); renderer-computed epochs from
node-tip distance (rejected — that measures node-tip vs network-tip, a *different* quantity than the
gate, so it could disagree with the offer and violates the "connected to gating" requirement).

**Locks touched.** ✅ Reaffirms D2 (immutable gate) and "no renderer-computed threshold" — the backend
still owns behind-ness, now including the epochs figure. ✅ Preserves "verified data" wording (#4) and
"keep copy simple" (#34). Adds one optional field (`behindByEpochs`) to the availability read model.

**Open dependents:** none blocking. The files-per-epoch source is **resolved** to a per-network constant
keyed off `environment.network` (mainnet/preprod 20, preview 4, unknown → 20). The final EN+JA behind-ness
sentences (the benefit-vs-waiting framing) are an **`interactive_decision`** requiring user sign-off at
implementation time (task-ux-304), per the sprint's copy-approval stop condition.

### D12 — User-facing epochs figure = exact **node-tip epoch difference** (renderer), not a backend immutable→epochs conversion

**Decision (2026-06-25, supersedes the *figure-source* half of D11).** The displayed "about N epochs
behind" figure is computed in the **renderer** as a whole-epoch subtraction of the node's own consensus
tips, read straight from `NetworkStatusStore`:

```
behindByEpochs = Math.max(1, networkTip.epoch − localTip.epoch)   // node-computed, exact, no constant
```

floored at 1 whenever the option is offered. The sync-% sentence is **retained** as a separate trailing
reference ("Cardano node is currently {X}% synced."), per the locked vocabulary and an explicit user
directive (2026-06-25).

- **Gate unchanged (D2 reaffirmed).** `isSignificantlyBehind` stays backend-owned, computed from the
  certified-immutable gap in `getPartialSyncBehindness` (`MithrilPartialSyncService.ts:677-706`).
  `behindByImmutables` remains an **internal** gate basis; it is **no longer consumed by the renderer for
  display**, and the planned `behindByEpochs` availability field + per-network `filesPerEpoch` table
  (D11) are **dropped** from task-ux-304's backend work.
- **Why node-tip, not the immutable→epochs conversion.** (1) The data is **already exact and already
  plumbed** to both consuming surfaces — `DaedalusDiagnostics` holds `localTip`/`networkTip`
  (`DaedalusDiagnostics.tsx:716-775`) and `SyncingConnectingPage` injects `networkStatus`
  (`SyncingConnectingPage.tsx:18`) — so neither the confirmation modal nor the proactive prompt needs a
  new backend field. (2) **No constant.** It is two integers the consensus layer already computes — the
  same epoch numbers shown in the "Last network block / Last synchronized block" rows on the very same
  Diagnostics screen — so the figure is **visually consistent** with what the user already sees. (3) **It
  cannot undercut the offer.** The network tip is always at-or-ahead of the latest certified snapshot, so
  node-tip distance ≥ the certified-immutable gap; whenever the gate fires (gap ≥ ~1 epoch) the displayed
  epochs is ≥ that, so the "never show 0 epochs while offering" property that motivated D11's 2026-06-24
  "connected to gating" requirement is **preserved without the conversion**.
- **Copy is now literally truthful.** Because the figure is the real distance to the live tip, "Your node
  is about {N} epochs behind the blockchain tip." is exactly correct. Partial sync restores verified data
  down to the latest *certified* point (a small residual — the aggregator's certification lag — is
  finished by normal sync), which the locked "catch up faster" framing already conveys.
- **Whole-epoch subtraction over slot-fractional (explicit user decision, 2026-06-25).**
  `networkTip.epoch − localTip.epoch` is preferred over `round(Δabsolute-slot ÷ epochLength)` for visual
  consistency with the displayed tip rows; the sub-epoch coarseness (±<1 epoch) is benign under the
  "about" hedge.
- **Unknown-figure fallback retained.** When `networkTip`/`localTip` (or their `epoch`) are unavailable,
  fall back to the generic `mithrilPartialSyncConfirmationBehindUnknown` line — no fabricated number.

**Options considered.** Node-tip whole-epoch subtraction in the renderer *(chosen)*; node-tip
slot-fractional (`Δabsolute-slot ÷ epochLength`) rounded (rejected — diverges from the epoch numbers
shown beside it, no real benefit under "about"); backend immutable-gap → epochs via per-network
`filesPerEpoch` (the prior D11 plan — rejected now that exact node data is confirmed already-plumbed; it
added a constant + a backend field for an approximation); hybrid backend-gap + renderer `epochLength`
(rejected — still measures the gate quantity and still needs a chunk-size constant).

**Locks touched.** ✅ Honors "no renderer-computed **threshold**" — this is a *display figure*, not a
threshold; the offer-or-hide gate stays backend-owned. ✅ Reaffirms D2. ✅ Preserves "verified data"
wording (#4) and "keep copy simple" (#34). ✅ Honors the locked behind-ness vocabulary (epochs + sync-%,
never immutable files, % as its own trailing sentence).

**Open dependents:** the final EN+JA behind-ness sentences (benefit-vs-waiting framing) remain an
**`interactive_decision`** at task-ux-304 implementation time. Whether to leave `behindByImmutables` in
the availability read model as an internal/debug field or remove it as cleanup is **non-blocking**.

---

## Overview

Add the missing **"prompt the user when significantly behind the tip"** behavior to the
already-implemented Mithril partial sync feature, and refine the surrounding UX so the diagnostics
trigger, confirmation, progress, success, and recovery flows are correct, honest, and consistent
with the Mithril bootstrap UX. The backend pipeline, IPC, store, and overlay already exist; this is
refinement + closing the 35 enumerated gaps, not a rebuild.

## Problem Statement

Issue #10 asked for a UX that **prompts the user to use a Mithril snapshot when they are
significantly behind the tip**. The shipped implementation has neither half: there is **no
behind-ness detection** (the diagnostics recommendation renders unconditionally) and **no prompt**
(it is a passive row, visible only if the user manually opens Diagnostics). Separately, the
implemented UX carries a blocker (the kill switch is invisible to the renderer, so a default-off
build shows an enabled Start button that throws a raw error) and a cluster of major issues: the
download progress bar mislabels file counts as bytes (and the bootstrap flow shows a synthetic
byte format), long phases look stalled, failures all show generic copy, Cancel is offered when the
backend rejects it, interrupted-cutover recovery shows two competing surfaces, and a renderer
status handler leaks on store teardown.

## Goals

- Satisfy issue #10: detect "significantly behind" in the backend and surface both a **proactive,
  session-dismissible prompt** during sync and a **gated manual trigger** in Diagnostics.
- Close blocker #1 (kill switch invisible to renderer) and the major UX gaps (#4–#9, #11, #14, #15).
- Make download progress **honest and consistent** across partial sync **and** bootstrap.
- Make failures **legible** (stage/code-specific copy) and recovery **safe and single-surfaced**.
- Preserve every locked safety boundary (staged-only restore, boundary recovery model, confirmation
  precedes start, latest-snapshot-only, supported networks).

## Non-Goals

- Auto-**starting** partial sync from any heuristic (locked). The behind-ness signal gates
  *discovery/recommendation* only; the user always confirms before any node-stopping work.
- Snapshot selection or **user-facing** storage-location pickers in partial sync (locked). *(This
  non-goal is about user pickers — the engineering placement of scratch/staging space is **in** scope;
  see D7/BUG3, which colocates staging on the chain volume.)*
- Re-architecting cutover/merge correctness (referenced backend track, D7), or push-vs-poll status
  redesign (status is already push via `onReceive`; the teardown handler is guarded, not unsubscribed —
  D8/#15), or adding removable-listener support to the shared IPC abstraction.
- Cross-platform / mainnet packaged-build certification — that is the QA gate, performed before
  merge/deploy, not produced by this PRD.

## UX Flows (target)

1. **Proactive prompt (NEW).** While the node is connecting/syncing
   (`components/loading/syncing-connecting/SyncingConnecting.tsx`), if `isEnabled &&
   isSignificantlyBehind && !proactivePromptDismissedThisSession`, show a dismissible prompt using the
   canonical D11 behind-ness copy (epochs lead; the sync-% sentence is optional in the space-constrained
   prompt):
   *"Your node is about N epochs behind. Mithril partial sync can catch it up faster than waiting for
   normal sync. [Review] [Not now]"*. **Review** opens the Diagnostics dialog at the partial-sync
   confirmation modal. **Not
   now / dismiss** sets an in-memory session flag (same pattern as `isCompletedOverlayDismissed`) so
   it does not reappear this session, and the copy notes the user can start it later from the
   Diagnostics page. **Review** carries a `{ showMithrilConfirmation: true }` payload so Diagnostics
   opens directly in the confirmation modal (D1 deep-link handoff).
2. **Diagnostics manual trigger.** The recommendation + Start CTA appear in Diagnostics only when
   `isEnabled && isSignificantlyBehind`; hidden otherwise (no enabled button that can throw). Button
   still disabled while any Mithril work is active (`isActionBlocked`).
3. **Confirmation modal (D6/D11).** Scrim + focus trap; behind-ness context line (epochs primary
   sentence + sync-% following sentence, D11); what-happens steps; verified-data wording; primary
   "Start" / secondary "Back to diagnostics"; ESC = back.
4. **Progress overlay (D4).** File-count download readout + real ancillary bytes; active indicator +
   elapsed + "can take several minutes" for verify/convert/install/finalize; **no Cancel** once
   post-cutover; the same presentation is applied to the bootstrap overlay.
5. **Success.** `completed` → "Continue to Daedalus" dismiss → renderer invokes the finalize channel →
   backend resets to `idle`, removes staging, clears the marker → normal app flow resumes, CTA re-armed.
6. **In-session failure/cancel (D5).** React overlay with stage/code-specific copy, recovery actions
   strictly from `allowedRecoveryActions`, distinct cancelled-vs-failed copy, defensive Quit only if
   no actions exist.
7. **Startup-interrupted unsafe cutover (D5a).** Single native blocking dialog (Wipe & full sync /
   Quit); no competing React overlay.

## Requirements

### Functional

- [ ] Backend computes `isSignificantlyBehind` from the certified-immutable gap with a launcher-
  config-tunable threshold; emits a `MithrilPartialSyncAvailability { isEnabled,
  isSignificantlyBehind, behindByImmutables? }` read model on a dedicated channel (D2/D3). The
  user-facing epochs figure is the renderer node-tip difference, not a backend field (D12 — no
  `behindByEpochs` on the contract).
- [ ] Renderer gates **all** partial-sync UI (proactive prompt + diagnostics rec/CTA) on
  `isEnabled`; gates the prompt + manual trigger additionally on `isSignificantlyBehind` (D1/D3).
- [ ] Proactive, session-dismissible prompt on the syncing screen that routes into the Diagnostics
  confirmation (D1).
- [ ] The prompt's **Review** deep-links into the confirmation modal via an
  `openDaedalusDiagnosticsDialog({ showMithrilConfirmation })` payload + a `showConfirmationOnOpen` prop;
  confirmation still precedes start (D1/lock #3).
- [ ] Confirmation is a focus-trapped modal with behind-ness context and a proper button hierarchy
  (D6).
- [ ] User-facing behind-ness copy uses **epochs** (renderer-computed node-tip difference
  `max(1, networkTip.epoch − localTip.epoch)`, D12) as the primary sentence and **sync-%** as a separate
  following sentence; immutable files are never shown to the user; copy conveys the time/effort benefit vs waiting (D11/D12).
- [ ] Download progress is file-count based and truthful in **both** partial sync and bootstrap;
  long phases show active progress (D4).
- [ ] Failures show stage/code-specific copy; cancelled ≠ failed; Cancel hidden post-cutover;
  defensive Quit fallback (D5).
- [ ] Startup-interrupted unsafe-cutover recovery uses the single native dialog (D5a).
- [ ] `MithrilPartialSyncStore` guards the status handler with `_isTornDown` so it is inert after
  teardown (no IPC-abstraction unsubscribe; benign listener retention documented) (D8/#15).
- [ ] i18n cleanup: remove orphaned key, fix descriptions, localize the row label (D8).
- [ ] Active step circle animates (rotating spinner) in **both** partial sync and bootstrap, and elapsed
  ticks continuously via a renderer-side timer (D4/UX6/UX7).
- [ ] `stopping-node` shows a populated, in-dialogue node-stop frame (active step + reassurance +
  ticking elapsed); Cancel is disabled-with-explanation during `stopping-node` and the UI always
  resyncs after any cancel (D8#19/D5f/UX5).
- [ ] On success-overlay **dismiss** the renderer invokes a finalize channel; the backend resets to
  `idle`, removes the staging dir, and clears the marker (idempotently), while the renderer derives
  `isActionBlocked` from `isWorking` so the CTA re-arms on `completed` without an app restart; the C2
  startup branch also reclaims leftover staging if the app was closed without dismissing (D9/BUG1/BUG2).
- [ ] Partial-sync staging is colocated on the resolved chain volume with a disk-space preflight
  (D7/BUG3).
- [ ] Holistic EN+JA copy review + one canonical ja-JP "partial sync" rendering + glossary (D10/UX1).

### Non-Functional

- Preserve all locked safety boundaries (research §5): staged-only restore, boundary A/B/C1/C2
  recovery model rendered strictly from `allowedRecoveryActions`, confirmation precedes start,
  cancellation forbidden post-cutover, latest-snapshot-only, supported networks, serialized
  mutation lock, marker-driven startup recovery, no synthetic throughput/remaining-time over IPC.
- The behind-ness Mithril query must be **cached** and low-frequency (no tight polling of the
  aggregator while syncing).
- Renderer stays threshold-free (the backend owns the threshold).
- No regression to the bootstrap success path from the shared-progress change.

## Technical Design

### Contracts (`source/common/types/mithril-partial-sync.types.ts`, `source/common/ipc/api.ts`)
- Add `MithrilPartialSyncAvailability` + a dedicated availability channel (one-shot query at store
  setup + periodic refresh while syncing). Keep it **separate** from the operation
  `MithrilPartialSyncStatusSnapshot`. `behindByImmutables?` stays the internal **gate basis only**; the
  user-facing epochs figure is the **renderer** node-tip difference, **not** a backend field — no
  `behindByEpochs` is added to the contract (D12 supersedes D11).
- Add a partial-sync error-copy mapping keyed by `error.code` / `MithrilPartialSyncErrorStage`.
- Add a **finalize/dismiss request channel** (a fifth partial-sync action channel) the renderer invokes
  on success-overlay dismiss to trigger the backend reset-to-idle + staging removal + marker clear (D9).

### Main process
- `MithrilPartialSyncService` / preflight: expose a cached behind-ness check reusing the existing
  latest-snapshot resolver (`derivePartialSyncRange` math) without running a restore — it computes the
  gate (`isSignificantlyBehind`) and the internal `behindByImmutables` only. The user-facing epochs figure
  is computed in the **renderer** from the node tips (D12); the main process does **no** epochs conversion
  and carries **no** files-per-epoch factor.
- `chainStorageCoordinator` / `config.ts`: surface `mithrilPartialSyncEnabled` into the availability
  read model.
- `mithrilPartialSyncNodeStartup.ts`: drop the redundant React `failed` emission on the
  startup-interrupted path (native dialog only); in `finalizeInstalledNodeStart`, **write the marker to
  `node-start-verified` after the node is proven `RUNNING`** (activating the live-but-currently-dead
  `:60-63` Boundary C2 branch) and defer `clearMithrilPartialSyncMarker()` to the dismiss-driven finalize
  step (D9, Option A); **extend the C2 branch (`:60-63`) to also `fs.remove(stagingRoot)`** so a
  close-without-dismiss reclaims leftover staging on next launch (D9 never-dismissed safety).
- Set `mithrilPartialSyncEnabled = true` on this branch's launcher config for manual testing.
- `MithrilPartialSyncService`: handle the new **finalize/dismiss** channel — on success-overlay dismiss
  run `_resetToIdleStatus` **+** `fs.remove(stagingRoot)` **+** `clearMithrilPartialSyncMarker()`
  **idempotently** (D9). This (not verified-success) is the authoritative reset/cleanup trigger; the
  verified-success path only stamps the marker + emits `completed` so the success screen survives (see
  D9). Derive the staging path from the **resolved chain work dir** (sibling of the managed chain) rather
  than the top-level state dir, and add a disk-space preflight (D7/BUG3); always re-emit status after a
  `cancel` request, and treat cancel during `stopping-node` as a no-op the renderer disables (D5f).

### Renderer
- `MithrilPartialSyncStore`: consume availability; add `proactivePromptDismissedThisSession`
  in-memory flag; **rely on the `_isTornDown` guard for teardown** (no IPC-abstraction unsubscribe — see
  D8/#15); add `startedAt` (anchor for the elapsed timer); derive `isActionBlocked` from `isWorking`
  (in-flight phases) not `isActive` so terminal states re-arm the CTA (D9); **on `dismissCompletedOverlay`
  invoke the finalize channel** so the backend resets to idle + cleans staging + clears the marker (D9);
  resync status on cancel (D5f).
- `SyncingConnecting.tsx`: mount the proactive prompt (gated, dismissible); its **Review** action calls
  `openDaedalusDiagnosticsDialog.trigger({ showMithrilConfirmation: true })` (D1 deep-link handoff).
- `MithrilPartialSyncConfirmation` → modal (scrim/focus-trap/hierarchy/ESC); behind-ness line =
  epochs (renderer node-tip difference `max(1, networkTip.epoch − localTip.epoch)`, D12) primary sentence
  + sync-% (existing `syncPercentage`) separate trailing sentence; no "immutable files" language to the
  user (D11/D12). `DaedalusDiagnostics` computes the figure from `localTip`/`networkTip` and passes it down.
- `MithrilPartialSyncRecommendation` / `Section`: gate visibility on availability; `Section` accepts a
  `showConfirmationOnOpen` prop (threaded from `openDaedalusDiagnosticsDialog`'s `showMithrilConfirmation`
  payload via `DaedalusDiagnostics`) and opens the confirmation modal on mount, respecting
  `isActionBlocked` (D1 deep-link handoff).
- `MithrilProgressView` / `MithrilStepIndicator`: file-count download semantic for both flows;
  active-phase affordance; render the existing `.iconSpinner`/`loading-spin` for the active top-level
  step (rotating circle, both flows — UX7); add an `isStoppingNode` reassurance block mirroring
  `isStartingNode`; drive elapsed from `bootstrapStartedAt`, not the frozen `elapsedSeconds` (D4/UX6).
- `MithrilStepIndicator.spec.tsx`: update the assertion that locks `.iconSpinner` to null (UX7).
- `MithrilPartialSyncOverlay`: stage/code error copy; extend `hideAction` to all post-cutover
  phases; defensive Quit fallback; remove dead `onWipeRetry`/`onDecline`; disable Cancel during
  `stopping-node` with an explanatory tooltip; stop passing the frozen `elapsedSeconds` prop (D5f/D4).
- `MithrilBootstrapStore` / `MithrilBootstrap`: drop the synthetic-byte getter; adopt file-count
  display (+ optional static total-size context).

### Storybook / tests
- Fix the broken confirmation story; add stories per D8/#28; add live-injection handoff test
  (#30); update bootstrap progress tests for the file-count semantic.

## Testing Strategy

- **Automated (agent-executable):** Jest main-process (availability/threshold, kill-switch gating,
  startup-dialog single-surface, hide-cancel-post-cutover, error-copy mapping, store teardown
  unsubscribe), renderer (gated visibility, proactive prompt + session dismissal, confirmation
  modal, file-count progress for both flows, recovery-action rendering), IPC round-trips, and
  Storybook visual coverage. Add coverage for the issue-#10-comment items: success-path reset-to-idle +
  staging cleanup + CTA re-arm (D9), staging colocation + disk preflight (D7/BUG3),
  elapsed-ticks-via-renderer-timer + animated active step circle (D4/UX6/UX7), and the populated
  `stopping-node` frame + cancel-disabled-during-stop + resync-on-cancel (D8#19/D5f).
- **Manual QA gate (deployment prerequisite, D7):** all 3 networks × default + custom chain storage
  × success/cancel/retry/restart-normal/wipe-and-full-sync recovery paths × packaged builds on
  Windows/macOS/Linux. Re-test the six task-401 regressions individually (gap #18). This branch does
  not merge/ship until this matrix passes.

## Rollout / Kill Switch

- `mithrilPartialSyncEnabled` remains the rollout lever; exposed to the renderer so all partial-sync
  UI hides when off. **This branch enables it** for full manual testing.
- **Production default deferred** to deployment-readiness (post full QA); flippable via launcher
  config without an app release. The startup-owned native recovery dialog remains available for any
  already-interrupted unsafe-cutover install even if the diagnostics entry is disabled.

## Open Questions

- Exact `THRESHOLD_IMMUTABLES` value + refresh cadence for the behind-ness check (calibrate during
  QA; default ≈ ≥1 epoch-equivalent).
- Whether partial sync's snapshot metadata can cheaply carry a real total **size** for the optional
  "≈ N GB total" context line (else omit).
- Final production default / network-scoping of the kill switch (deferred to readiness gate).

## Remaining-Issues Coverage Map (all 35 research gaps)

| Gap(s) | Disposition |
|--------|-------------|
| #1 kill switch invisible | **D3** — expose via availability read model; gate all UI |
| #2 no "behind" detection/prompt | **D1 + D2** — backend signal + proactive prompt + gated trigger |
| #3 QA matrix deferred | **D7** — full manual QA is the deployment gate |
| #4 file-count-as-bytes | **D4** — file-count semantic, both flows |
| #5 looks-stalled phases | **D4** — active indicator + elapsed + reassurance |
| #6 generic error copy | **D5b** — partial-sync stage/code copy map |
| #7 dual startup recovery | **D5a** — single native dialog |
| #8 no close affordance | **D5d** — recovery actions are exits + defensive Quit |
| #9 Cancel after cutover | **D5c** — hide Cancel post-cutover |
| #10 plans diverge from code | **D8** — re-baseline docs |
| #11 prose-only confirmation | **D6** — decision-style modal + behind-ness |
| #12 thin/unlocalized start error | **D5b/D6** — localized, humanized copy |
| #13 broken confirmation story | **D8** — fix story |
| #14 button hierarchy | **D6** — primary/secondary |
| #15 onReceive teardown leak | **D8** — guard handler with `_isTornDown` (inert after teardown; benign listener retention documented) |
| #16 cancel cleanup safety | **D7** — backend-correctness track (UX safe via D5) |
| #17 latest-drift UX | **D5** — retriable specific copy (backend behavior in D7) |
| #18/#19(merge) cutover/merge correctness | **D7** — re-validate; QA gate |
| #19(status) stopping-node | **D8#19** (expanded) — populated in-dialogue node-stop frame; **D5(f)** cancel-during-stop fix |
| #20 stale i18n | **D8** — delete orphaned key, fix descriptions |
| #21 hardcoded label | **D8** — i18n it |
| #22 in-place confirmation | **D6** — modal |
| #23 coarse close during confirm | **D6** — ESC = back to diagnostics |
| #24 no retry channel | **D8** — keep retry = start (reuse seam) |
| #25 hand-off copy | **D8** — clarify |
| #26 snapshot identity unsurfaced | **D2/D6** — surface behind-ness; full metadata = non-goal |
| #27 historical UX undocumented | this PRD documents the partial-sync UX |
| #28 missing stories | **D8** — add stories |
| #29 no "how far behind" copy | **D2/D6/D11/D12** — behind-ness line: epochs (renderer node-tip difference) + sync-% context |
| #30 handoff under-tested | **D8** — live-injection test |
| #31 no dismiss for failed/cancelled | **D5d** — actions are exits; no unsafe ignore |
| #32 cancelled==failed copy | **D5e** — distinct copy |
| #33 dead overlay props | **D8** — remove |
| #34 ancillary disclosure | **D8** — keep copy simple (non-goal to expose) |
| #35 storybook fixture seams | **D8** — reuse locale-at-render fixtures |

## Issue #10 Comment Coverage Map (comment 4623095092 — all 13 items)

Cross-checked against live code by the 2026-06-19 audit (`research/19`, verification pass 2) and the
round-2 grilling decisions above. This is the direct answer to "did all the comment items get
integrated?"

| Item | Comment ask | Disposition | Decision / locus |
|------|-------------|-------------|------------------|
| UX1 | EN + JA copy review & polish | scoped | **D10** — holistic EN+JA pass after copy churn; one canonical ja-JP "partial sync"; glossary |
| UX2 | Diagnostics partial-sync copy cleanup | integrated | **D8** #20/#21 + behind-ness line (D2/D6) |
| UX3 | Acknowledgment-message cleanup | integrated | **D6** modal confirmation; **D8** #25 hand-off copy |
| UX4 | Disable option when < 5% from tip | reframed (signed off) | **D1** sign-off — *hide* (not disable) on the **D2** certified-immutable gap (not sync-%); display figure = epochs + sync-% (**D11**) |
| UX5 | Node-stop refinement (no updates / `0:00` / greyed / hidden in bg / cancel dead) | scoped | **D8#19** in-dialogue node-stop frame; **D4** ticking elapsed; **D5(f)** cancel-during-stop fix |
| UX6 | Elapsed only updates on step finish (freezes) | scoped | **D4** — renderer-side ticking timer (`bootstrapStartedAt`) |
| UX7 | Step circles don't rotate (also bootstrap) | scoped | **D4** — render the existing `.iconSpinner` for the active step, both flows |
| UX8 | Uses files instead of estimated size | reframed (signed off) | **D4** sign-off — honest file counts both flows; size = static "≈ N GB total" context only |
| UX9 | Overlay error in nix dev-env (Linux/Preprod) | triage note | tracked under **D5b**; plausibly mitigated by **D9** staging cleanup / stale-marker reset |
| BUG1 | CTA stuck "active" after success | scoped | **D9** — backend reset-to-idle (amends lock #16) + renderer `isWorking` guard |
| BUG2 | Staging folder leftover after success | scoped | **D9** — `fs.remove(stagingRoot)` on verified success |
| BUG3 | Staging on top-level state dir / disk / symlink | scoped | **D7** — colocate on resolved chain volume + disk preflight (honors PRD:250); no user picker |
| BUG4 | Windows slow node stop → on-disk verification | **deferred** | Out of scope here — cardano-node's own unclean-shutdown recovery (Windows-only, not partial-sync-specific). File as a standalone issue (user decision 2026-06-19). |

**Verdict.** All 13 items are now dispositioned: **2** were already integrated (UX2, UX3); **7** are
scoped into D4/D5/D7/D8/D9/D10 (UX1, UX5, UX6, UX7, BUG1, BUG2, BUG3); **2** are reframed with explicit
user sign-off where the PRD intentionally diverges from the literal ask (UX4, UX8); **1** is a tracked
triage note (UX9); and **1** is deliberately deferred to a separate issue (BUG4).

---

**Status:** Draft for review (grilling round 5 complete — behind-ness *figure source* changed to the
exact node-tip epoch difference computed in the renderer via D12, superseding D11's backend
immutable→epochs derivation; the gate and the epochs + sync-% framing are unchanged; round 4: behind-ness
display refined to Cardano-native epochs + sync-% via D11; round 3: success-finalization seam,
proactive-prompt deep-link handoff, and `onReceive` teardown reconciled; issue #10 comment 4623095092
audited & integrated) · **Date:** 2026-06-25 · **Author:** Claude (with David Kirshon)
