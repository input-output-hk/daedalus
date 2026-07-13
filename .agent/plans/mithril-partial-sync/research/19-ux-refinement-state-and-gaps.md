# 19 — Mithril Partial Sync UX Refinement: Current State & Remaining Issues

> Research dossier for **issue #10 "Mithril Snapshot UX Design — Phase 2"**, supporting the
> UX-refinement PRD on branch `feat/mithril-partial-sync-ux-refinement`.
> Produced by a 6-reader parallel survey + adversarial gap-check; **corrections from the
> verification pass are folded in below** (notably the push-vs-poll inversion). Where a claim
> was corrected, the corrected statement is authoritative and the original is annotated.

Date: 2026-06-19 · Method: multi-agent research workflow (`wf_d9183642-c36`), lead-verified.

---

## 1. Current State Snapshot

The feature is **wired end-to-end on this branch — not a placeholder**. All six readers
independently confirmed a working backend → IPC → store → overlay pipeline. Remaining work is
UX *refinement* and closing deferred/residual issues, not net-new implementation.

### Genuinely done

**Backend orchestration (main process)**
- Full pipeline `preparing → downloading → verifying → converting → installing → finalizing`,
  serialized behind the `chainStorageCoordinator` mutation lock, fronted by `MithrilController`
  (`source/main/mithril/MithrilPartialSyncService.ts`).
- **12-value** status enum (`idle`, `stopping-node`, `preparing`, `downloading`, `verifying`,
  `converting`, `installing`, `finalizing`, `starting-node`, `completed`, `failed`, `cancelled`),
  transfer-progress detail, structured progress items, structured error object, and a
  backend-computed `allowedRecoveryActions`, all emitted as one `MithrilPartialSyncStatusSnapshot`
  (`source/common/types/mithril-partial-sync.types.ts:3-54`; `source/common/ipc/api.ts:459-463`).
- Per-boundary recovery gating: pre-cutover allows all three actions; at/post-cutover
  (installing/finalizing/starting-node) allows only `wipe-and-full-sync`, derived in
  `_deriveAllowedRecoveryActions` and re-asserted server-side in `_assertRecoveryActionAllowed`
  (`MithrilPartialSyncService.ts:506-535`).
- Cancel hard-rejected after cutover (`MithrilPartialSyncService.ts:249-253`).
- Durable on-disk marker (`mithril-partial-sync.lock`) with three states; unreadable marker
  treated as unsafe (`mithrilPartialSyncMarker.ts:6-20,71-75`). Startup gate + node-startup
  recovery handle interrupted runs (`MithrilStartupGate.ts:157-205`,
  `mithrilPartialSyncNodeStartup.ts:53-172`).
- Snapshot-drift guard re-validates the certified range mid-prep, fails with
  `PARTIAL_SYNC_LATEST_DRIFT` (`MithrilPartialSyncService.ts:169-180`).
- Kill switch `mithrilPartialSyncEnabled` enforced at coordinator level
  (`chainStorageCoordinator.ts:37-38,375-379`); **nix default `false`**
  (`nix/internal/launcher-config.nix:409`). The task-401 checkpoint generated it `true` by
  explicit user override — **ground-truth default was contested; RESOLVED in PRD D3** (branch sets
  `true` for manual testing; production default deferred to the readiness gate).

**Store / IPC (renderer)** — *corrected from original dossier*
- **The status pipeline is PUSH-DRIVEN via `onReceive`, not polling.** `MithrilPartialSyncStore`
  registers `mithrilPartialSyncStatusChannel.onReceive(update => this._updateStatus(update))` in
  `setup()` (`MithrilPartialSyncStore.ts:64`) and uses `request()` only **once** for the initial
  snapshot (`:67,123`). The main process actively pushes via
  `mithrilPartialSyncStatusChannel.send(status, window.webContents)`
  (`source/main/ipc/mithrilPartialSyncChannel.ts:84-85,110-114`). There is **no polling loop**.
  *(The original dossier called this "lifecycle-safe polling, deliberately not onReceive"; the
  verification pass proved that backwards. The corrected fact reverses former "constraint 17" and
  the former out-of-scope "push-driven status" item.)*
- Store maps backend snapshots verbatim (`this.status = update.status`) and derives
  `shouldShowOverlay`, `canRetry`, `canRestartNormally`, `canWipeAndFullSync`
  (`MithrilPartialSyncStore.ts:77-147`).
- Four real IPC actions: start, cancel, restart-normally, wipe-and-full-sync. **No retry channel**
  — retry re-invokes `startPartialSync` (`App.tsx:115`; `mithrilPartialSyncChannel.ts:22-45`).

**UX scaffolding (renderer)**
- Diagnostics section split (post-plan refactor, commit `69d258fa5`) into
  `MithrilPartialSyncSection.tsx`, `MithrilPartialSyncRecommendation.tsx`,
  `MithrilPartialSyncConfirmation.tsx`.
- Full-screen `MithrilPartialSyncOverlay` reuses bootstrap `MithrilProgressView` +
  `MithrilStepIndicator` + `MithrilErrorView` (`MithrilPartialSyncOverlay.tsx:14-16,83-174`),
  mounted from `App.tsx:97-123` gated on `shouldShowOverlay`.
- Diagnostics auto-closes on transition into an overlay status
  (`DaedalusDiagnosticsDialog.tsx:13-45`).
- ~25 i18n keys present across en-US, ja-JP, defaultMessages; a test enforces no `!!!` placeholders for
  the overlay strings (`MithrilPartialSyncOverlay.spec.tsx`). **Correction (verification pass 2,
  2026-06-19):** ja-JP is **not** fully translated — the *diagnostics* keys still carry the English
  "Mithril partial sync" (`ja-JP.json:158-167`) while the *overlay* keys use 「部分同期」
  (`ja-JP.json:332-343`). See PRD **D10** (holistic EN+JA copy pass + one canonical rendering).
- Storybook + automated coverage (≈10 suites / 121 tests) exist (task-400, task-304).

### Stubbed / placeholder / stale (cleanup, not missing function)
- **Orphaned i18n key** `mithrilPartialSyncButtonHint` ("Available after the confirmation step is
  added.") ships in all catalogs with **zero runtime references**; the live component uses
  `buttonHintReady`/`buttonHintBlocked` (`MithrilPartialSyncRecommendation.tsx:27-39,65-67`).
- **Stale description**: `buttonLabel` is still described as a "Disabled placeholder CTA label"
  though the button is functional (`MithrilPartialSyncRecommendation.tsx:25`).
- **`stopping-node`** is set optimistically by the store but **never emitted by the backend**
  (`MithrilPartialSyncStore.ts:31,157-166`; backend goes idle → preparing at
  `MithrilPartialSyncService.ts:127-135`).
- **Task plans are stale** relative to the branch: every 300-series plan describes the UX as
  inline in `DaedalusDiagnostics.tsx`; the refactor moved it into the three components above.

---

## 2. Headline Finding: CTA Wiring Status

**The diagnostics "Mithril Partial Sync" button IS fully wired and launches the flow. It is NOT a
disabled placeholder.** Lead-verified live.

`MithrilPartialSyncRecommendation.tsx:79-86`:
```jsx
<button
  className={styles.mithrilPartialSyncButton}
  disabled={isActionBlocked}      // disabled ONLY while other Mithril work runs
  onClick={onShowConfirmation}    // opens the confirmation panel
  type="button"
>
```
Flow: button → inline confirmation panel (`MithrilPartialSyncSection.tsx:105-114`) → confirm →
`stores.mithrilPartialSync.startPartialSync` via IPC (`DaedalusDiagnosticsDialog.tsx:137`;
`MithrilPartialSyncStore.ts:156-185`) → diagnostics auto-closes → overlay mounts (`App.tsx:97-123`).

**Why the "placeholder" myth persists:** task-301 *intentionally* shipped a disabled placeholder;
task-302 made it actionable; refactor `69d258fa5` restructured it. The dead
`mithrilPartialSyncButtonHint` copy and stale `buttonLabel` description are fossils of task-301.

**Critical caveat (the real "broken on a stock build" risk):** the button gates **only** on
`isActionBlocked`, never on the kill switch. On a default-off build the recommendation + enabled
CTA still render; clicking sends `start`, which the coordinator rejects with the raw string
`"Mithril partial sync is disabled by launcher configuration."` — stored in
`startFromConfirmation` (`MithrilPartialSyncSection.tsx:90-95`) and rendered verbatim by the child
(`MithrilPartialSyncConfirmation.tsx:74`). The kill switch is **invisible to the renderer**. See §3 #1.

---

## 3. Remaining Issues / UX Gaps (consolidated, severity-ordered)

| # | Title | Severity | Evidence | Surface |
|---|-------|----------|----------|---------|
| 1 | **Kill switch invisible to renderer; default-off build shows an always-enabled Start button that throws a raw error.** Refined UX must hide/disable the entry point when off, or expose the flag in the snapshot contract. | **Blocker** | `chainStorageCoordinator.ts:37-38,376-377`; `MithrilPartialSyncRecommendation.tsx:79-86`; `MithrilPartialSyncConfirmation.tsx:74`; `launcher-config.nix:409` | renderer + contract |
| 2 | **No "significantly behind the tip" detection or prompt — issue #10's core trigger is unimplemented.** Recommendation renders unconditionally; `isSynced` only swaps copy. Backend `derivePartialSyncRange` computes behind-ness only at start-time, never surfaced. | **Blocker** | `DaedalusDiagnostics.tsx:704-711`; `MithrilPartialSyncRecommendation.tsx:62-86`; `mithrilPartialSyncPreflight.ts:143-160`; issue #10 | renderer + backend + contract |
| 3 | **Manual QA matrix largely deferred; "enabled by default" rests on user override, not evidence.** Only preview/Linux/yarn-dev validated; mainnet/preprod/custom-storage/packaged/recovery paths deferred. *(Disposition: full QA is the deployment gate — PRD D7; kill-switch default resolved — PRD D3.)* | **Blocker** | `research/18-task-401-manual-qa-results.md:19-25,155-159`; PRD:323,338 | process / QA |
| 4 | **Overlay mislabels file COUNTS as BYTES.** `transferProgress.filesDownloaded/filesTotal` are integer file counts fed into `bytesDownloaded`/`snapshotSize` byte-props with `showDownloadProgressBar` unconditionally on. Only the ancillary stream carries real bytes. **Contradicts note-15's "keep footer disabled until truthful byte telemetry."** | **Major** | `MithrilPartialSyncOverlay.tsx:87-88,114` (lead-verified); `MithrilPartialSyncService.ts:402-417`; `research/15:11` | renderer + maybe backend |
| 5 | **Long installing/finalizing windows look stalled; no per-phase progress or ETA.** Only `elapsedSeconds` + step state drive non-download phases. task-303 *disabled* the byte footer rather than replacing it, so partial sync shows less detail than bootstrap. | **Major** | `research/18:147,163`; `MithrilPartialSyncService.ts:711-754`; task-303.md:328 | renderer + backend |
| 6 | **All failures show generic copy — stage/code-specific error copy never reached.** Overlay always passes explicit `title`+`hint` (`MithrilPartialSyncOverlay.tsx:121-130`), which short-circuits `ERROR_COPY_BY_STAGE` in `MithrilErrorView` *before* it is consulted — and that map is keyed to the *bootstrap* vocabulary anyway. Backend emits structured codes + a correctly-typed `MithrilPartialSyncErrorStage`, but the renderer never maps them. | **Major** | `MithrilPartialSyncOverlay.tsx:121-130`; `MithrilErrorView.tsx:41-60,88-94,114`; `mithril-partial-sync.types.ts:22-37` | renderer |
| 7 | **Interrupted unsafe-cutover recovery uses BOTH a native Electron dialog AND the React overlay.** `handleInterruptedRecovery` emits `failed` (overlay mounts) AND blocks on `dialog.showMessageBox`. Two competing surfaces, divergent button sets. | **Major** | `mithrilPartialSyncNodeStartup.ts:69-105`; `App.tsx:97-123`; `MithrilStartupGate.ts:194-205` | main + renderer |
| 8 | **Error/recovery overlay has no close affordance — dead-end if `allowedRecoveryActions` is empty.** Not reachable today (backend always emits ≥1), but no safety net. | **Major** | `MithrilErrorView.tsx:110-170`; `MithrilPartialSyncOverlay.tsx:117-174`; `App.tsx:97` | renderer |
| 9 | **Cancel button shown during installing/finalizing, but backend rejects cancel there.** Overlay hides action only for `starting-node`; clicking Cancel post-cutover silently rejects. | **Major** | `MithrilPartialSyncOverlay.tsx:113`; `MithrilPartialSyncService.ts:249-253` | renderer |
| 10 | **Live component structure diverges from every task plan** (refactor `69d258fa5`). Re-baseline PRD references. | **Major** | git `69d258fa5`; `MithrilPartialSyncSection.tsx` vs task-302.md:115-118 | docs |
| 11 | **Confirmation view is prose-only — no snapshot metadata, download size, "how far behind", or storage context.** Bootstrap's decision view shows a details grid; partial sync shows three paragraphs. | **Major** | `MithrilPartialSyncConfirmation.tsx:62-93`; `MithrilDecisionView.tsx:88-95`; `MithrilPartialSyncService.ts:140-167` | renderer + contract |
| 12 | **Start-rejection error path is thin and not localized** (hardcoded English fallback "Unable to start Mithril partial sync."). | **Major** | `MithrilPartialSyncSection.tsx:90-95`; `MithrilPartialSyncConfirmation.tsx:74`; `MithrilPartialSyncStore.ts:33-47` | renderer |
| 13 | **Broken "Partial Sync Confirmation" Storybook story** — sets a non-existent state key; confirmation never renders. | **Major** | `Diagnostics.stories.tsx:76-85`; `MithrilPartialSyncSection.tsx:19-30` | storybook |
| 14 | **Two confirmation buttons share the same cancel style class — no primary/secondary hierarchy** for a node-stopping, chain-rewriting action. (Readers split major/minor; treated major.) | **Major** | `MithrilPartialSyncConfirmation.tsx:77-91` | renderer |
| 15 | **`onReceive` status handler never unsubscribed on store teardown.** `teardown()` only sets `_isTornDown`; the subscription from `setup()` line 64 is never removed, so after `window.daedalus.reset()` the stale handler persists. **⚠ Disposed (PRD D8, 2026-06-19):** the `_updateStatus`/`syncStatus` `_isTornDown` guards already make the retained handler **inert**, so this is *benign listener retention*, not a correctness bug — downgraded from "real leak." A true unsubscribe is not cheaply implementable (`onReceive(): void`, no `removeListener` on `IpcReceiver`, anonymous wrapper discarded); PRD keeps the guard rather than adding unsupported IPC-abstraction surgery. | **Minor** | `MithrilPartialSyncStore.ts:63-75,128-131`; `common/ipc/lib/IpcChannel.ts:9-18,152-167` | renderer |
| 16 | **Cancellation mid-download leaves cleanup-required, non-restart-safe artifacts.** Post-cancel UX undefined; not exercised in task-401. | **Major** | `research/02-validation-spike-results.md:242-246,258` | backend + renderer |
| 17 | **Latest-snapshot drift can invalidate the range mid-run; user-facing UX undefined.** Backend has `PARTIAL_SYNC_LATEST_DRIFT`, but silent-retry vs visible-error vs reset is undocumented. | **Major** | `research/02:206-209,326`; `MithrilPartialSyncService.ts:169-180` | backend + renderer |
| 18 | **Six QA bugs re-tested only via a single combined pass; individual fixes pending re-test.** | **Major** | `research/18:69,85,101,118,135,159` | QA |
| 19 | **Install/cutover merge correctness not fully de-risked** (immutable-merge fix required to avoid missing `00000.primary`). | **Major** | `research/02:261-264`; `research/18:135` | backend |
| 20 | **Stale i18n: orphaned `mithrilPartialSyncButtonHint` + "Disabled placeholder CTA" description.** | **Minor** | `en-US.json:156`; `defaultMessages.json:1856,1860-1862`; `MithrilPartialSyncRecommendation.tsx:25,65-67` | i18n |
| 21 | **Diagnostics row label "Mithril Partial Sync" is hardcoded English, not i18n.** | **Minor** | `MithrilPartialSyncSection.tsx:117-121` | renderer / i18n |
| 22 | **Inline confirmation swaps the diagnostics row in-place** (560px panel inside the status grid, no scrim/focus trap). | **Minor** | `MithrilPartialSyncSection.tsx:100-114`; `DaedalusDiagnostics.scss:344-388` | renderer |
| 23 | **Coarse diagnostics close affordances during confirmation** (ESC/overlay-click exits whole dialog mid-confirmation). | **Minor** | task-302.md:132-137,169 | renderer |
| 24 | **No retry IPC channel — "retry" reuses `startPartialSync`**, conflating first-start and retry-after-failure. | **Minor** | `App.tsx:115`; `mithrilPartialSyncChannel.ts`; `common/ipc/api.ts:454-478` | renderer / IPC |
| 25 | **Confirmation recovery copy promises actions only shown in a different surface** (the overlay the user has left). | **Minor** | `en-US.json:163`; `MithrilPartialSyncOverlay.tsx:131-170` | copy |
| 26 | **Snapshot identity/range computed but never surfaced** ("you are at block X, snapshot brings you to Y"). | **Minor** | `MithrilPartialSyncService.ts:140-167`; `mithrilPartialSyncPreflight.ts:143-160`; `mithril-partial-sync.types.ts:39-54` | backend + contract |
| 27 | **Partial sync undocumented in the historical UX plan** (`mithril-snapshot-ux.md` covers only empty-DB). | **Minor** | `.agent/plans/mithril/mithril-snapshot-ux.md` | docs |
| 28 | **No isolated Storybook stories for recommendation/confirmation; error stages incomplete** (missing downloading/converting/installing/finalizing + full 3-action layout). | **Minor** | `MithrilPartialSyncOverlay.stories.tsx:143-184` | storybook |
| 29 | **Recommendation copy gives no concrete "how far behind" context** (only `syncPercentage`; locked: renderer must not compute a threshold). | **Minor** | `MithrilPartialSyncRecommendation.tsx:15-21`; PRD:32-33,79-80 | copy |
| 30 | **Container handoff (diagnostics → overlay) under-tested at the live-injection level.** | **Minor** | task-301-impl-review.md:73; task-400.md:135 | tests |
| 31 | **No store-level dismiss for terminal failed/cancelled** (only `completed` is dismissible). | **Polish** | `MithrilPartialSyncStore.ts:97-100,149-154` | renderer |
| 32 | **Cancelled and failed terminal states share identical recovery-hint copy.** | **Polish** | `MithrilBootstrap.messages.ts:298-315`; `MithrilPartialSyncOverlay.tsx:117-130` | copy |
| 33 | **`MithrilErrorView` carries dead `onWipeRetry`/`onDecline` props alongside explicit `actions[]`.** | **Polish** | `MithrilPartialSyncOverlay.tsx:171-172`; `MithrilErrorView.tsx:97-108` | renderer |
| 34 | **No-ancillary loses fast-restart; ancillary uses IOG-owned (non-Mithril-certified) keys — no user disclosure.** | **Polish** | `research/02:42-44,143-144,163` | copy / product |
| 35 | **Storybook fixture seams (locale-at-render-time, import fixups) easy to regress.** | **Polish** | `research/16:11-25` | storybook |

---

## 4. Issue #10 Intent Gap (the central decision)

Issue #10 asks to *"prompt the user to download, validate, and extract a mithril snapshot **in the
event that they are significantly behind the tip**."* That requires (a) detect "significantly
behind", and (b) *prompt*. **Neither exists today:**
- **No detection in the UI** — `MithrilPartialSyncSection` renders unconditionally
  (`DaedalusDiagnostics.tsx:704-711`); `isSynced` only swaps copy.
- **No prompt** — it is a *passive* recommendation row, visible only if the user manually opens
  Diagnostics. No proactive surface, toast, or distance-to-tip gauge.
- **"Significantly behind" is unconditional copy, not logic** — no threshold constant in `source`;
  the only behind-ness measure (`derivePartialSyncRange`) runs at start-time in the backend.

**The tension the PRD must resolve:** the planning docs **locked** "no auto-trigger, no renderer
threshold/heuristic, entry point = diagnostics only" (PRD non-goals 32/61; task-301:11-12). Issue
#10's "prompt when significantly behind" is in direct tension with that lock. The open question is
whether the *signal* moves to the backend (emit `isSignificantlyBehind`/`behindTip` in the
snapshot, renderer stays dumb) and/or whether a proactive surface is permitted — or whether the
passive recommendation is accepted as the closure of issue #10. **This is the single decision that
most needs the user.**

---

## 5. Locked Decisions & Safety Boundaries to Preserve

1. Entry point is **DaedalusDiagnostics only** (PRD:60). *(Collides with any proactive-prompt
   reading of issue #10 — flag for grilling.)*
2. **Manual trigger only; no auto-trigger, no renderer threshold/heuristic** (PRD 32/61; task-301).
3. **Confirmation precedes start**; confirming is the only path to backend start (task-302).
4. Confirmation copy must explicitly say downloading **verified** Mithril data (task-302:17).
5. **Boundary A vs B/C1/C2 recovery model is backend-authoritative.** Pre-cutover: all three
   actions. At/after cutover: **only** wipe-and-full-sync. Render strictly from
   `allowedRecoveryActions`; never infer from status names (`MithrilPartialSyncService.ts:506-535`).
6. **Cancellation forbidden after cutover** (`MithrilPartialSyncService.ts:249-253`). UI must not
   offer cancel post-cutover (currently does — gap #9).
7. **Latest snapshot only; no snapshot-selection UI** (PRD:34,71).
8. Supported networks: **mainnet, preprod, preview**.
9. **Staged-only restore; no in-place mutation of the live chain** (`research/02:297-301`).
10. **Fixed cutover allowlist** (clean, immutable, ledger, lsm, protocolMagicId); discard live
    `volatile/`. **QA amendment: preserve/merge existing `immutable/`** (`research/02:303-312`;
    `research/18:135`).
11. **Fail closed on recovery-fallback chain layout**; derive range from chain-storage helpers,
    never renderer sync state (`research/02:318-326`).
12. **Durable marker semantics drive startup recovery** and must not be bypassed.
13. **Automatic node restart suppressed during partial sync**; the UX owns driving node start
    (`setup.ts:132-142`).
14. **Operations serialized behind one mutation lock** (`chainStorageCoordinator.ts:349-361`);
    `isActionBlocked` reflects this.
15. **Cancel is a terminal state; overlay stays mounted on cancel** (routes to `cancelled`).
16. **Overlay must follow live backend lifecycle through `completed`** — explicit renderer dismiss
    (task-303:114,221-228). **⚠ AMENDED (PRD D9, 2026-06-19):** the earlier rule "backend does NOT
    reset to idle on success" is **superseded** — after node-start is *verified*, the backend resets to
    `idle` **and** removes the staging dir, so the CTA re-arms and the feature is re-runnable without an
    app restart (fixes BUG1/BUG2). The overlay still follows the lifecycle through `completed` and still
    requires an explicit dismiss; only the post-verification parked-in-`completed` behaviour changes.
17. **Status delivery is push (`onReceive`) with a single initial `request()` pull.** *(Corrected:
    the earlier "no permanent onReceive / polling" constraint was based on a misread and is
    withdrawn. The live concern is the missing unsubscribe on teardown — gap #15.)*
18. **Bootstrap design-language invariants:** no synthetic throughput/remaining-time/overall-% over
    IPC; never route raw mithril-client JSON strings into UI copy; the `preparing → downloading →
    finalizing` waterfall is canonical (`mithril-snapshot-ux.md`).
19. **Kill switch `mithrilPartialSyncEnabled` is the rollout lever**; guard-off blocks new
    diagnostics-launched start + restart-normal while preserving startup-owned wipe/full-sync
    recovery (PRD:340).

> **⚠ Contested → RESOLVED (PRD D3):** kill-switch **default** — nix `launcher-config.nix:409` =
> `false`, but the task-401 checkpoint generated it `true` ("enabled by default" by user override).
> **PRD D3 establishes ground truth:** this branch sets `mithrilPartialSyncEnabled = true` for full
> manual testing; the kill switch is now exposed to the renderer so all UI hides when off (closing
> blocker #1); the **production default and network-scoping are deferred** to the deployment-readiness
> gate (D7).

---

## 6. Backend States the UX Must Represent

**Status enum (12):** `idle` · `stopping-node`\* · `preparing` · `downloading` · `verifying` ·
`converting` · `installing` · `finalizing` · `starting-node` · `completed` · `failed` ·
`cancelled`.  \* `stopping-node` is **renderer-optimistic only — backend never emits it** (gap #19/§3-row, see store `START_PENDING_STATUS`).

**Snapshot update fields** (`mithril-partial-sync.types.ts:39-54`): `status`;
`allowedRecoveryActions`; `transferProgress { filesDownloaded, filesTotal (file COUNTS),
elapsedSeconds, ancillaryBytesDownloaded, ancillaryBytesTotal (BYTES) }`; `progressItems[]`;
`error { code, stage, message, logPath }`; `logPath`.

**Per-boundary allowed actions:** Boundary A (preparing, downloading, verifying, converting) →
retry + restart-normal + wipe-and-full-sync. Boundary B/C1/C2 (installing, finalizing,
starting-node, interrupted-install) → **wipe-and-full-sync only**.

**Backend data the UX does NOT yet surface:** snapshot identity / immutable range / digest /
target tip (logged only — #11/#26); true primary-stream byte total (doesn't exist — only file
counts — #4); kill-switch flag (backend-only — #1); `error.code` as a human category (#6); granular
progress for verify/convert/install/finalize (#5).

---

## 7. Terminology Glossary (for PRD consistency)

- **Mithril partial sync** — diagnostics-launched recovery flow (distinct from startup bootstrap):
  stop node → download/verify a bounded immutable range into staging → restore/convert → cut over
  onto the existing chain DB → restart node. Status type `MithrilPartialSyncStatus`.
- **Mithril bootstrap** — pre-existing first-run, empty-DB fast-sync flow shown during loading.
  Partial sync *reuses* its progress/error/step-indicator components and messages.
- **Cutover / live chain cutover** — the install step where the validated staged snapshot replaces
  the live managed chain DB. After cutover, cancel is rejected and the only safe recovery is
  wipe-and-full-sync.
- **Boundary A vs B/C1/C2** — failure-action boundary model (pre- vs at/after cutover).
- **`allowedRecoveryActions`** — backend-computed array; the *sole* authority for which recovery
  buttons render.
- **retry / restart-normal / wipe-and-full-sync** — recovery actions; `retry` re-runs partial sync
  (no dedicated channel); `restart-normal` starts node on existing DB without wiping (pre-cutover
  only); `wipe-and-full-sync` deletes chain+snapshots and re-enters startup (only post-cutover
  recovery).
- **`stopping-node`** — status the *renderer* optimistically seeds; **not emitted by the backend**.
- **starting-node** — node (re)starting after a successful cutover; overlay hides the action.
- **transferProgress** — `filesDownloaded`/`filesTotal` are integer file **counts**;
  `ancillaryBytes*` are byte counts.
- **Partial sync marker (`mithril-partial-sync.lock`)** — durable JSON recording cutover state:
  `cutover-in-progress` | `installed-awaiting-node-start` | `node-start-verified`; unreadable =
  unsafe.
- **staged-only restore** — restore happens in a scratch staging dir, never in-place.
- **ancillary data** — ledger-state files (`--include-ancillary`) for fast restart; verified with
  **IOG-owned keys (not Mithril-certified)**; still need post-restore ledger→lsm conversion.
- **`mithrilPartialSyncEnabled`** — LauncherConfig boolean kill switch. *(Updated per PRD D3: now
  **exposed to the renderer** via the availability read model; branch = `true`, production default
  deferred. The original "not exposed to the renderer / default contested" state was the blocker-#1
  condition D3 closes.)*
- **`isActionBlocked`** — renderer flag = `isMithrilPartialSyncActive || isMithrilBootstrapActive`;
  disables the CTA while any Mithril work runs. Does **not** reflect the kill switch.
- **tip / behind the tip** — latest chain position; the de-facto backend metric is local vs
  `latestCertifiedImmutableNumber` (`derivePartialSyncRange`), never surfaced.

---

## 8. Open Questions for the Grilling Session

**Trigger / threshold (core):** (1) What concretely defines "significantly behind"
(immutable-file gap / epoch-slot distance / % / ETA), and where is it computed? (2) Active prompt
vs passive recommendation — and where would an active surface live, given "entry = diagnostics
only"? (3) Hide the recommendation when *not* significantly behind? (4) Backend emits an
`isSignificantlyBehind` signal so the renderer stays threshold-free?

**Kill switch / rollout:** (5) Actual default of `mithrilPartialSyncEnabled`? Expose to renderer
so the entry point hides when off? (6) Assume cross-platform/mainnet correctness, or stay
conservative pending packaged QA?

**Progress / telemetry:** (7) Byte-accurate download bar (new backend signal) vs relabel to file
counts — resolve the note-15 vs live-code contradiction. (8) What progress detail for
verify/convert/install/finalize so they don't look stalled?

**Errors / recovery:** (9) Stage/code-specific failure copy — which codes deserve bespoke
messages? (10) Unify interrupted-cutover recovery into one surface (drop native dialog or React
overlay)? (11) Explicit Close/Quit so failed/cancelled is never an unclosable dead-end? (12)
Hide/explain Cancel during installing/finalizing? (13) Latest-drift UX? (14) Cancellation UX +
post-cancel state? (15) Distinguish cancelled vs failed copy?

**Copy / i18n / surface:** (16) Confirmation: bootstrap-style decision affordances (snapshot
details, size, how-far-behind, storage) vs prose-only? (17) Proper modal (focus trap,
primary/secondary hierarchy) vs in-place row swap; ESC = "back to diagnostics"? (18) Unify copy
under one localized namespace + glossary. (19) Delete stale `mithrilPartialSyncButtonHint` + fix
"placeholder" descriptions. (20) `retry` own channel vs alias of start. (21) Keep or remove
`stopping-node`. (22) Ancillary-trust / no-ancillary disclosure copy?

**Lifecycle / test:** (23) Fix `onReceive` teardown leak (gap #15). (24) Fix the broken
confirmation story; add isolated recommendation/confirmation + missing error-stage stories. (25)
Direct live-injection coverage for diagnostics → overlay handoff. (26) Independently re-test the
six QA regressions.

---

## 9. Recommended Scope for the UX-Refinement PRD *(starting position — to be grilled)*

**IN SCOPE**
- *Trigger & prompt (resolves issue #10 — highest priority):* define "significantly behind" as a
  backend-computed metric emitted in the snapshot (renderer stays threshold-free); gate the
  recommendation/CTA on it; decide active-vs-passive and document any exception to "entry =
  diagnostics only."
- *Correctness/safety blockers & majors:* hide/disable entry when kill switch off, or expose the
  flag (#1); fix Cancel-after-cutover (#9); add error-overlay close affordance (#8); unify
  interrupted-cutover recovery (#7); resolve file-count-vs-bytes + note-15 contradiction (#4);
  per-phase progress for install/finalize (#5); stage/code-specific error copy (#6); localize/
  humanize start-rejection errors (#12); fix the `onReceive` teardown leak (#15).
- *Polish & hygiene:* delete orphaned hint key + fix stale descriptions (#20); i18n the row label
  (#21); confirmation button hierarchy (#14); cancelled-vs-failed copy (#32); fix broken story +
  add isolated/missing stories (#13,#28); re-baseline PRD references to the 3-component structure
  (#10).

**OUT OF SCOPE / DEFER**
- Auto-*starting* partial sync (locked non-goal); snapshot-selection UI (latest-only lock);
  cross-platform/mainnet/preprod/custom-storage/packaged QA (large deferred matrix — keep guarded
  or commit to a QA gate, don't assume correctness); re-architecting cutover/merge correctness
  (#16,#17,#19 — backend track, not UX); bootstrap-only affordances (storage picker, snapshot
  selector — locked non-goal).

> **Single most important grilling target:** reconcile issue #10's "prompt when significantly
> behind" with the locked "no auto-trigger / no renderer threshold / diagnostics-only entry"
> constraints. Everything else is refinement; this is a genuine product-direction decision.

---

### Appendix — verification corrections folded in
1. **Push, not poll** (load-bearing): status is `onReceive`-driven; the teardown-leak is the real
   bug (gap #15); withdrew former constraint-17 and the out-of-scope "push-driven status" item.
2. Status enum is **12** values, not 13.
3. Gap #1 raw-string **render** is `MithrilPartialSyncConfirmation.tsx:74` (store sets it at
   `MithrilPartialSyncSection.tsx:90-95`); blocker fully confirmed.
4. Gap #6: generic copy is **structurally guaranteed** — the overlay always passes explicit
   title+hint, short-circuiting `ERROR_COPY_BY_STAGE`; backend's `MithrilPartialSyncErrorStage` is
   correctly typed but never mapped in the renderer.

---

## 10. Additional gaps from issue #10 comment 4623095092 (verification pass 2, 2026-06-19)

The original 35-gap survey predates GitHub issue #10 comment 4623095092 ("UI/UX items to address …"
+ "Potential bugs/issues"). A second, code-grounded audit mapped all 13 comment items against the live
branch and the PRD. Eight comment-derived defects not covered by gaps #1–#35 are enumerated here
(#36–#43), plus one adjacent bug found during the BUG4 triage (#44 — the never-written
`node-start-verified` marker); all are dispositioned in the PRD's *Issue #10 Comment Coverage Map* and/or
the decision log. (Comment items UX2/UX3 were already covered by existing gaps; UX1/UX4/UX5/UX6/UX8 map
to existing gaps + new ones; BUG4 was triaged out — see below.)

| # | Title | Severity | Evidence | PRD decision |
|---|-------|----------|----------|--------------|
| 36 | **Active top-level step circle never rotates** (static dot, both flows; UX7). | Minor | `MithrilStepIndicator.tsx:386-388` (`.activeCircle`); no animation `.scss:79-84`; a unit test *locks* it — `MithrilStepIndicator.spec.tsx:59-60` asserts `.iconSpinner` is null. The unused `.iconSpinner`/`loading-spin` (reduced-motion handled `.scss:299-311`) is the ready fix. | **D4** — render the spinner for the active step; update the spec. |
| 37 | **Partial-sync elapsed freezes between status events** — backend-snapshot-driven, no renderer clock (UX6/UX5a). | Major | Elapsed stamped only inside `_updateStatus` (`MithrilPartialSyncService.ts:711-722`); the bootstrap-proven ticking path (`MithrilProgressView.tsx:97-109`, gated on `bootstrapStartedAt`) is never wired; overlay passes the frozen `elapsedSeconds` prop (`MithrilPartialSyncOverlay.tsx:93`). | **D4** — renderer-side timer anchored to `startedAt`/`bootstrapStartedAt`. |
| 38 | **Node-stop is invisible in the dialogue** — only shown behind the opaque backdrop; `0:00` + greyed steps for minutes (UX5a/UX5b). | Major | No `isStoppingNode` block mirroring `isStartingNode` (`MithrilProgressView.tsx:142-163`); `stopping-node` is renderer-optimistic only (gap #19). | **D8 #19** (expanded) — populated in-dialogue node-stop frame + reassurance + ticking elapsed. |
| 39 | **Cancel during `stopping-node` is a silent no-op** (distinct from gap #9). | Major | `service.cancel` early-returns while `_activeWorkDir`/`_currentProcess` are null (`MithrilPartialSyncService.ts:241-247`); store never resyncs, so the button looks dead. Gap #9 is the *post-cutover throw* — this is the *pre-cutover* dead click. | **D5(f)** — disable Cancel during stop + always resync after cancel. |
| 40 | **CTA stuck "Unavailable while Mithril work is already active" after a *successful* sync** (clears only on restart). | Major | `isActive = (status !== 'idle')` (`mithril-partial-sync.types.ts:92-94`); `completed` counts as active → `isActionBlocked` never clears. `finalizeInstalledNodeStart` emits `completed` but never resets to idle (`mithrilPartialSyncNodeStartup.ts:147-172`); `dismissCompletedOverlay` only sets a flag (`MithrilPartialSyncStore.ts:149-154`). **Locked decision #16 mandates the cause.** | **D9** — backend reset-to-idle on verified success (amends #16) + renderer `isWorking` guard. |
| 41 | **Staging dir leftover after a *successful* sync** (root-shares the missing success-finalization with #40). | Major | `_cleanupPartialSyncArtifacts()`/`fs.remove(stagingRoot)` runs only on cancel (`:268`), restart-normal (`:306`), wipe (`:317`) — never on success; install removes only the staged DB dir; finalize clears only the marker. Gap #16/D7 is scoped to *cancellation/failure* cleanup, not success. | **D9** — `fs.remove(stagingRoot)` folded into the verified-success path. |
| 42 | **Staging hardcoded to the top-level state dir — contradicts the backend PRD's own line 250.** | Major | `path.join(stateDirectoryPath, 'mithril-partial-sync')` (`MithrilPartialSyncService.ts:567-569`) overrides the resolved `_activeWorkDir` (`:153`); with a relocated/symlinked chain, scratch fills the wrong volume and cutover is a cross-device copy (~2× transient dest). Backend PRD:250 says stage "under the resolved Mithril work directory". | **D7** — colocate on the resolved chain volume + disk preflight; no user picker. |
| 43 | **Transient overlay error in the nix dev environment (Linux/Preprod)** — undispositioned (UX9). | Polish/triage | Reporter notes "could have been a bad state or dev-env issue"; a stale marker or leftover staging dir (#41) could plausibly produce it on next launch. | **D5b** triage note; plausibly mitigated by **D9** cleanup. |
| 44 | **`node-start-verified` marker state is defined and read but never written** — dead code; Boundary C2 unimplemented. | Minor | Marker is a 3-state machine (`mithrilPartialSyncMarker.ts:6-9`) but writes are only `cutover-in-progress` (`MithrilPartialSyncService.ts:205`) and `installed-awaiting-node-start` (`:209`); `finalizeInstalledNodeStart` clears the marker (`mithrilPartialSyncNodeStartup.ts:165`) instead of stamping verified, so the `:60-63` C2 branch is unreachable. Backend PRD Boundary C2 (`mithril-partial-sync-prd.md:187-194`) is therefore not realised. Not a safety hole — C1 re-drive on an already-verified DB is safe/idempotent. | **D9** (Option A) — write `node-start-verified` after node proven RUNNING; clear only at dismiss/normal-return. |

**Triaged OUT — BUG4 (Windows: slow node stop after a successful sync → "verifying on-disk blockchain
state" on reopen).** The string is **cardano-node's own unclean-shutdown recovery**
(`SyncingProgress.messages.ts`), surfaced by the generic loading screen and **independent of partial
sync**. Daedalus stop is graceful-then-kill bounded by `shutdownTimeout` (`CardanoNode.ts:491-527`;
`setup.ts:89-90`); a slow Windows stop can hit `kill()` before the node flushes its clean marker. The
reporter's own framing ("likely not partial-sync related … Windows mainnet, not Linux preprod") matches
the code. **Per user decision 2026-06-19 this is deferred entirely** — file as a standalone issue (is
`shutdownTimeout` too short for Windows mainnet?), not tracked in this PRD. *(The `node-start-verified`
marker bug surfaced during this BUG4 triage is now tracked separately as gap #44 and scoped into PRD D9,
Option A — see above.)*
