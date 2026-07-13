# task-ux-702a — Grill Decision Record (manual-assessment remediation)

> Authoritative decision log produced by a `grill-with-docs` session on the
> `task-ux-702-manual-assessment.md` findings, **before** planning/implementation.
> Source findings: `task-ux-702-manual-assessment.md` (12 issues). Process rules:
> `prompt-ux-refinement.md`. Domain locks: `mithril-partial-sync-prd.md`,
> `mithril-partial-sync-ux-refinement-prd.md`. All citations verified against live code.

## Scope decision (D-702a-0)

`task-ux-702a` is a **net-new phase-7 remediation task** (its own four-file set:
`task-ux-702a.md`, `-plan-review.md`, `-impl-review.md`, `-research.md`). It remediates the
12 manual-assessment issues. **`task-ux-702` (the deployment QA gate) stays `pending` and is
gated behind 702a.** The tasks JSON gains `task-ux-702a` with `task-ux-702` depending on it
(`task-ux-702a` blocks `task-ux-702`). Naming per `prompt-ux-refinement.md:57-63`.

The 12 issues are grouped into **8 implementation categories (CAT-A…CAT-H)**; one code subagent
per category. `.scss`/theme work is isolated in **CAT-D** to minimize edit collisions.

---

## Locked decisions from the grill

### D-702a-1 — ISSUE-8 completion flow: auto-fire finalize on timeout (AMENDS locked D9)
Remove the **"Continue to Daedalus"** button. The completion overlay follows the bootstrap-style
flow: `Finalizing → Completed → timeout (loading animation) → Cardano node startup → Wallet
Summary`. **The D9 finalize IPC (reset-to-idle + remove staging dir + clear marker) fires
AUTOMATICALLY when the completed-timeout elapses** — not on a manual click. All cleanup semantics
are preserved (mithril-sync folder still deleted on completion, per assessment `:66`).

- **This amends locked decision #16-as-amended-by-D9** (`prompt-ux-refinement.md:107-111`:
  success overlay clears only on *explicit user dismiss*; dismiss triggers backend reset). The
  amendment swaps the explicit click for an auto-timeout trigger while keeping the finalize call.
  Record as an ADR in `task-ux-702a.md`.
- Touches: `MithrilPartialSyncOverlay.tsx` (`completed` handling, `onDismissCompleted` →
  auto-fire on timeout), `MithrilBootstrap.messages.ts:328` ("Continue to Daedalus" removal),
  `MithrilPartialSyncOverlay.spec.tsx:107` (asserts the button — update), store finalize wiring.
- Category: **CAT-G**.

### D-702a-2 — ISSUE-6 cancelled-dialogue recovery: trim wipe (pre-cutover only)
On the **Cancelled (pre-cutover) dialogue**, show only **"Retry Mithril Sync (fast)"** (`retry`)
and **"Restart Node Sync (slow)"** (`restart-normal`). **Remove the "wipe all chain data" option
there.** Rationale (PRD `mithril-partial-sync-prd.md:179-190, 256-261`): cancellation is only
possible **pre-cutover (Boundary A)**, where the existing chain DB is fully intact, so
`restart-normal` ("continue on the existing DB") is the viable slow path and wipe is unnecessary.

- **Backend trims the emitted list** (`MithrilPartialSyncService.ts:302-304/316-318` currently
  emits `['retry','restart-normal','wipe-and-full-sync']` for the cancelled/failed-pre-cutover
  state → change to `['retry','restart-normal']`). Renderer keeps rendering **strictly** from
  `allowedRecoveryActions` (locked #5 intact). Update
  `MithrilPartialSyncService.spec.ts:876-880`.
- **Post-cutover startup-failure dialog is OUT OF SCOPE and UNCHANGED**
  (`mithrilPartialSyncNodeStartup.ts:119/135` keeps `['wipe-and-full-sync']`): there the old DB is
  already emptied, so wipe is the only safe recovery; removing it would create a Quit-only
  dead-end (locked #6 / D5b). Wiping there remains a manual `--wipe-chain` operation only as a
  last resort, which is acceptable.
- Cancelled-dialogue **subtext → normal body text**; **action button on the RIGHT** (consistent
  with other Daedalus dialogs).
- Category: **CAT-E** (backend trim + renderer copy/layout).

### D-702a-3 — ISSUE-3 recommendation → tooltip + reword
Move the Diagnostics Mithril recommendation copy into a **hover tooltip on the Mithril Sync
button**, styled like the **state-directory tooltip** (`DaedalusDiagnostics.tsx` `PopOver` pattern
at `:9, 599, 615-634, 658-669`). Reword to the **verbatim** string below. **Remove** the
`mithrilPartialSyncButtonHintReady` hint ("Review what will happen before Daedalus starts Mithril
Sync.").

- Verbatim tooltip copy:
  > If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified
  > chain data to help speed up the sync.
- Category: **CAT-B**.

### D-702a-4 — ISSUE-4 confirmation body: simplify; KEEP epochs figure + behindUnknown fallback
Body becomes two parts:
1. **Subject text** (one paragraph): first sentence is the behind-ness line, then the restore
   sentence. **Keep the epochs figure** (locked D13 epochs-only behind-ness) **and keep the
   `behindUnknown` fallback** for when the epoch value is missing/uncomputable.
   - When epochs known (`behind`):
     > Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to
     > help your node sync faster.
   - When epochs unknown (`behindUnknown`, keep existing fallback, then the restore sentence):
     > Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain
     > data to help your node sync faster.
2. **Subtext** (spaced, subtext style) — the **canonical shutdown/restore/restart sentence**
   (see D-702a-6):
   > For this process to begin your Cardano node will need to be shutdown. Mithril will then be
   > used to sync the verified chain data. On Mithril Sync completion, the node will be restarted
   > to sync the remaining blocks.
- **Remove all other confirmation body copy** (`Intro`, `StepStop`, `StepDownload`, `StepRestart`,
  `Success`, `Recovery` body lines — `en-US.json:163-168`). Keep the title "Before Mithril Sync
  begins" and the action/cancel buttons.
- Confirm text style matches the initial Mithril (bootstrap) dialogs.
- Note: this deliberately re-touches `mithrilPartialSyncConfirmationBehind` (previously locked by
  304/601, `tasks.json:617`). Record the override so it does not read as a 304/601 regression.
- Category: **CAT-C** (copy/logic), styling via **CAT-D**.

### D-702a-5 — ISSUE-7 proactive prompt: known-behind gating + persist into Wallet Summary
- **Show only when it is KNOWN the user is behind the blockchain tip** — i.e. behind-ness has been
  computed (network tip available). Do **not** show during the early connecting/"verifying
  blockchain" checks when behind-ness is not yet known. (Couples to CAT-A / ISSUE-2: the network
  tip / "Last network block" must be available for behind-ness to be known.)
- **Persist the prompt across the loading → Wallet Summary transition.** Today it is mounted only
  on the loading screen (`SyncingConnecting.tsx`) and **disappears when the loading screen moves
  on** — that is the bug. Hoist ownership so the prompt survives into the Wallet Summary page and
  **stays until the user clicks "Standard Sync (slow)" or "Mithril Sync (fast)."**
- **"Mithril Sync (fast)" is the default/primary highlighted button** (like other primary actions).
- **Capitalize "Mithril Sync"** in the subtitle.
- Prompt **confirmation copy** = the canonical sentence (D-702a-6).
- Category: **CAT-F** (depends on CAT-A's "behind-ness known" signal).

### D-702a-6 — Canonical shutdown/restore/restart sentence (one shared key)
The confirmation subtext (D-702a-4) and the prompt confirmation (D-702a-5) use the **byte-identical**
sentence (assessment `:29` == `:59`). **Consolidate into ONE shared i18n key** (EN + JA) reused by
both surfaces:
> For this process to begin your Cardano node will need to be shutdown. Mithril will then be used
> to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync
> the remaining blocks.

### D-702a-7 — EN+JA handling + final copy-overview deliverable
- Each category subagent updates **both `en-US.json` and `ja-JP.json`** (plus
  `defaultMessages.json` / `translations/messages.json`) for every string it changes, following
  the existing ja-JP Mithril glossary from task-ux-601. New JA is **best-effort, provisional**.
- **After all copy lands**, a dedicated **copy-overview subagent** gathers **all** new/changed EN
  and JA strings for the mithril-partial-sync feature set and writes a **Markdown copy-overview**
  (old → new, per surface, EN + JA) so a **manual Japanese translator can review**. Output path:
  `task-plans-ux-refinement/phase-7/task-ux-702a-copy-overview.md`.

---

## Category map (one code subagent each)

| Cat | Issues | Scope | Primary files |
|---|---|---|---|
| **CAT-A** | ISSUE-1, ISSUE-2 | First-load Mithril availability + "Last network block"/behind-ness known timing; produce reliable "behind-ness known" signal for CAT-F | `MithrilPartialSyncStore.ts`, `NetworkStatusStore.ts`, `DaedalusDiagnostics.tsx`, availability IPC channels |
| **CAT-B** | ISSUE-3 | Recommendation → hover tooltip + reword; remove ready-hint | `MithrilPartialSyncRecommendation.tsx`, `MithrilPartialSyncSection.tsx`, `DaedalusDiagnostics.tsx` (PopOver pattern), i18n |
| **CAT-C** | ISSUE-4 | Confirmation body simplify; keep epochs + behindUnknown; canonical subtext | `MithrilPartialSyncConfirmation.tsx`, i18n |
| **CAT-D** | ISSUE-5, ISSUE-6(theme), ISSUE-9 | Theme/contrast audit across all 10 themes; chain-storage/bootstrap as guide; **owns all `.scss` + theme tokens** | `DaedalusDiagnostics.scss`, `SyncingConnectingMithrilPrompt.scss`, `Mithril*View.scss`, `themes/daedalus/*.ts` (10), `themes/utils/createTheme.ts` |
| **CAT-E** | ISSUE-6 | Cancelled recovery trim (backend) + button copy + subtext→body + right-align | `MithrilPartialSyncService.ts`, `MithrilErrorView.tsx`, `MithrilPartialSyncOverlay.tsx`, specs, i18n |
| **CAT-F** | ISSUE-7 | Prompt known-behind gating + persist into Wallet Summary + default button + capitalize | `SyncingConnectingMithrilPrompt.tsx`, `SyncingConnecting.tsx`, `SyncingConnectingPage.tsx`, wallet-summary surface, i18n |
| **CAT-G** | ISSUE-8 | Completion auto-transition; remove "Continue to Daedalus"; auto-fire D9 finalize on timeout | `MithrilPartialSyncOverlay.tsx`, `MithrilBootstrap.messages.ts`, store, specs, i18n |
| **CAT-H** | ISSUE-10, ISSUE-11, ISSUE-12 | Storybook: render real UI (not text); add partial-progress story; rename "Status"→"Diagnostic"; move dialogue views under Mithril; icon investigation | `storybook/stories/nodes/status/Diagnostics.stories.tsx`, `storybook/stories/loading/mithril/*`, `_support/mithril*` |

**Known couplings / overlaps (sequence/coordinate to avoid collisions):**
- **i18n (`en-US.json`/`ja-JP.json`)** touched by CAT-B/C/E/F/G → serialize i18n edits.
- `DaedalusDiagnostics.tsx`: CAT-A + CAT-B. `MithrilPartialSyncSection.tsx`: CAT-B + CAT-C.
- `MithrilPartialSyncOverlay.tsx`: CAT-E + CAT-G. `DaedalusDiagnostics.scss`: CAT-D (owner) ref'd by B/C.
- CAT-F depends on CAT-A's "behind-ness known" signal. CAT-D runs **after** the copy/logic
  categories (owns styling once structure is settled). CAT-H storybook theme half coordinates w/ CAT-D.
- **Implication:** implement categories **sequentially on the shared working tree** in a
  collision-safe order (CAT-A → CAT-C → CAT-B → CAT-E → CAT-F → CAT-G → CAT-D → CAT-H), each
  followed by a code-review pass.

---

## Environment caveats (brief implementers)
- **Renderer+scss verify env:** Node v24 needs `typed-scss-modules` to regen the `.scss.d.ts` for
  `tsc`, plus a gitignored `identity-obj-proxy` jest sidecar. Apply before treating tsc/jest fails
  as regressions (especially CAT-D scss work).
- **Workflow orchestration:** inline grounding into agent prompts (args may arrive unparsed); keep
  StructuredOutput schemas lenient (avoid strict `required`); make planner/scribe idempotent.

## Deferred / investigate (no silent caps)
- **ISSUE-1/2 root cause** (first-load availability) is not localized to one line — CAT-A
  investigates the store/IPC lifecycle; **fix if it is a renderer store-refresh timing issue**,
  otherwise document findings and flag if it needs the backend-correctness track.
- **Button right-alignment** (ISSUE-6 side-q) applies to the Mithril dialogs; if the footer/action
  layout is shared with the bootstrap dialogs, align those too **visual-only**, gated by code
  review against locked #11 (do not regress the empty-chain bootstrap flow).
- **Storybook icon inconsistency** (ISSUE-12 side-q) is an investigation; report, fix if trivial.

## Severity (assessment states none; orchestrator-assigned)
Highest: **ISSUE-1** (feature invisible on first load — functional) and **ISSUE-8** (amends a
locked decision). Then ISSUE-6/7 (recovery + prompt UX), ISSUE-3/4 (copy), ISSUE-5/9 (theme),
ISSUE-10/11/12 (storybook).
