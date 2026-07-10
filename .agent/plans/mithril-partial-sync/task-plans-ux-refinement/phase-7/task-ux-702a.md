# task-ux-702a — Mithril partial-sync UX remediation (manual-assessment fixes)

> Canonical task plan + single source of truth for the 12 manual-assessment findings.
> Authoritative inputs: `task-ux-702a-decisions.md` (locked decisions D-702a-0..7),
> `task-ux-702-manual-assessment.md` (12 issues), `prompt-ux-refinement.md` (process rules,
> small-model bar :75-89, locked safety boundaries :93-114). All citations verified against live code.
> Review logs: `task-ux-702a-plan-review.md` (Planner/Critiquer), `task-ux-702a-impl-review.md`
> (Implementation/Code-Review), `task-ux-702a-research.md` (durable findings).

## Task id + title
- **Id:** task-ux-702a
- **Title:** Mithril partial-sync UX remediation (manual-assessment fixes)

## Interaction mode (AUTHORITATIVE)
`interactive_validation` / `interactive_decision`. The copy approvals (D-702a-3 verbatim tooltip,
D-702a-4 confirmation body, D-702a-6 canonical shutdown/restore/restart sentence, button labels) were
**already gathered in the grill decision record** and are locked there. The remaining human-in-the-loop
surface is: (1) the per-category code-review pass after each category lands, (2) the manual Japanese
translator review of the final copy-overview deliverable (D-702a-7), and (3) operator visual validation
across the 10 themes for CAT-D / CAT-H. **Never relabel this autonomous** — phase-7 tasks require
operator-run validation and the locked copy was decided interactively. Implementation of each category's
mechanical steps is small-model-executable from this doc; the *decisions* behind them are not re-opened.

## Why now
`task-ux-702` (the deployment QA gate) is held `pending` and is **gated behind 702a** (D-702a-0). The
operator's manual assessment of the shipped Mithril partial-sync UX surfaced 12 issues spanning a
functional defect (feature invisible on first load), a locked-decision amendment (completion flow),
recovery/prompt UX, copy, theme contrast, and Storybook fidelity. 702a remediates all 12 before the
deployment gate can re-run. The tasks JSON gains `task-ux-702a`; `task-ux-702` depends on it.

## Scope
Remediate the 12 manual-assessment issues, grouped into **8 implementation categories (CAT-A…CAT-H)**,
one code subagent per category, implemented sequentially on the shared working tree in the collision-safe
order below, each followed by a code-review pass. `.scss`/theme work is isolated in **CAT-D**.

## Non-goals
- No change to the Boundary A/B/C recovery model (backend-authoritative; render strictly from
  `allowedRecoveryActions`).
- No new start path; confirmation still precedes every backend start.
- No renderer-computed behind-ness threshold; the backend `isSignificantlyBehind` stays the offer signal.
- No change to the post-cutover startup-failure dialog (`mithrilPartialSyncNodeStartup.ts`), which keeps
  `['wipe-and-full-sync']` (D-702a-2).
- No empty-chain Mithril bootstrap regression (locked #11); shared progress/error components are edited
  only additively and visual-only, verified against bootstrap stories.
- CAT-H is storybook-only (no app code, no `.scss`, no i18n).

## Dependencies
- Blocks `task-ux-702` (deployment gate).
- Internal sequencing dependency: **CAT-F depends on CAT-A's "behind-ness known" signal**; CAT-F and
  CAT-C share the canonical D-702a-6 i18n key (CAT-C creates it, CAT-F/CAT-C consume it); CAT-D consumes
  the final class names from CAT-B/CAT-C/CAT-E/CAT-F.

## Research / docs / workflows consulted
`task-ux-702a-decisions.md`, `task-ux-702-manual-assessment.md`, `prompt-ux-refinement.md`
(:36-162), the per-category investigation plans (8), and live code in `source/renderer`, `source/main`,
`source/renderer/app/i18n`, `source/renderer/app/themes`, and `storybook/`. Memory: Cardano-native
vocabulary (Mithril Sync / standard sync, epochs-only behind-ness, never "partial sync" in user copy);
renderer+scss verify env (Node v24 → regen `.scss.d.ts` via typed-scss-modules + gitignored
identity-obj-proxy jest sidecar before treating tsc/jest fails as regressions).

---

## Locked Decisions / ADRs

These restate the locked decisions from `task-ux-702a-decisions.md`. **D-702a-1 and D-702a-2 each amend a
locked invariant and are recorded below as explicit ADRs.**

### D-702a-0 — Scope
`task-ux-702a` is a **net-new phase-7 remediation task** (its own four-file set). It remediates the 12
manual-assessment issues. `task-ux-702` stays `pending` and is gated behind 702a. The 12 issues are
grouped into 8 implementation categories (CAT-A…CAT-H), one code subagent per category; all `.scss`/theme
work is isolated in CAT-D to minimize edit collisions.

### ADR D-702a-1 — Completion flow: auto-fire D9 finalize on timeout (AMENDS locked #9 / decision #16-as-amended-by-D9)
**Context.** Locked decision #16-as-amended-by-D9 (`prompt-ux-refinement.md:108-111`) requires the success
overlay to clear **only on an explicit user dismiss**, and that dismiss fires the backend finalize
(reset-to-idle + remove staging dir + clear marker). The manual assessment (ISSUE-8, `:61-66`) requires
the completion flow to match the bootstrap flow: `Finalizing → Completed → timeout (loading animation) →
Cardano node startup → Wallet Summary`, with **no "Continue to Daedalus" button**.

**Decision (amendment).** Remove the "Continue to Daedalus" button. **The D9 finalize IPC fires
AUTOMATICALLY when the completed-timeout elapses**, not on a manual click. The amendment swaps the explicit
click for an auto-timeout trigger **while keeping the finalize call and all cleanup semantics intact**
(mithril-sync staging folder still deleted, marker still cleared, reset-to-idle preserved — assessment
`:66`). The PRD success-flow line (`mithril-partial-sync-ux-refinement-prd.md:782` "Continue to Daedalus
dismiss") is **superseded** by this ADR.

**Consequences.** Implemented in CAT-G via a component `useEffect` `setTimeout` that calls the unchanged
`store.dismissCompletedOverlay` (→ `mithrilPartialSyncFinalizeChannel`). No backend, IPC-contract, or
store-logic change to the finalize path. Reviewers MUST read the completed-flow change against this ADR,
not as a #9/#16 violation, and the previously-601-touched `completed.subtitle` reword is a deliberate
override, not a 601 regression.

### ADR D-702a-2 — Cancelled-state recovery trim (pre-cutover only) (AMENDS the cancelled `allowedRecoveryActions` set; honors locked #2/#5/#6)
**Context.** On the **Cancelled (pre-cutover) dialogue** the backend currently emits
`['retry','restart-normal','wipe-and-full-sync']` (`MithrilPartialSyncService.ts:302-304/316-318`), so a
third "wipe all chain data" button renders. Cancellation is only possible pre-cutover (Boundary A,
`mithril-partial-sync-prd.md:179-190, 256-261`) where the existing chain DB is fully intact, so
`restart-normal` ("continue on the existing DB") is the viable slow path and wipe is unnecessary.

**Decision (amendment).** The backend **stops emitting `wipe-and-full-sync` for the cancel-originated
paths** (`cancelled` and cancel-cleanup-`failed`): trim those two arrays to `['retry','restart-normal']`.
The renderer keeps rendering **strictly from `allowedRecoveryActions`** (locked #5 intact); no renderer
logic changes. **Post-cutover is UNCHANGED:** `mithrilPartialSyncNodeStartup.ts:119/135` keeps
`['wipe-and-full-sync']` (there the old DB is already emptied, so wipe is the only safe recovery; removing
it would create a Quit-only dead-end — locked #6 / D5b). The active-sync **failure** path
(`_deriveAllowedRecoveryActions:560`, an inline 3-action return) is **NOT** cited by D-702a-2 and stays
3-action — so a pre-cutover sync *failure* dialog shows 3 actions while a *cancelled* dialogue shows 2.
This asymmetry is intentional per the literal citation; escalate only if code review wants `:560` trimmed.

**Consequences.** Implemented in CAT-E (backend array trim + spec). Locked #6 (cancellation forbidden
post-cutover) untouched: `cancel()` still hard-rejects for installing/finalizing.

### D-702a-3 — Recommendation → tooltip + reword (CAT-B)
Move the Diagnostics recommendation copy into a **hover tooltip on the Mithril Sync button**, styled like
the state-directory `PopOver` pattern. **Remove** the `mithrilPartialSyncButtonHintReady` hint. Verbatim
tooltip copy: *"If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified
chain data to help speed up the sync."*

### D-702a-4 — Confirmation body simplify; keep epochs + behindUnknown (CAT-C, styling CAT-D)
Body becomes two parts: (1) **subject paragraph** = behind-ness line + restore sentence (keep the
`{epochs}` figure per D13 and keep the `behindUnknown` fallback); (2) **subtext** = the canonical D-702a-6
sentence. Remove `Intro`, `StepStop`, `StepDownload`, `StepRestart`, `Success`, `Recovery` body copy. Keep
the title "Before Mithril Sync begins" and the action/cancel buttons. Deliberately re-touches
`mithrilPartialSyncConfirmationBehind` (previously locked by 304/601, `tasks.json:617`) — record the
override so it does not read as a 304/601 regression.

### D-702a-5 — Proactive prompt: known-behind gating + persist into Wallet Summary (CAT-F, depends on CAT-A)
Show the prompt **only when behind-ness is KNOWN** (network tip available), never during the early
connecting/"verifying blockchain" checks. **Persist the prompt across the loading → Wallet Summary
transition** (hoist its mount off the loading screen). "Mithril Sync (fast)" is the default/primary
highlighted button. Capitalize "Mithril Sync" in the subtitle. Prompt confirmation copy = the canonical
D-702a-6 sentence.

### D-702a-6 — Canonical shutdown/restore/restart sentence (ONE shared i18n key)
The confirmation subtext (D-702a-4) and the prompt confirmation (D-702a-5) use the **byte-identical**
sentence, **consolidated into ONE shared i18n key (EN + JA)** reused by both surfaces: *"For this process
to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain
data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks."*

### D-702a-7 — EN + JA handling + final copy-overview deliverable
Each category updates **both `en-US.json` and `ja-JP.json`** (plus `defaultMessages.json` /
`translations/messages.json`) for every changed string, following the task-ux-601 ja-JP Mithril glossary.
New JA is **best-effort, provisional**. After all copy lands, a dedicated **copy-overview subagent** writes
`task-ux-702a-copy-overview.md` (old → new, per surface, EN + JA) for a manual Japanese translator.

### Locked safety boundaries carried into every category (from `prompt-ux-refinement.md:93-114`)
Staged-only restore (#1); recovery rendered strictly from `allowedRecoveryActions` (#2); confirmation
precedes start (#3); no auto-trigger / no renderer-computed threshold (#4); latest snapshot only, no
selection/storage UI (#5-as-numbered there); cancellation forbidden post-cutover (#6); supported networks
mainnet/preprod/preview (#7); no synthetic throughput/%/remaining-time over IPC (#8); success overlay /
finalize per #16+D9 **as amended by D-702a-1** (#9); kill switch `mithrilPartialSyncEnabled` hides all
partial-sync UI when off (#10); do not regress the empty-chain Mithril bootstrap flow (#11).
**Locked vocabulary:** user copy says "Mithril Sync" / "standard sync" (never "partial sync"); behind-ness
is **epochs-only** (never %/immutable files).

---

## Sequencing

Implement **sequentially on the shared working tree** in this collision-safe order, each category
**followed by a code-review pass** before the next begins:

```
CAT-A → CAT-C → CAT-B → CAT-E → CAT-F → CAT-G → CAT-D → CAT-H
```

- **CAT-A first**: produces the reactive "behind-ness known" signal (`NetworkStatusStore.isBehindnessKnown`
  + `utils/mithrilBehindness.ts`) and fixes the availability refresh timing. **CAT-F depends on CAT-A's
  behind-ness-known signal** for its prompt gate.
- **CAT-C before CAT-B/CAT-F**: CAT-C creates the shared D-702a-6 i18n key
  (`MithrilSyncProcessSummary.messages.ts`, id `daedalus.diagnostics.dialog.mithrilSyncProcessSummary`);
  CAT-F consumes it. (Note: CAT-F's plan references the same shared key under the working id
  `mithrilPartialSyncProcessNote`; the implementer MUST use whichever single id CAT-C actually creates and
  import the one exported module — do not declare the id twice or a duplicate-id extraction conflict
  results. Pin the final id during CAT-C and carry it into CAT-F.)
- **i18n serialization**: CAT-B, CAT-C, CAT-E, CAT-F, CAT-G all edit `en-US.json`/`ja-JP.json` — apply
  their i18n edits in sequence to avoid merge collisions. CAT-A and CAT-H touch no i18n.
- **CAT-D runs after all copy/logic/structure categories** (CAT-A/C/B/E/F/G) because it **owns all `.scss`
  + theme tokens** and adapts to the final class names those categories settle (subject paragraph =
  `mithrilPartialSyncConfirmationBehind`, subtext = `mithrilPartialSyncConfirmationRecovery` restyled in
  place, prompt button = `primary`). CAT-E touches one layout line + one body rule in `MithrilErrorView.scss`
  (no color tokens), which CAT-D must not revert.
- **CAT-H last** (storybook-only): consumes the final component props/contrast from earlier categories;
  STEP 0 re-verifies post-CAT-B/CAT-F signatures before writing the moved stories.
- Shared-file overlaps to honor in-order: `MithrilPartialSyncOverlay.tsx` (CAT-E error branch, then CAT-G
  completed branch); `DaedalusDiagnostics.tsx` (CAT-A signal contract, CAT-B tooltip); `MithrilErrorView.*`
  (CAT-E structure/layout, then CAT-D color audit).

## Copy overview deliverable

**After all copy lands** (CAT-B/C/E/F/G complete), a dedicated **copy-overview subagent** gathers **all**
new/changed EN and JA strings for the mithril-partial-sync feature set and writes
`task-plans-ux-refinement/phase-7/task-ux-702a-copy-overview.md` as a Markdown table (old → new, per
surface, EN + JA) so a **manual Japanese translator** can review the provisional JA (D-702a-7). All JA in
this plan is best-effort/provisional and feeds that deliverable; it is not a blocker for the code categories.

---

## CAT-A — Diagnostics availability & behind-ness-known timing (ISSUE-1 + ISSUE-2)

**Issues:** ISSUE-1 (Mithril option hidden on first load, appears only after reload), ISSUE-2 ("Last
network block" not known early; behind-ness signal for CAT-F). Renderer-only investigation-and-fix; **no
backend edits**.

**Root cause (verified).** ISSUE-1 visibility is gated by a static config flag, so it is NOT a config
difference — it is a renderer freshness bug: `MithrilPartialSyncStore` fetches availability exactly once at
`setup()` then re-fetches on the 30s interval **only `if (this.isWorking)`**, which is false during normal
Cardano-node sync, so the one-shot read never self-corrects without a full reload. The backend
`getPartialSyncAvailability` also delays the static `isEnabled` behind a slow behind-ness probe on cold
start (flagged for the backend track, NOT edited here). ISSUE-2: `networkTip` ("Last network block") is
only populated after the node connects; the epochs figure is `undefined` until both tips arrive. CAT-A's
deliverable is a reactive **availability-of-tip** signal (NOT a threshold).

**Exact files:**
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/utils/mithrilBehindness.ts` (new)
- `source/renderer/app/utils/mithrilBehindness.spec.ts` (new)
- `source/renderer/app/stores/NetworkStatusStore.ts`

**Ordered steps:**
1. In `MithrilPartialSyncStore.ts`, after `_isTornDown = false;` (line 72) add field
   `_isRefreshingAvailability = false;` (re-entrancy guard against probe pile-up).
2. In `setup()` (lines 81-86) keep the immediate `this._refreshAvailability();` but replace the interval
   body so it re-fetches on **every** tick (drop the `if (this.isWorking)` gate). Keep
   `AVAILABILITY_REFRESH_INTERVAL` at `30_000`.
3. In `_refreshAvailability` (lines 194-208) make the early-return also check the guard, set the guard
   `true` before the request, and clear it in a `finally`. Leave `_applyAvailability` unchanged.
4. In `MithrilPartialSyncStore.spec.ts` rewrite the test at line 469 ("refreshes ... only while ... work is
   in flight") to assert refresh-while-idle: setup with idle status, assert 1 call after setup, advance
   `30_000` and `await mockAvailabilityRequest.mock.results[1].value` then assert 2 calls, advance again →
   3 calls. Leave the specs at :436 (one-shot), :461 (default-hidden), :501 (teardown-clears-interval).
5. Create `source/renderer/app/utils/mithrilBehindness.ts` exporting
   `isMithrilBehindnessKnown(localTip, networkTip): boolean` = both tips present with finite epochs
   (availability signal only, NOT a threshold).
6. Create `source/renderer/app/utils/mithrilBehindness.spec.ts` covering: missing networkTip → false,
   missing localTip → false, non-finite epoch → false, both finite → true.
7. In `NetworkStatusStore.ts` add `import { isMithrilBehindnessKnown } from '../utils/mithrilBehindness';`
   and a `@computed get isBehindnessKnown(): boolean` (after `isEpochsInfoAvailable`, before
   `isVerifyingBlockchain`) returning `isMithrilBehindnessKnown(this.localTip, this.networkTip)` (reads the
   tracked observables so consumers re-render when tips arrive).
8. **Contract for CAT-F (do NOT edit in CAT-A):** CAT-F AND-s **`networkStatus.isBehindnessKnown`** (this
   computed — NOT a local `behindByEpochs !== undefined` re-derivation) into the `showMithrilPrompt` gate, so
   this CAT-A deliverable is load-bearing and neither `isMithrilBehindnessKnown` nor `isBehindnessKnown`
   becomes orphaned dead code. The backend threshold `isSignificantlyBehind` is unchanged; the new signal
   only suppresses the early flash before the tip is known. Recorded here so CAT-F wires it (no "if available"
   hedge — the computed is the single source of the known-gate).
9. **Document + flag (no edit):** record in `task-ux-702a-research.md` that
   `MithrilController.getPartialSyncAvailability:167-174` delays static `isEnabled` behind the slow probe; an
   OPTIONAL backend hardening (return `isEnabled` immediately, refresh behind-ness independently) is flagged
   for the backend track but is out of CAT-A scope (keeps CAT-A renderer-only and collision-safe).

**i18n keys (EN+JA):** none (CAT-A adds no user copy).

**Tests/stories:**
- New `mithrilBehindness.spec.ts` (4 cases above). Command:
  `yarn test:jest source/renderer/app/utils/mithrilBehindness.spec.ts`.
- Updated `MithrilPartialSyncStore.spec.ts:469` (call counts 1→2→3 across idle ticks). Command:
  `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`.
- `isBehindnessKnown` is a one-line delegation covered transitively by the helper spec + CAT-F's
  prompt-gating test (no isolated `NetworkStatusStore` harness exists).
- Then `yarn compile`, `yarn lint`, `yarn prettier:check` on the five touched files.

**Locked-invariant notes:** Honors #4 (no renderer threshold — availability boolean only; significance
stays backend `isSignificantlyBehind`; no auto-trigger — read-only IPC). Honors D13/epochs-only + memory
vocabulary (no %/immutable values surfaced; no user copy). Honors #10 (kill-switch short-circuit keeps the
unconditional poll cheap). Honors #11 (additive edits, bootstrap untouched). **Amends no locked decision.**

**Acceptance criteria:**
- Diagnostics Mithril section self-corrects within ≤30s on first load **without a reload**.
- `NetworkStatusStore.isBehindnessKnown` is `true` only once both tips have finite epochs and re-renders
  consumers when the tips arrive.
- `mithrilBehindness.spec.ts` and the updated store spec pass; `tsc`/`lint`/`prettier` clean.
- No backend file is modified by CAT-A; the optional backend decoupling is flagged (not implemented).

---

## CAT-C — Confirmation body simplify (ISSUE-4, D-702a-4)

**Issues:** ISSUE-4 (confirmation body too verbose). Keep the epochs figure + `behindUnknown` fallback;
add the canonical D-702a-6 subtext via ONE shared i18n key. (Runs before CAT-B/CAT-F so the shared key
exists.)

**Exact files:**
- `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts` (new shared module)
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx`
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`,
  `translations/messages.json`

**Ordered steps:**
1. Create `MithrilSyncProcessSummary.messages.ts` exporting one `defineMessages` with key `processSummary`,
   id `daedalus.diagnostics.dialog.mithrilSyncProcessSummary`, defaultMessage (prefixed `!!!`) = the
   D-702a-6 canonical sentence, description noting it is shared by the confirmation modal and the proactive
   prompt. (Follows the existing `*.messages.ts` shared-module precedent; avoids a duplicate-id conflict.)
2. In `MithrilPartialSyncConfirmation.tsx` `defineMessages` (block :8-80): import the shared module; KEEP
   `title`/`cancel`/`confirm`; DELETE `intro`, `success`, `stepStop`, `stepDownload`, `stepRestart`,
   `recovery`; CHANGE `behind` defaultMessage to `!!!Your node is about {epochs} epochs behind. Mithril Sync
   will restore verified chain data to help your node sync faster.` and `behindUnknown` to `!!!Your node is
   behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync
   faster.`
3. Rewrite the render body (:131-151) to two `<p>` + the error: subject paragraph (className
   `styles.mithrilPartialSyncConfirmationBehind`) using `behind`/`behindUnknown` via the `hasBehindFigure`
   guard; subtext paragraph (className `styles.mithrilPartialSyncConfirmationRecovery`) using
   `mithrilSyncProcessSummaryMessages.processSummary`; then `{startError ? <div className={styles.error}> }`.
   **The subtext class is PINNED to `mithrilPartialSyncConfirmationRecovery`** (already in
   `DaedalusDiagnostics.scss.d.ts`): CAT-C never renames it, never adds a `...Subtext` class, and CAT-D
   restyles that exact `mithrilPartialSyncConfirmationRecovery` rule IN PLACE (no lockstep `.tsx`/scss
   rename — keeps CAT-D scss-only and guarantees the D-702a-4 restyle lands on the actually-rendered
   element). **Reuse existing class names only** — no `.scss` edit in CAT-C, and do NOT introduce a class
   absent from `.scss.d.ts` (breaks tsc).
4. Run `yarn i18n:extract` then `yarn i18n:check` to regenerate `translations/messages.json` +
   `defaultMessages.json` (drops the 6 removed ids, updates behind/behindUnknown, adds the new key).
5. Hand-finalize `en-US.json`: remove the 6 obsolete `...Confirmation{Intro,Recovery,StepDownload,
   StepRestart,StepStop,Success}` keys; set `...ConfirmationBehind` and `...ConfirmationBehindUnknown` to
   the EN strings (no `!!!`); ADD `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` = the EN canonical
   sentence. Keep Cancel/Confirm/Title.
6. Hand-finalize `ja-JP.json` mirroring step 5 with provisional JA (glossary: Mithril同期 / 検証済みのチェーン
   データ / Cardanoノード / 再起動 / シャットダウン): Behind = `ノードは約{epochs}エポック遅れています。Mithril同期で
   検証済みのチェーンデータを復元し、ノードの同期を高速化します。`; BehindUnknown = `ノードは最新の検証済みスナップ
   ショットより遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。`;
   processSummary = `この処理を開始するには、お使いのCardanoノードをシャットダウンする必要があります。その後、Mithril
   を使用して検証済みのチェーンデータを同期します。Mithril同期の完了時に、ノードを再起動して残りのブロックを同期します。`
7. Update `MithrilPartialSyncConfirmation.spec.tsx`: assert the new combined behind string (3 epochs) and
   keep the no-`% synced`/no-`immutable` negatives; assert the behindUnknown string; replace the
   steps-wording test with one asserting the canonical subtext renders AND removed copy is absent
   (`queryByText(/Daedalus stops Cardano node/)` null, `queryByText(/downloads and verifies Mithril data/)`
   null). Leave modal/ESC/buttons/error/blocked tests unchanged.
8. Verify: `yarn jest .../MithrilPartialSyncConfirmation.spec.tsx`, `yarn compile`, `yarn i18n:check`,
   `yarn lint` on the four source files. Confirm no `!!!` on the three changed/added EN keys and the 6
   removed keys are absent from all four catalogs.
9. Record in this doc + research note that `mithrilPartialSyncConfirmationBehind` was previously locked by
   304/601 (`tasks.json:617`) and is deliberately re-touched per D-702a-4; record the CAT-F handoff (reuse
   the shared `mithrilSyncProcessSummary` key, drop `mithrilProactivePromptConfirmBody`).

**i18n keys (EN+JA):**
- ADD `daedalus.diagnostics.dialog.mithrilSyncProcessSummary` (shared D-702a-6 subtext).
- CHANGE `...mithrilPartialSyncConfirmationBehind` (combined subject; overrides 304/601 lock).
- CHANGE `...mithrilPartialSyncConfirmationBehindUnknown` (fallback subject).
- REMOVE `...mithrilPartialSyncConfirmation{Intro,Success,StepStop,StepDownload,StepRestart,Recovery}`.
- KEEP `...mithrilPartialSyncConfirmation{Title,Cancel,Confirm}`.

**Tests/stories:** updated `MithrilPartialSyncConfirmation.spec.tsx` (3 changes above). No CAT-C storybook
change — the three confirmation stories render the simplified body automatically (storybook restructure is
CAT-H). Commands per step 8 (regen `.scss.d.ts` + identity-obj-proxy sidecar first under Node v24).

**Locked-invariant notes:** Honors #3 (confirm button is still the only start path), #4 (keeps backend
`behindByEpochs` + `hasBehindFigure`; no threshold), D13 (keeps `{epochs}` and the behindUnknown fallback;
no %/immutable), #11 (confirmation is partial-sync-only; the shared key is reused by the prompt, not the
bootstrap). **Deliberate amendment recorded:** re-touch of the 304/601-locked behind key.

**Acceptance criteria:**
- Confirmation body renders exactly two paragraphs (subject + canonical subtext) plus the error; the 6
  removed copy blocks are gone.
- The shared `mithrilSyncProcessSummary` key exists once and is consumed by both the confirmation and
  (later) the prompt.
- Spec asserts the new strings and the no-%/no-immutable negatives; `i18n:check` shows no missing/obsolete
  keys; tsc/lint clean.

---

## CAT-B — Diagnostics recommendation → hover tooltip + reword (ISSUE-3, D-702a-3)

**Issues:** ISSUE-3 (recommendation copy too long). Move it into a hover tooltip on the Mithril Sync
button, reword to the verbatim D-702a-3 string, remove the ready-hint.

**Exact files:**
- `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx`
- `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`,
  `translations/messages.json`

**Ordered steps:**
1. In `MithrilPartialSyncRecommendation.tsx` add `import { PopOver } from
   'react-polymorph/lib/components/PopOver';` (path matches `DaedalusDiagnostics.tsx:9`).
2. In `defineMessages` (:7-34): update `recommendation` defaultMessage to `!!!If Cardano Node syncing is
   taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync.`
   and its description to "Tooltip copy shown on hover over the Mithril Sync button in diagnostics"; DELETE
   the `buttonHintReady` descriptor (:27-33); leave `buttonLabel` and `buttonHintBlocked`.
3. Replace the render return (:53-74): wrap the existing Mithril Sync `<button>` in `<PopOver maxWidth={280}
   content={<div className={styles.tooltipLabelWrapper}>{intl.formatMessage(messages.recommendation)}</div>}>`;
   render the blocked hint only conditionally (`{isActionBlocked && <div
   className={styles.mithrilPartialSyncHint}>{intl.formatMessage(messages.buttonHintBlocked)}</div>}`);
   DELETE the old always-visible recommendation-copy `<div>` (:58-60) and the `hintMessage` ternary (:49-51).
   Reuse existing class names only (all in `DaedalusDiagnostics.scss.d.ts`); no `.scss` edit.
4. No `MithrilPartialSyncSection.tsx` change (props unchanged) — confirm by inspection.
5. `en-US.json`: delete the `mithrilPartialSyncButtonHintReady` line (:157); set
   `mithrilPartialSyncRecommendation` (:170) to the EN tooltip string.
6. `ja-JP.json`: delete the `buttonHintReady` JA line (:157); set the recommendation (:170) to provisional
   JA `Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、
   同期を高速化できます。`
7. `defaultMessages.json`: in the `MithrilPartialSyncRecommendation.tsx` descriptor block (:1908-1932)
   update the recommendation `!!!` defaultMessage + description and DELETE the `buttonHintReady` descriptor
   (with its preceding comma so JSON stays valid). Keep buttonLabel/buttonHintBlocked.
8. `translations/messages.json`: apply the identical descriptor edits (or run `yarn i18n:extract`).
9. `MithrilPartialSyncSection.spec.tsx`: add a PopOver mock rendering BOTH children and content; rewrite the
   three stale "back on recommendation" copy assertions (:63-67, :175-179, :255-259) to robust view-state
   assertions (button present + confirmation heading absent); add a tooltip-coverage test and a
   blocked-state test.
10. `DaedalusDiagnostics.spec.tsx` (PopOver mock renders only children, so tooltip content is absent):
    in the two recommendation tests (:76, :95) DELETE the now-lazy tooltip-content and ready-hint
    assertions; keep button-present + no-`/% synced/` + no-`/!!!/`; rename them. The existing blocked-hint
    test (:108-117) stays green (buttonHintBlocked retained).
11. **Flag for CAT-D (no edit):** `styles.tooltipLabelWrapper` is currently scss-nested under
    `.layoutData.stateDirectoryPath`, so CAT-D must hoist/duplicate it for general tooltip use;
    `.mithrilPartialSyncRecommendationCopy` becomes unused (CAT-D may prune). CAT-B leaves all scss untouched.

**i18n keys (EN+JA):**
- CHANGE `daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation` (EN+JA, → tooltip copy).
- REMOVE `daedalus.diagnostics.dialog.mithrilPartialSyncButtonHintReady` (EN+JA).
- KEEP `...mithrilPartialSyncButtonLabel` ("Mithril Sync") and `...mithrilPartialSyncButtonHintBlocked`.

**Tests/stories:** updated `MithrilPartialSyncSection.spec.tsx` + `DaedalusDiagnostics.spec.tsx` (above).
Commands: `yarn jest` on the two spec files; `yarn compile`; `yarn i18n:check`; `yarn lint` on the touched
files. (Node v24: regen `.scss.d.ts` + identity-obj-proxy sidecar first; CAT-B changes no scss so the
committed `.scss.d.ts` already covers every class.)

**Locked-invariant notes:** Copy/presentation only; amends no locked decision. Honors #3 (button still calls
`onShowConfirmation` → confirmation modal; tooltip is informational, no second start path), #4 (visibility
still gated upstream by backend `isMithrilPartialSyncSignificantlyBehind`; CAT-B computes no behind-ness),
#10 (the `isMithrilPartialSyncEnabled` gate untouched), #11 (no bootstrap component/scss touched). Locked
vocabulary preserved; specs keep no-`/% synced/`.

**Acceptance criteria:**
- The recommendation copy appears only on hover over the Mithril Sync button (tooltip), with the verbatim
  D-702a-3 text; the always-visible block and the ready-hint are gone.
- Blocked state still shows only the blocked hint; button disabled.
- Both specs pass; `i18n:check` shows no EN/JA drift; tsc/lint clean.

---

## CAT-E — Cancelled (pre-cutover) recovery trim + button copy + subtext→body + right-align (ISSUE-6)

**Issues:** ISSUE-6 (cancelled dialogue: trim wipe, button copy, subtext→body, action button on the right).
Backend array trim (D-702a-2) + renderer copy/layout.

**Exact files:**
- `source/main/mithril/MithrilPartialSyncService.ts`, `MithrilPartialSyncService.spec.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
- `.../mithril-bootstrap/MithrilErrorView.tsx`, `MithrilErrorView.scss`
- `.../mithril-bootstrap/MithrilPartialSyncOverlay.tsx`, `MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

**Ordered steps:**
1. **Backend trim (both cancel paths).** In `MithrilPartialSyncService.ts`, with `replace_all=true`, replace
   the multi-line `allowedRecoveryActions: ['retry','restart-normal','wipe-and-full-sync']` block (occurs
   exactly twice: ~:300-305 `status:'cancelled'`, ~:314-319 `status:'failed'` cancel-cleanup) with
   `allowedRecoveryActions: ['retry', 'restart-normal'],`. **Do NOT touch** the inline
   `return ['retry','restart-normal','wipe-and-full-sync'];` at :560 (active-sync failure path, out of
   scope) nor any `['wipe-and-full-sync']`.
2. **Backend spec (both cancel-path assertions).** In `MithrilPartialSyncService.spec.ts`, with
   `replace_all=true`, apply the same trim to the two multi-line blocks (~:877-881, ~:905-909). Leave the
   inline `['restart-normal','wipe-and-full-sync']` fixture (~:923) and all `['wipe-and-full-sync']` lines.
3. **Button copy.** In `MithrilBootstrap.messages.ts`: `partialSyncRetry` → `!!!Retry Mithril Sync (fast)`;
   `partialSyncRestartNormally` → `!!!Restart Node Sync (slow)`. Leave `partialSyncWipeAndFullSync`.
4. `en-US.json`: `loading.mithrilPartialSync.error.retry` → `Retry Mithril Sync (fast)`;
   `...error.restartNormally` → `Restart Node Sync (slow)`.
5. `ja-JP.json` (provisional, full-width 高速/低速 convention): retry → `Mithril同期を再試行（高速）`;
   restartNormally → `ノード同期を再起動（低速）`.
6. `defaultMessages.json`: retry defaultMessage → `!!!Retry Mithril Sync (fast)`; restartNormally →
   `!!!Restart Node Sync (slow)` (keep `!!!` + ids/descriptions).
7. **Subtext→body + primary-on-right (shared error view).** In `MithrilErrorView.tsx`: add
   `hintAsBody?: boolean;` to Props; destructure it; render the hint as `<p className={styles.hintBody}>`
   when `hintAsBody` else the existing `<div className={styles.errorHint}>`; before the `.actions` map
   compute `orderedActions = [...non-primary, ...primary]` and map that (primary renders last/rightmost).
   Bootstrap callers pass no `hintAsBody` (stays subtext).
8. **Layout-only scss (single edit).** In `MithrilErrorView.scss`: change `.actions` `justify-content:
   flex-start` → `flex-end`; add `.hintBody { color: var(--theme-mithril-body-text-color); font-size: 15px;
   line-height: 1.5; margin-top: 8px; }` (reuses the existing body token; color governed by CAT-D's later
   audit). This is the ONLY scss line/rule CAT-E touches (layout, not color) — coordinate so CAT-D does not
   revert it.
9. **Overlay wires hint-as-body.** In `MithrilPartialSyncOverlay.tsx` add `hintAsBody={status ===
   'cancelled'}` on the `MithrilErrorView` usage (~:196-202). No other overlay change here (CAT-G owns the
   completed branch).
10. **Overlay spec.** Replace both `/restart normally/i` matchers with `/restart node sync/i`; in the
    cancelled-recovery test set `canWipeAndFullSync:false`, remove the wipe click/assert, add
    `expect(screen.queryByRole('button',{name:/wipe chain data and do full mithril sync/i})).not
    .toBeInTheDocument()` and a DOM-order assertion `['Restart Node Sync (slow)','Retry Mithril Sync
    (fast)']`; in the cancelled-hint test assert the hint now renders inside a `<p>`. Do NOT touch the
    completed test (CAT-G).
11. **Story accuracy.** In `MithrilPartialSyncOverlay.stories.tsx` remove `canWipeAndFullSync` from the
    'Cancelled' story so it shows only Retry+Restart. Leave the two Failed stories.

**i18n keys (EN+JA):**
- CHANGE `loading.mithrilPartialSync.error.retry` (EN `Retry Mithril Sync (fast)`; JA `Mithril同期を再試行（高速）`).
- CHANGE `loading.mithrilPartialSync.error.restartNormally` (EN `Restart Node Sync (slow)`; JA `ノード同期を再起動（低速）`).
- No new/removed keys; cancelled/wipe copy unchanged.

**Tests/stories:** backend spec (trim) + overlay spec (label/trim/order/hint-as-body) + Cancelled story.
Commands: regen `.scss.d.ts` + identity-obj-proxy sidecar (Node v24) first; then
`yarn jest source/main/mithril/MithrilPartialSyncService.spec.ts`, `yarn jest
.../MithrilPartialSyncOverlay.spec.tsx`, the locale-strings guard test, `yarn compile`, `yarn jest
.../App.spec.tsx` (failed-state path unaffected).

**Locked-invariant notes:** Honors #2/#4/#5 (trim only the backend emitted array; renderer/store action
logic untouched), #6 (cancel still hard-rejects post-cutover; the guard spec stays green), #11 (the shared
`MithrilErrorView` right-align + primary-last reorder is visual-only, sanctioned by the decision's deferred
note, verified against bootstrap stories in code review). **ADR D-702a-2 amendment** applied; intentional
asymmetry (active-sync failure still 3-action via `:560`) flagged for review. The 13px-subtext→body change
is structural (`<p>` reusing `--theme-mithril-body-text-color`); colors left to CAT-D.

**Acceptance criteria:**
- Cancelled (pre-cutover) dialogue shows exactly two right-aligned buttons: "Restart Node Sync (slow)"
  (left/secondary) and "Retry Mithril Sync (fast)" (right/primary); no wipe button.
- Cancelled subtext renders as a body `<p>`.
- Backend + overlay specs pass; post-cutover wipe-only dialog and the active-sync failure 3-action set are
  unchanged; locale guard test passes.

---

## CAT-F — Proactive prompt: known-behind gating + persist into Wallet Summary (ISSUE-7, D-702a-5)

**Issues:** ISSUE-7 (prompt flashes during connecting; disappears when loading screen moves on; default
button; capitalize subtitle; canonical confirm copy). **Depends on CAT-A's behind-ness-known signal.**

**Root cause (verified).** The prompt is mounted inside the loading screen (`SyncingConnecting.tsx`) which
unmounts the moment Root returns the routed app, so it vanishes on the loading→Wallet Summary transition.
The App-level overlay region (`App.tsx`, sibling of `<Router>`) is the established persist surface. The
displayed epochs figure is `undefined` until `networkTip` arrives, so `behindByEpochs !== undefined` and
CAT-A's `networkStatus.isBehindnessKnown` computed encode the same "behind-ness is KNOWN" fact; CAT-F gates
on **CAT-A's computed** (keeping that deliverable load-bearing) while still computing `behindByEpochs` only
for the displayed figure. The current gate lacks any known-behind / status-idle guard.

**Exact files:**
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx` (new)
- `source/renderer/app/App.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnecting.tsx`
- `source/renderer/app/containers/loading/SyncingConnectingPage.tsx`
- `.../syncing-connecting/SyncingConnectingMithrilPrompt.tsx`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`
- `MithrilProactivePromptContainer.spec.tsx` (new), `SyncingConnectingMithrilPrompt.spec.tsx`

**Ordered steps:**
1. Create `MithrilProactivePromptContainer.tsx` (`@inject('stores','actions') @observer`,
   `defaultProps={stores:null,actions:null}`). In render: port the figure math verbatim from
   `SyncingConnectingPage.tsx:50-57` (`networkEpoch`/`localEpoch`/`behindByEpochs`) for the DISPLAYED figure
   only; gate on **CAT-A's `networkStatus.isBehindnessKnown` computed** read from the injected `networkStatus`
   store (do NOT re-derive a local `behindByEpochs !== undefined` — CAT-A's signal is the single source of the
   known-gate so it cannot be orphaned); compute `showMithrilPrompt =
   status==='idle' && isPartialSyncEnabled && isSignificantlyBehind && networkStatus.isBehindnessKnown &&
   !proactivePromptDismissedThisSession`; return `null` or `<SyncingConnectingMithrilPrompt
   behindByEpochs={...} onStart={mithrilPartialSync.startPartialSync}
   onDismiss={mithrilPartialSync.dismissProactivePrompt} />`. Top-of-file comment explains the hoist +
   `status==='idle'` mutual-exclusion with the overlay + the `networkStatus.isBehindnessKnown` anti-flash gate
   (CAT-A coupling).
2. Mount it app-level in `App.tsx`: after the `mithrilPartialSync.shouldShowOverlay && <MithrilPartialSync
   Overlay/>` block and before `<RTSFlagsRecommendationOverlayContainer/>`, insert
   `<MithrilProactivePromptContainer />` (no props; reads stores via inject). Renders outside `<Router>` so
   it survives every route; gated to `status==='idle'` so it never co-renders with the overlay.
3. Remove the old loading-screen mount in `SyncingConnecting.tsx`: delete the
   `SyncingConnectingMithrilPrompt` import, the four prompt props from `Props` (:48-51) and the destructure
   (:176-179), and the JSX block (:210-216).
4. Remove the now-dead gating in `SyncingConnectingPage.tsx`: delete `networkTip`/`localTip` from the
   destructure, the `behindByEpochs`/`showMithrilPrompt` computations (:45-61), `mithrilPartialSync` from
   the stores destructure if now unused, and the four JSX props passed to `<SyncingConnecting>`.
5. Repoint the prompt confirm body to the shared D-702a-6 key: in `SyncingConnectingMithrilPrompt.tsx`
   DELETE the local `promptConfirmBody` descriptor and in `renderConfirmView()` replace
   `messages.promptConfirmBody` with the **imported shared descriptor created by CAT-C** (single id; do NOT
   redeclare it). **Hard dependency on CAT-C** — use the exact exported module/id CAT-C created. Fallback
   only if CAT-C's module is somehow absent: create one exported shared module and import it in both
   surfaces (flag to orchestrator to avoid both categories creating it).
6. Capitalize: change `promptHandoffNote` defaultMessage `Mithril sync` → `Mithril Sync` (only that word).
7. Keep "Mithril Sync (fast)" as the primary/right action in `renderChoiceView()` (already `['primary',
   styles.actionButton, styles.primaryButton]`); do NOT add `autoFocus`. Add a one-line comment that the
   visible-highlight contrast fix is CAT-D's (theme tokens/.scss); CAT-F makes no scss change.
8. `en-US.json`: update `mithrilProactivePromptHandoffNote` value (capitalize); DELETE
   `mithrilProactivePromptConfirmBody`. Do not add the shared key here (CAT-C owns it).
9. `ja-JP.json`: update `mithrilProactivePromptHandoffNote` to `スキップした場合でも、Diagnostics画面から
   Mithril同期を開始できます。`; DELETE `mithrilProactivePromptConfirmBody`.
10. Run `yarn i18n:manage` (or hand-edit) to regenerate `defaultMessages.json`: drop the confirm-body
    descriptor, reflect the handoff-note change; verify the diff contains only those two prompt changes
    (no churn for the shared key).
11. Tests per below; confirm existing prompt label assertions still pass (labels unchanged).

**i18n keys (EN+JA):**
- CHANGE `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` (EN capitalize "Mithril Sync";
  JA → Mithril同期).
- REMOVE `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmBody` (consolidated into the shared key).
- REFERENCE (not create) the shared `mithrilSyncProcessSummary` key from CAT-C for the confirm view.

**Tests/stories:** new `MithrilProactivePromptContainer.spec.tsx` (gating matrix: all-pass renders + shows
"about 3 epochs behind"; non-idle status → nothing; disabled → nothing; not-significantly-behind →
nothing; `networkStatus.isBehindnessKnown===false` → nothing (anti-flash — assert the gate reads CAT-A's
computed, not a local `behindByEpochs` re-derivation); dismissed → nothing). Update
`SyncingConnectingMithrilPrompt.spec.tsx` to assert the canonical confirm body renders and the capitalized
handoff. Commands: `yarn jest` on both spec files; `yarn compile`; `yarn lint`; `yarn i18n:manage`. Manual:
prompt absent during connecting/verifying, appears once "Last network block" known, persists across
loading→Wallet Summary until Standard/Mithril clicked, never overlays an active overlay, hidden on
MithrilBootstrap (#11).

**Locked-invariant notes:** Honors #3 (choice button still only calls `showConfirmation`; `handleStart`
stays the sole `onStart` call site — no second start path), #4 (offer still uses backend
`isSignificantlyBehind`; the known-gate is a display-availability check, not a renderer threshold), #10
(kill-switch precondition kept), #11 (during bootstrap there is no local tip so `behindByEpochs` is
undefined and `status!=='idle'`, so the prompt stays hidden; manual verification + optional defensive
`mithrilBootstrap.status==='idle'` guard). **D-702a-5 governs** the mount relocation; D-702a-6 honored via
the shared key.

**Acceptance criteria:**
- The prompt does not flash during connecting/"verifying blockchain"; appears only once behind-ness is
  known and persists across the loading→Wallet Summary transition until the user picks Standard or Mithril.
- "Mithril Sync (fast)" is the highlighted primary/right button; subtitle reads "Mithril Sync".
- The confirm view uses the shared canonical sentence (no private duplicate key remains).
- Gating-matrix spec passes; no e2e regression (`ProactivePrompt`/`showMithrilPrompt` absent from tests).

---

## CAT-G — Completion auto-transition (auto-fire D9 finalize on timeout) (ISSUE-8, ADR D-702a-1)

**Issues:** ISSUE-8 (completion should match bootstrap flow; remove "Continue to Daedalus"; auto-fire D9
finalize on the completed-timeout). Renderer-only.

**Root cause / seams (verified).** Node startup already runs before 'completed', so the completed overlay
is a success acknowledgment that hands off to the normal loading/Wallet-Summary screen. The staging-folder
deletion lives in `MithrilPartialSyncService.finalizeCompletedPartialSync()` reached via
`store.dismissCompletedOverlay → mithrilPartialSyncFinalizeChannel`. CAT-G reuses this finalize path
unchanged and only swaps the trigger from a click to an auto-timeout.

**Exact files:**
- `.../mithril-bootstrap/MithrilPartialSyncOverlay.tsx`, `MithrilProgressView.tsx`
- `.../mithril-bootstrap/MithrilBootstrap.messages.ts`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`
- `MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (comment only)

**Ordered steps:**
1. `MithrilPartialSyncOverlay.tsx`: import `useEffect` (`import React, { useEffect } from 'react';`).
2. After `PROGRESS_STATUSES` (ends ~:52) add `const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;` (tunable
   linger).
3. After the props destructure (~:71) add an unconditional hook: `useEffect(() => { if (status !==
   'completed') return undefined; const timer = setTimeout(() => onDismissCompleted(),
   COMPLETED_AUTO_DISMISS_DELAY_MS); return () => clearTimeout(timer); }, [status, onDismissCompleted]);`
   (`onDismissCompleted` is the stable MobX action → fires the existing finalize IPC automatically). Keep
   the `onDismissCompleted` prop.
4. In the `MithrilProgressView` invocation simplify the removed-button props: `actionLabel` → always
   `MithrilBootstrapMessages.cancel`; add `'completed'` to the `hideAction` list; `onAction` → always
   `onCancel`. Keep the completed subtitle ternary.
5. Pass the new loading-animation prop:
   `completedTransitionLabel={intl.formatMessage(MithrilBootstrapMessages.partialSyncCompletedTransition)}`.
6. `MithrilProgressView.tsx`: add optional `completedTransitionLabel?: string;` to Props; destructure it.
7. Compute `isCompletedTransition = status==='completed' && !!completedTransitionLabel`; after the
   `isStartingNode` block render a gated completion block (spinner `SVGInline` + `<h2>` label) using the
   existing `.completionBlock`/`.completionSpinner`/`.completionTitle` classes (no scss edit). Bootstrap
   never passes the prop, so its completed frame is byte-for-byte unchanged (#11).
8. `MithrilBootstrap.messages.ts`: DELETE `partialSyncContinue`; reword `partialSyncCompletedSubtitle`
   defaultMessage to `!!!Mithril Sync completed successfully.`; ADD `partialSyncCompletedTransition` (id
   `loading.mithrilPartialSync.completed.transition`, defaultMessage `!!!Returning to Daedalus...`,
   description noting the spinner caption).
9. `en-US.json`: DELETE `loading.mithrilPartialSync.completed.continue`; set `...completed.subtitle` →
   `Mithril Sync completed successfully.`; INSERT `...completed.transition` → `Returning to Daedalus...`.
10. `ja-JP.json` (provisional): DELETE `...completed.continue`; set `...completed.subtitle` →
    `Mithril同期が正常に完了しました。`; INSERT `...completed.transition` → `Daedalusに戻っています...`.
11. `defaultMessages.json`: change completed.subtitle defaultMessage, DELETE the completed.continue object,
    ADD the completed.transition object (or run the project i18n regeneration after step 8).
12. `MithrilPartialSyncStore.ts`: update the stale `dismissCompletedOverlay` comment to reflect D-702a-1
    (auto-timeout replaces the explicit Continue click). No logic change.
13. Do NOT change `App.tsx`/`App.spec.tsx` or the store spec (`dismissCompletedOverlay` behavior unchanged).

**i18n keys (EN+JA):**
- REMOVE `loading.mithrilPartialSync.completed.continue`.
- CHANGE `loading.mithrilPartialSync.completed.subtitle` (EN `Mithril Sync completed successfully.`; JA
  `Mithril同期が正常に完了しました。`).
- ADD `loading.mithrilPartialSync.completed.transition` (EN `Returning to Daedalus...`; JA
  `Daedalusに戻っています...`).

**Tests/stories:** update the completed-overlay test to use `jest.useFakeTimers()` (scoped), assert no
"Continue to Daedalus" button, "Returning to Daedalus..." present, `onDismissCompleted` not called before
the timeout then called once after `advanceTimersByTime(4000)`; add `act` to the testing-library import.
Keep the locale-strings parity guard. Commands (Node v24 caveat): `yarn test:jest
.../MithrilPartialSyncOverlay.spec.tsx`, `yarn test:jest .../MithrilPartialSyncStore.spec.ts`,
`yarn compile`, `yarn lint`, `yarn prettier:check`, then `yarn i18n:extract` + `yarn i18n:check`.

**Locked-invariant notes:** **ADR D-702a-1** — amends locked #9/#16-as-amended-by-D9: the finalize IPC
still fires (via the unchanged `store.dismissCompletedOverlay`), now invoked by the timeout instead of a
click; all cleanup semantics (folder deletion + marker clear + reset-to-idle) preserved. Reviewers MUST
read this against the ADR; the PRD success-flow line (`...prd.md:782`) is superseded. Honors #11 (the
`MithrilProgressView` spinner is additive + gated on the new optional prop bootstrap never passes), #2/#5
(error/recovery branch untouched), #10 (no kill-switch change), no backend/IPC-contract change. The
601-touched `completed.subtitle` reword is a deliberate override (not a regression). Coordinate file overlap
with CAT-E (runs first; CAT-G only edits the completed/progress branch).

**Acceptance criteria:**
- The completed overlay shows a spinner + "Returning to Daedalus...", auto-dismisses after the timeout, and
  fires the finalize IPC exactly once; no "Continue to Daedalus" button.
- Staging folder deletion / marker clear / reset-to-idle are preserved (finalize path unchanged).
- Bootstrap's completed frame is unchanged; overlay spec + locale guard pass; tsc/lint clean.

---

## CAT-D — Theme/contrast audit (owns all `.scss` + theme tokens) (ISSUE-5, ISSUE-6(theme), ISSUE-9)

**Issues:** ISSUE-5/ISSUE-9 (theme contrast on dialogues/storybook), ISSUE-6(theme) (cancelled dialogue
colors). **Owns all `.scss` + theme token files.** Runs after the copy/logic/structure categories so class
names are settled.

**Root cause (verified).** Two components consume the WRONG token families: the proactive prompt
(`SyncingConnectingMithrilPrompt.scss`) uses report-issue tokens (near-transparent bg, white text →
invisible on light surfaces), and the confirmation modal's `.mithrilPartialSyncConfirmationBehind` forces
`--theme-network-window-white-color` (white-on-white in cardano/yellow modals). The 61-token
`mithrilBootstrap` blocks in all 9 daedalus themes + `createTheme.ts` are internally consistent — **no
token-value edits are expected** unless a concrete contrast failure shows up; the fix is component-scss
token migration plus a cross-theme visual audit.

**Exact files:** `SyncingConnectingMithrilPrompt.scss` (+`.d.ts`), `DaedalusDiagnostics.scss` (+`.d.ts`),
`MithrilErrorView.scss` (+`.d.ts`), `MithrilDecisionView.scss`, the 9 `themes/daedalus/*.ts`,
`themes/utils/createTheme.ts`.

**Ordered steps:**
1. **STEP 0 (sequencing/env):** run LAST; before treating tsc/jest fails as regressions, regen the scss
   type sidecars via `typed-scss-modules` and ensure the identity-obj-proxy jest sidecar is present.
2. **Prompt card** (`SyncingConnectingMithrilPrompt.scss`): migrate every color token from
   report-issue/color families to `--theme-mithril-*`: `.component` bg → `--theme-mithril-card-background`
   plus `box-shadow: 0 12px 42px var(--theme-mithril-card-shadow)` and `border: 1px solid
   var(--theme-mithril-panel-border-color)`; `.body`/`.confirmBody` → `--theme-mithril-card-text-color`;
   `.benefit` → `--theme-mithril-body-text-color`; `.handoffNote` → `--theme-mithril-secondary-text-color`;
   `.error` → `--theme-mithril-error-text-color`.
3. **Prompt button:** remove the `background-color: var(--theme-report-issue-button-background-color)`
   override from `.primaryButton` so the global `primary` ButtonSkin governs (keep the selector so CAT-F's
   `styles.primaryButton` reference stays valid).
4. **Prompt positioning (coordinate with CAT-F):** after CAT-F's final mount, adjust `.component`
   positioning only if needed so the card renders on both the loading screen and Wallet Summary (no
   `position: fixed` clipping the Wallet Summary header). Only judgement step; resolve against CAT-F's final
   JSX and verify in storybook + app.
5. **Confirmation modal** (`DaedalusDiagnostics.scss`): fix white-on-white with STANDARD dialog tokens (NOT
   mithril tokens — the modal surface is `--rp-modal-bg-color`, white in cardano/yellow): `.mithrilPartial
   SyncConfirmationBehind` color → `--theme-dialog-title-color`; `.mithrilPartialSyncConfirmationBody` add
   `color: var(--theme-dialog-text-color)`; **restyle the EXISTING `.mithrilPartialSyncConfirmationRecovery`
   rule IN PLACE** (this is the subtext paragraph CAT-C renders — do NOT add a `...Subtext` class and do NOT
   rename) to `{ color: var(--theme-dialog-text-color); font-family: var(--font-regular); font-size: 14px;
   line-height: 1.5; margin: 16px 0 0; opacity: 0.7; }`. **Class contract (matches CAT-C verbatim):** subject
   = `mithrilPartialSyncConfirmationBehind`, subtext = `mithrilPartialSyncConfirmationRecovery`. This keeps
   CAT-D scss-only (no `.tsx` edit) and guarantees the restyle lands on the actually-rendered element.
6. **Confirmation dead-code** (after CAT-C removes the JSX): delete only the genuinely-orphaned rules
   `.mithrilPartialSyncConfirmationSteps`, `.mithrilPartialSyncConfirmation`,
   `.mithrilPartialSyncConfirmationTitle`. **Do NOT delete `.mithrilPartialSyncConfirmationRecovery`** — it
   is the live subtext class CAT-C renders and step 5 restyles in place.
7. **Cancelled dialogue** (`MithrilErrorView.scss`): `.errorHint` color →
   `--theme-mithril-body-text-color`; confirm `.actions` `justify-content: flex-end` (set by CAT-E; keep
   it). Do not revert CAT-E's `.hintBody` rule.
8. **Shared-footer parity (optional, review-gated):** `MithrilDecisionView.scss` `.actions` → `flex-end`;
   leave `MithrilProgressView.scss` `.actions: center` unless review wants parity. Apply only if it does
   not regress bootstrap (#11).
9. **Theme-token audit (verify-first):** confirm all 9 daedalus mithrilBootstrap blocks + `createTheme.ts`
   have the full 61 tokens; visually audit contrast across all 10 theme tabs for prompt/confirmation/
   cancelled/progress. **Expectation: no token-value edits.** If a token fails contrast in a theme, fix it
   bounded to the mithrilBootstrap block AND update the matching derived rule in `createTheme.ts` to keep
   tooling parity. Do not touch any non-mithril token block.
10. **Regen + verify:** regen the changed `.scss.d.ts` (the confirmation `.d.ts` loses `Steps`,
    `mithrilPartialSyncConfirmation`, `mithrilPartialSyncConfirmationTitle`; it KEEPS
    `mithrilPartialSyncConfirmationRecovery`, now restyled — no `...Subtext` class is added); run
    `yarn prettier:check`, `yarn lint`, `yarn compile`, then `yarn storybook` and walk every theme tab for
    the four surfaces.

**i18n keys (EN+JA):** none (styling only).

**Tests/stories:** no new Jest specs; existing specs resolve classNames via the identity-obj-proxy sidecar,
so add/remove/rename scss classes will not break them — run `yarn test:jest` on the affected specs to
confirm green after `.scss.d.ts` regen. **Primary verification is Storybook visual review** across all 10
theme tabs for: proactive prompt (Known/Unknown), confirmation (Known/Unknown/Start Error), cancelled/error
overlay, progress overlay; plus a bootstrap regression check (open the bootstrap stories in Cardano + a
light theme to confirm the shared-view right-align/brighten did not break bootstrap). Consolidated gate:
`yarn check:all` after regen.

**Locked-invariant notes:** Edits ONLY `.scss` + (verify-only) theme tokens — no copy/IPC/store/behind-ness
change, so #2/#3/#4/#5/#8/#9/#10 untouched by construction. #11: STEP 7/8 touch shared `MithrilErrorView`/
`MithrilDecisionView` — visual-only, verified against bootstrap stories under the required code-review gate.
**Deliberate scoped amendment:** the cancelled overlay/prompt consume `--theme-mithril-*` per the decision,
but the CONFIRMATION MODAL is intentionally restyled with STANDARD `--theme-dialog-*` tokens (applying
mithril card/white tokens would reintroduce white-on-white on the white modal surface). No new
`--theme-mithril-*` tokens are added; any value fix stays bounded to the mithrilBootstrap block in both the
daedalus file and the `createTheme.ts` derivation.

**Acceptance criteria:**
- The proactive prompt card is a self-contained, always-visible card in all 10 themes (previously-invisible
  body text now readable); the prompt button matches other Daedalus primary buttons.
- The confirmation modal "behind" line and body are readable on the modal surface in all 10 themes (no
  white-on-white in cardano/yellow).
- The cancelled dialogue subtext reads as normal body text and the action button is right-aligned.
- Bootstrap stories show no visual/behavioral regression; tsc/lint/prettier clean; storybook walk passes.

---

## CAT-H — Storybook fidelity & reorg (ISSUE-10, ISSUE-11, ISSUE-12)

**Issues:** ISSUE-11 (only-text dialogue views), ISSUE-12 (missing download progress bar; icon
investigation), ISSUE-10 coordination (theme contrast → CAT-D). **Storybook-only**; no app components, no
`.scss`, no i18n. Runs LAST.

**Root cause (verified).** The only-text "Recommendation" views render the bare component without the
diagnostics chrome, so they look like text blocks; the real UI already exists as the full-page
`DaedalusDiagnostics` stories. The "Downloading File Count" story sets the active sub-item id to `'download'`
which `MithrilStepIndicator.showBars` does not match, so the bar never renders; the correct fixture uses id
`'step-3'` state `'active'`. The folder-vs-component sidebar icon is stock Storybook 6.4 behavior (nested
parent kinds get the folder glyph) — report-only, no trivial fix that preserves the requested grouping.

**Exact files:** `storybook/stories/nodes/status/Diagnostics.stories.tsx`,
`storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` (new),
`storybook/stories/loading/mithril/index.ts`,
`storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`.

**Ordered steps:**
1. **STEP 0 (read-first):** re-open the four targets + post-CAT-F `SyncingConnectingMithrilPrompt.tsx` and
   post-CAT-B/CAT-D components; verify the prompt prop signature is still `{ behindByEpochs?: number;
   onStart: () => Promise<void>; onDismiss: () => void }` and adjust the moved story args if CAT-F changed
   it.
2. **Download bar fix:** in `MithrilPartialSyncOverlay.stories.tsx` replace `getDownloadingProgressItems`
   with a 3-item array whose active sub-item is the `'step-3'` download anchor (`step-1`/`step-2`
   `completed`, `step-3` `active`, each `as const`) so the `InlineProgressBar` renders.
3. **Add the explicit partial-progress story:** after "Downloading File Count" add
   `.add('Download Progress Bar (Partial)', () => (<MithrilPartialSyncOverlayStory status="downloading"
   filesDownloaded={6} filesTotal={9} />))`.
4. **Rename Status → Diagnostic:** change the `storiesOf` kind strings `'Nodes / Status'` → `'Nodes /
   Diagnostic'` (and the Confirmation sub-kind to `'Nodes / Diagnostic / Mithril Partial Sync
   Confirmation'`). Do NOT rename the file or `status/` directory.
5. **Resolve only-text views (remove redundant Recommendation stories):** delete the `storiesOf('Nodes /
   Status / Mithril Partial Sync Recommendation', ...)` block and the now-unused
   `MithrilPartialSyncRecommendation` import + `recommendationBaseProps`. (Real UI preserved by the
   full-page `DaedalusDiagnostics` CTA stories, which after CAT-B render the recommendation as the
   tooltip-on-button.)
6. **Move the dialogue view under Mithril:** create `MithrilPartialSyncDialogue.stories.tsx` under kind
   `'Loading / Mithril / Mithril Partial Sync Dialogue'`, moving the proactive-prompt stories (Known Epochs
   Behind `behindByEpochs={120}`, Unknown Behind) with `proactivePromptBaseProps` copied verbatim; use
   `StoryDecorator` only.
7. **Remove the moved block** from `Diagnostics.stories.tsx` (the `'Nodes / Status / Mithril Proactive
   Prompt'` block) and the now-unused `SyncingConnectingMithrilPrompt` import + `proactivePromptBaseProps`.
8. **Register the new file:** add `import './MithrilPartialSyncDialogue.stories';` to
   `storybook/stories/loading/mithril/index.ts`.
9. **Icon investigation (report only):** record the root cause (folder glyph = nested parent kind; stock
   6.4 default; no icon config in `storybook/main.ts`) in `task-ux-702a-research.md`; state that the reorg
   keeps both parent groups by design so the folder icons persist and no trivial fix preserves the grouping.
   No code change.
10. **Theme/contrast coordination with CAT-D:** edit no `.scss`/token; after CAT-D lands, run
    `yarn storybook` and spot-check the touched stories across the 9 themes; report residual blends back as
    a CAT-D follow-up.

**i18n keys (EN+JA):** none (storybook-only; story labels pull from existing component i18n at render).
CAT-H does not participate in the serialized i18n chain or the copy-overview deliverable.

**Tests/stories:** no new spec files (no storyshots harness). Existing `MithrilPartialSyncOverlay.spec.tsx`
already protects the download-bar fixture shape (id `'step-3'` active) — do not modify it. Gates:
`yarn storybook:build` (must succeed — bundles every stories module), `yarn compile`, `yarn lint` on the
four touched files (keep `as const` on each state literal). Manual: sidebar shows NODES → Diagnostic and
LOADING → Mithril → Mithril Partial Sync Dialogue; the old Recommendation/Proactive-Prompt sub-kinds gone
from under Diagnostic; both downloading stories render the combined `InlineProgressBar`; the moved prompt
views render the full styled dialogue. Node v24: scss-typing failures are the known env setup, not a
regression (CAT-H edits no scss).

**Locked-invariant notes:** Touches ZERO app code/IPC/stores/.scss — cannot regress backend-authoritative
behavior. #11 honored (no bootstrap component/story modified; the download-bar fix reuses the same step-3
anchor convention the bootstrap preset already uses). #5 untouched (recovery wiring not edited). #9/#16
untouched (Completed story dismiss semantics are CAT-G's). Locked vocabulary honored (no copy added).
**Judgment recorded for reviewer:** the only-text views are satisfied by REMOVING the redundant standalone
Recommendation stories (real UI already covered by the full-page Diagnostic CTA stories) rather than
re-rendering them — flagged for explicit reviewer sign-off. All theme/contrast `.scss` stays CAT-D's.

**Acceptance criteria:**
- Storybook sidebar shows "Nodes / Diagnostic" (renamed) and "Loading / Mithril / Mithril Partial Sync
  Dialogue" (new) with the proactive-prompt views rendered as full styled UI, not text blocks.
- Both "Downloading File Count" and the new "Download Progress Bar (Partial)" overlay stories render the
  combined `InlineProgressBar` at a partial fill with the snapshot file counter.
- The icon root cause is documented (report-only); `yarn storybook:build`/`compile`/`lint` pass.

---

## Verification plan (whole task)
Per-category commands as listed above, run after each category and re-confirmed at the end:
`yarn test:jest` (touched specs), `yarn compile`, `yarn lint`, `yarn prettier:check`, `yarn i18n:check`
(after each i18n category), `yarn storybook:build`, and a manual cross-theme `yarn storybook` walk for
CAT-D/CAT-H. **Node v24 caveat (every category):** regen `.scss.d.ts` via `typed-scss-modules` and ensure
the gitignored identity-obj-proxy jest sidecar is present before treating any tsc/jest failure as a
regression. After all copy lands, the copy-overview subagent produces `task-ux-702a-copy-overview.md`.

## Risks / open questions
- **Shared-key coupling (CAT-C↔CAT-F):** the canonical D-702a-6 sentence must be a single exported i18n id;
  serialize CAT-C before CAT-F and pin the id. (CAT-C's plan uses `mithrilSyncProcessSummary`; CAT-F's plan
  references `mithrilPartialSyncProcessNote` — resolve to ONE id during CAT-C.)
- **Behind-ness-known wiring (CAT-A↔CAT-F):** CAT-F gates the prompt on CAT-A's
  `networkStatus.isBehindnessKnown` computed (not a local re-derivation), so the signal is load-bearing and
  CAT-A's `isMithrilBehindnessKnown` helper/computed cannot become orphaned; if that wiring is dropped the
  early flash returns. Tracked as an explicit cross-category contract.
- **Shared-component blast radius (CAT-E/CAT-G/CAT-D):** `MithrilErrorView`/`MithrilProgressView`/
  `MithrilDecisionView` are bootstrap-shared; all edits are additive/visual-only and gated by the bootstrap
  regression check (#11).
- **Asymmetry (CAT-E):** active-sync failure stays 3-action while cancelled is 2-action — intentional per
  ADR D-702a-2; escalate only if review wants `:560` trimmed.
- **`COMPLETED_AUTO_DISMISS_DELAY_MS = 4000` (CAT-G):** UX judgment not pinned by a decision — confirm with
  reviewer/operator.
- **Provisional JA:** all JA strings are best-effort and require the manual translator review via the
  copy-overview deliverable.

## Required doc/research updates
- `task-ux-702a-research.md`: backend `getPartialSyncAvailability` probe-delay finding + optional backend
  decoupling flag (CAT-A); the 304/601 override of `mithrilPartialSyncConfirmationBehind` (CAT-C); the
  storybook icon root cause (CAT-H); the deliberate ADR amendments D-702a-1/D-702a-2.
- `task-ux-702a-impl-review.md`: one `Implementation:` + `Code Review:` entry per category.
- `task-ux-702a-copy-overview.md`: produced after all copy lands (D-702a-7).

## Review-log paths
- Planning: `task-ux-702a-plan-review.md`
- Implementation: `task-ux-702a-impl-review.md`

## Status
- Planning status: `completed`
- Build status: `completed`
- Status: `completed`
- completedAt: `2026-06-29`

**Outcome summary.** All 8 implementation categories (CAT-A…CAT-H) landed sequentially on the shared
working tree in the collision-safe order `CAT-A → CAT-C → CAT-B → CAT-E → CAT-F → CAT-G → CAT-D → CAT-H`,
each followed by a per-category code-review pass that returned **APPROVED with zero open blockers**,
remediating all 12 manual-assessment issues. The two sanctioned ADR amendments held: **D-702a-1**
(auto-fire the D9 finalize on the completed-overlay timeout, replacing the "Continue to Daedalus" click
while keeping reset-to-idle + staging-folder removal + marker clear byte-for-byte intact) and **D-702a-2**
(the backend stops emitting `wipe-and-full-sync` for the two pre-cutover cancel-originated arrays only;
post-cutover wipe-only and the active-sync 3-action failure set are unchanged). The shared **D-702a-6**
canonical shutdown/restore/restart sentence is a single i18n id
(`daedalus.diagnostics.dialog.mithrilSyncProcessSummary`) consumed by both the confirmation modal and the
proactive prompt; **CAT-A's `isBehindnessKnown` computed is load-bearing** in CAT-F's anti-flash prompt
gate (not orphaned). Whole-task verification is **GREEN**: `yarn compile`, `yarn lint`, and `yarn stylelint`
each exit 0; `yarn test:jest` over all mithril/partial-sync suites + the changed `DaedalusDiagnostics.spec`
/ `App.spec` is 59 suites / 610 tests passing with 0 failures; no real regressions attributable to 702a.
Remaining human-gated items (not code blockers): the operator cross-theme Storybook visual walk for
CAT-D/CAT-H across all 10 themes, the manual Japanese-translator review of the provisional JA via
`task-ux-702a-copy-overview.md` (D-702a-7), and the OPTIONAL backend `getPartialSyncAvailability`
probe-delay decoupling flagged for the backend-correctness track (CAT-A, out of scope here). With 702a
complete, `task-ux-702` (the deployment QA gate) is unblocked.
