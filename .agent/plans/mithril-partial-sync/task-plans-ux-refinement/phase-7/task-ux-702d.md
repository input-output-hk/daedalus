# task-ux-702d — Mithril Sync UX finalization cleanup (ad-hoc wave)

> Canonical task plan + single source of truth for the **ad-hoc finalization cleanup wave** on the shipped Mithril
> Sync UX. This wave collects the small, independent presentation fixes that surfaced after the `task-ux-702c`
> manual-testing round and finalizes the remaining polish before the deployment QA gate (`task-ux-702`) can re-run.
> Findings are remediated as categories **CAT-A..** (extensible — more ad-hoc CATs are appended to this wave as they
> are triaged, each as its own per-category doc — see the decomposition index below). **CAT-A** (this document's first
> landed fix): the proactive-prompt card is translucent and bleeds the
> wallet summary behind it. Predecessor context: `task-ux-702c.md` / `task-ux-702c-decisions.md` (prior manual-testing
> wave, whose structure this mirrors), `mithril-partial-sync-prd.md` (status contract, Boundary A/B/C recovery model),
> `prompt-ux-refinement.md` (process rules, locked safety boundaries). All citations verified against the live
> post-702c working tree (2026-07-01).

## Execution status — CAT-A..CAT-E IMPLEMENTED and operator-validated (wave CLOSED 2026-07-02)

CAT-A (proactive-prompt card opacity), CAT-B (handoff-note copy), CAT-C (partial-sync overlay backdrop opacity), and
CAT-D (cancel correctness — the stage machine now honors cancellation, incl. the late-spawn follow-up hardening) are
implemented on the shared working tree. **CAT-E (cancel-teardown hardening) is IMPLEMENTED through Parts 1 +
2A/2B/2C** (2026-07-02): Part 1's cancel-lifecycle logging shipped first as the diagnostic gate; the resolved
operator re-repro confirmed the `_currentProcess` slot-clobber, so Part 2A fixes the slot lifecycle (opt-in
`trackAsCancelable` + the unconditional cancel-entry log), Part 2B gates the behind-ness probe during active work,
and Part 2C layers `detached` group-kill (`killProcessTree`) + the `safeExit()` shutdown reap as defense-in-depth;
Part 3 (exit-settle) stays evidence-gated and remained unnecessary. **Operator validation 2026-07-02 (re-repro #1
CONFIRMED):** cancelling out of the Mithril partial sync progress overlay reaches the terminal cancelled prompt
without error (no "restart Daedalus" / "Downloading the Mithril snapshot failed" floor), and every cancel-prompt
option works as expected — the last major issue spotted for the session. Re-repro #2 (quit-mid-download) was not
separately re-run and rides with the documented Windows/macOS teardown concerns into the `task-ux-702` manual
PR-testing/QA phase. **The wave is CLOSED** — no further ad-hoc categories were appended — and `task-ux-702` (the
deployment QA gate) is unblocked and may re-run.

| CAT | Scope (one line) | Status | Doc | Proof state |
|---|---|---|---|---|
| CAT-A | Proactive-prompt card fill token opaque (alpha `0.96 → 1`) | Landed | [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md) | Operator-verified presentation token |
| CAT-B | Handoff-note copy → "Daedalus Diagnostics screen under the Help menu. (Ctrl + D)" | Landed | [`task-ux-702d-cat-b.md`](./task-ux-702d-cat-b.md) | Copy/i18n landed; spec assertion updated |
| CAT-C | Partial-sync overlay backdrop tokens opaque (alpha `0.92 → 1`) | Landed | [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md) | Token landed; on-device theme sweep |
| CAT-D | Cancel correctness: interruptible stage machine + tracked converter + late-spawn kill | Landed | [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md) | 90/90 jest; operator cancel-flow validation CONFIRMED 2026-07-02 (with CAT-E re-repro #1) |
| CAT-E | Cancel teardown: slot-lifecycle fix (2A) + probe gating (2B) + group-kill/reap (2C) + cancel-lifecycle logging (Part 1) | **Parts 1 + 2A/2B/2C landed; Part 3 evidence-gated (not needed)** | [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md) | jest 174/174 + code review approved; operator re-repro #1 CONFIRMED 2026-07-02; re-repro #2 (quit-mid-download) + Windows tree path deferred to `task-ux-702` |

## Per-category implementation docs (decomposition — implement/record from these)

This canonical plan is decomposed into five self-contained per-category docs. CAT-E is implementable from its doc
alone to the small-model bar; CAT-A..D docs are the as-landed implementation records. This master stays the single
source of truth and the index.

| Order | Doc | Category (scope) | Findings | Status |
|---|---|---|---|---|
| 1 | [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md) | CAT-A — proactive-prompt card: make the fill fully opaque | #1 | Landed |
| 2 | [`task-ux-702d-cat-b.md`](./task-ux-702d-cat-b.md) | CAT-B — proactive-prompt handoff note: point to "Daedalus Diagnostics screen under the Help menu (Ctrl + D)" | #2 | Landed |
| 3 | [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md) | CAT-C — partial-sync overlay backdrop: make it a solid per-theme surface | #3 | Landed |
| 4 | [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md) | CAT-D — cancel correctness: make the stage machine honor cancellation | #4 | Landed (operator-validated 2026-07-02) |
| 5 | [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md) | CAT-E — cancel teardown: terminate the process group, reap on quit, log the cancel lifecycle | #5 | **Parts 1 + 2A/2B/2C landed; re-repro #1 CONFIRMED 2026-07-02 (re-repro #2 deferred to `task-ux-702`; Part 3 not needed)** |

**Appending future CATs (CAT-F..):** triage → (a) add the finding row to the traceability table, a 2-4-line
Scope entry, a status-table row, and an index-table row in THIS master; (b) author a self-contained
`task-ux-702d-cat-f.md` per the template above (sequencing, findings table, locked invariants inline, exact
files, ordered mechanical steps with pinned anchors, tests, verification, risks); (c) Planner + Critiquer
entries go to `task-ux-702d-plan-review.md`, Implementer/Code-Review entries to `task-ux-702d-impl-review.md`
— never to this master's tail; (d) mirror to the tasks JSON (targetPaths + notes/tests/acceptance, minor
version bump). The master grows ~8 lines per CAT.

## Task id + title
- **Id:** task-ux-702d
- **Title:** Mithril Sync UX finalization cleanup (ad-hoc wave)

## Interaction mode (AUTHORITATIVE)
`interactive_validation`. Decisions are recorded per-category in this file; the human-in-the-loop surfaces that keep
it non-autonomous are: (1) a per-category code-review pass after each CAT lands, and (2) operator visual re-test of
each touched surface on a running app across themes. Do **not** relabel autonomous — phase-7 tasks require
operator-run validation.

## Why now
`task-ux-702` (the deployment QA gate) is held `pending` behind the remediation waves
(`…702a → …702b → …702c`). Operator use of the post-702c build surfaced a further presentation defect on the
**proactive prompt** surface (CAT-A): the floating prompt card is semi-transparent, so the live wallet summary shows
through and the card copy is hard to read. This is a distinct issue from the two 702c prompt findings — 702c CAT-B
firmed the **full-screen overlay backdrop** (`MithrilBootstrap.scss .backdrop`), and 702c CAT-D re-aligned the
proactive prompt **chrome** (titles / alignment / buttons) and explicitly recorded that the reported "blur" on this
surface was a *misdiagnosis* (there is no `backdrop-filter` on the proactive card). CAT-A here is the actual
readability root cause on that same card: its **fill alpha**. 702d collects this and the remaining ad-hoc polish so
the gate can re-run. Chain: `…702a → …702b → …702c → …702d → …702`.

## Scope
**CAT-A (presentation / theme tokens):** the proactive-prompt card (`SyncingConnectingMithrilPrompt` — a floating
card with **no** dimming backdrop) bled the wallet summary through its `--theme-mithril-card-background` fill (alpha
`0.96` in every theme). Fix: token alpha → `1` in all nine static themes + the dynamic generator; styling/token-only.
Details: [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md).

**CAT-B (copy / i18n):** the proactive-prompt handoff note said only "…from the Diagnostics screen" — under-specified.
Retargeted to "…from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)" across the tsx `defaultMessage`,
EN + JA locales, and the regenerated `defaultMessages.json`; copy/i18n-only, Mithril-Sync vocabulary kept.
Details: [`task-ux-702d-cat-b.md`](./task-ux-702d-cat-b.md).

**CAT-C (presentation / theme tokens):** the partial-sync overlay backdrop tokens
(`--theme-mithril-overlay-backdrop-start`/`-end`, alpha `0.92`) bled the live Cardano Node status page through the
download/failure prompts (the overlay mounts *over* the syncing page). Fix: both stops' alpha → `1` in all nine
static themes + the dynamic generator; styling/token-only. Details: [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md).

**CAT-D (correctness / main-process cancel flow):** make canceling a Mithril Sync **actually stop the backend**, land
on the terminal `cancelled` prompt, and remove the staging dir at **any** cancelable stage. Three parts:
`_isCancelled` checkpoints at the `await` seams, tracked conversion subprocess, and the late-spawn
`_trackCurrentProcess(child)` registration hardening; main-process control-flow + tests only.
Details: [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md).

**CAT-E (correctness / main-process cancel teardown + observability):** make a genuine cancel (and an app quit during
an active sync) **actually terminate** the `mithril-client` process and settle cleanly, instead of leaving a live
download and firing the `abandonCancel` "restart Daedalus" floor. Three dependency-ordered parts: **Part 1** —
cancel-lifecycle logging (ships first; it is the diagnostic gate); **Part 2** (mechanism selected by Part 1's
`pstree` evidence) — `detached` spawn + `killProcessTree` + `safeExit()` reap; **Part 3** (evidence-gated) — additive
child-`'exit'` settle signal. This is the CAT-D "process-tree/group termination" residual promoted to a
planned/scheduled fix (not yet implemented). Touch-set widens beyond CAT-D to the eight CAT-E paths
(`mithrilCommandRunner.ts`/`.spec`, `killProcessTree.ts` (new), `MithrilPartialSyncService.ts`/`.spec`,
`chainStorageCoordinator.ts`/`.spec`, `MithrilController.ts`/`.spec` (new), `source/main/index.ts`).
Details: [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md).

**CAT-F..** *(reserved — additional ad-hoc finalization cleanups to be appended as triaged).*

## Non-goals
- **No change to the proactive-prompt gating / suppression logic** (`MithrilProactivePromptContainer.tsx:70,77,89`
  stays coupled to `isSignificantlyBehind` + the session-attempt guard — offer-signal boundary #4 is untouched).
- **No chrome / copy / layout change** — CAT-A does not touch titles, alignment, buttons, or i18n (those were 702c
  CAT-D/E). CAT-A changes only the card **fill opacity**.
- **No new colors, classes, or theme vars** — CAT-A only flips the existing token's alpha; it does not introduce a
  proactive-prompt-specific token.
- **No `backdrop-filter` on the proactive card** — the 702c misdiagnosis stands; the fix is opacity, not blur.
- **No SCSS / structure / mount change for CAT-C** — the partial-sync overlay is not re-parented or given a new
  background element, and `MithrilBootstrap.scss` (including the `.backdrop` `blur(5px)`) is untouched. CAT-C flips only
  the shared `--theme-mithril-overlay-backdrop-start`/`-end` token alpha; it does not fork a partial-sync-specific
  backdrop token or remove the (now-moot) blur.
- **No state-machine / PRD status-contract change for CAT-A/B/C** — those three are presentation-only. **CAT-D is a
  correctness fix** that changes `start()`'s internal control flow but adds **no new status** and makes **no change to
  the PRD status contract or the Boundary A/B/C recovery model** (see CAT-D scope + the reframed boundary #3).
- **Per-CAT non-goals** live in each cat doc (CAT-D's detailed non-goals: [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md)).

## Dependencies
- **Depends on `task-ux-702c`** (the prior manual-testing cleanup round).
- **Blocks `task-ux-702`** (the deployment QA gate; its `dependencies` gains `task-ux-702d`).

## Consulted docs / workflows / skills
- **Docs consulted:** `prompt-ux-refinement.md`, `.agent/readme.md`, `task-ux-702c.md`,
  `task-ux-702c-decisions.md`, `mithril-partial-sync-ux-refinement-tasks.json`, and the live repo files cited below.
- **Workflow docs consulted:** `.agent/workflows/frontend.md`, `.agent/workflows/storybook.md`,
  `.agent/workflows/update-doc.md`.
- **Skills consulted:** none.

---

## Finding → Category traceability (nothing dropped)

Full citation-bearing rows live in each cat doc's `## Findings closed & decisions implemented` table.

| # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #1 | Proactive-prompt card is translucent and hard to read (`--theme-mithril-card-background` alpha `0.96`; no dimming backdrop). Full citations: [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md). | **CAT-A** | Medium |
| #2 | Handoff note under-specifies where to restart Mithril Sync (named only "the Diagnostics screen"). Full citations: [`task-ux-702d-cat-b.md`](./task-ux-702d-cat-b.md). | **CAT-B** | Low |
| #3 | Partial-sync download-status + failure prompts show the node status page through the `0.92`-alpha backdrop token. Full citations: [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md). | **CAT-C** | Medium |
| #4 | Canceling a Mithril Sync does not stop the backend; the "Cleaning up…" frame is a veneer (uninterruptible `start()` stage machine). Full citations: [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md). | **CAT-D** | High |
| #5 | A genuine cancel leaves `mithril-client` running and fails cleanup on the first attempt (no group kill; `'close'`-not-`'exit'` settle; no quit reap; near-silent logs). Full citations: [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md). | **CAT-E** | High |
| … | *(reserved for appended ad-hoc cleanups)* | CAT-F.. | — |

---

## Locked safety boundaries (honor while reasoning)
1. **Vocab guardrail #8** holds — CAT-A is a token-alpha change only; no copy, no "partial sync"/%/immutable strings
   introduced. **CAT-D adds no user-facing copy** (main-process control-flow only).
2. **Offer-signal boundary #4** untouched — proactive-prompt suppression/gating logic is not modified.
3. **PRD status-contract & Boundary A/B/C model unchanged.** CAT-A/B/C are presentation-only; **CAT-D is a
   correctness fix** confined to `start()`'s internal control flow (no new status; Boundary-A-only cancel enforced,
   not weakened) — full text: [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md) `## Locked invariants`.
4. Carry the 702-series invariants unchanged (Boundary-A-only cancel, terminal `cancelled` recovery set
   `['retry','restart-normal']` with no wipe, kill switch #1/#2, staged-only #3, latest-snapshot-only #5, no bootstrap
   regression #7) — CAT-D and CAT-E touch none of them — full text:
   [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md) / [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md)
   `## Locked invariants`.

## Implementation approach — ordered checklist (wave level)

Implementation/record order: **CAT-A → CAT-B → CAT-C → CAT-D → CAT-E** (CAT-E strictly after CAT-D — shared
`MithrilPartialSyncService.ts`/`.spec.ts`; CAT-E internally Part 1 (logging) → re-repro evidence → Part 2
(termination, mechanism per the evidence) → Part 3 only if gated). Each CAT is followed by `yarn compile` +
touched-file lint/prettier + its jest specs + a per-category code-review pass. The per-CAT mechanical steps live in
each cat doc ([`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md) … [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md));
future CAT-F.. appends follow the convention in the decomposition index above.

## Verification plan

Wave-level gates (one line per CAT; details + exact commands in each cat doc):
- **CAT-A** — `yarn compile` + touched-file lint/prettier + Storybook/on-device theme sweep (card solid, copy
  legible) + operator visual check; details: [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md).
- **CAT-B** — spec assertion updated + Storybook/on-device read of the choice-view note; details:
  [`task-ux-702d-cat-b.md`](./task-ux-702d-cat-b.md).
- **CAT-C** — `yarn compile` + touched-file lint/prettier + on-device backdrop sweep (download **and** failure
  prompts solid across themes); details: [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md).
- **CAT-D** — jest (service + **required** coordinator assertion) green + operator cancel-at-each-stage re-test
  (verify-only, preprod/Linux); details: [`task-ux-702d-cat-d.md`](./task-ux-702d-cat-d.md).
- **CAT-E** — jest (service + coordinator + runner + controller specs) green + the **staged** Linux operator gate
  (Part-1 diagnostic run, then Part-2 cancel-at-each-stage + quit-mid-download). The Windows taskkill tree path is
  deferred to the `task-ux-702` packaged-build QA matrix; details: [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md).

The wave closes only when all appended CATs are landed **and** operator-validated.

## Risks / open questions
- **Wave is open:** additional ad-hoc CATs will be appended; the gate re-run waits on the full appended set.
- Per-CAT risks (shared-token blast radii; the CAT-E three-hypotheses mechanism gate, exit-settle trade-off,
  dev-terminal signal regression, quit-leak partial fix + deferred orphaned-staging sweep) live in the cat docs —
  see [`task-ux-702d-cat-a.md`](./task-ux-702d-cat-a.md), [`task-ux-702d-cat-c.md`](./task-ux-702d-cat-c.md),
  [`task-ux-702d-cat-e.md`](./task-ux-702d-cat-e.md).

## Review-log paths
- Decision record: *(not yet created — this wave records per-category decisions inline; add
  `task-ux-702d-decisions.md` if the wave grows to warrant a separate locked record).*
- Plan review: `task-ux-702d-plan-review.md` — **created**; carries ALL Planner/Critiquer entries for the wave
  (CAT-A..CAT-E), including the entries relocated on 2026-07-01 from this file's former journal tail (with
  provenance notes).
- Implementation review: `task-ux-702d-impl-review.md` — **created**; records the CAT-D landing plus the late-spawn
  hardening follow-up, focused validation, and the remaining operator gate; it now also carries the two Implementer
  journal entries relocated on 2026-07-01 from this file's former journal tail. It also carries the CAT-E entries —
  the Part-1 (logging) landing + review and the Parts 2A/2B/2C landing + two-iteration code review (2026-07-02).
- Research notes: `task-ux-702d-research.md` — **created**; captures the durable late-spawn child-registration race
  finding and the runtime-debugging cues that distinguished it from a stale renderer state, plus the **CAT-E** durable
  findings (no-`detached` process-group gap; `'close'`-vs-`'exit'` settle; app-quit reap gap; the diagnostic-logging
  seams).
- Per-category docs: `task-ux-702d-cat-a.md`, `task-ux-702d-cat-b.md`, `task-ux-702d-cat-c.md`,
  `task-ux-702d-cat-d.md`, `task-ux-702d-cat-e.md` (decomposed from this master 2026-07-01; this master is the
  single source of truth + index).

## Planning status / build status
- Planning status: **approved (CAT-A, CAT-B, CAT-C, CAT-D); CAT-E plan re-LOCKED (Part 2 re-scoped 2A → 2B → 2C)** —
  the CAT-E plan-critique passes are recorded in `task-ux-702d-plan-review.md`. The wave **closed on 2026-07-02**
  with no further ad-hoc CATs appended.
- Build status: **CAT-A + CAT-B + CAT-C + CAT-D landed** (incl. the CAT-D late-spawn `_trackCurrentProcess(child)`
  hardening, synced 2026-07-01); focused verification green — `yarn test:jest` service + coordinator specs
  **90/90 passed**, touched-file diagnostics clean, `mithrilCommandRunner.ts` untouched by CAT-D. The operator
  cancel-flow validation closed with the CAT-E re-repro #1 confirmation (2026-07-02).
- **CAT-E: Parts 1 + 2A/2B/2C IMPLEMENTED (2026-07-02); Part 3 evidence-gated and NOT needed.** Verification green
  (`yarn compile`, lint 0 errors, prettier clean on the authored files, scoped jest 174/174 across the six touched
  specs); code review **approved** after 2 iterations. **Operator re-repro #1 CONFIRMED 2026-07-02** (cancel out of
  the progress overlay lands on the cancelled prompt without error/floor; all cancel-prompt options work); re-repro
  #2 (quit-mid-download) deferred to the `task-ux-702` manual PR-testing/QA phase alongside the documented
  Windows/macOS teardown checks. See `task-ux-702d-cat-e.md` + the CAT-E entries in `task-ux-702d-plan-review.md` /
  `task-ux-702d-impl-review.md`.
- Tasks JSON: `task-ux-702d` **entry present** in
  `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-tasks.json` (`status: "completed"`,
  `completedAt: "2026-07-02"`, `dependencies: ["task-ux-702c"]`; targetPaths incl. the ten theme files + the CAT-D
  source/spec files + the 702d plan/review/research docs + the **eight** CAT-E paths (`mithrilCommandRunner.ts`/`.spec`,
  `killProcessTree.ts`, `MithrilController.ts`/`.spec`, `chainStorageCoordinator.ts`/`.spec`, `index.ts`) + the five
  per-category docs; CAT-A..E implementationNotes + testCases + acceptance incl. the 2026-07-02 closure entries;
  `metadata.version` `1.10.1`), and `task-ux-702d` is in `task-ux-702`'s `dependencies` (chain
  `…702a → …702b → …702c → …702d → …702`). The deployment QA gate `task-ux-702` is now the sole remaining
  phase-7 task.
