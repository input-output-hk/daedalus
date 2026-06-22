# Mithril Partial Sync — UX Refinement Sprint: Implementation Tasking Prompt

You are the orchestrator for the **Mithril partial sync UX refinement sprint** (issue #10, Phase 2).
The PRD, the tasks JSON, the research dossier, and the per-task docs you create under
`task-plans-ux-refinement/` are the only context needed to drive the work.

The feature is **already implemented end-to-end** (backend → IPC → store → overlay). This sprint
**refines the UX** and closes enumerated gaps. It is not a rebuild.

---

## Sources of truth (read these; do not re-derive)

- **Tasks (what to build, ordering, dependencies):**
  `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-tasks.json`
- **Design intent + locked decisions (D1–D10), scope, non-goals, testing, rollout:**
  `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-prd.md`
- **Current-state + gap dossier (35 gaps + issue-#10-comment items, with exact file/line anchors):**
  `.agent/plans/mithril-partial-sync/research/19-ux-refinement-state-and-gaps.md`
- **Backend safety posture (boundary recovery model, staged-restore, marker semantics):**
  `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` (original PRD; the UX PRD supersedes
  only the UX sections it decides differently)
- **Repo docs / workflows:** `.agent/readme.md`, `.agent/system/architecture.md`,
  `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`,
  `.agent/workflows/update-doc.md`
- **Repo-applicable skills:** `understand` (repo understanding), `i18n-messaging` (copy/translations),
  `storybook-creation` (stories), `e2e-test-creation` (Cucumber), `theme-management` (theme tokens),
  `git-commit-formatter` (final commit).

If the PRD, the research, the tasks JSON, and the live repo ever disagree, **prefer the live repo**,
record the conflict in the task's research doc, and reconcile the governing doc — never silently pick
one stale source.

---

## Per-task document structure (REQUIRED)

For this sprint, all task docs live under a **new** tree, separate from the original `task-plans/`:

```
.agent/plans/mithril-partial-sync/task-plans-ux-refinement/
  phase-1/
    task-ux-101.md                 # canonical task plan + final outcome (single source of truth)
    task-ux-101-plan-review.md     # append-only Planner/Critiquer transcript
    task-ux-101-impl-review.md     # append-only Implementation/Code-Review transcript
    task-ux-101-research.md        # durable findings (decisions, gotchas, evidence, residual gaps)
    task-ux-102.md  ...            # one set of four files per task
  phase-2/   task-ux-201.* ...
  phase-3/   task-ux-301.* ...
  phase-4/   task-ux-401.* ...
  phase-5/   task-ux-501.* ...
  phase-6/   task-ux-601.* ...
  phase-7/   task-ux-701.* ...
```

Rules:
- **Naming:** every task file is `task-ux-<NNN>*`. The task IDs in the JSON already encode the phase:
  `task-ux-1xx` → `phase-1/`, `task-ux-2xx` → `phase-2/`, and so on. **Put each task's four files in the
  subdirectory that matches its hundreds digit.** Create the phase subdirectory if it does not exist.
- **Four files per task:** the canonical plan (`task-ux-NNN.md`), the planning review log
  (`task-ux-NNN-plan-review.md`), the implementation review log (`task-ux-NNN-impl-review.md`), and the
  research note (`task-ux-NNN-research.md`). The research note is this sprint's equivalent of the old
  flat `research/NN-*-notes.md` files, colocated with the task.
- **Do not** backfill docs for the original (already-completed) `task-001…task-401` sprint, and do not
  write into the original `task-plans/` or flat `research/` trees for this sprint's work.

### Canonical task plan doc — minimum contents
`task-ux-NNN.md` must capture: task id + title; why now; **interaction mode** (`autonomous` |
`interactive_decision` | `interactive_validation` | `manual_execution`); scope + non-goals;
dependencies; research/docs/workflows/skills consulted; files expected to change; **implementation
approach (step-by-step)**; acceptance criteria; verification plan; risks/open questions; required
doc/research updates; review-log paths; planning status (`draft`|`in_review`|`approved`); build status
(`in_progress`|`in_review`|`completed`).

### Lower tasking detail must be small-model-implementable (REQUIRED)
The tasks JSON is **high-level**. The **canonical task plan doc is where the detail lives**, and it must
be concrete enough that **a smaller, less-capable model can implement the task end-to-end from that doc
alone**, without the orchestrator's reasoning or a large context window. Concretely, each canonical doc's
implementation approach must:
- name the **exact files** to edit (the tasks JSON and research-19 already give file/line anchors — carry
  them in and verify them against live code);
- specify the change as **ordered, mechanical steps** (what to add/remove/rename, the function/prop/type
  names, the new IPC channel/message shape, the i18n keys);
- quote or pin the **exact existing code seam** being changed (line anchors) so the implementer does not
  have to hunt;
- state the **locked invariants the change must not break** (see below) inline, not by reference only;
- list the **specific tests** to add/update and the **commands** to run for verification.
If a step would require judgment the small model cannot safely make, resolve it during planning (or escalate
to the user) rather than leaving it implicit.

---

## Locked safety boundaries (carry into EVERY task; never silently break)

From the UX PRD non-goals and research-19 §5. The orchestrator and every subagent must honor these:
1. **Staged-only restore**; no in-place mutation of the live chain.
2. **Boundary A vs B/C1/C2 recovery model is backend-authoritative.** Render recovery actions **strictly
   from `allowedRecoveryActions`**; never infer from status names.
3. **Confirmation precedes start.** Confirming is the only path to backend start — including the proactive
   prompt's "Review" deep-link, which lands on the confirmation modal, not a second start path.
4. **No auto-trigger** and **no renderer-computed threshold** — the backend owns the behind-ness signal.
5. **Latest snapshot only; no snapshot-selection UI and no user-facing storage-location picker.** (The
   *engineering* placement of scratch/staging space is in scope — D7/BUG3.)
6. **Cancellation forbidden after cutover.**
7. **Supported networks:** mainnet, preprod, preview.
8. **No synthetic throughput / remaining-time / overall-% over IPC**; never route raw mithril-client JSON
   strings into UI copy.
9. **Locked decision #16, as amended by PRD D9.** #16: the success overlay follows the live lifecycle
   through `completed` and clears only on an **explicit user dismiss**. D9 adds: on that dismiss the backend
   resets to `idle`, removes the staging dir, and clears the marker (the CTA re-arms via the `isWorking`
   guard). Implement D9 exactly as written; the success screen must stay visible until explicit dismiss.
10. **Kill switch `mithrilPartialSyncEnabled` is the rollout lever**; when off, all partial-sync UI hides.
11. **Do not regress the empty-chain Mithril bootstrap flow** (D4 changes the shared progress components —
    verify bootstrap's success path).

---

## Execution loop (per task — keep it lean)

Pick the **lowest-ID unblocked pending task** whose dependencies are all `completed` — **dependencies
always win over numeric order** (e.g. `task-ux-302` waits on the higher-numbered `task-ux-303`, which
builds the confirmation modal it deep-links into). The JSON's `dependencies` are authoritative;
`summary.criticalPath` is guidance and may lag. Then:

1. **Classify the interaction mode** and record it in the canonical doc. Phase-7 tasks
   (`task-ux-701`, `task-ux-702`) are `interactive_validation` / `manual_execution` — they require
   operator-run validation and a user rollout decision; never relabel them autonomous. If a task is
   `interactive_decision`, stop and ask the user the minimum blocking question before building.
2. **Plan.** Before any repo work, the active role reads `.agent/readme.md`, `.agent/system/architecture.md`,
   the workflow doc(s) relevant to the task (frontend / ipc / test / update-doc), the PRD decision(s) the
   task implements, the matching research-19 gap rows, and the live files. Use `understand` for nontrivial
   tasks; verify findings against live files. Write the canonical `task-ux-NNN.md` to the small-model bar
   above, then append a `Planner:` entry to `task-ux-NNN-plan-review.md`.
3. **Critique (subagent — REQUIRED).** Dispatch a reviewer subagent to do **one broad pass** over the plan
   for: gap/PRD coverage, consistency with the tasks JSON + locked invariants, hidden manual checkpoints,
   missing tests/docs, and **whether the lower detail is concise and small-model-implementable**. It returns
   a consolidated blocker list + `Decision: approved | requires_changes`, appended as a `Critiquer:` entry.
   One critique pass + at most one Planner fix pass, then build.
4. **Implement.** Execute the approved plan; keep each task's focused tests with it. Append an
   `Implementation:` entry to `task-ux-NNN-impl-review.md` summarizing changes, files, verification, and
   any deviations or user handoff.
5. **Code review (subagent — REQUIRED).** Dispatch a reviewer subagent for one broad pass over the diff vs
   the approved plan (correctness, locked-invariant regressions, bootstrap regression, IPC/contract drift,
   missing tests, doc drift, and unnecessary complexity). It returns blockers + `Decision`, appended as a
   `Code Review:` entry. Loop until approved (cap 5 iterations; escalate to the user if not clean).
6. **Document (Scribe).** Update the canonical doc's final outcome, set the JSON task `status` to
   `completed` (+ `completedAt`), and record durable findings in `task-ux-NNN-research.md` (or
   `no new research`).
7. **Commit.** Create exactly **one** commit per task with `git-commit-formatter`:
   `<type>(mithril): task-ux-NNN <short imperative summary>`. Commit only task-relevant files. The task is
   not done until the commit exists.

Review logs are **append-only** chronological transcripts: each entry carries a speaker label, an ISO-8601
UTC `Timestamp:`, and an outcome; Critiquer/Code-Review entries end with `Decision: approved` or
`Decision: requires_changes`. Append at end-of-file only; never rewrite history.

### Convergence
Prefer the **smallest truthful change** that satisfies the task, the PRD decision it implements, and the
locked invariants. Reuse existing seams, types, components, channels, and tests over new abstractions
(e.g. `retry` reuses the start path — no new channel; the only genuinely new channel this sprint is the
D9 finalize/dismiss channel). Critique and review should push toward simplification and convergence, not
maximum iteration count.

---

## Stop conditions (ask the user)

- An `interactive_decision` task's blocking question (e.g. confirmation copy approval).
- The phase-7 manual QA / rollout decisions (production kill-switch default, network-scoping, merge/ship
  readiness) — these are user/operator-owned.
- Planning or build max-iteration guard tripped.
- A destructive/irreversible action, or a material tradeoff the PRD/research does not already resolve.

When pausing for user input, persist the handoff in the task docs and stop; do not auto-advance or
speculate. A pause is a valid in-progress state, not a failure.

---

## Definition of done (per task)
Acceptance criteria met · verification executed and reported · subagent plan-critique and code-review
clean (or user-approved escalation) · canonical `task-ux-NNN.md` finalized · both review logs preserved ·
`task-ux-NNN-research.md` updated (or explicit `no new research`) · tasks JSON synchronized · exactly one
task commit created.
