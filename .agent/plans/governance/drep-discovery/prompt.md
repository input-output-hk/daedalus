# DRep Discovery — Vertical-Slice Implementation: Orchestration Prompt

You are the orchestrator for the **DRep Discovery** feature on branch `feat/drep-discovery`.
The plan, the tasks JSON, the design/research dossier, and the per-slice docs you create under
`task-plans/` are the only context needed to drive the work.

**Slice-1 (walking skeleton + sanitization floor) and its final pass are already landed and
closed out.** This prompt drives the remaining phases: `slice-2`…`slice-8`, `cv-1`/`cv-2`,
`anchor-1`/`anchor-2`, and `ux-refinement`, plus the event-driven `standing` guardrail. Do not
rebuild or refactor slice-1 beyond what a task explicitly asks.

---

## Sources of truth (read these; do not re-derive)

- **Tasks (what to build, acceptance, dependencies, status):**
  `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
- **Plan (requirements, Key Decisions table, technical design, risks, slice sequencing):**
  `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`
- **Locked decisions, binding feature scope, working conventions:**
  `.agent/plans/governance/drep-discovery/README.md`
- **Design contracts (IA, copy, badges, formatting, refresh states):**
  `designs/shared-design-tokens.md`, `designs/drep-discovery-design.md`,
  `designs/current-vote-display-design.md`, `designs/current-vote-display-ux.md`
- **Research (verified findings; do not re-litigate):** `research/external-research.md`,
  `research/ux-refinement-sync-and-load-research.md`, `research/slice-1-final-pass-findings.md`,
  and the captured preprod fixture `research/drep-state-preprod-epoch295-sample.json` (+ README)
- **Slice-1 precedent (doc style + as-built seams):** `task-plans/slice-1-PRD.md`,
  `task-plans/slice-1-implementation-guide.md`, `task-plans/slice-1-code-review.md`
- **Repo docs / workflows:** `.agent/readme.md`, `.agent/system/architecture.md`,
  `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`,
  `.agent/workflows/storybook.md`, `.agent/workflows/update-doc.md`
- **Repo-applicable skills:** `bech32-encoding-decoding` (CIP-129/CIP-105 encode/decode work),
  `i18n-messaging` (copy/translations), `storybook-creation` (stories), `e2e-test-creation`
  (Cucumber), `theme-management` (theme tokens), `evidence-rules` (citations in docs),
  `cardano-cli-doctor` / `cardano-cli-wallets` (CLI grammar questions), `git-commit-formatter`
  (per-task commit).

If the plan, the README, the tasks JSON, and the live repo ever disagree, **prefer the live
repo**, record the conflict in the slice's research note, and reconcile the governing doc —
never silently pick one stale source.

---

## Per-slice document structure (REQUIRED)

Docs are **slice-level**, following the slice-1 precedent — not per-task file sets. For each
phase `<id>` (`slice-2`, `cv-1`, `anchor-1`, `ux-refinement`, …):

```
.agent/plans/governance/drep-discovery/
  task-plans/
    <id>-PRD.md                    # slice scope, per-task acceptance, interaction modes, DoD
    <id>-implementation-guide.md   # small-model-implementable per-task detail
    <id>-code-review.md            # append-only Planner/Critiquer + Code Review transcript
  research/
    <id>-findings.md               # durable findings — create only when there is something to record
```

Rules:
- **Naming keys off the tracker phase id** (`slice-2-PRD.md`, `cv-1-PRD.md`,
  `ux-refinement-PRD.md`, …). Do not invent phase subdirectories or per-task file sets.
- **Do not** backfill or rewrite the slice-1 docs; they are the closed precedent.
- The research note is optional per slice: write it when a slice produces durable findings
  (decisions, gotchas, evidence, residual gaps); otherwise record `no new research` in the PRD's
  final outcome.

### Slice PRD — minimum contents
Slice id + name; why now; per-task: **interaction mode** (`autonomous` | `interactive_decision`
| `interactive_validation` | `manual_execution`), scope + non-goals, dependencies; docs /
designs / research / workflows / skills consulted; locked invariants touched; risks / open
questions; planning status (`draft` | `in_review` | `approved`); final outcome section filled at
slice close.

### Implementation guide — small-model-implementable (REQUIRED)
The tasks JSON is high-level. The **implementation guide is where the detail lives**, and it
must be concrete enough that **a smaller, less-capable model can implement each task end-to-end
from the guide alone**, without the orchestrator's reasoning or a large context window. Per
task it must:
- name the **exact files** to edit (several tasks already carry file/line anchors — carry them
  in and verify against live code);
- specify the change as **ordered, mechanical steps** (what to add/remove/rename, the
  function/prop/type names, IPC channel/message shapes, i18n keys);
- quote or pin the **exact existing code seam** being changed (line anchors) so the implementer
  does not have to hunt;
- state the **locked invariants the change must not break** inline, not by reference only;
- list the **specific tests** to add/update and the **commands** to run for verification.
If a step would require judgment the small model cannot safely make, resolve it during planning
(or escalate to the user) rather than leaving it implicit.

---

## Locked invariants (carry into EVERY task; never silently break)

From the plan's Key Decisions, the README's binding scope, and the two non-negotiable floors.
The orchestrator and every subagent must honor these:

1. **Local-first.** Discovery data comes only from the local node via the main-process
   `GovernanceQueryService`. No hosted explorers, indexers, GovTool, Koios, Blockfrost, or
   public governance APIs.
2. **Sanitization floor (inherited by every slice).** No DRep id, no `abstain` /
   `no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger, analytics, or
   electron-store payload — re-asserted via the task-111 spy suite in every slice. The task-168
   DRep-state snapshot is the one documented exception: public on-chain directory data that
   deliberately bypasses `filterLogData` and must never include the user's own vote.
3. **Anchor transport-security floor.** The full anchor-1 guard set (TLS on, redirects off,
   ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding mitigation,
   Blake2b-256 hash-verify before parse/cache/render, immutable hash-keyed cache) lands complete
   in `anchor-1` and is never thinned. No anchor-derived content renders without verification +
   a verified off-chain source label. Anchor URLs open only through the HTTPS-only-hardened
   `open-external-url` path (task-152).
4. **No second delegation backend.** Selection supplies a DRep ID to the existing
   `delegateVotes` / `VotingStore` signing paths via React Router `location.state` only.
   `VotingStore` never reads `GovernanceStore` directly.
5. **Lovelace losslessness.** `json-bigint` lossless parse → decimal-string IPC → renderer
   `BigNumber` rehydration. Never route raw `JSONbig` objects across IPC or into observables.
6. **CLI discipline.** Bulk `--all-dreps` once per refresh — per-DRep CLI invocations are
   forbidden. Network flag (`--mainnet` / `--testnet-magic <N>`) derives from node config only,
   never from renderer/IPC input. Socket goes through `CARDANO_NODE_SOCKET_PATH` in `spawn.env`,
   not argv. Era token `latest` with `conway` fallback.
7. **Default cohort is binding.** Exclude top 35 by voting power; up to the next 200 eligible
   (active, remaining `drepActivity` > 6 epochs, completed metadata when available), randomized.
   The 6-epoch floor is binding in production — fixtures that violate it must not ship. The
   default cohort IS the "Recommended" sort: no Recommended tab, no per-card Recommended badge.
8. **Badges are informational only.** The category badge (slice-5: Primary / Threshold /
   Non-metadata; High value only after anchor-1) never reorders, filters, or overrides the
   cohort.
9. **No auto-delegation.** Daedalus never picks a delegation. The `noDelegation` state shows
   the CIP-1694 reward-withdrawal warning + CTA.
10. **Byte-equality.** CIP-129, CIP-105, and the signed payload `vote.id` remain byte-equal
    through every identity-display change; on-device DRep ID equals `vote.chosenOption`.
11. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!` marker.
    Removing `!!!` is a release-end manual review, never a per-slice task.
12. **Favorites are per-device** via Electron local store — not per-wallet, not synced.
13. **`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory entries.
14. **DRep status grounding.** Canonical on-chain status is `active | inactive`
    (`currentEpoch >= expiry`); `expiring` is renderer-derived display state; `retired` is
    deferred until a distinct unregistration signal exists.

---

## Execution order and loop

### Slice order (locked)

```
slice-2 → slice-3 → ux-refinement → slice-4 → slice-5 → slice-6 → slice-7
        → cv-1 → cv-2 → anchor-1 → anchor-2 → slice-8
```

`ux-refinement` runs right after the foundation because it hardens the already-shipped
directory (sync banner, two-phase load) that users hit first. `slice-8` closes the feature
(release verification last). Within a slice, follow dependency order, then JSON listing order.
**The JSON's `dependencies` are authoritative and always win** — including cross-slice edges
(e.g. `task-119` needs `task-116`; `anchor-1`'s render surface needs `task-116`; `task-122`
needs `task-121`). `summary.criticalPath` is guidance and may lag. The `standing` phase
(task-158) is event-driven: it fires only on a cardano-wallet pin bump, never as a scheduled
step.

### Per-slice planning (once per slice)

1. **Plan.** On entering a slice, the planning inputs are `.agent/readme.md`,
   `.agent/system/architecture.md`, the relevant workflow docs, the plan sections and design
   docs the slice implements, the matching research findings, and the live files. The outputs
   are `<id>-PRD.md` (classifying every task's interaction mode),
   `<id>-implementation-guide.md` written to the small-model bar, and a `Planner:` entry
   appended to `<id>-code-review.md`.
   **Planner decomposition (default):** a single planner that both ingests the corpus and
   authors the PRD + guide overruns its context even on a one-task slice — its discretionary
   live-code reads and its own authored output scale with the work, so size estimates made
   before launch systematically undershoot. First have a scout agent distill the corpus into
   a **grounding brief** (a compact, line-anchored digest of the plan/design/research
   sections and the verified live-code seams the slice touches), then run planning as two
   agents at the deliverable seam: a **PRD/decisions author** (brief, tasks JSON, planning
   rules, precedent PRD; resolves the slice decisions and writes the PRD) followed by a
   **guide author** (the drafted PRD, precedent guide, plus only the live files it pins
   anchors into; writes the guide and the `Planner:` entry). Feed precedent docs as structure
   skeletons, not full reads, when matching structure/depth is the only need.
2. **Critique (subagent — REQUIRED).** Dispatch a reviewer subagent for **one broad pass** over
   the PRD + guide: task/plan/design coverage, consistency with the tasks JSON + locked
   invariants, hidden manual checkpoints, missing tests/docs, and **whether the guide is concise
   and small-model-implementable**. It returns a consolidated blocker list +
   `Decision: approved | requires_changes`, appended as a `Critiquer:` entry. One critique pass
   + at most one fix pass (a single agent addressing every blocker across both docs), then
   build.

### Per-task build loop

3. **Interaction gate.** If the task is `interactive_decision`, stop and ask the user the
   minimum blocking question before building. The locked non-autonomous set is: **task-125**
   (`manual_execution` — release verification on a synced node, including real-device HW QA),
   **task-166 remainder** (`manual_execution` — mainnet fixture capture + p50/p95 latency
   measurement), **task-158** (event-driven standing guardrail), and the **release-end `!!!`
   copy review** (user-owned). Never relabel these autonomous. Everything else is `autonomous`
   unless slice planning surfaces a genuine blocking decision.
4. **Implement.** Execute the approved guide via subagents; keep each task's focused tests
   with it. The gate before code review: `yarn compile` + `yarn lint` for the touched
   surfaces, the task's focused Jest, and `yarn i18n:manage` whenever copy changed — all
   green.
   **Implementer decomposition (default):** the guide is self-contained by construction — an
   implementer reads the guide and the files it edits, not the PRD or the planning corpus (put
   any scope/non-goals the implementer needs into the guide itself). When the guide spans
   multiple surfaces (storage, store, UI/routes, i18n, tests/stories), chain implementers over
   contiguous guide step ranges — the guide's ordered steps are the sharding seams; each fresh
   agent inherits the prior ranges' edits in the working tree and reads only its own step
   range. The verification gate above runs once, in a dedicated verifier agent after the last
   range — not inside an implementer (bulky output landing on an exhausted context) and not
   the step-5 reviewer; failures route to a fixer agent, not back to a spent implementer.
5. **Code review (subagent — REQUIRED).** Dispatch a reviewer subagent for one broad pass over
   the diff vs the approved guide (correctness, locked-invariant regressions, sanitization-floor
   regressions, IPC/contract drift, missing tests, doc drift, unnecessary complexity). It
   returns blockers + `Decision`, appended as a `Code Review:` entry. Loop until approved (cap 5
   iterations; escalate to the user if not clean).
6. **Document (Scribe).** Update the tasks JSON: `status`, a truthful `statusReason`,
   `evidence` (file paths), and `updatedAt`. Record durable findings in `<id>-findings.md` when
   they exist.
7. **Commit (one per task).** Run `nix fmt` first. Create exactly **one** commit per task with
   `git-commit-formatter`: `<type>(gov): task-NNN <short imperative summary>` — subject only, no
   body, no trailers. Commit only task-relevant files (the slice's doc/tracker updates ride with
   the task that produced them). The task is not done until the commit exists.

At slice close, fill the PRD's final outcome, refresh the phase's `auditSummary` in the tasks
JSON if one exists, and append a closing `Planner:` entry summarizing the slice.

### Status rule (tracker vocabulary)

- `in_progress` while building; `partial` / `blocked` / `deferred` only with a `statusReason`
  naming the gap, blocker, or deferral target.
- **`complete`** when implementation + focused in-task tests + code review are clean.
- **`verified` requires dedicated proof beyond the task's own unit tests**: the slice's
  in-slice verification task (e.g. task-114), a targeted regression suite (as task-109/110/111
  earned via the sanitization spy suite), or manual release verification (task-125). Never
  promote to `verified` on implementation evidence alone.

### Convergence

Prefer the **smallest truthful change** that satisfies the task, the plan decision it
implements, and the locked invariants. Reuse existing seams over new abstractions: the
`RendererIpcChannel` pattern, `GovernanceQueryService` + `governanceChannel`, the `_shared`
governance components (`DRepIdDisplay`, `DRepSourceLabel`, `DRepStatusBadge`), the existing
`delegateVotes` request and `VotingStore` signing flow, and the existing bech32 helpers (no new
bech32 dependency). Critique and review push toward simplification, not iteration count.

---

## Stop conditions (ask the user)

- An `interactive_decision` task's blocking question.
- The locked manual items: task-125 release verification, task-166 mainnet fixture + latency
  capture, a cardano-wallet pin bump tripping task-158, and the release-end `!!!` copy review.
- Planning or build max-iteration guard tripped.
- A destructive/irreversible action, or a material tradeoff the plan/research does not already
  resolve (e.g. the anchor ~1 MB cap vs inline `imageObject` risk in the plan's risk table, if
  anchor-1 planning cannot resolve it from realistic CIP-119 payloads).

When pausing for user input, persist the handoff in the slice docs and stop; do not auto-advance
or speculate. A pause is a valid in-progress state, not a failure.

---

## Definition of done

**Per task:** acceptance criteria met · verification executed and reported · code review clean
(or user-approved escalation) · tasks JSON synchronized (`status`, `statusReason`, `evidence`,
`updatedAt`) · exactly one task commit created.

**Per slice:** all tasks at `complete` or better (or explicitly `deferred`/`blocked` with
reasons) · slice PRD final outcome written · code-review log preserved · research note written
or `no new research` recorded · `auditSummary` refreshed where present · the inherited
sanitization-floor assertion green.
