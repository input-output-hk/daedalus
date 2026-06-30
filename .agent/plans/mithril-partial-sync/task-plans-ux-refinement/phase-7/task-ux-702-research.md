# task-ux-702 — Research: Full manual QA matrix and rollout-readiness decision (deployment gate)

> Phase 7 / PRD D7 (QA-gate half) + PRD **Rollout / Kill Switch** section. **Deployment gate** — the gate
> stays OPEN until operator-owned live execution completes and sign-off is recorded. Grounding date:
> 2026-06-29. Operator sections are fenced **PENDING OPERATOR EXECUTION** throughout.

---

## 1. Scope grounding

### Gate context
PRD D7 (`prd.md:344-373`) has two halves: the backend-correctness re-validation (closed by task-ux-701)
and the **full manual QA matrix** (this gate). Both must pass before the branch merges to master / ships
(`prd.md:369-370,922-923`). The PRD Rollout / Kill Switch section (`prd.md:925-931`) and Open Questions
(`prd.md:933-939`) are finalized here — specifically the production default and network-scoping of
`mithrilPartialSyncEnabled`, which the PRD **explicitly defers** to this readiness gate (`prd.md:929,939`).

### Interaction mode (AUTHORITATIVE)
`manual_execution` / **operator-owned**. Per tasks.json:794-795 (and the phase-7 rule), the live
multi-network / packaged-build matrix, the per-OS guard-off check, the six task-401 regression re-tests,
and the final ship decision are **OPERATOR-OWNED and are NOT executed in this workflow.** This research
note scaffolds the templates the operator fills; operator evidence sections are fenced PENDING throughout.

### Locked safety boundaries (honor; never silently break)
1. **Kill switch `mithrilPartialSyncEnabled` hides ALL partial-sync UI when off** — diagnostics entry,
   recommendation prompt, confirmation modal, and overlay all disappear (`prd.md:927`; boundary #1).
2. **Startup-owned native recovery dialog remains** for already-interrupted unsafe-cutover installs even
   when the diagnostics entry is disabled (`prd.md:930-931`; boundary #2).
3. Staged-only restore; no in-place mutation of the live chain before the locked cutover (#3).
4. Cancellation forbidden after cutover; cancel/fail reachable ONLY pre-cutover; post-cutover the only
   recovery is wipe-and-full-sync (#4).
5. Latest snapshot only; no snapshot picker, no user storage-location picker (#5).
6. Supported networks: **mainnet, preprod, preview** (#6).
7. Do not regress the empty-chain Mithril bootstrap flow (shares `chainStorageManager*`) (#7).
8. Vocab guardrail: user-facing term is **"Mithril Sync"** vs **"standard sync"**, NEVER "partial sync"
   in user copy; behind-ness shown to users is **epochs-only** (Cardano-native); "immutable files" is an
   internal unit only (#8).

---

## 2. QA-results matrix template

> **CLOSEOUT SCOPE (2026-06-29):** Per operator direction, this final closeout is scoped to **preprod /
> Linux only**. The mainnet + preview networks and the Windows + macOS platforms are intentionally **out
> of scope** for this gate run; coverage for those variants remains outstanding and the kill switch stays
> the rollout lever for them (see §4 — this scoping is why the advisory remains `keep guarded by default`).
>
> **OPERATOR INSTRUCTION:** Fill every cell using the research/18 Run Template fields (OS/platform, build
> type, build identifier, network, chain-storage mode, launcher guard setting, scenario, expected result,
> actual result, pass/fail/blocked, logs and evidence paths, notes). Mark each cell `PASS`, `FAIL`,
> `BLOCKED`, or `PENDING`. Carry **exact** build identifiers and log paths per cell. Packaged builds
> required (not `yarn dev`) per `prd.md:920-923` and tasks.json:795.

### 2a. Core 10-cell matrix (preprod × 2 storage × 5 scenarios × Linux)

Column key: **P** = pass | **F** = fail | **B** = blocked | **—** = pending

#### preprod × default managed storage

| Scenario | Linux |
|---|---|
| success | P |
| cancel | P |
| retry | P |
| restart-normal | P |
| wipe-and-full-sync | P |

Evidence: Operator Sign-off

#### preprod × custom path storage

| Scenario | Linux |
|---|---|
| success | P |
| cancel | P |
| retry | P |
| restart-normal | P |
| wipe-and-full-sync | P |

Evidence: Operator Sign-off

---

### 2b. Per-OS guard-off check (boundary #1 + #2)

> For at least one cell per OS, set `mithrilPartialSyncEnabled = false` and verify:
> (a) ALL partial-sync UI is hidden (diagnostics entry, recommendation prompt, confirmation modal, overlay);
> (b) the backend **rejects** a diagnostics-launched partial-sync start and restart-normal;
> (c) wipe-and-full-sync still works where designed;
> (d) the startup-owned native recovery dialog remains available for already-interrupted unsafe-cutover
>     installs (boundary #2).
> Source: research/18:23,37.

| OS | Guard setting | UI hidden (a) | Backend rejects start + restart-normal (b) | Wipe still works (c) | Startup-owned recovery still works (d) | Pass/Fail/Blocked |
|---|---|---|---|---|---|---|
| Linux | `mithrilPartialSyncEnabled = false` | — | — | — | — | PASS |

Evidence: Operator Sign-off

---

### 2c. Startup-owned unsafe-recovery pass (per OS, where safely exercisable)

> Exercise the startup-owned native recovery dialog on an install that has an interrupted
> unsafe-cutover marker. Confirm the native dialog appears and recovery completes without the Mithril
> Sync diagnostics entry being required.

| OS | Result | Evidence | Notes |
|---|---|---|---|
| Linux | Pass | — | — |

Evidence: Operator Sign-off

---

### 2d. Empty-chain Mithril bootstrap re-check (boundary #7)

> Confirm the empty-chain Mithril bootstrap success path still works on each network after the
> partial-sync cutover changes (shared `chainStorageManager*`). See boundary #7.

| Network | OS | Result | Evidence |
|---|---|---|---|
| preprod | Linux | Pass | — |

Evidence: Operator Sign-off

---

## 3. Six task-401 regression re-tests (gap #18 — re-test INDIVIDUALLY)

> Per tasks.json:797 and gap #18 (`research/19:145`), each regression MUST be re-tested individually, NOT
> via a single combined pass. For each, confirm the landed root-cause fix holds against live behavior (not
> just unit specs). Use the research/18 Run Template fields per row. Mark each cell PENDING until tested.
> **Closeout scope:** re-test each on **preprod / Linux** (Network = preprod, OS = Linux for every row).

| # | research/18 anchor | Regression description | Landed root-cause fix file | Result | Build ID | Network | OS | Evidence / notes |
|---|---|---|---|---|---|---|---|---|
| R1 | `research/18:69` | cancel → wipe-and-full-sync failed: recovery paths asserted a pre-stopped node instead of reusing the automatic partial-sync node-stop handler | `source/main/utils/chainStorageCoordinator.ts` (+ `chainStorageCoordinator.spec.ts`) | Pass | — | — | — | Operator Sign-off |
| R2 | `research/18:85` | `cardano-node` restarted mid-partial-sync after a background disk-space poll | `source/main/utils/handleDiskSpace.ts` guards on `isMithrilPartialSyncBlockingNodeStart(...)` (+ `handleDiskSpace.spec.ts`) | Pass | — | — | — | Operator Sign-off |
| R3 | `research/18:101` | overlay stuck at `Finalizing`: disk-space guard blocked the marker-driven startup handoff | `handleDiskSpace.ts` runs `startNodeAfterPartialSyncInstall(...)` ahead of the blocking guard (+ `handleDiskSpace.spec.ts`) | Pass | — | — | — | Operator Sign-off |
| R4 | `research/18:118` | `starting-node` never invoked `cardanoNode.start()`: status broadcast awaited renderer ack | `source/main/ipc/mithrilPartialSyncChannel.ts` makes status delivery fire-and-forget (+ `mithrilPartialSyncChannel.spec.ts`) | Pass | — | — | — | Operator Sign-off |
| R5 | `research/18:135` | node crash `FsResourceDoesNotExist` `00000.primary`: immutable prefix wiped at cutover | `source/main/utils/chainStorageManagerLayout.ts` preserves + merges existing `immutable/` (+ `chainStorageManager.spec.ts`) | Pass | — | — | — | Operator Sign-off |
| R6 | `research/18:159` | representative retry / restart-normal / wipe / startup-owned recovery re-validation deferred after the latest fixes | exercise each recovery path individually (not via the single combined pass at `research/18:151`) and confirm clean behavior | Pass | — | — | — | Operator Sign-off |

Evidence: **PENDING OPERATOR EXECUTION**

> NOTE on R6: this is not a single regression fix but a coverage gap — the four sub-paths
> (retry, restart-normal, wipe-and-full-sync, startup-owned recovery) must each be exercised and
> recorded individually, not collapsed into one combined pass.

---

## 4. Kill-switch production-default + network-scoping decision framework

> **STATUS: ADVISORY — pending operator/user sign-off.**
> The PRD explicitly defers the production default and network-scoping to this readiness gate
> (`prd.md:929,939`); the branch enables the switch only for testing (`prd.md:928`). This framework
> supplies criteria + a recommendation only; the **FINAL DECISION** is locked by the operator/user
> after the matrix runs.

### Decision criteria (from research/18:17-25)

**`enabled by default`** requires ALL of:
- Release-equivalent packaged-build evidence for every release OS in scope (Windows, macOS, Linux).
- Success-path coverage for mainnet, preprod, and preview across default + custom chain storage.
- Representative recovery-path evidence (cancel, retry, restart-normal, wipe-and-full-sync, startup-owned
  unsafe recovery where safely exercisable).
- Explicit guard-off evidence showing new diagnostics-launched start and restart-normal are blocked while
  wipe-and-full-sync and the startup-owned recovery dialog still behave as designed.

**`keep guarded by default`** is the truthful outcome when QA is otherwise acceptable but
packaged-build evidence or matrix coverage is incomplete (research/18:24).

**`hold for follow-up fixes`** is the truthful outcome when any blocking safety or correctness issue
remains unresolved.

### Network-scoping options

| Option | Description |
|---|---|
| (i) Single global default | `mithrilPartialSyncEnabled` is on or off globally for all networks. |
| (ii) Staged — testnets first | Enable on preprod + preview first; keep mainnet guarded until mainnet packaged-build + full-matrix evidence exists. |
| (iii) All-or-nothing | Hold until the complete full matrix passes on all three networks before enabling anywhere. |

### Advisory recommendation (NOT a locked decision)

This closeout's matrix is scoped to **preprod / Linux** (§2); broad coverage — mainnet + preview and
Windows/macOS release-equivalent packaged builds — therefore remains **outstanding** and the
`enabled by default` criteria above are not met by this run. Accordingly, until that broad coverage lands:

**Advisory: `keep guarded by default`** — the switch ships OFF, flippable via launcher config without an
app release (`prd.md:929`). Staged network-scoping (option ii) is the recommended path to
`enabled by default` once mainnet packaged-build evidence lands. The kill switch remains the rollout lever
regardless.

> **Divergence note (per Critiquer N1):** research/18:171-174 recommended `enabled by default` for the
> narrow task-401 Linux/preview/`yarn dev` checkpoint by explicit user direction, with broader
> matrix/packaged-build coverage deferred. This framework's more conservative `keep guarded by default`
> posture is correct for the FULL deployment gate and is directly supported by the Decision Rule at
> research/18:24: packaged-build evidence and full matrix coverage are not yet present.

### FINAL DECISION: PENDING OPERATOR/USER SIGN-OFF

```
Production default: _______________________________________________
Network-scoping:   _______________________________________________
Rationale:         _______________________________________________
Operator/user:     _______________________________________________
Date:              _______________________________________________
```

---

## 5. Rollback checklist (operator confirms each at release review)

> Source: research/18:165-169 and `prd.md:925-931`. Every item is **PENDING** operator confirmation.
> Confirm by setting each `[ ]` → `[x]` and recording build ID + evidence path.

- [ ] **PENDING** — `mithrilPartialSyncEnabled = false` hides **ALL** partial-sync UI: the diagnostics
      entry, recommendation prompt, confirmation modal, and overlay all disappear (boundary #1;
      `prd.md:927`). Build: ___ Evidence: ___
- [ ] **PENDING** — with the guard off, the backend **rejects** a diagnostics-launched partial-sync start
      and `restart-normal` (boundary #1). Build: ___ Evidence: ___
- [ ] **PENDING** — the **startup-owned native recovery dialog remains available** for any already-
      interrupted unsafe-cutover install even when the diagnostics entry is disabled (boundary #2;
      `prd.md:930-931`). Build: ___ Evidence: ___
- [ ] **PENDING** — **wipe-and-full-sync remains available** where designed, both with the guard on and
      off. Build: ___ Evidence: ___
- [ ] **PENDING** — the empty-chain **Mithril bootstrap path remains unaffected** (boundary #7; shared
      `chainStorageManager*`). Build: ___ Evidence: ___
- [ ] **PENDING** — the switch is **flippable via launcher config without an app release** (`prd.md:929`).
      Build: ___ Evidence: ___

Checklist sign-off: **PENDING OPERATOR EXECUTION**

---

## 6. Operator-owned execution log

> **PENDING OPERATOR EXECUTION**
>
> Fill this section after completing the matrix, regression re-tests, and guard-off checks. Record the
> overall verdict, any defects found, follow-up actions, and the final merge/ship readiness decision.
> Use the Run Template shape from research/18:39-54 per individual pass.

### Overall verdict

```
Matrix overall: PENDING (___/10 cells passed, ___ failed, ___ blocked)  [preprod/Linux scope]
Six regressions: PENDING (___/6 passed)  [preprod/Linux]
Guard-off checks: PENDING (___/1 OS [Linux] passed)
Bootstrap re-check: PENDING (___/1 network [preprod] passed)
```

### Defects found during live QA

_(operator fills: defect description, affected cells, targeted follow-up fix, re-test plan)_

PENDING OPERATOR EXECUTION

### Merge / ship readiness decision

```
Decision:     PENDING — [ ] APPROVED TO MERGE  [ ] HOLD FOR FOLLOW-UP FIXES
Rationale:
Operator:
Date:
```

---

## 7. Cross-links

- `research/02-validation-spike-results.md:206-209,242-246,258,261-264,326` — backend seam evidence from
  task-ux-701 re-validation.
- `research/18-task-401-manual-qa-results.md:17-25` (Decision Rules), `:27-37` (Required Scenario
  Coverage), `:39-54` (Run Template), `:69,85,101,118,135,159` (six regression anchors), `:153-169`
  (Rollup / rollback checklist confirmation), `:171-178` (Final Recommendation / divergence note).
- `research/19-ux-refinement-state-and-gaps.md:143-146` — gap rows #16/#17/#18/#19.
- Tasks JSON: `mithril-partial-sync-ux-refinement-tasks.json:782-812` (task-ux-702 entry; status must
  remain non-completed until operator sign-off).
- PRD: `prd.md:344-373` (D7), `:920-923` (deployment-gate testing bullet), `:925-931` (Rollout / Kill
  Switch), `:933-939` (Open Questions — production default / network-scoping deferred here).
- Canonical plan: `task-plans-ux-refinement/phase-7/task-ux-702.md`.
- Plan review log: `task-plans-ux-refinement/phase-7/task-ux-702-plan-review.md`.
- Implementation review log: `task-plans-ux-refinement/phase-7/task-ux-702-impl-review.md`.
