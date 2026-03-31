Planner: Iteration 1
Timestamp: 2026-03-31T08:45:00Z
Outcome: initial_plan_summary

- Created task plan at `.agent/plans/agentic/task-plans/task-904.md` with planning status `draft`, build status `in_progress`, and interaction mode `autonomous`
- The plan synthesizes existing PRD decisions, workflow doc policies, and research findings into one authoritative SOP for canonical baseline ownership, republish triggers, and Developer 2 recovery procedures
- Key sections: (1) Canonical Baseline Publisher — who, where, what, why local-only; (2) Republish Triggers — embedding contract change, content staleness, post-pilot refresh; (3) Developer 2 Recovery — three failure scenarios (stale, missing, incompatible) with distinct recovery paths; (4) Fallback Hierarchy — ordered from fetch latest compatible to full local rebuild; (5) Retention Policy — manual v1 retention with previous-known-good fallback; (6) Ownership Boundaries — designated publisher, temporary handoff, shared cleanup rights
- All four dependencies (task-605, task-606, task-611, task-902) are confirmed completed and their decisions incorporated
- No implementation changes expected; deliverable is one new SOP file in `.agent/SOPs/agentic/canonical-baseline-ownership.md`
- Risks identified: no objective staleness threshold, Dropbox conflict handling, Developer 2 as temporary publisher, no automated staleness detection in v1
- Downstream tasks task-905 and task-906 depend on this plan's completion

---

Critiquer: Iteration 1
Timestamp: 2026-03-31T01:33:09Z
Outcome: requires_changes

- **Write access to Daedalus_KB is correctly captured**: The plan states "Both developers share write access to `Daedalus_KB`" and "either can clean up stale pairs" (line 118), which is consistent with the PRD's "one shared folder that is writable by both developers" (PRD line 269). The policy distinction that both *have* write access but only the designated publisher *should* upload canonical pairs is appropriate and not contradictory.

- **Permanent publisher unavailability is under-specified**: The plan covers Developer 2 as a "temporary publisher" (line 124-125) but does not define a formal handoff procedure for when Developer 1 becomes permanently unavailable. The SOP should include a step for formally transferring the publisher role (e.g., "If Developer 1 is permanently unavailable, Developer 2 assumes the publisher role and documents this in the team coordination channel"). This is a gap for task-905 (two-developer handoff validation).

- **Retention policy lacks operational specificity**: The plan says "remove pairs older than the previous known-good once the new pair is confirmed" (line 117) but does not define what "confirmed" means or who is responsible for initiating cleanup. The PRD and task-605 research both say retention is manual but do not define a cadence. The SOP should recommend a concrete trigger (e.g., "cleanup happens after Developer 2 confirms successful import and sync on the new pair, or within 48 hours of a new publish whichever comes first").

- **Staleness threshold is acknowledged but not actionable**: The plan correctly identifies there is no automated staleness detection (line 75, Risks section), but the recovery path for Scenario A (stale snapshot) says "request Developer 1 republish if the delta is too large" without defining any heuristic for what "too large" means. The SOP should include a practical guideline (e.g., "if sync changed reports >N documents changed or takes >M minutes, consider the baseline stale and request republish").

- **Dropbox conflict handling is noted as a risk but not integrated into procedures**: The Risks section (line 154) mentions Dropbox conflict copies from simultaneous writes, but none of the three recovery scenarios address what to do if conflict copies appear in `Daedalus_KB`. The SOP should add guidance on identifying and resolving conflict copies (e.g., "if files named `* (conflicted copy)*` appear in Daedalus_KB, treat them as stale and do not import; use the canonical pair only").

- **Developer 2 local rebuild without GPU is mentioned but not clearly scoped**: Scenario B recovery (line 92) says "accept slower CPU embedding" as a last resort, but the PRD (line 222) says "CPU-only local operation must still work for import, status checks, BM25 queries, and targeted sync operations, even if full rebuilds are slower." The SOP should clarify that a full local rebuild on CPU is feasible but may take significantly longer, and that import/sync operations remain functional regardless.

- **No contradiction with pilot-failure-modes.md**: The three recovery scenarios (stale, missing, incompatible) do not duplicate the existing failure modes in `pilot-failure-modes.md`. The existing SOP covers operational failures (Dropbox sync delay, non-disposable target, token scope, publish/fetch command failures), while this plan covers ownership and policy-level recovery. The plan correctly references the pilot SOPs without duplicating them.

- **Content refresh vs contract change distinction is correct**: The plan correctly distinguishes between "content refresh" (same contract, routine baseline refresh) and "contract change" (requires republish, old snapshots invalid for import-then-sync). This is consistent with the PRD (lines 257-259) and the workflow doc (lines 173-174).

- **Fallback hierarchy is complete and correctly ordered**: The four-step hierarchy (fetch latest compatible → fetch previous compatible → request republish → full local rebuild) is consistent with the PRD's stated recovery path and the task-605 research finding on outage recovery.

- **Interaction mode `autonomous` is correct**: This is a documentation-only task with no implementation changes. No human checkpoint is hidden inside an autonomous loop. The deliverable is a single SOP file.

- **Missing verification criterion for SOP cross-references**: The acceptance criteria (line 137) says the SOP should reference pilot SOPs "where appropriate without duplicating their content," but there is no verification step to confirm the cross-references are accurate and point to the correct SOP IDs. Add a verification step: "Confirm SOP references to `pilot-two-developer.md`, `pilot-failure-modes.md`, and `pilot-rollback.md` use correct file names and SOP IDs."

Decision: requires_changes

---

Planner: Iteration 2
Timestamp: 2026-03-31T09:15:00Z
Outcome: revised_plan_addresses_review_feedback

- Added permanent publisher role transfer procedure in Ownership Boundaries section: formal step for Developer 2 to assume publisher role permanently if Developer 1 becomes permanently unavailable, including documentation requirement in team coordination channel and a fresh publish within one week
- Made retention policy operational: defined "confirmed" as Developer 2 successfully importing and completing `sync changed` without errors, or 48 hours elapsed since publish — whichever comes first. Specified that the importing developer (typically Developer 2) initiates cleanup, with publisher as alternative after confirming handoff
- Added staleness threshold heuristic for Scenario A: if `sync changed` reports >500 document changes or takes >15 minutes, treat baseline as stale and request republish rather than continuing incremental sync
- Integrated Dropbox conflict handling into all three recovery scenarios: added steps to identify files matching `* (conflicted copy)*` in `Daedalus_KB` and avoid importing them in Scenarios A, B, and C
- Clarified CPU-only rebuild scoping in Scenarios B and C: explicitly stated that per the PRD, CPU-only operation remains fully functional for import, status checks, BM25 queries, and targeted sync; full rebuild on CPU is feasible but significantly slower (embedding generation is the primary bottleneck, expect 3-10x slower throughput)
- Added SOP cross-reference verification step to Verification Plan and Acceptance Criteria: confirms that references to `pilot-two-developer.md`, `pilot-failure-modes.md`, and `pilot-rollback.md` use correct file names and SOP IDs matching actual files in `.agent/SOPs/agentic/`
