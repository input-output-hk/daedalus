Planner: Iteration 1
Timestamp: 2026-03-30T18:30:00Z
Outcome: initial_plan_documented

- Drafted the canonical `task-902` plan as a pure manual review gate for the five PRD Security and Operational Boundaries constraints (PRD lines 347-355).
- Set `planning_status: draft` and `build_status: in_progress`; this is a planning-only pass so implementation has not started.
- Marked `interaction_mode: manual_execution` explicitly because the task requires human audit of security boundaries across multiple code and doc artifacts.
- Defined five manual review steps covering: MCP read-only enforcement, token handling, snapshot storage boundaries, database isolation for import, and two-developer workflow limits.
- Listed the five completed dependency tasks (`task-801`, `task-901`, `task-605`, `task-710`) and their research artifacts so the review can build on existing evidence rather than re-auditing implementation.
- Included a risks/open-questions section capturing three observations that the human reviewer should verify explicitly: vector/hybrid failure behavior propagation, token propagation through Compose, and snapshot artifact gitignore coverage.
- Kept the plan focused on verification only; no implementation changes are expected during planning or review.
- Noted that if the manual review identifies gaps, they will be recorded as open questions requiring separate tracking rather than being fixed in this task.

Critiquer: Iteration 1
Timestamp: 2026-03-30T19:15:00Z
Outcome: critique_complete

Critique findings:

1. **Gaps - Scope completeness**: Step 5 ("Verify two-developer workflow limits") conflates two distinct concerns. The PRD constraint "Do not couple KB credentials or services to the wallet runtime" (PRD line 351) is NOT verified by any step. Step 5 maps to the two-developer Dropbox contract from task-605, which is operational guidance, not a named PRD security boundary. The wallet runtime coupling constraint requires a separate verification that KB Compose services cannot be triggered from or co-located with wallet runtime processes.

2. **Complexity - Step scoping**: Step 3 bundles two related constraints (snapshots as internal artifacts + git/LFS exclusion) which is reasonable. Step 4 correctly pairs fresh-DB import with the schema-upgrade prohibition as a single "supported recovery path" concept. Overall step complexity is appropriate for a manual review gate.

3. **Scope creep**: Non-goals (lines 21-26) are well-defined and prevent scope creep. The plan correctly restricts itself to verifying only what the PRD states.

4. **Missing direct verification**: The constraint "Do not couple KB credentials or services to the wallet runtime" (PRD line 351) has no corresponding review step. This is a genuine gap. The human reviewer needs to verify that KB credentials (Dropbox tokens, etc.) are never passed into wallet runtime containers or invoked from wallet processes.

5. **Human checkpoint validation**:
   - Required human inputs: Not explicitly stated. The reviewer needs read access to evidence source files and write access to append findings.
   - Evidence needed back from user: The plan says "produce a written disposition for each constraint" (Scope, line 18) but does not specify the expected output format (e.g., per-constraint honored/gap line with evidence citation).
   - Whether implementation can proceed before user interaction: Implicitly clear (no implementation in this task), but an explicit note would strengthen the plan.

6. **PRD constraint count discrepancy**: The user references "5 PRD boundary constraints (lines 347-355)" but the PRD defines 7 bullet points. The plan's 5 steps cover 6 of 7 PRD constraints with imperfect mapping. Reviewers should be aware that constraint 3 (line 351) lacks direct step coverage.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-30T20:00:00Z
Outcome: revised_plan_addresses_review_feedback

Summary of changes:

1. **Gap - Missing Step (constraint 3)**: Added Step 6 "Verify KB credentials not coupled to wallet runtime" which explicitly verifies that KB credentials are defined only in agentic-local env files, KB services cannot be triggered from wallet runtime, and KB services run on a separate Docker network.

2. **Step 5 conflates concerns (fixed)**: Step 5 is now "Verify canonical embedding contract" (constraint 7: "one canonical embedding contract at a time"). The two-developer workflow limits are now Step 7 "Verify two-developer snapshot sharing limits", with explicit note that they relate to snapshot artifact treatment (constraint 4) rather than being a standalone PRD security boundary.

3. **Human checkpoint underspecified (fixed)**: Added "Required Inputs for Human Reviewer" section (read access to evidence files, write access to findings) and "Expected Output Format Per Constraint" section specifying `constraint_id | status | evidence_file:line | gap_note_if_any` format.

4. **PRD constraint count discrepancy (fixed)**: Added explicit PRD constraint-to-step mapping table showing all 7 bullet points (lines 347-356) mapped to Steps 1-7. Step 4 now covers both constraints 5 and 6 (fresh-DB import + disposable volumes / no schema upgrade requirement).

  5. Updated all plan references from "five constraints" to "seven constraints" and updated Acceptance Criteria to list all 7 constraints explicitly.

Critiquer: Iteration 2
Timestamp: 2026-03-30T21:00:00Z
Outcome: critique_complete

Critique findings:

1. **Gap - Constraint 3 coverage (RESOLVED)**: Step 6 now provides thorough verification of "Do not couple KB credentials or services to wallet runtime" (PRD line 351). It checks: (a) KB credentials only in agentic-local env, (b) KB services unreachable from wallet runtime, (c) no wallet runtime docker-compose references agentic services, (d) KB services on separate Docker network. This is complete.

2. **Step separation (RESOLVED)**: Step 5 ("Verify canonical embedding contract") correctly maps to constraint 7 only. Step 7 ("Verify two-developer snapshot sharing limits") is explicitly scoped as operational guidance verifying constraint 4 (snapshots as internal artifacts), with the note that it is "verified indirectly through Step 3." The conflation from iteration 1 is eliminated.

3. **Human checkpoint specification (RESOLVED)**: Required Inputs section (lines 74-78) explicitly lists read access to evidence files and write access to append findings. Expected Output Format section (lines 80-89) specifies the `constraint_id | status | evidence_file:line | gap_note_if_any` format with a worked example. These are sufficient for a human reviewer to produce the correct output.

4. **No new issues identified**: The constraint-to-step mapping table (lines 60-68) correctly maps all 7 PRD bullet points. Step 4 appropriately pairs two related constraints (fresh-DB import + disposable volumes/no schema upgrade) under a single review step as they represent a single "supported recovery path" concept. Acceptance criteria (lines 137-144) explicitly list all 7 constraints with their verification methods.

Decision: approved
