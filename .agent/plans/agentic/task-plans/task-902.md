# Task Plan: task-902 Review security and shared-storage boundaries

- Task ID: `task-902`
- Title: `Review security and shared-storage boundaries`
- Interaction Mode: `manual_execution`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-902` is a planned verification gate after the core platform tasks (`task-801`, `task-901`, `task-605`, `task-710`) all completed. The PRD Security and Operational Boundaries section (lines 347-355) defines **seven** explicit constraints, and no task has yet confirmed the implementation respects them all.
- The platform now has a real MCP server (`task-801`), a validated bootstrap path (`task-901`), a selected shared snapshot backend (`task-605`), and documented token-scope failure handling (`task-710`). This is the appropriate moment to audit that the security boundaries intended by the PRD are actually honored before any wider team rollout.
- This task is classified `manual_execution` because it requires human review of security boundaries across multiple code paths and documentation artifacts; it cannot be fully validated by automated test runs alone.

## Scope

- Manually audit the implementation and documentation to confirm that every PRD Security and Operational Boundary constraint from lines 347-355 of `knowledge-base-platform-prd.md` is respected in current repo reality.
- Produce a written disposition for each constraint: either confirmed honored, or a specific gap that needs to be tracked as a follow-on item.
- Record findings in this task plan and in the planning review log.
- Produce per-constraint output in the format: `constraint_id | status | evidence_file:line | gap_note_if_any`

## Non-Goals

- Do not implement any fixes in this planning pass. This task is planning and review only.
- Do not broaden the review beyond the seven explicit PRD boundary constraints.
- Do not add new security boundaries or re-interpret the PRD; only verify what is already stated.
- Do not run automated test suites unless they are directly relevant to verifying one of the seven boundary constraints.

## Relevant Dependencies

Completed prerequisites that this task must verify:

| Task | Title | Completion State |
|------|-------|------------------|
| `task-801` | Implement read-only Search MCP server | Completed - `task-801.md:6` |
| `task-901` | Validate clean-machine bootstrap | Completed - `task-901.md:8` |
| `task-605` | Dropbox shared snapshot backend | Completed - `task-605-dropbox-shared-backend.md` |
| `task-710` | ProjectV2 token scope failure handling | Completed - `task-710.md:6` |

Relevant completed research artifacts:

- `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md` - confirms MCP is read-only with 7 tools
- `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md` - bootstrap contract
- `.agent/plans/agentic/research/task-605-dropbox-shared-backend.md` - Dropbox backend selection, shared folder contract
- `.agent/plans/agentic/research/task-710-projectv2-token-scope-and-failure-handling.md` - token scope requirements

PRD source: `.agent/plans/agentic/knowledge-base-platform-prd.md` lines 347-355.

## Files Expected To Change

- `.agent/plans/agentic/task-plans/task-902.md` - this canonical task plan (update `planning_status` when planning is approved)
- `.agent/plans/agentic/task-plans/task-902-plan-review.md` - append-only planning review log

No implementation changes are expected during planning. If the manual review identifies gaps, they will be recorded as open questions and tracked separately.

## Implementation Approach

### PRD Constraint to Step Mapping

| PRD Constraint | Line | Review Step |
|----------------|------|-------------|
| Keep MCP read-only in v1 | 347 | Step 1 |
| Keep GitHub tokens in env vars / gitignored local env files | 348-349 | Step 2 |
| Do not couple KB credentials or services to wallet runtime | 351 | Step 6 (NEW) |
| Treat snapshots as internal developer artifacts; keep out of git history and Git LFS | 352-353 | Step 3 |
| Snapshot import only targets fresh, isolated, or disposable KB databases | 354 | Step 4 |
| Disposable KB volumes acceptable in v1; in-place schema upgrade not required | 355 | Step 4 |
| Shared baseline uses one canonical embedding contract at a time | 356 | Step 5 |

### Manual Review Steps

Because this task is `manual_execution`, the implementation approach consists of human-led review steps:

**Required Inputs for Human Reviewer**

- Read access to all files listed in Verification Evidence Sources (below)
- Write access to append findings to this task plan and to `.agent/plans/agentic/task-plans/task-902-plan-review.md`
- The reviewer must produce output in the format: `constraint_id | status | evidence_file:line | gap_note_if_any`

**Expected Output Format Per Constraint**

```
C-1 | HONORED | search_server.py:28-45 | no write-capable tools found
C-2 | HONORED | .env.example:3 | GITHUB_TOKEN not present
C-3 | GAP | docker-compose.agentic.yml:?? | KB service reachable from wallet runtime network
...
```

Where `status` is either `HONORED` (constraint is met) or `GAP` (constraint is not met, requires follow-on tracking).

**Step 1: Verify MCP read-only enforcement**

- Read `agentic/src/agentic_kb/mcp/search_server.py` and confirm all 7 tools (`search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, `kb_status`) are read-only.
- Confirm no GitHub write actions, repo writes, sync mutations, or snapshot mutations appear in the MCP layer.
- Confirm the Compose `mcp-search` service runs only the stdio MCP server and exposes no write-capable HTTP endpoints.
- Confirm the MCP server reuses only the read-only seams from `agentic/src/agentic_kb/commands/` and `agentic/src/agentic_kb/search/`.

**Step 2: Verify token handling**

- Confirm `GITHUB_TOKEN` is used only as an environment-variable input to the existing sync/ingest layer and is never hardcoded in source files.
- Confirm there is no `GITHUB_TOKEN` value in any committed source file, config file, or documentation example.
- Confirm `agentic/.env.example` exists and does not contain a real or example token value.
- Confirm `.gitignore` or equivalent gitignore rules exclude `agentic/.env` if it exists locally.

**Step 3: Verify snapshot storage boundaries**

- Confirm shared snapshots are stored in Dropbox under the `Daedalus_KB` shared folder (as defined by `task-605`), not in git history, not in Git LFS, and not in any committed repo path.
- Confirm `agentic/README.md` and `.agent/workflows/agentic-kb.md` label snapshots as internal developer artifacts.
- Confirm no snapshot artifacts (`.dump`, `.manifest.json`) are committed to the repo.

**Step 4: Verify database isolation for snapshot import**

- Confirm `agentic/README.md` documents that snapshot import targets only fresh, isolated, or disposable KB databases.
- Confirm the workflow doc does not describe in-place schema upgrades or manual retrofit procedures for snapshot import.

**Step 5: Verify canonical embedding contract**

- Confirm `agentic/README.md` and `.agent/workflows/agentic-kb.md` specify that the shared baseline uses exactly one canonical embedding contract at a time.
- Confirm no workflow describes multiple concurrent embedding contracts or ad-hoc embedding model swaps.
- Note: The two-developer Dropbox workflow from `task-605` is operational guidance for snapshot artifact treatment (constraint 4: snapshots as internal developer artifacts); it is verified indirectly through Step 3.

**Step 6: Verify KB credentials not coupled to wallet runtime**

- Confirm KB credentials (Dropbox tokens, API keys) are defined only in `agentic/.env` or equivalent agentic-local env files.
- Confirm KB Compose services (`mcp-search`, `agentic-kb`, `chromadb`) cannot be triggered from wallet runtime processes.
- Confirm no wallet runtime `docker-compose.yml` or `Dockerfile` references `agentic` services, KB credentials, or KB network endpoints.
- Confirm KB services run on a separate Docker network (`agentic_kb_network`) not shared with wallet runtime containers.

**Step 7: Verify two-developer snapshot sharing limits**

- Confirm `task-605` shared-folder contract is limited to one shared Dropbox folder between two developers.
- Confirm no workflow documentation describes wider sharing, production access, or public distribution of snapshots.
- Confirm snapshots are labeled as internal developer artifacts in `agentic/README.md` and `.agent/workflows/agentic-kb.md`.

## Acceptance Criteria

- Each of the seven PRD Security and Operational Boundaries constraints (PRD lines 347-356) has a written disposition in this task plan: either `honored` with supporting evidence, or `gap` with a specific tracking note.
- The MCP read-only constraint is confirmed by reviewing `agentic/src/agentic_kb/mcp/search_server.py` and finding no write-capable tool implementations.
- The token handling constraint is confirmed by finding no hardcoded `GITHUB_TOKEN` values in committed source, config, or doc files.
- The KB runtime coupling constraint is confirmed by verifying KB credentials are only in agentic-local env files and KB services run on a separate network not accessible to wallet runtime.
- The snapshot storage constraint is confirmed by verifying shared snapshots are in Dropbox, not in git history or Git LFS, and are labeled as internal developer artifacts.
- The database isolation constraint is confirmed by reading `agentic/README.md` and confirming it labels snapshot import as targeting fresh/disposable KB databases only.
- The canonical embedding contract constraint is confirmed by verifying documentation specifies one canonical embedding contract at a time.
- The two-developer workflow constraint is confirmed by verifying no workflow documentation describes production access or wider sharing than two developers.
- This task plan is updated with `planning_status: approved` once the planning review approves the disposition approach.

## Verification Plan

Manual verification steps are the same as the implementation approach above (Steps 1-7). Each step produces a written finding recorded in this task plan using the per-constraint output format specified above.

Verification evidence sources:

- `agentic/src/agentic_kb/mcp/search_server.py`
- `docker-compose.agentic.yml`
- `agentic/src/agentic_kb/commands/search.py`, `entity.py`, `status.py`, `sync.py`
- `agentic/src/agentic_kb/ingest/github.py`, `project.py`
- `agentic/src/agentic_kb/snapshot_manifest.py`
- `agentic/README.md`
- `.agent/workflows/agentic-kb.md`
- `.gitignore` or equivalent
- `agentic/.env.example`
- `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`
- `.agent/plans/agentic/research/task-605-dropbox-shared-backend.md`

## Risks / Open Questions

- **MCP vector/hybrid fallback behavior**: `task-801` research notes that vector/hybrid search explicitly fails when Ollama is unavailable rather than falling back to BM25 silently. This is intentional for observability, but it is worth confirming this behavior does not create unexpected retry or write-side effects in the MCP layer.
- **Token propagation through Compose**: Confirm `GITHUB_TOKEN` passed via environment is not forwarded to any MCP or KB container that should not have it.
- **Snapshot artifact leakage**: If a developer accidentally commits a `.dump` or `.manifest.json` to the repo, the current `.gitignore` coverage should prevent it; verify the gitignore patterns are sufficient.
- **Workflow doc accuracy for two-developer limits**: Confirm `agentic/README.md` and `.agent/workflows/agentic-kb.md` do not describe any path that implies production wallet runtime access.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for `task-902` planning state, review findings, and final disposition.
- Planning review history lives in `.agent/plans/agentic/task-plans/task-902-plan-review.md`.
- Implementation review history should later live in `.agent/plans/agentic/task-plans/task-902-impl-review.md`, but that file must not be created during this planning pass.
- If the manual review finds no gaps, no research updates are required.
- If the manual review identifies specific gaps, they will be recorded as open questions in this task plan with a note that they require a separate tracking issue.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-902-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-902-impl-review.md`

---

## Revisions (Iteration 2)

- Added explicit PRD constraint-to-step mapping table (7 constraints mapped to Steps 1-7)
- Added "Required Inputs for Human Reviewer" section listing read/write access needs
- Added "Expected Output Format Per Constraint" section with `constraint_id | status | evidence_file:line | gap_note_if_any` format
- Renamed Step 3 to "Verify snapshot storage and artifact boundaries" (constraint 4)
- Added Step 5: "Verify canonical embedding contract" (constraint 7)
- Added Step 6: "Verify KB credentials not coupled to wallet runtime" (constraint 3) - NEW
- Clarified Step 7: "Verify two-developer snapshot sharing limits" - separated from Step 5, explains relationship to constraint 4 (snapshot artifact treatment)
- Updated all references from "five constraints" to "seven constraints"
- Updated Acceptance Criteria to list all 7 constraints explicitly
