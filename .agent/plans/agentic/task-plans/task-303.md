# Task Plan: task-303 Extract structured plan and workflow metadata

- Task ID: `task-303`
- Title: `Extract structured plan and workflow metadata`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-303` is the remaining Phase 3 metadata gap between shipped markdown chunking/idempotent docs ingest (`task-302`, `task-304`) and later search/filter work: current docs rows only expose `doc_kind` and path-level filters even though the allowlisted corpus now includes canonical task plans, review logs, research notes, and workflow docs with stable structured fields.
- Current repo reality is ready for a narrow implementation: `agentic/src/agentic_kb/ingest/docs.py` already centralizes docs row preparation and the existing docs search surfaces in `agentic/src/agentic_kb/search/` plus `agentic/src/agentic_kb/mcp/search_server.py` already provide one reusable docs filter contract.
- Planning this now keeps the task bounded to docs metadata extraction and docs-search wiring without reopening task-302 chunking or task-304 unchanged-doc skip semantics.

## Scope

- Extend allowlisted docs ingestion in `agentic/src/agentic_kb/ingest/docs.py` to extract stable structured metadata for workflow docs and canonical task-plan docs.
- Keep extracted metadata attached to each docs chunk row through the existing `metadata` JSON payload in `agentic.kb_documents`.
- Add the minimum search/filter support needed so the extracted metadata is actually queryable through the existing docs search surface.
- Preserve current docs chunking, ids, hashes, atomic replacement, and sync-state behavior from `task-302` and `task-304`.

## Non-Goals

- Do not broaden into code, GitHub, or project metadata extraction.
- Do not redesign markdown chunking, docs allowlisting, or unchanged-doc skip behavior.
- Do not introduce generic arbitrary JSON-path filtering for all entities; if metadata-backed filters are needed, keep them explicit and docs-scoped.
- Do not parse every `.agent/plans/**` markdown artifact into a new schema. This task should focus on the stable plan/workflow metadata already present in the current corpus.
- Do not create `.agent/plans/agentic/task-plans/task-303-impl-review.md` during planning.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-204` - current search registry and typed filter contract in `agentic/src/agentic_kb/search/config.py`
  - `task-301` - allowlisted docs ingestion in `agentic/src/agentic_kb/ingest/docs.py`
  - `task-302` - markdown chunking and atomic per-path docs replacement
  - `task-304` - pre-embed unchanged-doc skip behavior and explicit docs sync result metadata
- Boundaries to preserve:
  - `task-302` - chunking contract stays unchanged
  - `task-304` - unchanged-doc detection and content-versioned row metadata stay unchanged
  - `task-502` / `task-503` / `task-801` - should consume the resulting filter surface, not be reimplemented here
- Reviewed references:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-302-markdown-chunking.md`
  - `.agent/plans/agentic/research/task-304-idempotent-docs-sync.md`
  - `.agent/plans/agentic/research/task-204-search-config-registry.md`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/search/config.py`
  - `agentic/src/agentic_kb/search/query.py`
  - `agentic/schema/init.sql`
  - `agentic/tests/test_docs_ingest.py`
  - `agentic/tests/test_docs_sync_state.py`
  - `agentic/tests/test_search_config.py`
  - `agentic/tests/test_search_query_db.py`
  - `agentic/tests/test_search_command.py`

## Current Repo State To Reconcile

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-303.targetPath` at stale pre-package path `agentic/src/ingest/doc_metadata.py`, but the current docs ingestor lives in `agentic/src/agentic_kb/ingest/docs.py`.
- The same tracker row marks `task-303.status` as `pending` while also setting `completedAt` to `2026-03-29`, which is internally inconsistent and should be corrected when implementation lands.
- Current docs rows already persist `metadata JSONB`, but the shipped search registry only exposes docs filters `doc_kind` and `source_path_prefix`; richer docs metadata is not yet queryable.
- Current search filter execution in `agentic/src/agentic_kb/search/query.py` assumes entity filters map to simple typed columns, so metadata-backed docs filters will require a narrow extension rather than only new ingestion fields.
- `doc_kind` is already a shipped docs filter; task-303 should preserve it rather than duplicating it under a second name.
- The tracker wording for `task-303` currently says workflow descriptions are captured "as searchable filters", but current repo scope suggests a narrower contract unless implementation deliberately adds and wires a typed description filter through all coupled search surfaces.

## Files Expected To Change

- `agentic/src/agentic_kb/ingest/docs.py` - extract and persist structured workflow/task-plan metadata during docs preparation
- `agentic/src/agentic_kb/search/config.py` - add the approved new filter definitions and any docs result-column additions needed for structured metadata
- `agentic/src/agentic_kb/search/query.py` - add the narrow filter-compilation support needed for explicit docs metadata filters if simple column-backed filters are insufficient
- `agentic/src/agentic_kb/mcp/search_server.py` - update MCP docs/search filter allowlists and tool schemas if docs filter vocabulary changes
- `agentic/tests/test_docs_ingest.py` - focused metadata extraction coverage for workflows and canonical task plans
- `agentic/tests/test_search_config.py` - lock the approved docs filter vocabulary and its docs-only scope
- `agentic/tests/test_search_query_db.py` - DB-backed verification that the new docs filters actually narrow results correctly
- `agentic/tests/test_search_command.py` - only if search request parsing or serialized filter expectations need updates
- `agentic/tests/test_mcp_search_server.py` - verify MCP filter allowlists and docs tool schemas stay aligned if docs filter vocabulary changes
- `agentic/tests/test_mcp_search_server_db.py` - DB-backed MCP verification if docs filter vocabulary changes in the shipped MCP surface
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile `task-303.targetPath` to the packaged path and fix the contradictory `completedAt` value when implementation lands
- `.agent/plans/agentic/research/` - add a durable task-303 research note after implementation
- `.agent/workflows/agentic-kb.md` or `agentic/README.md` - update only if the shipped operator-visible search/filter contract changes enough to make existing docs stale

## Structured Metadata To Extract

- For workflow docs under `.agent/workflows/**/*.md`:
  - extract top-of-file YAML front matter `description` when present
  - store it in docs metadata under one stable key rather than leaving downstream code to scrape raw markdown
  - treat it as extracted metadata and searchable docs context by default, not as a required shipped typed filter for this task
- For canonical task-plan docs matching `.agent/plans/agentic/task-plans/task-*.md` and excluding `*-plan-review.md` plus `*-impl-review.md`:
  - extract `task_id`
  - extract canonical task `title`
  - extract `planning_status`
  - extract `build_status`
  - classify the plan artifact as a canonical task plan so review logs and other plan docs are not conflated with it
- For adjacent plan artifacts under `.agent/plans/**` when classification is trivial from path:
  - preserve a small explicit `plan_type` or equivalent artifact-kind classification only if it materially improves filters for current repo docs without widening into speculative taxonomy work

## Implementation Approach

- Keep the main implementation on the real packaged docs path `agentic/src/agentic_kb/ingest/docs.py`; do not create the stale tracker module `agentic/src/ingest/doc_metadata.py` unless the extraction logic clearly proves too large to keep local.
- Extend `_load_document_payload(...)` with one small docs-metadata extraction helper that merges the current stable metadata keys with any workflow/task-plan fields.
- Keep extraction deterministic and narrow:
  - workflow `description` should come only from leading YAML front matter, not from heuristic scanning deeper in the file
  - canonical task-plan fields should come from the current stable bullet metadata lines near the top of the canonical plan docs
  - review logs and unrelated plan docs should not pretend to have canonical task-plan status fields when they do not
- Preserve chunk behavior from `task-302`: extracted metadata should be attached consistently to every chunk row for the source document, not re-parsed per chunk in inconsistent ways.
- Preserve idempotency behavior from `task-304`: metadata extraction must feed into deterministic chunk drafts so unchanged-doc comparisons still behave correctly when content and extracted metadata are unchanged.
  - legacy rows that predate task-303 and therefore lack the new structured metadata must be treated as changed once so the extracted metadata is backfilled; after that rewrite, unchanged-doc skipping should resume normally when content and extracted metadata still match
- Add the smallest search/filter extension that matches current code reality:
  - keep `doc_kind` as-is
  - add explicit docs filters only for the metadata fields that are both stable and practically useful to query, expected to include `task_id`, `planning_status`, and `build_status`
  - if plan/workflow artifact classification proves necessary for practical filtering, add one explicit docs filter for that classification rather than a generic metadata filter surface
  - if docs filter vocabulary changes, update both the generic search path and the shipped MCP filter allowlists/schemas in the same task so the supported surfaces stay aligned
- For metadata-backed docs filters, prefer one narrow extension to the search registry/query contract over schema churn:
  - allow explicit docs filter configs to compile against safe predefined metadata accessors for `kb_documents.metadata`
  - keep this allowlist static and test-backed
  - do not allow arbitrary caller-supplied JSON paths or freeform SQL expressions
- Reconcile the workflow-description tracker wording explicitly during implementation:
  - preferred narrow outcome: keep workflow `description` as extracted metadata/search context only and update `.agent/plans/agentic/knowledge-base-platform-tasks.json` wording so it does not claim a shipped typed filter
  - only add a typed workflow-description filter if implementation proves it is necessary and the search config, CLI, and MCP surfaces are all updated together
- Keep operator-facing docs updates narrow. Only refresh workflow/README guidance if the CLI-supported search filter set changes materially.

## Acceptance Criteria

- `task-303` implementation lands on current packaged paths, not on stale path `agentic/src/ingest/doc_metadata.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated during implementation so `task-303.targetPath` matches repo reality and the contradictory `completedAt` state is corrected.
- Workflow docs with leading YAML front matter store a stable extracted description field in `kb_documents.metadata`.
- Workflow `description` is explicitly treated one way or the other in the final task-owned artifacts: either it ships as a typed filter across all coupled search surfaces, or the tracker/docs wording is reconciled so it is clearly stored metadata/search context only.
- Canonical task-plan docs store stable extracted task metadata in `kb_documents.metadata`, including `task_id`, `title`, `planning_status`, and `build_status`.
- Review-log docs are not misclassified as canonical task-plan docs solely because they live under the same directory.
- Existing docs metadata keys from `task-301` remain intact unless intentionally superseded by a documented better name.
- Current docs chunking, deterministic ids/hashes, atomic replacement, and unchanged-doc skip behavior continue to work after metadata extraction is added.
  - pre-task-303 docs rows that match by content hash alone but lack the new structured metadata are rewritten once so the metadata/filter surface is actually backfilled
- The search registry exposes the approved new filter vocabulary without reopening generic metadata filtering.
- Search requests can narrow docs results using the approved structured metadata filters without adding cross-entity global filter scope creep.
- If docs filter vocabulary changes, the shipped MCP search tool schemas and allowed filter-key lists are updated to match.
- The implementation stays within task scope and does not introduce code/GitHub/project metadata extraction or a broad JSON-filter engine.

## Verification Plan

- Extend `agentic/tests/test_docs_ingest.py` to cover:
  - workflow front matter description extraction
  - canonical task-plan metadata extraction from the current plan-doc header shape
  - exclusion of `*-plan-review.md` and `*-impl-review.md` from canonical task-plan metadata extraction
  - preservation of existing metadata keys and chunk semantics alongside the new metadata
  - one-time backfill of legacy rows that lack the new structured metadata despite matching content hashes
  - unchanged reruns still skipping when neither content nor extracted metadata changes
- Extend `agentic/tests/test_search_config.py` to cover:
  - the approved docs metadata filter keys and their restricted docs-only scope
  - rejection of unsupported filter expansion beyond the explicit allowlist
- Extend `agentic/tests/test_search_query_db.py` to prove the new docs filters work at the SQL seam against real `kb_documents` rows, including at least one positive and one negative case for canonical task-plan metadata filters.
- Extend `agentic/tests/test_search_command.py` only if the user-facing serialized request/result expectations change.
- Extend `agentic/tests/test_mcp_search_server.py` if docs filter vocabulary changes so `ALL_FILTER_KEYS`, `DOC_FILTER_KEYS`, and the `search` / `search_docs` tool schemas remain aligned with the approved docs filter set.
- Extend `agentic/tests/test_mcp_search_server_db.py` if docs filter vocabulary changes and needs DB-backed confirmation through the shipped MCP surface.
- Run focused local verification:
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_config`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_command`
- Run DB-backed search verification when `AGENTIC_TEST_DATABASE_URL` is available:
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_query_db`
- If metadata-backed filter compilation changes `agentic/src/agentic_kb/search/query.py`, run `python3 -m py_compile` on the touched ingest/search modules and tests.

## Risks / Open Questions

- **Workflow description contract**: the revised plan prefers keeping workflow `description` as extracted metadata/search context only, with tracker wording reconciled during implementation. The critiquer should verify this stays explicit and that the final task artifacts do not silently continue claiming a shipped typed filter if one is not implemented.
- **Docs metadata filter seam**: current search filters are column-backed. The minimal safe extension is an explicit allowlisted metadata-accessor seam for docs only, but the critiquer should verify that this stays narrow and does not accidentally become a generic JSON filter engine.
- **Filter-surface coupling**: any docs filter addition now necessarily touches search config, query behavior, CLI-facing parsing expectations, and MCP filter allowlists/schemas. The critiquer should verify the plan does not understate this coupling or leave one shipped surface stale.
- **Plan taxonomy creep**: current repo reality clearly supports canonical task-plan extraction, but broader plan classification under `.agent/plans/**` can sprawl quickly. The critiquer should push back if proposed `plan_type` values exceed what the current corpus materially needs.
- **Tracker wording drift**: `task-303` mentions `doc kind`, but `doc_kind` is already shipped. The implementation should preserve and reuse that existing field instead of inventing a parallel filter or claiming new coverage for already-completed behavior.

## Required Docs / Tracking / Research Updates

- Update this canonical plan with final implementation notes, verification notes, and outcome after the task is built and reviewed.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` during implementation to reconcile the stale `targetPath`, contradictory `completedAt` value, and workflow-description wording drift for `task-303` if the final implementation keeps workflow descriptions as stored metadata/search context only.
- Add a durable research note under `.agent/plans/agentic/research/` capturing the shipped metadata keys, the canonical task-plan detection rule, the workflow front matter rule, and the final search-filter seam.
- Update `.agent/workflows/agentic-kb.md` or `agentic/README.md` only if the CLI/operator-visible search filter contract changes in a user-relevant way.
- Keep planning review history in `.agent/plans/agentic/task-plans/task-303-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-303-impl-review.md` during this planning pass.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-303-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-303-impl-review.md`

## Implementation Notes

- Implemented the task on the current packaged docs/search paths: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/search/config.py`, `agentic/src/agentic_kb/search/query.py`, and `agentic/src/agentic_kb/mcp/search_server.py`; no stale `agentic/src/ingest/doc_metadata.py` module was created.
- Extended docs ingestion with one deterministic metadata extraction seam that preserves the existing task-301/task-302/task-304 behavior while attaching structured metadata to every chunk row through the existing `metadata` JSON payload.
- Tightened unchanged-doc comparison so legacy rows that match only by content hash but are missing task-303 structured metadata are rewritten once for metadata backfill, while post-backfill rows still skip when both content and extracted metadata are unchanged.
- Workflow docs now extract `workflow_description` only from leading YAML front matter when a simple `description:` scalar is present. That field ships as stored metadata/search context only, not as a typed filter.
- Canonical task-plan docs now persist stable metadata keys `task_id`, `title`, `planning_status`, `build_status`, and `plan_type = canonical_task_plan`, while adjacent `-plan-review.md` and `-impl-review.md` artifacts are classified separately and are not misidentified as canonical task plans.
- Added the smallest docs-only search extension needed for canonical task-plan querying by introducing explicit docs filters for `task_id`, `planning_status`, `build_status`, and `plan_type` through a static metadata-accessor seam; no generic JSON-path filtering was introduced.
- Kept the shipped MCP search surfaces aligned with the new docs filter vocabulary by updating the docs filter allowlist/schema in `agentic/src/agentic_kb/mcp/search_server.py` and its tests.
- Reconciled `task-303` tracking to current repo reality in `.agent/plans/agentic/knowledge-base-platform-tasks.json`, including the packaged target path, final completion status, and narrower workflow-description wording.
- Added the required durable research note in `.agent/plans/agentic/research/task-303-structured-plan-and-workflow-metadata.md`.

## Verification Notes

- Passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest`.
- Passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_config`.
- Passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_command`.
- Passed `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_mcp_search_server`.
- Passed `python3 -m py_compile "agentic/src/agentic_kb/ingest/docs.py" "agentic/src/agentic_kb/search/config.py" "agentic/src/agentic_kb/search/query.py" "agentic/src/agentic_kb/mcp/search_server.py" "agentic/tests/test_docs_ingest.py" "agentic/tests/test_search_config.py" "agentic/tests/test_search_query_db.py" "agentic/tests/test_mcp_search_server.py" "agentic/tests/test_mcp_search_server_db.py"`.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_query_db` and `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_mcp_search_server_db` remain environment-gated by `AGENTIC_TEST_DATABASE_URL`; they were not forced in this host session.

## Outcome

- Status: completed.
- Final outcome: task-303 now stores stable workflow/task-plan metadata in docs rows, exposes explicit docs-only canonical-task-plan filters across the search and MCP surfaces, and keeps workflow descriptions as stored metadata/search context only.
- Scope remained narrow: task-302 chunking and task-304 unchanged-doc skip semantics were preserved, and no generic metadata filter engine or broader cross-entity filter contract was added.

## Planning Status Rationale

- Planning status is `approved` because `.agent/plans/agentic/task-plans/task-303-plan-review.md` ends with `Decision: approved` after the revised plan addressed scope, MCP coupling, and workflow-description contract concerns.
- Build status is `completed` because the implementation, task-owned tracking/research updates, focused verification, and implementation review entry are now present for `task-303`.
- This document is the single source of truth for the final approved plan state, shipped implementation summary, verification posture, and task outcome for `task-303`.
