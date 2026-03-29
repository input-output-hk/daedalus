# Task Plan: task-404 Ingest Project 5 items and field values

- Task ID: `task-404`
- Title: `Ingest Project 5 items and field values`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-404` is the next unblocked critical-path task after `task-403`, which already established the packaged GitHub-ingestion layout, the standard-library HTTP approach, and the reusable embedding/write patterns this task should follow.
- `task-405` depends directly on project-item ingestion before it can persist project cursors or watermarks, and `task-701` needs a real project-ingestion library path before `sync project` can become more than a placeholder.
- Project 5 metadata is also the missing coordination domain promised by the platform plan: issues and pull requests are already ingestible, but the KB still cannot answer project-scoped questions about status, priority, phase, area, or KB impact until this task lands.

## Scope

- Add the first real Project 5 ingestor under the packaged `agentic_kb` module.
- Fetch items from `DripDropz` Project 5 (`Daedalus Maintenance`) and normalize the project-specific field values the platform design already treats as canonical: `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date`.
- Persist searchable project rows into `agentic.kb_project_items` with deterministic ids, project scope metadata, normalized field columns, raw field-value payloads, searchable body text, and embeddings.
- Support linked issue / pull-request project items plus draft-issue items without redesigning the table created in `task-202`.
- Keep the library surface reusable so `task-405` and `task-701` can add cursor persistence and CLI orchestration later without rewriting the ingestion core.

## Non-Goals

- Do not ingest repository issues, pull requests, or comments again; that remains the responsibility of `task-403`.
- Do not persist sync cursors, watermarks, or stale-state tracking; that remains `task-405` and later sync work.
- Do not add a real `agentic-kb sync project` command, search queries, MCP behavior, snapshot behavior, or GitHub Project mutation support.
- Do not shell out to `gh` from `kb-tools`; this task is about read-only project ingestion inside the Python tools image, not operator-side project coordination.
- Do not redesign `agentic.kb_project_items`; this task should fit the table shape already established by `task-202`, including the absence of a persisted `preview_text` column.
- Do not solve deletion or removal detection for items that later leave the project; rerun-safe upserts are enough for this task.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-103` - established the packaged `agentic_kb` module layout, `/workspace` runtime contract, and the fact that `kb-tools` does not ship the `gh` binary.
  - `task-202` - created `agentic.kb_project_items` with the exact column contract this task must satisfy.
  - `task-301` - established the first ingestion/write pattern, deterministic id style, preview shaping, and psycopg write seam.
  - `task-403` - established the accepted GitHub-ingestion approach: standard-library HTTP, reusable page/result contracts, and library-first packaging under `agentic/src/agentic_kb/ingest/`.
  - `task-501` - established `agentic_kb.embed.OllamaEmbeddingClient` and the `VECTOR(384)` contract used by KB entity rows.
- Direct downstream tasks unblocked by this work:
  - `task-405` - project sync-state tracking can wrap this ingestion flow with cursors and watermarks once the fetch contract exists.
  - `task-502` - hybrid search can include project-item entities after real rows exist.
  - `task-701` - `sync project` needs a real project-ingestion library path.
- Tracking and path mismatch to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-404.targetPath` at `agentic/src/ingest/project.py`, but repo reality after `task-103` and `task-403` is the packaged path `agentic/src/agentic_kb/ingest/project.py`.
  - The implementation should treat `agentic/src/agentic_kb/ingest/project.py` as the canonical destination and correct the tasks JSON before production code lands.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `.agent/plans/agentic/task-plans/task-403.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/ingest/__init__.py`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/github.py`
  - `agentic/tests/test_github_ingest.py`
  - `agentic/pyproject.toml`
  - `agentic/Dockerfile`
  - `docker-compose.agentic.yml`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-404.targetPath` to the packaged module path and mark completion when the task lands.
- `agentic/src/agentic_kb/ingest/__init__.py` - export the new project-ingestion surface.
- `agentic/src/agentic_kb/ingest/project.py` - GraphQL client, normalization helpers, embedding flow, and DB write path for Project 5 items.
- `agentic/tests/test_project_ingest.py` - focused unit coverage for GraphQL pagination, field normalization, content-type handling, and rerun safety.
- `.agent/plans/agentic/research/task-404-project-ingestion.md` - durable findings gathered during implementation.
- `.agent/workflows/agentic-kb.md` - only if implementation confirms a durable operator-facing token-scope requirement that should be documented now rather than deferred.

## Expected Library Surface

- `agentic/src/agentic_kb/ingest/project.py` should follow the same library-first pattern already used in `docs.py` and `github.py`.
- Expected public dataclasses / helpers:
  - `PreparedProjectItem`
  - `ProjectFetchBounds`
  - `ProjectItemsPage`
  - `ProjectPageWriteResult`
  - `ProjectIngestResult`
  - deterministic id and body-summary helpers for project items
- Expected public functions:
  - `iter_project_item_pages(...) -> Iterator[ProjectItemsPage]`
  - `write_project_item_page(...) -> ProjectPageWriteResult`
  - `ingest_project_items(...) -> ProjectIngestResult`
  - `ingest_project_items_from_config(...) -> ProjectIngestResult`
- `ProjectFetchBounds` should make the requested bounds explicit enough for later sync work, including at minimum:
  - `project_owner`
  - `project_number`
  - `page_size`
  - optional `max_pages`
  - optional `after_cursor`
- `ProjectIngestResult` should report enough state for later cursor/watermark tasks, including at minimum:
  - project scope used by the run
  - requested bounds
  - pages fetched
  - whether a page bound truncated the run
  - the final cursor / end cursor observed
  - latest project item `updatedAt` observed
  - rows written

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-404.targetPath` points to `agentic/src/agentic_kb/ingest/project.py` before any production code lands.
- **Package placement**: implement the ingestor under `agentic/src/agentic_kb/ingest/` to match the package layout already used by docs, code, and GitHub ingestion.
- **Transport choice**: use the Python standard-library HTTP stack with GitHub GraphQL over HTTPS instead of `gh`, REST-only workarounds, or a new GraphQL client dependency. The repo already proved the standard-library approach in `task-403`, and `kb-tools` does not include the `gh` binary.
- **Auth/runtime note**: this task should continue using `GITHUB_TOKEN` from `AgenticConfig.from_env()`, but unlike `task-403`, the token will likely need project-read scope for organization ProjectV2 access. Treat missing or insufficient scope as a loud ingestion error, not a silent partial success.
- **Project scope contract**: default to organization `DripDropz` and project number `5`, while keeping those values parameterized for tests and future reuse.
- **GraphQL query shape**: fetch ProjectV2 items through the organization project endpoint with cursor pagination on `items(first: ..., after: ...)`. Query enough item data to populate `kb_project_items` in one pass:
  - item node id and `updatedAt`
  - `isArchived`
  - linked content type and core fields for `Issue`, `PullRequest`, and `DraftIssue`
  - project field values with typed fragments for single-select and date fields, plus raw type metadata for any unexpected field-value nodes
- **Field-value boundary**: normalize the canonical project fields named in the platform plan by exact field name: `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date`.
- **Typed-field handling**:
  - single-select fields should populate the dedicated text columns (`status`, `priority`, `size`, `work_type`, `area`, `phase`, `kb_impact`) using the selected option name
  - date fields should populate `start_date` and `target_date`
  - all normalized field values should also be preserved in `field_values` with enough raw detail for later debugging or sync work
  - unrecognized project fields should stay in `field_values` / `metadata` and must not silently overwrite the canonical typed columns
- **Nested field pagination guardrail**: request a generous field-value page size for each item, and if GitHub reports additional field-value pages beyond that bound, fail loudly rather than silently truncating project metadata. The current project conventions only need a small fixed field set, so silent truncation would be a bug.
- **Content-type handling**:
  - linked GitHub issues should map to `content_type = 'issue'`
  - linked pull requests should map to `content_type = 'pull_request'`
  - draft issues should map to `content_type = 'draft_issue'`
  - if GitHub returns a content type outside those cases, preserve the raw type in `metadata`, keep `content_type` descriptive, and still ingest the item if title/body can be formed safely
- **Content-id linkage**: populate `content_id` only when the project item links to a `DripDropz/daedalus` issue or PR, using the deterministic KB ids already established by `task-403` (`deterministic_github_issue_id` / `deterministic_github_pr_id`). For linked content from any other repository, keep `content_id` null and preserve the repo, number, url, and GitHub node id through `repo`, `html_url`, `content_node_id`, and `metadata`. Draft issues should also leave `content_id` null.
- **Body shaping**: build a non-empty searchable `body_text` from the project item title, the linked content or draft body, and a short normalized field-summary block so project metadata such as phase or KB impact remains searchable even before filter-aware search lands.
- **Preview shaping**: if a short preview helper is useful for logs or CLI summaries, derive it in memory from `body_text`; do not persist `preview_text` unless a later task explicitly adds schema support.
- **Title / fallback contract**: prefer the linked content or draft title. If GitHub returns no usable title, fall back to a deterministic item label derived from the project item node id so schema-required text stays non-empty.
- **Repo contract**: persist `repo` when the linked content exposes a repository name, otherwise leave it null for draft-only items.
- **Source freshness contract**: use the project item's own `updatedAt` as the canonical `source_updated_at` because field-value changes are project events even when the underlying issue/PR body has not changed. Content-specific timestamps can stay in `metadata`.
- **Id contract**: use deterministic stable ids derived from project scope plus project item node id, for example `github-project-item:DripDropz/5:<item-node-id>`.
- **Metadata shape**: keep source-specific extras in `metadata` instead of widening schema. Recommended metadata includes at minimum:
  - project title / url if returned by the query
  - `is_archived`
  - raw content typename
  - content repo / url / number when available
  - content timestamps when available
  - raw unmapped field-value details and field node ids when useful for debugging
- **Write semantics**: use a narrow store with `INSERT ... ON CONFLICT (project_item_node_id) DO UPDATE` so reruns refresh rows in place without duplicate-item failures.
- **Deletion boundary**: reruns must refresh changed project rows in place, but detecting items removed from Project 5 or separately pruning archived items remains out of scope for this task.
- **Operator boundary**: keep this implementation library-first. `agentic-kb sync project` remains a placeholder until `task-701`.
- **Error model**: introduce task-local project-ingestion exceptions for missing token, GraphQL errors, malformed payloads, unexpected field-type shapes for the canonical mapped fields, and pagination contract violations.
- **Testing strategy**: mirror the focused task-specific style used by `task-403`:
  - stub GraphQL responses for deterministic unit tests
  - use in-memory stores for normalization and upsert behavior tests
  - reserve live GitHub verification for a bounded smoke pass only if a valid project-readable token is available

## Acceptance Criteria

- A project ingestor exists at `agentic/src/agentic_kb/ingest/project.py` in the current packaged module layout.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated during implementation so `task-404.targetPath` points at `agentic/src/agentic_kb/ingest/project.py`.
- The ingestor can fetch `DripDropz` Project 5 items over GitHub GraphQL using `GITHUB_TOKEN` plus standard HTTPS calls from Python.
- The ingestor writes rows into `agentic.kb_project_items` with deterministic ids, project scope fields, normalized typed project columns, searchable `body_text`, `field_values`, `source_updated_at`, `embedding`, and `metadata` without requiring a schema change.
- Canonical Project 5 fields are populated by exact field name mapping: `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date`.
- Linked `DripDropz/daedalus` issue and pull-request items preserve linkage back to `task-403` entity ids through `content_id`, while linked content from other repositories and draft issues keep `content_id` null and rely on repo/url/node metadata instead.
- Rerunning ingestion updates existing rows in place through the natural-key uniqueness constraint on `project_item_node_id` without duplicate-row failures.
- `ProjectIngestResult` concretely reports requested bounds, pages fetched, whether a bound truncated the run, the final cursor observed, the latest observed project-item update timestamp, and rows written.
- The implementation remains library-first and does not add sync-state persistence, CLI project sync orchestration, search behavior, MCP behavior, or GitHub Project mutations.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the Compose contract still resolves after any package changes.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so verification uses the current installed package.
- Add focused automated coverage in `agentic/tests/test_project_ingest.py` and run it locally and in-container, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_project_ingest.py'` plus the in-container equivalent using `--entrypoint python`.
- Cover at least these automated cases with stubbed GraphQL responses:
  - pagination across multiple project item pages with cursor tracking
  - mapping of the canonical single-select fields into the dedicated text columns
  - mapping of `Start date` and `Target date` into date columns
  - issue-backed, pull-request-backed, and draft-issue-backed project items
  - content-id linkage back to deterministic `task-403` ids only for `DripDropz/daedalus` issues and PRs, plus a non-Daedalus linked-content case that keeps `content_id` null
  - searchable body shaping that includes field-summary text and can derive a stable short preview in memory without persisting `preview_text`
  - rerun upsert behavior against the unique `project_item_node_id` constraint
  - `ProjectFetchBounds`, `ProjectItemsPage`, `ProjectPageWriteResult`, and `ProjectIngestResult` result-contract assertions
  - missing-token, GraphQL-error, malformed-payload, and nested-field-pagination failure paths
- Start an isolated live verification stack only if a valid `GITHUB_TOKEN` with project-read access is available, for example `AGENTIC_DB_PORT=5754 OLLAMA_PORT=11447 docker compose -p agentic-task-404 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools`, then wait for ParadeDB's known first-boot restart handoff before DB checks.
- Run a bounded live smoke command through `--entrypoint python` that ingests a page-limited subset of Project 5 items for `DripDropz` / project `5` into the live DB.
- Verify with SQL that representative rows exist in `agentic.kb_project_items` with non-null embeddings, expected `status` / `priority` / `work_type` style fields, non-empty `field_values`, and stable `project_owner` / `project_number` values.
- Re-run the bounded live smoke ingestion once more and confirm row counts for the same page-limited subset stay stable while `updated_at` refreshes in place.
- Tear down the isolated verification stack with `docker compose -p agentic-task-404 -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- **Token scope boundary**: this task likely needs an environment token with organization project-read access, not just repo-read access. The Critiquer should stress-test whether the plan is explicit enough about that operator contract and how much documentation must change once implementation proves it.
- **GraphQL shape risk**: ProjectV2 field-value unions are more irregular than the repository REST shapes used in `task-403`. The Critiquer should stress-test whether the proposed typed-field handling and failure behavior are strict enough without making the ingestor too brittle.
- **Field-value pagination**: the plan intentionally treats extra field-value pages as a loud failure instead of silent truncation. The Critiquer should confirm that this is the right guardrail for the current fixed field set.
- **Content-id linkage semantics**: this plan now restricts `content_id` to linked `DripDropz/daedalus` issues and PRs so it never points at KB entities the platform does not ingest. The Critiquer should verify that the null-on-non-Daedalus contract is explicit enough for later search and sync work.
- **Project membership churn**: removal from Project 5 and archived-item lifecycle are intentionally out of scope. The Critiquer should confirm that rerun-safe upserts are a sufficient v1 boundary for this task.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome. Until planning is approved and implementation actually begins, keep build status as `in_review` so the status value stays within the orchestrator's allowed vocabulary while the body text continues to note that no build work has started yet.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` before implementation to fix `task-404.targetPath`, then update it again when the task is completed.
- Add a research note under `.agent/plans/agentic/research/` for durable findings such as the accepted GraphQL query contract, token-scope expectations, field normalization rules, and any nested-pagination or item-type caveats discovered during implementation.
- Update `.agent/workflows/agentic-kb.md` only if implementation confirms a durable user-facing requirement for project-readable `GITHUB_TOKEN` scope or other workflow guidance that should no longer stay implicit.

## Implementation Notes

- Corrected `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-404.targetPath` now points at `agentic/src/agentic_kb/ingest/project.py`, and marked `task-404` completed in the tracker.
- Added `agentic/src/agentic_kb/ingest/project.py` with a library-first Project 5 ingestor that fetches ProjectV2 items over GitHub GraphQL using standard-library HTTP plus `GITHUB_TOKEN`.
- Implemented the approved reusable library surface: `PreparedProjectItem`, `ProjectFetchBounds`, `ProjectItemsPage`, `ProjectPageWriteResult`, `ProjectIngestResult`, `iter_project_item_pages`, `write_project_item_page`, `ingest_project_items`, and `ingest_project_items_from_config`.
- Canonical field mapping now normalizes `Status`, `Priority`, `Size`, `Work Type`, `Area`, `Phase`, `KB Impact`, `Start date`, and `Target date` into the dedicated schema columns while preserving all raw field-value details in `field_values`.
- Linked issue and pull-request project items now set `content_id` only for `DripDropz/daedalus` entities using the deterministic ids from `task-403`; draft issues and linked content from other repositories keep `content_id` null and preserve source details in `repo`, `content_node_id`, `html_url`, and `metadata`.
- Searchable `body_text` now combines title, linked content or draft body, and a normalized project-field summary block so project metadata is searchable without persisting a `preview_text` column.
- Added `PostgresProjectItemsStore` and `InMemoryProjectItemsStore` with `ON CONFLICT (project_item_node_id) DO UPDATE` semantics, and exported the new project-ingest surface from `agentic/src/agentic_kb/ingest/__init__.py`.
- Added focused unit coverage in `agentic/tests/test_project_ingest.py` and recorded durable implementation findings in `.agent/plans/agentic/research/task-404-project-ingestion.md`.
- Updated `.agent/workflows/agentic-kb.md` to state the now-confirmed operator contract that project ingestion needs a `GITHUB_TOKEN` with organization ProjectV2 read access, not just repo-read scope.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/ingest/project.py" "agentic/src/agentic_kb/ingest/__init__.py" "agentic/tests/test_project_ingest.py"` passed.
- Local unit tests passed: `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_project_ingest.py'`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- In-container unit tests passed: `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_project_ingest.py'`.
- Live GitHub Project smoke verification remains blocked because `GITHUB_TOKEN` with project-read scope was not available in the current environment.

## Outcome

- `task-404` is implemented within the approved scope as a reusable project-ingestion library under the packaged `agentic_kb` layout.
- No sync cursor persistence, real `sync project` CLI orchestration, search behavior, MCP work, deletion detection, or project mutation behavior was added.
- The only incomplete verification from the original plan is the optional live GitHub/ParadeDB smoke pass, which remains blocked by missing project-readable credentials rather than missing code.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-404-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-404-impl-review.md`

## Planning Status Rationale

- This plan remains `approved` because the implemented work follows the approved schema-fit, linkage, library-surface, and scope-boundary decisions without widening into later sync/search/MCP tasks.
- Build status is now `completed` because the production code, focused tests, tracker/doc/research updates, and local plus in-container verification are in place; only the credential-blocked live smoke pass remains outstanding.
