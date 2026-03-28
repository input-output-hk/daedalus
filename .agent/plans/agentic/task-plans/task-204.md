# Task Plan: task-204 Add search config registry and filters

- Task ID: `task-204`
- Title: `Add search config registry and filters`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-204` is the next unblocked search-foundation task after `task-202` and `task-203`, which established the searchable tables plus the stable BM25 and HNSW index contracts this registry must describe.
- `task-502` depends directly on this task and should not hard-code per-entity table names, searchable columns, or filter semantics in query code when the current repo already has multiple ingestion surfaces with different metadata shapes.
- The repo has now landed real docs, code, GitHub, Project, and sync-state ingestion work, so this is the first point where a reusable packaged registry can be defined against concrete entity contracts instead of plan-only assumptions.

## Scope

- Add a packaged search-configuration registry that describes every currently searchable KB entity type.
- Define the canonical filter set that v1 search may expose for each entity type, aligned with the current schema, ingestors, and the one-BM25-per-table budget settled in `task-203`.
- Expose a reusable library surface that later query, CLI, and MCP tasks can consume without duplicating table metadata or filter rules.
- Encode the mapping between entity types, source domains, storage tables, identifier columns, text columns, vector columns, and supported filter definitions.
- Make the registry explicit about which filters are first-class typed columns versus metadata-only values that are intentionally out of scope for v1 filtering.

## Non-Goals

- Do not implement BM25, vector, or RRF SQL query execution; that remains `task-502`.
- Do not add new schema columns, indexes, tables, or migrations unless implementation finds a hard blocker, which this plan does not expect.
- Do not add search CLI commands, MCP endpoints, or result-rendering UX; those remain `task-503` and `task-801`.
- Do not widen ingestion scope just to surface new filter fields; this task should reflect the current persisted schema and ingestion contracts rather than inventing speculative metadata.
- Do not treat arbitrary JSON `metadata` or `field_values` blobs as generic filter surfaces in v1; only stable documented filters should enter the registry now.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-103` - established the packaged `agentic_kb` Python layout and CLI/package boundaries.
  - `task-201` and `task-202` - created the searchable tables and their typed columns.
  - `task-203` - established the stable BM25/HNSW index contracts and the narrowed searchable/filter column budget.
  - `task-301` - fixed the current docs-ingestion `source_domain`, `doc_kind`, and normalized `source_path` contracts.
  - `task-401` - fixed the current code-ingestion `repo_path`, `language`, `symbol_name`, and `symbol_kind` contracts.
  - `task-403` - fixed the current GitHub repo/state/comment-type/path contracts.
  - `task-404` - fixed the current Project 5 column mapping for `status`, `priority`, `size`, `work_type`, `area`, `phase`, and `kb_impact`.
  - `task-405` - established packaged sync-state patterns and further confirmed the current packaged path convention under `agentic/src/agentic_kb/`.
- Direct downstream tasks unblocked by this work:
  - `task-502` - hybrid search queries should consume the registry instead of open-coding entity/table/filter rules.
  - `task-503` - search CLI/status surfaces can expose validated filter choices from the registry.
  - `task-701` - future sync/status surfaces may reuse the same entity taxonomy when reporting searchable coverage.
  - `task-801` - MCP search tools can align request validation and help text to the registry.
- Tracking mismatch discovered during planning:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-204.targetPath` at `agentic/src/search/config.py`, but current repo reality and prior completed tasks use the packaged layout under `agentic/src/agentic_kb/`. Implementation should reconcile this to `agentic/src/agentic_kb/search/config.py` before production code lands.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-203-bm25-hnsw-indexes.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-401-typescript-symbol-chunking.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-404-project-ingestion.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/task-plans/task-203.md`
  - `.agent/plans/agentic/task-plans/task-403.md`
  - `.agent/plans/agentic/task-plans/task-405.md`
  - `agentic/schema/init.sql`
  - `agentic/schema/create_indexes.sql`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/code.py`
  - `agentic/src/agentic_kb/ingest/github.py`
  - `agentic/src/agentic_kb/ingest/project.py`
  - `agentic/src/agentic_kb/ingest/__init__.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/pyproject.toml`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile the stale `task-204.targetPath` to the packaged module path before implementation code lands, then mark the task completed when done.
- `agentic/src/agentic_kb/search/config.py` - canonical packaged search registry, entity definitions, and filter definitions for all searchable entity types.
- `agentic/src/agentic_kb/search/__init__.py` - export the approved search-config surface for downstream tasks.
- `agentic/src/agentic_kb/ingest/__init__.py` - only if needed to re-export shared enums/constants for downstream callers, but avoid broadening the ingest package unnecessarily.
- `agentic/tests/test_search_config.py` - focused unit coverage for registry completeness, filter contracts, and alignment with current searchable tables.
- `.agent/plans/agentic/research/task-204-search-config-registry.md` - durable implementation findings about the accepted entity taxonomy, filter coverage, and any explicit metadata exclusions.

## Expected Library Surface

- `agentic/src/agentic_kb/search/config.py` should become the canonical packaged home for search registry logic, replacing the stale tracker path under `agentic/src/search/`.
- Expected public dataclasses, enums, or typed constants:
  - `SearchEntityConfig`
  - `SearchFilterConfig`
  - `SearchFilterValueType` or equivalent typed vocabulary for `text`, `integer`, `date`, and similar first-class filter value kinds
  - `SearchEntityType` or equivalent stable entity-name constants covering all current searchable tables
- Expected public helpers:
  - `SEARCH_ENTITY_REGISTRY` or equivalent immutable mapping keyed by entity type
  - `get_search_entity_config(entity_type)`
  - `list_search_entity_configs()`
  - `list_supported_filters(entity_type=None)`
  - optional helpers such as `list_searchable_tables()` or `entity_types_for_source_domain(...)` if they keep later query code simpler without widening scope

## Stable Registry Vocabularies

- **Public entity names are locked in this task**: downstream code and docs should treat these plural table-aligned identifiers as the stable v1 vocabulary unless a later task deliberately migrates them:
  - `documents`
  - `code_chunks`
  - `github_issues`
  - `github_issue_comments`
  - `github_prs`
  - `github_pr_comments`
  - `project_items`
- **Filter categories are explicit** so `task-502` does not have to infer semantics from column names:
  - `global` filters apply before per-entity SQL construction and are not required to map to a physical table column. In v1 this includes `entity_type`.
  - `entity` filters map to per-entity typed columns or approved prefix-match path fields within a specific table config.
  - `derived` filters are allowed only when they are registry-owned aliases over already persisted typed data and have explicit translation rules. The default v1 plan does not require any derived filters beyond the entity-config grouping helpers already described.
- **Source-domain handling**: `source_domain` should be treated as registry metadata and grouping vocabulary, not as a universal SQL filter column. It is a real stored column on `kb_documents`, but for other entities the registry should expose the owning domain through config metadata rather than pretending the same DB column exists everywhere.

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-204.targetPath` matches the packaged module destination `agentic/src/agentic_kb/search/config.py` before production code lands.
- **Package placement**: create a new packaged `agentic_kb.search` module under `agentic/src/agentic_kb/search/` to match the current repo layout used by ingest, sync, and command code rather than reviving the stale un-packaged path from the tracker.
- **Registry ownership**: make the registry the single source of truth for all current searchable entities. `task-502` should read entity/table/index/filter metadata from this module instead of embedding per-table decisions inside query code.
- **Entity taxonomy**: define exactly seven current searchable entity configs aligned to the existing searchable tables:
  - `documents` -> `agentic.kb_documents`
  - `code_chunks` -> `agentic.kb_code_chunks`
  - `github_issues` -> `agentic.kb_github_issues`
  - `github_issue_comments` -> `agentic.kb_github_issue_comments`
  - `github_prs` -> `agentic.kb_github_prs`
  - `github_pr_comments` -> `agentic.kb_github_pr_comments`
  - `project_items` -> `agentic.kb_project_items`
- **Entity-name stability**: lock the plural table-aligned names above as the public v1 registry contract. `task-502`, `task-503`, and `task-801` should consume those exact identifiers instead of inventing shorter aliases.
- **Per-entity config contract**: each entity config should at minimum describe:
  - stable entity type name
  - source domain (`docs`, `code`, `github`, `project`)
  - physical table name
  - primary key column (`id`)
  - canonical text columns used by later search/result shaping
  - embedding column name (`embedding`)
  - the BM25-relevant indexed columns already settled by `task-203`
  - supported typed filters and their backing columns
  - a small set of default select/result columns or preview columns if that materially simplifies `task-502`
 - **Filter discipline**: keep the distinction between registry-owned global filters and per-entity SQL filters explicit. Only entity-level filters should map to stable typed columns or approved prefix-match path fields that already exist in the schema and current ingestion outputs. Do not expose generic JSON-path filters over `metadata`, `heading_path`, `field_values`, or label arrays in this task.
- **V1 filter set**: the registry should support the practical filters the platform plan already commits to, mapped onto current schema reality:
  - registry/global filters: `entity_type`
  - registry/grouping metadata: `source_domain`
  - docs: `doc_kind`, `source_path_prefix`
  - code: `repo_path_prefix`, `language`, `symbol_kind`
  - GitHub parents/comments: `repo`, `state` where applicable, and `comment_type` for PR comments
  - project items: `repo`, `status`, `priority`, `size`, `work_type`, `area`, `phase`, `kb_impact`, `content_type`
- **Path-prefix modeling**: represent `source_path_prefix` and `repo_path_prefix` as explicit filter configs that later query code can translate into `LIKE 'prefix%'` or equivalent SQL rather than pretending they are exact-match scalar fields.
- **Global-vs-entity translation contract**: `task-502` should first narrow candidate entity configs by any global `entity_type` filter, then apply only the selected entity config's column-backed filters when generating SQL. The registry should expose enough metadata for that split without requiring query code to special-case entity names elsewhere.
- **Filter omissions stay explicit**: labels, dates, timestamps, line numbers, author logins, JSON field blobs, and heading hierarchies remain searchable text or metadata today but should not become first-class registry filters in this task unless implementation proves a concrete blocker for `task-502`.
- **Task-203 alignment**: keep the registry aligned with the narrowed BM25 column budget from `task-203`. A filter may exist even if it is not itself a BM25 column, but the config should not imply broader indexed support than the schema/index contract actually provides.
- **Future-query seam**: include enough metadata that `task-502` can build safe BM25/vector/RRF query plans per entity without reverse-engineering table structure, while still leaving query composition, ranking, and SQL generation out of scope here.
- **Immutability and validation**: make registry definitions deterministic and easy to validate in tests, ideally as frozen dataclasses or read-only mappings. Fail loudly if duplicate entity names, duplicate filter keys, or unsupported value types are defined.
- **Search-domain grouping**: keep the registry able to answer both `entity type` questions and higher-level `source domain` grouping questions so future CLI/MCP surfaces can expose domain-oriented search shortcuts without maintaining a parallel mapping.
- **Testing strategy**: add focused tests that verify all seven searchable tables are represented exactly once, filter keys are unique and mapped to real schema columns, path-prefix filters are tagged as prefix filters, and the registry stays consistent with the current packaged ingestion/schema contracts.

## Acceptance Criteria

- A packaged search-config module exists at `agentic/src/agentic_kb/search/config.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated during implementation so `task-204.targetPath` points at `agentic/src/agentic_kb/search/config.py`.
- The registry defines exactly one config for each currently searchable table: documents, code chunks, GitHub issues, GitHub issue comments, GitHub PRs, GitHub PR comments, and project items.
- The registry locks the public entity-name vocabulary to `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.
- Each entity config exposes stable table/column metadata that `task-502` can reuse for BM25/vector/RRF query construction without hard-coding those contracts elsewhere.
- The registry makes the distinction between global registry filters and entity-level SQL filters explicit, with `entity_type` modeled as a registry-level selector rather than a pretend shared table column.
- The registry includes the approved v1 typed filters aligned to current schema reality, including practical support for docs path/doc-kind filters, code path/language/symbol filters, GitHub repo/state/comment-type filters, and Project 5 repo/status/priority/size/work-type/area/phase/kb-impact/content-type filters.
- Path-prefix filters are represented explicitly as prefix-match filter definitions rather than as misleading exact-match scalar filters.
- The implementation does not introduce schema changes, query execution logic, CLI search commands, or generic metadata/JSON filter expansion.
- The registry surface is exported through a packaged import path that later search tasks can consume directly.
- Focused automated tests verify registry completeness, uniqueness, and alignment with the current searchable schema/entity contracts.

## Verification Plan

- Run focused unit coverage for the new registry module, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_config.py'`.
- Add deterministic tests that verify:
  - all seven current searchable entity tables are represented exactly once
  - entity names and table names are unique
  - filter names are unique within each entity config
  - global filters are explicitly marked as non-column registry filters, while entity-level filters point only to real typed schema columns or approved path-prefix fields intended for filtering in v1
  - prefix filters are explicitly marked as prefix/path filters rather than exact-match filters
  - the registry reports the expected source-domain grouping for docs, code, GitHub, and project entities
- Run `python3 -m py_compile` on the new search module and touched test files.
- Run `docker compose -f docker-compose.agentic.yml config` to confirm package/layout changes do not break the container contract.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so the packaged module is installed in-container.
- Run the focused search-config test suite inside `kb-tools` with `--entrypoint python` because the image keeps `ENTRYPOINT ["agentic-kb"]`.
- If implementation adds a tiny validation helper or import surface change, add one import-path test proving `agentic_kb.search` or the chosen packaged module exports the expected registry surface for `task-502`.

## Risks / Open Questions

- **Filter scope creep**: current ingestion stores richer data in `labels`, `metadata`, `field_values`, timestamps, and line-number columns, but exposing those now could over-commit v1 query semantics. The implementation should keep that boundary explicit unless critique identifies a truly blocking omission.
- **Cross-entity filter normalization**: `repo` exists on GitHub and project entities while docs/code use path fields instead. The registry should document this clearly so downstream query code does not assume every filter applies uniformly to every entity type.
- **Future path matching semantics**: this plan assumes prefix-style path filters rather than full glob semantics. If `task-502` later needs globbing or regex-like behavior, that should be introduced deliberately rather than implied by the registry now.

## Required Docs / Tracking / Research Updates

- Update this canonical plan doc during implementation with final planning/build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` to fix the stale `task-204.targetPath` and later mark the task completed.
- Add `.agent/plans/agentic/research/task-204-search-config-registry.md` capturing the accepted entity taxonomy, stable filter vocabulary, explicit exclusions, and any downstream query-design consequences discovered during implementation.
- Update `.agent/workflows/agentic-kb.md` or `agentic/README.md` only if implementation reveals a durable operator-facing search/filter contract that users must know before `task-503` or `task-801` lands.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-204-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-204-impl-review.md`

## Implementation Notes

- Added the packaged registry module at `agentic/src/agentic_kb/search/config.py` with frozen search entity and filter definitions, a locked public entity vocabulary for all seven searchable tables, explicit global-versus-entity filter scopes, and source-domain grouping helpers for downstream query, CLI, and MCP tasks.
- Added `agentic/src/agentic_kb/search/__init__.py` to export the approved search-config surface so later tasks can import registry helpers from a stable packaged path.
- Added `agentic/tests/test_search_config.py` with focused coverage for entity completeness, global filter semantics, prefix-path filter semantics, project-item typed filter coverage including `size`, source-domain grouping, package exports, and BM25-column parity with the settled task-203 index contract.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-204.targetPath` now points at `agentic/src/agentic_kb/search/config.py`, matching the current packaged module layout.
- Kept `size` as a supported typed Project 5 filter while leaving it out of `project_items.bm25_columns`, matching the actual `kb_project_items_bm25_idx` budget established in `agentic/schema/create_indexes.sql`.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/search/config.py" "agentic/src/agentic_kb/search/__init__.py" "agentic/tests/test_search_config.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_config.py'` passed locally.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_search_config.py'` passed.

## Outcome

- `task-204` is complete: the repo now has a packaged, test-backed search registry that defines stable entity configs and supported filters for every currently searchable table without widening into query execution.
- The implementation stays within scope: no schema migrations, search SQL, CLI search commands, MCP endpoints, or generic JSON/metadata filter expansion were introduced.
- The registry now gives `task-502`, `task-503`, and `task-801` one authoritative place to read entity/table/filter contracts while remaining aligned with the actual task-203 BM25/HNSW index definitions.

## Planning Status Rationale

- Planning status remains `approved` because the planning review loop converged with `Decision: approved` in `.agent/plans/agentic/task-plans/task-204-plan-review.md` after the entity vocabulary, filter-scope split, and Project 5 `size` coverage were clarified.
- Build status is `completed` because the implementation, focused tests, tracker reconciliation, research note, and local plus in-container verification are now all in place.
