# Task Plan: task-502 Implement BM25, vector, and RRF search queries

- Task ID: `task-502`
- Title: `Implement BM25, vector, and RRF search queries`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-502` is the next unblocked critical-path task in `.agent/plans/agentic/knowledge-base-platform-tasks.json` after completed upstream work on indexes, registry/filter contracts, embeddings, and the first searchable document/code/GitHub/project ingestion paths.
- The task sits directly on the critical path between data ingestion and the user-facing search surfaces: `task-503` and `task-801` both depend on real query execution rather than placeholder search contracts.
- Although `task-205` is still pending, it is not on the critical path and does not block the search-query layer itself; `task-503` is the first task that needs both `task-205` and `task-502` together.
- The repo now has enough concrete implementation surface to plan against real contracts instead of plan-only assumptions: ParadeDB BM25/HNSW indexes exist, the search registry is packaged, embeddings are standardized at `384`, and searchable rows are already written by docs/code/GitHub/project ingestors.

## Scope

- Add the packaged search-query layer under `agentic/src/agentic_kb/search/` for BM25-only, vector-only, and hybrid RRF search across all currently searchable entity types.
- Reuse the registry/filter contract from `task-204` so query construction stays table-driven rather than hard-coded per entity in CLI or MCP code.
- Reuse the embedding client from `task-501` so semantic search embeds the user query with the same `VECTOR(384)` contract used by ingested rows.
- Return one merged ranked result list that deduplicates hits by `(entity_type, id)` and carries enough per-hit metadata for later CLI and MCP tasks.
- Add deterministic automated coverage, including a real ParadeDB-backed verification path that proves the pinned BM25/vector SQL syntax and RRF behavior against seeded local fixtures.

## Non-Goals

- Do not add CLI commands, terminal output shaping, or `agentic-kb search` UX; that remains `task-503`.
- Do not add MCP handlers, tool schemas, or related-entity APIs; that remains `task-801`.
- Do not redesign the schema, search indexes, or registry/filter vocabulary unless implementation finds a hard blocker that must be surfaced back through planning.
- Do not implement snapshot, sync, staleness, or status behavior beyond what the query layer needs to run against the existing database state.
- Do not broaden filtering into generic JSON-path, label-array, date-range, or metadata-blob querying beyond the typed and prefix filters already accepted in `task-204`.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-203` - settled the stable BM25 and HNSW index contracts in `agentic/schema/create_indexes.sql`
  - `task-204` - settled the packaged entity registry, global/entity filter split, and stable entity vocabulary in `agentic/src/agentic_kb/search/config.py`
  - `task-301` - docs are now present in `agentic.kb_documents` with repo-relative `source_path`, `doc_kind`, previews, and embeddings
  - `task-402` - code coverage now writes `agentic.kb_code_chunks` across the full repository with `repo_path`, `language`, `symbol_kind`, previews, and embeddings
  - `task-403` - GitHub issues, PRs, and comments now populate the GitHub KB tables with searchable text and embeddings
  - `task-404` - project items now populate `agentic.kb_project_items` with typed Project 5 fields and searchable body text
  - `task-405` - established the current packaged Postgres-store patterns and deterministic ParadeDB-backed verification style for query-adjacent library code
  - `task-501` - settled the reusable `OllamaEmbeddingClient` contract and `384`-dimension guardrail
- Direct downstream tasks unblocked by this work:
  - `task-503` - CLI search/status commands need a real packaged query API
  - `task-504` - ranking-quality fixtures need a stable hybrid search contract to exercise
  - `task-801` - the read-only MCP server needs real search execution beneath its tool surface
- Tracker and repo inconsistencies to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-502.targetPath` at the stale pre-package path `agentic/src/search/query.py`; repo reality requires the packaged path `agentic/src/agentic_kb/search/query.py`.
  - The tracker dependency list for `task-502` omits `task-404` even though the current registry and platform plan treat `project_items` as a first-class searchable entity in this task. This does not block planning because `task-404` is already completed, but the mismatch should be corrected so tracker metadata matches actual search scope.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-203-bm25-hnsw-indexes.md`
  - `.agent/plans/agentic/research/task-204-search-config-registry.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-402-repository-code-coverage.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-404-project-ingestion.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`
  - `.agent/plans/agentic/task-plans/task-203.md`
  - `.agent/plans/agentic/task-plans/task-204.md`
  - `.agent/plans/agentic/task-plans/task-301.md`
  - `.agent/plans/agentic/task-plans/task-405.md`
  - `.agent/plans/agentic/task-plans/task-501.md`
  - `agentic/schema/init.sql`
  - `agentic/schema/create_indexes.sql`
  - `agentic/src/agentic_kb/search/config.py`
  - `agentic/src/agentic_kb/search/__init__.py`
  - `agentic/src/agentic_kb/embed/client.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/github.py`
  - `agentic/src/agentic_kb/ingest/project.py`
  - `agentic/src/agentic_kb/sync/state.py`
  - `agentic/tests/test_search_config.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile the stale `task-502.targetPath`, add the missing `task-404` dependency if the implementation keeps `project_items` in search scope, and later mark the task completed.
- `agentic/src/agentic_kb/search/query.py` - packaged BM25/vector/RRF query models, SQL generation, Postgres execution, and result merging.
- `agentic/src/agentic_kb/search/__init__.py` - export the approved search-query surface alongside the existing registry exports.
- `agentic/tests/test_search_query.py` - focused automated coverage for filter validation, per-modality queries, deduplication, and RRF ranking behavior.
- `agentic/tests/test_search_query_db.py` - deterministic ParadeDB-backed query verification against seeded local fixture rows, only if a separate DB-backed suite keeps unit coverage cleaner.
- `.agent/plans/agentic/research/task-502-search-queries.md` - durable findings about the accepted ParadeDB query syntax, vector-distance contract, RRF constants, and any pinned-image gotchas.

## Expected Library Surface

- `agentic/src/agentic_kb/search/query.py` should be the canonical packaged home for search-query logic, replacing the stale tracker path under `agentic/src/search/`.
- Expected public models and helpers:
  - `SearchRequest` - normalized query text, limit, mode, and filter values
  - `SearchMode` - stable vocabulary for `bm25`, `vector`, and `hybrid`
  - `SearchHit` - deduplicated hit record with `entity_type`, `source_domain`, stable row id, result payload, and per-modality rank details
  - `SearchResultSet` - ordered hits plus query metadata useful to later CLI/MCP layers
  - `PostgresSearchStore` or equivalently narrow execution class for real DB queries
  - top-level query helpers such as `search_bm25(...)`, `search_vector(...)`, and `search_hybrid(...)`
- Downstream code in `task-503`, `task-504`, and `task-801` should consume this packaged surface rather than rebuilding SQL or ranking logic locally.

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` before production code lands so `task-502.targetPath` points at `agentic/src/agentic_kb/search/query.py`, and reconcile the missing `task-404` dependency if `project_items` remain part of the implemented search scope.
- **Package placement**: keep the query layer under `agentic/src/agentic_kb/search/` so the registry and execution code live together in the packaged module structure already used by ingest, embed, and sync code.
- **Library-first boundary**: implement search as reusable Python library code. No new CLI parser wiring should land in this task; verification can call the library directly through unit tests or `python -c`.
- **Registry-driven entity selection**: treat `entity_type` as a global registry filter exactly as settled in `task-204`. Query code should first reduce the candidate entity configs from the registry, then build per-entity SQL only for the selected configs.
- **Shared filter translation**: use the existing `SearchFilterConfig` metadata to translate entity filters into SQL predicates. Exact-match filters should become typed parameterized equality predicates, while prefix filters should become parameterized `LIKE prefix%` or the ParadeDB-compatible equivalent. Query code should not infer filter semantics from raw column names.
- **BM25 plus filter contract**: separate text ranking from typed filtering explicitly. BM25 should rank candidate row ids from the table's BM25 index budget only, then join those candidate ids back to the base table and apply all requested entity filters as normal SQL predicates on real columns before final ordering/limit. This is mandatory for supported filters that are not in the BM25 column set, such as `project_items.size`; implementation must not assume every supported filter is BM25-indexed or expressible inside the ParadeDB BM25 term clause itself.
- **Result-shaping contract**: select the configured `result_columns` from each entity config and return them as a per-hit `fields` mapping, together with stable cross-entity metadata such as `entity_type`, `source_domain`, and row `id`. This keeps the search layer generic while giving later CLI and MCP code enough display/link context.
- **BM25 query path**: implement one registry-driven BM25 query per selected entity using the table's durable BM25 index contract from `task-203`. The implementation must verify the exact ParadeDB query syntax on the pinned image instead of assuming upstream examples blindly. For entities with supported filters outside the BM25 budget, the accepted pattern is `BM25-ranked candidate ids -> join back to base table -> apply typed/prefix filters -> order by BM25 rank/score`, not a plan that silently drops or mis-classifies those filters.
- **Vector query path**: embed the user query text once through `OllamaEmbeddingClient`, then run cosine-distance vector queries against each selected entity table's `embedding` column using the `vector_cosine_ops`/HNSW contract already validated in `task-203`. Keep vector-query filtering aligned with the same entity filter translation used by BM25.
- **Hybrid candidate strategy**: for `SearchMode.HYBRID`, fetch bounded BM25 and vector candidate lists per selected entity, assign rank positions within each modality, and over-fetch beyond the final `limit` so the RRF merge is not starved when one modality returns overlapping rows.
- **RRF contract**: compute hybrid ranking with Reciprocal Rank Fusion over rank positions, not raw score normalization. Deduplicate by the stable cross-entity key `(entity_type, id)` and sum modality contributions using a fixed task-local constant captured in tests and research notes. Ties should break deterministically by higher fused score, then better individual modality rank, then stable `(entity_type, id)` ordering.
- **Cross-entity merge boundary**: keep all three modes able to search across multiple entity types at once. The implementation may execute one SQL query per entity per modality and merge in Python rather than forcing a single brittle cross-table SQL statement, as long as the result contract and verification remain deterministic.
- **Validation boundary**: reject unsupported filter keys, invalid entity-type values, non-positive limits, and empty/whitespace-only query text with clear task-local errors before issuing DB or embedding requests.
- **Project-item coverage**: keep `project_items` in scope for all supported modes. The plan assumes `task-404`'s typed Project 5 columns and searchable `body_text` are sufficient for both BM25 and vector search without extra schema work, but implementation is not complete unless DB-backed fixtures prove `project_items` participates in BM25, vector, and hybrid/RRF execution paths, including a filter such as `size` that is supported by the registry but not part of the BM25 index budget.
- **Store pattern reuse**: follow the current packaged Postgres helper pattern from docs/GitHub/project/sync code: narrow `from_database_url(...)`, context-manager support, parameterized SQL, and optional in-memory or fake seams where they materially simplify unit coverage.
- **Deterministic query-embedding seam**: keep the public library able to use `OllamaEmbeddingClient` in production code, but structure execution so tests can inject a fixed query embedding or fake embedding provider. DB-backed verification must use injected deterministic vectors rather than live Ollama calls so pinned-image SQL behavior and container/package parity are proven without widening this task into live embedding-service testing.
- **Testing strategy**: split verification between pure unit tests and required real ParadeDB-backed suites. Unit tests should cover normalization, filter validation, entity selection, deduplication, RRF math, and the special handling for supported filters that are not BM25-indexed. DB-backed tests should seed representative rows into the searchable tables with synthetic `384`-dimension vectors and verify the pinned BM25/vector SQL behavior directly, without requiring GitHub credentials, a fully populated KB, or live Ollama access.

## Acceptance Criteria

- A packaged search-query module exists at `agentic/src/agentic_kb/search/query.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated so `task-502.targetPath` points at the packaged path, and tracker dependencies match the actual implemented search scope.
- The query layer supports three explicit modes: BM25-only, vector-only, and hybrid RRF search.
- The query layer reuses `task-204` registry metadata for entity selection, result columns, source-domain mapping, and filter translation instead of hard-coding per-table rules in multiple places.
- The query layer supports the approved global `entity_type` filter plus the existing entity-specific typed and prefix filters from `task-204`.
- The query layer applies supported filters as real SQL predicates even when those filters are not part of the BM25 indexed-column budget; specifically, a supported filter such as `project_items.size` must remain effective for BM25, vector, and hybrid search without being misrepresented as BM25-indexed.
- Query text validation is explicit and deterministic: empty or whitespace-only search text is rejected before any DB or embedding call.
- BM25 queries execute successfully against the pinned ParadeDB image for the current searchable tables and return ordered hits with stable row identifiers plus result payload fields.
- Vector queries execute successfully against the pinned ParadeDB image using the existing `VECTOR(384)` and cosine-distance contract, with one query embedding generated through `OllamaEmbeddingClient`.
- Hybrid queries deduplicate overlapping BM25/vector hits by `(entity_type, id)` and rank them by Reciprocal Rank Fusion using rank positions rather than raw-score normalization.
- Seeded ParadeDB-backed fixtures prove that `project_items` participates in BM25-only, vector-only, and hybrid/RRF queries rather than only existing in registry configuration.
- Seeded ParadeDB-backed fixtures prove that a non-BM25-budget project filter such as `size` is honored in `project_items` search results across BM25, vector, and hybrid/RRF paths.
- Result objects include enough stable per-hit metadata for later CLI and MCP tasks, including at minimum `entity_type`, `source_domain`, stable row `id`, per-hit result fields, and modality/ranking details.
- Automated tests cover filter validation, registry-driven entity selection, BM25-only search, vector-only search, deduplication, and hybrid RRF ordering.
- Deterministic ParadeDB-backed verification runs both locally and inside `kb-tools` against the pinned image using fixed/injected query embeddings rather than live Ollama calls, proving package/container parity for BM25, vector, and hybrid search behavior.
- The implementation does not add CLI command wiring, MCP endpoints, schema/index migrations, or generic metadata-filter expansion.

## Verification Plan

- Run focused unit coverage for the new query module, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_query.py'`.
- Add deterministic unit tests that verify:
  - supported and unsupported filter-key handling
  - `entity_type` global filtering before per-entity SQL planning
  - prefix-filter translation semantics for `source_path_prefix` and `repo_path_prefix`
  - non-BM25-budget filter handling, especially `project_items.size`, so query planning keeps those filters as SQL predicates rather than assuming they participate in BM25 index matching
  - explicit validation failures for empty query text and invalid limits
  - BM25/vector candidate deduplication by `(entity_type, id)`
  - RRF ordering with overlapping and non-overlapping modality results
  - stable tie-breaking behavior
- Run `python3 -m py_compile` on the new search-query module and touched tests.
- Run `docker compose -f docker-compose.agentic.yml config` to confirm package/layout changes do not break the container contract.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so in-container verification uses the current installed package.
- Run the focused unit suite inside `kb-tools` with `--entrypoint python` because the image keeps `ENTRYPOINT ["agentic-kb"]`.
- Run a deterministic ParadeDB-backed suite against a local database, for example by starting `paradedb`, seeding representative searchable rows with synthetic but schema-valid vectors, and executing the real search helpers against `AGENTIC_TEST_DATABASE_URL`.
- The local DB-backed suite must inject fixed query embeddings or a fake embedding provider so vector and hybrid verification never depends on live Ollama availability.
- Run the same DB-backed suite inside `kb-tools` with `--entrypoint python` and the same fixed/injected query embeddings so package/container parity is proven against the pinned image.
- The DB-backed verification must prove all of the following against the pinned image:
  - accepted BM25 query syntax for the task-203 index layout
  - accepted vector similarity query syntax and ordering for `VECTOR(384)` embeddings
  - filter application on both modalities
  - hybrid RRF merging across at least two entity types with overlapping candidates
- The DB-backed verification must include seeded `project_items` rows and prove all of the following specifically for that entity type:
  - BM25 returns project-item hits from searchable project text
  - vector search returns project-item hits from injected query embeddings
  - hybrid/RRF can merge project-item hits with another entity type without losing deterministic ordering
  - a supported but non-BM25-budget filter such as `size` is honored on `project_items` in BM25, vector, and hybrid modes
- If the DB-backed coverage is split into a dedicated file such as `agentic/tests/test_search_query_db.py`, run that suite locally and in-container as part of task verification.

## Risks / Open Questions

- **Pinned ParadeDB query syntax**: the repo already proved the index DDL, but not yet the exact SELECT/query syntax for BM25 ranking on the pinned image. This must be verified in deterministic DB-backed tests before implementation is considered complete.
- **Candidate-window sizing for hybrid search**: fetching only the final `limit` per modality can under-rank fused results when overlap is high. The implementation should choose and document a modest over-fetch policy rather than leaving this implicit.
- **Filter placement vs ranking behavior**: some supported filters, such as project-item `size`, are typed columns but not part of the BM25 indexed-column budget. The query layer should still treat them as SQL filters without implying they participate in BM25 term ranking.
- **Deterministic vector verification seam**: the task still needs production use of `OllamaEmbeddingClient`, but review should stress-test that the implementation keeps a clean injected-embedding seam for DB-backed tests so verification stays deterministic locally and inside `kb-tools`.
- **Result-shape consistency**: entity configs expose different result columns and preview fields, so the query layer must return a generic but stable hit contract without hiding useful entity-specific context needed by later CLI/MCP tasks.
- **Search-scope tracker drift**: if implementation keeps `project_items` in scope, the tracker must reflect that actual dependency chain so future planning and orchestration do not regress into partial-search assumptions.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan doc during implementation with final planning/build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` to correct the stale `task-502.targetPath`, reconcile task dependencies with actual search scope, and later mark the task completed.
- Add `.agent/plans/agentic/research/task-502-search-queries.md` capturing the accepted ParadeDB BM25 query syntax, vector-query contract, RRF constant and tie-break behavior, and any pinned-image or filter gotchas discovered during implementation.
- Update `.agent/workflows/agentic-kb.md` or `agentic/README.md` only if implementation reveals a durable operator-facing search contract that must be documented before `task-503` or `task-801` lands.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-502-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-502-impl-review.md`

## Implementation Notes

- Added the packaged search-query module at `agentic/src/agentic_kb/search/query.py` with library-first `SearchRequest`, `SearchMode`, `SearchHit`, `SearchResultSet`, and `PostgresSearchStore` contracts plus top-level helpers for BM25, vector, and hybrid search.
- Reused the `task-204` registry for entity selection and filter translation so `entity_type` remains a global registry filter while entity-specific typed and prefix filters compile into parameterized SQL per entity config.
- Implemented BM25 as a two-step ParadeDB query: rank candidate ids from the entity text columns with `@@@` plus `paradedb.score(id)`, then join back to the base table where entity filters apply as normal SQL predicates. This keeps supported non-BM25-budget filters such as `project_items.size` effective in BM25 and hybrid search.
- Implemented vector search with deterministic pgvector cosine-distance ordering via `embedding <=> %s::vector`, requiring an injected or configured `384`-dimension query embedding; hybrid search over-fetches candidates per modality and merges them with Reciprocal Rank Fusion using `k = 60`.
- Expanded `agentic/src/agentic_kb/search/__init__.py` to export the approved search-query surface and added focused tests in `agentic/tests/test_search_query.py` plus deterministic DB-backed coverage in `agentic/tests/test_search_query_db.py`, including seeded `project_items` participation across BM25, vector, and hybrid modes.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` to fix `task-502.targetPath` to the packaged module path and to add the missing completed `task-404` dependency because `project_items` remain in scope for this task.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/search/query.py" "agentic/src/agentic_kb/search/__init__.py" "agentic/tests/test_search_query.py" "agentic/tests/test_search_query_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_query.py'` passed locally.
- Deterministic DB-backed local verification passed against an isolated fresh ParadeDB stack with `AGENTIC_DB_PORT=5758 docker compose -p agentic-task-502 -f docker-compose.agentic.yml up -d paradedb`, then `AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@127.0.0.1:5758/agentic_kb /tmp/task502-venv/bin/python -m unittest discover -s agentic/tests -p 'test_search_query_db.py'` using injected fixed query embeddings instead of live Ollama.
- Live SQL probes against the pinned image confirmed the accepted task-local contracts before the full test suite: BM25 predicates using `<text_column> @@@ %s` plus `paradedb.score(id)` and vector ordering with `embedding <=> %s::vector`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- In-container verification passed with `docker compose -p agentic-task-502 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_search_query.py'` and the matching `test_search_query_db.py` run.

## Outcome

- `task-502` is complete: packaged BM25, vector, and hybrid search queries now exist under `agentic/src/agentic_kb/search/query.py`, deterministic tests cover filter validation and RRF merge behavior, and DB-backed verification proves `project_items` participation plus the non-BM25-budget `size` filter contract.
- The task remains intentionally bounded to the packaged search-query layer plus required tracker and research updates. No CLI search commands, MCP handlers, live Ollama verification, or schema/index changes were added.

## Review Outcome

- The implementation review loop is clean: iteration 2 of `.agent/plans/agentic/task-plans/task-502-impl-review.md` ended with `Decision: approved` after removing task-irrelevant bytecode artifacts from the change set.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-502-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the implementation, verification, tracking updates, research note, and implementation review loop all finished with `Decision: approved`.
