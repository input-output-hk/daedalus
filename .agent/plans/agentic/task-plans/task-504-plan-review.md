Planner: Iteration 1
Timestamp: 2026-03-29T18:37:50Z
Outcome: initial_plan_documented

- Drafted the canonical `task-504` plan around a single canonical fixture file at `agentic/config/search-fixtures.yaml`, keeping the task focused on known-query ranking regression coverage rather than search-engine redesign.
- Captured why `task-504` is the next unblocked task now: `task-502` and `task-503` are complete, the task remains pending, and later pending automation and rollout work should build on a maintained search-quality regression set instead of ad hoc queries.
- Scoped implementation toward deterministic seeded DB-backed verification using the existing packaged `PostgresSearchStore` and `SearchRequest` contracts, with only a narrow CLI parity check through the existing `search` command rather than adding a new public CLI surface.
- Flagged the main repo drift to reconcile during implementation: no canonical search-fixture file exists yet, known-query expectations are currently scattered across ad hoc tests, and the repo has no general YAML packaging/parsing contract for this new fixture source.

Critiquer: Iteration 1
Timestamp: 2026-03-29T18:40:53Z
Outcome: requires_changes

- The plan is directionally correct, but the deterministic execution contract is still too loose for `vector` and `hybrid` fixtures. In current repo reality, `PostgresSearchStore.search()` will call an embedding provider unless `query_embedding` is supplied directly, and the CLI path wires `OllamaEmbeddingClient.from_config()` from runtime config. The plan should explicitly require fixture evaluation to inject fixed query embeddings for non-BM25 modes and should narrow any CLI parity fixture to BM25 unless the test harness patches the embedding provider. Without that, the task can accidentally depend on live Ollama despite the stated goal.
- The plan does not cleanly separate this new fixture suite from the existing query-correctness tests already living in `agentic/tests/test_search_query.py`, `agentic/tests/test_search_query_db.py`, and `agentic/tests/test_search_command_db.py`. It should state that fixture-based tests are for durable known-query ranking regressions only, while SQL-shape, filter-selection, validation, and CLI contract coverage stay in the existing suites. Otherwise the task risks duplicating current assertions instead of adding a new regression layer.
- Acceptance criteria around ranking expectations are still too broad for cross-entity and hybrid cases. The current search implementation tie-breaks on fused score, best modality rank, per-modality ranks, then stable `(entity_type, id)` ordering, so exact multi-hit ordering across mixed entity types will be brittle unless fixtures are carefully scoped. The plan should require per-fixture assertion granularity such as `top_hit`, `ordered_prefix`, or `contains_unordered`, and should call out that cross-entity fixtures must avoid asserting long exact result lists unless the seeded corpus guarantees non-tied modality ranks.
- The schema discussion mentions optional entity-type and filters, but the acceptance criteria never require at least one entity-scoped fixture and at least one mode-specific expectation set. That gap matters because current search behavior differs materially by `bm25`, `vector`, and `hybrid`, and registry-backed filters prune eligible entity configs before SQL execution. The plan should explicitly cover at least: one BM25 filtered fixture, one vector or hybrid fixture using injected embeddings, and one cross-entity fixture with intentionally minimal ordering assertions.
- `agentic/src/agentic_kb/search/fixtures.py` may be unnecessary scope if fixture loading is only used by tests. Keeping the loader in packaged `src/` would force a clearer repo-root path contract and possibly package-data or dependency changes, while the current task target is just `agentic/config/search-fixtures.yaml`. The plan should either justify a packaged loader with a concrete non-test consumer or prefer a test-local helper to keep runtime/package surface unchanged.
- The repo already has repeated DB bootstrap and seed helpers across DB-backed tests. If task-504 is expected to reuse seeded corpora across `test_search_quality_fixtures.py` and any CLI parity coverage, the plan should mention the likely need for a small shared test helper module rather than only naming edits to existing test files. That is a likely repo change the current file list misses.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T18:41:42Z
Outcome: plan_revised

- Tightened the deterministic execution contract so `vector` and `hybrid` fixture verification must inject fixed query embeddings or use an equivalent fake embedding provider; the plan now forbids live Ollama dependence for fixture runs and limits any CLI parity fixture to BM25 unless the harness explicitly patches embedding behavior.
- Separated the new regression layer from existing correctness suites: the canonical plan now says fixture tests cover durable known-query ranking regressions only, while SQL shape, filter translation, request validation, and generic CLI contract coverage remain in the existing `task-502` and `task-503` test files.
- Reduced brittleness in ranking assertions by replacing broad exact-order expectations with explicit low-brittleness forms such as `top_hit`, short `ordered_prefix`, and `contains_unordered`, and by calling out that cross-entity hybrid fixtures should avoid long exact result lists unless the seeded corpus eliminates tie risk.
- Clarified minimum coverage and helper scope: the plan now requires at least one entity-scoped BM25 fixture, one `vector` or `hybrid` fixture with injected embeddings, one cross-entity fixture with minimal ordering assertions, and it prefers test-local shared helpers under `agentic/tests/` over a new packaged `search/fixtures.py` module unless a non-test consumer emerges.

Critiquer: Iteration 2
Timestamp: 2026-03-29T18:42:49Z
Outcome: approved

- The revised plan addresses the prior determinism gap against current repo reality. It now explicitly forbids live Ollama dependence for fixture verification, requires fixed injected embeddings or an equivalent fake provider for `vector` and `hybrid`, and narrows CLI parity coverage to BM25 unless embedding behavior is patched.
- Scope control is now clear. The plan distinguishes fixture-backed ranking regressions from the existing `test_search_query.py`, `test_search_query_db.py`, `test_search_command.py`, and `test_search_command_db.py` correctness suites, which aligns with the current repo’s already-established SQL/filter/CLI coverage.
- Assertion brittleness is handled appropriately for the current search implementation. The added `top_hit`, short `ordered_prefix`, and `contains_unordered` expectation shapes match the hybrid tie-break realities in `agentic/src/agentic_kb/search/query.py` and avoid overspecifying long mixed-entity rankings.
- Expected file changes and helper placement now better match the repo: a test-local helper module under `agentic/tests/` is called out as the default, and packaged-runtime expansion is treated as optional rather than assumed.
- Verification and documentation/tracker follow-through are realistic. The plan now calls for deterministic DB-backed execution, BM25-only CLI parity by default, preservation of existing focused suites, and the necessary research/tracker updates without forcing unrelated workflow or runtime changes.

Decision: approved
