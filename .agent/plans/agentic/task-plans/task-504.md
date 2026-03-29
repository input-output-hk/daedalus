# Task Plan: task-504 Add search quality fixtures

- Task ID: `task-504`
- Title: `Add search quality fixtures`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-504` is the next unblocked pending task in `.agent/plans/agentic/knowledge-base-platform-tasks.json` for the search stack: its only declared dependencies, `task-502` and `task-503`, are completed.
- This task closes the main remaining gap between shipped search behavior and later pending automation and rollout work. The repo now has real hybrid search, a packaged local CLI, snapshots, sync, freshness reporting, and MCP wiring, but it still lacks a durable regression set for ranking quality.
- The task sits ahead of later pending automation and rollout items such as `task-703`, `task-704`, `task-902`, and `task-903`; those follow-up tasks are safer once search quality checks have a stable fixture corpus instead of ad hoc queries.
- The current repo has enough settled search surface to plan against real contracts rather than assumptions: `task-502` fixed BM25/vector/RRF behavior and `task-503` fixed the local `search` command and JSON output shape.

## Scope

- Add a canonical search-quality fixture file at `agentic/config/search-fixtures.yaml` that captures a small regression set of known queries and expected ranking outcomes.
- Add a narrow test-oriented loader and validation boundary for those fixtures so tests can fail clearly when a fixture is malformed.
- Add deterministic automated verification that executes the fixtures against a seeded isolated KB database and checks ranking quality using the existing packaged search stack, with no live Ollama dependency.
- Keep the fixture set focused on durable ranking expectations for representative Daedalus KB queries rather than exhaustive search-engine benchmarking.
- Reuse the current packaged search/query and CLI contracts instead of introducing a second search implementation for tests.
- Add a separate regression layer on top of existing query and CLI correctness tests rather than replacing or duplicating those lower-level suites.

## Non-Goals

- Do not redesign BM25, vector, or RRF ranking; `task-502` already settled those contracts.
- Do not add a new search engine, alternate scoring model, or broad query-language expansion.
- Do not move SQL-shape, filter-validation, search-request validation, or general CLI-contract coverage out of the existing `task-502` and `task-503` suites.
- Do not broaden this task into MCP behavior, sync orchestration, freshness detection, or rollout workflow work beyond what fixture verification strictly needs.
- Do not turn this into a large corpus-ingestion or snapshot-validation task; fixture coverage should stay deterministic and small.
- Do not add broad operator-facing workflow or README updates unless implementation introduces a durable new verification command that must be documented.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-204` - established the registry-backed searchable entity and filter vocabulary in `agentic/src/agentic_kb/search/config.py`
  - `task-301`, `task-402`, `task-403`, `task-404` - established the current searchable docs, code, GitHub, and project-item shapes that the fixture set should represent
  - `task-501` - established the embedding client and deterministic `384`-dimension contract
  - `task-502` - implemented the packaged BM25/vector/hybrid search layer in `agentic/src/agentic_kb/search/query.py`
  - `task-503` - implemented the packaged local `search` CLI and stable JSON/text output shape in `agentic/src/agentic_kb/commands/search.py`
- Immediate downstream work made safer by this task:
  - `task-703` and `task-704` - scheduled refresh and incremental smoke checks should rely on a real ranking-regression corpus instead of one-off search strings
  - `task-902` and `task-903` - security review and multi-developer pilot work benefit from a repeatable way to prove search quality has not regressed
- Research and plan inputs reviewed for this task:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-502-search-queries.md`
  - `.agent/plans/agentic/research/task-503-search-cli-and-status-commands.md`
  - `.agent/plans/agentic/task-plans/task-502.md`
  - `.agent/plans/agentic/task-plans/task-503.md`
  - `agentic/src/agentic_kb/search/query.py`
  - `agentic/src/agentic_kb/search/config.py`
  - `agentic/src/agentic_kb/commands/search.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/tests/test_search_query.py`
  - `agentic/tests/test_search_query_db.py`
  - `agentic/tests/test_search_command.py`
  - `agentic/tests/test_search_command_db.py`

## Tracker And Repo Drift To Reconcile

- The tracker target path was already correct for `task-504`, and implementation resolved the main repo drift by creating the canonical fixture source at `agentic/config/search-fixtures.yaml` instead of scattering ranking-regression expectations across multiple test files.
- Before this task, the repo verified search behavior only through ad hoc unit and DB-backed tests. The new fixture corpus closes that gap by providing one reusable ranking-regression source of truth.
- `agentic/pyproject.toml` previously packaged only `config/*.json` and had no explicit YAML dependency. Task-504 kept fixture loading test-local and added explicit `PyYAML` dependency coverage for deterministic fixture parsing in isolated environments.
- `.agent/workflows/agentic-kb.md` still documents the bootstrap proof query rather than a user-facing ranking-regression workflow. That remains acceptable because task-504 did not add a new operator-facing verification command.

## Files Expected To Change

- `agentic/config/search-fixtures.yaml` - canonical regression set of known search queries and expected ranking outcomes.
- `agentic/tests/search_quality_helpers.py` - likely small shared test-only helper module for fixture loading, validation, deterministic seeded corpora, and injected fixed embeddings.
- `agentic/tests/test_search_quality_fixtures.py` - focused deterministic regression suite that loads the canonical fixture file and validates ranking expectations against a seeded isolated KB.
- `agentic/tests/test_search_query_db.py` - possibly trimmed or refactored only if shared DB bootstrap/seed helpers reduce duplication without moving query-correctness assertions out of this existing suite.
- `agentic/tests/test_search_command_db.py` - possibly extended with one representative BM25 fixture-backed CLI parity check using the existing `search` command.
- `agentic/pyproject.toml` - only if implementation needs an explicit YAML dependency or package-data expansion for fixture loading.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - later mark `task-504` completed and update metadata only if implementation changes the expected target path or fixture-verification contract.
- `.agent/plans/agentic/research/task-504-search-quality-fixtures.md` - capture durable findings about the accepted fixture schema, evaluation boundary, and anti-flake rules.
- `.agent/workflows/agentic-kb.md` - only if task-504 introduces a durable operator-facing fixture verification command that should be documented before later automation work.

## Implementation Approach

- **Canonical fixture source**: create `agentic/config/search-fixtures.yaml` as the single source of truth for search-quality regression cases. Keep the file human-editable and small enough to review like other repo config artifacts.
- **Tight fixture schema**: each fixture should capture only the fields needed for durable ranking checks, such as a fixture id, query text, search mode, optional entity-type or registry-backed filters, optional injected query-embedding key for `vector` or `hybrid`, and one of a small set of expectation forms like `top_hit`, `ordered_prefix`, or `contains_unordered`. Avoid over-designing a full benchmark DSL.
- **Expectation granularity**: prefer low-brittleness assertions. Use `top_hit` for a single strongest expectation, `ordered_prefix` for a short deterministic prefix, and `contains_unordered` when a fixture only needs presence among returned ids. Avoid asserting long exact mixed-entity result lists unless the seeded corpus makes all modality ranks non-tied by construction.
- **Deterministic corpus**: evaluate fixtures against seeded isolated test rows, not the caller's live imported KB. The seeded corpus should be intentionally small, cover representative docs/code/project/GitHub-style entities as needed, and preserve deterministic embeddings and tie-break behavior.
- **Reuse packaged search execution**: the primary evaluation path should call the existing packaged `PostgresSearchStore` and `SearchRequest` contracts from `task-502` so fixtures validate the real ranking layer rather than a test-only reimplementation.
- **No live Ollama in fixtures**: fixture verification for `vector` and `hybrid` must not depend on live Ollama or runtime `OllamaEmbeddingClient` wiring. The test harness should inject fixed `query_embedding` values directly into `PostgresSearchStore.search(...)` or use a fake embedding provider with deterministic vectors. Any CLI-facing fixture parity check should stay BM25-only unless the harness explicitly patches the embedding provider.
- **Regression-layer separation**: keep fixture tests focused on durable known-query ranking regressions only. Leave SQL syntax, filter translation, input validation, deterministic JSON serialization, and generic CLI behavior in `test_search_query.py`, `test_search_query_db.py`, `test_search_command.py`, and `test_search_command_db.py`.
- **Mode and scope coverage**: the fixture set should explicitly include at minimum one BM25 fixture with an entity-scoping filter, one `vector` or `hybrid` fixture that uses injected fixed embeddings, and one cross-entity fixture that uses intentionally minimal ordering assertions rather than a long exact ranked list.
- **CLI parity without new surface area**: do not add a new public CLI verb by default. If CLI parity is valuable, cover it with a narrow DB-backed BM25 test that runs the existing `search` command against at least one fixture-derived case instead of widening the parser surface.
- **Validation boundary**: implement a small loader that rejects malformed fixture records early with clear errors for missing ids, empty query text, unsupported modes, invalid filter shapes, duplicate fixture ids, unsupported expectation keys, or contradictory expectations.
- **Repository-versus-package path**: keep the fixture file repo-local unless implementation finds a strong need for package-bundled access. Because the task target path is outside `src/agentic_kb/`, the clean default is to load the YAML file from the workspace/repo path during tests and verification instead of changing runtime packaging unnecessarily.
- **Smallest helper surface**: do not default to a new packaged module such as `agentic/src/agentic_kb/search/fixtures.py`. Unless a concrete non-test consumer emerges, prefer test-local or config-local helpers under `agentic/tests/` so runtime/package surface stays unchanged.
- **Shared DB helpers**: expect a small shared DB test helper if it materially reduces repeated bootstrap, truncate, seed, and fixed-vector setup across the new fixture suite and any narrow CLI parity coverage. Keep shared helpers test-only rather than turning them into production code.
- **YAML parsing choice**: prefer the smallest maintainable solution. If the final fixture schema needs real YAML parsing, add one explicit dependency rather than writing a brittle ad hoc parser. If a simpler repo-local format is sufficient while still honoring the required `.yaml` contract, keep the loader narrow and well-tested.
- **Scope discipline**: keep this task about fixture data and verification only. Do not use task-504 as a reason to change ranking formulas, add new search filters, or redesign the current CLI/MCP search contracts.

## Acceptance Criteria

- A canonical fixture file exists at `agentic/config/search-fixtures.yaml`.
- The fixture file defines a reviewable regression set of known queries with explicit expected ranking outcomes.
- Fixture records are validated before execution, with clear failures for malformed records or duplicate fixture ids.
- The fixture set is intentionally small and regression-focused, not a broad benchmark framework.
- The fixture suite is clearly separated from existing query-correctness and CLI-correctness tests: fixture-based tests cover durable known-query ranking regressions only, while lower-level SQL/filter/validation/CLI contracts remain in the existing suites.
- The fixture corpus covers representative search behavior that matters to current KB usage, including at least one entity-scoped BM25 fixture, at least one `vector` or `hybrid` fixture, and at least one cross-entity case.
- The verification path executes fixtures against deterministic seeded KB rows rather than relying on whatever data happens to exist in a developer database.
- `vector` and `hybrid` fixture verification does not depend on live Ollama; it uses fixed injected query embeddings or an equivalently deterministic fake embedding provider.
- Fixture evaluation uses the packaged `PostgresSearchStore` and `SearchRequest` contracts from `task-502` rather than reimplementing search logic in tests.
- At least one fixture-backed verification path proves parity with the existing `task-503` local `search` command, either through a DB-backed command test or an equivalently narrow CLI-facing check; this parity coverage may stay BM25-only to avoid accidental live embedding dependencies.
- Per-fixture expectations use low-brittleness assertion forms such as `top_hit`, short `ordered_prefix`, or `contains_unordered`; cross-entity and hybrid fixtures do not require long exact ranked lists unless the seeded corpus guarantees deterministic non-tied ordering.
- Ranking regressions in required top-hit ordering, ordered prefixes, or required contained hits cause automated test failures.
- The implementation does not introduce a new public search CLI verb unless review shows it is strictly needed for fixture verification.
- The implementation does not redesign search ranking, MCP behavior, sync logic, or broad workflow behavior.
- Any packaging or dependency change required to load fixture YAML is explicit, minimal, and covered by tests.

## Verification Plan

- Run `python3 -m py_compile` on any new fixture-loading module and touched tests.
- Add focused tests for fixture parsing and validation, including duplicate ids, invalid modes, empty query text, malformed expectation shapes, and unsupported combinations of expectation keys.
- Add a deterministic DB-backed regression suite, for example `PYTHONPATH=agentic/src AGENTIC_TEST_DATABASE_URL=... python3 -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'`, that:
  - bootstraps an isolated schema
  - seeds a representative deterministic corpus
  - loads `agentic/config/search-fixtures.yaml`
  - executes each fixture through `PostgresSearchStore`
  - injects fixed query embeddings for all `vector` and `hybrid` fixtures
  - asserts the fixture's declared `top_hit`, `ordered_prefix`, or `contains_unordered` contract
- Reuse or lightly refactor existing DB test helpers if that reduces duplicate seed/bootstrap code without widening scope; a small shared helper module under `agentic/tests/` is an expected acceptable outcome.
- Keep vector and hybrid fixture checks deterministic by using fixed embeddings or the existing injected-embedding seam instead of live Ollama.
- Run at least one fixture-backed CLI parity check through the existing `search` command, either by extending `test_search_command_db.py` or by adding a similarly narrow DB-backed command suite; keep this parity check BM25-only unless the harness explicitly patches the embedding provider.
- If implementation adds a package dependency or packaged fixture loader, rebuild and verify the installed image with `docker compose -f docker-compose.agentic.yml build kb-tools` and run the relevant fixture suite inside `kb-tools` against an isolated ParadeDB instance.
- Preserve existing search query and search command suites, rerunning the focused `task-502` and `task-503` coverage if shared helpers or expectations are touched.
- Keep fixture verification distinct from current correctness suites by confirming the existing `test_search_query.py`, `test_search_query_db.py`, `test_search_command.py`, and `test_search_command_db.py` still own lower-level SQL, filter, validation, and CLI-contract assertions after any helper refactor.

## Risks / Open Questions

- **YAML dependency choice**: the repo currently has no general YAML parsing dependency for structured config files. The implementation should keep this minimal and avoid a fragile custom parser.
- **Fixture brittleness**: over-specifying full ranked lists could make fixtures noisy and expensive to maintain. Ordered top-hit or ordered-prefix assertions are the preferred default unless a stronger contract is truly needed.
- **Hybrid cross-entity ties**: the current implementation has deterministic tie-breaks, but mixed-entity hybrid results can still be brittle if the seeded corpus produces near-equal modality ranks. Cross-entity fixtures should therefore prefer minimal assertions unless the corpus is constructed to avoid ties.
- **Coverage balance**: the fixture set should include representative cross-entity behavior without exploding into every possible entity/filter combination already covered by lower-level tests.
- **Path resolution**: because the canonical fixture file lives under `agentic/config/` rather than packaged `src/agentic_kb/config/`, the implementation should make the repo-local loading contract explicit and test it.
- **Helper placement**: a packaged `search/fixtures.py` module is probably unnecessary unless a real non-test consumer appears. The default plan is to keep helpers test-local to avoid accidental runtime/package expansion.
- **Scope creep into automation docs**: later automation work may want a user-facing fixture-check command, but task-504 should not add one unless verification is clearly awkward without it.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan doc during implementation with final planning status, build status, implementation notes, verification notes, and outcome.
- Append planner and critique entries to `.agent/plans/agentic/task-plans/task-504-plan-review.md` until planning reaches an approved critique decision.
- Create `.agent/plans/agentic/task-plans/task-504-impl-review.md` only during implementation, not during planning.
- Add `.agent/plans/agentic/research/task-504-search-quality-fixtures.md` during implementation to capture the accepted fixture schema, evaluation path, and any dependency or anti-flake decisions.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when implementation lands so `task-504` status reflects reality and any fixture-related metadata drift is reconciled.
- Update `.agent/workflows/agentic-kb.md` only if task-504 introduces a durable operator-facing fixture verification command or workflow contract that later tasks should rely on.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-504-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-504-impl-review.md`

## Implementation Notes

- Added the canonical regression fixture file at `agentic/config/search-fixtures.yaml` with three intentionally small fixtures covering a filtered BM25 document case, a deterministic vector code case using injected embeddings, and a cross-entity hybrid case with low-brittleness assertions.
- Kept fixture loading and seeded-corpus helpers test-local in `agentic/tests/search_quality_helpers.py`, including early validation for duplicate ids, unsupported modes, unknown/contradictory expectation shapes, missing or unknown embedding keys, and registry-incompatible filters.
- Added `agentic/tests/test_search_quality_fixtures.py` as the dedicated regression layer and kept lower-level SQL/filter/CLI correctness ownership in the existing search test suites.
- Declared `PyYAML` in `agentic/pyproject.toml` so repo-local fixture loading stays explicit and reproducible in isolated verification environments.
- Fixed the DB-backed reset helper to use `TRUNCATE ... CASCADE`, which is required because the bootstrapped schema includes foreign-key child tables such as `agentic.kb_github_issue_comments` under `agentic.kb_github_issues`.
- Tightened the hybrid cross-entity fixture to the intended entity set and adjusted its low-brittleness expectations to match the actual deterministic seeded ranking produced by the current packaged RRF behavior.

## Verification Notes

- `python3 -m py_compile "agentic/tests/search_quality_helpers.py" "agentic/tests/test_search_quality_fixtures.py" "agentic/tests/test_search_query_db.py"` passed.
- `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'` passed locally, with the DB-backed fixture class skipped in that local invocation because `AGENTIC_TEST_DATABASE_URL` was not set.
- Authoritative DB-backed verification passed in the packaged tool image against an isolated ParadeDB instance with `AGENTIC_DB_PORT=36047 docker compose -p task504-impl2 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'`.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_query.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_command.py'` passed.

## Outcome

- Task-504 implementation landed with the planned minimal runtime surface: no new public CLI verbs, deterministic injected embeddings for non-BM25 fixtures, one BM25 CLI parity path, and a canonical fixture corpus under `agentic/config/`.
- The DB-backed fixture path is now verified against a real isolated ParadeDB schema. The earlier verification gap was not purely environmental: the reset helper needed an FK-safe truncate and the hybrid cross-entity fixture needed one expectation correction after authoritative DB execution.

## Planning Status Rationale

- Planning status is `approved` because `task-504-plan-review.md` reached an approved critique decision.
- Build status is `completed` because the focused local suites passed and the authoritative DB-backed fixture suite passed in the packaged `kb-tools` image against an isolated ParadeDB instance after fixing the FK-safe reset helper and correcting one hybrid fixture expectation.
