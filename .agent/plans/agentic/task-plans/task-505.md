# Task Plan: task-505 Add automated search ranking regression gate

- Task ID: `task-505`
- Title: `Add automated search ranking regression gate`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`
- Required User Inputs: `None`
- Required Manual Test Steps: `None for task completion; this task is an automated regression gate`
- Evidence Needed Back From User: `None`
- Implementation Can Proceed Before User Interaction: `Yes`

## Why This Task Was Chosen Now

- `task-505` is the next unblocked critical-path search task in `.agent/plans/agentic/knowledge-base-platform-tasks.json` after `task-504` completed.
- The PRD explicitly promises automated search regression coverage for `BM25`, `vector`, and `RRF` ranking, backed by checked-in query fixtures and enforced as an automated regression gate rather than a manual spot check.
- The repo now has the necessary settled contracts to gate against real behavior instead of planning assumptions: `task-502` fixed packaged BM25/vector/hybrid execution, `task-503` fixed the local CLI and script-facing search surface, and `task-504` created the canonical fixture corpus and deterministic seeded-corpus helpers.
- This is the smallest remaining search-stack contract gap before later automation, rollout, and multi-developer validation tasks assume that ranking quality regressions will fail fast in automated verification.

## Scope

- Turn the checked-in fixture corpus in `agentic/config/search-fixtures.yaml` into an explicit automated ranking-regression gate for the current packaged search stack.
- Add one focused regression suite at `agentic/tests/test_search_regression.py` that is the sole authoritative ranking-regression owner, executes the canonical fixtures against a deterministic seeded KB, and fails on ranking regressions in required expectations.
- Reuse the existing fixture loader, deterministic seeded corpus, and injected query-embedding helpers from `agentic/tests/search_quality_helpers.py` rather than inventing a second fixture format or search harness.
- Make the gate cover the three shipped search modes that matter to current repo behavior: `bm25`, `vector`, and `hybrid`/RRF.
- Move any retained BM25 CLI parity assertion that belongs to fixture-backed ranking verification into `agentic/tests/test_search_regression.py`, so the authoritative fixture-backed ranking contract is not split across multiple files.
- Keep the task strictly in automated regression-check territory rather than redesigning ranking formulas, search config, or workflow docs.

## Non-Goals

- Do not redesign BM25, vector, or RRF ranking behavior settled in `task-502`.
- Do not invent a second fixture schema, benchmark DSL, or packaged runtime fixture loader; `task-504` already established the canonical fixture contract and test-local loader boundary.
- Do not broaden this task into MCP coverage, snapshot behavior, sync/freshness orchestration, or Compose boot smoke checks.
- Do not require live Ollama access for regression execution; non-BM25 verification must stay deterministic via injected fixed query embeddings.
- Do not hide any manual operator checkpoint inside the regression gate; the gate must remain fully automatable.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-502` - implemented the packaged `PostgresSearchStore`, `SearchRequest`, and BM25/vector/hybrid query contracts in `agentic/src/agentic_kb/search/query.py`
  - `task-503` - implemented the packaged local `search` CLI contract and DB-backed search command tests
  - `task-504` - added the canonical fixture corpus, test-local loader, deterministic query-embedding seam, and seeded-corpus helpers
- Repo artifacts this task depends on directly:
  - `agentic/config/search-fixtures.yaml`
  - `agentic/tests/search_quality_helpers.py`
  - `agentic/tests/test_search_quality_fixtures.py`
  - `agentic/tests/test_search_query_db.py`
  - `agentic/tests/test_search_command_db.py`
- Downstream work made safer by this task:
  - later validation and rollout tasks can treat ranking quality as an executable regression contract instead of an ad hoc manual check
  - future search changes can rely on one authoritative suite that blocks regressions in fixture-backed expected ordering

## Current Build State

- The repo already has deterministic search-quality fixtures plus DB-backed fixture execution in `agentic/tests/test_search_quality_fixtures.py`, but that ownership is currently wrong for the promised release-blocking gate because ranking-regression execution and narrow BM25 CLI parity still live in the fixture-validation file instead of one dedicated authoritative suite.
- `agentic/config/search-fixtures.yaml` currently covers one filtered BM25 document case, one deterministic vector code case, and one cross-entity hybrid case with low-brittleness expectations.
- `agentic/tests/search_quality_helpers.py` already provides the important building blocks for a gate: fixture validation, deterministic injected embeddings for non-BM25 modes, seeded-corpus reset/bootstrap helpers, and reusable expectation assertions.
- The missing work is to move fixture-backed ranking execution ownership into `agentic/tests/test_search_regression.py`, leave `agentic/tests/test_search_quality_fixtures.py` focused on loader/schema validation only, and make the new DB-backed regression file the explicit release-blocking verification contract that future changes should run and treat as authoritative.

## Files Expected To Change

- `agentic/tests/test_search_regression.py` - new focused regression-gate suite for canonical search-ranking fixtures.
- `agentic/tests/search_quality_helpers.py` - only if a small helper extraction or naming cleanup is needed so both the fixture-validation suite and the new regression-gate suite can share deterministic execution helpers cleanly.
- `agentic/tests/test_search_quality_fixtures.py` - trim DB-backed fixture execution and any BM25 CLI parity assertions out of this file so it remains loader/schema validation only.
- `agentic/tests/test_search_command_db.py` - no ranking-gate ownership change is planned here; this file should remain generic CLI contract coverage rather than fixture-backed ranking-regression ownership.
- `.agent/plans/agentic/research/task-505-search-regression-gate.md` - implementation-time durable findings for the accepted gate boundary, anti-flake rules, and authoritative verification path.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - later update only to mark `task-505` completed and reconcile metadata if the final target path or task notes drift.
- This canonical plan doc - update planning/build status, implementation notes, verification notes, and final outcome during implementation.

## Implementation Approach

- Add a dedicated regression suite at `agentic/tests/test_search_regression.py` and make it the sole authoritative fixture-backed ranking-regression suite. Do not leave duplicate DB-backed ranking execution in `test_search_quality_fixtures.py`.
- Reuse `load_search_quality_fixtures()`, `bootstrap_database()`, `reset_and_seed_search_quality_corpus()`, `fixture_query_embedding()`, and `assert_fixture_result()` from `agentic/tests/search_quality_helpers.py` so fixture schema, seeded corpus, and expectation logic remain single-sourced.
- Keep fixture-schema validation and regression execution as separate concerns:
  - `test_search_quality_fixtures.py` should remain the place for loader/validation behavior, YAML/schema guardrails, and search-registry compatibility validation only.
  - `test_search_regression.py` should own all fixture-backed DB execution that proves ranking behavior, including the narrow retained BM25 CLI parity check if that assertion is kept.
- Keep the regression gate DB-backed and deterministic. It should bootstrap a disposable schema, seed the known corpus, run every fixture through the packaged `PostgresSearchStore`, inject fixed query embeddings for `vector` and `hybrid`, and assert declared expectations without live Ollama.
- Preserve low-brittleness expectation semantics from `task-504`: `top_hit`, short `ordered_prefix`, and `contains_unordered` remain the approved assertion vocabulary. The gate should not expand into long exact ranking snapshots that are harder to maintain than the current fixture contract.
- Keep BM25 CLI parity narrow and explicit inside the authoritative regression suite. It should execute only BM25 fixtures through the existing `search.run_search(...)` path and compare ids against direct-store results. Do not make `vector` or `hybrid` CLI regression execution depend on the runtime embedding client unless the harness explicitly patches that behavior.
- Prefer the smallest possible helper movement. If current helpers are already sufficient, the new gate should mostly be an orchestration layer over the existing fixture corpus rather than a refactor of the fixture system.
- Ensure the gate is easy to discover and run through normal unittest discovery, both locally and inside the packaged `kb-tools` image against an isolated ParadeDB instance.
- Treat the packaged DB-backed run of `agentic/tests/test_search_regression.py` as the authoritative release-blocking proof, because ranking behavior depends on the pinned ParadeDB image and real DB execution rather than pure in-memory mocks.

## Acceptance Criteria

- A dedicated automated regression-gate suite exists at `agentic/tests/test_search_regression.py` and is documented as the sole authoritative fixture-backed ranking-regression suite.
- The gate executes the canonical checked-in fixtures from `agentic/config/search-fixtures.yaml` against a deterministic seeded KB corpus.
- The gate covers the shipped search modes represented by the canonical fixtures: `bm25`, `vector`, and `hybrid`/RRF.
- Ranking regressions in required fixture expectations cause automated test failures.
- `vector` and `hybrid` regression execution stays deterministic by injecting fixed query embeddings or an equivalent fake embedding seam; it does not require live Ollama.
- The gate uses the packaged `PostgresSearchStore` and `SearchRequest` contracts rather than reimplementing search behavior in test code.
- `agentic/tests/test_search_quality_fixtures.py` remains limited to fixture loader/schema/search-contract validation and does not also own fixture-backed DB ranking execution.
- Any retained fixture-backed BM25 CLI parity coverage lives in `agentic/tests/test_search_regression.py`, remains narrow, deterministic, and BM25-only by default unless embedding behavior is explicitly patched in the harness.
- The regression gate is discoverable through normal unittest execution and can run in the packaged `kb-tools` environment against an isolated ParadeDB target.
- The task does not introduce new CLI verbs, MCP behavior, ranking-formula changes, schema/index changes, or manual operator checkpoints.

## Verification Plan

- Run `python3 -m py_compile` on `agentic/tests/test_search_regression.py` and any touched shared test helpers.
- Run focused local suites such as:
  - `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'`
  - `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_regression.py'`
- Run authoritative DB-backed verification against an isolated disposable ParadeDB target using `AGENTIC_TEST_DATABASE_URL`, for example:
  - `PYTHONPATH=agentic/src:agentic/tests AGENTIC_TEST_DATABASE_URL=... python3 -m unittest discover -s agentic/tests -p 'test_search_regression.py'`
- Run the same regression-gate suite in the packaged `kb-tools` image against an isolated ParadeDB target, waiting for DB readiness from inside the container context before invoking unittest so the known first-boot ParadeDB handoff does not race the test process. This packaged DB-backed `test_search_regression.py` run is the authoritative release-blocking verification step for task completion.
- If helper reuse touches adjacent search suites, rerun the focused DB-backed coverage that already owns lower-level contracts, especially:
  - `agentic/tests/test_search_query_db.py`
  - `agentic/tests/test_search_command_db.py`
- Keep the authoritative verification path DB-backed and centered on `agentic/tests/test_search_regression.py`, because ranking and ordering behavior are only meaningful against the real seeded ParadeDB execution contract.

## Risks / Open Questions

- The repo already has `test_search_quality_fixtures.py`, so the main planning risk is duplicated ownership: if DB-backed fixture execution remains in both files, future changes can drift, confuse reviewers, and weaken the release-blocking contract.
- If the new gate simply mirrors the existing DB-backed fixture suite without removing ranking ownership from `test_search_quality_fixtures.py`, the task would still miss the PRD's intended single authoritative regression-gate contract.
- Over-refactoring helpers would add churn without improving the gate. The preferred path is a thin explicit gate suite over the current fixture corpus.
- Because the authoritative contract depends on pinned ParadeDB behavior, packaged DB-backed verification of `test_search_regression.py` should remain the source of truth even if local non-DB discovery runs skip DB-backed classes.

## Required Docs / Tracking / Research Updates

- Update this canonical plan doc during implementation with final planning status, build status, implementation notes, verification notes, and outcome.
- Append planner and critique entries to `.agent/plans/agentic/task-plans/task-505-plan-review.md` until planning reaches an approved critique decision.
- Create `.agent/plans/agentic/task-plans/task-505-impl-review.md` only during implementation, not during planning.
- Add `.agent/plans/agentic/research/task-505-search-regression-gate.md` during implementation to capture durable findings about the accepted gate boundary and authoritative verification path.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only when implementation lands so `task-505` status reflects reality.

## Outcome

- Task-505 implementation landed with one explicit ownership move: `agentic/tests/test_search_regression.py` is now the sole authoritative fixture-backed ranking regression gate for the canonical search fixtures, and `agentic/tests/test_search_quality_fixtures.py` is limited to fixture loader/schema/search-contract validation.
- The retained BM25 CLI parity check now lives only in the authoritative regression suite, while `vector` and `hybrid` remain deterministic through injected fixture query embeddings and continue to avoid live Ollama dependencies.

## Implementation Notes

- Added `agentic/tests/test_search_regression.py` as the dedicated DB-backed regression suite that boots a disposable schema, seeds the deterministic fixture corpus, executes all canonical fixtures through `PostgresSearchStore`, and asserts the approved low-brittleness expectations.
- Moved the narrow BM25 CLI parity assertion out of `agentic/tests/test_search_quality_fixtures.py` and into `agentic/tests/test_search_regression.py` so fixture-backed ranking ownership is no longer split across files.
- Reduced `agentic/tests/test_search_quality_fixtures.py` to loader/schema/search-contract validation only, preserving the existing YAML and registry guardrails without duplicate DB-backed ranking execution.
- Added the planned task-505 research note at `.agent/plans/agentic/research/task-505-search-regression-gate.md` to capture the durable gate boundary, evidence pointers, and verification path now that the implementation is complete.

## Verification Notes

- `python3 -m py_compile "agentic/tests/test_search_regression.py" "agentic/tests/test_search_quality_fixtures.py" "agentic/tests/search_quality_helpers.py"` passed.
- `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_quality_fixtures.py'` passed.
- `PYTHONPATH=agentic/src:agentic/tests python3 -m unittest discover -s agentic/tests -p 'test_search_regression.py'` passed locally, with the DB-backed regression class skipped when `AGENTIC_TEST_DATABASE_URL` was unset.
- Authoritative packaged DB-backed verification passed with `AGENTIC_DB_PORT=36055 docker compose -p task505-verify -f docker-compose.agentic.yml up -d paradedb`, then `AGENTIC_DB_PORT=36055 docker compose -p task505-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint sh -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -lc 'until pg_isready -h paradedb -U agentic -d agentic_kb >/dev/null 2>&1; do sleep 2; done && python -m unittest discover -s agentic/tests -p "test_search_regression.py"'`, followed by `AGENTIC_DB_PORT=36055 docker compose -p task505-verify -f docker-compose.agentic.yml down -v`.

## Planning Status Rationale

- Planning status is `approved` because `task-505-plan-review.md` reached an approved critique decision.
- Build status is `completed` because the focused validation suite passed locally and the authoritative packaged DB-backed regression gate passed against an isolated ParadeDB instance.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-505-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-505-impl-review.md`
