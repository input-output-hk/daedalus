Planner: Iteration 1
Timestamp: 2026-03-29T21:29:28Z
Outcome: initial_plan_documented

- Created the canonical `task-505` plan and classified the interaction mode as `autonomous` because this task is an internal automated regression gate with no required user decision, manual execution step, or evidence checkpoint.
- Scoped the task to promoting the checked-in `search-fixtures.yaml` corpus into one explicit ranking-regression gate at `agentic/tests/test_search_regression.py`, reusing the existing deterministic seeded-corpus and injected-embedding helpers from `task-504`.
- Kept the plan narrowly aligned to current repo contracts: direct packaged `PostgresSearchStore` execution for all fixtures, deterministic non-BM25 runs without live Ollama, and only narrow BM25 CLI parity coverage where it proves the packaged command still matches direct store results.
- Flagged the main planning risk to resolve during implementation: avoid duplicating `test_search_quality_fixtures.py` by clearly separating fixture-schema validation from the canonical regression-gate suite and its authoritative packaged DB-backed verification path.

Critiquer: Iteration 1
Timestamp: 2026-03-29T21:31:14Z
Outcome: requires_changes

- The plan still leaves gate ownership ambiguous. `test_search_quality_fixtures.py` already contains the DB-backed fixture execution and BM25 CLI parity coverage that this task is supposed to promote into a new explicit regression gate, but the plan only says that file may be "trimmed or clarified" and that CLI parity is retained only "if" kept in the gate. That leaves an easy implementation path where `test_search_regression.py` duplicates the existing DB-backed assertions or where the authoritative ranking contract stays split across two files. Tighten the plan so `agentic/tests/test_search_regression.py` is the sole authoritative ranking-regression owner, with DB-backed fixture execution and any retained BM25 CLI parity moved there, while `agentic/tests/test_search_quality_fixtures.py` remains loader/schema validation only.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T21:45:00Z
Outcome: revised_plan_documented

- Re-read the full canonical plan, full planning review log, and the referenced PRD/workflow/research artifacts before revising the plan.
- Tightened the canonical plan so `agentic/tests/test_search_regression.py` is now explicitly the sole authoritative fixture-backed ranking-regression suite and the packaged DB-backed run of that file is the release-blocking verification step.
- Clarified that `agentic/tests/test_search_quality_fixtures.py` must be reduced to loader/schema/search-contract validation only, with DB-backed ranking execution and any retained BM25 CLI parity removed from that file.
- Removed the remaining ambiguity around BM25 CLI parity by assigning any retained fixture-backed CLI parity assertion to `agentic/tests/test_search_regression.py` only, while leaving `agentic/tests/test_search_command_db.py` as generic CLI contract coverage rather than ranking-gate ownership.
- Updated file-change expectations, implementation approach, acceptance criteria, verification language, and risk wording so duplicate ranking coverage is treated as a planning failure mode rather than an acceptable implementation variant.

Critiquer: Iteration 2
Timestamp: 2026-03-29T21:56:00Z
Outcome: approved

- The revised plan is now specific and truthful about current repo state. `agentic/tests/test_search_quality_fixtures.py` does currently own the DB-backed fixture execution and BM25 CLI parity coverage, while `agentic/tests/search_quality_helpers.py` already provides the shared loader, seeded-corpus bootstrap/reset helpers, deterministic embedding seam, and expectation assertions the new gate should reuse.
- The scope is now minimal and correctly bounded to ownership transfer plus one authoritative regression suite. It does not introduce a second fixture format, packaged runtime loader, ranking redesign, or unrelated CLI/MCP/sync work, and it correctly leaves `test_search_query_db.py` and `test_search_command_db.py` as lower-level contract coverage rather than ranking-gate owners.
- The interaction mode remains correct as `autonomous` because this is an internal automated regression gate task with no required user choice, manual checkpoint, or evidence handoff.
- The verification plan remains correct: focused unittest discovery is appropriate for local structure checks, while the DB-backed `AGENTIC_TEST_DATABASE_URL` run and the packaged `kb-tools` `--entrypoint python` DB-backed run are the right authoritative proofs because ranking behavior depends on the pinned ParadeDB/container contract.

Decision: approved
