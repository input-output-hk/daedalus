# Task 505 Research Note: Search Regression Gate

- Task: `task-505`
- Title: `Add automated search ranking regression gate`
- Status: `completed`

## Durable Findings

- The authoritative fixture-backed ranking regression gate now lives only in `agentic/tests/test_search_regression.py`.
- The canonical fixture corpus remains `agentic/config/search-fixtures.yaml`, and the regression gate executes those checked-in fixtures against the deterministic seeded search corpus.
- Deterministic non-BM25 verification continues to rely on the existing injected query-embedding seam in `agentic/tests/search_quality_helpers.py`, so `vector` and `hybrid` coverage do not require live Ollama access.
- `agentic/tests/test_search_quality_fixtures.py` remains the loader/schema/search-contract validation suite and no longer shares ownership of DB-backed ranking regression execution.
- The retained BM25 CLI parity assertion belongs in the authoritative regression suite only, keeping fixture-backed ranking ownership in one place.

## Evidence Pointers

- Canonical fixtures: `agentic/config/search-fixtures.yaml`
- Shared deterministic helpers: `agentic/tests/search_quality_helpers.py`
- Authoritative regression gate: `agentic/tests/test_search_regression.py`
- Loader/schema validation only: `agentic/tests/test_search_quality_fixtures.py`
- Canonical plan and verification record: `.agent/plans/agentic/task-plans/task-505.md`
- Implementation log: `.agent/plans/agentic/task-plans/task-505-impl-review.md`

## Verification Notes

- Existing implementation verification already recorded a successful syntax check, focused unittest discovery runs, and the authoritative packaged DB-backed `test_search_regression.py` run against isolated ParadeDB in `.agent/plans/agentic/task-plans/task-505.md` and `.agent/plans/agentic/task-plans/task-505-impl-review.md`.
- This research-note follow-up did not change executable code or test behavior; no additional verification beyond documentation consistency was required.
