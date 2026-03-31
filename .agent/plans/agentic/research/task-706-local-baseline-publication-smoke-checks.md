# Task 706 Local Baseline Publication Smoke Checks Research

- Date: 2026-03-31
- Task: `task-706`
- Evidence: `agentic/tests/test_snapshot_publication_workflow.py`, `agentic/tests/test_shell_wrappers.py`, `.agent/plans/agentic/task-plans/task-706.md`, `.agent/plans/agentic/task-plans/task-706-impl-review.md`

## Durable Findings

### Test Patterns Used
- Both test files use `unittest.TestCase` as the base class, consistent with `test_snapshot_compatibility.py` and the broader agentic test suite.
- `unittest.mock.patch` and `MagicMock` are used throughout. Shell wrapper tests patch `subprocess.run` to intercept `docker compose` invocations.
- Fixture helper functions follow the `_manifest_fixture` pattern from `test_snapshot_compatibility.py` for manifest construction.
- Shell wrapper tests use a `_setup_temp_repo()` helper that copies the real `scripts/agentic-kb-publish.sh` and `scripts/agentic-kb-fetch.sh` into a temporary directory and runs them with `cwd=repo_root`, avoiding side effects on the real repo.

### Call-Log Ordering Approach
- The prescribed `assert_has_calls(calls, any_order=False)` pattern was replaced with a shared `call_log: list[str]` approach for cross-function ordering verification.
- Each mocked workflow function appends its step name to the log via a `side_effect` callback.
- The test asserts the exact expected sequence (e.g., `["sync", "export", "ensure_compatible", "resolve_pair", "import"]`).
- This pattern is simpler, more readable, and produces clearer failure messages than per-mock `assert_has_calls` that must be cross-referenced.
- Two ordering tests use this pattern: `test_full_chain_executes_in_order` (publication chain) and `test_status_then_search_ordering` (post-import chain).

### Shell Wrapper Test Approach
- Tests invoke real bash scripts via `subprocess.run` with temporary directories and fake `docker` binaries on PATH.
- The fake `docker` binary is a small script that records invocations and can be configured to exit non-zero for failure-propagation tests.
- Shared-dir validation tests cover: missing env var, non-directory path, wrong basename (not `Daedalus_KB`), non-readable, and non-writable conditions.
- Basename/suffix parsing tests verify `.dump` and `.manifest.json` resolution and reject paths containing `/`.
- Sibling-pair enforcement tests verify both files must exist before publish/fetch proceeds.
- Sync failure propagation tests verify `set -e` abort behavior when `sync all` fails.
- Fetch directory creation tests verify `mkdir -p` for `agentic/snapshots/`.

## Gotchas

- **No Python-level orchestrator exists.** The real publication workflow orchestration lives in the bash scripts (`agentic-kb-publish.sh`, `agentic-kb-fetch.sh`). The Python-level publication tests mock individual functions and call them in sequence rather than testing a single orchestrating function. This is a known limitation — the shell wrapper tests cover the real orchestration.
- **`SiblingPairContractTests` document but do not enforce.** The Python-level sibling-pair tests verify the contract shape (both files must exist together) but do not exercise enforcement logic. Actual enforcement testing is done by the shell wrapper tests.
- **Test count reduced during review.** The publication workflow test file started with 11 tests but was reduced to 10 by removing two vacuous tests (`test_status_called_after_import`, `test_search_called_after_status`) that did not verify behavioral contracts.
- **Unused `call` import remains.** Line 9 of `test_snapshot_publication_workflow.py` imports `call` from `unittest.mock` but it is never used. This is a cosmetic issue only.

## Test Inventory

### `test_snapshot_publication_workflow.py` (10 tests)
- `PublicationWorkflowSmokeTests` (3): full chain ordering, sibling pair creation, shared-dir copy verification
- `SiblingPairContractTests` (4): publish with only dump, publish with only manifest, fetch with only dump in shared, fetch with only manifest in shared
- `PostImportWorkflowTests` (3): status returns compatible report, search result schema shape, status-then-search ordering

### `test_shell_wrappers.py` (23 tests)
- `PublishScriptTests` (11): shared-dir validation (5 cases), basename/suffix parsing (3 cases), sync failure propagation, sibling-pair enforcement, copy verification
- `FetchScriptTests` (12): argument requirement, shared-dir validation (4 cases), basename/suffix parsing (3 cases), sibling-pair enforcement (2 cases), mkdir -p directory creation, copy verification
