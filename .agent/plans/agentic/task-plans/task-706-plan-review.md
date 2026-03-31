Planner: Iteration 1
Timestamp: 2026-03-31T10:00:00Z
Outcome: initial_plan_documented

- Created the first canonical `task-706` plan and classified the task as an automated test addition: the upstream dependencies (`task-605` Dropbox backend selection, `task-705` publish/fetch helpers) are complete, so the remaining gap is a repeatable smoke test for the end-to-end local publication path.
- Scoped the test file to `agentic/tests/test_snapshot_publication_workflow.py` covering: sync all, snapshot export, manifest validation, simulated upload/download to shared storage, snapshot import, and deterministic post-import BM25 search proof.
- Kept the tests unit-level and mock-based, consistent with `test_snapshot_compatibility.py`. No live Docker Compose, Ollama, or ParadeDB required.
- Structured the test file into four classes: `PublicationWorkflowSmokeTests` (end-to-end mock chain), `ManifestValidationTests` (schema conformity), `SiblingPairContractTests` (publish/fetch pair enforcement), and `PostImportValidationTests` (status/search proof shape).
- Flagged that the BM25 first-hit expectation from `.agent/workflows/agentic-kb.md` is a mock assertion, not a real ranking guarantee. Task-705 research already noted that real runs may return different first hits.
- Explicitly rejected adding runtime code changes. This is test-only.
- Noted that the publish/fetch shell wrappers are tested indirectly through their underlying Python commands, matching the existing pattern in `test_snapshot_compatibility.py`.

Critiquer: Iteration 1
Timestamp: 2026-03-31T10:15:00Z
Outcome: requires_changes

- **ManifestValidationTests duplicates existing coverage**: `test_snapshot_manifest_schema.py` already exhaustively validates the canonical schema (required fields, artifact identity, embedding contract, entity counts, sync state). The plan's `ManifestValidationTests` class should be removed or narrowed to only workflow-specific assertions (e.g., verifying the export command populates the manifest with correct repo commit hashes and entity counts), not generic schema conformity.

- **Shell wrapper logic is untested**: The plan explicitly rejects testing the shell wrappers (`agentic-kb-publish.sh`, `agentic-kb-fetch.sh`) and says to mock at the Python command level. But the sibling-pair enforcement, basename/suffix parsing, shared-dir name validation (`Daedalus_KB`), and path-rejection logic all live in the shell scripts. Mocking only Python commands leaves this wrapper logic with zero coverage. Either add shell-level tests (mocking `docker compose` via `subprocess` or similar), or explicitly document this as an accepted gap in the Non-Goals.

- **Missing edge cases for shared-folder validation**: The shell scripts validate that `AGENTIC_KB_SHARED_DIR` is set, points to an existing directory, is named `Daedalus_KB`, and is readable/writable (publish) or readable (fetch). None of these error paths are mentioned in the plan's test classes.

- **Missing edge cases for basename/suffix parsing**: Both scripts accept `.dump` or `.manifest.json` suffixes and strip them. The plan does not mention testing that `publish agentic-kb-foo.dump` and `publish agentic-kb-foo.manifest.json` resolve to the same basename, nor that paths with `/` are rejected.

- **Missing error-propagation test for `sync all` failure**: The publish script runs `sync all` before `snapshot export`. If sync fails, export should not run. The plan does not include a test for this precondition failure path.

- **Ordering assertion under-specified**: `PublicationWorkflowSmokeTests` says "verify the full chain executes in the correct order" but does not specify how. Using `mock.call_args_list` or `assert_has_calls` with `any_order=False` should be explicitly called out so the implementer doesn't accidentally write a test that only checks each function was called once without verifying sequence.

- **Post-import search proof mock shape**: The plan correctly notes that BM25 first-hit ranking is not stable (task-705 research confirmed this). However, the mock search results should match the exact structure returned by the real `search` command (fields like `entity_type`, `fields.source_path`, score, etc.). The plan should reference the actual search output schema or an existing fixture from `test_search_command.py` / `test_search_quality_fixtures.py` to ensure the mock shape is realistic.

- **No test for `status --json` output parsing**: The plan mentions mocking `status --json` output and verifying `ok: true` and `embedding_compatibility.state = "compatible"`, but `test_snapshot_compatibility.py` already tests `collect_status_report` extensively. The post-import validation should focus on the integration shape (i.e., that the status command is called after import and its output is consumed correctly by the workflow), not on re-testing the status command internals.

- **Missing test for fetch into non-existent local snapshots directory**: The fetch script creates `agentic/snapshots/` with `mkdir -p` if it doesn't exist. The plan should include a test verifying this directory creation behavior.

- **Estimated 3 hours may be tight**: Four test classes with the gaps identified above (shell wrapper tests, shared-dir validation, basename parsing, sync-failure path, ordering assertions) could easily exceed 3 hours. Recommend bumping to 4-5 hours if shell-level tests are added.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-31T10:30:00Z
Outcome: revised_plan_addresses_review_feedback

- **ManifestValidationTests removed**: Deleted the entire `ManifestValidationTests` class from the plan. `test_snapshot_manifest_schema.py` already exhaustively validates the canonical schema (required fields, artifact identity, embedding contract, entity counts, sync state). The Non-Goals section now explicitly states "Do not duplicate manifest schema validation already covered in `test_snapshot_manifest_schema.py`."

- **Shell wrapper tests added**: Added a new test file `agentic/tests/test_shell_wrappers.py` with two test classes: `PublishScriptTests` and `FetchScriptTests`. These test the bash scripts directly using `subprocess.run` with a fake `docker` binary on PATH to record invocations. This covers sibling-pair enforcement, basename/suffix parsing, shared-dir validation, and copy verification — all logic that lives in the shell scripts and was previously untested.

- **Shared-dir validation failures covered**: `PublishScriptTests` and `FetchScriptTests` both include tests for missing env var, non-directory path, wrong basename (not `Daedalus_KB`), and permission checks (read/write for publish, read-only for fetch).

- **Basename/suffix parsing covered**: Both publish and fetch tests verify that `.dump` and `.manifest.json` suffixes resolve to the same basename, and that paths containing `/` are rejected.

- **Sync failure propagation covered**: `PublishScriptTests` includes a test verifying that if `sync all` fails, `snapshot export` must not run — checked via recorded subprocess call ordering.

- **Fetch directory creation covered**: `FetchScriptTests` includes a test verifying that `agentic/snapshots/` is created with `mkdir -p` if it doesn't exist.

- **Ordering assertions made explicit**: `PublicationWorkflowSmokeTests` now specifies using `mock.call_args_list` with explicit `assert_has_calls(calls, any_order=False)` to verify the full chain executes in the correct order. This is also reflected in Acceptance Criterion 8.

- **Post-import tests simplified**: Renamed `PostImportValidationTests` to `PostImportWorkflowTests` and narrowed scope to verify the publication workflow chain (status called after import, search called after status) without re-testing status command internals. The Non-Goals section now explicitly states "Do not duplicate status-command internals already covered in `test_snapshot_compatibility.py`."

- **Estimate revised to 4-5 hours**: Updated the header estimate from 3 hours to 4-5 hours to account for the additional shell-level test coverage.

- **Acceptance criteria updated**: Expanded from 7 to 11 criteria to explicitly cover shell wrapper tests, ordering assertions, workflow chain focus, and non-duplication of existing test coverage.

- **Verification plan updated**: Added separate verification steps for both test files (`test_snapshot_publication_workflow.py` and `test_shell_wrappers.py`).

Decision: ready_for_implementation
