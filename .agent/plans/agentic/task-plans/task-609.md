# Task Plan: task-609 Snapshot Compatibility Guard Tests

- Task ID: `task-609`
- Title: `Add snapshot compatibility guard tests`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-608` shipped snapshot embedding-contract compatibility enforcement across import, status, and sync commands. The implementation is verified through existing test suites (`test_snapshot_command.py`, `test_status_command.py`, `test_sync_command.py`) and their DB-backed counterparts.
- `task-609` creates a dedicated regression suite in `agentic/tests/test_snapshot_compatibility.py` that proves all three compatibility-safety guarantees in one place: import rejects mismatched contracts, status reports correct compatibility states, and post-import sync fails safely when contracts diverge.
- This task is on the critical path for downstream tasks that depend on trustworthy shared-baseline handoff and will serve as the definitive proof point for the safety guarantees introduced in task-608.

## Scope

- Create `agentic/tests/test_snapshot_compatibility.py` as a focused regression suite for snapshot embedding-contract compatibility guards.
- Cover import rejection: legacy manifest, dimension mismatch, model mismatch, contract ID mismatch, extra keys in embedding_contract.
- Cover status reporting: compatible, incompatible, missing_imported_snapshot_metadata, unsupported_legacy_manifest, unavailable.
- Cover sync changed: allows local-only KB, rejects legacy manifest, rejects incompatible contract, rejects malformed metadata.
- Ensure the test file uses the same patterns and helper fixtures as the existing test suites.
- Run verification against the existing unit test suites to confirm no regressions.

## Non-Goals

- Do not modify any implementation code in `agentic/src/agentic_kb/`.
- Do not create DB-backed tests in this file; those remain in the existing `test_*_db.py` suites.
- Do not add tests for non-snapshot-compatibility scenarios already covered in other test files.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-608` shipped the embedding-contract compatibility enforcement that these tests verify.
- Files and evidence reviewed for this plan:
  - `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md`
  - `.agent/plans/agentic/task-plans/task-608.md`
  - `agentic/src/agentic_kb/snapshot_manifest.py`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/tests/test_snapshot_command.py`
  - `agentic/tests/test_status_command.py`
  - `agentic/tests/test_sync_command.py`

## Files Expected To Change

- `agentic/tests/test_snapshot_compatibility.py` - new dedicated regression test file for snapshot compatibility guards.

## Implementation Approach

- Create `agentic/tests/test_snapshot_compatibility.py` using unittest framework matching the existing test style.
- Reuse existing helper fixtures from the other test files (e.g., `_snapshot_record` from `test_sync_command.py`, `_manifest_fixture` from `test_snapshot_command.py`).
- Organize tests into three classes: `SnapshotImportCompatibilityTests`, `SnapshotStatusCompatibilityTests`, `SnapshotSyncCompatibilityTests`.
- Use `@patch` to isolate the compatibility checks from external dependencies (database, Ollama, etc.).
- Each test method should be self-contained and use descriptive names that map directly to the acceptance criteria.

### SnapshotImportCompatibilityTests Test Cases

1. `test_import_snapshot_rejects_legacy_manifest` - rejects manifest missing embedding_contract field
2. `test_import_snapshot_rejects_embedding_dimension_mismatch` - rejects when embedding_dimension differs
3. `test_import_snapshot_rejects_embedding_model_mismatch` - rejects when embedding_model differs
4. `test_import_snapshot_rejects_contract_identifier_mismatch` - rejects when contract_id differs
5. `test_import_snapshot_rejects_embedding_contract_extra_keys` - rejects when embedding_contract contains unexpected keys

## Acceptance Criteria

- `agentic/tests/test_snapshot_compatibility.py` exists and runs without errors.
- Import tests prove rejection of: legacy manifest, dimension mismatch, model mismatch, contract ID mismatch, extra embedding_contract keys.
- Status tests prove correct reporting of: compatible, incompatible, missing_imported_snapshot_metadata, unsupported_legacy_manifest, unavailable.
- Sync tests prove safe failure for: legacy manifest, incompatible contract, malformed metadata, and correct allowance for local-only KBs.
- All existing test suites continue to pass after this file is added.

## Verification Plan

- Run `python3 -m py_compile agentic/tests/test_snapshot_compatibility.py` to confirm no syntax errors.
- Run the new test file: `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_compatibility.py'`.
- Run existing test suites to confirm no regressions:
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_command.py'`

## Risks / Open Questions

- None identified. The implementation is well-grounded in the task-608 research findings and existing test patterns.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-609-plan-review.md`
- Implementation review log: do not create during planning.

## Required User Inputs

- None required for planning or implementation.

## Required Manual Test Steps

- None required; verification is fully automated.

## Whether Implementation Can Proceed Before That User Interaction

- Yes. The task is fully agent-executable inside the repo with code changes and automated verification.
