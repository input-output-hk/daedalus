# Task 609 Plan Review Log

Planner: Iteration 1
Timestamp: 2026-03-30T00:00:00Z
Outcome: initial_plan_documented

- Created the canonical `task-609` plan for snapshot compatibility guard regression tests.
- Grounded the plan in task-608 research findings: the three-field embedding contract (contract_id, embedding_model, embedding_dimension), the legacy manifest rejection policy, the status reporting states, and the sync changed safety behavior.
- Scope is tight: one new test file `agentic/tests/test_snapshot_compatibility.py` that proves import rejection, status reporting, and sync safe failure for all compatibility states.
- Non-goals are explicit: no implementation changes, no DB-backed tests, no coverage beyond snapshot compatibility.
- Implementation approach follows existing test patterns and reuses helper fixtures from other test files.
- Verification is fully automated: compile check, unit run, and regression run on existing suites.
- Set `Interaction Mode: autonomous` because the task is code-and-tests only with no required human decision or manual execution checkpoint.

Critiquer: Iteration 1
Timestamp: 2026-03-30T02:30:00Z
Outcome: requires_changes

## Analysis

### Gap in Import Rejection Coverage

The acceptance criteria for import tests explicitly lists five rejection scenarios:
1. legacy manifest
2. dimension mismatch
3. model mismatch
4. contract ID mismatch
5. **extra keys in embedding_contract**

Scanning `test_snapshot_command.py` (the existing snapshot test file that task-609's new file will mirror), I found tests covering scenarios 1-4:
- `test_import_snapshot_rejects_legacy_embedding_model_only_manifest` (scenario 1)
- `test_import_snapshot_rejects_embedding_dimension_mismatch` (scenario 2)
- `test_import_snapshot_rejects_contract_identifier_mismatch` (scenario 4)

However, **scenario 5 (extra keys in embedding_contract) has no corresponding import rejection test**. The existing `test_snapshot_command.py` has no test for `ensure_compatible_snapshot_import` rejecting a manifest with unexpected fields in `embedding_contract`.

Contrast this with the sync tests where `test_sync_changed_rejects_imported_manifest_with_extra_embedding_contract_keys` provides explicit coverage for the extra-keys case. The import path (`ensure_compatible_snapshot_import` → `extract_snapshot_embedding_contract`) does raise `SnapshotManifestCompatibilityError` for extra keys, but no test verifies this at the import layer.

### Minor Note: Fixture Reference

The plan references `_snapshot_record` from `test_sync_command.py` as a fixture to reuse. However, `_snapshot_record` is actually defined in `test_status_command.py` (line 537) and duplicated in `test_sync_command.py` (line 1025). The reference is slightly imprecise but functionally correct since both exist.

### Status Reporting State: `unavailable`

The acceptance criteria lists five status states: compatible, incompatible, missing_imported_snapshot_metadata, unsupported_legacy_manifest, unavailable.

The `unavailable` state is reached when `extract_snapshot_embedding_contract` raises `SnapshotManifestCompatibilityError` for malformed contract metadata (extra keys, wrong types, etc.). Existing test `test_collect_status_report_marks_extra_embedding_contract_keys_as_invalid` provides this coverage. However, this test is indirect—it goes through `inspect_database` → `_collect_embedding_compatibility`. A more direct test that calls `_collect_embedding_compatibility` with a malformed manifest would be more explicit.

If the goal is a "dedicated regression suite" as stated in the task description, adding an explicit test directly for the `unavailable` state via `_collect_embedding_compatibility` (rather than through `collect_status_report`) would strengthen coverage.

### Scope Assessment

The scope is appropriately tight. The plan correctly:
- Limits to one new test file
- Excludes DB-backed tests (those remain in existing suites)
- Excludes implementation changes
- Focuses purely on compatibility guard verification

## Specific, Actionable Feedback

1. **Add `test_import_snapshot_rejects_embedding_contract_extra_keys`** to the import test class. This is the only missing scenario from the five acceptance criteria for import rejection.

2. **Consider adding a direct `_collect_embedding_compatibility` test for the `unavailable` state** (status side). This would be a targeted test that patches `inspect_database` to return a `SnapshotManifestRecord` with malformed contract metadata and verifies `EmbeddingCompatibilityReport.state == "unavailable"` without going through the full `collect_status_report` pipeline. However, given existing indirect coverage via `test_collect_status_report_marks_extra_embedding_contract_keys_as_invalid`, this is optional.

## Risks

- The missing import extra-keys test is a real gap in the acceptance criteria coverage. If this test is not added, the new test file would not fully satisfy the stated acceptance criteria.

## Verdict

The plan is well-structured and the implementation approach is sound. However, the acceptance criteria explicitly includes "extra keys in embedding_contract" as an import rejection scenario, and no such test exists in the existing test suites. The new test file must include this test to satisfy the stated criteria.

Decision: requires_changes

- Created the canonical `task-609` plan for snapshot compatibility guard regression tests.
- Grounded the plan in task-608 research findings: the three-field embedding contract (contract_id, embedding_model, embedding_dimension), the legacy manifest rejection policy, the status reporting states, and the sync changed safety behavior.
- Scope is tight: one new test file `agentic/tests/test_snapshot_compatibility.py` that proves import rejection, status reporting, and sync safe failure for all compatibility states.
- Non-goals are explicit: no implementation changes, no DB-backed tests, no coverage beyond snapshot compatibility.
- Implementation approach follows existing test patterns and reuses helper fixtures from other test files.
- Verification is fully automated: compile check, unit run, and regression run on existing suites.
- Set `Interaction Mode: autonomous` because the task is code-and-tests only with no required human decision or manual execution checkpoint.

Planner: Iteration 2
Timestamp: 2026-03-30T04:00:00Z
Outcome: plan_revised

## Changes Made

Critiquer Iteration 1 identified that the plan's acceptance criteria explicitly lists "extra keys in embedding_contract" as one of five import rejection scenarios, but the Implementation Approach section did not include an explicit test case name for this scenario.

**Change:** Added a dedicated "SnapshotImportCompatibilityTests Test Cases" subsection in the Implementation Approach that enumerates all five test cases by name:
1. `test_import_snapshot_rejects_legacy_manifest`
2. `test_import_snapshot_rejects_embedding_dimension_mismatch`
3. `test_import_snapshot_rejects_embedding_model_mismatch`
4. `test_import_snapshot_rejects_contract_identifier_mismatch`
5. `test_import_snapshot_rejects_embedding_contract_extra_keys` (the previously missing case)

This makes the missing scenario explicit and directly addressable during implementation, matching the critquer's actionable feedback item #1.

**Not changed (intentionally):** The critquer's optional suggestion (#2) to add a direct `_collect_embedding_compatibility` test for the `unavailable` state was noted but not incorporated, as it falls outside the minimum viable scope for a tight regression suite and has existing indirect coverage.

## What Was Changed in the Canonical Plan

- Added "SnapshotImportCompatibilityTests Test Cases" enumeration in the Implementation Approach section (lines 57-62) listing all five import rejection test case names, including the previously implicit `test_import_snapshot_rejects_embedding_contract_extra_keys`.
- This change directly addresses the Critiquer gap: "scenario 5 (extra keys in embedding_contract) has no corresponding import rejection test" in the new test file.

## Planner Iteration 2 Summary

The plan was revised to explicitly enumerate all five import rejection test cases in the Implementation Approach, making the `test_import_snapshot_rejects_embedding_contract_extra_keys` test case visible and actionable. The scope remains tight; no additional optional tests were added. The plan is now complete and ready for implementation.

Critiquer: Iteration 2
Timestamp: 2026-03-30T06:00:00Z
Outcome: approved

## Analysis

### Gap Resolution

Critiquer Iteration 1 identified that `test_import_snapshot_rejects_embedding_contract_extra_keys` was missing from the Implementation Approach section, even though the acceptance criteria explicitly lists "extra keys in embedding_contract" as the fifth import rejection scenario.

Planner Iteration 2 addressed this by adding a dedicated "SnapshotImportCompatibilityTests Test Cases" subsection (lines 57-63) that enumerates all five test cases by exact name:
1. `test_import_snapshot_rejects_legacy_manifest`
2. `test_import_snapshot_rejects_embedding_dimension_mismatch`
3. `test_import_snapshot_rejects_embedding_model_mismatch`
4. `test_import_snapshot_rejects_contract_identifier_mismatch`
5. `test_import_snapshot_rejects_embedding_contract_extra_keys`

This fully closes the gap identified in Iteration 1.

### Scope Verification

The scope remains appropriately tight:
- Still limited to one new test file
- No implementation changes
- Reuses existing helper fixtures
- Follows established test patterns
- No optional enhancements added (correctly deferred)

### Plan Completeness

The plan now fully specifies:
- All five import rejection test cases with explicit names
- All five status reporting states
- All sync changed rejection scenarios
- Clear acceptance criteria matching the test cases
- Automated verification steps

## Risks

None. The identified gap has been addressed and no new risks are present.

## Verdict

The plan is complete, well-structured, and ready for implementation. All acceptance criteria have explicit test case coverage.

Decision: approved
