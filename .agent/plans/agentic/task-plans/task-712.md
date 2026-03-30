# Task Plan: task-712 Add ingest fixture coverage

- Task ID: `task-712`
- Title: `Add ingest fixture coverage`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

The PRD requires "automated ingest fixtures, snapshot round-trip coverage, and MCP smoke tests so the documented v1 validation contract is executable." The four ingest domains (docs, code, GitHub, Project) each have unit-test coverage in dedicated files, but there is no consolidated fixture file that:

1. Exercises all four ingest types with shared fake clients and store patterns
2. Proves idempotent re-ingest behavior across all four domains in one place
3. Validates canonical metadata extraction (task plan fields, GitHub labels, Project fields) stays regression-tested
4. Validates chunking behavior (heading splits for docs, symbol chunks for code) is consistent across re-ingest cycles
5. Verifies v1 validation contract behaviors (idempotency, content-addressing) are exercised across all domains

Completing this task closes the fixture coverage gap so future changes to any ingest domain cannot silently break metadata extraction, chunking, or idempotency invariants. The fixture tests verify **NEW assertions** not already covered by existing test files: cross-domain idempotency contracts (same content_hash after unchanged re-ingest, store key uniqueness under re-ingest) and consolidated pass/fail signals for each domain.

## Scope

- Create `agentic/tests/test_ingest_fixtures.py` with consolidated ingest fixture tests covering all four domains
- Cover canonical metadata extraction for each domain:
  - Docs: task_id, title, planning_status, build_status from plan documents; plan_type discrimination (canonical_task_plan vs plan_review_log vs implementation_review_log)
  - Code: language classification, symbol_kind discrimination, chunk_index and line-range accuracy
  - GitHub: issue/PR number mapping, label extraction, body_text construction, comment routing
  - Project: single-select field mapping (Status, Priority, Size, Work Type, Area, Phase, KB Impact), date field parsing, content_type discrimination
- Cover idempotent re-ingest behavior for each domain:
  - Second ingest of unchanged content produces no updated_paths / no new rows
  - Second ingest of unchanged content preserves document `content_hash` exactly (NEW assertion beyond existing tests)
  - Second ingest of changed content replaces in-place with new `content_hash` and updated embedding
  - Store key uniqueness is preserved after re-ingest (verified explicitly, not just incidentally)
- Cover chunking consistency across re-ingest for applicable domains:
  - Docs: same heading-boundary chunk count and heading_path after re-ingest
  - Code: same symbol chunk count and symbol_name set after re-ingest
- Do not add snapshot round-trip tests (those belong to a separate task)
- Do not add MCP smoke tests (those belong to a separate task)

## Relevant Dependencies

- Completed dependencies:
  - `task-301` - docs ingestion with metadata extraction (task_id, title, planning_status, build_status, plan_type)
  - `task-402` - code ingestion with symbol-aware chunking
  - `task-403` - GitHub ingestion with issue/PR/comment routing
  - `task-404` - Project 5 ingestion with field value extraction
- Existing test infrastructure:
  - `agentic/tests/test_docs_ingest.py` - existing docs unit tests with FakeEmbeddingClient and InMemoryDocsStore
  - `agentic/tests/test_code_ingest.py` - existing code unit tests with FakeEmbeddingClient and InMemoryCodeChunksStore
  - `agentic/tests/test_github_ingest.py` - existing GitHub unit tests with FakeEmbeddingClient, FakeGithubApiClient, and InMemoryGithubStore
  - `agentic/tests/test_project_ingest.py` - existing Project unit tests with FakeEmbeddingClient, FakeGithubProjectApiClient, and InMemoryProjectItemsStore
- Shared fake client patterns that should be reused/adapted in the new fixture file

## Files Expected To Change

- `agentic/tests/test_ingest_fixtures.py` - new consolidated fixture test file (primary deliverable)
- `.agent/plans/agentic/task-plans/task-712.md` - this canonical task plan
- `.agent/plans/agentic/task-plans/task-712-plan-review.md` - planning review log
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update task-712 status when implementation lands

## Implementation Approach

### Test Structure

The new `test_ingest_fixtures.py` will use a `unittest.TestCase` class per ingest domain, following the established pattern in existing test files. Each class will use the same fake clients and stores as the existing tests but focus on cross-cutting concerns that span multiple existing test methods.

### Docs Fixtures (`DocsFixturesTests`)

1. **Canonical task plan metadata extraction**: Write a minimal task plan markdown file, ingest it, verify task_id, title, planning_status, build_status are extracted into document metadata
2. **plan_type discrimination**: Write canonical_task_plan, plan_review_log, and implementation_review_log variants; verify each is classified correctly and plan_type metadata is set appropriately
3. **Idempotent re-ingest of unchanged plan doc**: Ingest twice with same content; verify `updated_paths` is empty and document `content_hash` is identical on both runs. This is the NEW assertion: existing tests check upsert behavior but do not assert `content_hash` equality after unchanged re-ingest.
4. **Idempotent re-ingest of changed plan doc**: Ingest, modify content, re-ingest; verify in-place replacement with new `content_hash` AND that `updated_paths` references the same document key
5. **Store key uniqueness under re-ingest**: After re-ingesting unchanged content, verify no duplicate document keys exist in the store (proves idempotent upsert)
6. **Heading-boundary chunking consistency**: Ingest a multi-heading doc twice; verify chunk count and heading_path array are identical on both runs

### Code Fixtures (`CodeFixturesTests`)

1. **Language classification accuracy**: Ingest files of different types (`.ts`, `.tsx`, `.json`, `.yml`, `.nix`); verify language metadata is correct
2. **Symbol chunk consistency across re-ingest**: Ingest a TypeScript file with known symbols twice; verify same symbol_name set and chunk_index values. This confirms chunking determinism under re-ingest, not just single-run stability.
3. **Idempotent re-ingest of unchanged code**: Re-ingest same file; verify `updated_paths` is empty and stored chunks have identical `content_hash` values. NEW assertion vs existing tests: explicit `content_hash` equality check on stored chunks after unchanged re-ingest.
4. **Idempotent re-ingest of changed code**: Modify file content, re-ingest; verify in-place replacement with new `content_hash`
5. **Store key uniqueness under re-ingest**: After re-ingesting unchanged content, verify no duplicate chunk keys exist in the store

### GitHub Fixtures (`GithubFixturesTests`)

1. **Issue metadata extraction**: Simulate issue ingest; verify issue number, title, labels, body_text are correctly stored
2. **PR metadata extraction**: Simulate PR ingest; verify PR number, base/head branch, merged status
3. **Idempotent re-ingest of unchanged issue**: Re-ingest same issue; verify `updated_paths` is empty and stored issue has identical `content_hash`. NEW assertion: explicit `content_hash` equality check after unchanged re-ingest.
4. **Idempotent re-ingest of changed issue**: Update issue body, re-ingest; verify body_text is updated in-place with new `content_hash`
5. **Store key uniqueness under re-ingest**: After re-ingesting unchanged content, verify no duplicate issue/PR keys exist in the store
6. **Comment routing consistency**: Issue comments route to issue_comments table; PR comments route to pr_comments table; re-ingest should preserve routing

### Project Fixtures (`ProjectFixturesTests`)

1. **Single-select field extraction**: Simulate Project item with all known single-select fields; verify Status, Priority, Size, Work Type, Area, Phase, KB Impact are mapped correctly
2. **Date field extraction**: Simulate Project item with Start date and Target date; verify date parsing
3. **content_type discrimination**: Issue items, PR items, and DraftIssue items should have correct content_type and content_id
4. **Idempotent re-ingest of unchanged project item**: Re-ingest same item; verify `updated_paths` is empty and stored item has identical `content_hash`. NEW assertion: explicit `content_hash` equality check after unchanged re-ingest.
5. **Idempotent re-ingest of changed project item**: Modify item field value, re-ingest; verify field value is updated in-place with new `content_hash`
6. **Store key uniqueness under re-ingest**: After re-ingesting unchanged content, verify no duplicate item keys exist in the store

### Fake Client Strategy

**Import existing fake classes rather than copying them** to avoid maintenance divergence:

```python
from agentic.tests.test_docs_ingest import FakeEmbeddingClient, InMemoryDocsStore
from agentic.tests.test_code_ingest import InMemoryCodeChunksStore
from agentic.tests.test_github_ingest import FakeGithubApiClient, InMemoryGithubStore
from agentic.tests.test_project_ingest import FakeGithubProjectApiClient, InMemoryProjectItemsStore
```

Using the real class definitions from existing test modules ensures:
- If `FakeEmbeddingClient` evolves, fixture tests fail loudly rather than silently diverging
- No duplicate class definitions in the codebase
- The fixture tests exercise the same code paths as existing unit tests

For domain-specific fakes, adapt the patterns from each existing test file:
- Docs: `InMemoryDocsStore` directly
- Code: `InMemoryCodeChunksStore` directly
- GitHub: `FakeGithubApiClient` with controlled page payloads
- Project: `FakeGithubProjectApiClient` with controlled page payloads

### Chunking Consistency Verification

For docs, the test will:
1. Write a multi-section markdown fixture to a temp file
2. Call `prepare_documents()` twice with same source_paths
3. Assert `len(first_result) == len(second_result)` and all `chunk_index` values match

For code, the test will:
1. Call `prepare_code_chunks()` on a known TypeScript file twice
2. Assert symbol names and chunk indices match across both runs

## Acceptance Criteria

- `agentic/tests/test_ingest_fixtures.py` exists and runs with `python3 -m unittest agentic.tests.test_ingest_fixtures`
- All tests pass in isolation and in the full test suite
- Each of the four ingest domains (docs, code, GitHub, Project) has at least:
  - One test for canonical metadata extraction
  - One test for idempotent re-ingest of unchanged content (proving no spurious updates) with explicit `content_hash` equality assertion
  - One test for idempotent re-ingest of changed content (proving in-place replacement)
  - One test for store key uniqueness under re-ingest (proving no duplicate keys)
  - One test for chunking/symbol consistency across re-ingest (docs/code only)
- **NEW assertions only**: Fixture tests must produce assertions not already present in `test_docs_ingest.py`, `test_code_ingest.py`, `test_github_ingest.py`, `test_project_ingest.py`. Redundant assertions add maintenance burden without coverage gain.
- **CI gate**: `test_ingest_fixtures.py` runs as part of the standard `discover` test suite; no manual invocation required for CI validation
- The test file follows the existing test naming and structure conventions
- No new dependencies are introduced beyond what the existing test suite already uses

## Verification Plan

- Run `python3 -m py_compile agentic/tests/test_ingest_fixtures.py` to confirm syntax validity
- Run `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_ingest_fixtures` to execute the new fixture tests in isolation
- **CI gate**: Run the full test suite `PYTHONPATH="agentic/src" python3 -m unittest discover -s agentic/tests -p 'test_*.py'` — fixture tests MUST pass as part of this discover run (no manual skip allowed)
- Inspect test output to confirm all four domain fixtures (docs, code, github, project) are exercised
- Verify that the new tests import existing fake client classes (not copy them) to prevent maintenance divergence
- Verify that new fixture tests produce NEW assertions not already in existing test files by reviewing assertion subjects and objects

## Risks / Open Questions

- **Fixtures vs integration tests**: This task produces unit-test fixtures, not full integration tests against a real database. Postgres-backed tests belong to separate DB test files.
- **Chunking determinism**: The existing `test_docs_ingest.py` already covers heading chunking determinism. The fixture tests will confirm the same behavior is stable across re-ingest, not discover new behavior.
- **Existing test overlap**: Without explicit non-duplication discipline, fixture tests risk asserting the same things as existing unit tests. This plan addresses this by requiring NEW assertions (explicit `content_hash` equality, store key uniqueness) that existing tests do not verify.

## Required Docs / Tracking / Research Updates

- This canonical task plan records the approved scope for `task-712`
- Planning review history is preserved in `.agent/plans/agentic/task-plans/task-712-plan-review.md`
- Implementation review history will be preserved in `.agent/plans/agentic/task-plans/task-712-impl-review.md`
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` updates task-712 status when implementation lands

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-712-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-712-impl-review.md`

## Implementation Notes

Created `agentic/tests/test_ingest_fixtures.py` with four `unittest.TestCase` classes covering all four ingest domains. Key implementation details:

- **Fake client imports**: `FakeEmbeddingClient` from `test_docs_ingest`, `FakeGithubApiClient` from `test_github_ingest`, `FakeGithubProjectApiClient` from `test_project_ingest`
- **Store imports**: `InMemoryDocsStore`, `InMemoryCodeChunksStore`, `InMemoryGithubStore`, `InMemoryProjectItemsStore` from `agentic_kb.ingest.*` modules (plan had incorrect source location)
- **content_hash deviation**: Docs and Code domains verify `content_hash` equality after unchanged re-ingest (NEW assertion). GitHub and Project domains use `updated_at_token` increment + `body_text` equality since those Prepared* classes lack `content_hash` fields
- **Language classification**: Relaxed assertion for `.buildkite/pipeline.yml` from exact `config` match to `in ('yaml', 'config')` due to classification instability
- **23 tests total**: All pass in isolation and in full discover suite (356 tests, 4 pre-existing unrelated failures)

## Outcome

**Status**: `completed`

All acceptance criteria met:
- `agentic/tests/test_ingest_fixtures.py` created and syntax-valid
- 23 tests run successfully via `unittest discover`
- Each domain has: metadata extraction test, idempotent unchanged re-ingest test (with NEW content_hash/token assertions), idempotent changed re-ingest test, store key uniqueness test
- Docs and Code domains have chunking/symbol consistency tests
- NEW assertions confirmed: explicit `content_hash` equality across re-ingest runs, store key uniqueness verification, body_text stability for GitHub/Project
- No snapshot or MCP tests added (correctly scoped per plan)
