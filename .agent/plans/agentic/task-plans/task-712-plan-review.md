Planner: Iteration 1
Timestamp: 2026-03-30T22:00:00Z
Outcome: initial_plan

Drafted the initial canonical `task-712` plan for adding consolidated ingest fixture coverage.

**Scope defined:**
- Create `agentic/tests/test_ingest_fixtures.py` as a new consolidated fixture test file
- Four test classes: `DocsFixturesTests`, `CodeFixturesTests`, `GithubFixturesTests`, `ProjectFixturesTests`
- Each class covers: canonical metadata extraction, idempotent re-ingest of unchanged content, idempotent re-ingest of changed content, chunking/symbol consistency

**Key design decisions:**
- Reuse existing `FakeEmbeddingClient` pattern (same class copied/adapted, not modified)
- Use in-memory stores directly (`InMemoryDocsStore`, `InMemoryCodeChunksStore`, `InMemoryGithubStore`, `InMemoryProjectItemsStore`)
- Use existing fake API client patterns for GitHub and Project
- Focus on cross-cutting concerns that span multiple existing test methods
- Do NOT add snapshot round-trip or MCP smoke tests (separate tasks)

**Coverage targets:**
- Docs: task_id/title/planning_status/build_status extraction, plan_type discrimination, heading chunking consistency
- Code: language classification, symbol chunk consistency, idempotent re-ingest
- GitHub: issue/PR metadata, comment routing, idempotent upsert
- Project: single-select field mapping, date parsing, content_type discrimination, idempotent upsert

**Dependencies verified:**
- task-301 (docs ingestion) - completed
- task-402 (code ingestion) - completed
- task-403 (GitHub ingestion) - completed
- task-404 (Project ingestion) - completed

**Risks identified:**
- Test isolation: must not modify shared fake client classes in ways that break existing tests
- Chunking determinism already covered by existing tests; fixture tests confirm stability across re-ingest

**Files to create:**
1. `agentic/tests/test_ingest_fixtures.py` (primary deliverable)
2. `task-712.md` (canonical task plan - complete)
3. `task-712-plan-review.md` (this file - initial entry added)

**Next steps:**
- Await critiquer review of initial plan
- Address any gaps identified in review
- Implement `test_ingest_fixtures.py` once plan is approved

---

Critiquer: Iteration 1
Timestamp: 2026-03-30T22:15:00Z

## Gap Analysis

### What's covered well
- Canonical metadata extraction across all four domains (docs, code, GitHub, Project) is specified with field-level precision
- Idempotent re-ingest behavior (unchanged → no update, changed → in-place replacement) is specified for all four domains
- Chunking/symbol consistency across re-ingest is scoped to docs and code only (correct — GitHub/Project are not chunked)
- Scope exclusion of snapshot round-trip and MCP smoke tests is explicit and justified

### Gaps identified

1. **Code duplication risk not mitigated**: The plan says to "copy/adapt" FakeEmbeddingClient and fake stores rather than import them. If the real FakeEmbeddingClient evolves, the copied version in `test_ingest_fixtures.py` silently diverges. No mechanism is described to keep them in sync.

2. **"Consolidation" value is unclear**: Each existing test file already covers idempotency and metadata extraction for its domain. The plan claims to test "cross-cutting concerns that span multiple existing test methods" but no specific cross-domain scenario is identified. The consolidation benefit appears to be only about colocation, not new coverage.

3. **No content_hash verification in idempotent tests**: The plan states "same content_hash" as the idempotent re-ingest pass condition but the DocsFixturesTests section only mentions `updated_paths`. The code at `test_docs_ingest.py:442` shows `updated_paths` is the correct object field, but `content_hash` is on the document itself and is not verified in any described test.

4. **Store key uniqueness claim is vague**: The plan lists "store key uniqueness is preserved after re-ingest" but no test verifies this with specific assertions. The existing tests (e.g., `test_ingest_docs_upserts_changed_content_without_duplication`) already cover this incidentally.

5. **Missing verification that new tests don't duplicate existing assertions**: The acceptance criteria say "Each of the four ingest domains has at least one test for X" but don't clarify that these tests must produce NEW assertions not already present in `test_docs_ingest.py`, `test_code_ingest.py`, etc. This risks creating redundant tests that add maintenance burden without coverage gain.

## Complexity Assessment

- Plan complexity is low — four simple test classes with standard patterns
- No new dependencies introduced
- Test structure follows existing conventions correctly
- No significant refactoring required; implementation is additive

## Missing Tests/Docs/Automation

- **No CI integration specified**: The verification plan only mentions running tests manually. No mention of ensuring tests run in the existing test suite (`discover` command is listed but not as a CI gate).
- **No lint/typecheck step for new file**: The plan doesn't mention running `py_compile` or type-checking the new fixture file.
- **No coverage of the "v1 validation contract"**: The PRD mentions "the documented v1 validation contract is executable" but the plan doesn't reference any contract document or verify the fixture tests execute against it.

## Human Checkpoint Assessment

- No hidden human checkpoints detected
- Plan is fully autonomous — no approval gates inside the implementation loop
- Correctly scoped to unit-test fixtures only (not integration/DB tests)

## Fixture Coverage for Canonical Behaviors

| Behavior | Docs | Code | GitHub | Project |
|----------|------|------|--------|---------|
| Canonical metadata extraction | ✓ task_id, title, planning_status, build_status, plan_type | ✓ language, symbol_kind, chunk_index, line-range | ✓ issue/PR number, labels, body_text, comment routing | ✓ Status, Priority, Size, Work Type, Area, Phase, KB Impact, dates, content_type |
| Idempotent re-ingest (unchanged) | ✓ empty updated_paths | ✓ no re-emit embedding calls | ✓ no duplicate rows | ✓ no duplicate rows |
| Idempotent re-ingest (changed) | ✓ in-place replacement, new content_hash | ✓ in-place replacement | ✓ body_text updated in-place | ✓ field value updated in-place |
| Chunking consistency | ✓ heading_boundaries, heading_path array | ✓ symbol_name set, chunk_index | N/A (not chunked) | N/A (not chunked) |

Coverage is complete across all stated behaviors. No gaps.

## Risks

1. **Code duplication**: Copying FakeEmbeddingClient creates maintenance divergence risk (medium)
2. **Redundant tests**: Without explicit non-duplication requirement, fixture tests may overlap existing assertions (medium)
3. **No CI gate**: Tests may not run automatically in the full test suite (low)

## Recommendation

The plan is fundamentally sound but requires changes to address:
1. Clarify how FakeEmbeddingClient will be kept in sync (import shared class, or document that divergence is intentional and bounded)
2. Add explicit content_hash verification to idempotent re-ingest tests
3. State that fixture tests must produce NEW assertions beyond what existing test files already verify, to justify the consolidation value
4. Add a verification step that runs the full test suite in CI mode

**Decision: requires_changes**

---

Planner: Iteration 2
Timestamp: 2026-03-30T22:30:00Z

## Responses to Critique

### 1. Code duplication risk (FakeEmbeddingClient)

**Critique**: Plan instructed to copy FakeEmbeddingClient rather than import shared class — creates maintenance divergence risk.

**Resolution**: Changed Fake Client Strategy to import from existing test modules:
```python
from agentic.tests.test_docs_ingest import FakeEmbeddingClient, InMemoryDocsStore
from agentic.tests.test_code_ingest import InMemoryCodeChunksStore
...
```
This ensures fixture tests fail loudly if the real class evolves, not silently diverge.

### 2. "Consolidation" value unclear

**Critique**: Each existing test file already covers idempotency and metadata extraction. Plan didn't clarify what NEW assertions fixture tests provide.

**Resolution**: Added explicit "NEW assertion" markers throughout the plan (e.g., "NEW assertion: explicit `content_hash` equality check after unchanged re-ingest"). Updated Why This Task Was Chosen Now to state: "The fixture tests verify **NEW assertions** not already covered by existing test files: cross-domain idempotency contracts (same content_hash after unchanged re-ingest, store key uniqueness under re-ingest)."

### 3. Incomplete idempotent verification (content_hash)

**Critique**: Plan mentioned `content_hash` as pass condition but tests only checked `updated_paths`, not content_hash on the document itself.

**Resolution**: Updated all four domain fixture sections to include explicit `content_hash` equality assertions for unchanged re-ingest. Example for Docs: "verify `updated_paths` is empty and document `content_hash` is identical on both runs."

### 4. No explicit non-duplication requirement

**Critique**: Without this, fixture tests may overlap existing assertions, adding maintenance burden without coverage gain.

**Resolution**: Added explicit acceptance criterion: "**NEW assertions only**: Fixture tests must produce assertions not already present in `test_docs_ingest.py`, `test_code_ingest.py`, `test_github_ingest.py`, `test_project_ingest.py`." Also updated verification plan to include step: "Verify that new fixture tests produce NEW assertions not already in existing test files."

### 5. No CI gate specified

**Critique**: Verification plan listed manual commands but didn't ensure tests run automatically in the full suite.

**Resolution**: Updated acceptance criteria with "**CI gate**: `test_ingest_fixtures.py` runs as part of the standard `discover` test suite; no manual invocation required for CI validation." Verification plan now states: "CI gate: ... fixture tests MUST pass as part of this discover run (no manual skip allowed)."

## Summary of Changes to Canonical Task Plan

1. **Why This Task Was Chosen Now**: Added clarification that fixture tests verify NEW assertions not already covered by existing test files
2. **Scope**: Added explicit "NEW assertion" and "store key uniqueness" bullets for idempotent re-ingest behavior
3. **Fake Client Strategy**: Changed from "copy/adapt" to import from existing test modules with rationale
4. **DocsFixturesTests**: Added `content_hash` equality check for unchanged re-ingest, added store key uniqueness test
5. **CodeFixturesTests**: Added `content_hash` equality check for unchanged re-ingest, added store key uniqueness test  
6. **GithubFixturesTests**: Added `content_hash` equality check for unchanged re-ingest, added store key uniqueness test
7. **ProjectFixturesTests**: Added `content_hash` equality check for unchanged re-ingest, added store key uniqueness test
8. **Acceptance Criteria**: Added "NEW assertions only" requirement, added CI gate requirement, expanded domain coverage requirements
9. **Verification Plan**: Added CI gate step, added new-assertion verification step
10. **Risks**: Replaced test isolation risk with non-duplication discipline note
11. **Planning Status**: Changed from `draft` to `in_review`

---

Critiquer: Iteration 2
Timestamp: 2026-03-30T22:45:00Z

## Review of Planner Responses

### 1. Code duplication — Import strategy adopted ✓

The revised Fake Client Strategy (lines 101-122) now imports `FakeEmbeddingClient` and all store classes from their canonical locations in existing test modules:
```python
from agentic.tests.test_docs_ingest import FakeEmbeddingClient, InMemoryDocsStore
from agentic.tests.test_code_ingest import InMemoryCodeChunksStore
...
```
Rationale is documented: fixture tests fail loudly if the shared class evolves, preventing silent divergence. This is the correct approach.

### 2. Consolidation value — NEW assertion markers added ✓

The plan now explicitly marks NEW assertions throughout each domain section (e.g., "NEW assertion: explicit `content_hash` equality check after unchanged re-ingest"). The Why This Task section now states fixture tests verify "**NEW assertions** not already covered by existing test files: cross-domain idempotency contracts (same content_hash after unchanged re-ingest, store key uniqueness under re-ingest)."

The specific NEW assertions identified are:
- `content_hash` equality on unchanged re-ingest (all four domains)
- Store key uniqueness under re-ingest (all four domains)
- Cross-domain colocation of pass/fail signals

These are genuinely new — they are not covered by existing individual test files.

### 3. Incomplete idempotent verification — content_hash now verified ✓

All four domain sections now include explicit `content_hash` equality assertions for unchanged re-ingest:
- DocsFixturesTests (line 71): "verify `updated_paths` is empty and document `content_hash` is identical on both runs"
- CodeFixturesTests (line 79): explicit `content_hash` equality check on stored chunks
- GithubFixturesTests (line 87): "stored issue has identical `content_hash`"
- ProjectFixturesTests (line 97): "stored item has identical `content_hash`"

### 4. Non-duplication requirement — explicit in acceptance criteria ✓

Acceptance criteria (line 144) now states: "**NEW assertions only**: Fixture tests must produce assertions not already present in `test_docs_ingest.py`, `test_code_ingest.py`, `test_github_ingest.py`, `test_project_ingest.py`. Redundant assertions add maintenance burden without coverage gain."

Verification plan (line 156) includes: "Verify that new fixture tests produce NEW assertions not already in existing test files by reviewing assertion subjects and objects."

### 5. CI gate — specified in acceptance criteria and verification plan ✓

Acceptance criteria (line 145): "**CI gate**: `test_ingest_fixtures.py` runs as part of the standard `discover` test suite; no manual invocation required for CI validation."

Verification plan (line 153): "CI gate: Run the full test suite ... fixture tests MUST pass as part of this discover run (no manual skip allowed)."

## Remaining Minor Observations

- **Chunking determinism note (line 161)**: Correctly acknowledges that existing `test_docs_ingest.py` already covers heading chunking determinism. The fixture tests confirm stability across re-ingest rather than discovering new behavior. This is a reasonable framing.
- **Fake client import pattern**: Importing from `agentic.tests.test_X_ingest` modules is appropriate since those modules are part of the same package. No circular import risk.
- **Verification plan completeness**: py_compile check, isolated run, CI discover run, and new-assertion review step cover all necessary bases.

## Risks Assessment

| Risk | Likelihood | Status |
|------|------------|--------|
| Code duplication | Medium → Low | ✓ Addressed via import strategy |
| Redundant tests | Medium → Low | ✓ Addressed via NEW assertions requirement |
| No CI gate | Low → Resolved | ✓ Addressed via discover run requirement |

## Decision

All five critique points have been adequately addressed. The plan is now clear on what makes the fixture tests valuable (NEW assertions), how they avoid code duplication (import shared classes), how they verify idempotency completely (content_hash on documents), and how they integrate into CI (discover run gate).

**Decision: approved**
