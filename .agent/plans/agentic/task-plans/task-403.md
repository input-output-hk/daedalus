# Task Plan: task-403 Ingest GitHub issues, PRs, and comments

- Task ID: `task-403`
- Title: `Ingest GitHub issues, PRs, and comments`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-403` is the next unblocked critical-path GitHub-ingestion task after `task-202` and `task-501`, which already established the GitHub schema contract and reusable embedding client needed for this work.
- `task-404` depends directly on this task, and `task-502` plus `task-701` both need real GitHub history in the KB before hybrid search and real `sync github` orchestration can be validated.
- The repo now has working ingestion patterns for docs and TypeScript symbols under `agentic/src/agentic_kb/ingest/`, so this task can follow the current package layout instead of inventing a parallel ingestion path.
- That sequencing proved correct: the task shipped in the packaged `agentic_kb` layout without widening scope into sync orchestration, project ingestion, or search behavior.

## Scope

- Add the first real GitHub-history ingestor under the packaged `agentic_kb` module.
- Fetch and normalize repository issues, pull requests, issue comments, PR discussion comments, and PR review comments for `DripDropz/daedalus`.
- Write parent rows into `agentic.kb_github_issues` and `agentic.kb_github_prs`.
- Write issue-only comments into `agentic.kb_github_issue_comments` and PR comments into `agentic.kb_github_pr_comments` with `comment_type` set to either `issue_comment` or `review_comment`.
- Generate embeddings for parent items and comments using the existing Ollama client.
- Make reruns safe with deterministic ids, normalized content shaping, and upsert semantics that match the unique constraints already defined in `agentic/schema/init.sql`.
- Keep the library surface reusable so later `task-405` and `task-701` can add watermarks and sync orchestration without redesigning the ingestion core.

## Non-Goals

- Do not ingest GitHub Project 5 items or project field values; that remains `task-404`.
- Do not persist sync cursors, watermarks, or staleness state; that remains `task-405` and `task-701`.
- Do not add BM25 indexes, search queries, CLI search UX, MCP behavior, or snapshot behavior.
- Do not shell out to `gh` from `kb-tools`; this task is about repository-history ingestion inside the Python tools image, not coordination/project operations.
- Do not implement deletion detection for removed GitHub comments or closed/deleted source artifacts beyond normal upsert refreshes.
- Do not model review threads as a separate table; thread or reply relationships should stay in metadata for now.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-103` - established the packaged `agentic_kb` module layout, `/workspace` runtime contract, and the `kb-tools` image behavior.
  - `task-202` - created `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, and `kb_github_pr_comments` with the exact foreign-key and uniqueness constraints this task must satisfy.
  - `task-301` - established the first ingestion/write pattern, deterministic id hashing style, repo commit fallback behavior, and a narrow psycopg write seam.
  - `task-401` - established the current code-ingestion package layout and the pattern of library-first ingestion plus focused unit coverage.
  - `task-501` - established `agentic_kb.embed.OllamaEmbeddingClient` and the `VECTOR(384)` guardrail.
- Direct downstream tasks unblocked by this work:
  - `task-404` - project-item ingestion can align GitHub content metadata with project items once repository issues/PRs exist in the KB.
  - `task-405` - sync-state tracking can wrap this ingestion flow with watermarks and cursors once the fetch contract exists.
  - `task-502` - hybrid search needs real GitHub parent/comment rows.
  - `task-701` - `sync github` needs a real library ingestion path.
- Tracking and path inconsistencies discovered during planning:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-403.targetPath` at `agentic/src/ingest/github.py`, but repo reality is `agentic/src/agentic_kb/ingest/github.py`.
  - The same tracker still points `task-404.targetPath` at `agentic/src/ingest/project.py` and `task-405.targetPath` at `agentic/src/sync/state.py`; those look like the same pre-package-layout mismatch and should be reconciled when those tasks start.
  - The same tracker likely has the same stale package-layout problem for `task-502.targetPath` (`agentic/src/search/query.py`) and `task-701.targetPath` (`agentic/src/sync/commands.py`), which should be reconciled to the packaged `agentic/src/agentic_kb/...` layout when those tasks begin.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-201-schema-foundation.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-401-typescript-symbol-chunking.md`
  - `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `.agent/plans/agentic/task-plans/task-202.md`
  - `.agent/plans/agentic/task-plans/task-301.md`
  - `.agent/plans/agentic/task-plans/task-401.md`
  - `.agent/plans/agentic/task-plans/task-501.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/code.py`
  - `agentic/Dockerfile`
  - `agentic/pyproject.toml`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - fix `task-403.targetPath` to the packaged module path and update completion metadata when the task lands.
- `agentic/src/agentic_kb/ingest/__init__.py` - export the new GitHub ingestion surface.
- `agentic/src/agentic_kb/ingest/github.py` - GitHub REST client, normalization helpers, embedding flow, and DB write path for issues, PRs, and comments.
- `agentic/tests/test_github_ingest.py` - focused unit coverage for pagination, routing, row shaping, and rerun behavior.
- `agentic/pyproject.toml` - only if implementation proves a new dependency is unavoidable; the preferred plan is no dependency change.
- `.agent/plans/agentic/research/task-403-github-ingestion.md` - durable implementation findings gathered during execution.

## Expected Library Surface

- `agentic/src/agentic_kb/ingest/github.py` should expose the same kind of reusable library-first API shape already established in `docs.py` and `code.py`.
- Expected public dataclasses/protocols:
  - `PreparedGithubIssue`
  - `PreparedGithubIssueComment`
  - `PreparedGithubPullRequest`
  - `PreparedGithubPrComment`
  - `GithubFetchBounds`
  - `GithubPageBatch`
  - `GithubPageWriteResult`
  - `GithubStreamProgress`
  - `GithubIngestResult`
  - narrow store protocols or concrete stores for issues, PRs, issue comments, and PR comments
- Expected public functions:
  - `iter_github_pages(...) -> Iterator[GithubPageBatch]` or equivalent page-streaming generator that yields bounded normalized batches instead of requiring full-repo materialization in memory
  - `write_github_page_batch(...) -> GithubPageWriteResult` or equivalent narrow page-sized write helper used by the top-level ingest flow
  - `ingest_github(...) -> GithubIngestResult`
  - `ingest_github_from_config(...) -> GithubIngestResult`
  - optional per-stream normalization helpers for issues, PRs, issue comments, and review comments if that keeps unit tests focused without widening the public orchestration surface
- `GithubFetchBounds` should make the requested bounds explicit enough for reuse, for example repo, optional `updated_since`, page size, and optional max-pages-per-stream limits.
- `GithubStreamProgress` should be explicit enough for future sync/orchestration work and should include at minimum:
  - `pages_fetched`
  - `hit_bound`
  - `latest_source_updated_at`
  - per-entity rows written for that stream
- `GithubIngestResult` should at minimum report:
  - repo scope
  - requested bounds used by the run
  - pages fetched per stream
  - whether each stream hit a bound or truncation condition
  - latest observed `source_updated_at` values per parent/comment stream
  - per-entity row counts written across issues, issue comments, PRs, and PR comments
- The library surface should stay orchestration-friendly: later tasks should be able to pass bounded fetch parameters such as `updated_since`, `max_pages`, `page_size`, or explicit fixture streams without redesigning the ingestion core.
- The memory/write boundary should stay page-sized and explicit: normalization can materialize one fetched page plus any immediate parent-resolution lookups, but the top-level ingestion flow should not require loading the full repository history into memory before writing.

## Implementation Approach

- **First implementation step**: correct `task-403.targetPath` in `.agent/plans/agentic/knowledge-base-platform-tasks.json` to `agentic/src/agentic_kb/ingest/github.py` before any production code lands. Do not edit unrelated tracker tasks during this implementation beyond calling out the obvious stale paths noted in this plan.
- **Package placement**: implement the ingestor under `agentic/src/agentic_kb/ingest/` to match the current package layout already used by docs and code ingestion.
- **Transport choice**: use the Python standard library HTTP stack (`urllib`) against the GitHub REST API instead of adding `requests`, invoking `gh`, or introducing a GraphQL client. Repo reality today already uses standard-library HTTP in `agentic_kb.commands.status`, and the `kb-tools` image does not ship the `gh` binary.
- **Config contract**: reuse `AgenticConfig.from_env()` and require `GITHUB_TOKEN` for this ingestion path even though task-103 status treats it as optional for bootstrap. Keep the token source in env only; do not add new auth env vars unless a concrete API gap is discovered.
- **Auth/runtime note**: this task needs authenticated GitHub REST read access through `GITHUB_TOKEN`, but it does not require the `gh` binary, GitHub project scopes, or write permissions.
- **Timeout contract**: give GitHub HTTP requests a small explicit timeout, for example 15-30 seconds per request, and keep that timeout configurable through a narrow library parameter or module constant so later sync tasks can reuse it without redesign.
- **Default repo scope**: parameterize the ingestor by repository string but default it to `DripDropz/daedalus` so tests can inject fixtures while operator behavior stays aligned with the project anchors.
- **REST endpoint plan**: use separate repository-wide streams so the schema split stays explicit:
  - issues: `/repos/{owner}/{repo}/issues?state=all&sort=updated&direction=asc&per_page=100&page=n`
  - pull requests: `/repos/{owner}/{repo}/pulls?state=all&sort=updated&direction=asc&per_page=100&page=n`
  - issue comments: `/repos/{owner}/{repo}/issues/comments?sort=updated&direction=asc&per_page=100&page=n`
  - PR review comments: `/repos/{owner}/{repo}/pulls/comments?sort=updated&direction=asc&per_page=100&page=n`
- **Pagination contract**: implement Link-header pagination with deterministic page ordering and optional bounded parameters such as `max_pages` or `updated_since` for tests and future sync reuse. This task should expose those controls at the library boundary without persisting sync state.
- **Streaming contract**: reconcile fetching with the page-sized write goal by making the ingestion flow page-oriented. The fetch layer should yield one normalized page batch at a time, and the write layer should flush each page batch after parent-before-child ordering is satisfied for that page. This keeps memory bounded and matches the stated transaction strategy.
- **Issue-parent contract**: fetch issues from the issues endpoint and explicitly drop rows that contain a `pull_request` marker so `kb_github_issues` stores issues only.
- **PR-parent contract**: fetch PR parents from the pulls endpoint, not from issue stubs, so base/head branch data, merge timestamps, draft state, and PR-specific metadata are available without follow-up reshaping.
- **Parent text shaping**: normalize GitHub markdown bodies to LF line endings and build the searchable parent text from title plus body. If the raw body is empty, keep `body_text` non-empty by using a deterministic title-only fallback and record the empty-body fact in metadata.
- **Preview shaping**: reuse the existing deterministic preview style from docs/code ingestion: normalize whitespace, keep previews short and stable, and avoid empty preview text.
- **Id contract**: use deterministic stable ids derived from normalized source identity, for example issue parent by repo plus issue number, PR parent by repo plus PR number, issue comment by repo plus GitHub comment id, and PR comment by repo plus `comment_type` plus GitHub comment id.
- **Hash boundary**: do not add a top-level `content_hash` contract for GitHub tables because `agentic/schema/init.sql` does not define one for these entities. If implementation needs a deterministic change fingerprint for debugging or future sync support, keep it inside `metadata` only and document that choice consistently as a metadata detail rather than a schema contract.
- **Embedding contract**: use `agentic_kb.embed.OllamaEmbeddingClient` for issues, PRs, and both comment families. Reuse the docs-ingestion embedding segmentation helper so long GitHub bodies do not fail on model context length.
- **Parent/comment ordering**: preserve foreign-key safety by ensuring parent issues and parent PRs are written before any child comments that reference them.
- **Stream ordering note**: process the parent streams before the comment streams during a run, and keep on-demand parent resolution available as a safety net rather than the primary ordering model.
- **Issue-comment routing**: repository issue comments come from one shared GitHub stream that includes comments on both issues and PRs. Route each comment by parent number:
  - if the parent is an issue, store it in `kb_github_issue_comments`
  - if the parent is a PR, store it in `kb_github_pr_comments` with `comment_type = 'issue_comment'`
- **On-demand parent resolution**: do not assume the parent cache is complete. For repository issue comments, detect parent kind from the comment `issue_url`. If the parent is not already cached:
  - resolve the issue payload via the issue URL and treat it as an issue parent only when the payload does not contain a `pull_request` marker
  - when the payload indicates a PR, resolve the canonical PR parent through `/repos/{owner}/{repo}/pulls/{number}` before inserting the child comment so PR discussion comments always anchor to `kb_github_prs`
  - never treat the issue-style PR stub payload as sufficient parent data for `kb_github_prs`
- **Review-comment mapping**: map `/pulls/comments` rows into `kb_github_pr_comments` with `comment_type = 'review_comment'` and populate the schema-level review context columns directly from GitHub fields such as `path`, `commit_id`, `original_commit_id`, `diff_hunk`, `line`, `original_line`, `side`, `start_line`, and `start_side`.
- **Review-comment parent safety**: PR review comments should normally land after the PR parent stream has been processed, but if a bounded run or cache miss exposes a missing parent, the implementation may resolve the canonical PR parent on demand before inserting the review comment.
- **Metadata shape**: keep source-specific extras in `metadata` rather than widening schema. Recommended metadata includes:
  - issues: `author_association`, `assignees`, `milestone`, `comments_count`, `state_reason`, `locked`
  - PRs: `draft`, `merged`, `merge_commit_sha`, `requested_reviewers`, `review_comments_count`, `commits`, `changed_files`
  - issue comments: parent issue url/number resolution facts if useful for debugging
  - review comments: `in_reply_to_id`, `pull_request_review_id`, `position`, `subject_type`, reply/thread hints
  - optional deterministic fingerprints, if used at all, should live only inside `metadata`
- **Write semantics**: use narrow store classes similar to docs/code ingestion:
  - parent issue store: `INSERT ... ON CONFLICT (repo, issue_number) DO UPDATE`
  - parent PR store: `INSERT ... ON CONFLICT (repo, pr_number) DO UPDATE`
  - issue comment store: `INSERT ... ON CONFLICT (repo, github_comment_id) DO UPDATE`
  - PR comment store: `INSERT ... ON CONFLICT (repo, comment_type, github_comment_id) DO UPDATE`
  Page-sized or stream-sized transactions are preferred over one giant repository transaction.
- **Result accounting contract**: every streamed page write should contribute to a final `GithubIngestResult` accumulator so later tasks can inspect exactly which bounds were used, how many pages each stream fetched, whether a bound truncated the run, the latest observed source timestamp per stream, and how many rows were written per entity family.
- **Deletion boundary**: reruns must refresh changed rows in place, but deleted GitHub comments, force-pushed review-comment disappearances, and historical tombstones remain out of scope for this task.
- **Operator boundary**: keep this implementation library-first. Do not add a real `agentic-kb sync github` command yet; `task-701` still owns CLI orchestration.
- **Error model**: introduce task-local GitHub ingestion exceptions for missing token, non-2xx responses, malformed JSON, pagination contract violations, and impossible parent/comment routing states. Fail loudly rather than silently skipping malformed payloads.
- **Testing strategy**: mirror the style used by `task-301`, `task-401`, and `task-501`:
  - stub HTTP responses for deterministic unit tests
  - use in-memory stores for routing and upsert behavior tests
  - reserve live GitHub verification for a bounded smoke pass only if network and token are available

## Acceptance Criteria

- A GitHub ingestor exists at `agentic/src/agentic_kb/ingest/github.py` in the current packaged module layout.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated during implementation so `task-403.targetPath` points at `agentic/src/agentic_kb/ingest/github.py`.
- The ingestor can fetch and normalize repository issues, PRs, issue comments, PR discussion comments, and PR review comments for `DripDropz/daedalus` using `GITHUB_TOKEN` plus standard HTTPS calls from Python.
- `kb_github_issues` stores issues only and does not persist PR issue stubs from the GitHub issues endpoint.
- `kb_github_prs` stores pull-request parents with PR-specific metadata including base/head branch fields and merged/closed timestamps when present.
- Issue comments on real issues land in `kb_github_issue_comments`, while issue comments attached to PRs land in `kb_github_pr_comments` with `comment_type = 'issue_comment'`.
- Inline PR review comments land in `kb_github_pr_comments` with `comment_type = 'review_comment'` and populate the review-context columns defined in `agentic/schema/init.sql`.
- Inserted parent and comment rows populate the schema-required fields at minimum: stable `id`, repo identifier, source number/id fields, normalized text fields, preview text, source timestamps, embeddings, and metadata.
- Parent rows are written before child comments so the existing foreign keys stay valid during ingestion.
- The library surface includes concrete reusable entry points and result contracts for downstream sync/orchestration tasks, with a page-oriented boundary rather than full-repo materialization: `iter_github_pages`, `write_github_page_batch`, `ingest_github`, `ingest_github_from_config`, `GithubFetchBounds`, `GithubStreamProgress`, and `GithubIngestResult`.
- Deterministic ids are stable across reruns.
- Re-running ingestion updates existing rows in place through the existing natural-key uniqueness constraints without duplicate-row failures.
- `GithubIngestResult` concretely reports requested bounds, pages fetched per stream, whether a bound was hit for each stream, latest observed `source_updated_at` values per stream, and per-entity row counts written.
- The implementation remains library-first and does not add real `sync github`, project ingestion, sync-state persistence, search queries, or snapshot behavior.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the Compose contract still resolves after any package changes.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so verification uses the current installed package.
- Add focused automated coverage in `agentic/tests/test_github_ingest.py` and run it locally and in-container, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_github_ingest.py'` plus the in-container equivalent using `--entrypoint python` because `kb-tools` keeps `ENTRYPOINT ["agentic-kb"]`.
- Cover at least these automated cases with stubbed GitHub responses:
  - filtering PR stubs out of the issues parent stream
  - mapping PR parents from the pulls stream
  - routing repository issue comments to issue-comment vs PR-comment tables based on parent kind
  - on-demand parent resolution from `issue_url`, including canonical PR parent resolution through `/pulls/{number}` before inserting PR discussion comments
  - review-comment mapping into the dedicated PR review columns
  - deterministic ids, preview shaping, and non-empty body fallbacks
  - pagination through Link headers
  - rerun upsert behavior against the natural-key constraints for all four tables
  - downstream-facing result-contract assertions for `GithubFetchBounds`, `GithubStreamProgress`, `GithubPageWriteResult`, and `GithubIngestResult`, including requested bounds used, per-stream page counts, `hit_bound` or truncation flags, latest observed `source_updated_at` per stream, and accumulated per-entity row counts written
  - missing-token and malformed-response failure paths
- Start an isolated live verification stack only if a valid `GITHUB_TOKEN` is available, for example `AGENTIC_DB_PORT=5753 OLLAMA_PORT=11446 docker compose -p agentic-task-403 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools`, then wait for ParadeDB's known first-boot restart handoff before DB checks.
- Run a bounded live smoke command through `--entrypoint python` that ingests a small page-limited subset for `DripDropz/daedalus` into the live DB. The smoke path should exercise parents and comments without depending on the full repository history.
- Verify with SQL that representative rows exist in `agentic.kb_github_issues`, `agentic.kb_github_prs`, `agentic.kb_github_issue_comments`, and `agentic.kb_github_pr_comments`, with non-null embeddings and expected `comment_type` values.
- Verify with SQL that no issue-parent row has a PR stub marker persisted as an issue and that PR discussion comments and review comments remain separated by `comment_type` under `kb_github_pr_comments`.
- Re-run the bounded live smoke ingestion once more and confirm row counts for the same source subset stay stable while `updated_at` refreshes in place.
- Tear down the isolated verification stack with `docker compose -p agentic-task-403 -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- **Tracker mismatch**: `task-403.targetPath` is still stale in `.agent/plans/agentic/knowledge-base-platform-tasks.json`, and `task-404`, `task-405`, `task-502`, and `task-701` appear to have the same pre-package-layout problem. This plan notes the mismatch, but the tracker itself must be fixed during implementation.
- **`gh` vs API boundary**: the platform uses `gh` CLI for coordination and project operations, but `kb-tools` does not include `gh`. This task should use GitHub HTTPS APIs directly inside Python and avoid widening scope into image-level CLI installation.
- **Incremental-sync fit**: repository issues and issue comments support `since`, but the pulls list endpoint does not provide the same simple watermark parameter. The ingestion library should therefore expose optional bounds and parent re-fetch helpers now so `task-405` can layer watermarks without redesign.
- **Comment routing correctness**: PR discussion comments arrive through the shared issue-comments stream and must be routed by parent kind. The implementation must prove this with tests so PR discussion comments do not land in the issue-comment table.
- **Thread fidelity**: review-thread hierarchy is not modeled as a first-class table today. Metadata can preserve reply and review ids, but deeper thread-aware retrieval may need later follow-up work if search UX demands it.
- **GitHub API variability**: live repo history changes over time, so unit tests must rely on fixtures and stubs rather than mutable record counts from `DripDropz/daedalus`.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` when implementation starts to fix `task-403.targetPath`, and update it again when the task is completed.
- Record durable findings in `.agent/plans/agentic/research/task-403-github-ingestion.md`, including the chosen GitHub API contract, parent/comment routing rules, accepted metadata shape, and any incremental-sync caveats discovered during implementation.
- If implementation reveals a durable operator-facing contract change for `GITHUB_TOKEN`, GitHub sync expectations, or workflow guidance, update `.agent/workflows/agentic-kb.md` or related agent docs only after that behavior is actually implemented.
- When `task-404` and `task-405` are picked up, reconcile their apparent pre-package-layout `targetPath` values before code lands; this plan intentionally records the mismatch without editing those tracker entries now.

## Implementation Notes

- Corrected `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-403.targetPath` now points at `agentic/src/agentic_kb/ingest/github.py`, and marked `task-403` completed in the tracker.
- Added `agentic/src/agentic_kb/ingest/github.py` with a library-first, page-oriented GitHub ingestor that fetches issues, pull requests, repository issue comments, and PR review comments through the GitHub REST API using standard-library HTTP plus `GITHUB_TOKEN`.
- Implemented the requested reusable result and bounds contracts: `GithubFetchBounds`, `GithubPageBatch`, `GithubPageWriteResult`, `GithubStreamProgress`, `GithubIngestResult`, `iter_github_pages`, `write_github_page_batch`, `ingest_github`, and `ingest_github_from_config`.
- Parent text shaping stores `title + body` for issues and PRs with deterministic title-only fallback for empty bodies, while comments use a deterministic non-empty fallback string so schema-required text and preview columns remain populated.
- Repository issue comments are routed by parent kind: issue comments land in `kb_github_issue_comments`, PR discussion comments land in `kb_github_pr_comments` with `comment_type = 'issue_comment'`, and review comments from `/pulls/comments` land in `kb_github_pr_comments` with `comment_type = 'review_comment'` plus direct review-context column mapping.
- On-demand parent resolution now fetches missing issue parents from the issue URL and missing PR parents from canonical `/pulls/{number}` endpoints before child writes, so bounded or comment-only runs can still satisfy the existing foreign keys.
- `GithubFetchBounds.updated_since` is now sent as a request-time `since` parameter for the GitHub REST streams that support it (`/issues` and `/issues/comments`), while unsupported streams still use post-fetch filtering to preserve a uniform bounds contract.
- Added `PostgresGithubStore` and `InMemoryGithubStore` with natural-key upsert semantics for all four GitHub tables, and exported the new GitHub ingest surface from `agentic/src/agentic_kb/ingest/__init__.py`.
- Added focused unit coverage in `agentic/tests/test_github_ingest.py` and recorded durable implementation findings in `.agent/plans/agentic/research/task-403-github-ingestion.md`.
- Adjusted `agentic/src/agentic_kb/ingest/__init__.py` to import code-ingestion exports lazily only for the optional `tree_sitter` / `tree_sitter_language_pack` dependency gap, so narrow docs/GitHub tests can still run locally without masking unrelated import failures.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/ingest/github.py" "agentic/src/agentic_kb/ingest/__init__.py" "agentic/tests/test_github_ingest.py"` passed.
- Local unit tests passed: `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_github_ingest.py'`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- In-container unit tests passed: `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_github_ingest.py'`.
- Focused regression coverage now also verifies request-time `updated_since` handling and confirms comment-stream `latest_source_updated_at` stays pinned to comment timestamps even when on-demand parent resolution fetches newer parent rows.
- Record reconciliation completed cleanly after implementation review: tracker state, packaged target path, implementation evidence, and research notes now agree on the shipped behavior.
- Live GitHub smoke verification remains blocked because `GITHUB_TOKEN` was absent in the current environment, so the bounded live ingestion and SQL checks in this plan are still the only unverified portion of the original review loop.

## Outcome

- `task-403` is implemented within the approved scope as a reusable ingestion library under the packaged `agentic_kb` layout.
- No real `sync github` CLI orchestration, project ingestion, sync-state persistence, search changes, or snapshot behavior were added.
- No additional operator-facing workflow or project metadata update was applicable from repo state during this task: the durable shipped changes are the packaged ingestor, tests, tracker completion, and research/task-plan reconciliation only.
- The review loop ended cleanly aside from the still-blocked live smoke verification against GitHub and ParadeDB once a valid `GITHUB_TOKEN` is available.

## Planning Status Rationale

- This plan remains `approved` because the implemented work follows the approved package placement, routing rules, result contracts, and library-first boundary without widening scope.
- Build status is now `completed` because the implementation, focused unit coverage, compose validation, and container build/test verification are all in place; only the optional live GitHub smoke pass remains blocked by missing credentials rather than missing code.
