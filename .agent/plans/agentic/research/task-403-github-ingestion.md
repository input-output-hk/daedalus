# Task 403 GitHub Ingestion Research

- Date: 2026-03-28
- Task: `task-403`
- Evidence: `agentic/src/agentic_kb/ingest/github.py`, `agentic/tests/test_github_ingest.py`, `docker-compose.agentic.yml`

## Durable Findings

- The accepted library-first GitHub ingestor now lives at `agentic/src/agentic_kb/ingest/github.py` and uses the Python standard library HTTP stack plus `GITHUB_TOKEN`; no `gh`, GraphQL client, or new Python dependency was needed.
- Repository issues, pull requests, repository issue comments, and PR review comments are streamed as separate page-oriented REST flows, with explicit parent-before-child handling via stream ordering plus on-demand parent resolution when a comment references a parent not already cached.
- Repository issue comments are correctly split by parent kind: true issue comments land in `kb_github_issue_comments`, while comments attached to pull requests land in `kb_github_pr_comments` with `comment_type = 'issue_comment'`.
- PR parents come from `/pulls` or canonical `/pulls/{number}` fetches only, never from issue-style PR stubs, so base/head branch fields and PR-specific metadata stay complete.
- Parent `body_text` is stored as `title + blank line + body`, with a deterministic title-only fallback when GitHub bodies are empty; comment `body_text` uses an explicit `[empty GitHub comment]` fallback so schema-required text and preview fields remain non-empty.
- Review comments map the schema-level review context columns directly from GitHub payload fields (`path`, commit ids, diff hunk, line/original_line, side, start_line, start_side) while thread/reply hints stay in `metadata`.
- The package `agentic_kb.ingest` now imports code-ingestion exports lazily enough for isolated GitHub/docs tests to run in environments that do not have `tree-sitter` installed locally, while the full package still exports code-ingestion APIs when that dependency is present.

## Verification Notes

- Local unit coverage passed for PR-stub filtering, PR-parent shaping, issue-comment vs PR-comment routing, on-demand parent resolution, review-comment column mapping, deterministic ids, preview and body fallbacks, Link-header pagination behavior, rerun upserts, result contracts, and failure paths.
- In-container unit coverage passed inside `kb-tools` for `agentic/tests/test_github_ingest.py`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed without Dockerfile changes.
- Live GitHub smoke verification did not run in this implementation session because `GITHUB_TOKEN` was absent in the current shell environment.

## Caveats

- `updated_since` now uses request-time `since` parameters for the GitHub REST streams that support them (`/issues` and `/issues/comments`), while unsupported streams such as `/pulls` and `/pulls/comments` still rely on ordered fetches plus client-side filtering; full watermark persistence and broader incremental-sync optimization remain follow-up work for later sync tasks.
- `agentic_kb.ingest.__init__` now degrades gracefully when `tree-sitter` is unavailable locally, which is useful for narrow task-specific tests but means import-time code-ingestion symbols depend on that optional runtime dependency being installed.
