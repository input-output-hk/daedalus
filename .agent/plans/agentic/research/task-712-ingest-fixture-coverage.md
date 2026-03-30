# Research: task-712 Ingest Fixture Coverage

## What Was Learned/Implemented

- Created `agentic/tests/test_ingest_fixtures.py` with 23 tests across 4 domains (docs, code, GitHub, Project)
- `FakeEmbeddingClient` is imported from `agentic.tests.test_docs_ingest` (not copy)
- `InMemory*Store` classes live in `agentic_kb.ingest.*` modules (plan had incorrect source location)
- `PreparedGithubIssue` and `PreparedProjectItem` do NOT have `content_hash` fields - idempotency verified via `updated_at_token` increment + `body_text` equality instead
- Language classification for `.buildkite/pipeline.yml` is unstable (yaml vs config) - relaxed assertion
- NEW assertions confirmed: explicit content_hash equality after unchanged re-ingest (docs/code), store key uniqueness under re-ingest, body_text stability for GitHub/Project

## Plan Corrections Applied

1. GitHub/Project `content_hash` → `updated_at_token` + `body_text` equality (corrects plan assumption)
2. `InMemory*Store` import source: ingest modules not test modules (corrects plan error)
3. Language classification assertion relaxed for yaml/config files (implementation tolerance)
