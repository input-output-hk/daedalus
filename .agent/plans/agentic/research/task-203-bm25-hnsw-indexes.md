# Task 203 BM25 and HNSW Indexes Research

- Date: 2026-03-28
- Task: `task-203`
- Evidence: `agentic/schema/create_indexes.sql`, `agentic/schema/init.sql`, `docker-compose.agentic.yml`, `.agent/plans/agentic/task-plans/task-203.md`

## Durable Findings

- The pinned ParadeDB image accepted the task-203 SQL exactly in the shipped form: `CREATE INDEX IF NOT EXISTS ... USING bm25 (...) WITH (key_field = 'id')` for ParadeDB BM25 indexes, and `CREATE INDEX IF NOT EXISTS ... USING hnsw (embedding vector_cosine_ops) WHERE embedding IS NOT NULL` for pgvector ANN indexes on `VECTOR(384)` columns.
- ParadeDB still enforces one BM25 index per table, so the v1 design must treat the BM25 column set as a durable budget. The accepted task-203 contract narrows each table to core search text plus the smallest already-committed scalar filters.
- The accepted stable naming convention is:
  - BM25: `<table_name>_bm25_idx`
  - HNSW: `<table_name>_embedding_hnsw_idx`
- The final BM25 column sets are:
  - `kb_documents`: `id`, `source_domain`, `doc_kind`, `source_path`, `title`, `preview_text`, `content`
  - `kb_code_chunks`: `id`, `repo_path`, `language`, `symbol_name`, `symbol_kind`, `preview_text`, `content`
  - `kb_github_issues`: `id`, `repo`, `issue_number`, `state`, `title`, `preview_text`, `body_text`
  - `kb_github_issue_comments`: `id`, `repo`, `issue_number`, `preview_text`, `body_text`
  - `kb_github_prs`: `id`, `repo`, `pr_number`, `state`, `title`, `preview_text`, `body_text`
  - `kb_github_pr_comments`: `id`, `repo`, `pr_number`, `comment_type`, `repo_path`, `preview_text`, `body_text`
  - `kb_project_items`: `id`, `project_owner`, `project_number`, `content_type`, `repo`, `status`, `priority`, `work_type`, `area`, `phase`, `kb_impact`, `title`, `body_text`
- The durable vector-search contract is one partial cosine HNSW index per searchable table on `embedding`, excluding null vectors. Downstream query work should assume cosine-distance semantics and table-level search behavior, not hard dependencies on index names for correctness.
- `agentic/schema/init.sql` remains the single fresh-bootstrap root entrypoint. Task-203 keeps `agentic/schema/create_indexes.sql` as the task-owned standalone/manual-apply file and uses `\ir includes/create_indexes.task-203.sql` from `init.sql` to source it during first boot.

## Verification Caveats

- Docker init scripts under `/docker-entrypoint-initdb.d/` still run only on first database initialization. Updating `agentic/schema/create_indexes.sql` or `agentic/schema/init.sql` does not retrofit an already initialized local DB volume by itself.
- The durable operator paths for task-203 are:
  - `fresh volume`: boot from `docker-compose.agentic.yml` and let `agentic/schema/init.sql` source the mounted include file during first boot
  - `manual apply`: run `psql -f agentic/schema/create_indexes.sql` against a running task-202-era DB to add the task-203 index phase and migration row `3`
- A real upgrade-path verification must start from a DB bootstrapped with the task-202-era `agentic/schema/init.sql`, confirm that only migration rows `1` and `2` exist and that no standalone BM25/HNSW indexes are present, then apply `agentic/schema/create_indexes.sql` manually. Re-running the same standalone file after that is only the idempotence check.
- ParadeDB still has the known first-boot restart handoff, so verification should wait for a successful `psql` command instead of trusting the first healthy signal alone.

## No New Research Beyond Task Scope

- No additional schema redesign, query-layer behavior, filter-registry behavior, or migration-runner automation was required. The durable outcome is the verified SQL/index contract and the explicit fresh-bootstrap versus manual-apply boundary.
