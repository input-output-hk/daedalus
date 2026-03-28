# Task 301 Docs Ingestion Research

- Date: 2026-03-28
- Task: `task-301`
- Evidence: `agentic/src/agentic_kb/ingest/docs.py`, `agentic/tests/test_docs_ingest.py`, live ParadeDB/Ollama verification against `docker-compose.agentic.yml`

## Durable Findings

- The approved docs allowlist was implemented as explicit glob patterns rooted at `/workspace`: `AGENTS.md`, `CLAUDE.md`, `README.md`, `.agent/readme.md`, `.agent/workflows/**/*.md`, `.agent/skills/**/*.md`, `.agent/SOPs/**/*.md`, `.agent/plans/**/*.md`, `tests/README.md`, `tests/news/README.md`, `installers/README.md`, `installers/icons/README.md`, and `source/renderer/app/themes/README.md`.
- Normalized `source_path` values should be stored as repo-relative POSIX paths only, for example `AGENTS.md` and `.agent/workflows/agentic-kb.md`; live SQL verification confirmed no `/workspace/...` paths were written.
- The minimal `doc_kind` mapping that stayed within task scope worked as: `.agent/readme.md` -> `agent_index`; `AGENTS.md` / `CLAUDE.md` -> `agent_instruction`; `.agent/workflows/**` -> `workflow`; `.agent/skills/**` -> `skill`; `.agent/SOPs/**` -> `sop`; `.agent/plans/**` -> `plan`; everything else in the allowlist -> `readme`.
- A narrow PostgreSQL write seam using `psycopg[binary]` was sufficient for `INSERT ... ON CONFLICT (source_path, chunk_index) DO UPDATE` writes into `agentic.kb_documents` without widening scope into a migration or sync framework.
- Deterministic ids and hashes were stable with `id = "docs:{source_path}#0"` and `content_hash = sha256(source_path + NUL + normalized_content)`.
- Live ParadeDB verification on a representative subset confirmed the file-level row contract: `chunk_index = 0`, `source_domain = 'docs'`, null `section_title` / `subsection_title`, empty `heading_path`, populated embeddings, stable normalized `source_path`, and in-place updates on re-ingest.
- Full approved-corpus live ingestion now succeeds on the pinned stack by keeping one stored row per file while splitting oversized documents only for embedding generation, then aggregating the segment embeddings back into one document embedding with length-weighted averaging.

## Gotchas And Constraints

- The `kb-tools` image does not include the `git` binary, so repo commit hash capture for docs ingestion must fall back to reading `.git/HEAD`, loose refs, or `packed-refs` directly when `git rev-parse HEAD` is unavailable.
- The pinned Ollama model and image can reject some whole-document inputs with `the input length exceeds the context length`, including files as small as ~3 KB in this corpus depending on content. Deterministic embedding-only segmentation at about 600 characters per segment avoided that limit in live verification without changing stored document rows.
- Segment aggregation used length-weighted averaging so the final document embedding stays deterministic and remains tied to the whole file even though the model sees smaller embedding inputs.

## Verification Notes

- Local unit coverage passed for allowlist discovery, repo-relative path normalization, title/preview shaping, deterministic ids and hashes, embedding segmentation/aggregation, rerun upserts, and git-metadata fallback.
- Containerized unit coverage passed inside `kb-tools` for both `test_docs_ingest.py` and the existing `test_embed_client.py` regression suite.
- Live SQL verification succeeded on both a representative subset and the full approved allowlist of 67 currently matched docs, proving normalized paths, `chunk_index = 0`, `source_domain = 'docs'`, populated embeddings, stable rerun row counts, and in-place updates on changed content.
