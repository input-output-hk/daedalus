# Task 401 TypeScript Symbol Chunking Research

- Date: 2026-03-28
- Task: `task-401`
- Evidence: `agentic/src/agentic_kb/ingest/code.py`, `agentic/tests/test_code_ingest.py`, live ParadeDB/Ollama verification against `docker-compose.agentic.yml`

## Durable Findings

- `tree-sitter` plus `tree-sitter-language-pack` install cleanly in both a local Python 3.12 venv and the existing `python:3.12-slim-bookworm` `kb-tools` image without adding system toolchains or changing `agentic/Dockerfile`.
- The accepted task-401 allowlist is implemented as explicit globs rooted at `/workspace`: `source/common/**/*.ts`, `source/main/**/*.ts`, `source/renderer/app/**/*.ts`, and `source/renderer/app/**/*.tsx`, with `*.d.ts` and `*.scss.d.ts` excluded.
- Stored `repo_path` values remain repo-relative POSIX paths only; live SQL verification confirmed no `/workspace/...` prefixes and no declaration-file rows were written.
- The generic extractor successfully captures exported top-level `class`, `function`, `interface`, `type_alias`, `enum`, and `variable` declarations, including destructuring exports such as `export const { cluster, stateDir } = launcherConfig`, plus methods/getters/setters and function-valued `public_field_definition` class members for exported classes.
- Same-file named export lists such as `export { AnalyticsProvider }` and trailing same-file default exports such as `export default MithrilBootstrapPage` work by resolving export bindings back to declarations already present in the same file; cross-file re-export barrels remain intentionally skipped.
- Per-file replace semantics are the minimal safe write boundary for code chunks: delete all rows for a processed `repo_path`, then insert the current chunk set inside one transaction so symbol removals and renames do not leave stale rows.
- Reusing the docs-ingestion embedding segmentation helper for code chunks keeps stored rows symbol-sized while still tolerating Ollama context-length limits for large class chunks.

## Verification Notes

- Local unit coverage passed for allowlist discovery, path exclusions, TS vs TSX language tagging, export-list resolution, trailing default export resolution, destructuring variable extraction, class/member extraction, deterministic ids and hashes, per-file replace semantics, and parser-only allowlist sweeping.
- Containerized unit coverage passed inside `kb-tools` for `agentic/tests/test_code_ingest.py`.
- Parser-only sweeping succeeded both locally and in-container across 1003 current task-401 source files.
- Live ingestion smoke verification succeeded for `source/common/ipc/api.ts`, `source/main/ipc/cardano.ipc.ts`, `source/renderer/app/stores/TransactionsStore.ts`, `source/renderer/app/components/analytics/AnalyticsProvider.tsx`, and `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`, with non-null embeddings and expected language/symbol metadata in `agentic.kb_code_chunks`.

## Caveats

- Direct default exports that omit a local declaration name are currently stored with `symbol_name = 'default'`; that stays within task scope but may be worth refining later if search UX needs friendlier labels.
- Class chunks and member chunks intentionally overlap in content for now; this is accepted by the task plan and left for later ranking/deduplication work.
