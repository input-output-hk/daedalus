# Task Plan: task-401 Implement TypeScript symbol chunking

- Task ID: `task-401`
- Title: `Implement TypeScript symbol chunking`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-401` is the next critical-path code-ingestion task after `task-202`, `task-301`, and `task-501`, which already established the code-chunk schema, the current Python package layout, the first ingestion pattern, and a working embedding client.
- `task-402` depends on a stable symbol-aware chunk contract before it can widen coverage to the whole repository, and `task-502` needs real code chunks before hybrid search quality can be evaluated against Daedalus source.
- The highest-value agent navigation targets in Daedalus are TypeScript-heavy and already visible in repo reality: IPC contracts under `source/common/ipc/`, IPC handlers under `source/main/ipc/`, and MobX stores plus React renderer code under `source/renderer/app/`.

## Scope

- Add the first real TypeScript/TSX code ingestor inside the packaged `agentic_kb` Python module.
- Discover and chunk an initial high-value code subset only: `source/common/**/*.ts`, `source/main/**/*.ts`, `source/renderer/app/**/*.ts`, and `source/renderer/app/**/*.tsx`.
- Parse source with a TypeScript-aware AST approach and emit symbol chunks for exported top-level declarations plus member symbols of exported classes.
- Support both direct export syntax (`export const ...`, `export default class ...`) and same-file named export lists (`export { AnalyticsProvider }`) when the export target resolves to a declaration in the same file.
- Write chunk rows into `agentic.kb_code_chunks` with embeddings, deterministic ids, normalized repo-relative paths, symbol metadata, and safe per-file re-ingest behavior.

## Non-Goals

- Do not widen discovery to the entire repository, `storybook/**`, `tests/**`, selected `utils/**`, or non-TS source families; that remains `task-402`.
- Do not add generic whole-file or arbitrary line-window fallback chunks for files with no supported symbols; this task is specifically about symbol-aware chunking.
- Do not add Daedalus-specific structured extractors for IPC channels, MobX stores, or React components beyond generic symbol metadata; those remain later follow-up work.
- Do not add CLI orchestration for `sync code`, sync-state writes, deleted-file cleanup across the repo, stale-index detection, or search-query behavior; those remain `task-405`, `task-701`, and `task-502`.
- Do not add BM25/HNSW indexes, search config, MCP behavior, or snapshot logic.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-101` - established the Compose stack and runtime environment contract in `docker-compose.agentic.yml`
  - `task-103` - established the packaged `agentic_kb` service image, `/workspace` repo mount, and current CLI/runtime layout
  - `task-202` - created `agentic.kb_code_chunks` and the core schema contract this task must write into
  - `task-301` - established the current ingestion style, deterministic path handling, repo commit fallback logic, and ParadeDB write seam pattern in the `agentic_kb` package
  - `task-501` - established the reusable Ollama embedding client and the `VECTOR(384)` guardrail
- Direct downstream tasks unblocked by this work:
  - `task-402` - widen code ingestion coverage to the full repository without redesigning chunk format
  - `task-502` - make real code chunks available to hybrid BM25/vector/RRF search
  - `task-701` - wire real `sync code` orchestration on top of the library path introduced here
- Tracking mismatch to correct during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-401.targetPath` at `agentic/src/ingest/code.py`, but repo reality after `task-103` is `agentic/src/agentic_kb/ingest/code.py`. Correct that path before code lands.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`
  - `.agent/plans/agentic/task-plans/task-202.md`
  - `.agent/plans/agentic/task-plans/task-301.md`
  - `.agent/plans/agentic/task-plans/task-501.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/pyproject.toml`
  - `agentic/Dockerfile`
  - `source/common/ipc/api.ts`
  - `source/main/ipc/cardano.ipc.ts`
  - `source/main/ipc/index.ts`
  - `source/renderer/app/stores/TransactionsStore.ts`
  - `source/renderer/app/components/analytics/AnalyticsProvider.tsx`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-401.targetPath` to the real packaged module path before implementation
- `agentic/src/agentic_kb/ingest/__init__.py` - package surface for code-ingestion helpers
- `agentic/src/agentic_kb/ingest/code.py` - TypeScript/TSX discovery, parsing, symbol extraction, embedding, and DB write path
- `agentic/tests/test_code_ingest.py` - focused unit coverage for symbol extraction, export resolution, row shaping, and re-ingest behavior
- `agentic/tests/fixtures/code/` - minimal TS/TSX fixtures for parser and chunk-shape tests if inline strings are not enough
- `agentic/pyproject.toml` - add the smallest practical AST-parser dependency set for TypeScript/TSX parsing

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-401.targetPath` matches `agentic/src/agentic_kb/ingest/code.py` before any production code lands.
- **Package placement**: implement the ingestor under `agentic/src/agentic_kb/ingest/` to match the package layout already established by `task-103`, `task-301`, and `task-501`.
- **Shared-helper reuse**: reuse narrow ingestion utilities from `task-301` where they already fit this task, especially repo-relative POSIX path normalization, repo commit hash fallback, deterministic preview shaping patterns, and embedding-only segmentation helpers. If a helper must be shared, extract the minimal common utility without changing docs-ingestion behavior.
- **Parser choice**: use a Python-native `tree-sitter` stack so `kb-tools` remains a Python service with no added system toolchains. The intended package set for `python:3.12-slim-bookworm` is:
  - `tree-sitter`
  - `tree-sitter-language-pack`
  These packages must provide TypeScript and TSX grammars via wheels or otherwise install cleanly with plain `pip install` in the current image. Avoid introducing a Node runtime, `npm`, `ts-morph`, or compiler toolchains for this task.
- **Dependency boundary**: keep `agentic/Dockerfile` unchanged if at all possible. If the named parser packages cannot be installed cleanly in the current image without adding build-essential or other system packages, stop and document that as a blocking implementation finding rather than silently widening scope into image/toolchain rework.
- **Initial source-set contract**: centralize the first code-ingestion allowlist in code and keep it narrower than `task-402`:
  - `source/common/**/*.ts`
  - `source/main/**/*.ts`
  - `source/renderer/app/**/*.ts`
  - `source/renderer/app/**/*.tsx`
  - explicit exclusions: `**/*.d.ts`, `**/*.scss.d.ts`, and any generated declaration artifacts
- **Path contract**: normalize every discovered file to a repo-relative POSIX `repo_path`, for example `source/common/ipc/api.ts` and `source/renderer/app/components/analytics/AnalyticsProvider.tsx`. Never store `/workspace/...` paths or native separator variants.
- **Language contract**: set `language` to `typescript` for `.ts` files and `typescriptreact` for `.tsx` files so later search/UI work can filter TSX separately without inventing a second schema.
- **Supported symbol kinds**: emit chunks for these declaration families only:
  - exported top-level `class`, `function`, `interface`, `type_alias`, `enum`, and variable declarations (`const` / `let` / `var`)
  - exported default declarations when their target declaration is in the same file
  - methods, getters, setters, and function-valued class fields inside exported classes
  - static methods and static function-valued class fields under the same member-chunk rules
  - non-function-valued class properties remain out of scope for this task
- **Class-member contract**: treat a class field as chunkable only when its initializer is function-like, for example arrow-function properties such as `_refreshTransactionData = async () => {}` and concise function expressions assigned to a field. This keeps `TransactionsStore.ts` and similar MobX stores in scope without widening into every observable/property declaration.
- **Export resolution contract**: recognize export status from inline export syntax, same-file named export lists, and trailing same-file default exports such as `class MithrilBootstrapPage ...; export default MithrilBootstrapPage;`. Re-export barrels that only forward symbols from another module are out of scope; this task should skip those rather than invent synthetic source ownership.
- **Chunk granularity**: emit one chunk for each supported top-level exported symbol. For exported classes, also emit child chunks for supported member symbols with `parent_symbol_name` and `parent_symbol_kind` populated. Accept the resulting class/member overlap for now; keep that duplication explicit in metadata rather than trying to solve deduplication in this task.
- **Chunk ordering and ids**: assign `chunk_index` by lexical order within each file after extraction. Use deterministic ids derived from normalized `repo_path` plus `chunk_index`, for example `code:source/common/ipc/api.ts#7`.
- **Preview and hash contract**: persist the exact symbol source text in `content`, derive a deterministic short `preview_text`, and compute `content_hash` from normalized `repo_path`, symbol identity metadata, and normalized chunk content so re-ingest comparisons remain stable.
- **Line mapping**: populate `start_line` and `end_line` from the AST node span using 1-based source lines. Store any additional source-shape facts such as `is_default_export`, `exported_via_list`, `symbol_path`, or `has_member_chunks` in `metadata` rather than widening schema.
- **Representative symbol expectations**: the generic extractor should be able to capture:
  - channel constants and type aliases from `source/common/ipc/api.ts`
  - IPC channel declarations from `source/main/ipc/cardano.ipc.ts`
  - the exported class plus member getters and function-valued class fields from `source/renderer/app/stores/TransactionsStore.ts`
  - a TSX export-list component like `source/renderer/app/components/analytics/AnalyticsProvider.tsx`
  - a decorated TSX class exported via trailing default export like `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`
- **Embedding behavior**: use `agentic_kb.embed.OllamaEmbeddingClient` for each chunk. If a large class chunk exceeds model context limits, reuse the same embedding-only segmentation pattern already proven in `task-301`, either by importing the existing helper or by extracting a minimal shared helper without changing docs-ingestion behavior.
- **Repo commit contract**: reuse the commit-hash capture pattern already established in `task-301`, including the `.git` fallback path when the `git` binary is unavailable inside `kb-tools`.
- **Database write semantics**: do not rely on row-level upserts alone. Because a file's symbol count and order can change between runs, this task should use per-file replace semantics inside a transaction: delete existing `agentic.kb_code_chunks` rows for the processed `repo_path`, then insert the current chunk set for that file. This keeps re-ingested files accurate without expanding scope into repo-wide deleted-file cleanup.
- **Deletion boundary**: per-file stale-chunk cleanup for processed files is in scope. Detecting repo files that disappeared from the source set entirely and removing their rows is not in scope for `task-401`; that remains later sync work.
- **Invocation boundary**: implement this as library functionality, not as a new CLI surface. Verification can call the module directly with `python -c`. `agentic-kb sync code` remains a placeholder until `task-701`.
- **Failure boundary**: fail loudly on parser errors in the selected source set rather than silently falling back to line windows. To de-risk that behavior, verification must include a parser-only sweep across the full task-401 allowlist before live DB ingestion checks, so syntax compatibility is proven broadly even though live end-to-end embedding verification remains narrower.
- **Testing approach**: add focused unit tests for path discovery, `.d.ts` exclusion, TS vs TSX language tagging, export-list resolution, trailing same-file default export resolution, decorated class parsing, class/member extraction including function-valued class fields, deterministic ids and hashes, line-number mapping, parser-only allowlist sweeping, and per-file replace semantics on re-ingest. Use fixture-backed snippets for parser edge cases and reserve live ParadeDB/Ollama verification for a small representative subset of real repo files.

## Acceptance Criteria

- A code ingestor exists under `agentic/src/agentic_kb/ingest/code.py` in the current packaged module layout.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated before implementation so `task-401.targetPath` points at `agentic/src/agentic_kb/ingest/code.py`; this tracker fix is a required artifact of the task.
- The initial discovery scope is limited to `source/common/**/*.ts`, `source/main/**/*.ts`, `source/renderer/app/**/*.ts`, and `source/renderer/app/**/*.tsx`, with `*.d.ts` and generated declaration files excluded.
- Stored `repo_path` values are normalized repo-relative POSIX paths only, never `/workspace/...` paths.
- The ingestor writes rows into `agentic.kb_code_chunks` with the schema-required fields populated at minimum: `id`, `repo_path`, `language`, `chunk_index`, `start_line`, `end_line`, `content`, `preview_text`, `content_hash`, `repo_commit_hash`, and `embedding`.
- The parser dependency choice is concretely `tree-sitter` plus `tree-sitter-language-pack`, installed without changing `agentic/Dockerfile` or adding system build toolchains.
- Supported exported symbol kinds from this plan are captured from real TS/TSX files, including inline exports, same-file export-list declarations, and trailing same-file default exports.
- Member chunks for methods/getters/setters and function-valued class fields inside exported classes populate `parent_symbol_name` and `parent_symbol_kind` correctly.
- The generic chunker can successfully capture representative Daedalus symbols from `source/common/ipc/api.ts`, `source/main/ipc/cardano.ipc.ts`, `source/renderer/app/stores/TransactionsStore.ts`, `source/renderer/app/components/analytics/AnalyticsProvider.tsx`, and `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`.
- A parser-only sweep succeeds across the full task-401 allowlist, proving the selected parser can read all in-scope files before the narrower live end-to-end smoke ingestion is considered passing.
- Re-ingesting a processed file after its symbol set changes replaces that file's stored chunks without leaving stale rows for the same `repo_path`.
- Repo-wide deleted-file cleanup, full-repo discovery beyond this task's allowlist, special IPC/MobX extractors, sync-state writes, and search-query logic are not introduced.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the Compose contract still resolves after any package changes.
- Rebuild the tools image with `docker compose -f docker-compose.agentic.yml build kb-tools` so parser dependencies and package code are current.
- Run focused unit tests locally or in-container against the new suite, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_code_ingest.py'` and the in-container equivalent using `--entrypoint python` because `kb-tools` keeps `ENTRYPOINT ["agentic-kb"]`.
- Run a parser-only sweep against the full task-401 allowlist before any live DB smoke verification. This sweep should discover every in-scope `source/common/**/*.ts`, `source/main/**/*.ts`, `source/renderer/app/**/*.ts`, and `source/renderer/app/**/*.tsx` file after exclusions, parse each file with the production parser stack, and fail if any file in the allowlist cannot be parsed.
- Start an isolated verification stack with fresh services needed for live ingestion, for example `AGENTIC_DB_PORT=5752 OLLAMA_PORT=11445 docker compose -p agentic-task-401 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools`, then wait for ParadeDB's known first-boot restart handoff before DB checks.
- Run a direct library smoke command through `--entrypoint python` that ingests only a representative explicit file list rooted at `/workspace`, not the whole repo. Use at minimum:
  - `source/common/ipc/api.ts`
  - `source/main/ipc/cardano.ipc.ts`
  - `source/renderer/app/stores/TransactionsStore.ts`
  - `source/renderer/app/components/analytics/AnalyticsProvider.tsx`
-  - `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`
- Verify with SQL that representative rows exist in `agentic.kb_code_chunks`, have normalized `repo_path` values, non-null embeddings, expected `language` values, and symbol kinds/names matching those files.
- Verify that at least one class-member row from `source/renderer/app/stores/TransactionsStore.ts` has `parent_symbol_name = 'TransactionsStore'`, a non-null `parent_symbol_kind`, and a symbol name matching a function-valued class field such as `_refreshTransactionData` or `calculateTransactionFee`.
- Verify that `source/renderer/app/components/analytics/AnalyticsProvider.tsx` produces a chunk for `AnalyticsProvider`, proving same-file named export-list resolution works for TSX.
- Verify that `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` produces a chunk for `MithrilBootstrapPage`, proving decorated TSX classes exported via trailing same-file default export are recognized.
- Verify negative discovery assertions by confirming no row is written for `%.d.ts` paths and no `repo_path` begins with `/workspace/`.
- Validate per-file replace semantics in automated coverage by re-ingesting a fixture file after adding, removing, and renaming symbols, then confirming stored chunk count and symbol names for that one `repo_path` exactly match the new file state.
- Tear down the isolated verification stack with `docker compose -p agentic-task-401 -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- **Parser coverage**: the selected Python AST parser must correctly handle Daedalus syntax such as decorators, typed class members, function-valued class fields, TSX components, and large export-heavy files. The new parser-only full-allowlist sweep reduces risk, but critique should still stress-test whether the named packages really cover representative files like `TransactionsStore.ts`, `MithrilBootstrapPage.tsx`, and `api.ts` without hidden gaps.
- **Export resolution**: same-file export lists and trailing same-file default exports are both required by real repo files. The implementation must prove those two cases work without widening into cross-file symbol chasing.
- **Chunk overlap**: emitting both class-level chunks and member-level chunks will create overlapping content. That is acceptable for this task, but critique should confirm the overlap is bounded enough for later search ranking rather than causing pathological duplication.
- **Per-file replace semantics**: this plan intentionally goes beyond naive upserts because code chunk counts are not stable. Critique should confirm that this is the minimal safe boundary and does not accidentally expand into broader sync/deletion responsibilities.
- **Image/dependency fit**: if `tree-sitter` plus `tree-sitter-language-pack` unexpectedly require native build tooling unavailable in the current image, implementation may block. That should be captured as a research finding, not solved by quietly adding a large Node or compiler toolchain.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` before implementation to correct `task-401.targetPath`, then update it again when the task is completed.
- Add a research note under `.agent/plans/agentic/research/` capturing durable findings such as the accepted parser package choice, supported symbol kinds, export-list handling, per-file replace semantics, and any syntax/embedding caveats discovered in live verification.
- Only update `.agent/plans/agentic/knowledge-base-platform-prd.md`, `.agent/workflows/agentic-kb.md`, or `agentic/README.md` if implementation reveals a durable platform-contract change beyond this task's internal library behavior.

## Implementation Notes

- Corrected `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-401.targetPath` now points at the packaged module path `agentic/src/agentic_kb/ingest/code.py`, marked `task-401` completed in the tracker, and restored unrelated tracker regressions so already-completed `task-001` and `task-002` remain completed.
- Added `agentic/src/agentic_kb/ingest/code.py` with a narrow task-401 TypeScript/TSX ingestor that discovers only the approved allowlist, parses files with `tree-sitter-language-pack`, extracts exported symbol chunks, and writes them with per-file replace semantics.
- Reused existing docs-ingestion helpers for repo path normalization, git commit fallback, embedding segmentation, psycopg loading, and vector literal formatting to keep the implementation tight and consistent with `task-301` and `task-501`.
- Export handling covers inline exports, same-file named export lists, and trailing same-file default exports. Re-export barrels with a `from` clause remain intentionally skipped.
- Exported variable support now includes destructuring bindings from top-level exported `const` / `let` / `var` declarations, including the in-scope `source/main/config.ts` pattern `export const { ... } = launcherConfig;`.
- Exported classes emit both class-level chunks and member-level chunks for methods, getters, setters, and function-valued class fields, with `parent_symbol_name`, `parent_symbol_kind`, and `symbol_path` metadata populated.
- Added focused unit coverage in `agentic/tests/test_code_ingest.py` and a durable research note at `.agent/plans/agentic/research/task-401-typescript-symbol-chunking.md`.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed; `tree-sitter` and `tree-sitter-language-pack` installed cleanly without Dockerfile or system toolchain changes.
- Local unit tests passed: `PYTHONPATH=agentic/src /tmp/task401-venv/bin/python -m unittest discover -s agentic/tests -p 'test_code_ingest.py'` and `PYTHONPATH=agentic/src /tmp/task401-venv/bin/python -m unittest discover -s agentic/tests -p 'test_docs_ingest.py'`, including new destructuring-export coverage.
- In-container unit tests passed: `docker compose -p agentic-task-401 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_code_ingest.py'`.
- Parser-only allowlist sweeping passed locally and in-container across 1003 current in-scope files.
- A targeted local smoke check confirmed `source/main/config.ts` now yields variable chunks for the destructured export at line 116: `cluster`, `nodeImplementation`, `stateDir`, `legacyStateDir`, `logsPrefix`, `isFlight`, and `smashUrl`.
- Live smoke ingestion passed on an isolated verification stack for the representative file subset in this plan, and SQL verification confirmed normalized `repo_path` values, non-null embeddings, expected language tags, working same-file export-list resolution for `AnalyticsProvider`, working trailing default-export resolution for `MithrilBootstrapPage`, and member chunks for `TransactionsStore` including `_refreshTransactionData`, `calculateTransactionFee`, and `recentTransactionsRequest`.
- The isolated verification stack was started with `AGENTIC_DB_PORT=5752 OLLAMA_PORT=11445 docker compose -p agentic-task-401 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools`.

## Outcome

- `task-401` is implemented within scope.
- No Dockerfile changes or extra system build tooling were needed for the parser dependency choice.
- Repo-wide deleted-file cleanup, full-repo scope widening beyond the approved allowlist contract, new sync CLI orchestration, and search-layer changes were not introduced.

## Planning Status Rationale

- This plan remains `approved` because the critique items are now addressed concretely: function-valued class fields are explicitly in scope, same-file trailing default export handling is called out with a real TSX representative, parser package choices are named, and verification now includes a parser-only sweep across the full task-401 allowlist.
- The boundary with `task-402` is explicit: this task lands symbol-aware chunking for a narrow high-value TS/TSX subset only, while `task-402` owns widening coverage to the full repository.
- The main uncertainties are implementation details around parser coverage and dependency fit inside `kb-tools`, which are real execution risks but not blockers to starting the task.
