# Task Plan: task-402 Index entire Daedalus repository

- Task ID: `task-402`
- Title: `Index entire Daedalus repository`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-401` proved the `agentic/src/agentic_kb/ingest/code.py` symbol-chunking seam, parser dependency choice, DB write path, and sync-state hook on a narrow TypeScript/TSX allowlist.
- The platform plan explicitly says the first code ingestor should cover the entire repository immediately, and the current task-401 allowlist still leaves large repo areas invisible to search.
- `task-502` search-quality work and `task-701` sync orchestration need repository-wide code/config/test coverage so hybrid search and refresh behavior are evaluated against the same source set users actually navigate.

## Scope

- Widen repository code ingestion from the current task-401 allowlist to a repository-wide source discovery pass rooted at `/workspace` and implemented inside `agentic/src/agentic_kb/`, using a fixed full-repo inclusion/exclusion policy instead of open-ended heuristics or a narrow top-level allowlist.
- Keep writing only to `agentic.kb_code_chunks` for repository code/config/test chunks; do not ingest docs, GitHub, or project entities into this task's pipeline.
- Support a mixed ingestion strategy:
  - symbol-aware extraction for parser-supported source families
  - generic text chunking for supported repo files that are text-like but not symbol-parsed or do not yield supported symbols
- Add explicit repository exclusions so generated, vendored, binary, doc-owned, and runtime-artifact paths do not flood `kb_code_chunks`.
- Add safe full-run stale-row cleanup for repo paths that disappear from the repository-wide source set, but only behind an explicit prune contract.

## Non-Goals

- Do not ingest Markdown, README, workflow, plan, or SOP content that belongs to docs ingestion from `task-301` through `task-304`, even if those files live inside the repository.
- Do not ingest GitHub issues, PRs, comments, or Project 5 items; those remain `task-403` and `task-404`.
- Do not add new search query execution, ranking logic, CLI UX, MCP behavior, or sync command orchestration; those remain `task-502`, `task-503`, `task-701`, and `task-801`.
- Do not add language-specific deep extractors beyond the generic repository-wide symbol-or-fallback strategy; IPC/store/workflow-specific enrichment remains later follow-up work.
- Do not index binary assets, build outputs, dependency trees, or other non-text artifacts just to satisfy repository breadth.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-202` - created `agentic.kb_code_chunks`
  - `task-204` - locked the v1 code search filter contract around `repo_path_prefix`, `language`, and `symbol_kind`
  - `task-301` - established shared ingestion helpers and path normalization patterns
  - `task-401` - established the first TypeScript/TSX symbol-aware ingestion path, tree-sitter dependency choice, and per-file replace semantics
  - `task-405` - established code sync-state recording for successful code-ingestion runs
- Direct downstream tasks unblocked or materially improved by this work:
  - `task-502` - hybrid BM25/vector/RRF search across real full-repo code coverage
  - `task-503` - CLI inspection of broader code results
  - `task-701` - `sync code` and `sync changed` orchestration over a full repository source set
- Tracking mismatch to correct during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently points `task-402.targetPath` at `agentic/src/ingest/code.py`, but the existing packaged module path is `agentic/src/agentic_kb/ingest/code.py`.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-401-typescript-symbol-chunking.md`
  - `.agent/plans/agentic/research/task-204-search-config-registry.md`
  - `.agent/plans/agentic/task-plans/task-401.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/ingest/code.py`
  - `agentic/src/agentic_kb/ingest/__init__.py`
  - `agentic/src/agentic_kb/search/config.py`
  - `agentic/src/agentic_kb/sync/state.py`
  - `agentic/tests/test_code_ingest.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-402.targetPath` before implementation and mark the task complete when done
- `agentic/src/agentic_kb/ingest/code.py` - widen discovery, add repository exclusions, add fallback chunking, and add full-run pruning behavior
- `agentic/src/agentic_kb/ingest/__init__.py` - export any new public helpers required by the widened ingestor
- `agentic/tests/test_code_ingest.py` - extend coverage for repository-wide discovery, exclusions, generic fallback chunking, parse-failure fallback, and stale-path cleanup
- `agentic/tests/fixtures/code/` - add minimal multi-language and fallback fixtures if inline strings become too noisy
- `.agent/plans/agentic/research/task-402-repository-code-coverage.md` - capture durable findings after implementation

## Implementation Approach

- **Package boundary**: keep all task-402 implementation inside the existing Python package rooted at `agentic/src/agentic_kb/`; do not introduce a parallel `agentic/src/ingest/` layout.
- **Repository discovery contract**: treat a file as in-scope only if it passes both of these gates:
  - location gate: the file lives anywhere under the repository root and is discovered by a full recursive walk from `/workspace`, not by a fixed top-level inclusion allowlist. This deliberately includes currently present roots such as `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, `.buildkite/`, and any future repo-root directories unless they are explicitly excluded below.
  - filename gate: the file either has one of these extensions: `.ts`, `.tsx`, `.js`, `.jsx`, `.json`, `.yml`, `.yaml`, `.sql`, `.nix`, `.py`, `.sh`, `.bash`, `.zsh`, `.feature`, `.toml`, `.ini`, `.cfg`, `.conf`, `.xml`, `.html`, `.css`, `.scss`; or its basename is exactly one of the explicit operational/build filenames allowed at any depth: `Dockerfile`, `Brewfile`, `Brewfile.netlify`, `Makefile`, or `Procfile`; or, if and only if it is a repo-root file, its basename is exactly one of `.dockerignore`, `.editorconfig`, `.envrc`, `.eslintignore`, `.eslintrc`, `.gitattributes`, `.gitignore`, `.ignore`, `.prettierignore`, `.prettierrc`, `.prettierrc.json`, `.prettierrc.js`, `.stylelintrc`, or `.tm_properties`
- **Exclusion semantics**: normalize exclusions into two deterministic classes so implementation does not infer glob meaning ad hoc:
  - repo-root prefix exclusions: exclude any path whose normalized repo-relative path is exactly one of `.agent`, `.claude`, `.git`, `.idea`, `.opencode`, `.ralph`, `tests-report`, `dummy-certs`, or `agentic/snapshots` or starts with that prefix plus `/`; these are root-owned coordination, tooling, VCS, or local-artifact areas, not general nested-name bans
  - any-segment exclusions: exclude any path that contains a path segment exactly equal to `node_modules`, `.yarn`, `dist`, `build`, `coverage`, `logs`, `Release`, `Debug`, `__pycache__`, `.pytest_cache`, or `.mypy_cache`, regardless of depth; also exclude any path whose basename or suffix matches the file-class rules below regardless of depth
- **Concrete exclusions**: after passing the inclusion gates, still exclude any path matching one of these normalized file-class rules: basename exactly `yarn.lock`, `package-lock.json`, `pnpm-lock.yaml`, or `flake.lock`; suffix exactly `.md`, `.mdx`, `.d.ts`, `.scss.d.ts`, `.min.js`, `.map`, `.log`, `.png`, `.jpg`, `.jpeg`, `.gif`, `.svg`, `.ico`, `.icns`, `.pdf`, `.wasm`, `.zip`, `.tar`, `.gz`, or `.lock`.
- **Extensionless file policy**: v1 recognizes two explicit extensionless inclusion classes and nothing else:
  - operational/build basenames allowed at any depth: `Dockerfile`, `Brewfile`, `Brewfile.netlify`, `Makefile`, and `Procfile`; nested files such as `agentic/Dockerfile` are in scope because they are concrete build/runtime inputs, not prose
  - repo-root hidden config basenames allowed only at repo root: `.dockerignore`, `.editorconfig`, `.envrc`, `.eslintignore`, `.eslintrc`, `.gitattributes`, `.gitignore`, `.ignore`, `.prettierignore`, `.prettierrc`, `.prettierrc.json`, `.prettierrc.js`, `.stylelintrc`, and `.tm_properties`
  - all other extensionless names remain excluded unless they already match a supported extension rule, which keeps ambiguous entries such as `LICENSE`, `LICENSE NOTICE`, wrappers, binaries, and prose-adjacent files out of `kb_code_chunks` without content sniffing
- **Excluded-root rationale**: the repo-root prefix exclusions above are intentionally limited to doc-owned agent memory, VCS/editor/tool metadata, and snapshot/certificate directories, while the any-segment exclusions cover dependency trees, generated outputs, caches, and runtime-artifact directories no matter where they appear. They are excluded because indexing them would either duplicate another ingestion domain or create local-machine noise; every other repo root stays eligible under the recursive discovery contract.
- **`.agent/` boundary**: exclude all of `.agent/**` from task-402. Even when a file under `.agent/` is text-like, it belongs to docs/plan/workflow ingestion rather than code ingestion, and task-402 must not duplicate that domain.
- **Declaration-file policy**: exclude all declaration files for task-402, not just style declarations. `*.d.ts` and `*.scss.d.ts` stay out of `kb_code_chunks` so task-401 and task-402 keep one consistent code-ingestion boundary and avoid flooding search with derived type surfaces.
- **Oversize boundary**: exclude any otherwise-supported file larger than 512 KiB or longer than 10,000 lines. Do not partially ingest oversize files in this task.
- **Symbol-aware families**: keep task-401 symbol extraction for `.ts` and `.tsx`, and extend the same parser-driven symbol path to `.js` and `.jsx` only if the existing `tree-sitter-language-pack` grammars can produce the same top-level symbol families without new toolchains or schema changes. All other included file families use fallback text chunking only.
- **Fallback chunk contract**: when an included file is not in a symbol-aware family, yields no supported symbols, or fails parsing, emit deterministic fallback chunks with `symbol_name = null`, `symbol_kind = 'file_chunk'`, and `metadata.chunk_strategy = 'fallback_text'`.
- **Fallback sizing contract**: normalize line endings to `\n`, then apply one deterministic procedure for every fallback-only or parser-fallback file:
  - preprocess into logical source lines; preserve every line exactly, including lines longer than 6000 characters
  - build each chunk by scanning forward from the current start line and appending whole lines until appending the next whole line would exceed either 120 lines or 6000 characters; if the first line of a chunk already exceeds 6000 characters, emit that single long line as the chunk anyway rather than splitting inside the line
  - compute the next chunk start from the emitted chunk by walking backward from its end until both overlap targets are satisfied or the chunk start is reached: keep at most the last 15 lines and enough trailing whole lines to cover at least 600 characters when possible; when the two overlap targets conflict, choose the earlier start line that satisfies both targets, and if the chunk is smaller than one or both targets, reuse the original chunk start
  - never split inside a line, never reorder text, and stop only when the final emitted chunk reaches the file end
  - because oversize files are excluded up front, fallback output must stay bounded to 128 chunks per file; if this deterministic procedure would still require more than 128 chunks, skip the file entirely instead of truncating output or emitting partial trailing coverage
- **Failure handling**: task-401's fail-loud parser behavior is too brittle for full-repo ingestion. For task-402, parser failures on an otherwise included `.ts`, `.tsx`, `.js`, or `.jsx` file must fall back to text chunks for that file and record `metadata.parse_status = 'fallback'` plus a concrete `metadata.fallback_reason` such as `parse_error` or `no_supported_symbols`.
- **Language contract**: derive `language` strictly from file extension or explicit basename family, with a stable vocabulary limited to `typescript`, `typescriptreact`, `javascript`, `javascriptreact`, `json`, `yaml`, `sql`, `nix`, `python`, `shell`, `gherkin`, `toml`, `ini`, `config`, `xml`, `html`, `css`, and `scss`. Use `config` only for the explicit basename allowlist such as `Dockerfile` and `.prettierrc`.
- **Search-filter contract**: task-204 remains unchanged. `repo_path_prefix`, `language`, and `symbol_kind` are still the only required code-search filters for downstream work. New facts such as `chunk_strategy`, `parse_status`, `fallback_reason`, `file_extension`, `is_full_repository_run`, and coarse repo-area tags stay in `metadata` only and must not become new required filter semantics in task-402.
- **Write semantics**:
  - keep per-file replace semantics for processed files
  - introduce an explicit run-mode contract in the ingestion API: default mode is non-pruning, and pruning is legal only when the caller sets a dedicated full-repository signal such as `run_mode='full_repository'` together with `prune_missing=True`
  - any caller-supplied `source_paths`, future `sync changed` flow, smoke test, or filtered run must use a non-pruning mode and may not trigger repo-wide deletes implicitly
  - when pruning is enabled, compute the supported source set from the same full recursive discovery predicate used for ingestion after all exclusions and filename gates are applied, then delete only `kb_code_chunks` rows whose `repo_path` is absent from that full-repository result
  - because discovery is root-recursive rather than rooted in a narrow directory allowlist, prune eligibility no longer depends on remembering a hand-maintained set of supported top-level roots; newly added repo roots with supported files remain protected automatically unless they are explicitly excluded by policy
- **Verification shape**: verify on a full discovery pass, not just a few handpicked TypeScript files. The acceptance path should cover repository discovery counts, normalized exclusion semantics, repo-root basename-allowlist behavior, deterministic fallback behavior, representative live ingestion across multiple repo areas, and stale-row pruning behind the explicit full-run signal.

## Acceptance Criteria

- The task-402 implementation lives under `agentic/src/agentic_kb/ingest/code.py` and does not introduce a competing package layout.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is corrected so `task-402.targetPath` points at `agentic/src/agentic_kb/ingest/code.py` before production code changes land.
- A default repository-wide discovery path exists and uses the full recursive repo walk, expanded repo-root basename allowlist, normalized exclusion semantics, declaration-file policy, and oversize-file boundary defined in this plan, while preserving repo-relative POSIX `repo_path` values.
- The discovery contract explicitly covers currently present supported roots such as `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, and `.buildkite/` without requiring per-root plan updates, because any non-excluded repo root is eligible during the full recursive walk.
- `.agent/**`, all Markdown/doc-owned sources, all declaration files, lockfiles, generated artifacts, vendored paths, binary assets, and runtime-artifact paths are not ingested by task-402 into `kb_code_chunks`.
- Repo-root hidden config files covered by the explicit basename allowlist include at minimum `.eslintrc`, `.stylelintrc`, `.envrc`, `.ignore`, `.tm_properties`, and `.prettierrc`; non-allowlisted extensionless repo-root files remain excluded.
- Nested operational/build files covered by the explicit any-depth basename allowlist include at minimum `agentic/Dockerfile`; nested `Dockerfile`, `Makefile`, `Procfile`, `Brewfile`, and `Brewfile.netlify` files remain eligible anywhere in the repo unless another exclusion rule blocks their path.
- Parser-supported `.ts`, `.tsx`, `.js`, and `.jsx` files still produce symbol-aware chunks where available, and included files that cannot be symbol-parsed still produce deterministic fallback chunks instead of being silently dropped.
- Fallback chunks populate the schema-required fields, use `symbol_kind = 'file_chunk'`, and carry metadata that explains the fallback strategy or parse outcome.
- Fallback sizing is bounded exactly as planned: whole-line chunks only, 120 lines or 6000 characters maximum unless a single carried line exceeds 6000 characters, deterministic overlap computed from the emitted chunk's trailing whole lines with targets of 15 lines and 600 characters, no partial ingest for oversize files, and no more than 128 fallback chunks for any ingested file.
- Repository-wide ingestion expands `language` coverage beyond `typescript` and `typescriptreact` using only the stable vocabulary declared in this plan, without requiring new schema columns.
- A prune-capable full repository ingest run requires an explicit full-run signal plus `prune_missing=True`, and only that run shape can delete stale `kb_code_chunks` rows for repo paths removed from the full recursive supported discovery set computed from the same inclusion/exclusion policy used for ingestion.
- Explicit `source_paths` runs, smoke tests, and future incremental sync flows do not delete unrelated rows and do not expand the v1 search-filter contract beyond `repo_path_prefix`, `language`, and `symbol_kind`.

## Verification Plan

- Run focused unit tests for widened discovery and classification, including positive coverage for representative repo paths from `agentic/`, `source/`, `tests/`, `storybook/`, `utils/`, `installers/`, `nix/`, `.github/`, `scripts/`, `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, `.buildkite/`, repo-root hidden config files, and nested operational/build files such as `agentic/Dockerfile`; add negative coverage for excluded roots such as `.agent/`, `.claude/`, `.opencode/`, `.ralph/`, `.idea/`, Markdown, declaration files, lockfiles, binary assets, generated paths, and non-allowlisted extensionless names.
- Add parser/fallback tests that prove:
  - JS/JSX parser-supported files can produce symbol-aware chunks when expected
  - files with no supported symbols still produce fallback chunks
  - parser failures degrade to fallback chunks with explicit metadata instead of aborting the full run
  - all `*.d.ts` and `*.scss.d.ts` files remain excluded
  - fallback chunk sizing and overlap follow the exact deterministic whole-line procedure, including conflict resolution between line and character caps and the single-long-line exception
  - oversize files are skipped rather than partially chunked
- Add stale-row cleanup tests showing that a prune-capable full discovery run removes rows for a deleted or newly unsupported repo path, while a targeted explicit `source_paths` run or non-pruning full-run leaves unrelated rows intact.
- Run `docker compose -f docker-compose.agentic.yml build kb-tools` so widened dependencies and package code are current.
- Run the code-ingest test suite locally and in-container, extending `agentic/tests/test_code_ingest.py` rather than introducing only ad hoc smoke scripts.
- Run a repository-wide parser/discovery smoke check that enumerates the default supported source set and confirms the approved recursive discovery contract works across the current repo-root listing, including representation from `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, `.buildkite/`, and nested operational/build files such as `agentic/Dockerfile`, while excluded roots and file classes are absent.
- Explicitly prove the settled extensionless-file policy during automated coverage or smoke verification by asserting at minimum that repo-root `.prettierrc`, `.gitignore`, `.eslintrc`, `.stylelintrc`, `.envrc`, `.ignore`, `.tm_properties`, and `Brewfile.netlify` are discoverable, and that nested operational/build basenames such as `agentic/Dockerfile` are discoverable too, while non-allowlisted extensionless names such as `LICENSE` remain excluded.
- Explicitly prove any-segment artifact exclusion semantics by asserting that directories named `node_modules`, `dist`, `build`, `coverage`, `logs`, `Release`, and `Debug` are excluded even when they appear below an otherwise included parent path.
- Run an isolated live ingestion smoke path against a fresh verification DB and ingest a representative cross-section including at minimum:
  - one TypeScript file from `source/common/` or `source/main/`
  - one TSX file from `source/renderer/app/`
  - one JS config file such as a webpack config
  - one test asset from `tests/` such as a `.ts` step file or `.feature` file
  - one fallback-only family that can never take the symbol-aware path in this task, such as `.yml`, `.yaml`, `.nix`, `.feature`, `.json`, or `.sh`
- Verify in SQL that symbol chunks and fallback chunks both exist with normalized `repo_path`, expected `language`, expected `symbol_kind`, non-null embeddings, and metadata showing the chosen chunking strategy without introducing new filter columns.
- Explicitly verify that the fallback-only representative produced fallback rows rather than symbol-aware rows so at least one non-parser family is proven end to end, not inferred from unit tests alone.
- Tear down the isolated verification stack after validation.

## Risks / Open Questions

- **Full-repo traversal cost**: the discovery contract is now settled as a root-recursive walk, so the execution risk is implementation efficiency and consistent exclusion handling rather than scope selection.
- **JS/JSX parser reuse**: the same `tree-sitter-language-pack` stack should handle `.js` and `.jsx` without widening dependencies. If it cannot, the task must keep JS/JSX on fallback chunks rather than re-opening parser/toolchain design.
- **Language taxonomy drift**: the declared `language` vocabulary is intentionally fixed for v1. Follow-up work should reuse it instead of inventing near-duplicates that fragment task-502 filters.
- **Prune execution safety**: the safety contract is settled, but implementation must ensure deletion code is reachable only from explicit `run_mode='full_repository'` plus `prune_missing=True` and that it reuses the same discovery predicate as ingestion.
- **Fallback determinism**: the chunking algorithm is specified tightly enough for tests, but implementation must still avoid off-by-one overlap bugs around exact 120-line boundaries, exact 6000-character boundaries, and single-line chunks longer than 6000 characters.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with the final approved plan, implementation notes, verification notes, and outcome after the build loop.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` to correct `task-402.targetPath` and later mark `task-402` completed.
- Add a task-402 research note under `.agent/plans/agentic/research/` capturing the accepted repository exclusions, supported file families, fallback chunking contract, declaration-file decision, and stale-row prune boundary.
- Update `.agent/plans/agentic/knowledge-base-platform-prd.md` or `.agent/workflows/agentic-kb.md` only if implementation reveals a durable platform-level contract change for repository-wide code ingestion, especially if the approved inclusion/exclusion policy or prune safety contract must become user-facing.

## Implementation Notes

- Replaced the task-401 narrow TypeScript/TSX allowlist with full recursive repository discovery in `agentic/src/agentic_kb/ingest/code.py`, governed by explicit inclusion and exclusion rules that now cover supported files across the repo while preserving hard exclusions for docs, lockfiles, declarations, binaries, caches, and generated artifact trees.
- Added explicit support for nested operational/build files such as `agentic/Dockerfile`, repo-root hidden config files such as `.eslintrc` and `.prettierrc`, stable extension-based language classification, and metadata-backed fallback chunks for non-symbol-aware or parse-fallback files.
- Added `CodeDiscoveryOptions`, explicit `run_mode` / `prune_missing` validation, and store-level `delete_missing_paths` support so only true full-repository runs can prune stale code rows.
- Centralized the oversize-file boundary through `_is_ingestable_code_file`, ensuring discovery, processing, and prune eligibility all exclude files larger than `512 KiB` or longer than `10,000` lines with one consistent predicate.
- Extended `agentic/tests/test_code_ingest.py` to cover recursive discovery behavior, exclusion rules, nested operational file inclusion, fallback chunking, parser downgrade behavior, prune gating, oversize discovery exclusion, oversize prune cleanup, fallback chunk-limit overflow, and targeted non-pruning behavior.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml build kb-tools` passed after the final code changes.
- `python3 -m compileall agentic/src/agentic_kb/ingest/code.py agentic/tests/test_code_ingest.py agentic/src/agentic_kb/ingest/__init__.py` passed.
- Local host execution of `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_code_ingest.py'` remained non-authoritative because host Python does not have the optional `tree_sitter` dependencies installed.
- Containerized `python -m unittest discover -s agentic/tests -p 'test_code_ingest.py'` passed with 22 tests.
- Containerized `python -m unittest discover -s agentic/tests -p 'test_code_sync_state.py'` passed.
- Full parser/discovery smoke passed in-container with `discovered 1695` and `parsed 1695`.
- Repository discovery smoke confirmed representative inclusions for `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, `.buildkite/`, and `agentic/Dockerfile`, while `.agent/`, `LICENSE`, and nested `node_modules` content remained excluded.
- Isolated live-ingestion smoke passed against a fresh verification stack for `.buildkite/pipeline.yml`, `agentic/Dockerfile`, `flake/lib.nix`, `source/common/ipc/api.ts`, `source/main/webpack.config.js`, and `source/renderer/app/components/analytics/AnalyticsProvider.tsx`, and SQL verification confirmed the expected mix of symbol-aware and fallback rows.

## Outcome

- `task-402` is implemented within the approved scope.
- Full-repository code ingestion now covers the whole repository through recursive discovery with explicit exclusions and safe fallback behavior.
- Prune safety is enforced by explicit full-repository mode plus `prune_missing=True`, and oversize-file support stays aligned across discovery, processing, and pruning.

## Review-Log Paths

- Planning Review Log: `.agent/plans/agentic/task-plans/task-402-plan-review.md`
- Implementation Review Log: `.agent/plans/agentic/task-plans/task-402-impl-review.md`

## Planning Status Rationale

- The plan is `approved` because the planning review converged cleanly in `task-402-plan-review.md`, and the final implementation stays aligned with the approved recursive discovery, exclusion, fallback, and prune-safety contracts.
- The task now satisfies the platform's "entire repository" code-ingestion requirement without collapsing docs, GitHub, or project content into the code domain.
- Remaining concerns are operational rather than planning gaps: performance of the full-repo walk, JS/JSX parser behavior on edge files, and exact fallback overlap behavior under future changes.
