# Task 402 Repository Code Coverage Research

- Date: 2026-03-28
- Task: `task-402`
- Evidence: `agentic/src/agentic_kb/ingest/code.py`, `agentic/src/agentic_kb/ingest/__init__.py`, `agentic/tests/test_code_ingest.py`, `agentic/tests/test_code_sync_state.py`, containerized verification via `docker-compose.agentic.yml`

## Durable Findings

- The accepted task-402 discovery contract is a full recursive walk from the repo root, not a hardcoded top-level allowlist. Coverage now automatically includes supported files under roots such as `translations/`, `hardware-wallet-tests/`, `perSystem/`, `flake/`, and `.buildkite/` unless an explicit exclusion rule applies.
- Extensionless inclusion is intentionally split into two stable classes: operational/build basenames allowed at any depth (`Dockerfile`, `Brewfile`, `Brewfile.netlify`, `Makefile`, `Procfile`) and repo-root hidden config basenames allowed only at repo root (for example `.eslintrc`, `.stylelintrc`, `.envrc`, `.ignore`, `.prettierrc`, `.tm_properties`). Non-allowlisted extensionless files such as `LICENSE` remain excluded.
- Generated/dependency/runtime directories must be excluded by any matching path segment, not just at repo root. The durable excluded-name set for v1 is `node_modules`, `.yarn`, `dist`, `build`, `coverage`, `logs`, `Release`, `Debug`, `__pycache__`, `.pytest_cache`, and `.mypy_cache`.
- Oversize-file handling has to participate in discovery and pruning, not just file reading. The durable boundary is `>512 KiB` or `>10,000` lines, and the same ingestability predicate now controls source discovery, processing eligibility, and prune eligibility so files that later become oversize are removed as unsupported on prune-capable full runs.
- `config` remains reserved for explicit basename-driven config/build files. Extension-driven files such as `.cfg` and `.conf` should not classify as `config`; mapping them to `ini` keeps the language vocabulary aligned with the approved search-filter contract.
- Parser failures and non-symbol-aware families are both accepted fallback paths in v1. Deterministic fallback chunks use `symbol_kind = 'file_chunk'` plus metadata keys `chunk_strategy`, `parse_status`, and `fallback_reason`, while `repo_path_prefix`, `language`, and `symbol_kind` remain the only required code-search filters.
- The full-repository parser/discovery smoke passed in-container after the oversize predicate fix with `discovered 1695` and `parsed 1695`, reflecting that oversize files are now excluded from the supported source set before parsing and pruning.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml build kb-tools` passed after the final task-402 changes.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_code_ingest.py'` passed with 22 tests.
- `docker compose -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_code_sync_state.py'` passed.
- `python3 -m compileall agentic/src/agentic_kb/ingest/code.py agentic/tests/test_code_ingest.py agentic/src/agentic_kb/ingest/__init__.py` passed.
- Full parser/discovery smoke passed in-container with `discovered 1695` and `parsed 1695`.
- Discovery smoke confirmed representative inclusions for `translations/formatter.js`, `hardware-wallet-tests/index.ts`, `perSystem/packages.nix`, `flake/lib.nix`, `.buildkite/pipeline.yml`, and `agentic/Dockerfile`, while `.agent/readme.md`, `LICENSE`, and nested `node_modules` content remained excluded.
- Isolated live-ingestion smoke succeeded for `.buildkite/pipeline.yml`, `agentic/Dockerfile`, `flake/lib.nix`, `source/common/ipc/api.ts`, `source/main/webpack.config.js`, and `source/renderer/app/components/analytics/AnalyticsProvider.tsx`, and SQL verification confirmed the expected mix of symbol-aware rows and `file_chunk` fallback rows.

## Caveats

- Host-local parser-dependent test runs remain non-authoritative because the host Python environment does not include the optional `tree_sitter` runtime dependencies; containerized verification remains the reliable path for code-ingestion tests.
- The implementation review log for task-402 already contained an earlier sequencing violation before final approval. The task preserved history and resumed by appending a later corrective iteration rather than rewriting the log.
