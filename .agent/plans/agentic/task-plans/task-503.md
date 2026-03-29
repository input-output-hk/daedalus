# Task Plan: task-503 Add search CLI and status commands

- Task ID: `task-503`
- Title: `Add search CLI and status commands`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-503` is the next unblocked critical-path task in `.agent/plans/agentic/knowledge-base-platform-tasks.json` after `task-205` and `task-502` completed.
- The repo now has the two prerequisites this task needs: real DB-aware `status` behavior from `task-205` and real BM25/vector/hybrid query execution from `task-502`.
- The workflow doc already shows a local `kb-tools search "..."` shape, but that command is still missing from the packaged CLI; this task closes that gap for local users and scripts.
- `task-504` and `task-801` both depend on a stable local CLI and entity-inspection contract, so this task should settle that surface before ranking fixtures or MCP wrappers build on top of it.

## Scope

- Add a packaged local `search` command to `agentic-kb` for docs, code, GitHub, and project-item queries.
- Extend the existing `status` command with a script-friendly machine-readable output mode while preserving the current human-readable and `--healthcheck` behavior from `task-205`.
- Add a local entity-inspection command for fetching one indexed entity by `entity_type` and stable row `id`.
- Keep the CLI usable for both humans and scripts with concise default text output and JSON output for automation.
- Reuse existing packaged modules and task-204/task-502 registry contracts instead of creating a second search vocabulary in the CLI layer.

## Non-Goals

- Do not implement MCP tools, MCP server behavior, or the planned `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, or `kb_status` MCP tool surface; that remains `task-801`.
- Do not implement sync orchestration, stale-index detection, snapshot manifest work, or future snapshot-sharing polish; those remain later tasks.
- Do not redesign search ranking, embedding behavior, schema/index layout, or registry filter semantics already settled in `task-204`, `task-205`, and `task-502`.
- Do not broaden this task into generic doc cleanup unrelated to the CLI surfaces touched here.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-204` - locked the searchable entity/filter vocabulary in `agentic/src/agentic_kb/search/config.py`
  - `task-205` - implemented real DB-aware `status` and the packaged CLI path in `agentic/src/agentic_kb/cli.py`
  - `task-301`, `task-402`, `task-403`, `task-404` - populated the current searchable entity tables
  - `task-405` - established current sync-state reporting that `status` already exposes
  - `task-502` - implemented the reusable packaged search-query layer in `agentic/src/agentic_kb/search/query.py`
- Direct downstream tasks unblocked by this work:
  - `task-504` - search-quality fixtures need a settled local CLI contract to mirror
  - `task-801` - MCP search tools should wrap the same search/entity/status library contracts rather than inventing their own
  - `task-803` - workflow updates can document real local search/entity usage once this task lands
- Tracker and repo inconsistencies to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-503.targetPath` at stale pre-package path `agentic/src/cli.py`, but the real CLI entrypoint is `agentic/src/agentic_kb/cli.py`.
  - `.agent/workflows/agentic-kb.md` already documents `kb-tools search "mithril bootstrap"`, but today that command is still unimplemented; this task should make the workflow example true, with only a narrow interim workflow update rather than the full final doc pass reserved for `task-803`.
  - `agentic/README.md` still contains older task-103 placeholder wording for `snapshot` and bootstrap-only `status`; that drift is adjacent but not core `task-503` scope unless touched command documentation needs a minimal correction alongside this task's CLI updates.

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile only the stale `task-503.targetPath` and later mark the task completed.
- `agentic/src/agentic_kb/cli.py` - add parser wiring for `search` and the entity-inspection command, plus any new shared output flags on `status`.
- `agentic/src/agentic_kb/commands/search.py` - add CLI-facing search request parsing, output shaping, and error handling around `PostgresSearchStore`.
- `agentic/src/agentic_kb/commands/entity.py` - add generic entity lookup/inspection by registry entity type and stable row id.
- `agentic/src/agentic_kb/commands/status.py` - add JSON serialization/output support without changing the underlying healthcheck contract.
- `agentic/src/agentic_kb/search/__init__.py` - only if a small export adjustment helps the new CLI commands import stable query types.
- `agentic/tests/test_search_command.py` - focused unit coverage for CLI search parsing, output shaping, and error behavior.
- `agentic/tests/test_search_command_db.py` - DB-backed verification that CLI search returns seeded ParadeDB results and preserves deterministic JSON/text ordering.
- `agentic/tests/test_entity_command.py` - focused unit coverage for entity lookup, missing-row handling, and text/JSON output.
- `agentic/tests/test_entity_command_db.py` - DB-backed verification that generic entity lookup works against seeded searchable tables and returns the agreed not-found contract.
- `agentic/tests/test_status_command.py` - extend current coverage for new JSON output or shared status serialization behavior.
- `.agent/workflows/agentic-kb.md` - minimal interim update so the documented local `search` command matches reality once implemented, while leaving full final workflow reconciliation to `task-803`.
- `agentic/README.md` - only if a narrow command-surface correction is needed alongside the CLI changes.
- `.agent/plans/agentic/research/task-503-search-cli-and-status-commands.md` - capture durable findings about the accepted CLI contract, output format, and verification boundaries.

## Implementation Approach

- **Parser placement**: keep `agentic/src/agentic_kb/cli.py` as the only packaged entrypoint. This task should add new subparsers there rather than creating a second CLI module.
- **Search command shape**: add a top-level `search` command with a positional query string and focused flags such as `--mode`, `--limit`, repeated `--entity-type`, repeated `--filter key=value`, and `--json`. The CLI should translate these arguments into one `SearchRequest` and call `PostgresSearchStore.from_config(...)`.
- **Filter UX**: keep the parser generic and registry-driven instead of hard-coding a dedicated CLI flag for every current entity filter. A repeated `--filter key=value` contract plus an ergonomic `--entity-type` alias keeps scope tight, lets scripts pass through new registry-supported filters without parser churn, and avoids duplicating task-204 filter metadata in `argparse`.
- **Default search output**: print concise human-readable ranked hits by default, including at minimum result rank, `entity_type`, a best title or identifier field, a path/URL field when present, and preview text when present. Do not dump full bodies by default.
- **Search JSON output**: add `--json` so scripts can consume the full `SearchResultSet` in a stable machine-readable form, including query metadata, hit ordering, ids, fields, and rank/score fields. JSON should serialize enums as their string values.
- **Deterministic JSON contract**: for `search --json`, `status --json`, and successful `entity get --json`, write exactly one JSON object to stdout followed by a newline, with no prose prefix/suffix. Keep object keys stable via explicit serializers, keep list ordering deterministic, serialize enums as strings, normalize timestamps to ISO-8601 strings, emit `null` for absent optional values, and keep all error messages on stderr so shell pipelines can safely consume stdout.
- **Status JSON output**: extend `agentic-kb status` with `--json` rather than inventing a second status command. The existing `StatusReport` data model already supports this boundary well; implementation should serialize the same report that text output uses so human and script modes stay aligned.
- **Healthcheck preservation**: `status --healthcheck` must keep the current exit-code-oriented behavior from `task-205`. If `--json` is allowed with `--healthcheck`, it should only change formatting, not semantics or exit codes.
- **Entity-inspection command surface**: add a grouped top-level `entity` command with `get` subcommand, for example `agentic-kb entity get <entity_type> <id>`. This aligns with the future MCP `get_entity` concept without consuming more root-level verbs than necessary.
- **Entity lookup contract**: `agentic-kb entity get <entity_type> <id>` should validate `entity_type` through the task-204 registry before any SQL runs, fetch one row by the entity config's primary key from the configured table, and keep the lookup path generic across all current searchable entity types without invoking embeddings or ranking logic.
- **Entity failure semantics**: invalid `entity_type` should fail as a usage/validation error with exit code `2`; a well-formed lookup whose row is absent should fail with exit code `4` and a concise stderr not-found message that names the requested `entity_type` and `id`; `entity get --json` should still emit no stdout payload on error; unexpected DB/runtime failures should remain a separate non-zero runtime failure path rather than being collapsed into not-found.
- **Inspection payload**: for scripts, `entity get --json` should return the full fetched row with JSON-safe values. For default text output, keep formatting simple and generic: print entity metadata first, then include richer content fields such as `content` or `body_text` when present so the command is actually useful for inspection rather than just repeating search previews.
- **Library boundaries**: keep most logic in reusable command helpers rather than inline in `argparse` handlers. Search parsing, JSON serialization, and entity fetch helpers should be directly unit-testable without shelling out.
- **Database access for entity get**: implement a small generic query helper using safe identifier quoting and the registry's `table_name` and `primary_key_column` metadata, rather than hard-coding per-table lookup SQL. This can live with the entity command or in a tiny shared helper, but it should stay scoped to exact-id fetch only.
- **Error handling**: convert validation failures, missing required runtime config, not-found entity lookups, and search/store exceptions into concise user-facing CLI messages with non-zero exit codes that are stable enough for scripting.
- **Output stability**: choose one explicit JSON shape for each command and cover it in tests so future MCP and workflow tasks can rely on it. Avoid adding color, tables, or shell-dependent formatting in this task.
- **Scope boundary on subcommands**: do not add local `search_docs`, `search_code`, or `search_github` command aliases in this task. The local CLI can cover those use cases via `--entity-type` or matching filters, while the domain-specific names remain MCP-surface work for `task-801`.

## Acceptance Criteria

- The packaged CLI entrypoint remains `agentic/src/agentic_kb/cli.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated so `task-503.targetPath` points at the packaged CLI path instead of stale `agentic/src/cli.py`.
- `agentic-kb search <query>` exists and executes real packaged search queries against the configured KB database.
- The search command supports the `task-502` search modes through CLI flags, at minimum `bm25`, `vector`, and `hybrid`.
- The search command supports a bounded result count and current registry-backed filter vocabulary without hard-coding a separate per-filter parser contract for every entity type.
- The search command supports machine-readable JSON output for scripts.
- `agentic-kb status` keeps the current task-205 behavior and gains machine-readable JSON output without regressing `--healthcheck` semantics.
- A local entity-inspection command exists for fetching one entity by `entity_type` and stable row id.
- `agentic-kb entity get <entity_type> <id>` validates unknown `entity_type` values before querying, returning exit code `2` for validation errors.
- `agentic-kb entity get <entity_type> <id>` returns exit code `4` when the entity type is valid but the row does not exist, emits no stdout payload even with `--json`, and writes a concise stderr message that clearly identifies the requested `entity_type` and `id`.
- Entity lookup works generically across the current searchable entity types.
- Default text output for `search`, `status`, and entity inspection is concise and human-usable.
- `search --json`, `status --json`, and successful `entity get --json` emit exactly one deterministic JSON object on stdout with stable top-level keys, stable list ordering, stringified enums, normalized timestamp/non-primitive values, and no extra human-readable lines; validation, not-found, and runtime errors emit no stdout JSON and instead use stderr plus non-zero exit codes.
- No MCP server behavior, sync orchestration, ranking redesign, or schema/index changes are introduced.
- Focused automated tests cover parser behavior, deterministic JSON serialization, error handling, and entity lookup/search command integration boundaries, including at least one DB-backed verification path for both `search` and `entity get`.

## Verification Plan

- Run `python3 -m py_compile` on touched CLI and command modules plus new tests.
- Run focused unit suites such as:
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_entity_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'`
- Run DB-backed command suites against an isolated ParadeDB stack or disposable test database, for example:
  - `PYTHONPATH=agentic/src AGENTIC_TEST_DATABASE_URL=... python3 -m unittest discover -s agentic/tests -p 'test_search_command_db.py'`
  - `PYTHONPATH=agentic/src AGENTIC_TEST_DATABASE_URL=... python3 -m unittest discover -s agentic/tests -p 'test_entity_command_db.py'`
- Add unit coverage for:
  - `search` argument parsing into `SearchRequest`
  - repeated `--entity-type` and `--filter key=value` handling
  - JSON serialization of search hits and status reports
  - text-output shaping for representative search results
  - entity lookup success and not-found handling, including `--json` success stdout versus `--json` error stderr/no-stdout behavior
  - exit-code behavior for validation and runtime failures
- Add DB-backed coverage for:
  - seeded `search` results across at least one real searchable table, asserting deterministic hit ordering and JSON payload shape
  - seeded `entity get` success against a real searchable table plus an explicit not-found case with the agreed exit code `4`, stderr error text, and empty stdout under `--json`
  - `status --json` serialization against a live database if the serializer touches DB-derived non-primitive values not already covered by task-205 DB tests
- Re-run existing `task-502` and `task-205` focused suites if touched code shares their modules, especially `agentic/tests/test_search_query.py` and `agentic/tests/test_status_command.py`.
- Run a lightweight packaged CLI smoke check after implementation, for example `PYTHONPATH=agentic/src python3 -m agentic_kb.cli --help` and representative `search`/`entity` help commands.
- Run at least one packaged CLI smoke path against a live isolated DB for each new command, for example seeded `agentic-kb search <query> --json` and `agentic-kb entity get <entity_type> <id> --json`, to prove the installed CLI contract matches the DB-backed helpers rather than only parser-level tests.
- If the task updates container-packaged modules only, run `docker compose -f docker-compose.agentic.yml build kb-tools` and at least one in-container help or parser smoke check to confirm the installed CLI surface matches the source tree.

## Risks / Open Questions

- **Filter CLI ergonomics**: a generic `--filter key=value` surface is the tightest implementation, but the task should make sure common usage is still readable enough for humans. The current recommendation is to pair it with explicit `--entity-type` support rather than proliferating per-filter flags.
- **Entity output breadth**: returning only search preview fields would make inspection too shallow, while entity-specific pretty printers would expand scope quickly. The recommended balance is generic full-row JSON plus simple text rendering that includes long-form content fields when present.
- **JSON stability**: status and search use enums, dataclasses, and timestamps. Implementation should normalize these into a deterministic JSON contract early so future tasks do not have to break it.
- **Adjacent doc drift**: `agentic/README.md` already lags task-205. This task should avoid broad cleanup, but if command examples are touched, it should not leave newly implemented `search` behavior undocumented.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan doc during implementation with final planning/build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only to fix the stale `task-503.targetPath` and later mark `task-503` completed.
- Add `.agent/plans/agentic/research/task-503-search-cli-and-status-commands.md` with durable findings about the accepted CLI syntax, JSON contract, entity-inspection shape, and verification caveats.
- Apply only the minimum workflow/doc updates needed so `.agent/workflows/agentic-kb.md` no longer claims `search` is future work once this task lands, while keeping the broader final doc reconciliation in `task-803` and `task-804` pending as intended.

## Implementation Notes

- Implemented parser wiring in `agentic/src/agentic_kb/cli.py` for `search`, `status --json`, and grouped `entity get` without changing the packaged entrypoint.
- Added `agentic/src/agentic_kb/commands/search.py` to parse repeated `--entity-type` and `--filter key=value`, call the existing `PostgresSearchStore`, print concise ranked text output, and emit deterministic JSON on success.
- Added `agentic/src/agentic_kb/commands/entity.py` to validate registry-backed entity types before querying, fetch one full row generically by configured primary key, return exit code `2` for invalid entity types, and return exit code `4` plus stderr-only diagnostics for not-found rows.
- Added `agentic/src/agentic_kb/commands/output.py` so `search --json`, `status --json`, and `entity get --json` share one JSON-safe serialization boundary for enums, dates, timestamps, UUID/path-like values, mappings, and sequences.
- Extended `agentic/src/agentic_kb/commands/status.py` with explicit JSON serialization while preserving existing human text output and `--healthcheck` exit-code semantics.
- Added focused unit and DB-backed tests for the new command behavior, plus minimal workflow and README updates so the documented local CLI now matches the implemented command surface.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/cli.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/commands/search.py" "agentic/src/agentic_kb/commands/entity.py" "agentic/src/agentic_kb/commands/output.py" "agentic/tests/test_status_command.py" "agentic/tests/test_search_command.py" "agentic/tests/test_search_command_db.py" "agentic/tests/test_entity_command.py" "agentic/tests/test_entity_command_db.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_search_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_entity_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task503-verify -f docker-compose.agentic.yml build kb-tools` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task503-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_search_command_db.py'` passed against an isolated ParadeDB stack.
- `AGENTIC_DB_PORT=5761 docker compose -p task503-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_entity_command_db.py'` passed against the same isolated ParadeDB stack.
- `AGENTIC_DB_PORT=5761 docker compose -p task503-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_status_command_db.py'` passed.
- `PYTHONPATH=agentic/src python3 -m agentic_kb.cli --help` and local `search`, `entity`, and `status` help checks passed.
- Packaged smoke checks passed for `kb-tools search ... --json`, `kb-tools entity get ... --json`, and `kb-tools status --json` against the isolated DB. The first entity smoke probe intentionally failed because the earlier DB test bootstrap had replaced the seed row; rerunning it against an existing seeded row passed and confirmed the installed CLI contract.

## Outcome

- `task-503` is complete: the packaged `agentic-kb` CLI now exposes local `search`, machine-readable `status --json`, and generic `entity get` inspection on top of the existing packaged search and DB-status foundations.
- Acceptance criteria are satisfied for the CLI surface, deterministic JSON success output, stderr-only error behavior, registry-backed entity lookup validation, focused unit coverage, isolated DB-backed verification, workflow and README alignment, research capture, and tracker metadata alignment.
- The task stayed within scope: no MCP server behavior, sync orchestration, ranking redesign, or schema/index changes were introduced.

## Review Outcome

- The implementation review loop is clean: iteration 2 of `.agent/plans/agentic/task-plans/task-503-impl-review.md` ended with `Decision: approved` after reordering `entity get` validation so unsupported entity types fail before any missing-DB-config path.

## Planning Status Rationale

- Planning status is `approved` because iteration 3 of `.agent/plans/agentic/task-plans/task-503-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the implementation, verification, workflow/tracking updates, research note, and implementation review loop all finished with `Decision: approved`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-503-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-503-impl-review.md`
