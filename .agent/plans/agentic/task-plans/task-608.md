# Task Plan: task-608 Enforce snapshot embedding-contract compatibility

- Task ID: `task-608`
- Title: `Enforce snapshot embedding-contract compatibility`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`

## Why This Task Was Chosen Now

- `task-608` was the next safety-critical Phase 6 follow-up after `task-602`, `task-612`, `task-503`, and `task-701`: snapshot export/import, disposable-target enforcement, status reporting, and post-import incremental sync all existed, but they still allowed silent embedding-contract drift.
- The PRD and task tracker already lock the acceptance scope for this task: reject incompatible snapshot imports, surface mismatch clearly in status output, and block post-import sync when the snapshot embedding model or vector contract does not match the local KB contract.
- At planning time, repo reality confirmed the gap this task closed. The external manifest schema at `agentic/config/snapshot-manifest.schema.json` then recorded only `embedding_model`, while runtime code used a fixed vector width from `agentic/src/agentic_kb/embed/client.py` and schema-level `VECTOR(384)` columns from `agentic/schema/init.sql` without any versioned compatibility contract tying those together.
- Runtime validation reality is dual-path: the checked-in root schema at `agentic/config/snapshot-manifest.schema.json` is the canonical source of truth, while `agentic/src/agentic_kb/snapshot_manifest.py` loads the packaged runtime copy at `agentic/src/agentic_kb/config/snapshot-manifest.schema.json`. Task-608 must keep those paths aligned without re-opening the task-610 source-of-truth decision.
- This task is on the critical path for `task-609`, `task-611`, `task-606`, `task-705`, `task-901`, and `task-906`, because shared-baseline handoff is not trustworthy until import, status, and sync all enforce the same embedding contract boundary.

## Scope

- Define the current runtime KB embedding contract in implementation terms and make it available to snapshot, status, and sync code.
- Extend the snapshot manifest contract so it carries an explicit embedding contract that includes at least embedding model name, expected vector dimensionality, and a versioned contract identifier.
- Fail snapshot import before destructive restore when the imported manifest contract does not match the local runtime KB contract expected by the current tooling.
- Surface embedding-contract compatibility explicitly in `status` output without redefining top-level readiness semantics.
- Block post-import incremental sync, specifically `sync changed`, when the imported snapshot contract does not match the local runtime KB contract.
- Add focused unit and DB-backed tests for the compatibility guard behavior across snapshot import, status inspection, and post-import sync.

## Non-Goals

- Do not define the broader operator-facing republish policy and discovery workflow text promised by `task-611`, except for the minimal wording needed to keep this task's implementation and status output accurate.
- Do not redesign the disposable import-target checks from `task-612` or the manifest dump/hash validation from `task-602`.
- Do not add helper publish/fetch commands, shared-storage selection, or publication automation; those remain later tasks.
- Do not broaden this task into full snapshot round-trip coverage or a separate compatibility-only test file if the existing snapshot/status/sync suites can absorb the coverage cleanly.
- Do not change the canonical embedding runtime itself in this task unless a minimal refactor is required to expose the already-shipped contract cleanly.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-601` locked the canonical external snapshot manifest schema and sidecar contract.
  - `task-602` implemented manifest-aware export/import and persists raw manifest metadata in `agentic.kb_snapshot_manifest`.
- `task-503` shipped the current `status` command surface and JSON/text output structure.
- `task-604` and `task-701` shipped the post-import `sync changed` path that now needs a compatibility gate.
- `task-612` already guarantees import targets are disposable; task-608 must compose with that guard, not bypass it.
- Direct downstream tasks unblocked or clarified by this work:
  - `task-609` snapshot compatibility guard regression coverage.
  - `task-611` canonical embedding contract and republish-policy docs.
  - `task-606`, `task-705`, `task-901`, and `task-906`, which all depend on explicit mismatch handling in import, status, and sync.
- Repo evidence reviewed for this plan:
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/workflows/agentic-kb.md`
  - `agentic/config/snapshot-manifest.schema.json`
  - `agentic/src/agentic_kb/config/snapshot-manifest.schema.json`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/embed/client.py`
  - `agentic/src/agentic_kb/snapshot_manifest.py`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/tests/test_snapshot_command.py`
  - `agentic/tests/test_snapshot_command_db.py`
  - `agentic/tests/test_status_command.py`
  - `agentic/tests/test_status_command_db.py`
  - `agentic/tests/test_sync_command.py`
  - `agentic/tests/test_sync_command_db.py`

## Files Expected To Change

- `agentic/config/snapshot-manifest.schema.json` - extend the canonical manifest contract so snapshot metadata includes a versioned embedding contract, expected vector dimensionality, and embedding model name in one explicit structure.
- `agentic/src/agentic_kb/config/snapshot-manifest.schema.json` - update the packaged runtime copy in lockstep with the canonical root schema so validator behavior matches the checked-in contract.
- `agentic/src/agentic_kb/config.py` - add, only if needed, a narrow runtime helper or dataclass for embedding-contract representation/comparison so snapshot, status, and sync share one contract view without introducing a new raw embedding-dimension constant.
- `agentic/src/agentic_kb/snapshot_manifest.py` - add manifest helpers for building, validating, extracting, and comparing the embedding contract.
- `agentic/src/agentic_kb/commands/snapshot.py` - export the richer embedding contract metadata and reject incompatible imports before schema drop.
- `agentic/src/agentic_kb/commands/status.py` - surface snapshot-versus-runtime embedding compatibility clearly in normal status output and JSON.
- `agentic/src/agentic_kb/commands/sync.py` - refuse `sync changed` when the current imported-baseline manifest contract does not match the local runtime contract.
- `agentic/tests/test_snapshot_command.py` - add unit coverage for manifest contract generation and import mismatch rejection.
- `agentic/tests/test_snapshot_command_db.py` - extend DB-backed coverage for compatible versus incompatible imports and persisted manifest metadata.
- `agentic/tests/test_status_command.py` - add status output/JSON assertions for compatible, incompatible, and unavailable snapshot-contract inspection states.
- `agentic/tests/test_status_command_db.py` - only if DB-backed coverage is needed for the real snapshot-manifest row inspection path.
- `agentic/tests/test_sync_command.py` - add unit coverage that `sync changed` refuses incompatible imported baselines before any sync work begins.
- `agentic/tests/test_sync_command_db.py` - extend DB-backed coverage for the post-import incremental-sync compatibility block if current in-memory coverage is not sufficient.
- `.agent/workflows/agentic-kb.md` - only for minimal wording updates so the documented current behavior matches the enforced mismatch handling.
- `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md` - capture durable implementation findings and verification evidence after implementation lands.

## Implementation Approach

- Preserve the current packaged CLI surfaces: `agentic-kb snapshot import`, `agentic-kb status`, and `agentic-kb sync changed` stay the operator entrypoints, with compatibility enforcement added inside the existing modules instead of via a new coordination layer.
- Introduce one explicit runtime embedding-contract representation used everywhere in code. That contract must include at least:
  - embedding model name
  - expected vector dimensionality
  - versioned contract identifier
- Any new helper added for task-608 must centralize representation/comparison only. It must not introduce a new raw embedding-dimension constant in `config.py` or anywhere else.
- Anchor the runtime contract to current repo reality rather than operator guesswork:
  - embedding model comes from the configured `OLLAMA_EMBED_MODEL`
  - vector dimensionality must be derived from the already-shipped runtime/schema contract, currently `EXPECTED_EMBEDDING_DIMENSION = 384` in `agentic/src/agentic_kb/embed/client.py` and `VECTOR(384)` in `agentic/schema/init.sql`, rather than re-declared as a third constant
  - the versioned contract identifier should be an explicit constant under source control, not inferred from the model string alone
- Update the canonical manifest schema to carry the embedding contract as structured metadata rather than only a bare `embedding_model` string. Keep the change minimal and deterministic so `task-602` export/import semantics remain intact while the external contract becomes enforcement-grade.
- Keep root-versus-packaged schema-path reality explicit during implementation: update the canonical root schema at `agentic/config/snapshot-manifest.schema.json`, then ensure the packaged runtime copy at `agentic/src/agentic_kb/config/snapshot-manifest.schema.json` stays mechanically aligned so export/import validation uses the same contract.
- Legacy-manifest policy is explicit and intentionally strict. Older imported snapshots that carry only `embedding_model` and omit expected vector dimensionality or versioned contract identifier are not safe to treat as compatible. Under task-608, they must be treated as unsupported for compatibility-sensitive flows:
  - snapshot import must reject those legacy manifests before destructive restore with a clear error that the snapshot lacks the required embedding-contract metadata for this tooling version
  - status may report that an already-imported legacy snapshot row has `status=unsupported_legacy_manifest` or equivalent, but it must not pretend compatibility is known
  - `sync changed` must refuse to extend an imported baseline whose latest imported manifest lacks the full embedding contract, because unsupported compatibility is not safe to mix
- Compatibility comparison must be exact for the three required fields. A mismatch in model name, vector dimensionality, or contract identifier is a hard failure for import and for post-import incremental sync.
- Import validation ordering must remain strict and safe:
  - parse and validate manifest JSON
  - resolve the sibling dump
  - validate size/hash
  - validate disposable target if still needed in the existing order
  - compare imported snapshot embedding contract to the local runtime contract
  - only then allow destructive restore
- Reuse persisted snapshot-manifest metadata rather than inventing a second store for status/sync checks. `agentic.kb_snapshot_manifest` already keeps raw manifest JSON and import timestamps.
- Imported snapshot precedence is explicit for all compatibility reporting and enforcement:
  - status and `sync changed` must prefer the latest row with non-`NULL imported_at`
  - export-only rows with `imported_at IS NULL` must not drive compatibility reporting or enforcement for current-KB safety decisions
  - if no imported row exists, status should report a separate `missing_imported_snapshot_metadata` or equivalent compatibility state and `sync changed` should preserve current local-baseline behavior rather than inventing a new snapshot requirement
- Define the status behavior tightly:
  - normal `status` should add a separate embedding-compatibility section/object based on the latest imported snapshot row versus the local runtime contract
  - top-level `status.ok` must remain about runtime/dependency/database readiness and must not silently change meaning because of embedding compatibility
  - the new compatibility section/object should carry its own state, detail, snapshot source information, and local/runtime contract details so scripts and operators can distinguish `compatible`, `incompatible`, `missing_imported_snapshot_metadata`, `unsupported_legacy_manifest`, and `unavailable`
- Define the `sync changed` guard narrowly but decisively:
  - before any docs/code/GitHub/Project delta work starts, inspect the imported baseline manifest metadata
  - use the latest imported `agentic.kb_snapshot_manifest` row only; ignore export-only rows for this decision
  - if no imported snapshot metadata exists, preserve current baseline-loading behavior and do not invent a new requirement for purely locally built KBs
  - if the latest imported row lacks the full embedding contract or differs from the runtime contract, fail immediately with an operator-facing message that explains the imported baseline is not safe to extend and the KB should be recreated or rebuilt with a compatible snapshot
- Keep the import and sync error messages recovery-oriented and consistent with the PRD: recreate the disposable KB volume, then import a compatible snapshot or rebuild locally.
- Prefer small shared helpers over cross-module duplication. A compact compatibility helper used by snapshot, status, and sync is preferable to three slightly different comparison implementations.
- Keep task-608 scoped to enforcement and surfacing. The broader canonical-policy docs, republish cadence, and operator discovery narrative remain `task-611` except for any minimal wording needed so current workflow docs do not lie.

## Acceptance Criteria

- The canonical snapshot manifest contract records an explicit embedding contract that includes at least embedding model name, expected vector dimensionality, and a versioned contract identifier.
- The checked-in canonical schema at `agentic/config/snapshot-manifest.schema.json` and the packaged runtime schema copy at `agentic/src/agentic_kb/config/snapshot-manifest.schema.json` remain aligned after the contract update.
- `snapshot export` writes that embedding contract into the manifest using the same local runtime contract used by the running tooling.
- `snapshot import` rejects incompatible snapshots before destructive restore when the manifest's embedding model, expected vector dimensionality, or versioned contract identifier does not exactly match the local runtime contract.
- `snapshot import` also rejects legacy manifests that expose only `embedding_model` and omit the full required embedding contract fields, because unsupported compatibility must not be treated as safe.
- The import failure message clearly identifies the mismatch and points operators to recreate the disposable KB and use a compatible snapshot or rebuild locally.
- Normal `status` surfaces embedding-contract compatibility in a separate section/object clearly enough that operators can distinguish database readiness from sync-safe compatibility.
- `status --json` exposes machine-readable embedding compatibility state and details as a separate object rather than folding it into existing readiness or freshness buckets.
- `status` and `sync changed` both prefer the latest imported snapshot manifest row and ignore export-only rows for current-KB compatibility decisions.
- `sync changed` refuses to run when the latest imported snapshot manifest recorded in the KB does not match the local runtime embedding contract or lacks the full required contract metadata.
- `sync changed` preserves the existing behavior for non-imported/local-only baselines rather than requiring snapshot metadata for every KB.
- Compatibility enforcement composes safely with existing task-602/task-612 validation order and does not weaken manifest validation, dump verification, or disposable-target checks.
- The implementation keeps scope tight and does not fold in `task-611` policy docs beyond strictly necessary accuracy updates.

## Verification Plan

- Run `python3 -m py_compile` on all touched Python modules and tests.
- Run focused unit suites:
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'`
  - `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_command.py'`
- Add deterministic unit coverage for:
  - runtime embedding-contract construction from current config/constants
  - manifest construction/validation with the richer embedding-contract shape
  - import rejection when model name mismatches
  - import rejection when vector dimensionality mismatches
  - import rejection when contract identifier mismatches
  - explicit import rejection for legacy manifests that expose only `embedding_model`
  - status text and JSON serialization for `compatible`, `incompatible`, `missing_imported_snapshot_metadata`, `unsupported_legacy_manifest`, and `unavailable` compatibility states
  - imported-row precedence over export-only rows for status and sync decisions
  - `sync changed` failing before any source-specific sync work when the latest imported snapshot metadata is incompatible or legacy/underspecified
- Run DB-backed verification with `AGENTIC_TEST_DATABASE_URL` to prove:
  - export persists the richer manifest contract
  - import succeeds for a compatible snapshot
  - import fails before restore for an incompatible snapshot contract
  - import fails before restore for a legacy manifest that lacks full contract fields
  - post-import `sync changed` refuses an incompatible or legacy imported baseline but still works for a compatible one
  - status inspection reports the real DB-backed compatibility state from the latest imported `kb_snapshot_manifest` row and ignores export-only rows for this purpose
- If packaging/runtime assets change, rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` and run the focused suites in-container so installed-package behavior matches source behavior.

## Risks / Open Questions

- The legacy-manifest safety rule is now intentionally strict, but the exact user-facing wording still matters: the error should clearly say the snapshot is too old or underspecified for compatibility-safe import under current tooling, not merely that validation failed.
- There is still a subtle status-contract question around presentation, not semantics: the plan now fixes compatibility as a separate status object/section and keeps top-level `status.ok` unchanged, but critique should stress-test whether that object shape is concrete enough for scripts without widening into unrelated status redesign.
- `sync changed` currently operates from sync-state baselines only. Adding the imported-row compatibility block should not accidentally require imported snapshot metadata for locally built KBs that were never restored from a snapshot.
- The runtime contract should have one authoritative dimensionality source for code-level comparison. Repo reality currently duplicates `384` in schema and embedding client; task-608 must not add a third drifting constant and should limit any new helper to deriving from the already-shipped runtime/schema contract.
- Minimal workflow-doc edits may be necessary so the shipped docs reflect the new hard failure modes, but broader canonical policy language must stay deferred to `task-611`.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final implementation notes, verification notes, outcome, and approved statuses once the task lands.
- Append further planning review decisions to `.agent/plans/agentic/task-plans/task-608-plan-review.md`.
- Do not create the implementation review log during planning.
- Add `.agent/plans/agentic/research/task-608-snapshot-embedding-contract-compatibility.md` after implementation with durable findings, the explicit legacy-manifest rejection rule, imported-row precedence behavior, and verification evidence.
- Update `.agent/workflows/agentic-kb.md` only if minimal wording changes are needed so the workflow accurately states the shipped import/status/sync mismatch behavior.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` later only for `task-608` task metadata when implementation is complete.

## Final Implementation Notes

- The shipped manifest contract now requires a structured `embedding_contract` object with exact `contract_id`, `embedding_model`, and `embedding_dimension` fields in both schema copies and the checked-in example manifest.
- `agentic/src/agentic_kb/snapshot_manifest.py` now owns the shared runtime contract representation and comparison helpers. Runtime contract construction derives dimensionality from the already-shipped embedding runtime constant in `agentic_kb.embed`, so task-608 did not introduce a third raw dimension constant.
- `snapshot export` now writes the full runtime embedding contract into the manifest and persists that richer manifest metadata into `agentic.kb_snapshot_manifest`.
- `snapshot import` now rejects legacy `embedding_model`-only manifests and any incompatible snapshot embedding contract before destructive restore. The operator-facing failure path points back to recreating the disposable KB and importing a compatible snapshot or rebuilding locally.
- `status` now reports embedding compatibility as a separate section/object instead of changing top-level readiness semantics. It inspects only the latest imported snapshot row, reports `missing_imported_snapshot_metadata` for local-only KBs, reports `unsupported_legacy_manifest` for imported legacy manifests, and treats malformed persisted compatibility metadata as unavailable/invalid rather than silently coercing it.
- `sync changed` now checks the latest imported snapshot manifest before any sync work or embedding-client creation. Imported incompatible, legacy, or malformed manifests are rejected immediately; local-only KBs without imported snapshot metadata remain allowed.
- Focused regression coverage landed in the manifest schema tests, snapshot/status/sync unit tests, DB-backed snapshot/status/sync suites, and the MCP status test that serializes the new status payload shape.

## Final Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/src/agentic_kb/commands/snapshot.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/commands/sync.py" "agentic/tests/test_snapshot_manifest_schema.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py" "agentic/tests/test_status_command.py" "agentic/tests/test_status_command_db.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py" "agentic/tests/test_mcp_search_server.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_command.py'` passed.
- DB-backed coverage was added for snapshot import, status compatibility inspection, and `sync changed` compatibility enforcement, but execution remained skipped in this environment because `AGENTIC_TEST_DATABASE_URL` was not set.
- Final verification status is therefore: implementation complete, focused source-level/unit verification passed, DB-backed execution pending only on environment availability rather than missing coverage.

## Final Outcome

- `task-608` is complete. Snapshot manifests now carry an explicit embedding contract, import rejects incompatible or legacy manifests before restore, `status` surfaces compatibility separately from readiness, and `sync changed` refuses to extend imported incompatible baselines while preserving local-only behavior.
- The task stayed within the approved safety scope and did not widen into task-611 policy rollout work beyond the minimal workflow truthfulness update needed to describe the shipped behavior.

## Review Outcome

- Planning review log: `.agent/plans/agentic/task-plans/task-608-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-608-impl-review.md`
- Planning concluded with `Decision: approved` after Iteration 3 in the planning review log.
- Implementation concluded with approval after the schema/runtime parity fixes and DB-backed test additions requested by review.
- The implementation review log needed one append-only sequencing repair: `Implementation: Iteration 4` records that `Code Review: Iteration 3` had been appended before `Code Review: Iteration 2`. The repair preserved history without rewriting prior entries, and no source-code changes were made in that repair step.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-608-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-608-impl-review.md` (do not create during planning)

## Required User Inputs

- None required for planning or for the expected implementation path.

## Required Manual Test Steps

- None required before implementation begins; the expected acceptance boundary is primarily automated unit and DB-backed verification.

## What Evidence Is Needed Back From The User

- None before implementation can proceed.

## Whether Implementation Can Proceed Before That User Interaction

- Yes. The task is fully agent-executable inside the repo with code changes and focused automated verification.
