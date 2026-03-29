# Task Plan: task-602 Implement snapshot export and import commands

Historical note: this task plan predates the retirement of the GitHub Actions-based snapshot publication path. Any references here to `task-603` as a future GitHub Actions publication workflow are now outdated; local GPU-backed publication to private shared storage is the only supported v1 direction.

- Task ID: `task-602`
- Title: `Implement snapshot export and import commands`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-602` is the next unblocked critical-path task after `task-601`: the repo already has real dump-only `snapshot export` and destructive `snapshot import`, but they still stop short of the approved manifest-aware sharing contract.
- `task-205` delivered the live PostgreSQL dump and restore mechanics, and `task-601` locked the external manifest schema plus sync-state normalization rules that `task-602` now needs to make export/import portable and verifiable.
- This task directly gates `task-603`, `task-604`, `task-803`, and `task-901`, all of which assume a developer can export a dump plus manifest pair, import that pair safely, and trust the imported baseline metadata.
- The current workflow doc still says manifest generation and validation are later work, so landing `task-602` now closes the biggest remaining gap in the Phase 6 snapshot-sharing foundation without pulling in publication automation or sync orchestration.

## Scope

- Extend `agentic-kb snapshot export` so it emits the approved manifest sidecar alongside the PostgreSQL custom dump.
- Extend `agentic-kb snapshot import` so it accepts a manifest-aware snapshot input, validates the dump against the approved manifest contract, and then performs the existing destructive restore flow.
- Compute and persist the manifest artifact identity required by `task-601`: sibling dump filename, size, and `sha256:<lowercase-hex>` content hash.
- Read the existing KB state needed to populate manifest fields: repo/docs baseline, embedding model, entity counts, and normalized sync-state export.
- Store import or export metadata in `agentic.kb_snapshot_manifest` in a way that stays aligned with the richer external manifest JSON.
- Add focused unit and DB-backed tests for manifest-aware export/import behavior.

## Non-Goals

- Do not implement snapshot publication automation, GitHub Actions artifact upload, or baseline distribution workflow; that remains `task-603`.
- Do not implement import-then-`sync changed` orchestration or any `sync` CLI behavior; that remains `task-604` and `task-701`.
- Do not redesign the approved manifest schema from `task-601` unless implementation finds a concrete blocker that must be captured as follow-up.
- Do not broaden this task into general KB freshness, stale-index detection, or MCP setup.
- Do not replace the existing destructive restore safety boundary from `task-205`; manifest awareness should wrap that flow, not weaken it.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-205` - real `pg_dump` export and destructive filtered-schema restore already exist in `agentic/src/agentic_kb/commands/snapshot.py`.
  - `task-405` - sync-state persistence already provides the repo, GitHub, and Project watermark inputs that the manifest must normalize.
  - `task-503` - the packaged CLI surface is stable, so `task-602` can keep working inside `agentic/src/agentic_kb/commands/snapshot.py` without inventing a new entrypoint.
  - `task-601` - the canonical manifest schema, example fixture, helper constants, and sync-state normalization rules now exist in `agentic/config/snapshot-manifest.schema.json`, `agentic/config/snapshot-manifest.example.json`, and `agentic/src/agentic_kb/snapshot_manifest.py`.
- Direct downstream tasks unblocked by this work:
  - `task-603` - needs a real manifest-producing snapshot command before publication automation can ship.
  - `task-604` - needs import to preserve a trustworthy baseline before delta sync can build on top of it.
  - `task-803` and `task-901` - need a real documented manifest-aware operator flow.
- Tracking inconsistencies to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently lists `task-602.targetPath` as `agentic/src/snapshot/commands.py`, but repo reality puts the live snapshot commands in `agentic/src/agentic_kb/commands/snapshot.py`; this task should treat that as tracker drift and update it when implementation lands.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/snapshot_manifest.py`
  - `agentic/schema/init.sql`
  - `agentic/tests/test_snapshot_command.py`
  - `agentic/tests/test_snapshot_command_db.py`
  - `agentic/tests/test_snapshot_manifest_schema.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile `task-602.targetPath` with the packaged snapshot command path and later update only `task-602` status/completion metadata.
- `agentic/src/agentic_kb/commands/snapshot.py` - add manifest-aware export and import behavior on top of the existing dump/restore implementation.
- `agentic/src/agentic_kb/snapshot_manifest.py` - add any narrow manifest-building, hashing, validation, or DB-mapping helpers that keep the command module focused.
- `agentic/tests/test_snapshot_command.py` - expand unit coverage for sidecar manifest pathing, digest generation, validation failures, and manifest-aware command flow.
- `agentic/tests/test_snapshot_command_db.py` - expand DB-backed verification for manifest plus dump round-trips and import validation.
- `agentic/tests/test_snapshot_manifest_schema.py` - only if a narrow regression test is needed to lock task-602 usage assumptions without changing the schema contract itself.
- `.agent/workflows/agentic-kb.md` - update the workflow note so it reflects that manifest generation and import validation are now real.
- `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md` - add durable findings about the accepted export/import contract, DB mappings, and verification behavior.
- `agentic/config/snapshot-manifest.example.json` - only if implementation proves the checked-in fixture needs a small realism update while staying schema-compatible.

## Implementation Approach

- **Preserve the CLI surface**: keep `agentic-kb snapshot export` and `agentic-kb snapshot import` as the operator-facing verbs and extend the existing packaged module rather than creating a parallel snapshot command package.
- **Sidecar contract**: treat the manifest as a sibling JSON file to the dump artifact. Export should create one dump and one manifest in the same directory, with the manifest referencing the dump by basename-only `artifact.filename` per `task-601`.
- **Default naming**: keep the current timestamped dump naming pattern from `task-205` and derive the manifest filename deterministically from the same snapshot name so developers get a predictable pair rather than two unrelated filenames.
- **Canonical pair naming**: export should keep the existing `.dump` artifact name and emit the manifest beside it as `<snapshot_name>.manifest.json`. Import should do no directory scanning: if the operator passes a dump path, resolve the manifest only as the same-directory sibling produced by replacing the trailing `.dump` suffix with `.manifest.json`; if the operator passes a manifest path, use that exact file and resolve the dump only from `artifact.filename` in the same directory.
- **Manifest construction**: build the export manifest from live repo and DB state, not from the checked-in example fixture. Use `agentic/src/agentic_kb/snapshot_manifest.py` as the single place for shared constants and normalized sync-state mapping, and extend it only with narrow helper functions if that reduces duplication.
- **Consistent export view**: the manifest metadata must describe the same logical KB state as the exported dump, not a best-effort later read. The recommended implementation is one repeatable-read database transaction that gathers manifest metadata, exports a PostgreSQL snapshot identifier, and then runs `pg_dump` against that exact snapshot before committing; if the current client/runtime cannot do that safely, export must fail loudly rather than emit a potentially mismatched dump/manifest pair.
- **Artifact identity generation**: after `pg_dump` succeeds, compute exact `size_bytes` from the written dump and compute a deterministic SHA-256 digest string in the `sha256:<lowercase-hex>` format approved by `task-601`.
- **Entity count sourcing**: query the current seven searchable KB tables and populate only the approved count keys: `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.
- **Repo baseline sourcing**: populate `repo.name`, `repo.docs_commit_hash`, and `repo.code_commit_hash` from the current persisted sync-state rows rather than inventing new storage.
- **Embedding model sourcing**: use the active configured embedding model name that already anchors the current KB contents, and fail clearly if the implementation cannot determine a trustworthy value for the exported manifest.
- **Manifest schema enforcement on export**: validate the generated manifest against `agentic/config/snapshot-manifest.schema.json` before writing success output so export cannot emit an invalid sidecar.
- **Import input resolution**: support manifest-aware import from either the dump path or the manifest path, but only with deterministic same-directory sibling rules. If the operator passes a dump path, import must require the derived `<snapshot_name>.manifest.json` sibling; if the operator passes a manifest path, import must require the manifest's `artifact.filename` dump sibling. Any missing file, directory-bearing artifact filename, basename mismatch, or attempt to resolve outside the manifest directory must fail with a precise error before restore.
- **Import validation boundary**: before destructive restore, import must parse the manifest, validate it against the schema, resolve the sibling dump path, verify that the dump exists, and compare on-disk size plus content hash to the manifest values. Any mismatch should fail before schema drop begins.
- **Restore behavior preservation**: once validation passes, reuse the existing filtered `agentic`-schema restore flow from `task-205` so task-602 adds verification and metadata handling without regressing restore safety.
- **Internal table alignment**: write a summarized record into `agentic.kb_snapshot_manifest` after successful export and after successful import, preserving the raw manifest JSON in `manifest`, mapping artifact digest into `content_hash`, using external `snapshot_name` and `snapshot_created_at`, and storing a useful local source path for operator inspection.
- **Singular repo-commit mapping**: because `agentic.kb_snapshot_manifest` has only one `repo_commit_hash` column while the external manifest carries separate docs/code baselines, map the internal summary field deterministically: store the shared commit hash when `repo.docs_commit_hash` and `repo.code_commit_hash` are equal, otherwise store `NULL` and rely on the raw `manifest` JSON for the full dual-baseline detail. This avoids inventing a lossy precedence rule.
- **Failure ordering**: do not record successful snapshot-manifest DB metadata before the external dump and manifest are both written and validated. On import, do not record an imported manifest row unless validation and restore both complete.
- **Test structure**: keep pure file-path, hashing, and schema-validation logic unit-testable without a live database, and keep live ParadeDB coverage focused on full export/import round-trips plus `kb_snapshot_manifest` persistence.
- **Scope guardrail**: keep publication, CI artifact handling, and sync follow-up behavior explicitly out of this implementation even if the command now produces the metadata those later tasks need.

## Acceptance Criteria

- `agentic-kb snapshot export` produces both a PostgreSQL custom dump and a schema-valid sidecar manifest in the same directory.
- The exported manifest matches the approved `task-601` contract, including basename-only dump filename, exact `size_bytes`, `sha256:<lowercase-hex>` content hash, repo baseline, embedding model, seven-key entity counts, and normalized sync state.
- Export validates the generated manifest against `agentic/config/snapshot-manifest.schema.json` before reporting success.
- Exported manifest metadata and the dump contents come from the same logical database snapshot rather than from separate best-effort reads.
- `agentic-kb snapshot import` validates the manifest, verifies the dump artifact identity against on-disk size and digest, and only then performs the existing destructive `agentic`-schema restore flow.
- Import fails before schema drop if the manifest is missing, invalid, points outside the sibling snapshot pair contract, cannot resolve the exact same-directory counterpart, or does not match the dump on disk.
- Successful export and successful import both persist aligned metadata to `agentic.kb_snapshot_manifest` without requiring a schema migration.
- `agentic.kb_snapshot_manifest.repo_commit_hash` is populated only when the exported docs/code baselines match; otherwise it remains `NULL` and the raw manifest JSON preserves the full dual-baseline contract.
- The implementation keeps the existing packaged CLI surface and the existing restore-safety rules from `task-205`.
- The task does not add publication automation, sync orchestration, stale-index behavior, or unrelated tracker cleanup.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is corrected for `task-602.targetPath` before the task is marked complete.

## Verification Plan

- Run `python3 -m py_compile` on touched Python modules and tests.
- Run focused local unit suites such as `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` and any manifest-focused companion coverage.
- Add deterministic unit coverage for:
  - manifest filename derivation and sibling-file resolution
  - operator input resolution from both dump-path and manifest-path entry without cross-directory guessing
  - SHA-256 digest and byte-size calculation for a written dump file
  - export manifest generation from stubbed DB and sync-state inputs
  - same-logical-snapshot export behavior, including failure if the implementation cannot guarantee snapshot consistency
  - schema validation failures for malformed generated or imported manifests
  - import rejection for missing sibling files, mismatched digest, mismatched byte size, and invalid artifact filename semantics
  - DB-table payload shaping for `agentic.kb_snapshot_manifest`, including the equal-baseline-or-null rule for `repo_commit_hash`
- Run DB-backed verification against an isolated ParadeDB instance via `AGENTIC_TEST_DATABASE_URL`, extending `agentic/tests/test_snapshot_command_db.py` to prove:
  - export writes a dump plus manifest pair
  - export metadata matches the same logical DB snapshot as the dump contents
  - import validates and restores that pair successfully
  - a tampered dump or tampered manifest fails before destructive restore
  - successful operations write expected rows into `agentic.kb_snapshot_manifest`
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` if packaging or runtime dependencies change.
- Run the focused snapshot suites inside `kb-tools` with `--entrypoint python` so installed-package behavior matches local source behavior.
- Update `.agent/workflows/agentic-kb.md` and verify its example commands still match the real CLI behavior after implementation.

## Risks / Open Questions

- **Export snapshot consistency mechanics**: the plan recommends a shared repeatable-read snapshot between metadata queries and `pg_dump`, but implementation needs to confirm the exact PostgreSQL client invocation pattern in this repo and capture any caveats in research/tests rather than silently degrading to best-effort metadata reads.
- **Embedding model trust**: the manifest needs one authoritative `embedding_model` value, but the KB may already contain rows written under a prior config. Implementation should prefer the explicit current config and capture any mismatch risk in research rather than silently inventing a hybrid value.
- **Internal-table mapping limits**: `agentic.kb_snapshot_manifest` stores a summary plus raw JSON. Task-602 should avoid overfitting the external document to this table while still making export/import history useful.
- **Hashing cost vs simplicity**: SHA-256 over a large dump is expected and acceptable for portability, but implementation should stream the file rather than reading the whole dump into memory.
- **Tracker drift nearby**: the tasks JSON clearly has a stale target path for `task-602`, and adjacent future sync-command tasks still appear to use old un-packaged paths. This task should fix the confirmed `task-602` drift and note any neighboring drift without broadening scope.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final planning status, build status, implementation notes, verification notes, and outcome when task-602 lands.
- Append planning review decisions in `.agent/plans/agentic/task-plans/task-602-plan-review.md` and create the implementation review log separately when implementation begins.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-602` metadata, including the packaged command `targetPath` correction and final task status.
- Add `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md` with durable findings about manifest-aware export/import behavior, import validation order, `kb_snapshot_manifest` mapping, and verification caveats.
- Update `.agent/workflows/agentic-kb.md` so it no longer says manifest generation and import validation are pending later tasks.
- Only update `.agent/plans/agentic/knowledge-base-platform-prd.md` if the final implementation reveals a meaningful contract clarification that the higher-level plan currently states inaccurately.

## Implementation Notes

- Extended `agentic/src/agentic_kb/commands/snapshot.py` so `snapshot export` now emits a sibling `.manifest.json`, captures metadata from the same logical PostgreSQL snapshot used by `pg_dump`, computes artifact size plus `sha256:<lowercase-hex>`, validates the manifest contract, and persists summary metadata to `agentic.kb_snapshot_manifest` only after export succeeds.
- Extended `agentic/src/agentic_kb/commands/snapshot.py` import flow so operators can pass either the dump or manifest path under deterministic same-directory sibling rules, with manifest schema validation plus dump size/hash verification running before any destructive schema drop.
- Added reusable manifest helpers in `agentic/src/agentic_kb/snapshot_manifest.py` for schema validation, manifest construction, timestamp parsing/formatting, deterministic record ids, and the equal-baseline-or-`NULL` mapping for `agentic.kb_snapshot_manifest.repo_commit_hash`.
- Expanded `agentic/tests/test_snapshot_command.py` to cover sibling path derivation, `.dump` enforcement on manifest-resolved artifacts, artifact hashing, export fail-loud behavior when `pg_export_snapshot()` cannot provide a usable identifier, and pre-restore validation paths.
- Expanded `agentic/tests/test_snapshot_command_db.py` to cover full export/import round-trips, schema-valid manifest tampering rejection before restore, the shared-baseline `repo_commit_hash` branch, and the existing filtered-restore safety boundary against non-`agentic` dump entries.
- Updated `.agent/workflows/agentic-kb.md` so the documented operator flow matches the now-real manifest-aware snapshot behavior.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/commands/snapshot.py" "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_snapshot_command_db.py" "agentic/tests/test_snapshot_manifest_schema.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed (`Ran 19 tests`, `OK`).
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed (`Ran 9 tests`, `OK`).
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- `docker compose -p task602-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb -e OLLAMA_EMBED_MODEL=all-minilm:l6-v2 kb-tools - <<'PY' ... unittest discover('agentic/tests', pattern='test_snapshot_command_db.py') ... PY` passed (`Ran 4 tests`, `OK`) after waiting for the in-container DB endpoint to accept connections.

## Outcome

- `task-602` now delivers manifest-aware snapshot export/import on top of the approved `task-601` contract, including export-side manifest emission, import-side manifest/dump validation before destructive restore, and persistent snapshot metadata rows in `agentic.kb_snapshot_manifest`.
- The implementation keeps publication automation, import-then-sync orchestration, and broader freshness work deferred to later tasks as planned.
- The task tracker, workflow note, review logs, and research capture now align with the completed implementation.

## Review Outcome

- Planning review is complete and approved in `.agent/plans/agentic/task-plans/task-602-plan-review.md`.
- Implementation review is complete and approved in `.agent/plans/agentic/task-plans/task-602-impl-review.md`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-602-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-602-impl-review.md`

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-602-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the manifest-aware export/import implementation, focused verification, workflow update, research capture, tracker sync, and clean implementation review loop are complete.
