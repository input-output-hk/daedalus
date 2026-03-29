# Task Plan: task-601 Define snapshot manifest format

- Task ID: `task-601`
- Title: `Define snapshot manifest format`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-601` is the next unblocked critical-path task after `task-503` in `.agent/plans/agentic/knowledge-base-platform-tasks.json`.
- Its direct dependencies are complete: `task-205` already shipped real `snapshot export` and destructive `snapshot import`, and `task-405` already persists the sync cursors and watermarks a portable manifest needs to describe.
- `task-205` explicitly deferred any external manifest contract, so the repo now has real snapshot behavior but still lacks the durable machine-readable schema that downstream export/import, bootstrap, automation, and documentation work need.
- This task gates `task-602`, `task-604`, `task-603`, `task-701`, `task-801`, `task-802`, `task-803`, and `task-901`, so the contract needs to land before more snapshot-sharing and freshness work builds on ad hoc assumptions.

## Scope

- Define the canonical external snapshot manifest format that accompanies exported KB dumps.
- Express that format as a durable machine-readable schema at `agentic/config/snapshot-manifest.schema.json`.
- Settle the public field vocabulary for snapshot identity, dump metadata, repo baseline, embedding model, entity counts, and exported sync-state metadata.
- Define the artifact-identity contract for the exported dump itself so downstream validation can prove a manifest matches one specific dump file without inventing new rules later.
- Define how current `task-405` sync-state rows map into the external manifest without leaking every internal DB column verbatim.
- Add the minimum companion fixtures, tests, and schema-adjacent documentation needed so `task-602` can generate and validate manifests against a stable contract.

## Non-Goals

- Do not change `agentic-kb snapshot export` or `agentic-kb snapshot import` behavior in this task; manifest-aware command plumbing remains `task-602`.
- Do not implement snapshot publication, import-then-sync bootstrap flow, stale-index detection, or MCP setup; those remain `task-603`, `task-604`, `task-701`, and later tasks.
- Do not redesign the existing `agentic.kb_sync_state` schema from `task-405` or the current `agentic.kb_snapshot_manifest` table from `task-202` unless implementation finds a hard blocker.
- Do not broaden this task into general workflow polishing beyond narrow manifest-contract references that future tasks need.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-205` - real snapshot dump/export and destructive import already exist in `agentic/src/agentic_kb/commands/snapshot.py`, but no external manifest file contract exists yet.
  - `task-405` - sync state already captures repo commit hashes, GitHub stream watermarks, and Project 5 cursor/watermark state in `agentic/src/agentic_kb/sync/state.py`.
  - `task-503` - current local CLI and status/search surfaces are stable enough that this task can stay schema-focused instead of also settling CLI UX.
  - `task-202` - `agentic/schema/init.sql` already includes `agentic.kb_snapshot_manifest`, so this task should align the external contract with an existing internal storage surface instead of inventing a disconnected model.
- Direct downstream tasks unblocked by this work:
  - `task-602` - needs the approved manifest schema before export/import can emit or validate sidecar manifest files.
  - `task-604` - needs a durable import baseline format for import-then-sync-changed behavior.
  - `task-603`, `task-701`, `task-801`, `task-802`, `task-803`, `task-901` - all depend on a stable snapshot-sharing contract for docs, automation, and onboarding.
- Current repo realities this plan must reconcile:
  - `agentic/config/` does not exist yet, so task-601 will need to create that directory instead of only editing an existing file.
  - `.agent/plans/agentic/knowledge-base-platform-prd.md` still describes snapshot contents at a high level only; task-601 must tighten that into an exact field contract without forcing the full task-602 implementation.
  - `task-205` uses `pg_dump --format=custom --compress=6` today, so the manifest contract must describe that dump identity precisely enough for later validation while still keeping checksum generation and enforcement out of scope until `task-602`.

## Files Expected To Change

- `agentic/config/snapshot-manifest.schema.json` - canonical machine-readable schema for exported snapshot manifests.
- `agentic/config/snapshot-manifest.example.json` - schema-aligned example fixture that downstream tasks and docs can copy from.
- `agentic/tests/test_snapshot_manifest_schema.py` - focused automated coverage for required fields, strictness, and example-manifest validity.
- `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md` - durable findings about the approved manifest shape, task-205/task-405 mapping rules, and any implementation caveats.
- `.agent/plans/agentic/knowledge-base-platform-prd.md` - only if the final schema naming or boundaries materially refine the current high-level snapshot-format bullets.
- `.agent/workflows/agentic-kb.md` - only if a narrow note is needed so the workflow stops implying the manifest contract is still undefined; broader workflow reconciliation still belongs to `task-803`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update only `task-601` status/completion metadata when the implementation lands; no target-path correction is currently required.
- `agentic/src/agentic_kb/commands/snapshot.py` or a small schema-helper module - only if implementation adds narrowly scoped constants or schema-loading helpers needed by tests, while still deferring manifest-aware command behavior to `task-602`.

## Implementation Approach

- **Contract-first boundary**: treat `agentic/config/snapshot-manifest.schema.json` as the canonical source of truth for the external manifest format. Task-601 should define the contract, not merely describe it in prose.
- **Top-level shape**: require one JSON object with a stable top-level vocabulary covering `$schema`, `schema_version`, `snapshot_name`, `snapshot_created_at`, `artifact`, `repo`, `embedding_model`, `entity_counts`, and `sync_state`.
- **Schema versioning**: start the external manifest contract at `schema_version = 1` and reserve schema-version bumps for backward-incompatible manifest changes rather than task-local implementation details.
- **Artifact section**: define a verification-grade artifact contract, not just descriptive metadata. Require a basename-only `filename` whose value is resolved relative to the manifest file's own directory, so copied or downloaded manifest/dump pairs stay portable without embedding machine-specific paths.
- **Artifact identity fields**: require `dump_format`, `compression`, `size_bytes`, and `content_hash` in addition to schema scope metadata. For v1, the contract should match current task-205 reality explicitly: `dump_format` identifies a PostgreSQL custom dump, `compression` records the current built-in dump compression configuration from `pg_dump --format=custom --compress=6`, `size_bytes` captures the exact artifact byte length, and `content_hash` is the immutable dump checksum value that downstream tasks compare against the on-disk dump.
- **Checksum expectation**: define `content_hash` as a required portable digest string for the dump artifact itself, aligned with `agentic.kb_snapshot_manifest.content_hash` so `task-602` can populate the DB table without inventing a second checksum field. Lock the v1 representation to one explicit algorithm/encoding format in the schema so later export/import validation can compare values mechanically.
- **Artifact locator boundary**: keep paths out of the manifest beyond the basename. Task-601 should define that the dump is the sibling file named by `artifact.filename`; generating that file, hashing it, and rejecting mismatches remain task-602 behavior.
- **Repo baseline section**: include the repo identity and repo commit used for docs/code ingestion so imported snapshots can be compared against current `HEAD` later without reinterpreting DB internals.
- **Entity counts vocabulary**: use one stable key set for the current searchable KB entities: `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`. Counts should be non-negative integers and additional entity keys should not appear without an intentional schema update.
- **Sync-state mapping**: normalize the current `task-405` state into a public manifest section rather than exposing raw `kb_sync_state` rows directly:
  - `docs` and `code` should carry the repo commit baseline plus last successful sync timestamp.
  - `github` should carry one nested record per current stream (`issues`, `pulls`, `issue_comments`, `review_comments`) with exported watermark timestamps.
  - `project` should carry the Project 5 cursor plus latest observed item watermark.
- **Public-field normalization**: use manifest-friendly field names and ISO-8601 UTC timestamps instead of leaking internal DB column names like `watermark_text`. `task-602` can map from internal storage to this external contract.
- **Failure-state boundary**: do not include transient `last_error` values in the canonical exported manifest. The manifest should describe the accepted exported KB baseline, not the operator history of failed sync attempts.
- **Internal table alignment**: keep the schema compatible with `agentic.kb_snapshot_manifest` by ensuring its summary columns can be populated from the external manifest, but do not force the external JSON to mirror the table one-for-one. The full external document can remain the richer source of truth, with the DB table storing a summarized subset plus raw manifest JSON.
- **Task boundary on generation/enforcement**: task-601 only defines the required artifact identity fields and their mapping targets. Computing `size_bytes` and `content_hash`, writing manifest values during export, storing rows in `agentic.kb_snapshot_manifest`, and enforcing dump/manifest match checks remain implementation work for `task-602`.
- **Companion artifacts**: the schema file alone is not enough. Add a checked-in example manifest and focused tests so downstream tasks have an executable reference rather than only prose.
- **Validation strategy**: prefer schema-focused tests and fixtures over early export/import plumbing. If implementation needs code, keep it limited to schema path/version constants or schema-loading helpers that reduce duplication for task-602.
- **Directory creation**: create `agentic/config/` as a narrowly scoped config home for this schema and future task-602/task-603 snapshot-sharing artifacts, not as a broad new settings system.

## Acceptance Criteria

- `agentic/config/snapshot-manifest.schema.json` exists and is the canonical machine-readable contract for exported snapshot manifests.
- The schema defines a stable top-level object with required fields for schema version, snapshot identity, creation timestamp, dump artifact metadata, repo baseline, embedding model, entity counts, and exported sync state.
- The schema requires artifact identity fields that let later tasks validate one specific dump against one specific manifest: basename-only `artifact.filename`, explicit dump-format and compression fields matching current `pg_dump --format=custom --compress=6` behavior, exact `size_bytes`, and required `content_hash`.
- The schema captures the current searchable entity-count vocabulary explicitly and rejects unplanned extra entity keys.
- The schema captures exported sync-state metadata for docs, code, GitHub streams, and Project 5 using manifest-friendly field names and normalized UTC timestamp strings.
- The contract is explicitly derived from current repo reality: current task-205 snapshot artifacts and current task-405 sync-state semantics.
- The artifact checksum field is explicitly mapped to `agentic.kb_snapshot_manifest.content_hash`, while still leaving checksum generation, DB writes, and runtime validation to `task-602`.
- A checked-in example manifest exists and conforms to the schema.
- Focused automated tests verify the schema parses, the example manifest is valid, and representative invalid payloads fail the expected required-field or strictness checks.
- The task does not add manifest-aware snapshot command behavior, publication workflows, bootstrap sync logic, or stale-index detection.
- Any optional code added in support of the schema stays narrowly scoped to schema loading/constants and does not pre-implement task-602 command flow.

## Verification Plan

- Run `python3 -m py_compile` on any new Python helper or test modules if task-601 adds them.
- Run focused automated coverage such as `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'`.
- Add deterministic test coverage for:
  - schema file JSON parsing
  - required top-level fields and required nested sections
  - artifact filename strictness and sibling-file semantics (for example, rejecting directory-bearing paths)
  - exact artifact identity requirements for dump format, compression metadata, `size_bytes`, and `content_hash`
  - strict entity-count key vocabulary
  - valid example-manifest shape
  - representative invalid manifests missing required fields or containing forbidden extra keys
  - normalized sync-state sections for docs/code/github/project
- Re-run `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` if implementation touches `agentic/src/agentic_kb/commands/snapshot.py` or shared snapshot constants.
- Treat live DB round-trip verification as out of scope for this task unless implementation unexpectedly touches snapshot command behavior; those runtime checks remain primarily task-602 work.

## Risks / Open Questions

- **External vs internal shape**: `agentic.kb_snapshot_manifest` already exists, but the external file format should not be forced into an awkward one-to-one mirror of current DB columns. The recommended approach is a clean public schema plus a documented mapping back into the table.
- **Dump metadata drift**: the platform plan mentions zstd-compressed custom dumps, while current task-205 code actually emits `pg_dump --format=custom --compress=6`. Task-601 should lock the v1 manifest fields to current behavior rather than the older prose, and treat any future export-format change as a deliberate follow-up contract update instead of an implicit assumption.
- **Entity-count vocabulary lock-in**: downstream tasks need stable keys, but locking the set too loosely weakens validation and locking it too tightly may require a schema bump when new searchable entity types appear. The current recommendation is to lock the present seven-entity set in v1.
- **Sync-state normalization boundary**: task-405 stores both typed timestamps and text fields internally. Task-601 should expose one normalized external timestamp representation so task-602 does not duplicate both.
- **Checksum format choice**: the internal table has a generic `content_hash` column but no algorithm contract yet. Task-601 needs to choose and document one required v1 digest representation so export/import validation stays deterministic and `task-602` does not have to invent it during implementation.
- **Config-directory introduction**: creating `agentic/config/` is small but new. Implementation should keep it narrowly snapshot-contract-focused so it does not imply a broader configuration system that the repo does not yet use.

## Required Docs / Tracking / Research Updates

- Update this canonical task-plan doc during implementation with final planning status, build status, implementation notes, verification notes, and outcome.
- Create `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md` with the accepted field contract, task-205/task-405 mapping rules, and any durable schema-versioning decisions.
- Update `.agent/plans/agentic/knowledge-base-platform-prd.md` only if the final field names or validation boundary materially refine the current high-level snapshot-format bullets.
- Update `.agent/workflows/agentic-kb.md` only if the repo needs a narrow note that the manifest contract now exists; keep the broader operator workflow pass deferred to `task-803`.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-601` status/completion metadata when implementation is done.

## Implementation Notes

- Added the canonical external manifest contract at `agentic/config/snapshot-manifest.schema.json` and a schema-aligned fixture at `agentic/config/snapshot-manifest.example.json`.
- Locked v1 artifact identity to the current task-205 dump reality: basename-only sibling `artifact.filename`, `dump_format = postgresql_custom`, explicit compression metadata `{ "algorithm": "gzip", "level": 6 }`, exact `size_bytes`, and required `content_hash`.
- Chose and documented the v1 digest representation as algorithm-prefixed lowercase hexadecimal `sha256:<64 hex chars>`, aligned with `agentic.kb_snapshot_manifest.content_hash` for later task-602 DB writes and validation.
- Added the narrow helper module `agentic/src/agentic_kb/snapshot_manifest.py` so constants and sync-state normalization rules are reusable without pre-implementing manifest-aware export/import behavior.
- Normalized exported sync state into public manifest fields only: docs/code expose `repo_commit_hash` and `last_synced_at`, GitHub exposes one `updated_at_watermark` per current stream, and Project 5 exposes `owner`, `number`, `cursor`, and `updated_at_watermark`, with no exported `last_error`.
- Locked the current seven entity-count keys in schema and example fixture: `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/snapshot_manifest.py" "agentic/tests/test_snapshot_manifest_schema.py"` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_manifest_schema.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'` passed.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'` passed.

## Outcome

- `task-601` implementation is in place for schema, example fixture, narrow helper constants/normalization, and focused automated coverage.
- Manifest-aware snapshot export/import behavior, checksum generation, DB writes, and runtime manifest-vs-dump enforcement remain intentionally deferred to `task-602`.
- The schema contract, example fixture, helper constants, focused tests, workflow note, plan update, and research capture are complete and implementation review is now clean.

## Review Outcome

- Planning review is complete and approved in `.agent/plans/agentic/task-plans/task-601-plan-review.md`.
- Implementation review is complete and approved in `.agent/plans/agentic/task-plans/task-601-impl-review.md`.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-601-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the implementation, focused verification, review loop, research update, workflow note, and tracker sync are complete.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-601-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-601-impl-review.md`
