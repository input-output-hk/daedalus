# Task 604 Research: import-then-sync-changed bootstrap flow

## Findings

- The operational bootstrap baseline after `snapshot import` should come from restored `agentic.kb_sync_state` rows, not by replaying `agentic.kb_snapshot_manifest` contents.
- `agentic.kb_snapshot_manifest` remains useful provenance proving that the KB came from a validated snapshot import, but task-604 uses live sync-state rows as the source of truth.
- Local docs and code need independent baseline commits because snapshot export/import already allows docs and code baselines to diverge.
- Safe local delta detection is `git diff <baseline_commit>..HEAD` filtered through the existing docs allowlist and code-path support rules.
- Git rename entries need delete-plus-add handling instead of `--name-only` output alone so old docs/code paths are purged when the new renamed path is ingested.
- Docs needed a small path-level delete contract so removed allowlisted files do not leave stale `kb_documents` rows after bootstrap delta sync.
- Existing code ingestion already had a per-path replace contract, so deleted files can be handled by replacing the path with zero chunks instead of widening into full-repository prune mode.

## Remote Contract

- Task-604 GitHub bounded behavior is intentionally limited to `issues` and `issue_comments`.
- For task-604 safety, `sync changed` should not claim bounded behavior for `pulls` or `review_comments`; explicit defer/skip output is preferred.
- When deriving one shared `updated_since` watermark for the supported GitHub streams, the safe lower bound is the earliest restored supported watermark, not the newest one, so older supported updates are not skipped.
- A supported GitHub stream baseline without a restored watermark is not trustworthy for task-604; `sync changed` should fail clearly instead of widening to `updated_since=None`.
- Task-604 Project behavior is cursor continuation only from stored `after_cursor`.
- Task-604 does not guarantee detection of updates to already-seen Project items because current ingestion remains append-style by cursor.

## Verification Notes

- Focused unit coverage can validate baseline loading, local delta filtering, deletion handling, bounded GitHub stream selection, deferred unsupported GitHub streams, and Project cursor continuation without requiring live network access.
- DB-backed verification is feasible for the full bootstrap command by seeding imported-style sync-state baselines and indexed rows into an isolated KB database, then running `sync_changed` end-to-end against a temporary git repo while patching remote ingestors to avoid live network calls.
- In this environment, runtime execution of that DB-backed suite remained gated by missing `AGENTIC_TEST_DATABASE_URL`, `psycopg`, and optional parser dependencies.
