# Task 603 Shared Snapshot Publication Workflow Research

Historical note: this research captured the now-retired GitHub Actions publication path for `task-603`. That path has been removed, and local GPU-backed publication to private shared storage is the only supported v1 direction.

- Date: 2026-03-29
- Task: `task-603`
- Evidence: `.github/workflows/agentic-kb-sync.yml`, `docker-compose.agentic.yml`, `agentic/README.md`, `.agent/workflows/agentic-kb.md`, `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/commands/search.py`

## Durable Findings

- The canonical shared baseline publication path now lives in one workflow file, `.github/workflows/agentic-kb-sync.yml`, with `workflow_dispatch` as its only trigger for task-603. Scheduled refresh remains intentionally deferred to task-703 so later automation extends the same file instead of creating a second publication path.
- Canonical publication is guarded to `refs/heads/develop` only. Manual dispatch from any other ref fails early with a clear message instead of silently producing a non-canonical branch snapshot.
- The workflow uses the shipped Compose and CLI surfaces end to end: `docker compose -f docker-compose.agentic.yml build kb-tools`, `up -d paradedb ollama`, `up --exit-code-from ollama-init ollama-init` to wait for the embedding model pull, `run --rm kb-tools sync all`, `snapshot export`, `status --json`, and the deterministic `search --entity-type documents --mode bm25 --json ...` proof.
- The publication artifact contract is exactly the portable task-602 sibling pair: one `.dump` file plus one sibling `.manifest.json` file uploaded directly as an Actions artifact. The workflow does not wrap the pair in a repo-specific archive format and does not use GitHub Releases assets.
- The artifact name is deterministic and revision-specific as `agentic-kb-develop-baseline-<full-github-sha>`. The uploaded files keep the snapshot export filenames produced in `agentic/snapshots/`.
- Publication verification is locked to one repo-backed BM25 proof grounded in the task-301 docs allowlist and current workflow doc wording: `search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"` must report `mode == "bm25"`, at least one hit, first-hit `entity_type == "documents"`, and first-hit `fields.source_path == ".agent/workflows/agentic-kb.md"`.
- The workflow also requires `status --json` to parse successfully and report `ok: true` before artifact upload, so a snapshot is not published from an obviously degraded KB state.
- `sync github` and `sync project` already fail hard when `GITHUB_TOKEN` is absent, and Project 5 access cannot be assumed from the default GitHub Actions token. The workflow therefore requires an explicit `AGENTIC_KB_SYNC_GITHUB_TOKEN` secret and fails with setup guidance when it is missing instead of skipping GitHub-backed sources.
- The workflow sets artifact retention explicitly to 14 days. That keeps the baseline publication path durable enough for short-term sharing while leaving longer-horizon retention and refresh policy as separate operational decisions.

## Verification Notes

- Repo-local verification can confirm the workflow trigger shape, ref guard, secret requirement, artifact-name contract, and documentation alignment, but it cannot fully validate `workflow_dispatch` behavior or hosted-runner publication from this local implementation loop.
- The repo-local contract is strongest when paired with static YAML parsing plus doc/contract checks against the implemented files; actual manual dispatch on GitHub remains a follow-up runtime validation step.
- One repo-local parser gotcha was confirmed during verification: some YAML 1.1 tooling can materialize the top-level workflow `on` key as boolean `True`. The adjusted static verification now tolerates that quirk while still asserting the presence of `workflow_dispatch`.
