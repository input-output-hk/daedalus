# Task 901 Clean-Machine Bootstrap Research

- Date: 2026-03-29
- Task: `task-901`
- Evidence: `docker-compose.agentic.yml`, `agentic/Dockerfile`, `agentic/README.md`, `.agent/workflows/agentic-kb.md`, `agentic/src/agentic_kb/commands/snapshot.py`, `agentic/src/agentic_kb/commands/status.py`, `agentic/src/agentic_kb/commands/search.py`, `agentic/src/agentic_kb/commands/sync.py`, `agentic/src/agentic_kb/snapshot_manifest.py`

## Durable Findings

- The narrow clean-machine bootstrap contract is valid after small runtime/doc fixes: fresh isolated Compose stack, `snapshot import`, `status --json`, and deterministic `search --mode bm25 --json` all succeed without relying on `sync changed`.
- The current container image needed `git` for the shipped in-container sync/bootstrap tooling to work at all. Without it, even `sync docs` fails before any import/export validation can seed a snapshot.
- Bind-mounted repo use inside the container also needs a local safe-directory override for git commands. The safe fix is per-invocation `git -c safe.directory=/workspace ...`, not global git config mutation.
- Snapshot export/import packaging had one clean-machine blocker: the manifest schema JSON was not available inside the installed package. Packaging the schema under `agentic_kb/config/` and resolving it with `importlib.resources` fixes the image/runtime contract.
- `agentic/.env.example` was genuinely missing from repo reality. The supported bootstrap path works with Compose defaults, so the correct disposition is to add the file as an optional override example rather than making an env file mandatory.
- The current docs previously overstated the broader team fast-start story. Task-901 validation supports only the narrower shipped path today; CI-published baseline snapshots remain pending behind `task-603`, and `sync changed` remains outside the clean-machine acceptance path even though it is still documented as a follow-on incremental command.
- A minimal MCP stdio smoke using the documented launcher remains feasible after bootstrap, but it is sensitive to using the same Compose env overrides as the isolated stack when running on a busy shared machine.

## Verification Notes

- Validation used two isolated Compose projects with fresh named volumes: `task901-seed` and `task901-target`.
- Because this workstation already had default host ports in use, the isolated validation used env overrides (`AGENTIC_DB_PORT`, `OLLAMA_PORT`) to avoid collisions. That was a local-machine accommodation, not a change to the documented clean-machine contract.
- Snapshot source for validation was a task-local isolated seeded environment, not a CI-published shared artifact, because `task-603` remains pending.
- The acceptance query used deterministic BM25 against a seeded document row: `task-901 clean machine bootstrap` with `--entity-type documents --mode bm25 --json`.

## No New Research Beyond Task Scope

- Task-901 did not validate the broader CI snapshot publication workflow, did not pull `sync changed` into the acceptance path, and did not resolve the separate `sync code` ingestion failure surfaced during exploratory seeding.

---

## Task-902 Security Review Findings (2026-03-31)

- All 7 PRD Security and Operational Boundaries constraints (lines 347-355) are HONORED in current repo reality.
- MCP server (`search_server.py`) is strictly read-only — 7 tools, no write-capable endpoints, stdio only.
- `GITHUB_TOKEN` sourced only from environment (`config.py:33`), never hardcoded; `agentic/.env.example` has empty placeholder.
- KB services isolated from wallet runtime — separate Compose file, zero references in `source/` to KB config.
- Snapshots excluded from git via `agentic/.gitignore:2` (`snapshots`); no `.dump` or `.manifest.json` committed.
- Snapshot import documented as targeting disposable databases only; no in-place schema upgrade procedures.
- One canonical embedding contract enforced in docs and tooling.

---

## Task-903 Pilot SOP Completion (2026-03-31)

- task-903 ("Pilot with multiple developers") completed as a documentation-only deliverable.
- Five SOP files created in `.agent/SOPs/agentic/`:
  - `pilot-two-developer.md` — End-to-end pilot workflow with pre-pilot checklist, Dev 1/Dev 2 steps, HUMAN CHECKPOINT callouts, conditional sync gating
  - `pilot-failure-modes.md` — 9 failure modes with symptoms and recovery procedures
  - `pilot-evidence-template.md` — Structured evidence capture with JSON artifacts and task-905 field mapping
  - `pilot-rollback.md` — Per-stage and full rollback procedures
  - `pilot-coordination.md` — Developer coordination, notification, checkpoint acks, escalation, debrief
- Planning review: 2 iterations in `task-903-plan-review.md` (approved)
- Implementation review: 1 iteration in `task-903-impl-review.md` (approved)
- No code, Compose, or helper script changes — documentation only
- All commands cross-referenced against `agentic/README.md` and `.agent/workflows/agentic-kb.md`
