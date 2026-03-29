# Task Plan: task-901 Validate clean-machine bootstrap

Historical note: this task plan was written before the GitHub Actions-based snapshot publication path was retired. References here to `task-603` as a pending CI publication workflow are now outdated; v1 now assumes local GPU-backed publication to private shared storage, and `task-901` has been reopened in the tracker to validate that final workflow instead.

- Task ID: `task-901`
- Title: `Validate clean-machine bootstrap`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-901` is the next unblocked critical-path task after `task-602`, `task-801`, `task-802`, and `task-803` shipped the snapshot import contract, read-only MCP server, client setup docs, and current-state workflow documentation.
- The repo now has the operator-facing pieces needed for a fast-start bootstrap, but there is not yet a task-owned validation pass proving that a fresh developer environment can actually boot the stack, import a snapshot, and search successfully.
- Downstream rollout tasks `task-902` and `task-903` should not proceed until this bootstrap path is validated and any blocking clean-machine gaps are either fixed or documented explicitly.

## Scope

- Validate the documented clean-machine bootstrap path against current repo reality using a fresh isolated Compose project and fresh named volumes.
- Prove the minimum supported fast-start workflow end to end: build or pull the required containers, start the KB stack, import a valid snapshot, confirm KB readiness, and run a successful search against imported data.
- Confirm the current operator docs describe that validated path accurately, and make only the smallest doc or runtime corrections needed for the validated flow.
- Resolve the current clean-bootstrap documentation contradiction explicitly: final task output must say whether the currently documented team snapshot-publication and import-then-`sync changed` story remains deferred behind `task-603` and the known `sync changed` container limitation, and the operator docs must be updated if they currently overstate that broader story as shipped clean-machine guidance.
- Require a concrete disposition for the missing `agentic/.env.example` mismatch as part of clean-machine scope: either prove no env file is needed for the supported bootstrap path and document that clearly, or add the missing env example / tracking correction during implementation.
- Record the final validated bootstrap contract, current caveats, and verification outcome in the task-owned documentation and tracking artifacts.

## Non-Goals

- Do not implement snapshot publication automation or GitHub Actions artifact distribution; that remains `task-603`.
- Do not broaden into stale-index detection, scheduled refresh automation, or multi-developer pilot work; those remain `task-702`, `task-703`, and `task-903`.
- Do not treat `sync changed` as part of the clean-machine success bar for this task; it remains a follow-on incremental command and is not required to prove clone -> boot -> import -> search.
- Do not do the broader agent-index cleanup from `task-804` unless validation exposes a narrow contradiction that blocks the bootstrap flow.
- Do not rewrite the workflow around full-rebuild ingestion from scratch unless the fast-start import path proves unusable.

## Relevant Dependencies

- Completed implementation dependencies already in repo reality:
  - `task-602` - manifest-aware `snapshot export` and `snapshot import`.
  - `task-701` - current sync command family and incremental baseline contracts.
  - `task-801` - packaged read-only stdio MCP search server.
  - `task-802` - finalized OpenCode, Claude Code, and `.mcp.json` setup docs in `agentic/README.md`.
  - `task-803` - current-state workflow doc in `.agent/workflows/agentic-kb.md`.
- Current repo/doc/runtime sources reviewed for planning:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`
  - `.agent/plans/agentic/research/task-802-client-setup-docs.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `agentic/README.md`
  - `docker-compose.agentic.yml`
  - `agentic/Dockerfile`
  - `agentic/src/agentic_kb/cli.py`

## Current Repo State To Reconcile

- `docker-compose.agentic.yml` defines the full local stack: `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`.
- `agentic-kb` exposes the required CLI surface for this task: `status`, `search`, `snapshot`, `sync`, and `mcp-search`.
- `agentic/README.md` and `.agent/workflows/agentic-kb.md` now distinguish the validated clean-bootstrap contract (`up -> snapshot import -> status --json -> bm25 search --json`) from the broader team-sharing workflow.
- The shipped `kb-tools` image now installs `git`, and sync commands use a per-command safe-directory override for the bind-mounted `/workspace` checkout.
- `agentic/.env.example` now exists as an optional override example; the supported clean-bootstrap path still works with Compose defaults and does not require a local env file.
- `task-603` is still pending, so there is still no shipped CI publication workflow for a shared baseline snapshot artifact.
- The final validation source was a task-local snapshot exported from a separate isolated seeded environment rather than a shared published artifact.

## Files Expected To Change

- `agentic/README.md` - if the validated clean-machine/bootstrap sequence or caveats need a narrow operator-doc update.
- `.agent/workflows/agentic-kb.md` - if the workflow needs a narrow correction so the validated fast-start path is explicit and accurate.
- `docker-compose.agentic.yml` - only if validation finds a small Compose-level blocker to the supported clone -> up -> import -> search path.
- `agentic/Dockerfile` - only if validation finds a small image-level blocker to the supported clone -> up -> import -> search path.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - only to update `task-901` completion metadata when implementation finishes.
- `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md` - if implementation captures durable validation findings that should live outside the canonical task plan.
- `.agent/plans/agentic/task-plans/task-901.md` - canonical task plan and final task record.
- `.agent/plans/agentic/task-plans/task-901-plan-review.md` - append-only planning review log.

## Implementation Approach

- Use an isolated validation setup that does not depend on any pre-existing local DB or Ollama volumes. Prefer a dedicated Compose project name and fresh named volumes so the results represent a clean machine rather than a reused developer environment.
- Validate the fast-start path, not the full rebuild path. The primary success route should be: fresh checkout prerequisites, `docker compose -f docker-compose.agentic.yml up -d`, wait for service readiness, import a valid snapshot pair, confirm `agentic-kb status` reports usable runtime/DB state, and run `agentic-kb search` successfully against imported content.
- Make the success route deterministic instead of best-effort:
  - readiness should be proven with `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json` after the stack is up, not with a generic human-readable status glance
  - the required post-import search proof should use `--mode bm25 --json`, not the default `hybrid`, so first-boot Ollama/model timing does not decide task success
  - any optional MCP sanity check should remain separate from the core acceptance path
- Treat the snapshot input explicitly. Because `task-603` has not shipped a shared publication workflow yet, implementation should either:
  - generate a valid snapshot from a separate seeded isolated environment as test input for the clean-machine target environment, or
  - use another already-available validated snapshot artifact if one exists during implementation.
  The final task notes must state which input source was used.
- Keep validation grounded in the documented user-facing commands from `agentic/README.md` and `.agent/workflows/agentic-kb.md`. If the real successful path differs, fix the docs or the narrow blocker rather than leaving the divergence implicit.
- Require a final doc disposition for the current snapshot-sharing contradiction:
  - if validation only proves `clone -> up -> import -> status --json -> search --mode bm25 --json`, then docs must clearly label any CI-published baseline snapshot workflow and import-then-`sync changed` flow as pending, conditional, or outside the currently validated clean-machine contract
  - task-901 must not finish while the docs still imply that CI publication and routine in-container `sync changed` are already part of the shipped clean-machine story
- Keep `sync changed` out of the clean-machine acceptance path unless a small follow-up smoke is useful after the main bootstrap succeeds. Even with the in-container git/runtime fix, it remains a follow-on incremental command rather than part of the bootstrap success bar.
- Require a final disposition for `agentic/.env.example` during implementation. It is not enough to note the mismatch; the task must end with one of two explicit outcomes:
  - documented proof that the supported clean-machine bootstrap path works with Compose defaults and does not require `agentic/.env.example`, plus tracker/docs clarification if needed
  - or creation of the missing env example as the minimal bootstrap fix
- Include one MCP sanity check if feasible after import and search succeed, using the exact documented stdio launcher `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`, so task-901 confirms the clean-machine bootstrap did not leave the search surface usable only through the local CLI.
- Prefer the smallest corrective change if validation fails. For example:
  - doc-only drift -> update `agentic/README.md` and/or `.agent/workflows/agentic-kb.md`
  - missing prerequisite in image/compose path that blocks import/search -> fix that specific blocker only
  - broader artifact-distribution concern -> document as out-of-scope residual risk and leave to `task-603`

## Acceptance Criteria

- A fresh isolated local environment can boot the stack with `docker compose -f docker-compose.agentic.yml up -d` without relying on pre-existing KB volumes.
- A valid snapshot dump/manifest pair can be imported successfully into that fresh environment using the shipped `snapshot import` flow.
- After import, `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json` succeeds and is used as the authoritative clean-machine readiness proof for the imported environment.
- After import, `docker compose -f docker-compose.agentic.yml run --rm kb-tools search --mode bm25 --json <deterministic-query>` returns at least one expected hit from imported data, proving the KB is queryable on a clean-machine bootstrap path without depending on first-boot Ollama/vector readiness.
- The final task record states exactly what snapshot source was used for validation and whether it represents the current documented team fast-start story or a temporary validation-only stand-in.
- The final task record includes an explicit disposition for the current doc contradiction around snapshot publication and import-then-`sync changed`: either the docs are updated to distinguish the currently validated clean-machine flow from the still-pending broader team workflow, or implementation proves that the broader wording is already accurate as written.
- The final task record includes an explicit disposition for `agentic/.env.example`: either no env example is required for the validated path and the docs/tracker say that clearly, or the missing env example is added as part of the task.
- If the documented clean-machine path needed corrections, `agentic/README.md` and/or `.agent/workflows/agentic-kb.md` are updated so they match the validated operator flow.
- Known residual limitations that are not part of the bootstrap success bar remain explicit, especially the lack of shipped snapshot publication automation and the fact that `sync changed` remains a separate post-bootstrap incremental workflow.
- If feasible in the same clean environment, the documented stdio MCP launcher can initialize successfully after bootstrap; if not feasible, the final task notes explain why without overstating MCP validation.

## Verification Plan

- Use a fresh isolated Compose project name and fresh named volumes for authoritative validation, and remove or avoid reusing any prior task volumes when measuring the clean-machine path.
- Build and start the stack through the documented Compose path, then verify service readiness with `docker compose -f docker-compose.agentic.yml ps` plus `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json`. Use that JSON-capable `status` command as the authoritative readiness check for this task instead of a looser human-readable interpretation.
- Validate `snapshot import` using a real manifest/dump pair and keep the input source explicit in the task outcome.
- Run at least one real post-import search command through the packaged CLI using `--mode bm25 --json` and a deterministic query known to exist in the imported snapshot. Do not use default `hybrid` mode as the acceptance query because it can fail or delay on first-boot Ollama/model readiness.
- If feasible, run one lightweight MCP stdio smoke after import using the exact documented launcher contract; keep this to initialization or one simple tool call rather than a large new MCP test suite.
- Re-read any changed docs against `docker-compose.agentic.yml`, `agentic/Dockerfile`, `agentic/src/agentic_kb/cli.py`, `agentic/README.md`, and `.agent/workflows/agentic-kb.md` so the final documented bootstrap contract matches shipped behavior.
- Re-read the final docs specifically for the current contradiction between the shipped clean-machine path and the broader team-sharing story. If `task-603` is still pending, ensure the docs distinguish the validated clean-machine contract from that broader future/pending workflow instead of presenting both as equally shipped.
- Resolve the `agentic/.env.example` mismatch before task completion: either verify Compose defaults make it unnecessary and document that explicitly, or add the missing env example / tracker correction as the smallest clean-bootstrap fix.

## Verification Notes

- Validation used two isolated Compose projects with fresh named volumes: `task901-seed` and `task901-target`.
- Because the workstation already had default host ports in use, the isolated runs used `AGENTIC_DB_PORT` and `OLLAMA_PORT` overrides. This was a local verification accommodation, not a change to the documented clean-machine contract.
- The snapshot source used for acceptance was a task-local snapshot exported from the isolated `task901-seed` environment because `task-603` CI publication is still pending.
- Verification proved the target bootstrap path with `docker compose -p task901-target -f docker-compose.agentic.yml up -d`, `docker compose -p task901-target -f docker-compose.agentic.yml run --rm kb-tools snapshot import agentic/snapshots/task901-seed.dump --yes`, `docker compose -p task901-target -f docker-compose.agentic.yml run --rm kb-tools status --json`, and `docker compose -p task901-target -f docker-compose.agentic.yml run --rm kb-tools search --mode bm25 --entity-type documents --json "task-901 clean machine bootstrap"`.
- `status --json` succeeded with `ok: true`, and the deterministic BM25 query returned the expected imported `doc:task901-bootstrap` result.
- A minimal stdio MCP smoke using `docker compose -p task901-target -f docker-compose.agentic.yml run --rm -T mcp-search` also exited successfully.

## Risks / Open Questions

- **Snapshot source gap**: `task-603` is still pending, so a clean-machine import validation cannot assume a published shared artifact already exists. Implementation needs to define the validation snapshot source explicitly.
- **First-boot cost and timing**: `ollama-init` must pull the embedding model on first boot, which can make clean-machine readiness slower and more failure-prone than earlier warm-cache task runs.
- **Validation realism boundary**: a locally generated snapshot proves import/search mechanics, but it is not the same as validating the eventual team-distributed artifact workflow from `task-603`. Task-901 therefore needs explicit doc wording that distinguishes the currently validated bootstrap contract from the broader pending team-sharing story when those differ.
- **MCP breadth**: task-901 should avoid turning into a second implementation test suite for MCP. One clean-machine sanity check is useful, but deeper MCP behavior remains covered by task-801 verification.

## Required Docs / Tracking / Research Updates

- This canonical task plan is the single source of truth for the approved task-901 plan, current build state, and final outcome.
- Planning review history lives in `.agent/plans/agentic/task-plans/task-901-plan-review.md`.
- Implementation review history should later live in `.agent/plans/agentic/task-plans/task-901-impl-review.md`, but that file must not be created during this planning pass.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only when implementation actually changes `task-901` status metadata.
- If implementation produces durable validation findings beyond this task plan, add `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-901-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-901-impl-review.md`

## Final Outcome

- Outcome: completed and approved.
- The final approved implementation installed `git` in the `kb-tools` image, added per-command git `safe.directory` handling for bind-mounted repo operations, packaged the snapshot manifest schema into the installed Python package, and added the missing optional `agentic/.env.example`.
- The operator docs in `agentic/README.md` and `.agent/workflows/agentic-kb.md` now preserve the narrow validated clean-bootstrap contract and explicitly mark CI snapshot publication as still pending behind `task-603`.
- The documented clean-machine acceptance path is now the validated `up -> snapshot import -> status --json -> search --mode bm25 --json` flow; `sync changed` remains a follow-on incremental command and is not part of bootstrap acceptance.
- The validation snapshot source was a task-local isolated seeded environment rather than a shared published artifact.
- The `agentic/.env.example` contradiction is resolved: the file now exists, but it remains optional because the supported bootstrap path works with Compose defaults.
- Verification performed: fresh-image build, isolated source export, isolated target import, `status --json` readiness proof, deterministic post-import BM25 search proof, and one minimal stdio MCP initialization smoke.
- Residual risks remain unchanged and explicit: CI-published snapshot distribution is still pending in `task-603`, and exploratory seeding surfaced separate ingestion behavior outside task-901 scope.
- Review-log references are unchanged: planning approved in `.agent/plans/agentic/task-plans/task-901-plan-review.md`; implementation approved in `.agent/plans/agentic/task-plans/task-901-impl-review.md`.
