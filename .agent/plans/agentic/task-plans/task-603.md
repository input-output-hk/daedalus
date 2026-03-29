# Task Plan: task-603 Add shared snapshot publication workflow

- Task ID: `task-603`
- Title: `Add shared snapshot publication workflow`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-603` was chosen as the next high-priority unblocked task after `task-602`: manifest-aware `snapshot export` and `snapshot import` existed, but the repo still lacked a shared publication path for distributing a trusted baseline snapshot to other developers.
- This was the remaining Phase 6 gap between local snapshot mechanics and the documented multi-developer workflow. `agentic/README.md` and `.agent/workflows/agentic-kb.md` previously stated that CI-published snapshot distribution was pending.
- The workflow file `.github/workflows/agentic-kb-sync.yml` did not exist when planning began, so publication automation was still missing even though adjacent shipped contracts already existed in `task-604`, `task-701`, `task-801`, `task-802`, and `task-803`.
- Landing a narrow publication workflow now unlocks `task-703`, which is specifically about adding scheduled refresh automation on top of this same baseline workflow rather than inventing a second publication path later.

## Scope

- Add the first repo-supported GitHub Actions workflow for producing the canonical team baseline KB snapshot from `develop` as a GitHub Actions artifact.
- Keep the workflow narrow and publication-focused: bootstrap the agentic stack in CI, build a fresh baseline KB, export the manifest-backed snapshot pair, and upload it as an artifact.
- Document how developers discover, download, import, and validate the published artifact using the already-shipped `snapshot import`, `status --json`, and deterministic BM25 search flow.
- Define the artifact contract for this workflow: what files are uploaded, how they are named, which ref is allowed to publish the canonical shared baseline, and what verification must pass before upload.
- Align the workflow and docs with current shipped contracts from `task-602`, `task-604`, `task-701`, `task-801`, `task-802`, and `task-803`.

## Non-Goals

- Do not add a `schedule:` trigger or any recurring refresh cadence; that remains `task-703`.
- Do not redesign or extend snapshot manifest/export/import behavior; that remains the shipped `task-602` contract.
- Do not change the clean-machine acceptance boundary from `task-901`: import, `status --json`, and deterministic BM25 search stay the validation path, with `sync changed` remaining optional follow-on work for already-seeded KBs.
- Do not broaden this task into stale-index detection, freshness warnings, or status UX work; that remains `task-702`.
- Do not publish KB snapshots as GitHub Releases assets; GitHub Actions artifacts remain the only supported publication channel.
- Do not add MCP changes, agent setup changes, or new CLI subcommands unless a small documentation-facing adjustment is required to describe artifact consumption.

## Relevant Dependencies

- Declared dependency already satisfied:
  - `task-602` - manifest-aware snapshot export/import now produces and consumes the portable `.dump` plus `.manifest.json` pair this workflow will publish.
- Current shipped contracts this task must build on:
  - `task-604` - `sync changed` is a post-import incremental command for an already-seeded KB, not part of the narrow clean bootstrap success path.
  - `task-701` - the packaged `sync docs`, `sync code`, `sync github`, `sync project`, and `sync all` commands are now real and provide the workflow's seeding/build path before export.
  - `task-801` and `task-802` - MCP and setup docs are already shipped, so task-603 should update only the snapshot publication/consumption sections without reopening MCP scope.
  - `task-803` - `.agent/workflows/agentic-kb.md` already documents the shipped local/operator workflow and currently calls CI publication out as pending; task-603 should replace only that gap.
- Downstream boundary to preserve:
  - `task-703` still owns scheduled refresh automation and any cadence-based snapshot publishing expansion in the same workflow file.
- Planning nuance to keep explicit:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` points both `task-603` and `task-703` at `.github/workflows/agentic-kb-sync.yml`. That overlap is intentional only if `task-603` establishes the manual/on-demand baseline workflow and `task-703` later extends that same file with scheduled automation rather than creating a parallel workflow.
- Primary references reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md`
  - `.agent/plans/agentic/research/task-601-snapshot-manifest-format.md`
  - `.agent/plans/agentic/research/task-602-snapshot-export-import-commands.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `.agent/plans/agentic/research/task-604-import-then-sync-changed-bootstrap-flow.md`
  - `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`
  - `agentic/README.md`
  - `.github/workflows/verify_pr.yml`

## Files Expected To Change

- `.github/workflows/agentic-kb-sync.yml` - new workflow that builds and publishes the baseline snapshot artifact.
- `.agent/workflows/agentic-kb.md` - replace the current "publication is pending" guidance with the real artifact publication and consumption workflow while keeping clean bootstrap and `sync changed` boundaries intact.
- `agentic/README.md` - document where the published artifact comes from and how to consume it with the existing import-and-validate commands.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update only `task-603` status metadata when implementation lands.
- `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md` - capture durable findings about the accepted workflow trigger shape, artifact contract, token/permissions requirements, and verification behavior.
- `.agent/plans/agentic/knowledge-base-platform.md` - only if implementation reveals a meaningful contract clarification about publication versus scheduled refresh that the higher-level plan currently states ambiguously.

## Implementation Approach

- **Create one baseline workflow file**: add `.github/workflows/agentic-kb-sync.yml` as the single future home for both manual publication and later scheduled refresh work. In `task-603`, keep its trigger shape narrow to explicit/manual publication so `task-703` can later extend it instead of duplicating it.
- **Canonical baseline ref**: lock the shared team baseline publication contract to the platform's intended canonical branch, `develop`. `task-603` should treat only `refs/heads/develop` as eligible to publish the canonical shared artifact. If the workflow is manually dispatched from any other ref, it must fail clearly or otherwise refuse to mark that run's snapshot as the canonical team baseline.
- **Prefer explicit manual dispatch first**: start with `workflow_dispatch` rather than `schedule:`. Any optional non-scheduled trigger added in this task must still resolve only to canonical baseline publication from `develop`; this task should not normalize arbitrary branch snapshots as the shared team artifact.
- **Build from current packaged surfaces**: the workflow should use the existing Compose stack and shipped CLI commands rather than custom one-off export scripts. The expected path is `docker compose -f docker-compose.agentic.yml up -d`, then packaged sync commands, then `snapshot export`.
- **Seed before export**: use the shipped sync surface to build a fresh KB baseline in CI before export. Prefer `sync all` unless implementation finds a concrete reason to invoke the source-specific commands separately for clearer failure reporting.
- **Artifact shape**: upload the snapshot as one GitHub Actions artifact containing the exact portable pair produced by `task-602`: `<snapshot>.dump` plus `<snapshot>.manifest.json`. Do not transform the files into a second custom archive format.
- **Artifact naming**: use a deterministic artifact name that identifies it as the `develop` baseline and includes enough provenance for operators to identify the source revision, such as `agentic-kb-develop-baseline-<short-sha>` or equivalent plus run metadata. The uploaded files themselves should retain the CLI-generated snapshot filenames.
- **Pre-upload verification**: require the workflow to prove the exported snapshot is usable before upload. At minimum, that means export succeeds and the workflow can run the already-supported validation path against the built KB: `status --json` plus one repo-controlled deterministic BM25 proof grounded in content that a fresh repo-backed `sync all` baseline is guaranteed to ingest. Because task-301's docs allowlist explicitly includes `.agent/workflows/**/*.md` but does not include `agentic/README.md`, use the allowlisted workflow doc as the proof target: run `search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"` and assert the JSON payload reports `mode = "bm25"`, includes at least one hit, and the first hit has `entity_type = "documents"` with `fields.source_path = ".agent/workflows/agentic-kb.md"`. If implementation can cheaply add a disposable import smoke check inside CI without widening scope too much, that is desirable but not required for `task-603`.
- **Consumption contract**: document the operator path as "download the canonical `develop` baseline Actions artifact, extract the `.dump` plus `.manifest.json` pair into `agentic/snapshots/`, run `snapshot import <dump-or-manifest> --yes`, then validate with `status --json` and the same deterministic documents-only BM25 proof against `.agent/workflows/agentic-kb.md`." Keep `sync changed` explicitly documented as optional follow-on work, not bootstrap success criteria.
- **Permissions and secrets boundary**: keep the workflow aligned with the current GitHub/project ingestion contracts. If `sync github` and `sync project` require broader token scopes than the default GitHub Actions token can guarantee, the workflow should use an explicitly documented secret or fail with clear setup guidance rather than silently skipping those sources.
- **No Releases fallback**: keep the artifact publication channel limited to GitHub Actions artifacts, matching the higher-level plan and current workflow documentation.
- **Task boundary with task-703**:
  - `task-603` should deliver on-demand baseline publication and documented consumption.
  - `task-603` may add the workflow skeleton, artifact naming, retention, permissions, and pre-upload verification needed for reliable manual publication.
  - `task-603` should not add scheduled cadence, automatic recurring refresh, or policy around when to republish.
  - `task-703` should later extend the same workflow file with `schedule:` or other cadence-based automation once the baseline publication contract is proven.

## Acceptance Criteria

- `.github/workflows/agentic-kb-sync.yml` exists and provides a repo-supported GitHub Actions path for publishing a baseline KB snapshot as a GitHub Actions artifact.
- The workflow trigger shape is explicit/manual for `task-603` and does not add scheduled automation.
- The workflow publishes the canonical shared baseline only from `develop`, and it rejects or clearly refuses canonical publication from any other branch or ref.
- The workflow uses the current packaged agentic KB surfaces to bootstrap the stack, build the KB baseline, and export the snapshot rather than relying on ad hoc publication scripts.
- Successful runs upload the exact `task-602` portable snapshot pair as the artifact payload: one `.dump` file plus its sibling `.manifest.json` file.
- The artifact upload happens only after the workflow's required publication verification succeeds.
- The publication verification includes `status --json` and a deterministic documents-only BM25 proof against the built KB so the artifact is not uploaded from an obviously broken baseline.
- The deterministic search proof uses `search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"` and asserts an expected JSON hit shape grounded in current repo reality and the task-301 docs allowlist: at least one hit, first hit `entity_type = "documents"`, and first-hit `fields.source_path = ".agent/workflows/agentic-kb.md"`.
- The documented consumption path uses the existing `snapshot import <dump-or-manifest> --yes` contract and the existing clean-bootstrap validation commands rather than inventing a second import path.
- `.agent/workflows/agentic-kb.md` and `agentic/README.md` no longer describe CI snapshot publication as pending, and they keep `sync changed` documented as optional follow-on incremental work after import.
- The task does not add `schedule:` or otherwise absorb `task-703` scheduled refresh work.
- The task does not publish KB snapshots through GitHub Releases assets.

## Verification Plan

- Validate the workflow YAML syntax and structure in the repo before merge.
- Run the workflow through at least one intentional dry run or real manual dispatch path once implementation is available.
- Verify that manual publication from `develop` succeeds and that manual dispatch from any non-`develop` ref is rejected or otherwise does not publish the canonical shared baseline artifact.
- Verify the workflow can:
  - start the agentic stack on a GitHub Actions runner,
  - build a fresh KB baseline using the shipped sync commands,
  - export a valid snapshot pair,
  - run `status --json`,
  - run `search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"`,
  - assert the resulting JSON payload has `mode = "bm25"`, at least one hit, first-hit `entity_type = "documents"`, and first-hit `fields.source_path = ".agent/workflows/agentic-kb.md"`,
  - upload the pair as a GitHub Actions artifact only after those checks succeed.
- Verify the documented post-download consumption path end to end:
  - place the downloaded `.dump` plus `.manifest.json` pair in `agentic/snapshots/`,
  - run `docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import <dump-or-manifest> --yes`,
  - run `docker compose -f docker-compose.agentic.yml run --rm kb-tools status --json`,
  - run `docker compose -f docker-compose.agentic.yml run --rm kb-tools search --entity-type documents --mode bm25 --json "GitHub Releases assets are out of scope for KB snapshot sharing"`,
  - confirm the imported JSON result shape matches the same expected first-hit assertions used in CI publication verification.
- Confirm the docs match the actual workflow trigger name, artifact naming, and consumption steps after implementation.
- If the workflow requires a dedicated token or secret for `sync github` or `sync project`, verify the docs call out that prerequisite explicitly.

## Risks / Open Questions

- **Runner feasibility**: the current stack includes ParadeDB, Ollama, model initialization, and full KB sync work. A GitHub-hosted runner may prove slow or fragile for full baseline publication, especially if the embedding model pull becomes the critical path.
- **Artifact size and retention**: Actions artifact limits and retention policies may constrain how long baseline snapshots remain available or how large a seeded KB dump can grow before publication becomes unreliable.
- **GitHub token scope**: the default `GITHUB_TOKEN` may not be sufficient for `sync project` against DripDropz Project 5. The workflow may need a repo/org secret with explicit Project read access, and the plan should keep that requirement explicit rather than assuming default-token success.
- **Freshness semantics**: a manually published artifact is only as current as the workflow run. The docs must avoid implying that the artifact is continuously fresh before `task-703` adds scheduling.
- **Canonical baseline branch discipline**: because the plan now locks canonical publication to `develop`, implementation must be careful not to let a convenience manual dispatch path accidentally bless snapshots from feature branches as the shared team baseline.
- **Verification depth tradeoff**: export-side proof with `status --json` plus deterministic BM25 search is the minimum reliable check. A full disposable import smoke test would increase confidence but may also increase runtime enough to threaten workflow reliability.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan with final implementation notes, verification notes, outcome, and final statuses when `task-603` lands.
- Append planning review decisions in `.agent/plans/agentic/task-plans/task-603-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-603-impl-review.md` during this planning step; that starts only when implementation begins.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-603` metadata when implementation lands.
- Add `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md` with durable findings about workflow triggers, artifact contract, permissions, retention, and verification behavior.
- Update `.agent/workflows/agentic-kb.md` and `agentic/README.md` so the published snapshot consumption path matches the implemented workflow.
- Update `.agent/plans/agentic/knowledge-base-platform.md` only if implementation shows that the higher-level plan needs a clearer distinction between baseline publication in `task-603` and scheduled automation in `task-703`.

## Final Implementation Notes

- `.github/workflows/agentic-kb-sync.yml` now exists as the single manual/on-demand baseline publication workflow for task-603.
- The workflow now starts `paradedb` and `ollama` first, then waits for `ollama-init` to complete successfully with `docker compose -f "${AGENTIC_COMPOSE_FILE}" up --exit-code-from ollama-init ollama-init` before running `sync all`, closing the fresh-runner model-pull race identified in code review.
- `agentic/README.md` and `.agent/workflows/agentic-kb.md` now document publication, download, import, and deterministic validation of the canonical shared baseline artifact and keep `sync changed` as optional follow-on work.
- `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md` captures the durable workflow contract, including branch restriction, artifact shape, explicit secret requirement, and deterministic BM25 proof.

## Final Verification Notes

- Repo-local static workflow checks passed for trigger shape, `develop` branch guard, explicit `AGENTIC_KB_SYNC_GITHUB_TOKEN` requirement, Ollama-init sequencing, deterministic artifact naming, and absence of any `schedule:` trigger.
- Repo-local YAML parsing verification passed after accounting for the YAML 1.1 parser quirk that can materialize the top-level `on` key as boolean `True` in some tooling.
- Repo-local documentation and research contract checks passed against `agentic/README.md`, `.agent/workflows/agentic-kb.md`, and `.agent/plans/agentic/research/task-603-shared-snapshot-publication-workflow.md`.
- A real GitHub-hosted `workflow_dispatch` run, artifact upload/download validation, and live GitHub/Project token-scope verification remain follow-up runtime checks outside this local implementation loop.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-603-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-603-impl-review.md`

## Final Outcome

- `task-603` is complete. The planning loop reached approval, the implementation loop reached approval after one workflow sequencing fix, and the task now ships a manual GitHub Actions artifact publication path for the canonical `develop` KB baseline.

## Planning Status Rationale

- Planning status is `approved` because the planning review log reached an approved decision.
- Build status is `completed` because the implementation review log reached `Decision: approved` for iteration 2 and the final task docs, research, workflow docs, and tracking updates are now aligned with the shipped task-603 contract.
