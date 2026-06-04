# Task task-001: Validate Mithril partial restore CLI behavior against shipped artifacts

## Task ID and Title

- ID: `task-001`
- Title: `Validate Mithril partial restore CLI behavior against shipped artifacts`

## Why Chosen Now

`task-001` is the first critical gate in phase 0. Later partial-sync design and implementation work depends on evidence about the exact Mithril binary family Daedalus ships today, especially partial restore flags, ancillary behavior, target-directory safety, and whether Daedalus must still run the existing LSM conversion path after a partial restore.

## Interaction Mode

- Mode: `manual_execution`

This task is primarily a validation and evidence-capture spike against shipped or pinned Mithril artifacts. Repo inspection and some local command prep are agent-executable, but truthful completion may still require operator-run validation in an environment that can actually execute the pinned `mithril-client` family against network-accessible Mithril services and capture filesystem results. If that full execution is not possible in the current environment, the task should still produce the exact procedure, expected evidence, and repo-backed constraints needed for an operator to finish it.

## Scope

- Confirm which Mithril artifact family Daedalus currently bundles, including the pinned Mithril source revision and bundle locations.
- Validate, against that binary family, how `cardano-db download` behaves for partial restore flags: `--start`, `--end`, `--include-ancillary`, and `--allow-override`.
- Capture target-directory and produced-artifact evidence needed to decide whether staged install is mandatory.
- Confirm whether ancillary-enabled partial restore still leaves restored ledger state requiring the existing Daedalus LSM conversion path.
- Record durable evidence in plan-workspace research notes so later tasks do not rely on chat memory.

## Non-Goals

- Implementing diagnostics-launched partial sync.
- Adding new IPC, store, UI, or backend orchestration code for partial sync.
- Deciding the final install strategy beyond what the validation evidence directly proves.
- Broadening into supported-network manual QA across all later feature paths.

## Dependencies

- None in the task graph.
- Practical dependency: access to the pinned or shipped `mithril-client` binary family and enough environment support to run targeted CLI validation.

## Research Consulted

- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
- Workflows:
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- Skills:
  - None loaded for this planning pass. `understand` was not needed because direct repo reads were sufficient to verify the small set of Mithril code and build surfaces relevant to this task.

## Live Repo Findings Verified For Planning

- `source/main/mithril/MithrilBootstrapService.ts` currently uses full snapshot bootstrap, not partial restore, and always downloads with `--include-ancillary` before running local snapshot conversion and install.
- `source/main/mithril/MithrilBootstrapService.ts` resolves older/newer CLI layout differences for full download by detecting whether download is under `cardano-db download` or `cardano-db snapshot download`; the task should verify the shipped binary's partial-restore syntax directly instead of assuming docs match.
- `source/main/mithril/MithrilBootstrapService.ts` still performs a local `snapshot-converter` step after restore, so this task must explicitly test whether partial restore with ancillary data still requires that same conversion assumption.
- `source/main/mithril/mithrilCommandRunner.ts` runs bundled binaries via `DAEDALUS_INSTALL_DIRECTORY` when present and logs to `stateDir/Logs/mithril-bootstrap.log`, which gives a natural evidence path for CLI-output capture.
- `source/main/mithril/mithrilNetworkConfig.ts` hardcodes Mithril support only for `mainnet`, `preprod`, and `preview`, and fetches verification keys at runtime from Mithril infra URLs.
- `flake.nix` points the Mithril input at branch `sl/fix-mismatch-rust-versions`, but the effective pinned source for planning is the lockfile revision, not just the branch name.
- `flake.lock` currently pins Mithril to `input-output-hk/mithril` revision `2940975d3f9020d975b4bc62d75a646aa353c03a`.
- `nix/internal/common.nix` maps bundled Mithril packages per target OS and exposes both `mithril-client` and `snapshot-converter` to the Daedalus bridge bundle.
- `nix/internal/cardano-bridge.nix` copies `mithril-client*` and `snapshot-converter*` into the bundle and adds a Windows `mithril-client.exe` fallback name.
- `nix/internal/any-darwin.nix` bundles `mithril-client` explicitly for macOS app packaging.
- `nix/internal/x86_64-windows.nix` includes `mithril-client.exe` and `snapshot-converter.exe` in the Windows installer payload.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md` only if the new evidence materially corrects or supersedes current findings
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md` only if implementation realities or review outcomes materially change the approved plan or final outcome

## Implementation Approach

1. Lock the artifact target before validation.
    - Treat `flake.lock` Mithril revision `2940975d3f9020d975b4bc62d75a646aa353c03a` as the source-of-truth pinned family for this task unless the repo changes before execution.
    - Confirm where Daedalus packages the relevant binaries on Linux, macOS, and Windows from the existing Nix files.

2. Use the same invocation shape Daedalus uses in production.
    - Prefer running the bundled or Nix-built `mithril-client` binary rather than relying on external docs or a globally installed CLI.
    - Reuse Daedalus-relevant environment assumptions where applicable: aggregator endpoint resolution, runtime verification keys, `DAEDALUS_INSTALL_DIRECTORY`, and scratch work directories separate from any live chain state.
    - Capture the identity of the exact exercised CLI artifact before running the spike: resolved binary path, `mithril-client --version`, and the relevant help output that proves which partial-restore syntax the executed binary accepts.

3. Run the smallest validation matrix that answers the task's acceptance criteria.
    - Partial download without ancillary data.
    - Partial download with ancillary data.
    - Partial download with `--allow-override` into an already populated target.
    - Cancellation during active download.
    - If the current environment cannot reliably produce a cancellable long-running transfer, switch to an operator-run execution path and do not mark the task complete until cancellation evidence is captured.

4. Capture filesystem and process evidence, not just success or failure.
    - Save exact commands, resolved binary path, `--version` output, help output excerpts, stdout and stderr excerpts, exit codes, and whether the shipped CLI accepts the expected partial flags at the top-level command path or a nested subcommand path.
    - Record produced directory layout, especially whether `immutable`, `volatile`, `ledger`, `lsm`, `clean`, `db`, or ancillary-specific artifacts appear and whether they are replaced, merged, or refused when the destination is already populated.
    - Record whether post-restore output still points to conversion commands or produces ledger content that Daedalus would need to convert.
    - For cancellation, record whether the destination remains usable, requires cleanup, or is left in an indeterminate state.

5. Finish with a durable repo-local research note.
   - Write `02-validation-spike-results.md` with validated behavior, safe and unsafe directory assumptions, LSM-conversion conclusion, and any environment limitations.
   - If the environment cannot complete the full execution, include the exact operator procedure, missing prerequisite, and evidence checklist so task closure stays truthful.

## Acceptance Criteria

- A written validation note exists at `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- The note anchors the validation to the Mithril binary family Daedalus actually ships or pins, not generic latest docs alone.
- The note identifies the exact exercised CLI artifact with binary path plus version/help evidence.
- The note documents safe and unsafe target-directory assumptions for partial restore, including whether staging is required.
- The note states whether ancillary-enabled partial restore still requires Daedalus's LSM conversion path.
- The note includes cancellation evidence and states whether cancellation leaves the destination reusable, requires cleanup, or blocks normal restart assumptions.
- The note captures enough command, output, and filesystem evidence to unblock task-002 without relying on external chat context.
- If end-to-end validation cannot be completed in the current environment, the note explicitly says what was verified locally, what remains operator-run, and what evidence must be returned; the task remains in progress until the missing required evidence, including cancellation behavior, is captured.

## Verification Plan

- Repo verification already completed for planning:
  - `source/main/mithril/MithrilBootstrapService.ts`
  - `source/main/mithril/mithrilCommandRunner.ts`
  - `source/main/mithril/mithrilNetworkConfig.ts`
  - `flake.nix`
  - `flake.lock`
  - `nix/internal/common.nix`
  - `nix/internal/cardano-bridge.nix`
  - `nix/internal/any-darwin.nix`
  - `nix/internal/x86_64-windows.nix`
- Execution verification for the task itself should capture:
  - Resolved binary path plus `mithril-client --version` and help output proving the actual exercised partial-restore syntax.
  - Output and directory tree for partial restore without ancillary data.
  - Output and directory tree for partial restore with ancillary data.
  - Output and filesystem effect of `--allow-override` into a populated target.
  - Cancellation behavior and destination usability.
  - Any converter command hint, ledger layout evidence, or restart-relevant artifact produced after restore.

## Risks and Open Questions

- The pinned Mithril source is currently a branch-backed lockfile revision, so future rebases or lockfile updates could change task evidence if validation is delayed.
- The local environment may not have the packaged binary, Nix build availability, or external network access required for truthful end-to-end CLI validation.
- Partial restore syntax may differ from the currently shipped full-download compatibility logic in `MithrilBootstrapService`, especially across CLI subcommand layouts.
- Cancellation may be hard to validate deterministically without a large enough transfer window.
- Platform packaging is repo-verifiable, but cross-platform runtime behavior may still require operator validation on actual packaged artifacts.

## Required Docs, Research, and Tracking Updates

- Create `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- Update `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md` only if validated behavior differs from current assumptions.
- No PRD or task-graph change is planned in this task unless validation immediately exposes a blocking contradiction that must be recorded for task-002 handoff.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-001-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-001-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Repo-backed validation completed for the Daedalus-side assumptions that shape the spike.
- Pinned Mithril source revision confirmed from `flake.lock`: `2940975d3f9020d975b4bc62d75a646aa353c03a`.
- Executed provenance-confirmed CLI artifact: `/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client` reporting `mithril-client 0.13.9`.
- Live validation completed on `preview` against the latest snapshot using minimal partial ranges `26048..26049` and then `26049..26050` after the snapshot advanced during testing.
- Confirmed outcomes:
  - `cardano-db download` accepts `--start`, `--end`, `--include-ancillary`, and `--allow-override`
  - no-ancillary restore produces `db/clean`, `db/immutable`, and `db/protocolMagicId` only
  - ancillary restore additionally materializes `db/ledger` and emits explicit `snapshot_converter_cmd_to_lsm`, `snapshot_converter_cmd_to_lmdb`, and `snapshot_converter_cmd_to_legacy`
  - `--allow-override` succeeds against already populated scratch download targets
  - interrupted ancillary download leaves partial artifacts behind, including a partially materialized `db/` plus a separate ancillary temp directory, so cancellation is not restart-safe without cleanup
  - latest-snapshot drift can invalidate a previously chosen end range if the artifact advances mid-run, so later design work must bind range derivation to current snapshot metadata carefully
- The spike evidence supports staged restore as the governing safety posture for later tasks.

## Final Outcome

- `task-001` completed with provenance-confirmed live evidence from the flake-pinned Mithril CLI artifact.
- Acceptance criteria satisfied:
  - durable validation note created under the plan workspace
  - note anchored to the Daedalus-pinned artifact family
  - safe and unsafe target-directory assumptions documented
  - ancillary conversion requirement confirmed
  - cancellation fallout documented with cleanup-required conclusion
  - enough detail captured to unblock restore-strategy design in `task-002`
- Locked design inputs carried forward:
  - staged restore remains the default safety posture
  - ancillary-enabled partial restore still requires explicit conversion before Daedalus handoff
  - interrupted partial restore is not restart-safe without cleanup
  - latest-snapshot drift can invalidate a previously chosen range and must be handled explicitly by later design work

## Self-Review

- Kept the plan scoped to the validation spike only; no partial-sync feature implementation work was pulled in.
- Classified the task as `manual_execution` because final truth depends on artifact execution and evidence capture that may not be fully agent-executable here.
- Replaced stale assumptions about Mithril pinning with the current lockfile revision and current bundle paths.
