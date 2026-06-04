Planner: Iteration 1
Timestamp: 2026-05-18T19:29:49Z

Plan Summary:
- Created the canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.
- Kept scope limited to validating shipped Mithril partial-restore CLI behavior and capturing durable evidence for later design work.
- Classified the task as `manual_execution` because truthful completion may require operator-run validation against the pinned/shipped Mithril binaries and live network-accessible services.

Docs, Workflows, Research, and Skills Consulted:
- Read `.agent/readme.md` and `.agent/system/architecture.md`.
- Read `.agent/workflows/test.md` and `.agent/workflows/update-doc.md`.
- Read task-specific sources:
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
  - `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
  - `.agent/plans/mithril/bootstrap-cardano-node.md`
  - `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
  - `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
  - `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- Did not load `understand`; direct repo reads were sufficient for this planning pass and the plan records that explicitly.

Repo-Verified Findings Used To Shape The Plan:
- Verified current bootstrap behavior in `source/main/mithril/MithrilBootstrapService.ts`: Daedalus still performs full snapshot download with `--include-ancillary`, then runs local `snapshot-converter`, then installs into managed chain storage.
- Verified command execution assumptions in `source/main/mithril/mithrilCommandRunner.ts`: Daedalus runs bundled binaries via `DAEDALUS_INSTALL_DIRECTORY` when present and logs to `stateDir/Logs/mithril-bootstrap.log`.
- Verified network config source in `source/main/mithril/mithrilNetworkConfig.ts`: only `mainnet`, `preprod`, and `preview` are supported and keys are fetched at runtime.
- Verified packaging/pinning in build files:
  - `flake.nix` uses a Mithril branch input
  - `flake.lock` currently pins Mithril to revision `2940975d3f9020d975b4bc62d75a646aa353c03a`
  - `nix/internal/common.nix` exposes both `mithril-client` and `snapshot-converter`
  - `nix/internal/cardano-bridge.nix`, `nix/internal/any-darwin.nix`, and `nix/internal/x86_64-windows.nix` bundle the Mithril binaries in shipped artifacts

Planned Approach:
- Validate the pinned/shipped Mithril binary family, not generic latest docs.
- Run the smallest validation matrix needed by acceptance:
  - partial restore without ancillary data
  - partial restore with ancillary data
  - partial restore with `--allow-override` into a populated target
  - cancellation during active download if feasible
- Capture commands, stdout/stderr, exit codes, accepted CLI syntax, and produced directory layout.
- Record whether ancillary-enabled partial restore still requires the Daedalus LSM conversion path.
- Write durable results to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- If this environment cannot complete full execution, preserve a truthful operator handoff and evidence checklist instead of pretending the spike is autonomous.

Scope Guard / Self-Review:
- No partial sync implementation work was added.
- No IPC, renderer, or orchestration changes were pulled into this task.
- The plan avoids stale wording by anchoring validation to the current lockfile revision and current bundle paths rather than older Mithril notes.

Outcome: Canonical task plan drafted and ready for critique

Planner: Iteration 2
Timestamp: 2026-05-18T19:32:12Z

Plan Updates In Response To Critique:
- Made cancellation evidence mandatory for task closure instead of optional.
- Added an explicit fallback rule: if this environment cannot reliably exercise cancellation, the task shifts to operator-run execution and stays in progress until that evidence is captured.
- Added a required artifact-identity capture step so the validation note must record the exact exercised CLI binary path plus `mithril-client --version` and relevant help output.
- Tightened the expected file-change list so the canonical plan doc is only updated again if implementation reality or review outcomes materially change the approved plan or final outcome.

Resulting Approved Plan Shape:
- Validate the exact pinned and actually executed Mithril CLI artifact, not just the repo pin.
- Capture proof of accepted partial-restore syntax, output, exit codes, directory layout, ancillary behavior, allow-override behavior, cancellation behavior, and post-restore conversion implications.
- Write durable evidence to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- Keep the task as a validation spike only, with no partial sync feature implementation work added.

Scope Guard / Self-Review:
- The revision only closes the critique gaps and does not widen scope.
- The plan now matches the task graph requirement that cancellation behavior be part of the spike evidence.
- The plan now distinguishes repo pinning from the identity of the binary actually exercised during validation.

Outcome: Canonical task plan revised after critique and approved for build execution

