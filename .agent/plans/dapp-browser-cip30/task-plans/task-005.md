# Task task-005: Investigate portable Linux Chromium sandbox feasibility

## Task

- Task ID: `task-005`
- Title: `Investigate portable Linux Chromium sandbox feasibility`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`
- Tracker result: `cancelled`

## Why This Task Was Chosen

- Task-001 made active packaged Chromium OS sandboxing a release gate for hostile remote content.
- The historical Linux package was a self-extracting `.bin` installed under the invoking user's home directory and could not establish a root-owned mode-`4755` Chromium helper.
- Both development and packaged launch paths used process-wide sandbox-disabling switches, so a package-equivalent feasibility spike was required before guest implementation.

## Interaction Mode

- Mode: `manual_execution`.
- The original plan required an authoritative Linux matrix and disposable-host evidence.
- Agent-executable work produced the bounded probe and one package-equivalent Ubuntu 24.04 diagnostic run. No host-wide policy was changed.

## Historical Scope

- Test whether the portable home-directory package could launch a local sandboxed renderer without process-wide bypasses.
- Identify the exact renderer through `webContents.getOSProcessId()` and require same-PID `/proc` evidence.
- Keep raw paths, argv, environment, and stderr on the disposable host and export deterministic normalized evidence.
- Freeze fail-closed behavior and prohibit automatic unsandboxed retry.

## Outcome And Cancellation

- The package-equivalent Ubuntu 24.04 portable artifact failed closed during Chromium sandbox bootstrap with `SIGILL`; it produced no exact-renderer containment proof.
- A local-only explicit `--no-sandbox` diagnostic reached a renderer and was rejected by the probe as designed. It is not containment evidence or a permitted fallback.
- Probe timeout, cleanup, PID correlation, and privacy-redaction defects found during review were corrected.
- Product/release decision on 2026-08-12 rejected portable `.bin`, AppImage, Flatpak, Snap, and other portable Linux channels in favor of privileged system `.deb` and `.rpm` packages.
- The portable objective is therefore cancelled rather than relabeled successful. Research 05 remains negative evidence, and research 06 records the accepted replacement strategy.

## Successor Tasks

- `task-005-a`: freeze the fresh system-package contract, exact `/opt/daedalus/<cluster>` layout, authoritative support matrix, probe contract, and post-install behavior using a new planning transcript.
- `task-108`: implement the flag-free `.deb` package.
- `task-109`: implement the flag-free `.rpm` package.
- `task-005-b`: certify both installed artifacts across the authoritative matrix and restricted-sandbox cases.
- `task-103`: after certification, remove remaining development/legacy bypasses and add runtime argv/environment rejection plus the local canary.
- `task-110`: retire `.bin` shipping and migrate Linux update behavior after both system packages exist.

## Non-Goals

- Do not claim the portable artifact passed sandbox certification.
- Do not use the diagnostic unsandboxed control in production or ordinary verification.
- Do not implement or certify `.deb` or `.rpm` artifacts under this historical task.
- Do not enable production guest launch.

## Research Consulted And Produced

- `research/01-hostile-renderer-threat-model-traceability.md`
- `research/05-linux-chromium-sandbox-packaging.md`
- `research/06-linux-system-package-decision.md`
- No new research beyond the graph/ownership correction was produced in the current continuation.

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/plans/readme.md`, the dApp-browser PRD, and task graph.
- `.agent/workflows/electron.md`, `.agent/workflows/nix.md`, `.agent/workflows/build.md`, `.agent/workflows/test.md`, and `.agent/workflows/update-doc.md` during the historical probe and current graph reconciliation.
- `understand` was loaded; material conclusions were checked against live files and history.

## Files Changed

- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
- `.agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-001.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md`

## Verification

- Probe self-test, Node syntax check, focused formatting, and the historical package-equivalent diagnostics are recorded in the implementation review log and research 05.
- The successor graph must have unique IDs, resolvable dependencies, no cycles, matching task/hour metadata, and a dependency-valid longest critical path.
- Current documentation must distinguish cancelled portable evidence, system-package contract ownership, package implementation, installed-artifact certification, and runtime enforcement.
- `git diff --check` and focused formatting must pass before review.

## Acceptance And Final Evidence

- The portable experiment is reproducible and its negative result is not overstated.
- The product decision and rejected channels are durable.
- Cancellation does not weaken the packaged sandbox gate or permit unsandboxed remote content.
- Successor tasks have an acyclic contract -> package -> certification -> runtime-enforcement order.
- Historical review logs remain append-only and new task-specific plan files are deferred until Selector chooses those tasks.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md`

## Planning Status

- `approved`

The planning log preserves the approved historical portable feasibility plan. It is not repurposed for the new system-package contract; task-005-a receives a fresh planning sequence when selected.

## Build Status

- `completed`

## Current Outcome

- Historical portable feasibility work is cancelled by the accepted replacement product strategy, not marked as passing certification.
- The user-confirmed task graph correction separates task-005-a contract readiness from task-005-b installed-artifact certification and leaves production guest launch disabled.
- Implementation review iteration 5 approved the cancellation, successor ownership, graph accounting, exact package layout, and release-gate ordering with no blocking findings.
