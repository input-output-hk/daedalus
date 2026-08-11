Implementation: Iteration 1
Timestamp: 2026-08-11T18:22:32Z

Changes made:
- Added `scripts/linux-chromium-sandbox-probe/main.cjs`, a dependency-free local Electron probe with exact renderer PID correlation, allowlisted same-PID `/proc` evidence, sandbox-bypass rejection, helper/file identity, deterministic privacy redaction, stderr summarization, cleanup, timeout handling, and an embedded golden self-test.
- Added `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md` with the user-namespace strategy, package-equivalent proof-variant procedure, positive and restricted-userns host steps, evidence schema/privacy contract, task-103 canary contract, and later revalidation ownership.
- Created and maintained the canonical plan and append-only planning/implementation logs; marked planning approved and recorded the current manual checkpoint without claiming package certification.

Files touched:
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-plan-review.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md`

Verification run:
- `node scripts/linux-chromium-sandbox-probe/main.cjs --self-test` passed.
- `node --check scripts/linux-chromium-sandbox-probe/main.cjs` passed.
- Focused Prettier check for the probe, research note, and canonical plan passed.
- `git diff --check` passed.
- `nix eval --raw .#installer-mainnet.drvPath` was attempted; evaluation began building dependencies and timed out after 120 seconds, so no installer or packaged-host evidence was produced.

Deviations from approved plan:
- None in scope or security posture. The package build and host matrix remain manual release-owner work as planned. Production launch flags and runtime code remain unchanged.

User interaction required: yes.

User Handoff

Why user interaction is required now:
- Task completion requires an authoritative supported Linux matrix, a package-equivalent or release-owned artifact, disposable default-policy hosts, and a snapshotted restricted-userns VM. This workspace cannot truthfully supply that external release and host evidence.

Exact manual steps:
1. Provide the authoritative x86_64 Linux distribution/version matrix and identify its release owner/revision.
2. Follow `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md` to build or supply the exact artifact, recording source, proof-patch, lock, and installer hashes.
3. Run the installed `libexec/electron` wrapper and checked-in probe on each default-policy matrix host from a disposable `HOME`; sanitize all stderr on-host.
4. In a snapshotted disposable VM, deny unprivileged user namespaces using the supported distribution policy, independently verify denial, rerun without a bypass, confirm failure and no unsandboxed retry, then restore policy or revert the snapshot.
5. Return only the normalized evidence rows defined in the research note; do not transfer raw argv, paths, environment, stderr, usernames, hostnames, wallet data, or URLs.

Expected results:
- Every supported default-policy row exits zero with exact-renderer no-new-privileges, seccomp, zero capabilities, separate user/PID/mount namespaces and maps, no usable root-setuid helper assumption, and no bypass.
- The restricted-userns row fails nonzero without automatic `--no-sandbox` retry or host-policy mutation, and rollback is confirmed.

Output required back:
- Matrix authority/revision; source/task-diff/proof-patch/lock/installer hashes; normalized host and policy fields; installed runtime/helper identities; normalized probe JSON or absence reason; sanitized stderr summaries; bypass checks; and rollback confirmation.

Blocked or parallel:
- Agent-executable work is complete for this iteration. Review, tracker/PRD completion updates, final signoff, and the required task commit are blocked until the manual evidence is returned.

Outcome: Implementation paused at the approved manual-execution checkpoint; task remains in progress and production guest launch remains disabled.

Code Review: Iteration 1
Timestamp: 2026-08-11T19:27:26Z

Outcome: The implementation is not approvable. The live Ubuntu result is valid fail-closed feasibility evidence, but it does not satisfy task-005 acceptance and does not establish why Electron received `SIGILL`. The probe also has bounded-execution and privacy-redaction defects. No production guest, renderer authority, IPC, or wallet behavior was added, so those boundaries remain unaffected and unapproved.

Blocking findings:
1. Code defect: `scripts/linux-chromium-sandbox-probe/main.cjs:304-317,343-371` does not enforce its promised timeout. The timer starts only after `app.whenReady()`, and `await window.loadURL()` is awaited before the timed `rendererReady` promise. A readiness or load hang can therefore leave the probe running indefinitely. Cleanup at lines 415-423 can also skip `app.quit()` if storage clearing or profile removal stalls or throws.
2. Code defect: The fail-closed privacy sanitizer is incomplete. `scripts/linux-chromium-sandbox-probe/main.cjs:75-87` replaces roots without the documented path-component boundaries, while lines 109-137 and 152-170 recognize only a limited set of absolute-path prefixes, URL schemes, uppercase environment assignments, and exact-case username occurrences. Paths after delimiters such as `[` or `{`, `file://` URLs, hostname occurrences, case variants, and environment values outside `NAME=value` form can survive both sanitization and residual validation. The embedded self-test does not cover these bypasses.
3. Evidence/conclusion blocker: `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md:10-26,267-277` moves from an unexplained `SIGILL` to rejecting the userns-only packaging strategy and requiring a packaging architecture decision. The run produced no renderer evidence, stderr, AppArmor denial, or matching AppArmor audit event, while direct `unshare -Ur` succeeded. Without a same-artifact unsandboxed control or root-cause evidence, this proves that the proof variant cannot currently start, not that user namespaces caused the failure or cannot work after a packaging/runtime correction. The product decision may still be required, but debugging the invalid-opcode failure must remain an explicit option.
4. Evidence-quality blocker: The claim at `research/05-linux-chromium-sandbox-packaging.md:16-17` that Electron aborted before the main script ran is stronger than the evidence. The first milestone is optional through `DAEDALUS_PROBE_DEBUG=1` and occurs only after `require("electron")`; the documented execution command does not enable it. Absence of that milestone cannot prove that no probe JavaScript executed or that a future in-process canary could never run.
5. Acceptance blocker: No exact renderer PID or same-PID `NoNewPrivs`, seccomp, capabilities, namespace, or map evidence exists. Consequently task-005 acceptance criteria and the tracker requirement are unmet. Fail-closed startup failure is safe behavior, but it is not packaged Chromium sandbox proof.
6. Acceptance/product-decision blocker: The canonical plan still selects and requires proof of a userns-only strategy, while the current outcome records that strategy as disproved and leaves the build in progress. The authoritative supported Linux matrix and restricted-userns case are also absent. A product/release owner must decide the deployment or Linux support model, after which the plan, probe assertions, host matrix, and task-103 contract must be replanned consistently. A SUID-helper strategy, for example, is incompatible with the current probe assertion rejecting a usable helper.
7. Workflow blocker: The implementation log still ends at the original user handoff and states that no installer evidence was produced. The returned Ubuntu evidence and material deviation from the approved strategy were incorporated into research and the canonical outcome but not into an Implementation continuation as required by the build-loop workflow. Review history must preserve that evidence and escalation before signoff; task status must remain pending/in progress.

Non-blocking observations:
- The isolated proof patch, disposable `HOME`, installed artifact identities, user-owned mode-`0555` helper, absence of bypass flags, and prohibition on host-policy modification or unsandboxed retry are appropriately recorded.
- The intended exact-renderer authority model is sound: main obtains `webContents.getOSProcessId()` and verifies `--type=renderer` against the same PID allowlisted `/proc` evidence without preload, IPC, Node, remote content, or `process.sandboxed`.
- Production launch flags and runtime behavior remain unchanged, so the failed spike did not silently weaken the existing wallet or enable remote dApp content.
- `node --check`, the embedded self-test, and `git diff --check` pass, but they do not cover the blockers above.
- Loading `understand` without generating its file-writing graph, followed by verification against live files and all untracked task files, was appropriate under the no-edit constraint.
- The implementation is otherwise narrowly scoped; no additional application, renderer, IPC, backend, hardware, or network-policy machinery should be added before the packaging decision.

Approval bar:
- Make probe execution and cleanup genuinely bounded across `app.whenReady()`, `loadURL`, renderer readiness, session cleanup, profile removal, and quit.
- Strengthen the sanitizer and residual-leak tests for component-boundary roots, all relevant URL/path delimiters and schemes, hostnames, username variants, and environment-derived secrets.
- Run an instrumented same-artifact control or otherwise root-cause the `SIGILL`; narrow documentation claims to what the evidence proves.
- Obtain a product/release decision and authoritative Linux support matrix, then revise the canonical plan and probe contract for the selected packaging strategy.
- Produce passing exact-renderer packaged evidence for every supported Linux row and the required fail-closed restricted-host evidence, or explicitly decide that Linux dApps remain unavailable.
- Record the returned manual evidence and strategy deviation in the append-only implementation transcript while keeping task-005 and all downstream sandbox gates incomplete.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-08-11T20:04:23Z

Changes made:
- Incorporated the user authorization to execute all safe local Ubuntu testing and completed the isolated package-equivalent build/install/probe workflow without changing host policy or the real wallet home.
- Added a process-wide hard deadline beginning before `app.whenReady()`, concurrent load/renderer waiting, bounded session cleanup, forced profile cleanup, deterministic app exit, and failure reporting before window teardown.
- Strengthened evidence privacy with component-boundary root replacement, all-scheme URL removal, case-insensitive username/hostname removal, sensitive environment-value removal, broader path delimiters, residual checks, unrelated-stderr suppression, and expanded golden tests.
- Corrected exact-renderer evidence for Linux zygote forking: Electron `webContents.getOSProcessId()` remains authoritative while inherited `--type=zygote` argv is recorded rather than falsely rejected.
- Narrowed research/canonical claims to the evidence and recorded immutable package, host, helper-removal, kernel-trap, and local-only control results.

Files touched:
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md`

Verification run:
- Built the package-equivalent Mainnet installer from base `ac80c32310787ed4d49283be81675ce7fe682f7f` with only the two launch-flag removals; proof patch SHA-256 is `97d561998d3adbba3bbc2be2be8c0dd03df824ef9b0342de429469316f4a9fce`.
- Installer SHA-256 is `c9f1d8de93efd76e7b4c3e382165b431e32ff5f3692e4086157b390b72a07567`; install under a mode-0700 disposable `HOME` succeeded.
- Installed helper is user-owned mode `0555`; wrapper contains neither bypass. Default launch exits `132`/`SIGILL` with empty stderr and no optional post-require milestone. Removing and restoring the helper in the disposable install does not change the trap.
- Direct `unshare -Ur true` succeeds while Ubuntu AppArmor userns restriction is enabled.
- One explicit local-only same-artifact `--no-sandbox` diagnostic reaches and loads the renderer, records inherited `--type=zygote`, and exits `1` because the probe rejects `forbidden-main-switch`. This control is not passing evidence and is not a fallback.
- Probe self-test, Node syntax check, focused Prettier, sanitizer control-fixture execution, and `git diff --check` pass.

Deviations from approved plan:
- Material live evidence blocks the approved userns-only package proof. The exact cause is isolated to Chromium sandbox bootstrap but is not directly attributed to AppArmor or a specific syscall. The canonical substantive plan now requires Planner revision after a product/release decision.
- The original `--type=renderer` command-line assumption was invalid for Electron 41 Linux zygote forking; Electron still returns the associated renderer OS PID, whose inherited argv reports `--type=zygote`.

User interaction required: yes.

User Handoff

Why user interaction is required now:
- The current self-extracting Linux package cannot start Chromium sandbox bootstrap on the Ubuntu 24.04 development host, and no passing renderer evidence exists. Continuing requires a product/release choice about Linux deployment and support, not another safe local command the agent can infer.

Exact decision requested:
1. Choose `system_package`: embedded dApps must ship on Linux, and Linux packaging may become privileged/system-managed so a reviewed root-owned SUID helper or AppArmor policy can be installed.
2. Choose `linux_dapps_deferred`: keep embedded dApps disabled in the current Linux self-extracting package while Windows/macOS work proceeds; create a separate Linux packaging redesign task.
3. Choose `portable_debug`: retain the portable package goal and authorize further distro-specific Chromium/AppArmor investigation, accepting that Ubuntu 24.04 and possibly other rows remain unsupported for dApps until proof passes.
4. Provide the authoritative supported Linux distribution/version matrix for whichever option is selected.

Expected result:
- The returned decision establishes whether task-005 is replanned around system packaging, explicit Linux dApp deferral, or additional portable-package research. No option permits automatic `--no-sandbox` fallback or remote content without passing OS evidence.

Output required back:
- One decision token (`system_package`, `linux_dapps_deferred`, or `portable_debug`) plus the supported Linux distribution/version matrix or the owner/date by which that matrix will be supplied.

Blocked or parallel:
- Probe code defects and evidence wording are corrected, but task acceptance, Planner revision, remaining host evidence, final review, tracker/PRD synchronization, and the required task commit are blocked on this decision. Production guest launch remains disabled.

Outcome: Implementation iteration 2 paused at a product/release decision checkpoint; task-005 remains in progress.

Implementation: Iteration 3
Timestamp: 2026-08-12T13:28:25Z

Changes made:
- Recorded product/release decision: Linux ships system .deb and .rpm only; portable .bin, AppImage, Flatpak, and Snap are rejected.
- Added research/06-linux-system-package-decision.md as the durable decision record.
- Updated research/05 as portable negative evidence pointing at research 06; updated PRD Linux sandbox/packaging section, locked decisions, non-goals, risks, and gates.
- Updated dapp-browser-cip30-tasks.json: rewrote task-005 for .deb/.rpm proof; expanded task-103; added task-108 (.deb), task-109 (.rpm), task-110 (.bin retirement and auto-update migration); metadata v1.6.0 totalTasks 84.
- Updated architecture.md planned boundary, research/01 traceability, and task-005.md current outcome.

Files touched:
- .agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md
- .agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md
- .agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md
- .agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md
- .agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json
- .agent/plans/dapp-browser-cip30/task-plans/task-005.md
- .agent/system/architecture.md
- .agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md

Verification run:
- JSON parse of dapp-browser-cip30-tasks.json succeeded; task count 84 including task-108/109/110.
- No production runtime or packaging implementation was changed in this documentation pass.

Deviations from approved plan:
- Material product decision supersedes the portable userns-only strategy. Canonical task-005 plan requires Planner revision before the next implementation loop.

User interaction required: no for recording the decision; yes for subsequent matrix hosts and .deb/.rpm proof artifacts once packaging tasks produce them.

Outcome: Decision recorded across research, PRD, tasks, and architecture. task-005 remains in_progress without packaged sandbox certification. Production guest launch remains disabled.


