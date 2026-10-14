Implementation: Iteration 1
Timestamp: 2026-08-18T14:53:22Z

Changes made:
- Added additive root-managed `.deb` outputs for all four Linux clusters, including Debian metadata, desktop integration, absolute launcher configuration, bundled ELF dependency auditing, and Hydra/Buildkite exposure without removing the portable `.bin` outputs.
- Added transactional `preinst`, `postinst`, `prerm`, and `postrm` handling with lock-based serialization, exact package/helper/profile ownership checks, upgrade snapshots and unwind paths, downgrade and foreign-state refusal, safe removal/purge behavior, and fail-closed Ubuntu AppArmor setup.
- Added system-package update mode so package builds reject the portable updater before reading or executing installer bytes, while preserving existing portable behavior.
- Updated the sandbox probe and governing documentation to `task-108-matrix-2026-08-18`, including Ubuntu 22.04 wallet-only handling, supported Ubuntu semantic AppArmor checks, Debian support, omitted-host wallet-only behavior, and the manual evidence boundary.
- Added the exact disposable-host handoff at `.agent/plans/dapp-browser-cip30/research/08-task-108-deb-validation-handoff.md`.

Verification run:
- `yarn compile` passed.
- Focused Jest passed the package updater refusal test.
- Probe syntax and embedded self-test passed.
- Nix shellcheck passed all maintainer scripts.
- `linux-deb-package-contract` passed.
- Mainnet, preprod, preview, and selfnode `.deb` outputs built successfully.
- `nix build --rebuild .#deb-installer-mainnet --no-link` reproduced the candidate output.
- `nix flake check --no-build` passed after restricting `.deb` package outputs to native x86_64 Linux; incompatible Darwin systems were omitted by Nix.
- Focused Prettier, Nix formatting, task JSON parse/count/dependency/acyclicity validation, and `git diff --check` passed.

Deviations from approved plan:
- No scope deviation. Ubuntu 22.04 remains wallet-only pending separate AppArmor proof, and production dApp guest launch remains disabled.
- Local static/build verification cannot substitute for package-manager, host-policy, reboot, rollback, and startup evidence on the required disposable distributions.

User interaction required now: yes.

User Handoff:
- Why user interaction is required now: the approved `interactive_validation` gate requires privileged installation and lifecycle evidence on disposable, snapshotted hosts. This environment cannot truthfully provide the required distro/AppArmor/package-manager matrix without those hosts.
- Exact candidate: `/tmp/opencode/task-108-mainnet/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `e2ff93c109b1ca1763ce169c7f5a00a50f2c4d492e10b03ffad7bf704dd723bb`.
- Exact manual steps: execute `.agent/plans/dapp-browser-cip30/research/08-task-108-deb-validation-handoff.md` on clean Ubuntu 22.04.x, Ubuntu 24.04.x, Ubuntu 26.04.x, Debian 12.x, Debian 13.x, and one omitted distribution. Include its repeated configure, remove/purge/reinstall, running-process refusal, profile/statoverride/nosuid failures, interrupted configure, upgrade/downgrade/mixed-script unwind, failed-upgrade rollback, supported-Ubuntu parse/load failure, reboot persistence, startup smoke, and wallet-sentinel checks.
- Expected result: every row reports its prescribed supported or wallet-only state; all owned state, rollback, refusal, no-bypass, non-mutation, and cleanup assertions pass; no production dApp or remote URL is opened.
- Return to the Orchestrator: the normalized per-row evidence listed under `Evidence To Return` in the handoff, with raw host identifiers, traces, paths, PIDs, environment, and stderr retained only on each disposable host.

Work status: blocked at the required human validation checkpoint. No code-review iteration, completion claim, tracker completion, or final commit may proceed until the normalized evidence is returned.

Outcome: Agent-executable implementation and automated verification completed; awaiting disposable-host validation evidence.

Implementation: Iteration 2
Timestamp: 2026-08-18T15:44:23Z

User evidence received:
- Ubuntu 24.04.4 LTS x86_64 matched the supported matrix row and the original candidate and lock-file hashes.
- Initial package metadata and dependency inspection passed, and the wallet sentinel hash was recorded.
- Initial installation failed closed during `postinst` with `AppArmor profile loaded identity mismatch`; dpkg correctly retained a `half-configured` package and the profile file was rolled back.
- A temporary parser load established the authoritative kernel representation: parser name `/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron`, loaded as the exact same path with suffix ` (unconfined)`.

Root cause and correction:
- `flags=(default_allow)` is intentionally represented by Ubuntu/AppArmor as ` (unconfined)`, while the package identity check and probe fixtures incorrectly required ` (enforce)`.
- Changed the package load assertion and manifest suffix to exact ` (unconfined)` while retaining the exact attachment path, ABI 4.0, `default_allow`, `userns`, parser acceptance, loaded-profile, and same-renderer checks.
- Added a probe manifest assertion that rejects caller-controlled or stale loaded-profile suffixes, including ` (enforce)`.

Verification run:
- Probe syntax, self-test, and focused Prettier passed.
- Nix shellcheck and `linux-deb-package-contract` passed.
- Mainnet, preprod, preview, and selfnode corrected `.deb` outputs built successfully.
- Mainnet reproducibility rebuild passed.
- `nix flake check --no-build` and `git diff --check` passed.

User interaction required now: yes.

User Handoff:
- Purge the failed same-version package before installing the corrected candidate; same-version in-place replacement is intentionally refused.
- Corrected candidate: `/tmp/opencode/task-108-mainnet-fixed/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `d9030fd9b5d2578f3dc154a1743d7765b2f1f7f439b90ee3f4811a7c4ca4ed19`.
- Preserve and recheck the existing wallet sentinel before and after purge/install.
- Resume the Ubuntu 24.04 row from a clean package state using the corrected candidate and return the normalized evidence from `.agent/plans/dapp-browser-cip30/research/08-task-108-deb-validation-handoff.md`.

Work status: blocked at the corrected-candidate human validation checkpoint.

Outcome: The first live-host defect is fixed and locally verified; awaiting corrected-candidate Ubuntu 24.04 evidence.

Implementation: Iteration 3
Timestamp: 2026-08-18T17:01:09Z

User evidence received:
- The corrected AppArmor candidate purged cleanly after the prior half-configured failure, preserving the wallet sentinel.
- Fresh corrected installation succeeded; dpkg reported installed, the exact profile loaded as ` (unconfined)`, helper ownership/mode was `root:root 4755`, profile ownership/mode was `root:root 0644`, and the sentinel remained unchanged.
- `dpkg --verify` returned no package-file differences.
- Repeating `dpkg --configure` on an already configured package produced dpkg's expected refusal without invoking `postinst`; the handoff now uses repeated direct `postinst configure` for convergence and reserves `dpkg --configure -a` for an intentionally unconfigured recovery state.
- Manifest inspection found invalid JSON: `matrixRow` contained literal backslashes around its quoted string.

Root cause and correction:
- The shell formatter used `printf '\"%s\"'`, which emitted JSON-invalid `\"ubuntu-24.04\"` bytes.
- Corrected matrix-row encoding to emit a JSON string, added fail-closed character validation for distro-derived manifest values, and extended `linux-deb-package-contract` to execute the packaged `write_manifest` function and parse/assert the generated JSON with `jq`.

Verification run:
- Nix formatting, shellcheck, generated-manifest package contract, and `git diff --check` passed.
- A new mainnet candidate built successfully.

User interaction required now: yes.

User Handoff:
- Purge the installed same-version package, verify the sentinel remains unchanged, and install the new candidate from a clean package state.
- New candidate: `/tmp/opencode/task-108-mainnet-json-fixed/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `a9bd54c30fbc9cc8468209746ac01138b9a0e8a1799c59b80f19064ac9ca0b2b`.
- Resume Ubuntu 24.04 validation with manifest parsing first.

Work status: blocked at the regenerated-candidate human validation checkpoint.

Outcome: The live manifest defect and its missing automated regression check are fixed; awaiting regenerated-candidate Ubuntu 24.04 evidence.

Implementation: Iteration 4
Timestamp: 2026-08-18T17:54:18Z

User evidence received:
- The JSON-corrected candidate installed successfully on Ubuntu 24.04.4, produced the expected supported matrix identity, exact AppArmor policy identity and ` (unconfined)` suffix, helper mode/hash, absolute launch paths, and preserved the wallet sentinel.
- Purging the preceding candidate warned that `/opt/daedalus/mainnet/share` was not empty. The operator manually removed `/opt/daedalus` before inspecting the exact leftover, but code inspection confirmed that the generated manifest was not a dpkg-owned payload file and `postrm` never removed it.

Root cause and correction:
- `postrm remove|purge` removed the profile and state markers but omitted the authenticated runtime-generated manifest, so dpkg could not remove its parent directory.
- Added exact marker/hash verification and removal for the generated manifest, then removal of only empty package-root ancestors.
- Extended profile and manifest cleanup checks to reject dangling symlinks rather than silently preserving them.
- Added package-contract assertions for authenticated manifest cleanup and empty-directory cleanup.

Verification run:
- Nix shellcheck, generated-manifest package contract, Nix formatting, and `git diff --check` passed.
- A new mainnet candidate built successfully.

User interaction required now: yes.

User Handoff:
- The currently installed prior candidate has the old `postrm`; record its expected leftover on purge before cleaning that exact stale package root manually.
- Install the new candidate, verify its manifest, then purge it and prove `/opt/daedalus/mainnet`, its profile, and package state are absent without manual cleanup; confirm the sentinel remains unchanged and reinstall for continued testing.
- New candidate: `/tmp/opencode/task-108-mainnet-purge-fixed/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `f78bf9fe1e79d460cd8fcb9b2062c5e015be736a89108fefeca6680a6ad7fc9c`.

Work status: blocked at the regenerated-candidate purge/reinstall checkpoint.

Outcome: Authenticated generated-manifest cleanup is fixed and locally verified; awaiting live purge/reinstall evidence.

Implementation: Iteration 5
Timestamp: 2026-08-18T18:13:51Z

User evidence received:
- Purging the prior candidate proved the exact stale object was the runtime-generated `share/daedalus-sandbox-identity.json`, confirming the diagnosed cleanup defect.
- The purge-fixed candidate hash matched, installed and configured successfully, and emitted parseable manifest JSON.
- Purging the purge-fixed candidate completed without a dpkg directory warning.
- The package root, profile file, loaded profile, and package state directory were all absent after purge without manual cleanup.
- The wallet sentinel hash remained unchanged.

Result:
- Corrected-candidate fresh install, authenticated generated-state purge, AppArmor unload, state cleanup, and wallet non-mutation pass on Ubuntu 24.04.4.

User interaction required now: yes.

User Handoff:
- Reinstall the same exact candidate for final baseline, repeated direct `postinst configure`, static privilege/configuration, AppArmor reload, trace non-access, and bounded startup-smoke checks before destructive lifecycle fixtures.

Work status: blocked at the remaining Ubuntu 24.04 validation checkpoints.

Outcome: Live purge/reinstall checkpoint passed; continue with the unchanged candidate.

Implementation: Iteration 6
Timestamp: 2026-08-18T18:48:59Z

User evidence received:
- Final-candidate traced reinstall succeeded.
- Two repeated direct `postinst configure` invocations converged with identical helper, profile, and manifest hashes.
- `dpkg --verify` returned no differences.
- Launcher configuration selected `system-package-disabled`, omitted `updateRunnerBin`, and used absolute package-owned binary paths.
- The helper filesystem was writable and not `nosuid`; helper ownership/mode/link/type was `root:root 4755`, with no file capabilities.
- The helper was the only setuid/setgid file under the package root, recursive capability inspection was empty, and the portable update runner was absent.
- AppArmor 4.0.1 parsed and reloaded the profile, and the exact Electron attachment remained loaded as ` (unconfined)`.
- Installation traces did not access the wallet sentinel, whose hash remained unchanged.

Result:
- Ubuntu 24.04 final-candidate baseline, convergent configure, privilege inventory, absolute launcher/update refusal contract, AppArmor identity, and wallet non-mutation checks pass.

User interaction required now: yes.

User Handoff:
- Run the bounded startup smoke and running-process package-removal refusal, then prove the package remains configured and unchanged after cleanup.

Work status: blocked at the remaining Ubuntu 24.04 runtime and destructive lifecycle checkpoints.

Outcome: Final baseline passed; continue with runtime refusal evidence.

Implementation: Iteration 7
Timestamp: 2026-08-18T19:27:04Z

User feedback:
- The manual validation sequence involved too much command-by-command copy and paste; an operator driver is the appropriate interface.

Changes made:
- Added a one-command Ubuntu 24.04 driver that verifies and installs the exact candidate, preserves the wallet sentinel, launches Daedalus in a dedicated process group, records startup evidence, freezes that exact process group, verifies package-removal refusal, performs bounded cleanup, and validates rollback state and hashes.
- Kept raw process IDs, argv, stderr, paths, and package-manager transcripts in private local evidence directories while printing normalized results only.
- Restricted the driver explicitly to Ubuntu 24.04 rather than implying coverage for other matrix rows.
- Safety review found that refused removal left the package `removing` marker behind. Changed `postinst abort-remove` to clear only that marker.
- Safety review also found successful configuration retained stale pre-upgrade rollback state. Successful `configure` now commits by removing the prior snapshot; `abort-remove` no longer restores unrelated state.
- Moved operator-only drivers under the task research handoff so future driver edits do not perturb packaged application inputs.

Verification run:
- Bash syntax, repository Nix shellcheck, package contract, Nix formatting, focused documentation formatting, and `git diff --check` passed.
- Three implementation safety-review rounds completed; final review reported no high/critical findings.
- Final mainnet candidate built successfully.

User interaction required now: yes.

User Handoff:
- Run `.agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-run-validation.sh` once from the repository root.
- The wrapper uses `/tmp/opencode/task-108-mainnet-final-validation/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `505ed386b1481d94aa37f4cc71fece085dc589e790ee61600d3b3650b002f9f9`, unless an explicit candidate path is provided.
- Return only its normalized output; raw evidence remains local.

Work status: blocked at the automated Ubuntu 24.04 runtime/refusal checkpoint.

Outcome: Manual command sequence replaced with a reviewed one-command driver; awaiting its result.

Implementation: Iteration 8
Timestamp: 2026-08-18T19:31:28Z

Safety correction:
- Review of the candidate-install wrapper found that automating replacement of an arbitrary installed package was not safely recoverable under interruption or installation failure. The wrapper was withdrawn before execution.
- The supported automation boundary is now the lengthy runtime/removal-refusal checkpoint only. Exact-candidate hash verification and package installation remain an explicit short operator step.
- Strengthened the runtime driver to compare wallet-sentinel content plus uid, gid, mode, link count, file type, size, and mtime on success and every post-launch failure path.
- Explanatory prompts now go to stderr; normalized evidence remains on stdout and raw data remains local.

Verification run:
- Bash syntax, repository Nix shellcheck, focused documentation formatting, and `git diff --check` passed.
- Final safety re-review reported no high or critical findings in the runtime driver or handoff.

User interaction required now: yes.

User Handoff:
- Purge the installed prior candidate and install `/tmp/opencode/task-108-mainnet-final-validation/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `505ed386b1481d94aa37f4cc71fece085dc589e790ee61600d3b3650b002f9f9`.
- Run `.agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-deb-runtime-validation.sh` and return only its normalized output.

Work status: blocked at the automated Ubuntu 24.04 runtime/refusal checkpoint.

Outcome: Unsafe package-replacement automation withdrawn; reviewed runtime automation is ready.

Implementation: Iteration 9
Timestamp: 2026-08-18T21:01:48Z

User evidence received:
- Final candidate hash matched and fresh installation succeeded.
- Runtime driver observed the exact Electron process, expected AppArmor label, no sandbox-bypass arguments, and successfully froze the test process group.
- Package-manager refusal assertion did not run because detached-session `sudo` could not use the tty-scoped credential established by `sudo -v`.
- Post-cleanup package verification, package/AppArmor hashes, and wallet-sentinel content/metadata all remained unchanged; the package stayed installed.

Driver correction:
- Retained GNU `timeout` process-group ownership but removed `setsid` from the package-manager subprocess so tty-scoped sudo credentials remain available.
- Added `sudo -n` and a categorized cached-credential failure instead of allowing a hidden prompt.
- Verified experimentally that the background `timeout` monitor owns a process group matching its tracked PID, preserving bounded signal cleanup.

Verification run:
- Bash syntax, repository Nix shellcheck, and `git diff --check` passed.

User interaction required now: yes.

User Handoff:
- Rerun `.agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-deb-runtime-validation.sh`; reinstall is not required.
- Return only its normalized output.

Work status: blocked at the rerun of the Ubuntu 24.04 removal-refusal checkpoint.

Outcome: First runtime attempt was non-mutating; tty-scoped sudo handling is corrected for rerun.

Implementation: Iteration 10
Timestamp: 2026-08-18T21:13:43Z

User evidence received:
- The rerun stopped before package-manager mutation because Electron exited with `SIGILL`; package and wallet state remained unchanged.
- Kernel evidence showed a stable invalid-opcode trap at image-relative offset `0x8ae5ea8`, matching the prior portable failure.

Root cause and correction:
- Bounded syscall tracing showed the trap before sandbox syscalls or JavaScript startup.
- Disassembly identified an intentional `ud2` reached when `getenv("CHROME_DEVEL_SANDBOX")` returned null; the referenced binary string and exact branch establish the missing startup contract.
- A bounded same-installed-binary diagnostic with `CHROME_DEVEL_SANDBOX` set to the exact root-owned mode-`4755` helper reached Electron/JavaScript startup without `SIGILL` or any sandbox-bypass switch.
- The package launcher now exports `CHROME_DEVEL_SANDBOX` as the build-time literal package helper path. The package contract asserts that exact binding.
- Historical portable SIGILL evidence remains negative evidence; documentation now records the later root cause without approving a portable/user-owned helper.

Verification run:
- Nix formatting, package contract, probe self-test, and `git diff --check` passed.
- A new mainnet candidate built successfully.

User interaction required now: yes.

User Handoff:
- Purge the installed prior candidate and install `/tmp/opencode/task-108-mainnet-sandbox-env-fixed/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`, SHA-256 `d21261951b7e0105a57edeb41a9c43d12fe23ec8a40ad6aa4718ab2c0cf03966`.
- Rerun `.agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-deb-runtime-validation.sh` and return only its normalized output.

Work status: blocked at corrected-candidate Ubuntu 24.04 startup/removal-refusal evidence.

Outcome: The longstanding Electron SIGILL is root-caused and fixed in the package launcher; awaiting live corrected-candidate evidence.

Implementation: Iteration 11
Timestamp: 2026-08-18T21:20:35Z

User evidence received:
- Ubuntu 24.04.4 final sandbox-environment candidate hash matched and installation succeeded.
- Bounded startup observed seven package Electron processes; all seven had the expected exact AppArmor ` (unconfined)` label.
- No `--no-sandbox`, `--disable-setuid-sandbox`, or `ELECTRON_DISABLE_SANDBOX` bypass appeared.
- The driver froze the exact package process group and native package removal returned nonzero status `100` with the expected running-process refusal.
- Bounded cleanup removed all test processes; dpkg remained installed, `abort-remove` cleared the removal marker, AppArmor identity remained loaded, and package hashes were unchanged.
- Wallet-sentinel content and metadata were unchanged.

Result:
- Ubuntu 24.04 root-managed launcher startup, required SUID-helper environment binding, AppArmor identity, no-bypass argv, running-process removal refusal, abort-remove rollback, and wallet preservation pass for candidate SHA-256 `d21261951b7e0105a57edeb41a9c43d12fe23ec8a40ad6aa4718ab2c0cf03966`.

Remaining validation:
- Ubuntu 24.04 destructive failure/recovery and reboot fixtures remain.
- Ubuntu 22.04 wallet-only, Ubuntu 26.04 supported, Debian 12/13 supported, and one omitted-distribution row remain.
- Byte-distinct upgrade/downgrade/mixed-script fixtures require separately versioned candidates and snapshots.

User interaction required now: yes, on additional disposable/snapshotted hosts.

Work status: blocked on the remaining authoritative host matrix; the current Ubuntu 24.04 runtime checkpoint passed.

Outcome: Ubuntu 24.04 baseline runtime/removal-refusal evidence accepted; remaining matrix evidence is still required before review or completion.

User Acceptance: Iteration 12
Timestamp: 2026-08-18T21:25:10Z

Decision:
- The user accepted task-108 as completed with the implemented package, automated verification, and Ubuntu 24.04 live evidence.
- The unexecuted Ubuntu 22.04/26.04, Debian 12/13, omitted-distribution, reboot, destructive failure/recovery, and byte-distinct upgrade/downgrade fixtures are deferred until the full PRD implementation has been assembled.
- Deferred rows remain explicitly untested; this decision does not convert them to passes.
- Task-005-b exact-renderer certification and later real-guest, release-candidate, and post-pilot package gates remain mandatory and are not waived.

Documentation disposition:
- The PRD records the deferred package-lifecycle validation decision.
- `research/08-task-108-deb-validation-handoff.md` remains the executable manual runbook.
- Task-807 owns reconciliation of this deferred evidence against the final release-candidate package after the full PRD implementation is assembled.

User interaction required now: no for task-108 completion; deferred manual interaction remains at the release-candidate gate.

Outcome: User completion acceptance recorded truthfully; proceed to final implementation review and tracker reconciliation.

Reviewer: Iteration 1
Timestamp: 2026-08-18T21:58:12Z

Verdict: requires_changes

Findings:
- Failed configuration did not transactionally restore AppArmor bytes, helper mode, ownership markers, configured version, and statoverride state.
- Package checks searched transition names but did not execute rollback behavior.
- Buildkite's generic Linux artifact glob already uploaded the `.deb`, making the explicit `.deb` upload duplicate.
- Research incorrectly said the identity manifest pinned the observed AppArmor parser version.

Implementer response:
- Snapshot and restore now include profile/manifest markers, configured version, and statoverride state; fresh candidate profile/manifest state is removed and a fresh helper returns to payload mode `0755`.
- Candidate profile ownership remains pending until the complete configure transaction commits.
- Added executable upgrade and fresh-failure rollback fixtures.
- Removed the duplicate Buildkite upload and corrected parser-version documentation.

Reviewer: Iteration 2
Timestamp: 2026-08-18T21:58:12Z

Verdict: requires_changes

Finding:
- Rollback helpers were corrected, but real `postinst configure` failures were not yet wrapped in a transaction that invoked them immediately.

Implementer response:
- Factored configuration into a guarded subshell transaction and routed any nonzero configure status through immediate state restoration.
- Changed the fresh fixture to execute the transaction with a late injected failure.

Reviewer: Iteration 3
Timestamp: 2026-08-18T21:58:12Z

Verdict: requires_changes

Finding:
- Invoking the transaction on the left side of `||` suppressed POSIX `errexit` inside the function stack.

Implementer response:
- Removed conditional invocation. Caller `errexit` is temporarily disabled only while the transaction runs as a simple command; status is captured before `errexit` is restored and checked separately.
- The fixture now injects an unguarded `false`, asserts a following command never runs, and verifies complete rollback.

Reviewer: Iteration 4
Timestamp: 2026-08-18T21:58:12Z

Verdict: approved

Verification:
- Package contract passed, including executable upgrade and fresh failed-configure rollback fixtures.
- Shellcheck and `git diff --check` passed.
- Reviewer reproduced the control flow under `dash` and POSIX Bash and reported no findings.

Residual accepted gap:
- The user-deferred package lifecycle matrix remains mandatory task-807 release-candidate manual validation after the full PRD implementation is assembled. No deferred row is represented as passed, and task-005-b remains unchanged.

Outcome: Implementation review approved; task-108 may be marked completed under the recorded user acceptance and evidence deferral.
