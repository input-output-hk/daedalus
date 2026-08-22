Implementation: Iteration 1
Timestamp: 2026-08-21T17:45:38Z

Changes made:
- Added private `internal.x86_64-linux.rpmPrototype` for the approved Phase A mainnet package-equivalent RPM only.
- Added restrictive cluster-specific candidate CIL with priority-200 module identity and exact Electron/helper file contexts; no permissive, `dontaudit`, unconfined-domain attribute, or broad generated policy.
- Added `--transition-only` probe mode with authoritative `webContents.getOSProcessId()` renderer correlation, semantic SELinux source-to-target context validation, exact-file-context checks, no-bypass checks, and explicit `containmentChecked: false`.
- Added the exact Fedora operator handoff at `.agent/plans/dapp-browser-cip30/research/09-task-109-rpm-validation-handoff.md` and marked task-109 in progress.

Files touched:
- `nix/internal/linux-rpm-prototype.nix`
- `nix/internal/linux-rpm-prototype.cil`
- `nix/internal/x86_64-linux.nix`
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/plans/dapp-browser-cip30/research/09-task-109-rpm-validation-handoff.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/plans/dapp-browser-cip30/task-plans/task-109.md`
- task-109 planning and implementation logs

Verification run:
- `nix build -L path:.#internal.x86_64-linux.rpmPrototype --no-link` passed.
- Final `--rebuild` reproducibility check passed with no output mismatch.
- Candidate NEVRA is `daedalus-mainnet-0:11.2.0-0.git2f5e92213.prototype2.x86_64`; SHA-256 is `464495d2720bab436e46477f2042fe101959721706216b49ed8cc1ff082874f0`.
- RPM header, unsigned state, explicit post dependencies, and `%post` were queried; payload link metadata contains no `/nix/store` target.
- Probe syntax, self-test including semantic SELinux context cases, focused Prettier, Nix formatting, task JSON parse, and `git diff --check` passed.
- One attempted Nix formatting check used an unsupported `--check` flag; it was rerun successfully with `--fail-on-change`.

Deviations from approved plan:
- None. Phase B public outputs, full lifecycle scriptlets, all clusters, CI/Hydra wiring, and implementation-complete product documentation were intentionally not started.

User interaction required: yes.

User Handoff:
- Why required now: Fedora 43 native SELinux compilation, module activation, graphical-login transition, exact main/renderer labels, AVC behavior, and bounded packaged startup cannot be established in this workspace. The approved plan forbids Phase B until this checkpoint passes.
- Exact manual steps: use a clean snapshotted Fedora 43 x86_64 graphical desktop in enforcing mode; execute `.agent/plans/dapp-browser-cip30/research/09-task-109-rpm-validation-handoff.md` against the exact candidate and probe hashes; keep raw host data local; restore the snapshot after the run.
- Expected result: exact RPM hash/NEVRA, successful priority-200 module install, exact file contexts, source mapping `unconfined_r:unconfined_t`, exact derived `daedalus_mainnet_t` main/renderer contexts, no unexplained AVC, no sandbox bypass, bounded startup if transition succeeds, and successful snapshot rollback. A fail-closed module/transition/startup category is also valid evidence but blocks Phase B.
- Return: only the normalized JSON record specified in the handoff, including fixed failure categories and rollback result; do not return raw AVCs, PIDs, paths, usernames, hostnames, environment, or stderr.
- Work status: blocked. No implementation or review work can continue in parallel until this evidence is reviewed.

Outcome: Phase A implementation and agent-executable verification complete; task-109 paused at the mandatory Fedora 43 user checkpoint.

Implementation: Iteration 2
Timestamp: 2026-08-21T20:45:00Z

Changes made:
- Added the two missing `object_r` associations required for Fedora to accept the custom executable file contexts and incremented the byte-distinct prototype revision to 3.
- Ran the exact prototype in a disposable Fedora 43 KVM graphical desktop with targeted SELinux enforcing and normalized all retained evidence.
- Recorded the failed checkpoint in the canonical plan, handoff, and tracker; Phase B was not started.

Verification run:
- `nix build -L path:.#internal.x86_64-linux.rpmPrototype --no-link --rebuild --print-out-paths` passed with no output mismatch.
- Candidate NEVRA is `daedalus-mainnet-0:11.2.0-0.git2f5e92213.prototype3.x86_64`; SHA-256 is `cced2f1dc08d39b664049bbdd85bdea5fd01ce1ec65f44989527870a9341ba70`.
- Fedora 43 source mapping was `unconfined_r:unconfined_t`; RPM install and exact Electron/helper contexts passed.
- The expected priority-200 module identity was missing. The transition fixture failed as `fail:electron-runtime`, bounded startup was not run, and normalized AVC categories were dynamic-loader `bin_t:file map` plus disposable evidence-sink `var_lib_t:file write`.
- No sandbox bypass was used. The Fedora backing-image checksum matched before the overlay and all raw evidence were deleted; rollback passed.

Deviations from approved plan:
- The mandatory checkpoint failed, so the approved fail-closed boundary stopped policy expansion and all Phase B lifecycle/output/CI work.

Outcome: task-109 remains in progress and blocked on policy redesign or a contract-authority decision to narrow Fedora support.

Review: Iteration 2
Timestamp: 2026-08-21T20:55:00Z
Status: changes requested; Phase B remains blocked

Findings:
- High: the installed `daedalus-mainnet.cil` basename does not match the expected active module identity `daedalus_mainnet`; this explains the missing priority result but not the independent dynamic-loader AVC.
- High: transition-only probe output does not independently verify enforcing state, active module priority, or active/extracted fingerprints before carrying expected module metadata.
- Medium: the RPM payload records helper mode `0755` while `%post` and the manifest establish `4755`, causing an RPM verification mismatch.
- Medium: prototype revision 3 changed policy bytes without incrementing the declared policy semantic version from `1.0.0`.
- Medium: the exact normalized checkpoint result needed to be retained alongside the fixed-schema template.
- Low: the plan's future task-807 final-RPM revalidation requirement is not yet synchronized to tracker acceptance.

Disposition:
- Added the exact normalized checkpoint result and the known module-identity/helper/semantic-version blockers to the Fedora handoff.
- Did not change package or policy bytes after the failed checkpoint, expand permissions from AVCs, or begin Phase B. A new design decision and byte-distinct candidate are required before further Fedora execution.

Implementation: Iteration 3
Timestamp: 2026-08-21T23:25:00Z

Changes made:
- Following the user-selected redesign, created byte-distinct prototypes 5 through 8 with coherent policy semantic versions.
- Corrected active module identity by installing a temporary underscore-basename CIL, encoded helper mode `4755` in RPM metadata, and added enforcing/priority checks plus exit-only transition evidence to the probe.
- Added only permissions observed in successive enforcing runs for the loader and Chromium sandbox initialization; no permissive domain, unconfined-domain attribute, `dontaudit`, generated policy, or coredump permission was added.

Verification run:
- Prototype 8 reproducibly rebuilt at `/nix/store/pyfax9gdd0dbwjanj7c2dzkba6ljps9h-daedalus-mainnet-11.2.0-0.gitf302ed920.prototype8-rpm-prototype` with SHA-256 `e1a0104f145e993b8d973e3e4aff37df2f33eeb4f6d6d57cf0d06e8554284b7a`.
- Probe self-test, focused Prettier, and `git diff --check` passed.
- Clean Fedora 43 KVM evidence passed install, enabled module `daedalus_mainnet` at priority 200, policy version `1.0.4`, exact Electron/helper contexts, default `unconfined_r:unconfined_t` source mapping, RPM-owned helper `4755`, and no-bypass checks.
- Transition remained `fail:electron-runtime`; main/renderer transitions and bounded startup were not established. Remaining normalized app AVCs were helper `execute`, `proc_t:file read`, `sysctl_fs_t:dir search`, and `sysfs_t:file read`. A coredump collector denial was excluded from policy.
- The Fedora backing-image checksum matched before every clean-overlay replacement and before final deletion; no libvirt session domain or raw VM evidence remains.

Deviations from approved plan:
- The user authorized a bounded policy redesign after prototype 3 failed. Expansion stopped when prototype 8 exposed an ordinary desktop/runtime permission surface requiring separate policy review.

Outcome: Phase A feasibility remains failed closed. Phase B is blocked pending a reviewed Fedora Electron desktop policy or a contract decision to narrow Fedora support.

Implementation: Iteration 4
Timestamp: 2026-08-21T23:40:00Z

Changes made:
- Hardened transition-only module evidence to reject disabled rows and distinguish configured manifest policy identity from active observations.
- Made `--exit-only` suppress deadline, final, and global-failure output paths.
- Corrected and hash-pinned the Fedora handoff, documented the full prototype-8 policy scope, and added the promised task-807 final-RPM release gate.
- Incremented to byte-distinct prototype 9 because the probe bytes changed after prototype 8 testing.

Verification run:
- Probe self-test, focused Prettier, JSON parsing, and `git diff --check` passed.
- Probe self-test includes an exit-only failure subprocess and proves zero stdout/stderr bytes.
- Prototype 9 built successfully at `/nix/store/8wz6jrv3ka7pvgbija6942965vqg3hn7-daedalus-mainnet-11.2.0-0.git246b06b0e.prototype9-rpm-prototype` with SHA-256 `29920a5b1641942787244b47d46a5c0a42e5a65671d1c42669c7c2a4c540fb5e` and NEVRA `daedalus-mainnet-0:11.2.0-0.git246b06b0e.prototype9.x86_64`.
- Prototype 9 was not run on Fedora and carries no checkpoint claim; prototype 8 remains the last tested failed candidate.

Outcome: review-evidence defects are corrected, but Phase A feasibility and Phase B remain blocked by the failed prototype-8 transition and need for a separately reviewed Fedora desktop policy.

Review: Iteration 4
Timestamp: 2026-08-22T00:20:01Z
Reviewer: OpenCode independent Reviewer agent
Status: approved; Phase B remains blocked

Findings:
- No release-blocking correctness findings remain.
- Prototype 8 active policy bytes were not authenticated; the handoff records both fingerprint checks as false and does not claim otherwise.
- Prototype 9 remains Fedora-untested. Its exit-only self-test covers an early failure; late Electron/runtime silence remains a Fedora testing gap.
- Normalized evidence template array/notes consistency was corrected after review.
