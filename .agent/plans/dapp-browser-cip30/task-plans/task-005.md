# Task task-005: Validate Linux Chromium sandbox packaging

## Task

- Task ID: `task-005`
- Title: `Validate Linux Chromium sandbox packaging`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- `task-001` is complete and makes active packaged Chromium OS sandboxing a release gate for hostile remote content. `task-103` cannot safely remove the current process-wide bypasses until this task proves a viable Linux package strategy and freezes the unsupported-host behavior.
- Live source still passes `--disable-setuid-sandbox --no-sandbox` from both the development watcher and packaged `daedalus-frontend`. Electron documents that `--no-sandbox` disables Chromium containment for all processes even if `sandbox: true` still removes Node from a renderer.
- The portable self-extracting installer writes under the invoking user's home directory. Its copied `chrome-sandbox` is currently mode `0555`, and this package model cannot establish a root-owned mode-`4755` helper. The smallest viable strategy to validate is therefore unprivileged user namespaces, not a SUID-helper redesign.
- Ubuntu's AppArmor user-namespace restriction and hosts that disable unprivileged user namespaces make a single-host development launch insufficient evidence. The supported host matrix and a deliberately unavailable-userns case must be exercised against the installed artifact.

## Interaction Mode

- Mode: `manual_execution`.
- Reason: the task is primarily package-equivalent installer and Linux host-policy validation. This workspace can inspect and build artifacts, but it cannot truthfully represent every supported distribution or safely toggle host-wide namespace/AppArmor policy on the user's real workstation.
- Required user/release-owner inputs:
  - the authoritative supported Linux distribution/version matrix, which is not named in current repository documentation;
  - disposable x86_64 Linux VMs or test users for that matrix, including at least one default host where unprivileged user namespaces work and one host where they are disabled or denied by policy;
  - the exact self-extracting artifact to certify, or authorization to build a package-equivalent proof variant from the reviewed task commit with only the retained flag-removal patch described below. Task-103 and later release gates must separately certify the actual production artifact.
- Required manual steps: install the artifact under a disposable `HOME`, execute the checked-in probe with its packaged Electron launcher on each matrix host, capture the evidence listed in Verification, and run the negative namespace-policy case only in a disposable VM with rollback/reboot available.
- Evidence required back: artifact SHA-256 and source revision; distribution, kernel, desktop/session, and namespace/AppArmor settings; installed helper ownership/mode; deterministically normalized launch argv; probe JSON identifying the renderer OS PID; same-PID `/proc` status and namespace evidence; expected success/failure, stderr category/hash/sanitized excerpt; and confirmation that no launcher silently added a sandbox bypass. Raw argv, paths, environment values, and stderr remain on the disposable host.
- Agent work can proceed first: source/history analysis, the bounded probe and evidence schema, package build/static inspection, current-host positive execution if available, and the operator runbook do not depend on the final matrix runs. Completion, tracker status, and the packaged sandbox gate remain blocked until the manual evidence is returned and reviewed.
- No wallet, funds, recovery phrase, configured Cardano network, hardware device, or remote dApp is required.

## Scope

- Select and document one Linux Chromium sandbox strategy compatible with the current home-directory self-extracting package.
- Add a small release-artifact probe that creates one local `BrowserWindow` with the planned guest's sandbox-critical preferences and identifies its exact renderer using `webContents.getOSProcessId()`.
- Collect machine-readable evidence from the exact renderer: normalized command line, `/proc/<pid>/status` sandbox fields, effective capabilities, and user/PID/mount namespace identities compared with the Electron main process.
- Validate the exact installed Electron executable and wrapper, not `node_modules` Electron or an unpackaged development-only binary.
- Exercise the release-owner-confirmed distribution matrix with unprivileged user namespaces available and an unavailable/restricted negative case.
- Freeze the concrete development/package flag removals and runtime fail-closed checks for `task-103` without applying those production changes here.
- Define the unsupported-host contract: never auto-retry with `--no-sandbox`; retain ordinary wallet startup through an explicitly user-selected legacy escape hatch where practical; release configuration must not enable dApps when approved package evidence is absent or invalid, and runtime must reject dApp launch whenever sandbox-disabling CLI/environment state is present or the task-103 local sandbox canary fails.

## Non-Goals

- Do not remove `--disable-setuid-sandbox`, `--no-sandbox`, or the documented `ELECTRON_DISABLE_SANDBOX` escape hatch in this task. `task-103` owns production launch changes, runtime availability detection, and wallet-startup documentation.
- Do not implement `DappBrowserManager`, the dApp preload, guest session/network policy, route lease, feature switches, or any CIP-30 surface. Tasks `104` through `106-a` own those boundaries.
- Do not build the full hostile-renderer suite or claim proof for the future production guest. `task-107` must prove the implemented guest, `task-802` must run release-equivalent packaged adversarial validation, `task-807` must freeze the release-candidate package evidence, and `task-903-a` must revalidate material post-pilot changes.
- Do not adopt a root-installed helper, privileged installer, AppArmor installation step, distro package, container-only workaround, or automatic host-policy modification. Those would change the current package/deployment model and require separate approval.
- Do not add a preload or IPC for supplementary renderer assertions, and do not treat `sandbox: true`, startup success, a different Chromium utility process, or a development run as sufficient proof.
- Do not edit application runtime source, renderer code, IPC, backend, package dependencies/lockfiles, translations, Storybook, Cucumber tests, or either review log.

## Dependencies And Ownership

- `task-001`: completed; requires fail-closed production guest launch and packaged OS containment proof.
- `task-103`: consumes this task's selected strategy, removes default bypass flags, detects bypass configuration, and implements unsupported-host dApp unavailability while documenting wallet fallback.
- `task-104` and `task-105`: later produce the dedicated preload and actual guest whose critical `webPreferences` the probe mirrors only for phase-0 feasibility.
- `task-107`: proves the implemented guest and hostile boundary, including active Linux OS sandboxing, with the same-PID evidence contract.
- `task-802`: runs release-equivalent packaged adversarial validation, including Linux sandbox proof, on the actual supported-platform artifacts.
- `task-807`: records the audited release-candidate source, dependencies, package hashes, launcher variants, and linked packaged evidence; a material package change reopens affected certification.
- `task-903-a`: compares post-pilot artifacts with the task-807 baseline and reruns affected packaged/security scopes for every material delta before deployment.
- Release engineering owns the supported Linux matrix and artifact identity. A missing authoritative matrix is a completion blocker, not permission to invent product support.

## Research, Docs, Workflows, And Skills Consulted

- `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/plans/readme.md`, the full dApp-browser PRD and task graph, and the empty task-005 review logs.
- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`: assigns packaged Chromium OS sandbox evidence to task-005 and implementation to task-103. The other existing research notes were checked for sandbox/package coupling and add no task-005 constraints.
- `.agent/plans/dapp-browser-cip30/prompt.md`: task-plan fields, manual interaction policy, release-gate invariants, review-log ownership, and iteration-stable wording.
- `.agent/workflows/electron.md`, `.agent/workflows/nix.md`, `.agent/workflows/build.md`, `.agent/workflows/test.md`, and `.agent/workflows/update-doc.md` in the required order.
- Electron Process Sandboxing documentation: renderer sandboxing is disabled by Node integration; `--no-sandbox` disables Chromium's process sandbox globally and is testing-only.
- Chromium/Linux and Ubuntu user-namespace guidance: user namespaces avoid a setuid helper, while Ubuntu AppArmor may restrict an unprofiled home-installed executable and therefore requires explicit matrix evidence.
- `understand` was loaded first where applicable. Its full graph workflow would create files outside the permitted canonical-plan path, so no graph was generated; all material findings were verified against live files and history.
- No Cardano, CBOR, hardware-wallet, frontend, i18n, Storybook, or operator transaction skill applies to this packaging validation.

## Verified Live Findings

- `source/main/webpack.config.js` starts development Electron with `--disable-setuid-sandbox --no-sandbox`.
- `nix/internal/x86_64-linux.nix` packages `daedalus-frontend` with the same two switches. Commit `bfaaf79f7` added the development flags specifically to match production; commit `d17ba5061` introduced the portable home-directory package with the production bypasses; later relocatable-Electron work preserved them.
- The Linux package copies nixpkgs' Electron assets into a relocatable bundle, embeds and later repairs the ELF interpreter, and launches the installed binary through `libexec/electron`. The current evaluated `relocatableElectron` contains `chrome-sandbox` as root-owned mode `0555`, not a usable mode-`4755` helper.
- `linux-self-extracting-archive.sh` extracts to `$HOME/.daedalus/<cluster>` as the invoking user and does not perform privileged installation. A SUID-helper strategy is therefore incompatible with the present artifact unless the deployment model changes.
- `README.md` currently advises `ELECTRON_DISABLE_SANDBOX=true` for Chrome sandbox errors without warning that future dApp launch must remain disabled. Task-103 must preserve any explicit wallet fallback only as a clearly unsandboxed, dApp-disabled mode.
- This Ubuntu 24.04 planning host reports `kernel.unprivileged_userns_clone=1`, a nonzero `user.max_user_namespaces`, and `kernel.apparmor_restrict_unprivileged_userns=1`; a basic user-namespace creation probe succeeds. That is useful local context, not cross-distribution or packaged-renderer proof.
- No dApp guest exists yet. The phase-0 probe must be labeled a package-strategy surrogate, and later tasks must not cite it as proof of the implemented guest.

## Expected Files

- `scripts/linux-chromium-sandbox-probe/main.cjs`
  - Minimal local Electron fixture, exact renderer PID correlation, `/proc` collection, deterministic redacted JSON output, timeout, and nonzero failure exit. It must not load remote content or application IPC.
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
  - Strategy decision, immutable source/artifact identities, host matrix, exact commands, normalized evidence, supported/unsupported outcomes, task-103 change contract, limitations, and manual evidence provenance.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - After review, add only a concise pointer/result and retain production-launch-disabled wording.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - After manual evidence and implementation review approval, add truthful task-005 completion metadata without changing task-103 ownership.
- `.agent/plans/dapp-browser-cip30/task-plans/task-005.md`
  - Lifecycle, manual handoff, evidence, review result, and final outcome updates.

Production edits to `nix/internal/x86_64-linux.nix`, `nix/internal/linux-self-extracting-archive.sh`, `source/main/webpack.config.js`, `source/main/index.ts`, and `README.md` are explicitly deferred to `task-103`. `package.json`, `yarn.lock`, `flake.nix`, and `flake.lock` are not expected to change.

## Smallest Implementation Approach

1. Freeze the strategy and evidence contract.

   - Select Chromium's unprivileged-user-namespace path for the existing self-extracting package. Record why the SUID helper is unavailable after user-owned home extraction and why automatic sandbox bypass is forbidden.
   - Obtain the release owner's supported distribution/version matrix before claiming completion. At minimum, the actual matrix must include one default userns-capable host and one deliberately unavailable/restricted case; Ubuntu AppArmor behavior must be represented if Ubuntu is supported.
   - Normalize each evidence record with schema version, source commit, proof-patch SHA-256 when applicable, installer SHA-256, Electron/Chromium versions, installed executable/helper/wrapper/probe SHA-256 values, distro/kernel/session, relevant sysctls/AppArmor state, normalized launch argv, process identities, allowed `/proc` fields, namespace links/maps, result, and stderr category/hash/byte count/sanitized excerpt. Exclude usernames, raw home/install/probe/profile paths, wallet data, URLs, environment values, and unrelated process details.
   - Before launch, resolve canonical real paths and assign `<INSTALL_ROOT>` to the installed artifact root, `<PROBE_ROOT>` to the probe directory, `<PROFILE_ROOT>` to its generated temporary Electron profile, and `<HOME>` to the disposable home. Normalize path separators and replace these roots longest-first on path-component boundaries before JSON or stderr leaves the host.
   - Preserve argv as an ordered JSON string array, not a reconstructed shell command. For each argument, replace exact paths, path prefixes, and `--switch=<path>` values with the root tokens while retaining switch names, ordering, basenames outside sensitive roots, and the separately recorded file/artifact hashes. Normalize renderer and main `/proc` references to `/proc/<RENDERER_PID>` and `/proc/<MAIN_PID>` in exported text.
   - Keep raw argv, environment, command lines, and stderr only in the disposable host's restricted evidence directory. Export stderr only as exit code, byte count, SHA-256 of the raw bytes, one precedence-ordered category (`success`, `namespace-denied`, `apparmor-denied`, `sandbox-init-failed`, `timeout`, `renderer-exited`, `evidence-invalid`, or `other`), and a sanitized excerpt: normalize CRLF to LF, apply root tokens, replace username occurrences with `<USER>`, replace URLs with `<URL>`, replace each remaining absolute path by first-occurrence order as `<PATH_1>`, `<PATH_2>` without exporting a reverse map, then retain the first 8192 UTF-8 bytes on a code-point boundary and append a fixed truncation marker when needed. Reject evidence export if a raw root, username, URL, environment assignment value, or non-allowlisted absolute path remains. Apply the same sanitizer to positive and negative runs before transfer or commit.

2. Add one bounded local probe.

   - Run from the installed artifact's `libexec/electron` wrapper with no sandbox-disabling argument or environment variable.
   - Create a hidden local `BrowserWindow` using `nodeIntegration: false`, `nodeIntegrationInWorker: false`, `nodeIntegrationInSubFrames: false`, `contextIsolation: true`, `sandbox: true`, `webSecurity: true`, `allowRunningInsecureContent: false`, `webviewTag: false`, and `plugins: false`. Load only a local data page and expose no application preload or IPC.
   - Capture the renderer OS PID from main with `webContents.getOSProcessId()` after load. Correlate it with `/proc/<pid>/cmdline` containing `--type=renderer`; reject a PID mismatch, early exit, utility/GPU process, forbidden switch, or missing evidence. The main process reads all proof directly from `/proc`; the local page has no preload, IPC, Electron object, or Node authority.
   - Require Linux evidence from that same PID: `NoNewPrivs: 1`, seccomp filter mode (`Seccomp: 2` with a nonzero filter count where the kernel reports it), zero effective capabilities, and user/PID/mount namespace identity/mapping consistent with the selected userns strategy. Compare namespace inodes with the main process and preserve only the raw allowlisted `/proc` fields for review rather than reducing them to one boolean.
   - Exit deterministically, clean the temporary profile, and fail on timeout. Do not probe filesystem escape or execute hostile remote code in this phase-0 task.

3. Build and statically inspect the package-equivalent proof variant.

   - Build `nix build -L .#installer-mainnet` or use the exact artifact supplied by release engineering. If current source still contains the deferred production bypasses, label the result a package-equivalent proof variant and record source revision, complete flag-removal patch SHA-256 and contents, flake-lock identity, artifact SHA-256, and Electron version. Do not call that variant the production or release-candidate artifact.
   - Extract/install only under a disposable test user's `HOME`; the installer replaces `$HOME/.daedalus/mainnet`, so never run it against a real wallet profile.
   - Record installed Electron/helper/wrapper hashes, ownership, modes, ELF interpreter, sanitized wrapper contents, and normalized final launch argv. Confirm that the proof variant has no `--no-sandbox`, `--disable-setuid-sandbox`, `ELECTRON_DISABLE_SANDBOX`, or equivalent process-wide bypass. A temporary proof build may remove flags without committing the production change, but its complete patch, patch hash, and artifact hash must be retained; task-103 must reproduce the reviewed change.

4. Execute the positive host matrix.

   - On each release-owner-confirmed supported host, install the same artifact, run the packaged wrapper against the probe, and retain only normalized JSON plus the stderr category/hash/byte count/sanitized excerpt outside the host.
   - Require exact-renderer OS evidence, not startup alone. A host passes only when all mandatory `/proc` assertions pass and no sandbox bypass is present.
   - Repeat at least once from the desktop-installed path, not only a Nix store path, so home extraction, interpreter repair, wrapper resolution, and permissions are covered.

5. Execute the unavailable-userns case in a disposable VM.

   - Snapshot the VM, disable or deny unprivileged user namespaces using that distribution's supported host policy, verify the denial independently, then launch the default proof artifact without bypasses.
   - Require a clear startup/probe failure and prove there is no automatic retry with `--no-sandbox`. Restore the VM policy or snapshot afterwards.
   - Separately record the task-103 contract: an explicit user-selected unsandboxed wallet fallback may keep legacy wallet functionality available, but runtime dApp availability must be false for CLI flags, Electron sandbox-disabling environment variables, or a failed/unsupported local sandbox canary. Independently, release configuration must remain disabled when approved package evidence is absent or invalid. Task-005 does not claim either gate is implemented.

6. Freeze task-103 and later revalidation obligations.

   - Name exact task-103 edits: remove both bypass switches from `ManageElectronProcessPlugin` and packaged `daedalus-frontend`; detect sandbox-disabling argv/environment before guest enablement; preserve no automatic fallback; update README with supported-host and explicit dApp-disabled fallback behavior; and add package smoke tests.
   - Require task-103 to implement the Linux host-viability gate before any remote URL is loaded. On the first dApp launch request in each app process, after bypass argv/environment rejection and before guest creation/navigation, create a hidden local-only canary `BrowserWindow` with the probe's sandbox-critical preferences, no preload or IPC, a fresh random nonpersistent session, and a bundled local/data document. Main obtains its exact renderer PID with `webContents.getOSProcessId()` and applies the same mandatory same-PID `/proc` assertions used by this probe. Destroy the canary, clear session storage, and release the session after success or failure; cache success only in memory for that app process and never persist it across launches.
   - A canary timeout, crash, PID/type mismatch, forbidden switch, missing `/proc` evidence, or failed no-new-privileges/seccomp/capability/namespace assertion sets dApp availability false and rejects the launch before any remote guest or request exists. It must not retry the canary or application with `--no-sandbox`, alter host policy, or offer an in-app bypass. An explicit legacy unsandboxed wallet launch remains dApp-disabled because argv/environment rejection occurs before the canary.
   - Require task-107 to apply the same evidence contract to the real guest `webContents`; task-802 to repeat it against release-equivalent supported-platform artifacts and adversarial fixtures; task-807 to bind the resulting evidence to the recorded release-candidate baseline; and task-903-a to rerun affected packaged and security scopes for material post-pilot deltas. Material changes to Electron/Chromium, nixpkgs, `nix-bundle-exe`, ELF interpreter repair, archive extraction, helper handling, launcher flags, canary/probe assertions, or supported distributions invalidate affected task-005 evidence and flow through those owners.

7. Synchronize only after evidence and review.

   - Record every host result, including valid unsupported outcomes, in research without broadening support claims.
   - Update the PRD and task tracker only after manual evidence and implementation review approval. Keep production guest launch disabled and task-103 pending.

## Acceptance Criteria

- One viable strategy is proved for the current self-extracting package: unprivileged user namespaces with no usable SUID-helper assumption and no process-wide sandbox bypass.
- The release-owner-confirmed Linux matrix is recorded with immutable artifact/source identities and includes both working-userns and unavailable/restricted-userns behavior. No unnamed or untested distribution is claimed supported.
- The packaged Electron wrapper launches a local sandbox surrogate from the installed home-directory artifact, and the evidence identifies the exact renderer through `webContents.getOSProcessId()` plus matching `--type=renderer` `/proc` data.
- The same exact renderer shows mandatory OS evidence: no-new-privileges, seccomp filter mode/filter presence, zero effective capabilities, and namespace identity/mapping consistent with the selected strategy; no renderer preload or IPC is added merely to collect a supplementary assertion.
- The installed helper ownership/mode and archive behavior prove why a root-owned mode-`4755` helper is not assumed.
- The unavailable-userns case fails without an automatic unsandboxed retry. The research freezes future behavior so an explicit legacy wallet escape hatch never enables dApp launch.
- The exact development and package flag removals, runtime bypass detections, pre-remote-content local canary with fail-closed/no-retry behavior, README changes, package smoke tests, and revalidation triggers owned by `task-103` are documented without implementing them early.
- The probe is local, bounded, deterministic, privacy-safe, and clearly labeled as phase-0 package feasibility rather than proof of the not-yet-implemented production guest. Exported argv, paths, and stderr use the fixed redaction contract while immutable file/artifact hashes preserve reproducibility.
- Production guest launch remains disabled; no guest, IPC, wallet, backend, or network-security implementation is introduced.

## Verification

- Parse/lint the probe with the repository's Node runtime and run focused Prettier/ESLint where applicable.
- Build or obtain the Linux self-extracting artifact; identify it truthfully as either the package-equivalent proof variant or later production artifact, and record `git rev-parse HEAD`, complete proof-patch hash when applicable, `flake.lock` hash, artifact SHA-256, and Electron/Chromium versions.
- Install under a disposable `HOME`; verify installed executable/helper/wrapper/probe hashes, `stat` ownership/modes, ELF interpreter, sanitized wrapper contents, and absence of sandbox-disabling argv/environment.
- Run the installed `libexec/electron` wrapper against the probe on every approved matrix host. Validate the normalized JSON schema and fixed `<INSTALL_ROOT>`/`<PROBE_ROOT>`/`<PROFILE_ROOT>`/`<HOME>` substitutions; preserve only the stderr category/hash/byte count/sanitized excerpt outside the host.
- For the renderer PID returned by `webContents.getOSProcessId()`, verify matching `/proc/<pid>/cmdline`, `NoNewPrivs`, `Seccomp`, `Seccomp_filters` where available, `CapEff`, user/PID/mount namespace links, and user/group maps. Compare namespace identities with the Electron main process.
- In a snapshotted disposable VM, deny unprivileged user namespaces, independently verify denial, run the default artifact, confirm failure/no retry/no bypass, and restore the host policy.
- Inspect the retained proof-build diff and confirm it changes only the two launch flag sites needed to exercise the strategy; production source remains for task-103.
- Run `git diff --check`, JSON validation for normalized evidence, deterministic-redaction golden tests covering argv plus positive/negative stderr, and a focused diff audit for accidental runtime/Nix/README/tracker changes, secrets, raw roots/usernames/URLs/environment values, unrelated process data, or review-log edits. Evidence export must fail if its residual-leak scan finds prohibited content.
- Re-read the latest planning and implementation review entries generically before lifecycle synchronization; canonical verification wording must not depend on a particular review iteration.
- Manual evidence is mandatory. If the authoritative supported distribution matrix or any required host result is absent, leave the task in progress and do not mark the packaged sandbox gate complete.

## Risks And Open Questions

- Supported distribution scope: current docs do not define one. Release engineering must supply it; testing an invented Ubuntu/Fedora/Debian sample cannot establish product support.
- Ubuntu AppArmor: a home-installed executable may lack a profile allowing user namespaces even when `kernel.unprivileged_userns_clone=1`. If a supported default Ubuntu host fails, the userns-only strategy is not viable for that host; do not disable AppArmor or silently bypass Chromium. Escalate deployment-model or support-scope changes.
- Chromium evidence stability: `/proc` fields and namespace topology can vary by kernel/Chromium strategy. Mandatory assertions must be source-backed and reviewed against Electron 41.3.0's Chromium version; unexplained variation blocks rather than being normalized away.
- Package drift: current relocatable Electron work patches the ELF interpreter and copies assets through several derivations. Any later packaging change may alter helper permissions, process spawning, or namespace behavior and must trigger revalidation.
- Legacy wallet fallback: Electron may fail before application code runs when user namespaces are unavailable. The only practical fallback may remain an explicit environment/CLI launch mode; task-103 must detect it before any future guest launch and must not advertise containment.
- Headless execution: Xvfb/Wayland differences can change utility processes. The exact renderer PID contract avoids confusing GPU/utility processes, but at least one installed desktop-path run remains required.
- Negative-host safety: host-wide sysctl/AppArmor changes can break applications. Run only in disposable snapshotted VMs and record rollback; never modify the user's primary workstation.
- Runtime canary limits: task-103's per-process canary detects current Linux host containment availability before remote content, but it does not replace package certification or prove the later guest's full hostile boundary. Task-107, task-802, task-807, and task-903-a retain those gates.

## Docs, Tracking, And Research Updates

- Add `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md` with strategy, source links/revisions, evidence/redaction schema, artifact and host results, operator commands, limitations, the task-103 pre-remote-content canary contract, and task-107/task-802/task-807/task-903-a revalidation triggers.
- After manual evidence and implementation review approval, add a concise task-005 result/pointer to the PRD and mark only task-005 complete in the tracker with truthful matrix/artifact notes.
- Do not update `.agent/system/architecture.md` yet: this task validates a future package strategy and ships no runtime architecture. Task-103 updates operational docs when behavior changes.
- Do not update Electron/Nix/build/test workflows unless execution proves a generally reusable command in them is stale. Record task-specific commands in research instead.
- Keep `task-103`, `task-107`, `task-802`, `task-807`, `task-903-a`, and all production guest release gates pending.
- Do not edit either review log.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope: production flag removal, runtime gating, README behavior changes, and real guest implementation remain with tasks 103-107. The only executable addition is a bounded package-feasibility probe.
- Workflow freshness: live `package.json` uses Nix installer outputs rather than the workflow's generic AppImage wording; verification follows `README.md` and live flake outputs instead of stale package generalization.
- Manifests/tests/docs: the plan includes a normalized evidence schema, probe verification, immutable artifact identity, research, PRD/tracker synchronization, and explicit non-updates for unrelated manifests and docs.
- Security drift: remote content is never loaded; no preload/IPC is added for an unnecessary renderer assertion; exact renderer PID correlation and same-PID OS evidence are mandatory; task-103's canary runs before remote content and cannot retry unsandboxed; production guest launch remains disabled.
- Privacy consistency: raw paths, argv, environment, command lines, and stderr stay on disposable hosts; deterministic root tokens, ordered argv, hashes, categorized/sanitized stderr, and a fail-closed residual-leak scan provide reproducible export evidence without personal paths.
- Package drift: SUID-helper assumptions are rejected for the home installer, Ubuntu AppArmor and unavailable-userns outcomes block rather than weaken containment, and package/Electron changes trigger revalidation through task-107, task-802, task-807, and task-903-a.
- Interaction truthfulness: release-owner matrix input and disposable-host execution are explicit manual blockers. Agent work may proceed first, but completion cannot be inferred from this single planning host.
- Consistency: task-005 proves strategy/feasibility and freezes task-103 work without claiming the future runtime gate or real dApp guest is implemented.

## Planning Status

- `approved`

## Build Status

- `in_progress`

## Current Outcome

- Planning was approved for a portable userns-only candidate. Agent-executable work added the local exact-renderer probe and research `05` evidence schema.
- Portable package-equivalent Ubuntu 24.04 run: Electron exited `132`/`SIGILL` during sandbox bootstrap; `--no-sandbox` control reached a renderer and was correctly rejected by the probe. Negative evidence only.
- **Product decision 2026-08-12:** Linux ships **`.deb` and `.rpm` only**. Portable `.bin`, AppImage, Flatpak, and Snap are **rejected**. Durable record: `research/06-linux-system-package-decision.md`. PRD Linux sandbox section and tasks `005`/`103`/`108`/`109`/`110` updated.
- Task-005 acceptance is **not** met until exact-renderer OS sandbox proof exists on installed `.deb` and `.rpm` artifacts. Canonical plan must be replanned for the system-package strategy before the next implementation loop.
- Production guest launch remains disabled. Production launch sites still contain sandbox bypasses until task-103.
