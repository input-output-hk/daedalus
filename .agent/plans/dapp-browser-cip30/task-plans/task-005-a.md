# Task task-005-a: Freeze Linux .deb/.rpm sandbox packaging contract

## Task

- Task ID: `task-005-a`
- Title: `Freeze Linux .deb/.rpm sandbox packaging contract`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`
- Tracker state at planning: `pending`

## Why This Task Was Chosen Now

- `task-001` makes active packaged Chromium OS sandboxing a release gate for hostile remote content.
- Historical `task-005` is cancelled after the portable package failed closed on Ubuntu 24.04 and product/release selected system `.deb` and `.rpm` packages.
- `task-108` and `task-109` cannot implement consistent packages until one exact install, post-install, launcher, host-policy, and probe contract is frozen.
- The current probe still rejects a usable root-owned SUID helper because it implements the superseded portable contract. The development and legacy portable launchers still pass `--disable-setuid-sandbox --no-sandbox`; those removals are downstream work, not this task.

## Interaction Mode

- Mode: `interactive_decision`.
- The actual release-owner-authoritative x86_64 Linux distribution/version support matrix was supplied by the user acting as release/product authority and approved as revision `task-005-a-matrix-2026-08-14`. This conversation and its durable repository record are the approval record; the authority confirmed that no separate reviewer is required.
- An owner name and promised delivery date are handoff metadata only. They do not satisfy this task's acceptance or completion gate.
- The interactive decision is resolved and build lifecycle is `in_progress`.

## Required User Input And Evidence

Release/product engineering must return a durable, reviewed matrix containing:

1. A revision identifier and durable repository path or review record for the matrix.
2. The release owner/authority and the reviewer or approval record for that revision.
3. One row for every supported x86_64 Linux distribution and exact version, with package family (`.deb` or `.rpm`) and support state.
4. For every row, the accepted sandbox prerequisite path: root-owned SUID helper, unprivileged user namespaces, or both; any required AppArmor profile/ABI behavior or SELinux mode/file-context/policy behavior; and the expected fail-closed result when prerequisites are absent.
5. An explicit statement that omitted or unapproved distro/version rows are unsupported for dApp launch and are not silently covered by a family name such as "Debian-class" or "RPM-family".

No package installation or host-policy test result is required to approve this planning task. Disposable-host execution and installed-artifact evidence belong to `task-005-b` after `task-108` and `task-109` produce packages.

### Approved Matrix Revision

| Distribution/version | Package | Accepted routes | Required policy |
|---|---|---|---|
| Ubuntu 22.04.x LTS | `.deb` | independently proven SUID or userns | AppArmor |
| Ubuntu 24.04.x LTS | `.deb` | independently proven SUID or userns | AppArmor |
| Ubuntu 26.04.x LTS | `.deb` | independently proven SUID or userns | AppArmor |
| Debian 12.x | `.deb` | independently proven SUID or userns | none by default |
| Debian 13.x | `.deb` | independently proven SUID or userns | none by default |
| Fedora 43 | `.rpm` | independently proven SUID or userns | SELinux |

- No Ubuntu interim version is currently supported. Each future interim release must be added by an exact reviewed matrix revision and certified before dApp enablement.
- Fedora 42 and openSUSE Leap 15.6 are excluded as EOL for this baseline. Every other omitted row is wallet-only and dApp-disabled.
- A mode-`4755` helper is not mandatory when the reviewed userns route passes; userns is not mandatory when the reviewed SUID route passes. Both routes still require the common exact-renderer containment checks.

## Scope

- Freeze system `.deb` and `.rpm` as the only Linux product package formats.
- Freeze `/opt/daedalus/<cluster>` as `INSTALL_ROOT`, where `<cluster>` is the build-time installer cluster slug.
- Freeze the package-visible executable and policy-asset paths, ownership, and modes below; the matrix selects among predefined mechanism/policy classes and cannot redesign them.
- Freeze exact `.deb` `postinst` and `.rpm` `%post` responsibilities for ownership/mode, AppArmor/SELinux integration, idempotent install/upgrade behavior, nonzero failure behavior, and prohibition of network fetches or host-wide containment weakening.
- Freeze flag-free task-108/109 desktop entries and launcher/wrapper behavior.
- Adapt the local probe contract from the rejected portable-only helper assertion to approved SUID, userns, and combined system-package rows while retaining exact-renderer same-PID OS evidence and deterministic privacy controls.
- Freeze unsupported-host and sandbox-disabling behavior as fail closed with no automatic unsandboxed retry.
- Synchronize the PRD, task tracker, research 05/06, and canonical task outcome without claiming package implementation or certification.

## Non-Goals

- Do not build, install, publish, sign, or certify `.deb` or `.rpm` artifacts.
- Do not add Nix package outputs, maintainer scripts, desktop files, SELinux policy modules, or AppArmor profiles; those are task-108/109 implementation.
- Do not execute the support matrix or restricted-host certification; that is task-005-b.
- Do not remove current development or legacy portable sandbox flags, add the runtime canary, or implement runtime availability enforcement; that is task-103 after certification.
- Do not retire `.bin`, redesign auto-update, or change wallet-data migration; that is task-110.
- Do not create a dApp guest, load remote content, enable connector APIs, or weaken any production release gate.
- Do not broaden this task into Windows/macOS sandboxing, general IPC hardening, renderer migration, or package-manager policy design.

## Dependencies And Downstream Ownership

- Required dependency: completed `task-001` threat model and ADR.
- Historical input only: cancelled `task-005` and its negative portable evidence.
- `task-108` owns `.deb` implementation, its idempotent privileged `postinst`, AppArmor assets/actions, fixed layout, and flag-free launchers.
- `task-109` owns `.rpm` implementation, its idempotent privileged `%post`, SELinux-compatible files/actions, fixed layout, and flag-free launchers.
- `task-005-b` owns installed-artifact hashes, exact matrix execution, positive SUID/userns/AppArmor/SELinux evidence, restricted-prerequisite negative cases, rollback evidence, and certification.
- `task-103` owns only remaining development/legacy bypass removal plus argv/environment rejection and the pre-remote-content local canary after task-005-b.
- `task-110` owns portable-package retirement, update migration, and user-facing package migration documentation.
- `task-107`, `task-802`, `task-807`, and `task-903-a` retain later real-guest, platform, release-candidate, and post-pilot proof.

## Frozen Contract To Record

### Layout And Launchers

- `INSTALL_ROOT` is exactly `/opt/daedalus/<cluster>` and is root-managed. Package files must not be installed below a user's home or wallet-data directory.
- The exact package paths are `/opt/daedalus/<cluster>/bin/daedalus`, `/opt/daedalus/<cluster>/libexec/daedalus-frontend`, `/opt/daedalus/<cluster>/libexec/electron`, `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/electron`, adjacent `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/chrome-sandbox`, and `/opt/daedalus/<cluster>/share/daedalus-sandbox-identity.json`. The identity manifest binds the matrix revision, cluster, exact package hashes, and reviewed policy identity. This matches the live bundle; tasks 108/109 must not introduce aliases or alternate roots.
- Directories and the launcher, frontend, wrapper, and resolved Electron executable are package-owned `0:0` and mode `0755`. `chrome-sandbox` is always an exact-path regular non-symlink owned `0:0`: mode `4755` for a SUID-capable row and mode `0755` for a non-SUID userns row. These modes are predefined; the matrix selects a row, not a new mode.
- The `.deb` AppArmor asset is exactly `/etc/apparmor.d/opt.daedalus.<cluster>.electron`, a regular non-symlink owned `0:0` mode `0644`; its profile identity/attachment is the exact resolved Electron path above, grants only the reviewed `userns,` capability, and records the reviewed parser version in the identity manifest. The `.rpm` SELinux policy asset is exactly `/usr/share/selinux/packages/daedalus-<cluster>.cil`, a regular non-symlink owned `0:0` mode `0644`; task-109 freezes its reviewed process label, module identity, and exact resolved Electron/helper file contexts in the identity manifest rather than inheriting invented generic type names. Active policy-store locations remain OS-managed and are not package-owned paths.
- Wallet/application state remains under `XDG_DATA_HOME/Daedalus`; install, upgrade, removal, or post-install sandbox setup must not inspect, move, chmod, or delete wallet state.
- Every package-provided desktop entry, launcher, wrapper, restart path, and post-update entry point is flag-free: no `--no-sandbox`, `--disable-setuid-sandbox`, or equivalent Chromium sandbox bypass.

### Task-108 `.deb` Responsibilities

- Install the fixed tree and sandbox assets as root without a network fetch.
- Establish and verify the predefined exact-path helper ownership/mode contract selected by the matrix: `0:0`/`4755` for SUID-capable and `0:0`/`0755` for non-SUID userns.
- Install a package-owned AppArmor profile only on matrix rows requiring it, attach it to the exact Electron executable path, grant only the reviewed `userns,` capability, and load/reload it only when the matrix-approved parser ABI accepts it.
- Define idempotent install/upgrade behavior. On a supported matrix row, fail package configuration nonzero only when neither approved route or its mandatory AppArmor invariant can be established. A passing userns route may use mode `0755`; a passing SUID route may proceed with userns unavailable. Do not disable AppArmor, alter global userns policy, or retry Electron unsandboxed.

### Task-109 `.rpm` Responsibilities

- Use the same fixed application layout and helper/launcher invariants as `.deb`.
- Establish and verify the predefined exact-path `0:0`/`4755` or `0:0`/`0755` helper mode in `%post` without a network fetch.
- Package the exact frozen SELinux asset/module and matrix-reviewed file contexts for the resolved Electron/helper paths. Do not disable SELinux, switch enforcement mode, add a broad permissive domain, or suppress denials.
- Define idempotent install/upgrade behavior. On Fedora 43, fail package configuration nonzero only when neither approved route or its mandatory SELinux invariant can be established. A passing userns route may use mode `0755`; a passing SUID route may proceed with userns unavailable. Do not retry Electron unsandboxed.

### Probe And Evidence Behavior

- Main identifies the created renderer only with `webContents.getOSProcessId()`; the recorded `--type=renderer` or reviewed Electron 41 `--type=zygote` argv is supporting evidence, not renderer authority.
- Every accepted class has common same-renderer predicates: exact WebContents-to-OS-PID correlation; no forbidden argv or `ELECTRON_DISABLE_SANDBOX`; `NoNewPrivs: 1`; `Seccomp: 2`; positive `Seccomp_filters` when exposed; zero `CapEff`; expected package/runtime hashes and exact paths; and no timeout, crash, retry, or host-policy mutation.
- A `userns-only` row additionally requires independent host userns availability, renderer user/PID/mount namespace relationships distinct from Electron main, renderer UID/GID maps distinct from main, the exact helper at `0:0`/`0755`, and any matrix-required same-PID AppArmor proof.
- A `suid-only` row additionally requires independent proof that unprivileged userns is unavailable, the exact regular non-symlink helper at `0:0`/`4755`, and renderer PID/mount namespace relationships distinct from Electron main. It does not require a distinct renderer user namespace or UID/GID maps; the unavailable-userns result plus exact helper/bootstrap invariants and common passing containment are the reviewed SUID predicate.
- A `combined-unattributed` row requires both independently available userns prerequisites and the exact `0:0`/`4755` helper plus all common predicates and required policy proof. Namespace/map observations are recorded, but success is not attributed to SUID or userns unless task-005-b runs an isolation case; helper presence alone never establishes selection.
- `process.sandboxed`, `sandbox: true`, successful startup, a utility/zygote PID without WebContents correlation, or helper metadata alone is never accepted as proof.
- The probe must reject known bypass argv and `ELECTRON_DISABLE_SANDBOX`, must never retry, and must remain local-only with no preload, IPC, Node integration, persistent session, wallet profile, dApp, or remote URL.
- Evidence records the expected matrix class separately from observed facts. AppArmor proof requires the exact renderer's `/proc/<renderer-pid>/attr/current` (or a reviewed equivalent authoritative same-process label), the exact profile identity/attachment, package profile hash, enablement, and parser ABI/result. SELinux proof requires the same exact-renderer process label, normalized enforcement state, exact Electron and helper file contexts, module identity, and package policy hash. A label on another process or file-context metadata without the same renderer is non-passing.
- Missing policy evidence, helper mismatch, symlink/path escape, timeout, native failure, missing renderer evidence, failed same-PID assertion, or unsupported matrix row is a non-passing result.
- Raw argv, paths, environment, numeric PIDs, namespace inode IDs, UID/GID maps, numeric file ownership, stderr, audit output, usernames, and hostnames remain in a mode-`0700` disposable-host directory. Export is one versioned allowlisted schema containing only: matrix revision/row and package family; expected class and observed result; manifest-bound artifact/runtime hashes, root/non-root ownership classifications, and root-tokenized exact paths; ordered sanitized argv; `<MAIN_PID>`/`<RENDERER_PID>` tokens and PID relationships; per-user/PID/mount-namespace `same-as-main` booleans with no inode IDs; UID/GID-map `same-as-main` and `identity-or-remapped` classifications with no numeric map; common and class-specific assertion booleans; distribution ID/version, `x86_64`, allowlisted kernel release, and session type with no hostname; allowlisted AppArmor/SELinux enablement, parser/enforcement, exact-renderer label match, exact-file context matches, identities, and hashes; and stderr/audit category, raw-byte count/hash, truncation flag, and bounded sanitized excerpt.
- Root substitution is longest-first using `<INSTALL_ROOT>`, `<PROBE_ROOT>`, `<PROFILE_ROOT>`, and `<HOME>`. Process/policy labels and audit-derived excerpts receive the same path/identity/environment sanitization and residual-leak rejection as argv/stderr. Export contains relationships instead of unnecessary host identifiers, no unrelated `/proc` or policy fields, and no reverse token map.
- Task-005-b must run installed artifacts on every exact approved row and, for every distinct accepted mechanism or required AppArmor/SELinux policy class, a snapshotted denied-prerequisite case. Combined rows require an all-prerequisites-denied case and may claim route attribution only from separately reviewed isolation runs. Every negative run proves package/dApp refusal as applicable, no unsandboxed retry or host-policy mutation, and rollback.

### Unsupported Hosts And Runtime Boundary

- A listed supported row where neither approved SUID nor userns route, or its mandatory AppArmor/SELinux policy, can be established is a package-configuration refusal: maintainer setup exits nonzero and never falls back to weakened ownership, mode, profile, SELinux, or launcher flags.
- A distro/version omitted from or explicitly unsupported by the reviewed matrix is not a failed supported setup. The system package may install only as a wallet-only package without applying unapproved host-policy changes; dApp capability is marked unavailable and no remote guest can launch. It is never silently promoted by a family label. This preserves the PRD's wallet-availability-where-practical contract while refusing dApp support.
- Until task-103 exists, production guest launch remains disabled everywhere. After task-103, runtime enforcement consumes the frozen supported/unsupported result: sandbox-disabling argv/environment, a missing/stale package evidence gate, an unsupported host, or canary failure keeps dApp launch unavailable. Task-103 does not redefine package refusal or wallet-only eligibility.

## Expected Changes

- `scripts/linux-chromium-sandbox-probe/main.cjs`
  - After the matrix checkpoint, replace the portable-only `unexpected-usable-setuid-helper`/`noUsableSetuidHelper` contract with the frozen common, userns-only, SUID-only, and combined-unattributed predicates and normalized same-renderer AppArmor/SELinux evidence fields.
  - Preserve the existing exact-renderer `/proc`, timeout, cleanup, forbidden-switch, local-only, and privacy guarantees.
  - Extend the dependency-free in-script fixtures for SUID-only, userns-only, combined-unattributed, required AppArmor, required SELinux, unsupported row, helper owner/mode/symlink failure, process-label mismatch, all-prerequisites-denied, normalized PID/namespace/map relationships, policy/audit allowlisting, and residual privacy rejection. Do not add package installation or host-policy mutation.
- `.agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md`
  - Replace the non-authoritative "minimum product intent" with the returned authoritative exact matrix, reviewed revision, exact layout paths, and concrete task-108/109 post-install contracts.
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
  - Preserve portable results as historical negative evidence; replace provisional portable procedures/assertions with the frozen system-package probe/evidence contract and clearly defer execution results to task-005-b.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - Mirror the authoritative support matrix reference, exact fixed-path contract, fail-closed unsupported-host behavior, and downstream ownership at the release-gate level without adding implementation claims.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - Keep `task-005-a` pending until all acceptance evidence exists; on truthful completion, update only its lifecycle/completion notes and any synchronized contract wording. Do not advance tasks 108/109/005-b/103 or alter dependencies without evidence.
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a.md`
  - Track the approved plan, user input, implementation state, verification, and final evidence.
- No architecture, workflow, README, Nix/package, launcher, application source, or review-log content is expected to change unless implementation discovers a direct current-contract inconsistency. Package/workflow migration documentation remains task-108/110 scope.

## Implementation Approach

1. Stop at the user checkpoint and obtain the complete reviewed matrix revision; do not substitute inferred distro families, the local Ubuntu result, or an owner/date promise.
2. Normalize the returned rows into one canonical matrix in research 06 and cross-reference it from the PRD, research 05, tracker, and this plan.
3. Apply the already frozen paths, ownership, modes, policy asset identities, supported-row package-refusal rule, and omitted-row wallet-only rule to each matrix selection; do not let a row redesign those contracts.
4. Make the smallest probe change that removes the portable-only SUID rejection, applies the reviewed class-specific predicates and normalized export schema, and does not pretend to detect which Chromium bootstrap path a combined row selected.
5. Update historical/provisional research wording without rewriting the portable result or claiming package certification.
6. Run focused static, self-test, JSON, documentation-consistency, privacy, and scope checks.
7. Submit the contract for implementation review. Keep tracker/build completion blocked unless the actual matrix revision is present and every acceptance item is met.

## Acceptance Criteria

- The actual release-owner-authoritative x86_64 distro/version matrix and its reviewed revision are recorded durably. Owner/date alone, broad distro-family labels, and the historical Ubuntu diagnostic do not satisfy this criterion.
- `.deb`/`.rpm` only, `/opt/daedalus/<cluster>`, every exact launcher/wrapper/Electron/helper path, the AppArmor/SELinux asset identities, root ownership, and SUID/non-SUID modes are consistent across the PRD, tracker, research 05/06, and canonical plan before the matrix selects predefined rows.
- Task-108 and task-109 have concrete, internally consistent, idempotent, no-network post-install responsibilities for the frozen helper ownership/modes, policy assets, supported-row nonzero package refusal, omitted-row wallet-only behavior, and flag-free launchers.
- The exact-renderer probe/evidence contract uses reviewed common, userns-only, SUID-only, and combined-unattributed predicates; binds required process labels and exact-file contexts to the authoritative renderer and package files; and never treats `process.sandboxed` or helper metadata alone as proof.
- Evidence export conforms to the frozen allowlisted PID/root/namespace/map/host/kernel/process-label/file-context/policy/audit schema with deterministic tokenization and residual-leak rejection, while raw host data remains local to disposable hosts.
- Dependency-free fixtures cover every accepted mechanism/policy class and named failure branch; task-005-b owns every authoritative positive row and a denied case for every distinct accepted mechanism/policy class.
- Supported-row setup failure refuses package configuration; omitted/unsupported rows are wallet-only and dApp-disabled. Both remain fail closed with no automatic unsandboxed retry or host-policy weakening.
- Ownership remains contract (`task-005-a`) -> package implementation (`task-108/109`) -> installed certification (`task-005-b`) -> remaining bypass/runtime enforcement (`task-103`), with migration and later release gates unchanged.
- No package is implemented or certified, no current flags are removed, and no production guest or remote content is enabled.

## Verification Plan

- Run `node scripts/linux-chromium-sandbox-probe/main.cjs --self-test`.
- Run `node --check scripts/linux-chromium-sandbox-probe/main.cjs`.
- Run the repository's focused Prettier check on every changed Markdown, JSON, and JavaScript file.
- Parse `dapp-browser-cip30-tasks.json` and validate unique task IDs, resolvable dependencies, acyclicity, and unchanged task accounting unless an intentional synchronized tracker edit requires recalculation.
- Search current non-log docs for stale portable support, task-005 ownership of current system-package proof, task-103 ownership of package launcher flags, placeholder matrix rows, generic `/opt/...` paths, and any owner/date-as-completion wording.
- Search the probe and package surfaces to confirm the portable-only SUID rejection is gone from the current probe contract while current production/development bypasses remain unchanged and explicitly downstream.
- Review dependency-free exported-evidence fixtures for SUID-only, userns-only, combined-unattributed, required AppArmor, required SELinux, unsupported row, helper owner/mode/symlink failure, process-label mismatch, and all-prerequisites-denied results.
- Validate fixture schemas for PID/root tokens, namespace/map relationships without raw IDs, allowlisted host/kernel fields, exact-renderer process labels, exact-file contexts, bounded policy/audit excerpts, and residual sensitive content rejection.
- Review task-005-b handoff coverage to ensure every authoritative row plus one denied case for every accepted mechanism/policy class is mandatory, including rollback and no-retry/no-policy-mutation assertions.
- Run `git diff --check` and inspect the complete task diff for package/runtime/guest scope creep.
- Do not run installed-artifact or host-policy certification under this task. Absence of those results is expected and must not be represented as a verification gap for this contract-only scope.

## Risks And Open Questions

- The authoritative matrix input is resolved by revision `task-005-a-matrix-2026-08-14`; installed-artifact proof remains intentionally downstream in task-005-b.
- Exact AppArmor parser ABI and SELinux label values may differ by matrix row. The paths, assets, ownership, modes, attachment/module identities, evidence schema, and failure behavior are already frozen; only reviewed row-specific ABI/label selections remain matrix input. Do not invent generic commands or weaken host policy to make a row pass.
- Helper presence does not prove whether Chromium selected SUID or userns. Keep observed containment, prerequisites, and inferred mechanism labels separate.
- The frozen executable paths match the current live bundle. Any later bundle-shape change is a contract re-review trigger before task-108/109 may consume it; do not introduce compatibility aliases silently.
- The broad build and Nix workflows still describe historical Linux outputs. Updating those docs before packages and migration exist would be stale; task-108/110 own that correction.

## Research And Tracking Updates

- Research 05 remains the durable negative portable evidence and gains only the current system-package probe/evidence contract.
- Research 06 becomes the canonical system-package strategy, exact support-matrix revision, layout, and post-install contract record.
- The PRD and tasks JSON mirror release-gate and ownership facts rather than duplicating unreviewed operational guesses.
- On final completion, record durable findings and final evidence in this plan and synchronized research/tracking. If implementation produces no additional durable finding beyond the frozen contract, record `no new research` in the final outcome.

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/electron.md`
- `.agent/workflows/nix.md`
- `.agent/workflows/build.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `research/01-hostile-renderer-threat-model-traceability.md`
- `research/05-linux-chromium-sandbox-packaging.md`
- `research/06-linux-system-package-decision.md`
- Historical `task-plans/task-005.md`, `task-005-plan-review.md`, and `task-005-impl-review.md`
- Full current `task-005-a-plan-review.md` and empty fresh `task-005-a-impl-review.md`
- `understand` skill loaded first for repository-understanding guidance. No existing knowledge graph was present; material conclusions were verified against the live probe, Linux Nix packaging surface, webpack launch surface, PRD, tracker, and research rather than generating a broad new graph for this bounded documentation task.
- `understand-diff` was consulted after implementation review; no knowledge graph existed, so the fix pass used the complete live diff and direct file verification instead of a stale overlay.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-a-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-005-a-impl-review.md`
- Both logs are Orchestrator-owned append-only transcripts and are not modified by this plan.

## Implementation Evidence

- User feedback approved matrix revision `task-005-a-matrix-2026-08-14`, the six exact version-series rows, future-interim certification, EOL exclusions, either-route SUID/userns semantics, Ubuntu AppArmor, Fedora 43 SELinux, Debian no-policy default, omitted-row wallet-only behavior, and this repository record as the complete authority/approval evidence.
- Probe schema version 2 pins that revision, independently verifies `/etc/os-release` and `unshare -Ur`, rejects unsupported rows, validates every exact package path/directory/ownership/mode/hash against a root-owned identity manifest, applies common and mechanism-specific exact-renderer checks, binds immutable `/proc` process start time across collection with an event-loop lifecycle check, and independently queries exact-renderer AppArmor/SELinux plus manifest-matched parser/file/module state and non-loading parse acceptance of the exact AppArmor asset.
- Export replaces raw PIDs, PID-bearing argv values, namespace inode IDs, UID/GID maps, and numeric file ownership with tokens, relationships, classifications, and root/non-root values; policy identities remain reviewable after safe normalization. Exactly one schema-v2 success or failure object is emitted after cleanup, with bounded sanitization and residual-leak rejection.
- Dependency-free fixtures cover the exact matrix and version boundaries, stale/unsupported rows, independently observed userns states, userns/SUID/combined and all-routes-denied branches, helper/package ownership/mode/symlink/hash failures, normalized process evidence, exact AppArmor/SELinux label/context failures, final-object privacy, forbidden switches, and timeout behavior.
- Verification passed: probe self-test, Node syntax check, focused Prettier, JSON task parsing, dependency/acyclicity validation, and `git diff --check`.
- Installed package and host-policy certification was not run and remains task-005-b scope. No package, launcher, runtime guest, or production feature gate was enabled.

## Lifecycle Status

- Planning status: `approved`
- Build status: `completed`
- Current outcome: contract implementation, agent-executable verification, and implementation review approved; tracker/research/docs synchronized. Installed-artifact certification remains task-005-b and production guest launch remains disabled.
- Completion rule: do not mark planning approved until Critiquer approval is appended; do not mark build completed, tracker completed, or any sandbox release gate passed until the matrix is present, implementation review approves the frozen contract, documentation/tracking is synchronized, and final task signoff is complete.

## Final Outcome

- Acceptance criteria are satisfied for this contract-only task. The latest Code Review entry is `Decision: approved` with no blocking findings.
- The approved matrix, package/post-install contract, identity-manifest handoff, probe schema, evidence privacy rules, unsupported-host behavior, and downstream ownership are synchronized across code, PRD, tracker, architecture, and research 05/06.
- Agent-executable verification passed. Installed `.deb`/`.rpm` execution, denied-prerequisite VM runs, rollback evidence, and release certification intentionally remain task-005-b after tasks 108/109 produce packages.
- User feedback was required and incorporated as the authoritative product/release matrix decision; no further task-005-a user checkpoint remains.
- Production guest launch remains disabled, existing sandbox bypasses remain unchanged for their downstream owners, and no package or host policy was implemented by this task.
- Durable research was added to research 05/06; this task does not record `no new research`.

## Planner Self-Review

- Scope creep: package creation, host certification, bypass removal, runtime canary, migration, and guest enablement are explicitly downstream.
- Lifecycle: the interactive decision is resolved by the approved matrix revision and build status is `completed` after Code Review approval.
- Frozen package contract: live bundle paths, policy asset/identity paths, root ownership, `0755`/`4755` helper modes, supported-row package refusal, and omitted-row wallet-only behavior no longer depend on matrix design.
- Evidence predicates: common checks are separated from userns-only, SUID-only, and combined-unattributed checks; AppArmor/SELinux proof binds the exact renderer PID to exact package files.
- Privacy and tests: the allowlisted normalized schema covers PID/root tokens, namespace/map relationships, host/kernel, labels/contexts, and bounded policy/audit excerpts; dependency-free fixtures and downstream denied cases cover every class and failure branch.
- Stale wording: portable results remain historical; current ownership and exact `/opt/daedalus/<cluster>` language supersede provisional task-005 wording without rewriting logs.
- Security boundaries: no host-policy weakening, no unsandboxed retry, disabled production guests, actual reviewed matrix requirement, and downstream task ownership are preserved.
- Complexity: the plan reuses the existing dependency-free probe and two research records, adds no package framework or runtime service, and leaves commands, package mechanics, policy implementation, and host execution to tasks 108/109/005-b/103.
