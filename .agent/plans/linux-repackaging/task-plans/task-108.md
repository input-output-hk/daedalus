# Task task-108: Implement Linux .deb system package with sandbox postinst

## Task

- Task ID: `task-108`
- Title: `Implement Linux .deb system package with sandbox postinst`
- Phase: `phase-1` (`Electron And IPC Security Foundation`)
- Priority: `critical`
- Tracker state at planning: `pending`

## Why This Task Was Chosen Now

- Linux still exposes only the portable self-extracting `.bin`; the reusable bundle launches Electron through `PATH`, passes `--disable-setuid-sandbox --no-sandbox`, can restart `$HOME/.daedalus/<cluster>/bin/daedalus`, and remains connected to an updater that chmods downloaded content `0777` and executes it.
- `task-005-a` froze the system-package layout and matrix, but authoritative Canonical material now contradicts two frozen AppArmor assumptions. Ubuntu's Chromium exception is an exact-path `flags=(default_allow)` profile with `userns,`, not an ordinary enforcing profile containing only `userns,`; Canonical also states that restricted unprivileged user namespaces were introduced after Ubuntu 22.04 and do not affect prior releases through an HWE kernel alone.
- The live schema-v2 probe compounds that contradiction by requiring an exact parser patch version and `<electron-path> (enforce)`. Those predicates cannot govern package implementation until the task-005-a contract, research 05/06, PRD/tracker wording, and probe are corrected through the existing fail-closed authority process.
- The user-approved planning escalation now supplies that narrow correction: Ubuntu 22.04.x is wallet-only pending separate proof, supported Ubuntu 24.04.x/26.04.x use reviewed row-specific semantic AppArmor compatibility rather than exact parser patch equality, and the remaining Debian unwind transition is fixed below. Task-108 can therefore proceed without taking over `.rpm`, exact-renderer certification, final package-manager update UX, portable-user migration, runtime canary enforcement, or guest enablement.

## Interaction Mode And Implementation Gate

- Mode: approved `interactive_decision`, followed by pending `interactive_validation` during implementation.
- Build lifecycle remains `in_progress`. The user-approved escalation resolves the planning authority checkpoint, so implementation may synchronize the governing documents/probe and then proceed to package code; completion still requires disposable-host validation on every supported `.deb` row.
- The approved successor matrix revision is `task-108-matrix-2026-08-18`. Ubuntu 22.04.x is an explicit wallet-only row with reason `apparmor-policy-proof-pending`; it may become supported only through separate row proof and a later reviewed matrix revision. Ordinary wallet-only installation must not be blocked, and this row creates or loads no Daedalus AppArmor policy and leaves the helper mode `0755`.
- Ubuntu 24.04.x and 26.04.x remain supported subject to reviewed, row-specific AppArmor profile mode/label and semantic compatibility checks. Each row must prove its declared ABI/features and profile semantics; observed `apparmor_parser` versions are evidence only and exact parser patch-version equality is forbidden as a trust decision.
- The Canonical `flags=(default_allow)` plus `userns,` profile class may be used only where the reviewed row contract and host policy support it. It is an exception granting ordinary access by default plus explicit userns permission, not ordinary least-access confinement.
- A harmless unrelated profile, a profile that merely parses, a SUID helper, or the superseded task-005-a predicates cannot substitute for mandatory supported-row policy. Any supported-row prerequisite or semantic check failure remains fail-closed without retry, weakening, or unsandboxed fallback.

### Approved Escalation Resolution And Interaction Sequence

1. Planner Iteration 1 proposed `interactive_validation`; Critiquer Iteration 1 returned `requires_changes` with thirteen blockers.
2. Planner Iteration 2 changed the plan to `interactive_decision` followed by `interactive_validation` and resolved blockers 1-3 and 5-13; Critiquer Iteration 2 returned `requires_changes` solely because `old-preinst abort-upgrade <new-version>` was omitted.
3. The planning review log then closed. The user, acting as authoritative escalation owner, approved Ubuntu 22.04.x as wallet-only pending separate proof, row-specific semantic AppArmor compatibility for supported Ubuntu rows, and the non-destructive old-preinst unwind defined below.
4. This escalation resolves the exact outstanding approval bar and approves planning without inventing another Critiquer transition or modifying either append-only review log.

## Required Decision And Manual Evidence

### Approved Authority Resolution

The durable user authority response establishes:

1. Successor revision `task-108-matrix-2026-08-18` supersedes the contradicted Ubuntu predicates in `task-005-a-matrix-2026-08-14`: Ubuntu 22.04.x is wallet-only pending separate proof; Ubuntu 24.04.x and 26.04.x remain supported; Debian 12.x and 13.x remain supported without Daedalus AppArmor policy. Other matrix and scope decisions remain unchanged.
2. Each supported Ubuntu row has its own reviewed profile template class, AppArmor ABI/feature requirements and compatibility rule, expected loaded-profile listing, and exact renderer `/proc/<pid>/attr/current` representation. Compatibility is semantic and row-specific; observed parser versions are recorded as evidence, never required as permanent exact patch-version equality.
3. Ubuntu 22.04.x is not eligible for supported-row claims or AppArmor setup under this revision. Separate real-host proof and a later reviewed matrix revision are required before that disposition can change.
4. `flags=(default_allow)` is an exception granting ordinary access by default plus explicit `userns,` permission, not a claim that AppArmor confines Electron's files, libraries, process execution, or IPC.

### Completion Evidence

Use disposable, snapshotted x86_64 hosts for supported Ubuntu 24.04.x/26.04.x and Debian 12.x/13.x, plus an Ubuntu 22.04.x wallet-only fixture. No wallet, credentials, funds, Cardano network, hardware wallet, dApp, or remote URL is needed. Return only normalized evidence; raw paths, PIDs, usernames, hostnames, traces, audit output, and stderr stay host-local.

For every row, record the exact distro/point release, candidate `.deb` SHA-256, source revision, `flake.lock` SHA-256, package/control metadata, support-state manifest, installed path/owner/mode/hash inventory, package-manager states, desktop launch chain, helper metadata, mount `nosuid` result, and lifecycle results. Supported Ubuntu evidence also records row-specific semantic ABI/feature compatibility, observed parser version, non-loading parse, atomic load/reload, loaded-profile representation, reboot persistence, and cleanup. Ubuntu 22.04.x and Debian evidence prove no Daedalus AppArmor profile is created or loaded.

Mandatory lifecycle runs are fresh install, repeated `dpkg --configure`, byte-distinct upgrade, explicit downgrade refusal with old state preserved, interrupted/failed configuration recovery, byte-distinct failed upgrade with prior profile/helper/manifest/package state preserved, successful reconfiguration, remove, purge, reinstall, and snapshot rollback. Mixed-version/failure fixtures include `old-preinst abort-upgrade <new-version>`. At least one supported Ubuntu run must induce a real parse or load failure and prove nonzero configuration, no retry or weakening, exact rollback, and later successful configure.

Create a sentinel below a disposable `XDG_DATA_HOME/Daedalus`. Prove non-mutation by before/after metadata and content hashes. Support the stronger non-inspection assertion with maintainer-script source/static checks and host-local file-syscall tracing scoped to maintainer-script processes and descendants; if tracing cannot establish that claim, report only non-mutation.

Run a bounded installed-package startup smoke on every supported row through the package desktop/launcher path. It proves only that the absolute chain and host policy permit Daedalus/Electron startup and that argv has no bypass; it is not task-005-b exact-renderer sandbox certification.

## Scope

- Apply the approved `task-108-matrix-2026-08-18` AppArmor/matrix/probe correction before implementing package behavior against it, preserving fail-closed history and authority.
- Add reproducible x86_64 cluster-specific `.deb` outputs for every `installer-clusters.cfg` cluster while preserving legacy `installer-<cluster>` unchanged.
- Install a fully materialized root-managed tree under `/opt/daedalus/<cluster>`, package-owned desktop/icon entries, an absolute package-only launch/restart chain, support-state identity, and `.deb`-specific portable-update disablement.
- Implement complete Debian lifecycle behavior, transactional AppArmor ownership/rollback on supported Ubuntu 24.04.x/26.04.x rows, and a narrowly verified SUID-capable helper setup on supported Debian/Ubuntu rows.
- Add deterministic package metadata, package checks, focused maintainer-script fixtures, Hydra `binary-dist`, Buildkite artifact wiring, and truthful package/sandbox documentation.

## Non-Goals

- Do not implement `.rpm`, SELinux, Fedora behavior, or alter `task-109` ownership.
- Do not certify Chromium's exact renderer route/containment, perform mechanism attribution, or complete denied-prerequisite certification; those remain `task-005-b`.
- Do not implement the final updater/package-manager UX, repository signing/publication, legacy-user migration, or retire the `.bin`; those remain `task-110`. This task only prevents the `.deb` from executing portable updates or restarting a home install.
- Do not remove development/legacy portable bypasses globally, add runtime argv/environment rejection, or add the canary; those remain `task-103`.
- Do not add AppImage, Flatpak, Snap, host-wide AppArmor/userns changes, automatic privilege escalation, network fetches, or wallet-state migration.
- Do not create or enable a dApp guest, connector API, remote content, or production release gate.

## Dependencies And Downstream Ownership

- Required dependency: completed `task-005-a`, as superseded only for the approved Ubuntu matrix/profile predicates by `task-108-matrix-2026-08-18` because later authoritative evidence invalidated part of its frozen conclusion.
- During implementation, update `task-005-a.md`, research 05/06, PRD, tracker, architecture wording, and the live probe only to record the approved successor matrix/profile semantics. Preserve old approvals and negative evidence as history; do not rewrite review logs or claim task-005-b results.
- `task-109` owns `.rpm` and Fedora/SELinux; `task-005-b` owns installed exact-renderer certification; `task-103` owns remaining bypass/runtime enforcement; `task-110` owns final updater UX, release replacement, `.bin` retirement, and migration.
- `task-107`, `task-802`, `task-807`, and `task-903-a` retain packaged hostile-guest and release-candidate proof.

## Fixed Implementation Decisions

### Outputs, Metadata, And Reproducibility

- Preserve `internal.x86_64-linux.unsignedInstaller`, `installer-<cluster>`, and current consumers. Add `internal.x86_64-linux.debInstaller.<cluster>` and `deb-installer-<cluster>`; `nix build -L .#deb-installer-mainnet` is canonical.
- Package name is `daedalus-<cluster>`, `Architecture: amd64`, `Section: utils`, `Priority: optional`, `Maintainer: DevOps <devops@iohk.io>`, and the existing project description/homepage/license metadata is reused where available. Packages for different clusters do not conflict because paths/names are cluster-specific.
- Debian `Version` is `${package.json.version}+build${buildCounter}.git${buildRevShort}-1`. `buildCounter` is the release-ordering component and must increase for a new artifact at the same upstream version; a reused counter with different bytes is a build failure. Validate with `dpkg --validate-version` and ordering fixtures.
- Use pinned `dpkg-deb --root-owner-group` with uniform compression. Set one `SOURCE_DATE_EPOCH` from source revision, normalize all staged mtimes, owner/group, permissions, locale, timezone, file order, gzip/xz options, control formatting, and archive member metadata. Two independent clean builds with separate output paths must have identical SHA-256; if not, identify and remove every differing field before acceptance.
- Freeze the runtime library `Depends` from an extraction-time ELF scan against the bundled tree and an allowlisted Debian/Ubuntu package mapping; no broad guessed dependency list. `apparmor_parser` is conditional host setup in a shared Ubuntu/Debian artifact: use `Suggests: apparmor`, never install it in maintainer scripts, require it and the approved row-specific semantic ABI/features on supported Ubuntu 24.04.x/26.04.x rows, and do not require or invoke it on Debian/wallet-only rows.
- Emit package MD5/SHA-256 inventories and `nix-support/hydra-build-products` with `file binary-dist "<deb>"`. Add `hydraJobs.deb-installer.x86_64-linux.<cluster>` to `nonrequired`, a representative `checks.x86_64-linux.linux-deb-package-contract`, and explicit Buildkite x86_64-linux build/upload of `deb-installer-<cluster>` `.deb` artifacts alongside, not instead of, `installer-*`.

### Installed Layout And Absolute Launch Chain

- Reuse/materialize `newBundle.<cluster>` rather than Nix-store symlinks. Repatch Electron's ELF interpreter to the final fixed path during the build and ship no patch tool.
- Retain frozen paths: `/opt/daedalus/<cluster>/bin/daedalus`, `libexec/daedalus-frontend`, `libexec/electron`, resolved `libexec/bundle-electron/lib/electron/electron`, adjacent `chrome-sandbox`, and `share/daedalus-sandbox-identity.json`.
- Add `/usr/bin/daedalus-<cluster>` mode `0755` as a package-owned regular wrapper that execs only `/opt/daedalus/<cluster>/bin/daedalus`; `/usr/share/applications/Daedalus-<cluster>.desktop` mode `0644` has an exact absolute `Exec=/opt/daedalus/<cluster>/bin/daedalus` and `Icon=daedalus-<cluster>`; `/usr/share/icons/hicolor/512x512/apps/daedalus-<cluster>.png` is regular root-owned mode `0644`.
- Every package launcher derives the root from the build-time literal `/opt/daedalus/<cluster>`, clears or overwrites inherited `ENTRYPOINT_DIR`, does not prepend an inherited `PATH`, and invokes exact packaged paths. The launcher execs exact packaged `cardano-launcher`; launcher config sets absolute `daedalusBin` to the package frontend; frontend execs the exact packaged Electron wrapper and JS tree. No package restart path reads a caller-controlled root or `$HOME/.daedalus`.
- Restrict forbidden-string checks to generated shell launchers, launcher config, maintainer scripts, and parsed desktop `Exec` fields so embedded Chromium switch text is not a false positive. Shell/config fixtures exercise branches to prove they do not synthesize `--no-sandbox`, `--disable-setuid-sandbox`, or equivalent.

### Package-Specific Portable-Update Disablement

- Add a package launcher-config value such as `applicationUpdateMode: system-package-disabled` and consume it in `manageAppUpdateChannel.ts` before installer existence/hash reads, chmod, spawn, or app exit. In this mode, return a fixed unsupported/system-package result and never execute `updateRunnerBin`.
- The `.deb` tree contains no functional `update-runner`; its config does not select one, and its frontend has no `daedalus_lockfile.pre-auto-update` or home-install restart branch. Focused tests prove a `.deb` request cannot chmod/execute a downloaded portable artifact and cannot restart `$HOME/.daedalus/<cluster>/bin/daedalus`.
- Keep the existing portable configuration and code path behavior unchanged for legacy `installer-*`. Task-110 later supplies final package-manager UX and removes legacy machinery.

### Support-State Manifest

- Upgrade the identity schema in coordination with the probe and task-103 handoff. The selected root-owned mode-`0644` manifest records schema version, package family `deb`, matrix revision, exact matrix row key, detected `ID`/`VERSION_ID`, `supportState` (`supported` or `wallet-only`), fixed reason code, cluster, package version/build/source identity, policy kind/profile semantic identity, helper expectation, absolute launch identities, and exact file hashes.
- Supported Debian rows use distinct `debian-12`/`debian-13` row keys with `supportState: supported` and `policy.kind: none`. Omitted distributions use `matrixRow: null`, `supportState: wallet-only`, and `reason: unsupported-distro-version`; they can never serialize identically to supported Debian.
- Ubuntu 22.04.x uses an explicit `ubuntu-22.04` row key with `supportState: wallet-only`, `reason: apparmor-policy-proof-pending`, `policy.kind: none`, and helper expectation `0755`. It can never serialize as a supported Ubuntu row and does not create or load a Daedalus AppArmor profile.
- Generate the selected manifest deterministically from immutable package templates and host identity. Bind its hash and selection fields in a root-only mode-`0600` ownership marker under `/var/lib/daedalus-package/<cluster>/`; the probe verifies both package-derived fields and marker hash. Unsupported rows retain helper `0755`, create no host policy, and never claim dApp support.

### Helper Provenance And Permissions

- The archive ships `chrome-sandbox` root-owned mode `0755`. Embed its expected SHA-256 independently in the installed maintainer script/package metadata, not only in a manifest beside the helper. Before elevation, open/inspect the exact path without following symlinks and require regular file, link count one, expected device/inode stability across hashing/chown/chmod, expected hash, root ownership, and no ACL/file capability.
- Require every ancestor from `/` through the helper directory to be root-owned, non-symlink at the checked component, and not group/world writable. Scan the complete package tree for unexpected SUID/SGID bits and file capabilities; only the exact helper may end mode `4755`, and no package hardlink may alias it.
- Honor `dpkg-statoverride`: an absent override permits package setup; an exact root/root/`4755` override may be retained; any conflicting override is never overwritten or removed and makes a listed supported-row configuration fail closed. Cleanup removes only an override created by this package, though the preferred implementation creates none.
- Detect a `nosuid` effective mount for the helper. Do not claim an effective SUID route from metadata on `nosuid`; a listed row may continue only if the corrected contract explicitly permits and later certification proves the independent userns route. Task-108 records setup state only and never claims Chromium selected a route.

### Transactional AppArmor Ownership

- For supported Ubuntu 24.04.x and 26.04.x rows, package immutable, separately reviewed row-specific profile templates below `/opt/.../share`; select only the matching row template after its semantic ABI/feature checks, never by exact parser patch string. The expected Canonical-derived class is exact Electron attachment with `flags=(default_allow)` plus `userns,`, but only the reviewed row template is accepted. Record compatibility requirements and observed parser version separately.
- Generated active profile path remains `/etc/apparmor.d/opt.daedalus.<cluster>.electron`, root-owned regular mode `0644`. A root-only marker records package/version, template/profile hashes, previous owned hash, semantic ABI class, loaded identity representation, and transaction phase.
- Refuse pre-existing symlinks, non-regular files, foreign files, absent/mismatched ownership markers, or administrator-modified bytes. Never clobber or delete them. An owned old profile may be replaced only when its bytes match the marker's old hash.
- Build a same-directory mode-`0600` temporary file with exclusive creation, write the complete package-derived profile, set final ownership/mode, non-loading parse it, and atomically rename it. On upgrade, never unload first: atomically replace, invoke parser replace/reload, verify the approved loaded identity and renderer-label contract, then commit marker/selected manifest. If any step fails, atomically restore prior bytes, reload and verify prior policy, restore helper/manifest/marker state, and return nonzero. Failure to restore is a hard diagnostic requiring snapshot recovery, never policy weakening.
- Use normal AppArmor load/cache behavior so the profile persists across reboot; tests record cache result but never make stale cache authoritative over source profile bytes. Reboot evidence must show the exact source hash and loaded identity.
- A package lifecycle lock and a `removing` marker make new package launches refuse during removal. `prerm remove` then verifies no process whose `/proc/<pid>/exe` is the exact packaged Electron remains; if one does, removal fails before profile/helper changes. This prevents silently unconfined surviving Daedalus processes. Upgrade does not unload policy or block the replace sequence.
- Remove/unload only when active bytes and marker identify package-owned content. Foreign/modified content is preserved and reported. Cleanup never removes a profile written by a newer version or another package instance.

### Complete Debian Maintainer-Script State Machine

- All scripts are executable, noninteractive, `set -eu`, idempotent, no-network, and use one versioned helper library copied into each script so old/new script ordering does not depend on files already unpacked. They use a root-only transaction directory and atomic markers; no script references `HOME`, `XDG_DATA_HOME`, `DAEDALUS_DIR`, or wallet paths.
- `new-preinst install [old new]`: validate no foreign fixed-root/profile/marker collision, create a transaction snapshot when prior package configuration exists, and otherwise make no helper/policy mutation. `new-preinst upgrade old new`: compare Debian versions, snapshot exact owned helper/profile/manifest/marker state, and reject downgrades before mutation with old state intact.
- `old-preinst abort-upgrade <new-version>`: acquire the lifecycle lock and non-destructively recover the prior committed state. If the package-owned transaction snapshot exists, restore and verify the prior helper mode/ownership/hash and AppArmor source/loaded state plus their markers; if no candidate mutation occurred, verify the prior committed state unchanged and return success. Never unload/delete policy, demote/delete the helper, consume newer/foreign state, or guess without an ownership-valid snapshot; an ownership or restore failure returns the fixed fail-closed recovery code.
- `old-prerm upgrade new`, `new-prerm failed-upgrade old new`, and deconfigure/conflict forms: perform no unload, profile deletion, helper demotion, or selected-manifest deletion. Return success unless ownership corruption requires fail-closed recovery; mixed-version scripts cannot remove new state.
- `old-postrm upgrade new` and `postrm disappear`: explicit no-ops for package-owned active policy/state. This guard is present from the first task-108 package so an old script cannot unload a newly unpacked version.
- `postinst configure [last-configured]`: under the lifecycle lock, resume or start one transaction; revalidate package tree/helper provenance; select wallet-only, supported Debian, or supported Ubuntu state; establish helper and transactional policy; write selected manifest last; verify all committed state; and mark configured. Repeated configure converges without changing hashes or weakening state.
- `old-postinst abort-upgrade new`, `postinst abort-remove`, and `postinst abort-deconfigure`: restore and verify the previous committed snapshot if this package owns it; otherwise make no destructive guess and fail with a fixed recovery code.
- `new-postrm abort-install [old new]`, `abort-upgrade old new`, and `failed-upgrade old new`: roll back only candidate state carrying this transaction/version ID, restore and verify the previous committed snapshot, and preserve any newer or foreign state. If no candidate mutation occurred, they are idempotent no-ops.
- `prerm remove`: set the no-new-launch marker, acquire the lock, refuse while exact packaged Electron processes run, snapshot owned policy state, and otherwise leave policy loaded until package execution is stopped. `postrm remove`: unload then remove only hash-matching package-owned profile/selected manifest/helper marker, remove the launch block, and retain only minimal purge/recovery metadata needed for safe reinstall. `postrm purge`: repeat safe cleanup, remove matching package-created statoverride if any, and remove root-only package state only when no foreign/modified asset remains.
- Tests exercise every Debian Policy invocation above plus fresh install, upgrade, downgrade refusal, failed `preinst`, failed `postinst`, old/new mixed ordering, interrupted configure, failed-upgrade recovery, remove, purge, reinstall, and repeated configure. Mixed-version/failure fixtures explicitly invoke `old-preinst abort-upgrade <new-version>` both before candidate mutation and after candidate helper/AppArmor mutation, proving exact prior-state preservation/restoration and fail-closed ownership-corruption behavior.

## Expected Changes

- `nix/internal/x86_64-linux.nix`, new focused `nix/internal/linux-deb*.nix`/templates/scripts: reusable materialized package tree, absolute package launchers, metadata, lifecycle, support manifest, profile templates, helper setup, and deterministic archive.
- `nix/internal/launcher-config.nix`, `source/main/config.ts`, `source/main/ipc/manageAppUpdateChannel.ts`, and focused tests: `.deb`-specific portable-update refusal while preserving legacy portable behavior.
- `scripts/linux-chromium-sandbox-probe/main.cjs`: successor matrix/schema, semantic AppArmor profile/parser/label predicates, and support-state distinction; no exact-renderer certification result.
- `perSystem/packages.nix`, `perSystem/checks.nix`, `flake.nix`, `nix/internal/buildkite-pipeline.nix`, and `tests/shellcheck.nix`: exact output/check/Hydra/nonrequired/download wiring.
- `README.md`, `.agent/workflows/build.md`, `.agent/workflows/nix.md`, architecture, research 05/06, PRD/tracker, task-005-a/task-108 plans, and `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`: remove/disposition stale `.bin`/AppImage/sandbox-disable advice and record corrected package truth. `.bin` remains a legacy output until task-110 and is never described as a supported dApp-capable artifact.
- No review transcript is edited by implementation agents; Orchestrator-owned logs remain append-only.

## Implementation Approach

1. Record the user-approved `task-108-matrix-2026-08-18` resolution and corrected default-allow/row-specific semantic ABI/label predicates across governing docs and probe before package code, preserving the contradicted task-005-a record as history.
2. Add the minimal package-specific update-mode contract and tests so a `.deb` cannot execute/restart into the portable installer path.
3. Materialize the exact tree and absolute launcher/desktop/icon chain; normalize ownership/modes and verify no path/symlink/hardlink escape or unexpected privilege bit/capability.
4. Generate exact Debian metadata, support-state/profile templates, embedded helper hash, lifecycle scripts, transaction markers, and deterministic archive.
5. Add exhaustive dependency-free synthetic-root lifecycle/failure fixtures and package extraction checks, then expose package/check/Hydra/Buildkite artifacts.
6. Build twice cleanly, compare bytes, run all focused static/tests, and hand one exact candidate to operators.
7. Run every supported-row lifecycle plus the Ubuntu 22.04.x wallet-only fixture, mandatory negative/recovery, startup smoke, reboot, tracing, and rollback case. Correct package bytes and rerun affected rows if evidence changes a template or predicate.
8. Synchronize only implemented facts, submit implementation review, and leave task/build completion blocked until all manual evidence and review pass.

## Acceptance Criteria

- User-approved successor matrix `task-108-matrix-2026-08-18` supersedes the contradicted task-005-a Ubuntu statements. Ubuntu 22.04.x is explicitly wallet-only pending separate proof and a later reviewed revision; supported Ubuntu 24.04.x/26.04.x use reviewed row-specific semantic AppArmor checks. No exact parser patch equality remains a trust decision.
- `nix build -L .#deb-installer-<cluster>` produces byte-reproducible `amd64` packages for every cluster with frozen metadata, absolute layout, desktop/icon paths, checksums, and `binary-dist` exposure; two clean builds hash identically.
- The support manifest distinguishes every supported Debian/Ubuntu row, the explicit Ubuntu 22.04.x wallet-only row, and omitted wallet-only hosts, and is bound to package-derived identity and root-only transaction state.
- Helper setup verifies independent immutable provenance, ancestors, link/inode stability, modes, statoverride, capabilities, all package privilege bits, and `nosuid`; only the exact helper may become `4755`, without claiming route selection.
- AppArmor ownership is non-clobbering and transactional, survives reboot, replaces without an unload gap, restores previous state on failure, cleans only matching owned bytes, and cannot unconfine a still-running package process.
- Every maintainer-script invocation and recovery transition, including `old-preinst abort-upgrade <new-version>`, has defined, idempotent behavior; fresh install, upgrade, downgrade refusal, interruption/failure recovery, remove, purge, reinstall, and repeated configure pass.
- Every package/desktop/restart path uses exact package-owned executables and contains no sandbox bypass. The `.deb` cannot chmod/execute portable updates or restart a home install; legacy portable outputs remain unchanged pending task-110.
- Supported-row setup failures return nonzero without policy weakening or retry. The explicit Ubuntu 22.04.x wallet-only row and omitted rows install wallet-only with helper `0755`, no Daedalus AppArmor asset, no dApp-support claim, and no unsandboxed fallback.
- Installed startup smoke passes on each supported `.deb` row without being labeled sandbox certification. Mandatory parse/load failure and byte-distinct failed-upgrade recovery preserve old state and are followed by successful configure and rollback.
- Maintainer scripts do not mutate wallet state; source checks plus host-local syscall tracing substantiate non-inspection, or the final claim is narrowed to proven non-mutation.
- Exact Hydra/nonrequired/check and Buildkite artifact wiring works; focused package, lifecycle, updater, probe, shellcheck, format, Nix, graph, and documentation consistency checks pass.
- Docs truthfully call `.bin` legacy until task-110, remove AppImage and unsafe sandbox-disable guidance from current instructions, and do not claim `.rpm`, task-005-b certification, final updater UX, migration, canary enforcement, or guest enablement.

## Verification Plan

### Agent-Executable

- Validate the successor matrix/profile schema and probe fixtures, including default-allow semantics, loaded-label forms, parser semantic compatibility boundaries, supported Debian versus omitted host identity, and rejection of old schema/exact-version assumptions.
- Evaluate all `deb-installer-{mainnet,preprod,preview,selfnode}` outputs while confirming all legacy `installer-*` outputs remain. Build mainnet twice from clean independent roots and compare `.deb` SHA-256 and archive member metadata.
- Run `dpkg-deb --info`, `--contents`, and extraction checks for exact control fields/version ordering/dependency mapping, maintainer scripts, conffile treatment, checksums, desktop/icon paths, ELF interpreter, support templates, helper embedded hash, permissions, privilege bits/capabilities, and archive determinism.
- Parse desktop `Exec`; inspect only launch/config/control surfaces for forbidden switches; execute branch fixtures for absolute root derivation, exact executable selection, no caller `ENTRYPOINT_DIR`/`PATH`, no home restart, and no portable updater execution.
- Run synthetic-root fixtures for every maintainer-script argument/order and ownership case, including foreign/symlink/admin-modified profiles, stale/newer markers, parser/load/replace/rollback failure, cache/reboot state model, statoverride conflict, `nosuid`, hardlink/inode swap, capabilities, repeat configure, downgrade, interruption, mixed old/new scripts, `old-preinst abort-upgrade <new-version>` before/after candidate mutation, remove with running process, and safe purge.
- Run focused Jest for package update mode, package Nix check, shellcheck, probe self-test/syntax, focused Prettier, affected Nix evaluation/flake checks, task JSON parse/unique/dependency/acyclicity checks, internal-link/stale-guidance searches, and `git diff --check`.
- Inspect the complete diff for `.rpm`/SELinux, exact-renderer result claims, global portable behavior changes, final updater UX/migration, guest/runtime canary, wallet-state, or review-log scope creep.

### Operator-Executed

- Run the complete evidence matrix above on clean snapshots with native `apt`/`dpkg`, including repeated configure, byte-distinct upgrades, downgrade refusal, interruption, remove/purge/reinstall, and reboot.
- On supported Ubuntu 24.04.x/26.04.x, independently verify the reviewed row-specific semantic parser/profile compatibility, non-loading parse, loaded exact profile/renderer-label representation, atomic replacement, cache/reboot persistence, and hash-safe cleanup. Never disable AppArmor or global userns restrictions to pass.
- Induce at least one real parse/load failure and one separate byte-distinct failed upgrade. Verify old profile remains loaded, helper/manifest/package state remains exact, no new launch can escape policy, successful `dpkg --configure -a` recovery, and snapshot rollback.
- On Debian, verify supported support-state identity and absence of a Daedalus AppArmor asset/load. On Ubuntu 22.04.x, verify the explicit wallet-only identity, reason, helper `0755`, and no Daedalus AppArmor creation/load. On one other omitted distro fixture, verify distinguishable wallet-only identity and no policy mutation.
- Trace maintainer-script file syscalls locally against the sentinel and retain raw traces only on the host. Return the normalized no-access result plus sentinel hashes, or narrow evidence to non-mutation.
- Start through desktop and absolute launcher with a bounded timeout; record sanitized argv/startup category only. Do not load a remote dApp or call this task-005-b containment evidence.

## Risks And Fail-Closed Decisions

- The original task-005-a Ubuntu contract is not implementable as written. Applying the user-approved successor contract to governing docs/research/probe during task-108 is mandatory, not scope creep; Ubuntu 22.04.x remains wallet-only unless separate proof supports a later reviewed revision.
- `flags=(default_allow)` intentionally permits ordinary accesses and only mediates the added userns rule. It satisfies Ubuntu's documented exception pattern when approved, but it is not broad Electron confinement and must never be described that way.
- Shared Ubuntu/Debian metadata cannot express a distro-conditional hard dependency on `apparmor_parser`; `Suggests` plus explicit supported-Ubuntu row prerequisites avoids silently installing policy infrastructure on Debian or Ubuntu 22.04.x. Missing supported-row prerequisites fail configuration without network fetch.
- Metadata and a startup smoke cannot prove the SUID route is effective or selected, especially on `nosuid`; task-005-b retains exact-renderer and route evidence.
- Removal safety may require the operator to close Daedalus before retrying package removal. Failing removal is preferable to unloading policy beneath a running exact package process.
- Package-specific updater refusal is required to avoid switching back to the rejected artifact. It is not task-110's final UX or migration design.

## Sources Consulted

- `.agent/readme.md`, `.agent/system/architecture.md`, and workflows `nix.md`, `build.md`, `test.md`, and `update-doc.md`.
- PRD/tracker, full task-005-a canonical plan and both review transcripts, full task-108 planning transcript/current plan, and Linux research 05/06.
- Live probe; Linux Nix bundle/launcher/config/self-extractor; package/check/flake/Hydra/Buildkite seams; updater IPC/config/types; installer metadata; README/build/Nix/Mithril sandbox guidance; shellcheck and cluster sources.
- Canonical, “Restricted unprivileged user namespaces are coming to Ubuntu 23.10,” including the exact `flags=(default_allow)` Chromium example and prior-release statement.
- Debian Policy Manual chapter 6, including complete old/new maintainer-script invocation and unwind ordering, and Debian `dpkg-statoverride(1)` behavior.
- User-approved planning escalation resolution of 2026-08-18: non-destructive `old-preinst abort-upgrade <new-version>` recovery, Ubuntu 22.04.x wallet-only pending separate proof, and reviewed row-specific semantic AppArmor compatibility for supported Ubuntu rows.
- `understand` was loaded for repository-understanding guidance. Generating its knowledge graph would violate this task's one-file write constraint, so every conclusion was verified directly against live files and authoritative sources.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-108-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-108-impl-review.md`
- Both are Orchestrator-owned append-only transcripts and must not be modified by Planner or implementation work.

## Lifecycle Status

- Planning status: `approved`
- Build status: `completed`
- Current outcome: agent-executable implementation and automated verification
  are complete. All four additive `.deb` outputs build, package/probe/updater
  contracts pass, and Ubuntu 24.04 live installation, AppArmor, startup,
  no-bypass, removal-refusal, rollback, purge, and wallet-preservation evidence
  passed. On 2026-08-18 the user accepted task-108 as complete and explicitly
  deferred the remaining package lifecycle matrix to manual release-candidate
  validation after the full PRD implementation is assembled.
- Completion rule: the authoritative user acceptance supersedes this task's
  original all-row completion gate without claiming unexecuted rows passed.
  `research/08-task-108-deb-validation-handoff.md` remains the durable deferred
  runbook. Task-005-b exact-renderer certification and later packaged release
  gates are unchanged. Final implementation review approved the transactional
  rollback and package contract in Iteration 4.

## Planner Self-Review

- Blockers 1-3: default-allow versus enforce semantics, exact loaded-label proof, Ubuntu 22.04 disposition, semantic parser compatibility, and governing-contract correction are resolved by `task-108-matrix-2026-08-18`; implementation must encode those approved predicates before package code.
- Blockers 4-5: every Debian old/new script transition, including non-destructive `old-preinst abort-upgrade <new-version>`, downgrade/interruption/recovery path, transactional profile ownership, foreign/admin state, atomic replacement/rollback, reboot persistence, and running-process removal safety is fixed and explicitly covered by mixed-version/failure fixtures.
- Blockers 6-9: helper provenance/ancestors/hardlinks/capabilities/statoverride/`nosuid`, machine-consumable support state, absolute launch/desktop/icon paths, and `.deb`-specific portable-update/home-restart disablement are complete without taking task-110's final UX.
- Blockers 10-13: Debian metadata/dependencies/version ordering, byte reproducibility, bounded launch checks/smoke, mandatory privileged negative/recovery/tracing evidence, exact Hydra/Buildkite wiring, and all stale guidance including Mithril notes are named.
- Scope and elegance: the plan reuses `newBundle`, pinned `dpkg-deb`, launcher config, existing updater gate, schema-v2 probe, shellcheck, and cluster generation. It adds no package framework, daemon, repository, signing system, compatibility alias, `.rpm`, exact-renderer certification, final updater UX, migration, or guest machinery.
- Interaction truthfulness: the closed planning log remains unchanged at `requires_changes`; authoritative user decisions, not fabricated transcript transitions, approved planning and later accepted implementation completion with an explicit evidence deferral. Unexecuted root-host rows remain manual release-candidate obligations and are not represented as passed.
