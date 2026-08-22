# Task task-109: Implement Linux .rpm system package with sandbox postinst

## Task

- Task ID: `task-109`
- Title: `Implement Linux .rpm system package with sandbox postinst`
- Phase: `phase-1` (`Electron And IPC Security Foundation`)
- Priority: `critical`
- Tracker state at planning: `pending`

## Why This Task Was Chosen Now

- Completed `task-005-a` freezes Fedora 43 x86_64 as the only supported RPM row, `/opt/daedalus/<cluster>` as the install root, mandatory SELinux integration, either independently certified SUID or userns Chromium containment, and no unsandboxed fallback.
- Completed `task-108` supplies the reusable bundle, fixed absolute launch chain, `CHROME_DEVEL_SANDBOX` binding, system-package updater refusal, identity-manifest schema, package checks, and CI seams. The RPM must reuse those facts without changing the accepted `.deb`.
- No RPM output or production SELinux policy exists. The live probe's `reviewed_daedalus*` names are fixtures, and its exact full-context equality and module-name-only check are insufficient for production Fedora evidence.
- Fedora policy feasibility is the first unknown, not a final validation detail. Lifecycle implementation must not be built around an unproven login-domain transition or an invented full security context.

## Production Resolution (2026-08-22)

The failed bespoke `daedalus_<cluster>_t` Phase-A direction is superseded.
Production uses a cluster-specific priority-200 label-only module: exact
Electron is `bin_t`, exact `chrome-sandbox` is `chrome_sandbox_exec_t`, and the
stock Fedora Chrome policy owns any SUID-helper transition. The module adds no
permissions. Exact Fedora 43 evidence proves Chromium userns/seccomp
containment on the renderer PID; SELinux supplies enforcing exact-path policy
integration rather than a duplicate Electron confinement policy.

Implemented outputs, lifecycle, CI wiring, focused checks, and exact native
evidence are recorded in research 09 and the canonical tracker. The historical
checkpoint restrictions and proposed dedicated-domain lifecycle below remain
as provenance only where contradicted by this resolution.
Final cleanup removed the private prototype derivation and CIL after preserving
their hashes and failed evidence in research 09.

## Interaction Mode And First Checkpoint

- Mode: `interactive_validation` with an early mandatory checkpoint.
- Before the first user checkpoint, implementation may do only Phase A: materialize the existing mainnet bundle into a minimal, unsigned, package-equivalent Fedora 43 prototype; add the candidate cluster-specific CIL and exact-path launcher/helper setup; add a transition-only mode to the existing local probe; validate RPM grammar and deterministic prototype construction; and prepare bounded commands and normalized output. It must not yet add full production lifecycle machinery, all-cluster public outputs, Hydra/Buildkite jobs, or implementation-complete documentation claims.
- The first checkpoint requires a clean, snapshotted Fedora 43 x86_64 graphical desktop using the default targeted policy in enforcing mode and native RPM/SELinux tools. No wallet, funds, Cardano network, hardware wallet, dApp, or remote URL is used.
- The prototype installs the exact packaged Electron/helper paths and starts the trusted wallet under an external timeout. It must demonstrate the exact graphical-login source context, the Electron main transition, bounded startup far enough to exercise the launcher, Electron, renderer creation, packaged backend launches, ordinary disposable user-state access, and absence of unexpected AVC denials. Hardware access is not claimed without a device; policy rules for device access remain unaccepted until later physical-device gates exercise them.
- The existing probe gains an explicit transition-only fixture using `webContents.getOSProcessId()` to correlate one local renderer solely for SELinux label evidence. It reports no seccomp/namespace/route result and cannot satisfy `task-005-b`. Ordinary trusted-wallet startup evidence is limited to its authoritative main PID, exact file contexts, bounded outcome, and no-bypass argv; child enumeration is never renderer authority.
- Evidence required to continue is: prototype RPM SHA-256 and NEVRA, source/diff and `flake.lock` SHA-256, Fedora point release, SELinux policy/tool versions, graphical-login source context, expected-versus-observed main and transition-fixture renderer contexts, exact Electron/helper `matchpathcon` and on-disk contexts, module priority/version/checksum/extracted-CIL fingerprint, effective file-context precedence, bounded startup category, fixed AVC categories, no-bypass result, and snapshot rollback.
- The checkpoint freezes one supported default Fedora graphical-login mapping, transition source/type/role behavior, and context-comparison rule. Process comparisons parse `user:role:type:range`: target type and frozen target role are exact, user and range must equal the values derived from the checkpoint-approved source mapping, and the resulting full context must equal that derived value. A different login mapping is unsupported until separately reviewed; no build-time `system_u:system_r:...:s0` process literal is fabricated. Exact file contexts remain full-string comparisons.
- The checkpoint also records whether the module is an enforcing dedicated domain with reviewed permissions or merely supplies labels. Task-109 claims only the demonstrated transition, policy enforcement state, exact labels, and bounded startup; it does not call SELinux labeling Chromium sandbox containment or least-privilege confinement. A permissive domain, `unconfined_domain_type`, broad generated allow set, unresolved transition, unbounded startup, or unexplained AVC blocks Phase B.
- If the checkpoint fails, stop after preserving normalized evidence and revise or narrow Fedora support through the contract authority. Do not build lifecycle infrastructure around the failure, weaken SELinux, add `dontaudit`, retry unsandboxed, or silently accept another login mapping.

## Scope

- Phase A: add the smallest package-equivalent Fedora 43 prototype around `newBundle.mainnet`, candidate CIL, and transition-only probe mode needed for the early checkpoint.
- Phase B, only after checkpoint approval: add reproducible `rpm-installer-<cluster>` outputs for all four existing x86_64 Linux installer clusters while preserving `deb-installer-*` and legacy `installer-*` outputs.
- Reuse task-108's fixed package tree, exact absolute executables, `CHROME_DEVEL_SANDBOX`, desktop/icon integration, system-package updater refusal, and wallet-state separation.
- Implement native RPM install/upgrade/erase semantics, priority-aware SELinux ownership, default helper mode `4755`, focused package/lifecycle/probe checks, and one concrete Fedora handoff.
- Update task-807 tracker acceptance so the exact final release-candidate RPM repeats lifecycle, SELinux, and bounded startup validation after later bundle changes.

## Non-Goals

- Do not change or refactor the accepted `.deb`, AppArmor policy, task-108 evidence, or deferred `.deb` task-807 gate except for an unchanged-behavior regression check.
- Do not perform task-005-b exact-renderer Chromium SUID/userns/seccomp/namespace certification, route attribution, or denied-route certification. The transition-only fixture proves only authoritative PID-to-SELinux-label correlation.
- Do not remove development/legacy bypasses, add the runtime canary, enable dApps, load remote content, retire `.bin`, migrate home installs, or redesign update UX.
- Do not add another RPM distribution row, a generic RPM portability framework, repository/signing infrastructure, a daemon, SELinux boolean, global userns change, policy generation from AVCs, permissive policy, denial suppression, network fetch, or automatic privilege escalation.
- Do not inspect, move, chmod, or delete `XDG_DATA_HOME/Daedalus`.
- RPM has no separate purge state. This task defines install, upgrade, erase, reinstall, and residual-state recovery only; it makes no purge claim.

## Dependencies And Downstream Ownership

- Required dependencies: completed `task-005-a` and completed `task-108`.
- `task-005-b` retains exact installed-renderer Chromium containment and mechanism certification after task-109 supplies the reviewed RPM.
- `task-103` retains remaining bypass removal, runtime argv/environment rejection, and local canary ownership. `task-110` retains portable retirement, migration, and final update UX.
- `task-107`, `task-802`, `task-807`, and `task-903-a` retain real-guest and release gates. Task-807 must rerun the final candidate RPM lifecycle/SELinux/startup matrix because later PRD work changes RPM bytes; task-109 and task-005-b evidence is not immutable across package-boundary changes.

## Fixed Implementation Decisions

### Outputs, Layout, And Minimal Seams

- Phase B adds `internal.x86_64-linux.rpmInstaller.<cluster>` and `rpm-installer-<cluster>`; canonical build command is `nix build -L .#rpm-installer-mainnet`.
- Reuse pinned nixpkgs `rpmbuild`; add no external package dependency and no `.deb` packaging abstraction.
- Preserve `/opt/daedalus/<cluster>/bin/daedalus`, `libexec/daedalus-frontend`, `libexec/electron`, resolved `libexec/bundle-electron/lib/electron/electron`, adjacent `chrome-sandbox`, and `share/daedalus-sandbox-identity.json`. Add only `/usr/bin/daedalus-<cluster>`, the existing desktop-name pattern, icon path, and `/usr/share/selinux/packages/daedalus-<cluster>.cil`.
- Extend only the existing seams: `newBundle`, `x86_64-linux.nix`, `perSystem/packages.nix`, `perSystem/checks.nix`, `flake.nix` Hydra/nonrequired aggregation, the existing Buildkite cluster build step, and `scripts/linux-chromium-sandbox-probe/main.cjs`. Keep Buildkite's generic artifact upload glob unchanged; add an RPM result directory beside the `.deb` result, not another uploader.
- The RPM launcher configuration remains `applicationUpdateMode: system-package-disabled`, has no `updateRunnerBin`, and selects only exact package executables. No application updater source change is expected.

### RPM Identity, Dependencies, And Reproducibility

- Name is `daedalus-<cluster>`, architecture is `x86_64`, and Epoch is absent (equivalent to `0`) and may not later appear without an ordering migration review.
- `Version` is the unchanged `package.json` version and the build fails unless it matches `[0-9]+(\.[0-9]+)*` and contains no RPM separator or prerelease syntax. `Release` is exactly `<buildCounter>.git<buildRevShort>`, where `buildCounter` is a positive decimal integer and a release artifact requires a clean nine-lowercase-hex `buildRevShort`; no `%{?dist}` or host-derived suffix is allowed.
- Validate every N-E-V-R-A with native `rpm` query tools and freeze ordering with `rpmdev-vercmp`/RPM's `rpmvercmp`: old fixture `<` failure fixture `<` final candidate, final-to-old is a downgrade, clusters do not affect version order, and no two byte-distinct fixtures share NEVRA.
- Build with one pinned RPM implementation and explicit macros: `_buildhost daedalus.invalid`, source-date epoch as build time, mtime clamping, deterministic file order, locale/timezone, owner/group, payload compressor/level/single-thread settings supported by that pinned RPM, no build-id links, and a fixed RPM filename. Materialize payload files, normalize mtimes, and ship no Nix-store symlink or runtime patch tool. Two independent clean builds in different roots must be byte-identical, including headers and payload.
- Before Phase B, the Fedora checkpoint uses `dnf repoquery --whatprovides` and package-header resolution against enabled Fedora 43 base/updates metadata to freeze exact providers for every payload/runtime and scriptlet command. Record metadata timestamp/checksum. Use scriptlet-scoped `Requires(pre)`, `Requires(post)`, `Requires(preun)`, and `Requires(postun)` where applicable rather than one guessed SELinux dependency.
- The hard requirements include only tools actually invoked on Fedora 43, expected to come from shell/core utilities, `util-linux` for locking/mount checks, capability tools, targeted SELinux policy, `semodule`/`restorecon`/`matchpathcon`, and `semanage`; exact Fedora package names are checkpoint outputs, not guesses in this plan. The omitted-row fixture is limited to an x86_64 Fedora/RPM host on which the frozen requirements resolve. It does not promise installation on arbitrary RPM distributions.

### SELinux Identity, Ownership, And Context Precedence

- Use cluster-specific SELinux identifiers with underscores, never RPM hyphens: module `daedalus_<cluster>`, process type `daedalus_<cluster>_t`, Electron executable type `daedalus_<cluster>_electron_exec_t`, and helper type `daedalus_<cluster>_sandbox_exec_t`. The module semantic version starts at `1.0.0`; any policy-content change increments it and changes the candidate.
- Install only at module priority `200`. At `%pre`, query `semodule -lfull -m` and reject a same-name module at any other priority, a disabled instance, or a priority-200 instance not authenticated by the package marker. Priority 200, semantic version, source CIL SHA-256, the `semodule -lfull -m` active checksum, and SHA-256 of `semodule -X 200 --cil --extract` output are recorded and compared. The prototype freezes the exact Fedora 43 output grammar before Phase B fixtures depend on it.
- A root-only mode-`0600` marker under `/var/lib/daedalus-package/<cluster>` is authoritative only when package NEVRA, source CIL hash, priority, semantic version, active checksum/fingerprint, exact contexts, and committed transaction phase all match. The marker is written only after module install, relabel, and verification; its absence never authorizes overwrite or removal.
- Upgrade snapshots only authenticated external state that RPM does not own: the prior owned module source/fingerprints, helper metadata, selected manifest, marker, and labels. Install/replace the priority-200 module in one `semodule` transaction without an unload gap. On setup failure, reinstall and verify the authenticated prior module and external state. Scriptlets never claim to restore RPM payload bytes or database state.
- Before any module mutation, inspect all module priorities and `semanage fcontext -l -C`. Local file-context entries override module contexts, so any local rule or substitution matching either exact executable path is foreign precedence and a supported-row failure; it is never deleted. After install, require `matchpathcon` to resolve the module's exact full contexts, run `restorecon` on only the two package files, and require observed contexts to match.
- Final removal may remove only priority-200 module/state whose marker, source hash, active checksum/fingerprint, semantic version, and contexts all match the authenticated reservation made by `%preun`. A same-name module at another priority, changed active checksum, local context override, or modified marker blocks erasure in `%preun`. Another cluster is never inspected or changed beyond collision checks on its distinct names/paths.
- The manifest records the approved login mapping/comparison rule, transition sources, process type/role, exact file contexts, module name/priority/version/checksums, CIL hash, support state, helper expectation, and package/source identity. The probe parses process contexts according to the frozen semantic rule rather than comparing one fabricated full literal.

### Helper And Support State

- The RPM payload and supported Fedora 43 setup default to a root-owned regular non-symlink mode-`4755` helper and report `combined-unattributed` prerequisites. Task-109 never claims Chromium selected SUID or userns.
- Mode `0755` is deferred. It may be introduced only by a later reviewed matrix/package revision with same-artifact authoritative userns containment evidence; metadata or `unshare -Ur` alone is insufficient.
- Reuse task-108 checks for immutable embedded helper hash, regular file/link count one, stable device/inode, root ownership, non-writable non-symlink ancestors, no ACL/capability, no other SUID/SGID/capability, no hardlink alias, and effective-mount `nosuid`. `nosuid` is a recorded prerequisite failure for this task, not a reason to switch to `0755`.
- Fedora 43 selects `supported` and SELinux policy identity only after setup commits. An omitted resolvable Fedora row selects `wallet-only`, reason `unsupported-distro-version`, helper `0755`, no module activation, and no dApp-support claim. Write the root-owned mode-`0644` manifest last.

### Native RPM Lifecycle And Failure Semantics

- Use `%pre`, `%post`, `%preun`, and `%postun`. RPM passes the number of same-name package instances remaining: new `%pre/%post` receive `1` on install and `2` on upgrade; old `%preun/%postun` receive `1` during upgrade and `0` on final erase. Freeze and execute the native order: new `%pre`, new payload, new `%post`, old `%preun`, old payload cleanup, old `%postun`.
- All scriptlets are noninteractive, bounded, idempotent, no-network, use absolute host-tool paths frozen from Fedora 43, never use wallet paths or `$HOME`, and serialize on one root-only lifecycle lock. Durable atomic phases are `prepared`, `external-mutating`, `external-committed`, `erase-authorized`, and `residual-cleanup`; each entry validates package NEVRA and authenticated marker before resuming, rolling back external state, or refusing.
- Fresh install `%pre $1=1` validates host/support selection, downgrade/order, path/module/context collisions, helper payload expectations available from immutable script data, and writes `prepared` without host-policy mutation. Failure leaves no new payload/database entry. `%post $1=1` verifies the installed payload, configures helper/module/contexts, writes manifest last, and commits. If it fails, RPM may leave the new payload and database entry installed; the script restores only authenticated external state, leaves launch blocked/uncommitted, records `post-failed`, and requires exact-NEVRA reinstall or erase/reinstall (or snapshot/transaction rollback), never a dpkg-like configure retry claim.
- Upgrade new `%pre $1=2` authenticates/snapshots prior external state and rejects forced downgrades before mutation. New `%post $1=2` replaces external state and commits. Old `%preun $1=1` and `%postun $1=1` are explicit no-ops for active new state. A failed new `%post` can still leave new payload/database state and native RPM may continue old-version cleanup; normalized evidence records actual `rpm -q` instances and file ownership, while recovery uses the next higher untampered RPM or snapshot/transaction rollback. A failed old `%preun` can leave both NEVRAs registered; recovery removes/reinstalls the exact recorded instance only after external ownership verification.
- Final erase `%preun $1=0` is the only blocking boundary. Under the exclusive lock it validates every foreign/modified-state condition, writes the no-new-launch marker, removes execute permission from exact package launchers/Electron, and then scans immutable `/proc/<pid>/exe` identities. On refusal or any `%preun` failure it restores exact modes, clears the marker, verifies launchability, and returns nonzero, leaving payload/database installed.
- Package launchers hold a shared lock across `exec`; final `%preun` takes the exclusive lock before marking/chmod/checking. The marker plus execute-bit removal closes the supported non-root launcher/direct-exec race. It does not defend against root deliberately executing package bytes or an already-authorized kernel exec racing the mode change, so task-109 claims safe ordinary package-managed removal, not a universal launch barrier. No new runtime daemon or application IPC is invented.
- After successful `%preun`, `%postun $1=0` runs after RPM has erased payload/database state and therefore cannot preserve an installed package. It removes only the pre-authorized matching module/manifest/state and clears the marker. Failure is reported as residual external state; recovery is exact-candidate reinstall followed by erase or snapshot restoration. `%postun` performs no new foreign-state decision and never claims rollback of erased payload.
- Named interruption tests are: `I1` after `prepared` before external mutation, `I2` after helper mutation before module replace, `I3` after module replace before manifest commit, `I4` after `erase-authorized` before payload erase, and `I5` after payload erase before external cleanup. For each, record expected and observed RPM database instances, owning NEVRA per path, payload hashes/modes, module priorities/fingerprints, contexts, marker/phase, launch block, and the exact recovery route. Hard power interruption uses VM snapshot rollback unless native RPM/DNF recovery is independently demonstrated; scriptlets do not promise repair of an interrupted RPM database.

## Expected Changes

Expected additions after the checkpoint permits Phase B:

- `nix/internal/linux-rpm.nix`
- `nix/internal/linux-rpm-common.sh`
- `nix/internal/linux-rpm-pre.sh`
- `nix/internal/linux-rpm-post.sh`
- `nix/internal/linux-rpm-preun.sh`
- `nix/internal/linux-rpm-postun.sh`
- `.agent/plans/dapp-browser-cip30/research/09-task-109-rpm-validation-handoff.md`

Expected focused modifications:

- `nix/internal/x86_64-linux.nix`
- `perSystem/packages.nix`
- `perSystem/checks.nix`
- `flake.nix`
- `nix/internal/buildkite-pipeline.nix`
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `README.md`, `.agent/workflows/build.md`, `.agent/workflows/nix.md`, and `.agent/system/architecture.md`
- Research 05/06, PRD, `dapp-browser-cip30-tasks.json` task-109 facts and task-807 final-RPM acceptance, and this plan

`tests/shellcheck.nix` already discovers `.sh` files. Updater source/tests and `.deb` implementation files do not change absent a demonstrated shared regression. Review logs are Orchestrator-owned and never modified.

## Smallest Truthful Implementation Approach

1. Phase A only: build the non-public mainnet package-equivalent prototype, candidate CIL, semantic context comparison, and transition-only probe fixture; run static/reproducibility checks and produce a hash-pinned checkpoint handoff.
2. Stop for the Fedora 43 checkpoint. Freeze the actual login mapping, transition/context semantics, policy scope, module checksum grammar, context precedence, tool providers, and bounded startup result. Do not continue on guessed values.
3. After approval, add the minimal native RPM derivation and four scriptlets around the existing bundle. Default helper to `4755`; add no userns route selector.
4. Add one executable RPM package/lifecycle check and extend existing probe fixtures for production module priority/version/checksum/fingerprint, semantic full-context comparison, effective context precedence, policy hash, and supported/omitted manifest selection.
5. Expose all clusters through existing package, check, Hydra nonrequired, and Buildkite build seams while retaining the generic upload glob.
6. Build three recorded mainnet artifacts from clean inputs: old fixture with build counter `B-2`, failure-injected fixture `B-1`, and final candidate `B`, where `B` is the real final build counter and each clean NEVRA/hash is unique. The failure candidate exits at named `%post` checkpoints without other byte changes. Also build every final cluster and rebuild final mainnet independently for byte equality.
7. Run the concrete research-09 Fedora matrix. Any package-byte change invalidates prior native evidence and requires a new final hash and affected reruns.
8. Synchronize only passed implementation facts and add task-807's final-candidate RPM revalidation requirement. Keep task-109 pending until exact-candidate evidence and implementation review pass.

## Concrete Native Evidence Handoff

- Research 09 contains bounded, reviewed Fedora commands or one minimal operator driver, not prose-only instructions. It verifies SHA-256 before every install and rejects any NEVRA/hash not in its allowlist.
- Hash-pin old, failure-injected, and final RPMs plus source commit/diff, `flake.lock`, CIL, probe/driver, and expected RPM query output schemas. Record `rpm -qip`, `rpm -qlp`, `rpm -qp --scripts`, `rpm -qpR`, signature/digest state, payload inventory, and `rpmdev-vercmp` results.
- Execute fresh install, repeated exact scriptlet invocation only where its `$1` contract is valid, old-to-final upgrade, forced downgrade refusal, old-to-failure then final recovery, `%pre` failure, `%post` failure, old `%preun` failure, final `%preun` refusal, `%postun` residual cleanup failure, I1-I5 interruption points, erase, reinstall, reboot, multi-cluster isolation, foreign priority/module/context collisions, supported policy failure, omitted resolvable Fedora row, and snapshot rollback.
- Normalized output is one fixed JSON record per case containing candidate IDs/hashes, operation and injection point, command exit category, before/after installed NEVRAs, payload owner/hash/mode class, module priority/version/checksum/fingerprint class, effective/observed context matches, marker phase, launch-block state, recovery command category/result, startup/no-bypass category, sentinel result, and rollback result. Raw commands, PIDs, usernames, hostnames, environment, paths outside tokenized roots, AVC records, traces, and stderr remain host-local.
- Start only trusted local content. The transition fixture returns authoritative main/renderer label matches but no containment result. The regular app startup returns main-label, exact-file-context, bounded-startup, and no-bypass evidence only.

## Acceptance Criteria

- The early Fedora checkpoint passes and freezes actual default-login transition/context semantics, policy claim boundary, module fingerprints, context precedence, dependencies, and bounded startup before Phase B lifecycle work.
- `rpm-installer-{mainnet,preprod,preview,selfnode}` produces additive x86_64 RPMs; `.deb` and legacy outputs remain available and behaviorally unchanged.
- Final RPM NEVRA/header/payload/dependencies/scriptlets, fixed layout, ELF interpreter, desktop/icon files, deterministic macros, Hydra/nonrequired, Buildkite build, and independent byte reproducibility pass.
- Fedora 43 setup owns only priority-200 cluster-specific policy, rejects same-name/foreign/local-precedence collisions, verifies active checksums/extracted fingerprints and exact contexts, replaces without unload, restores only authenticated external state, and records the committed identity.
- Helper is `4755` with combined-unattributed prerequisites; no task-109 path selects `0755` on the supported row or claims SUID/userns route attribution.
- Every native `$1` branch, failure consequence, I1-I5 state, and recovery route is executable and matches observed RPM/DNF state. No purge or payload rollback fiction remains.
- `%preun $1=0` blocks foreign/modified state and ordinary package launches before erase; refusal restores launchability. `%postun` cleanup failures are truthfully residual after payload/database erasure.
- The omitted resolvable Fedora fixture is wallet-only with helper `0755`, no Daedalus module activation, and no dApp claim. Package lifecycle does not mutate wallet state; non-inspection is claimed only with host-local syscall evidence.
- Probe fixtures validate priority/version/checksum/fingerprint, semantic process contexts, effective file-context precedence, policy hash, and manifest selection. Task-109 does not claim task-005-b containment certification.
- Task-807 acceptance explicitly requires the exact final release-candidate RPM lifecycle/SELinux/bounded-startup rerun after the full PRD is assembled; missing evidence blocks release.

## Verification Plan

### Agent-Executable

- Phase A: evaluate/build the prototype twice, inspect RPM grammar/payload/CIL, run transition-mode fixtures without claiming live Fedora results, run probe self-test/syntax, and produce the checkpoint allowlist/evidence schema.
- Phase B: evaluate/build all final clusters; independently rebuild mainnet; compare whole RPM bytes and queried headers/payload. Build and hash old/failure/final fixtures and prove native version ordering.
- Query RPM info/files/scripts/requires/digests/signature; extract and verify exact paths, owners, modes, hashes, capabilities, interpreter, launch config, desktop entry, helper, CIL, and no bypass/update path.
- Execute focused synthetic-root scriptlet fixtures for every `$1`, phase, ownership collision, rollback-limited external state, residual cleanup, interruption model, and multi-cluster case. Synthetic results supplement but never replace native Fedora evidence.
- Run unchanged `linux-deb-package-contract`, shellcheck, focused probe fixtures, Prettier/treefmt/Nix formatting, `nix flake check --no-build`, task JSON uniqueness/dependency/acyclicity checks, internal links/stale claims, and `git diff --check`.
- Inspect the complete diff for `.deb` regression, duplicate Buildkite upload, invented Fedora facts, policy weakening, broad host mutation, runtime/guest/task-005-b scope, wallet access, remote content, and review-log edits.

### Operator-Executed

- Run the Phase A checkpoint first on a disposable Fedora 43 snapshot and return only the normalized continuation evidence. Phase B is forbidden until that evidence is reviewed.
- Run research 09 against the exact old/failure/final hashes using native DNF/RPM and SELinux tools, including every failure/interruption/recovery case and final exact-candidate startup/reboot/erase result.
- Preserve enforcing mode, never suppress denials or load remote content, keep raw evidence host-local, and revert snapshots when native state is ambiguous.
- Task completion remains blocked until final-candidate Fedora evidence and implementation review pass. Task-005-b and every later guest/release gate remain independently blocked.

## Risks And Fail-Closed Decisions

- The dedicated Electron domain may not support the full trusted-wallet startup on Fedora 43 without broad policy. The early checkpoint exists to reject or narrow that design before lifecycle code is written.
- RPM scriptlet failure is not transactional package configuration. External state can be restored, but payload/database outcomes follow RPM; reinstall, erase/reinstall, transaction rollback, or snapshot restoration is required as recorded.
- SELinux source CIL hash alone does not identify active policy. Priority-aware listings, active checksum, extracted fingerprint, marker authentication, and effective context checks are all required.
- Advisory launcher locking alone cannot close direct-exec removal races. The plan combines shared/exclusive locks with marker and execute-bit removal and narrows the claim to ordinary non-root package launch; broader protection would require runtime support outside this task.
- SUID metadata does not prove helper selection or containment. Task-005-b remains authoritative.
- Any policy, bundle, Electron/nixpkgs, dependency, or RPM-byte change invalidates affected evidence. Task-807 repeats final-artifact RPM validation.

## Docs, Research, Workflows, Live Files, And Skills Consulted

- `.agent/readme.md`, architecture, PRD, tracker, full task-005-a/task-108 plans, full task-109 review, and research 05/06/08.
- Workflows `nix.md`, `build.md`, `electron.md`, `test.md`, and `update-doc.md`.
- Live Linux bundle/`.deb`, package/check/flake/Hydra/Buildkite seams, probe SELinux implementation/fixtures, source version fields, and task-807 acceptance.
- RPM Packaging Guide and rpm.org scriptlet documentation; `semodule(8)` priority/full-list/checksum/extract behavior and `semanage-fcontext(8)` local-context precedence. Fedora guideline endpoints presented an anti-bot challenge, so inaccessible text is not treated as evidence and Fedora 43 providers/behavior remain checkpoint outputs.
- `ponytail` loaded at full level: prototype before machinery, reuse existing seams, default to `4755`, avoid a generic packaging framework, and narrow the removal claim instead of adding runtime infrastructure.
- `understand` loaded for repository exploration. Generating a knowledge graph would violate the one-file write constraint, so conclusions were checked directly against the live files.

## Documentation And Tracking Updates

- Research 09 becomes the exact prototype and final-candidate Fedora handoff with fixed commands/driver and normalized schema; it records no pass before execution.
- README, workflows, architecture, research 05/06, PRD, and tracker receive only reviewed implemented facts after the relevant checkpoint.
- Task-807 tracker acceptance gains mandatory exact final-candidate RPM lifecycle, SELinux, bounded startup, reboot, failure/recovery, and erase revalidation. Task-005-b remains the exact-renderer containment gate.
- Task-109 remains pending until final evidence and review. No deferred or failed row is represented as passed.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-109-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-109-impl-review.md`
- Both are Orchestrator-owned append-only transcripts and must not be modified by Planner or implementation work.
- The implementation log ends with the historical approved-but-blocked prototype review. It is preserved append-only and is not rewritten to imply review of the later production continuation; the final production outcome and evidence are recorded in this canonical plan, research 09, the PRD, and the tracker.
- The legacy implementation log uses `Review:` labels and non-contiguous iteration transitions that predate the current `Code Review:` grammar. Per the append-only rules, final cleanup neither rewrites that history nor appends an invalid transition.

## Lifecycle Status

- Planning status: `approved`
- Build status: `completed`
- Current outcome: Production `rpm-installer-{mainnet,preprod,preview,selfnode}` outputs are implemented. The exact Fedora 43 mainnet candidate recorded in research 09 installed and launched successfully, enforced the label-only SELinux contract, and passed exact-renderer no-new-privileges, seccomp-BPF, zero-capability, PID/user namespace, UID/GID map, exact-file, policy, and no-bypass assertions with no Electron AVC.
- Completion rule: task-109 is complete. Task-005-b and later package/guest/release gates remain independent, and any package-byte change requires installed-artifact revalidation.

## Planner Self-Review

- Early feasibility: Phase A and its exact continuation evidence precede lifecycle/output/CI machinery; implementation-before-checkpoint limits are explicit.
- SELinux truth: default login mapping, semantic full-context comparison, renderer authority, policy claim boundary, module priority/version/checksums/fingerprint, and local context precedence are frozen or explicitly checkpoint-derived.
- RPM truth: exact `$1`/ordering, per-script failure outcomes, no configured/purge fiction, limited external rollback, durable phases, named interruptions, and native recovery routes are explicit.
- Removal truth: every blocking condition is in `%preun`; refusal restores launchability; `%postun` only cleans pre-authorized residual state; the remaining root/kernel exec race is not overclaimed.
- Package identity: Epoch, Version/Release grammar, ordering, deterministic macros, Fedora provider source, and unique hash-pinned old/failure/final NEVRAs are required.
- Scope/minimality: default `4755`, deferred `0755`, one transition-only extension to the existing probe, one package check, existing package/Hydra/Buildkite seams, unchanged upload glob, no `.deb` refactor, and no runtime daemon.
- Evidence/revalidation: concrete normalized native artifacts cover failures and interruptions, while task-807 explicitly reruns the final RPM after later bundle changes.
