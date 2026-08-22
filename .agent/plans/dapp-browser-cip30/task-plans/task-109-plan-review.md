Planner: Iteration 1
Timestamp: 2026-08-21T15:54:37Z

Plan: Created `.agent/plans/dapp-browser-cip30/task-plans/task-109.md` with planning status `draft` and build status `in_progress`.

Interaction mode: Classified as `interactive_validation`. Implementation can proceed through RPM construction and automated checks, but completion requires privileged lifecycle, enforcing SELinux, exact context/process-label, startup, failure-recovery, reboot, and rollback evidence from disposable Fedora 43 x86_64 hosts.

Scope: Adds additive `rpm-installer-<cluster>` outputs by reusing the existing bundle, fixed `/opt/daedalus/<cluster>` layout, flag-free absolute launch chain, system-package updater refusal, Chromium helper contract, probe, and CI seams. It adds native RPM lifecycle handling and package-owned SELinux policy without changing the accepted `.deb`.

Security boundaries: Preserved enforcing SELinux, exact package identities, authenticated rollback, foreign-state refusal, no permissive or `dontaudit` fallback, no unsandboxed retry, wallet-only omitted hosts, disabled production guests, and all task-005-b/task-103/release gates.

Live findings: Verified that no RPM output exists, the Fedora 43 probe contract already exists, and its `reviewed_daedalus*` SELinux identities are fixtures rather than production labels. The plan requires compiled policy and live Fedora evidence before promoting exact process/file identities.

Verification: Focused Prettier and `git diff --check` passed. Only the canonical plan was created; both review logs remain untouched.

Self-review: Confirmed mandatory fields, exact expected files, smallest implementation path, acceptance criteria, agent and operator verification, manual evidence, implementation-before-interaction status, risks, documentation/tracking/research updates, review-log paths, and lifecycle status. No speculative repository, signing, policy-generation, migration, guest, or cross-distribution infrastructure was added.

Outcome: Canonical task-109 plan drafted and ready for critique.

Critiquer: Iteration 1
Timestamp: 2026-08-21T15:59:05Z

Outcome: The plan preserves the correct package, updater, guest, and downstream certification boundaries, but it is not yet implementable truthfully. Fedora SELinux feasibility, native RPM failure semantics, module ownership, removal safety, helper-route selection, evidence tooling, and release-candidate revalidation require correction before implementation begins.

Blocking findings:
1. `task-109.md:20-23,83-88,145,153,182-184` postpones the decisive Fedora feasibility question until after a complete candidate exists. A cluster-specific SELinux transition from Fedora’s desktop login domain into a confined Electron domain must support Electron, renderer, Chromium SUID bootstrap, launcher, wallet backend, node, hardware, and user-state access without permissive or generated broad policy. Full `/proc/<pid>/attr/current` values also vary with SELinux user/role/range, while the live probe requires exact string equality at `scripts/linux-chromium-sandbox-probe/main.cjs:924-927,995-999`. Require an early Fedora 43 checkpoint using a minimal package-equivalent Electron/policy prototype, and freeze supported login-domain mappings, transition sources, full-context comparison semantics, and whether the policy provides actual confinement or only labeling before building lifecycle machinery. Static Nix CIL parsing cannot establish this.
2. `task-109.md:101-104,155,184` assumes Debian-like transactional restoration that native RPM does not provide. A failed `%post` can leave the new RPM payload and database entry installed even if package-owned helper/module/manifest state is restored; RPM has no configured/half-configured state equivalent to dpkg. Scriptlets cannot restore the old RPM-owned payload behind the package database. Define expected RPM/DNF state and recovery for each `%pre`, `%post`, `%preun`, and `%postun` failure, narrow “prior-state restoration” to authenticated external state that scriptlets can restore, and require reinstall/remove or transaction rollback where native RPM semantics demand it.
3. `task-109.md:35,99-104,155` does not define a complete executable RPM lifecycle. Freeze exact `$1` meanings and ordering for fresh install, upgrade, final erase, failed `%pre`, failed `%post`, failed `%preun`, failed `%postun`, and interrupted transactions. RPM has no separate purge operation, so “remove” and “purge” cannot be claimed as distinct native cases. Also specify durable transaction phases and re-entry behavior rather than merely requiring an “interrupted transaction recovery” result.
4. `task-109.md:87-88,95,101-103,187` requires foreign-module preservation and exact active-module restoration without defining an authoritative ownership test. The live probe only checks whether `semodule -l` contains the module name (`main.cjs:1049-1075`); it does not verify module priority, version, effective selected instance, or active policy bytes. Freeze module priority and version, detection of same-name modules at every priority, an independently verifiable active-module fingerprint, ownership-marker ordering, replacement without unload, prior-owned-module restoration, effective file-context precedence, and final removal behavior. A package CIL hash alone does not prove that the same module is active.
5. `task-109.md:103` places foreign/modified-state refusal and cleanup in the final-erase flow without respecting the irreversible boundary: `%postun` runs after payload erasure and cannot fail closed while preserving an installed package. All conditions that must block erasure must be authenticated in `%preun`; `%postun` may only perform cleanup whose failure is reported as residual state. The no-new-launch marker must be cleared when erase is refused, and the exact-Electron process scan must close the marker/check/exec race rather than relying on a racy `/proc` snapshot.
6. `task-109.md:47,93-95,154,185` leaves helper-route selection circular. Task-109 explicitly does not perform task-005-b exact-renderer containment proof, so it cannot independently justify changing the helper to `0755` for a userns-only route. Use the already-established package default of root-owned `4755` and report combined/unattributed prerequisites; permit `0755` only after separately reviewed same-artifact userns evidence exists. This removes speculative route-selection machinery and preserves task-005-b ownership.
7. `task-109.md:32-39,84,145,158` asks the trusted-wallet startup run to prove the exact renderer label, but the ordinary packaged app does not expose an authoritative renderer PID. Child-process enumeration is not equivalent to `webContents.getOSProcessId()`. Add a narrow local package fixture that correlates the exact renderer solely for SELinux transition/label evidence, or defer renderer-label proof to task-005-b and limit task-109 startup evidence to the main process and exact file contexts. Do not silently run the full containment probe and then disclaim certification.
8. `task-109.md:73-74,78,150-151,166-170,186` does not freeze enough RPM identity or reproducibility detail. Specify an RPM-valid `Version` and exact `Release` grammar, Epoch policy, `rpmvercmp` ordering for upgrades and downgrade fixtures, deterministic build macros/compressor/build-host settings, exact scriptlet-scoped `Requires`, and the Fedora 43 dependency resolution source. A generic requirement on SELinux tools may also prevent the promised omitted-row wallet-only installation; constrain that fixture to a resolvable RPM host or revise dependency behavior.
9. `task-109.md:35-39,104,144-146,173-178` lacks concrete native evidence artifacts. Full upgrade and failed-upgrade testing requires hash-pinned older/newer NEVRAs and a recorded failure-injected RPM, not one final candidate. The interrupted-transaction cases need named interruption points and expected RPM database, payload, module, marker, and recovery states. Research 09 must provide bounded, reviewed Fedora commands or a minimal operator driver and fixed normalized outputs; task-108’s history demonstrates that command-by-command evidence and synthetic lifecycle models alone miss real package defects.
10. `task-109.md:125,143,158,168-170` understates required probe and CI changes. The live SELinux path recognizes only module name and exact context strings and still uses `reviewed_daedalus*` solely in fixtures. Extend executable fixtures for production module priority/version/fingerprint, full-context semantics, policy hash, effective context precedence, and supported/omitted manifest selection. Keep the existing generic Buildkite artifact glob at `nix/internal/buildkite-pipeline.nix:102-107`; add the RPM build result but do not add a duplicate upload path. Add RPM outputs/checks/Hydra aggregation alongside the existing `.deb` seams in `perSystem/packages.nix`, `perSystem/checks.nix`, and `flake.nix`.
11. `task-109.md:65,159,178,209` does not assign final-artifact RPM lifecycle revalidation explicitly. Later PRD work changes the bundled application and therefore the RPM bytes after task-109’s candidate is tested. Update task-807’s tracker acceptance, which currently names only deferred `.deb` validation at `dapp-browser-cip30-tasks.json:2018`, to require the final release-candidate RPM lifecycle/SELinux/startup rerun. Task-005-b remains the exact-renderer containment gate; neither earlier task-109 evidence nor task-005-b evidence should be treated as immutable across package-boundary changes.

Non-blocking observations:
- Additive `rpm-installer-<cluster>` outputs around `newBundle` are the correct minimal shape; preserving `.deb` and legacy `.bin` outputs avoids premature task-110 work.
- Reusing the existing absolute launch chain, `CHROME_DEVEL_SANDBOX`, schema-v2 manifest, package-specific updater refusal, cluster generation, shellcheck discovery, Hydra aggregate, and generic Buildkite upload is preferable to refactoring the accepted `.deb`.
- No application updater change is needed: `manageAppUpdateChannel.ts:187-196` already refuses portable updates before file access when `applicationUpdateMode` is `system-package-disabled`.
- Fedora 43 as the sole supported RPM row and a Fedora omitted-version wallet-only fixture are appropriately narrow; a generic RPM portability layer is unnecessary.
- Keeping raw SELinux/audit/process evidence host-local, narrowing wallet claims to non-mutation unless tracing proves non-inspection, and retaining disabled production guests are correct.
- Separate common, pre, post, preun, and postun scripts are reasonable native boundaries, but they should implement RPM’s actual semantics rather than reproduce the Debian state machine.
- Planning status `draft`, tracker `pending`, and completion blocked on exact-candidate Fedora evidence are truthful once the early feasibility checkpoint and native lifecycle corrections are added.

Approval bar:
- Prove the basic Fedora 43 SELinux transition and bounded startup model early, then freeze supported login contexts, exact comparison semantics, policy scope, and authoritative renderer-label evidence.
- Replace Debian-style rollback claims with an exact RPM/DNF scriptlet ordering, failure-state, interruption, recovery, and final-erase contract; remove the nonexistent purge distinction.
- Define collision-safe, priority-aware, byte-verifiable SELinux module ownership, replacement, restoration, context precedence, and removal.
- Close removal marker/process races and ensure refused erase restores launchability while preserving foreign or modified state.
- Default to the existing `4755` helper contract and defer `0755` userns-only selection until independently proven.
- Freeze NEVRA ordering, deterministic rpmbuild settings, dependency resolution, hash-pinned lifecycle fixtures, and concrete normalized Fedora handoff evidence.
- Extend only the existing probe/package/check/Hydra/Buildkite seams, without duplicate upload machinery or `.deb` refactoring.
- Add explicit final release-candidate RPM lifecycle/SELinux revalidation to task-807 while preserving task-005-b containment certification and every guest/release gate.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-21T16:04:49Z

Plan: Revised only `.agent/plans/dapp-browser-cip30/task-plans/task-109.md`.

Interaction mode: Retained `interactive_validation` with an early Fedora 43 package-equivalent SELinux prototype checkpoint. Phase A limits and continuation evidence are explicit.

Corrections: Defined semantic login/process-context comparison, authoritative renderer-label boundaries, native RPM scriptlet ordering/failure/recovery semantics, priority-aware module ownership, context precedence, `%preun` blocking, narrowed launch-race claims, default helper `4755`, RPM-valid NEVRA/reproducibility/dependencies, and hash-pinned lifecycle candidates.

Scope: Reuses only existing probe, package, check, Hydra, and Buildkite seams. Purge fiction, speculative `0755` selection, duplicate upload machinery, and broader runtime infrastructure are excluded.

Release gate: Task-807 now explicitly owns final release-candidate RPM lifecycle, SELinux, startup, reboot, failure/recovery, and erase revalidation.

Verification: Focused Prettier and `git diff --check` passed. Review logs were not modified.

Outcome: Canonical task-109 iteration-2 plan revised and ready for critique.

Critiquer: Iteration 2
Timestamp: 2026-08-21T16:06:21Z

Outcome: Every iteration-1 blocker is resolved, and the revision introduces no material new issue.

Blocking findings:
- None.

Non-blocking observations:
- The plan now requires the early Fedora 43 package-equivalent prototype and user checkpoint before lifecycle or CI expansion.
- SELinux context semantics, policy scope, module priority/version/fingerprints, ownership markers, effective context precedence, and foreign-state refusal are explicit.
- Native RPM `$1` ordering, failure outcomes, interruption phases, recovery limits, `%preun` blocking boundary, `%postun` residual cleanup, and absence of purge semantics are defined.
- Helper mode defaults to `4755`; `0755` is deferred pending separately reviewed same-artifact userns evidence.
- Renderer-label evidence is confined to a `webContents.getOSProcessId()` transition fixture and does not claim task-005-b certification.
- NEVRA grammar, ordering, deterministic build settings, Fedora dependency resolution, hash-pinned candidates, normalized native evidence, and existing package/check/Hydra/Buildkite seams are specified.
- Task-807 is explicitly assigned final release-candidate RPM lifecycle, SELinux, startup, reboot, failure/recovery, and erase revalidation.

Approval bar:
- Met. The revised plan is implementable truthfully, preserves downstream certification boundaries, and requires Phase A approval before Phase B.

Decision: approved

