# Task 807 release-candidate gate

## Rebase status

This document certifies only the immutable 11.3.0 task-807 artifact below. The `amw/cip30` rebase onto master `6c57eb94753211f66d3a63f49d031bf746044755` is a new 11.4.0 candidate, not the same package or a continuation of its VM evidence. Its historical Windows/macOS waivers, old launcher-policy hashes, and old Linux/Omarchy matrix observations cannot certify rebased packages. The current launch contract is the watchdog-provided `daedalus-config.json` with `dappBrowserPolicy`, `dappSandboxPackageCluster`, and `dappNetwork`, and the Linux package identity remains mandatory before guest construction. Record fresh installed-package VM results and hashes separately before any release approval.

### Rebased 11.4.0 verification (not a release approval)

The current tree is dirty relative to `655488e49d7272148cff769923de5c5e95bec3f0`; its package is not an immutable release artifact. On a fresh disposable Ubuntu 24.04 VM (kernel `6.8.0-142-generic`), an earlier rebased preprod DEB SHA-256 `b00ffea56eae51f33b3c74761280ba160c6895a196e685b037c40eaa6c9b327f` installed after removing an earlier same-version disposable build. The package rejected a same-version replacement with different contents as a downgrade; no production package was modified. The installed watchdog was Electron's parent, pointed at `/opt/daedalus/preprod/config/daedalus-config.json`, and the onboarding UI rendered. That earlier snapshot's installed hostile-guest harness passed its six task-802 matrices, policy variants, sandboxed-renderer assertion, and zero-side-effect/zero-unhandled-rejection checks (`manifestChannels: 64`). The packaged config reported `preprod`, `genesis.json`, and enabled launch. This SHA does **not** represent the current source after the mount-namespace fail-closed change.

After enforcing mount-namespace isolation, the rebuilt disposable preprod DEB SHA-256 `c182b2d76a6d7bf3a4a337edb711ef0ff8f26d93273dea99122fcdf1dcebf51b` installed and started its watchdog/UI. Its installed guest harness refused at the pre-launch canary with `{ "status": "unavailable", "reason": "canary-failed" }` on this host, rather than constructing a guest in the shared mount namespace. The six hostile matrices cannot be claimed as passes for this final package on this VM.

The separate installed Chromium sandbox probe **failed** with `evidence-invalid:shared-mount-namespace` on that mutable VM image, confirming the reason for the final package's fail-closed canary. No positive sandbox certification is claimed. Full Jest passed 175 suites/1,758 tests before the final sandbox regression was added, but the hardware capability-matrix suite failed two dependency-tree identity assertions after a local Yarn install (installed package-tree hashes differ from its frozen evidence). Do not repin those hashes without a reviewed dependency/physical-evidence decision. TypeScript compilation, source lint, watchdog Rust tests (141), Clippy, Linux DEB/RPM/Arch package contracts, the generated policy matrix, and the Electron trusted IPC/session/egress/canary smoke checks passed. Rust formatting and whole-repository Prettier checks still report formatting drift. Windows, macOS, NixOS, RPM/Arch installed VMs, and a clean pinned-image Ubuntu sandbox probe remain unexecuted for this rebased artifact.

The legacy Spectron E2E bootstrap in `tests/setup-e2e.ts` still launches Electron directly and was not run; the new watchdog-parent development launcher was exercised separately with a real watchdog and child config-path precedence. Spectron's direct launch is not evidence for the production parent topology.

### Flight launch investigation, 2026-10-13 (release blocked)

The installed Flight manifest identifies `32f1f5126638f4260c191be7ae41bad0576db028`, package `11.4.0+build87821.git32f1f5126-1`, Ubuntu 26.04. The startup log reports only the relevant sandbox outcome: `unavailable`, `canary-failed`. `git log -S 'rendererEvidence.namespaces.mnt !== mainEvidence.namespaces.mnt'` and the introducing patch identify `37e262d2bf3a3010b4110b444e032f1c52153a8a` as adding unconditional mount-namespace inequality. This predates the backend update; the installed wallet-release commit also changed node/build/config provenance, not sandbox implementation or catalog UI.

APT records recover the immediately preceding installer at `/nix/store/337xb0rj6025x1lx28n7bdzr959pf5ay-daedalus-mainnet-flight-11.4.0+build87815.git74e5469ea-1-deb/daedalus-11.4.0-87815-mainnet-flight-74e5469ea-x86_64-linux.deb`. Its extracted Electron executable and helper are byte-identical to the current installation (SHA-256 `2531faa7c39f1fe4dcd638f2f9522b6260ce215ae67ffa77eb4aa9ab793d6818` and `f165cc9b5c792514027c01dae55bd7e3b2fea809dfcdd808b607a22fb61acd43`). Its extracted payload has no sandbox identity manifest; installation-time identity is not recoverable from that payload alone. The immediately preceding package is not established as the user's last working package. No old backend was run and no user database was accessed.

The recovered prior package's `main/index.js.map` also embeds `dappSandboxAvailability.ts` with the same unconditional mount inequality; this is evidence from the actual packaged source, not just the revision title.

The exact installed runtime reports Electron `41.10.6`, Chromium `146.0.7680.216`, Node `24.18.0`, and available `fs.statfsSync`. Electron's tagged [DEPS](https://raw.githubusercontent.com/electron/electron/v41.10.6/DEPS) pins that Chromium tag, resolved to `ff9214069a282850ff7ebbef6615bb009cf45c57`. Its [namespace sandbox](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/linux/services/namespace_sandbox.cc) defaults to user/PID/network namespaces, not a separate mount namespace. Its [credentials implementation](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/linux/services/credentials.cc) chroots to a helper's procfs `fdinfo`, resets cwd, and waits for the helper to die.

A disposable data-URL canary using the installed Electron, normal helper, isolated profile, no preload and no backend captured different renderer PID/user/network namespaces, shared mounts, NoNewPrivs=1, seccomp=2, zero effective/permitted/inheritable capabilities, and single-entry UID/GID maps. The renderer PID/start identity was unchanged across the lifecycle yield. Both root and cwd symlinks matched the proposed procfs-fdinfo pattern, but each read-only `O_DIRECTORY` open returned `ESRCH` before and after the yield. The mandatory root device/inode, procfs enumeration and cwd comparison therefore cannot be collected. A matching symlink is not containment proof.

The approved fail-closed contingency applies: startup/live validation and the independent probe retain their existing refusal; no shared-mount acceptance branch is introduced. Catalog rejection now has generic English/Japanese feedback, guarded against retired lifecycles, without changing Diagnostics rejection ownership. Launch restoration remains blocked on an approved collectable containment proof. No Ubuntu 26.04 disposable VM was available in either local libvirt connection; installed hostile matrices, installed independent certification and remote catalog launches are unverified for this change. Storybook rendering is UI evidence only, not installed sandbox certification.

Feedback verification: the seven scoped launch/store/catalog suites passed 129 tests; the independent probe selftest and generated `dapp-launch-policy-contract` check passed. Actual Storybook component rendering showed one English failure alert and one Japanese failure alert, with no alert on the ready story. This does not prove installed guest launch.

The feedback-only Flight DEB built successfully at `/nix/store/gx4pvkkqlk3vh99p6nb205lvgdffsb9d-daedalus-mainnet-flight-11.4.0+build0.git32f1f51-dirty-1-deb/daedalus-11.4.0-0-mainnet-flight-32f1f51-dirty-x86_64-linux.deb`, SHA-256 `082fb96f2e56963c4c62f7a040dd0f107e690f28af19e6dacbb51b2fd8cf3821`. This mutable-source build was not installed over the user's package and is not a restored-launch or certified release artifact. Node 11.1.3 and both wallet selections were left unchanged.

### Flight regression correction, 2026-10-13

The subsequent investigation collected the missing metadata without opening Chromium's root for reading: Linux `O_PATH | O_DIRECTORY` holds the actual root object, and bigint `fstat` plus `statfs` through that descriptor identify a procfs directory. The exact installed Electron exposed equal renderer root/cwd device/inode pairs distinct from main; the fdinfo link and identities remained stable across the lifecycle yield. Enumeration through the held root returned `ESRCH`, not a successful empty listing. This is recorded as `deadProcRoot`, never misreported as `empty`.

This specifically follows Chromium's pinned fdinfo-helper chroot, not an arbitrary chroot. Linux [`proc_fdinfo_permission`](https://raw.githubusercontent.com/torvalds/linux/v7.0/fs/proc/fd.c) returns `ESRCH` when the fdinfo inode's task is gone. [`open(2)`](https://man7.org/linux/man-pages/man2/open.2.html) documents metadata-only `O_PATH` descriptors. Chromium's pinned [`SandboxLinux::InitializeSandbox` and `SealSandbox`](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/policy/linux/sandbox_linux.cc) check unexpected open directories and close the temporary proc descriptor; the [options](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/policy/linux/sandbox_linux.h) enable the directory check by default. Its [renderer policy](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/policy/linux/bpf_renderer_policy_linux.cc) delegates path/cwd access to the denying [baseline](https://raw.githubusercontent.com/chromium/chromium/146.0.7680.216/sandbox/linux/seccomp-bpf-helpers/baseline_policy.cc). The ESRCH observation is made by the trusted main process; it is not caused by the renderer's seccomp filter.

Startup/live validation and the independent probe now preserve the separate-mount route while accepting shared mounts only with that kernel-observed fdinfo mechanism, root/cwd confinement, different PID/user/network namespaces, zero effective/permitted/inheritable capabilities and valid, differing single-ID renderer maps. Optional filesystem failures do not break separated-mount acceptance; shared mounts fail closed on missing, malformed or changed proof. Both paths bracket proc samples and repeat complete main/renderer evidence and the full predicate at the final check. Probe output is privacy-safe and normalizes the validated final samples; historical evidence and schema version 2 remain unchanged.

The fresh compiled candidate, running the exact installed Electron and normal helper against the existing root-owned package identity on Ubuntu 26.04, passed startup availability and the unchanged six task-802 hostile matrices, including exact live guest containment and zero privileged side effects/unhandled rejections. It ran with isolated profiles and no wallet/node processes. This is candidate-runtime evidence, not certification of a freshly installed repaired DEB.

The real independent installed probe additionally discovered `evidence-invalid:expected-regular-file`: the installed DEB's `libexec/electron` is a symlink to `bundle-electron/bin/electron`. The DEB builder wrote through the inherited symlink, unlike the RPM/Arch builders, which remove it first. The DEB builder now removes that link before creating the regular wrapper, and its package contract explicitly rejects a symlink. The probe's root-owned regular-file checks were not weakened; the user's installation was not replaced.

The production-manager smoke also exposed a separate close-time exception: the queued bounds debounce called `window.getBounds()` after destruction, matching the user's `/tmp/flight-repair-runtime/flightLaunchSmoke.js` stack. Bounds persistence now cancels on `closed` and checks destruction before reading bounds. Regression coverage exercises pending saves both with and without an emitted close event. The temporary smoke is not the user's wallet process, but the failing callback was production code.

Final source verification passed eight scoped suites/466 examples, independent probe selftest, and `tsc --noEmit`. Scoped ESLint reported no errors (existing/type-signature warnings remain). A separate actual-Electron native-window smoke queued movement/resize saves, destroyed the window, and observed zero bounds writes and zero uncaught exceptions after the debounce interval. No hardware, connection, signing or submission flow was exercised.

The repaired Flight DEB and generated launch-policy/DEB-package contracts built successfully. Exact DEB: `/nix/store/8k4j83isrw62xczp4fq6jvrrxd9d6lwc-daedalus-mainnet-flight-11.4.0+build0.git1a1f815-dirty-1-deb/daedalus-11.4.0-0-mainnet-flight-1a1f815-dirty-x86_64-linux.deb`, SHA-256 `556d8748d75ae1519eed352f3dd78480818b9f87717da11e3364424815d3472d`. The package's embedded validator matches the candidate source bytes. This dirty build has counter 0 and is for disposable first-install verification, not an upgrade over the user's build 87842; a normal clean-commit rebuild supplies the release build identity. No downgrade override was used.

A fresh QEMU Ubuntu 26.04.1 VM (kernel `7.0.0-34-generic`) was provisioned from Canonical's `resolute-server-cloudimg-amd64.img`, SHA-256 `8800651811af9a85465ad1d552add729947bb16488dddb4a9b5305a3d97332b2`, verified against the publisher's checksum before boot. Normal installation of the exact repaired DEB succeeded at `/opt/daedalus/mainnet-flight`: wrapper is a root-owned regular 0755 file, helper is root-owned 4755, and the named AppArmor profile is loaded. The actual installed hostile harness passed all six task-802 matrices, exact guest renderer validation and zero-unhandled-rejection checks.

The production-manager candidate smoke, using that installed executable/helper/config with isolated profiles, returned `available`, opened all five catalog windows at their canonical origins, passed synchronous live sandbox validation and completed teardown without the reported bounds exception. Unfrack.it rendered visibly. Early SteelSwap/Liqwid/FluidTokens captures were uniform backgrounds; a later SteelSwap DOM inspection did not complete within the test deadline. Strike's captured origin returned nginx 404. Thus window construction/visibility/origin/containment/teardown are verified, but full remote-site rendering and an eligible synchronized-wallet catalog UI are not certified. An independent ordinary-browser visit rendered SteelSwap with its own server-unavailable banner. No connect/sign/submit or backend processes were used.

Independent probe certification remains blocked on the stock VM's ordinary-user access to AppArmor's global policy view. The default `ubuntu` username first collided with the public distribution string in the privacy check; a disposable non-colliding username with the same UID 1000 removed that collision. The unchanged probe then reached `renderer-verified` but failed when reading `/sys/kernel/security/apparmor/profiles` (permission denied); the ordinary-user parser also denied policy-cache access. Root installation evidence confirms the loaded profile, but was not substituted for the probe's checks. No privilege, policy, helper or acceptance override was introduced, and no final independent-probe pass or normalized restricted-root certificate is claimed. Other platform/matrix rows remain unverified for this artifact.



Completed: 2026-09-02

## Disposition

The task-807 gate passes for the immutable candidate below. No critical production-dependency advisory remains in the Yarn audit, the pinned backend advertises the exact revision-1 Conway capability contract, tested era and hardware claims match the packaged policy, and the final Windows/Linux candidates passed their applicable lifecycle and hostile-renderer checks.

macOS x64 and arm64 were not executed. The operator explicitly waived only those two rows for this gate. They are recorded as waived, not inferred passes; rollout requiring either macOS architecture must obtain separate evidence.

## Immutable candidate

| Identity | Value |
| --- | --- |
| Daedalus package build source | `1621ceed52049b4daa283251a2dd0f7b1758a430` |
| Package version | `11.3.0` |
| `package.json` SHA-256 | `baa6988796048a83412cce95dc3d85d321f0423b2677e80a3fbcbbf764ea33d1` |
| `yarn.lock` SHA-256 | `41c3a1da2b8f85b6be6eed791bb99fa3f0fa112c81f8852c80a07319c3c4205b` |
| `flake.nix` SHA-256 | `c37f80dfd1b12852a622ab094c54788c178e4760e5ebb7dff6dc6d48d99210de` |
| `flake.lock` SHA-256 | `520c33ea7acbb781c2d8426eacebc40c0749e68a66023dbce8c92e74f11a6a95` |
| cardano-wallet production pin | `bc9b5b9c62cbf526a4806857f7692c3c9d2d2f5e` |
| cardano-wallet Nix narHash | `sha256-cPs4H/6+vRdo42w0R1Hljlg9I9jAOW7a8sX3C/qAuKM=` |
| cardano-wallet test-only evidence commit | `859f89411b0e3a5c6507167554816fb8304ad4a0` on top of the production pin |
| cardano-node pin | `11.0.1` |
| catalog | revision 1, zero entries |
| catalog source SHA-256 | `cb399bc9bbaf3b497da02234d7ba903b0fe249b7be40f69660f9441d7d760961` |
| launcher policy source SHA-256 | `81b941af11f037ba42179fa35687471a9558a9f6d9fb78dcf3ed741f1e65eb17` |

The final task commit differs from the package build snapshot only in documentation and tracker files; packaged runtime inputs are identical. Rollout must consume these exact artifact hashes, not rebuild an arbitrary later tree.

## Package outputs

| Platform/package | Exact version | SHA-256 |
| --- | --- | --- |
| Windows x64 NSIS | `11.3.0` | `f0b73e9586d6064264c1619a156d6590e04ba2defd82537ca756d03cfcd01619` |
| Linux x86_64 DEB | `11.3.0+build86860.git1621ceed5-1` | `269dbef283ca860f111a385225b92ec22ad72fae1d99cc374b4a4ed1089b4e00` |
| Linux x86_64 RPM | `11.3.0-86860.git1621ceed5` | `2a2635a5bccfa5e1b5e7490df577bf6bba3f0c76b49ba096dce780bf499168e2` |
| Linux x86_64 pacman | `11.3.0-1` | `acaa563492d04c53e277a0ff86695758155af30e0c2aa43085aefab78022efb1` |

DEB destructive fixtures were derived from the final DEB payload: old-version SHA-256 `894d554faef4c16f5d5d10bf15abefe4160a7261e1c89e32691ee57c7c3eda1b`; failed-higher-version SHA-256 `2c6f6e71c0e2aa33f3b6661f376d39cbd5d7a5ebe405e986e9eb0e565b034363`.

## Dependency, CVE, era, and hardware review

- Electron is pinned to `41.10.6`; the installed candidates reported Chromium `146.0.7680.216`.
- Signing-facing direct dependencies remain Ledger `8.0.0`, Trezor Connect `9.7.2`, and locked Cardano SDK Core `0.41.4`.
- Security resolutions are exact: `elliptic@6.6.1`, `cipher-base@1.0.5`, `protobufjs@7.5.5`, `form-data@4.0.5`, `sha.js@2.4.12`, `tar@7.5.19`, and `pbkdf2@3.1.3`.
- `yarn audit --groups dependencies --level critical --json` reported 1,207 production dependencies and `critical: 0`, `high: 356`, `moderate: 113`, `low: 54`. Exit 14 reflects lower-severity findings; it is not reported as a clean all-severity audit.
- The exact CBOR/era selection passed 46 Jest examples. Advertised backend capabilities remain revision 1 and Conway-only.
- Hardware capability/certification validation passed 15 examples after rebinding the dependency-graph manifest. Six retained Ledger physical records match the candidate manifest. No hardware connector row is product-enabled: the packaged certified-row set remains empty.

## Backend capability and migration evidence

The Daedalus backend client contract passed 4 focused Jest examples against the exact revision-1 capability shape. The selected production backend remains `bc9b5b9...`; sibling commit `859f894...` changes tests only.

Focused Hspec at the test-only evidence commit passed 4 examples with 0 failures. It proves the V6 target, successful V5-to-V6 commit, byte-exact `.v5.bak`, malformed-live-row transaction rollback to schema 5 without partial V6 tables, and restoration from the backup. Existing durable-submission store coverage separately exercises active-claim conflicts. This closes the retained task-806 V6 evidence gap without changing backend production code or the selected pin.

## Installed platform matrix

All VMs used fresh copy-on-write disks over the exact source images listed below. Wallet sentinels were outside package roots and remained unchanged.

| Row | Source image SHA-256 | Observed host | Result |
| --- | --- | --- | --- |
| Ubuntu 22.04 | `46c966c646ab2e73af6ce8a2bdd20fefbc20f851794bbd46de31d2d1103b72c0` | 22.04.5, kernel 5.15.0-190 | Wallet-only as specified; helper 0755; install/reconfigure/remove/purge/reinstall passed |
| Ubuntu 24.04 | `d0fe84bb5f80853425fa6be28e2c106f30104c3cfe8611933f2e65c9b63f0e30` | 24.04.4, kernel 6.8.0-138 | Supported; AppArmor/helper identity, exact-renderer probe, hostile matrix, lifecycle, destructive recovery, and post-reboot probe passed |
| Ubuntu 26.04 | `8196be9d7958059cb56c6c75c80fdf6cee8a8885bc149ea791d7db1c7ef93035` | 26.04, kernel 7.0.0-30 | Supported; AppArmor/helper identity, exact-renderer probe, hostile matrix, and lifecycle passed |
| Debian 12 | `3ac58d009df21d570bb10811ae1e07afd1125e9669c87897bfb58e52b8f5c937` | 12, kernel 6.1.0-52 | Supported; helper identity, exact-renderer probe, hostile matrix, and lifecycle passed |
| Debian 13 | `85a969b7e99d7c817414136033df18c58d5c45ac8d27bb36e8ccb67173d2d4e3` | 13.6, kernel 6.12.107 | Supported; helper identity, exact-renderer probe, hostile matrix, and lifecycle passed |
| Omitted distribution | `9e857537bdd9f1e6a038bc48ca31d6ee299e52e85f5627245468864469cf58ee` | Debian 11, kernel 5.10.0-46 | Matrix row `null`, wallet-only, helper 0755; lifecycle passed |
| Fedora 43 | `846574c8a97cd2d8dc1f231062d73107cc85cbbbda56335e264a46e3a6c8ab2f` | 43, kernel 6.17.1-300 | Supported; SELinux enforcing, priority-200 module, labels/helper identity, exact-renderer probe, hostile matrix, missing-owner refusal/recovery, erase/reinstall, and post-reboot probe passed |
| Arch Linux | `be8458032f8105e60ee2a3067f950b6e3c007ee51b38dac50e8b48e765561c91` | image 2026.09.01, kernel 7.2.2 | Exact row; userns-only helper 0755, exact-renderer probe, hostile matrix, remove/reinstall passed |
| Omarchy | `2ef8e624aa1bec7e277e28056b8535a6c9373ba48d7ede3f1a01cb6d2373cfb8` | 4.0.2 image 2026.08.31, kernel `7.1.8-arch1-Watanare-T2-3-t2` | Exact corrected row; userns-only helper 0755, exact-renderer probe, hostile matrix, remove/reinstall passed |
| Windows x64 | `a61adeab895ef5a4db436e0a7011c92a2ff17bb0357f58b13bbc4062e535e7b9` | Windows 11 Enterprise Evaluation, build 10.0.26200.0, AMD64 | NSIS silent install/uninstall/reinstall, version/payload identity, wallet preservation, and all task-802 hostile matrices passed |
| macOS x64 | not executed | operator waiver | Waived, not passed |
| macOS arm64 | not executed | operator waiver | Waived, not passed |

The Windows matrix used installed `Daedalus Mainnet.exe` and the packaged harness whose SHA-256 was `56cecfa159e569c533b3dc2352cde214c07ec44cc67b422f9573ead7a03c23ff`. A disposable copy skipped only the Linux package-availability and `/proc` assertions, which correctly return `unsupported-host` on Windows. The IPC, transport, destination-binding, broker-authority/lifecycle-race, switch-variant, and nonpersistent-storage matrices were unchanged and passed with zero privileged side effects and zero unhandled rejections. The installed harness was restored byte-for-byte. This is not a claim that the Linux-only production availability gate enables dApps on Windows.

## Destructive lifecycle evidence

The final DEB passed ordinary upgrade, installed downgrade refusal, failed higher-version post-install rollback, and recovery by reinstalling the exact previously configured version. Helper and manifest hashes remained unchanged through the failed transition. Symlink/foreign/admin-modified policy refusal, statoverride conflict, nosuid failure, interrupted configure recovery, mixed-script unwind, and AppArmor parse/load failure were also exercised fail-closed.

The final RPM passed native install, in-place reinstall, erase/reinstall, SELinux policy ownership and label checks, a deliberately missing policy-owner marker refusal with unchanged payload hashes, marker restoration/recovery reinstall, and reboot persistence.

## Post-change security review

The independent task-807 delta reviewer inspected the dependency/lock changes, fixed-output Electron packaging, Linux package lifecycle changes, hostile-renderer authority, hardware fail-closed gates, and the backend test-only commit. It found no evidence-backed critical or high security or transaction-integrity issue. The task-805 batch-index diagnostic residual is unchanged. The task-806 V6 evidence residual is narrowed to covered and passing migration plus store-conflict tests. The privileged trusted-renderer legacy debt and disabled hardware/product activation remain unchanged.

## Release checklist

- Internal review: [task-805 implementation review](../task-plans/task-805-impl-review.md)
- Independent external audit: [task-806 audit and remediation](../task-plans/task-806-impl-review.md)
- Ubuntu DEB lifecycle handoff: [task-108 evidence](08-task-108-deb-validation-handoff.md)
- Fedora RPM lifecycle handoff: [task-109 evidence](09-task-109-rpm-validation-handoff.md)
- Linux packaging decision and matrix: [system package decision](06-linux-system-package-decision.md)
- Exact era fixtures: [CBOR era coverage](04-exact-cbor-era-coverage.md)
- Backend contract: [cardano-wallet backend contract](03-cardano-wallet-backend-contract.md)
- Hardware contract: [hardware capability contract](07-hardware-wallet-capability-contract.md)
- Hostile-renderer traceability: [threat-model traceability](01-hostile-renderer-threat-model-traceability.md)

## Packaged launcher variants

The reviewed `defaultLauncherConfig` is shared by every cluster and package producer. Mainnet, mainnet-flight, preview, preprod, and selfnode variants therefore carry the same dApp release policy: `globalEnabled=false`, `preferredCatalogEnabled=false`, `diagnosticsEnabled=false`, `cip104Revision=0`, `cip142Revision=0`, and `hardwareConnectorRows=[]`. Catalog revision 1 contains zero entries. No tested package silently activates a dApp, proposed extension, catalog entry, or hardware signing row.

## Subsequent Windows production activation

The task-807 rows above remain the immutable baseline they describe. A subsequent Windows-only production gate supersedes the Windows launcher-policy statement: Linux and macOS remain disabled, while shipped x64 NSIS packages at the exact protected default Program Files root now set `globalEnabled=true` and `diagnosticsEnabled=true`. `preferredCatalogEnabled=false`, `cip104Revision=0`, `cip142Revision=0`, and `hardwareConnectorRows=[]` remain unchanged.

The rebuilt mainnet candidate `daedalus-11.3.0-86861-mainnet-dirty-x86_64-windows.exe` has SHA-256 `2a48d55d5599739f24a1a3156de3a1f14cbfb7f14980abbea533e7e319631398`. On Windows 11 Enterprise Evaluation build `10.0.26200.0` AMD64, the installed package at `C:\Program Files\Daedalus Mainnet` passed exact package identity, Electron 41.10.6 native OS-sandbox/integrity attestation, the unchanged packaged hostile matrices, explicit `--no-sandbox` refusal, and byte-for-byte harness restoration. The installed executable SHA-256 was `17932bde662f2cf4258f10ebbf987f92d590afee11d473b338361a983c6fc26a`. Custom installation roots remain wallet-only.

## Subsequent CIP-104 activation

On 2026-09-03 the deterministic `newm-chain` account-xpub vector superseded
the CIP-104 revision-0 baseline. Every package variant now carries
`cip104Revision=1`; variants whose global dApp launch gate remains disabled
still cannot expose it. Software wallets require a transient spending
password. Hardware wallets require an independently certified and packaged
connector row advertising CIP-104 and use the account xpub retained during
pairing. `cip142Revision=0`, `preferredCatalogEnabled=false`, and
`hardwareConnectorRows=[]` remain unchanged.
