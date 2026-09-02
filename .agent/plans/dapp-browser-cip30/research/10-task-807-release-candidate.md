# Task 807 release-candidate gate

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
