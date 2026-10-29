# Linux Pilot installed acceptance

Task T021 remains pending. Archive and launcher checks do not certify package
manager transactions, real sandbox policy, concurrent desktop identity or rollback.
Use disposable hosts matching the existing package support matrix, with working
desktop sessions for launch checks. Do not install test artifacts onto a developer
host as a substitute.

On 2026-10-29 this agent session had no accessible `/dev/kvm`; no native VM was
booted and no installed acceptance result is claimed. Pilot VM helpers in
`perSystem/vms.nix` launch Preview explicitly and share all three profiles' state
under a disposable per-VM cache directory by default. Existing standalone helper
defaults are unchanged; when using them for migration, point `VM_DAEDALUS_DIR` at
disposable acceptance data, rather than a developer's existing wallet state.

On a KVM-capable host, boot one disposable guest at a time:

```sh
nix run .#test-vm-deb-pilot
nix run .#test-vm-rpm-pilot
nix run .#test-vm-arch-pilot
```

These existing convenience VMs use mutable image URLs. Record the actual guest
identity and validate it against the support matrix; successful VM startup is
not sandbox certification. Pilot has no automatically inherited evidence from
the standalone packages.

All three Pilot helper derivations built successfully, including generated shell
syntax/ShellCheck validation. The generated cloud-init launch and shared-state
mount paths were inspected, and the disposable host-state default was verified
with a cache path containing spaces. No guest boot was attempted.

## Build and inspect

From this worktree:

```sh
nix build --no-link .#checks.x86_64-linux.pilot-profile-configs \
  .#checks.x86_64-linux.pilot-launch-network \
  .#checks.x86_64-linux.pilot-linux-launchers \
  .#checks.x86_64-linux.pilot-linux-package-contract \
  .#checks.x86_64-linux.pilot-linux-artifacts
```

`pilot-linux-package-contract` uses tiny ELF fixtures through the real builders.
`pilot-linux-artifacts` inspects full-payload DEB/Arch packages with fast
compression and the production RPM. Release compression outputs are
`deb-installer-pilot`, `rpm-installer-pilot` and `arch-installer-pilot`.

Full artifact build and archive checks passed on 2026-10-29, from the uncommitted
implementation stacked on `b8c741c49`. These are unsigned development checkpoints:

| Format | Bytes | SHA-256 |
|--------|------:|---------|
| Fast DEB | 560589556 | `a898ea7f93677ef87edef29328b7b906216a593087f6d937892fa95cc77faf91` |
| Production RPM | 350978193 | `e6e3472a85a7e81826b28d7ddef013a8652f7cfc419152007805299d21134587` |
| Fast Arch | 562255349 | `b60d4a3001261910362b862da2a88e55ec87916aec0e532d1bb1610a23b5d138` |

Exact local paths:

```text
/nix/store/gs4dqgz35cmnrks2irxh522pd4bvxgbi-daedalus-pilot-11.6.0+build0.gitb8c741c-dirty-1-deb/daedalus-11.6.0-0-pilot-b8c741c-dirty-x86_64-linux.deb
/nix/store/x9754q47yv818nhw67c4p50kp7h3r8j5-daedalus-pilot-11.6.0-0.git18a43d06b-rpm/daedalus-11.6.0-0-pilot-18a43d06b-x86_64-linux.rpm
/nix/store/kwj81753g4ccqgl1calniv6ksy3yrhh1-arch-installer-pilot/daedalus-11.6.0-0-pilot-18a43d06b-x86_64-linux.pkg.tar.zst
```

## Migration and ownership

Exercise every subset of previously installed Preview, Preprod and Flight native
packages, including none and all three. Record package-manager versions and exact
artifact versions/hashes. Keep standalone Mainnet installed throughout.

1. Confirm direct installation with a conflicting legacy package is rejected.
2. Record all profile state directories, chain paths, configured custom chain
   paths and representative file hashes. Stop legacy applications and uninstall
   their native packages without removing user data.
3. Install Pilot through the native package manager. Confirm the package owns
   `/opt/daedalus/pilot`, all three shortcuts/icons, and its sandbox policy assets.
4. Verify legacy data and custom paths remain intact, and each Pilot shortcut
   adopts the matching profile without creating another chain directory.
5. Verify no superseded package remains able to remove Pilot-owned files.

Automatic legacy replacement is deliberately absent until ownership is proved.

## Launch and sandbox

Launch Preview, Preprod and Flight together, alongside standalone Mainnet. Check
the selected watchdog configurations, node network arguments, sockets, TLS paths,
wallet database paths and logs against the pre-migration values. Flight must
retain its visible mainnet/real-ADA identity. Use empty test wallets for launch
acceptance; no transaction is required.

Confirm distinct running window/taskbar identities and icons, shortcut relaunch
into the same profile, and profile-specific single-instance behavior. Repeat with
an XDG data directory containing spaces. Direct shared executable launch must
explain shortcut recovery instead of silently selecting a profile.

On each supported host, verify package-owned files, sandbox helper owner/mode,
manifest/hash validation and the host's applicable AppArmor/SELinux policy. Run
the existing sandbox diagnostics for each profile. Mainnet must retain its own
package root and existing behavior.

## Maintenance and recovery

For each profile, attempt upgrade/removal with its frontend running, then with
only one of its backend processes running. The transaction must refuse before
replacing shared files. Also exercise repeated launches during maintenance: the
current process guard alone does not prove absence of a launch/transaction race.

After stopping all profiles, upgrade, repair/reinstall and remove Pilot through
each package manager. Verify user data and Mainnet remain intact. Test interrupted
configuration, aborted removal, downgrade/rollback policy and recovery using the
existing package-manager contract. Confirm policy ownership is preserved and
that recovery neither removes another package's policy nor trusts modified
package identity files.

Record host, artifact hashes, commands, observed results and any unresolved
failures here before marking T021 complete.
