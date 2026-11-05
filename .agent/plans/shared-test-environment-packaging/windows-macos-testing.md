# Windows and macOS Pilot test handoff

These are initial test artifacts. Self-updates are disabled until shared release
routing is implemented. Clean installation and native desktop behavior are the
next acceptance gate; upgrade races, rollback, signing/notarization and full
migration certification remain open. Production Mainnet outputs are unchanged.

## CI targets

Hydra exposes these explicit nonrequired artifact jobs:

| Job | Output | Build host |
|-----|--------|------------|
| `installer.x86_64-windows.pilot` | `installer-x86_64-windows-pilot` | x86_64 Linux cross build |
| `checks.x86_64-linux.pilot-windows-archive` | `windows-pilot-archive-check` | x86_64 Linux archive inspection |
| `installer.x86_64-darwin.pilot` | `installer-pilot` | Intel macOS |
| `installer.aarch64-darwin.pilot` | `installer-pilot` | Apple Silicon macOS |

No environment flag is needed. These jobs are explicit flake outputs; `pilot`
must not be added to `installer-clusters.cfg`, whose entries select the legacy
single-environment builders. Linux Pilot artifacts are exposed separately as
`deb-installer.x86_64-linux.pilot`, `rpm-installer.x86_64-linux.pilot` and
`arch-installer.x86_64-linux.pilot`.

PR 3447 evaluation 128644 evaluated all four former `pilot-tests` jobs. Windows and
its archive inspection passed (builds 15406082 and 15406083). Both Darwin
prototype builds failed because codesign rejects symlinks outside an app bundle:
`invalid destination for symbolic link in bundle`. The inappropriate CI signing
and signature-verification steps have been removed: CI produces unsigned
installers, and signing happens only during release. The layout's compatibility
with release signing remains an open release task, not a CI build requirement.
The three evaluation errors were separate: both Darwin `drt-clippy` jobs pulled
in Linux-only pacman through shared integration-test inputs, and `nonrequired`
inherited that failure. Those runtime test inputs now belong only to the package
test derivation, leaving Clippy and dependency compilation independent of them.
After the CI fixes, both Darwin Clippy derivations and all three Linux Pilot
installer jobs evaluate locally; Linux Clippy, lint and all 219 Jest suites pass
(2,925 tests passed, three skipped, six snapshots). Evaluating the full
`nonrequired` aggregate locally still needs a Darwin builder for its Node IFD;
the next Hydra evaluation must validate that aggregate on CI.

Linux CI also runs `pilot-windows-configs`, which compiles the actual NSIS scripts
against a tiny payload and executes the packaging bootstrap through mocked
Electron process/window boundaries. This is separate from the full installer
build and does not certify Windows runtime behavior.
`windows-pilot-archive-check` extracts the real unsigned installer and verifies
the shared application, configs, small frontends and profile icons. Generated
macOS build/package scripts also receive a Linux syntax check, with native
payloads replaced only for that check; it does not execute Darwin code.

Local validation on 2026-10-29: the complete unsigned Windows EXE built and passed
archive inspection. Its development checkpoint filename is
`daedalus-11.6.0-0-pilot-b8c741c-dirty-x86_64-windows.exe`, size 356,467,930 bytes,
SHA-256 `7e67274291ee01a1a72195cb613c91895a99ad4c49de7396d0fa20314244699c`.
The corresponding output is
`/nix/store/h0j5mwj9vf1rr58whhd28xm1d4v9mv6k-win64-installer-pilot`.
CI will use the committed revision in its artifact name. TypeScript and 219 Jest
suites pass (2,925 tests passed, three skipped, six snapshots). Generated Windows
bootstrap/taskbar/configuration tests, real NSIS fixture compiles, macOS script
syntax and changed-file formatting checks pass. No native Windows GUI or native
macOS build/runtime success is claimed here.

## Windows

```sh
nix build .#installer-x86_64-windows-pilot
```

The unsigned installer installs one shared executable/resources/backend tree at
`C:\Program Files\Daedalus Pilot`. Three desktop and Start Menu shortcuts each
choose an explicit watchdog configuration. Small per-profile frontend directories
set application/taskbar identity and load the same shared JavaScript. The loader
rejects direct launch without a matching installed configuration and offers
shortcut repair guidance. Package validation accepts only this explicit layout.

The test installer rejects the legacy Preview, Preprod and Flight installations
and an existing Pilot root. Uninstall those products first while retaining user
data. Standalone Mainnet can stay installed. Installing over Pilot is deliberately
not supported in this slice. The uninstaller checks for all processes under the
shared root and preserves its ownership marker/uninstaller if owned payload files
remain in use. This check alone is not proof against launch/removal races.

On a disposable Windows host:

1. Record the existing profile preferences, consent, wallet and chain paths.
2. Close legacy profiles, remove their installers without deleting data, and
   install Pilot. Confirm the Pilot-owned firewall rule and all six shortcuts.
3. Launch each profile and verify its network, Flight's real-ADA identity, saved
   locale/terms/analytics preferences, custom chain path and existing wallets.
4. Run all three with standalone Mainnet. Reopen each shortcut and confirm only
   its own instance activates. Inspect separate icons and taskbar groups.
5. Pin and unpin before/after first launch; quit and relaunch through each pin.
   Confirm its command targets the matching watchdog config, not the bare shared
   executable. Check display names and icon paths.
6. Try removal with each frontend running, then with only a backend running.
   Verify refusal and successful retry after stopping all profiles. Remove Pilot
   and confirm profile data and Mainnet remain intact. Confirm the installation
   root is removed, including Electron's extension-less `version` file, then
   reinstall Pilot. Removal checks that file before deleting ownership metadata.
7. Record interrupted install/removal and unknown-file behavior as follow-up
   acceptance; do not mark lifecycle certification complete from a clean test.

## macOS prototype

Run on each native macOS architecture:

```sh
nix build .#installer-pilot
# Optional launch from the Nix output before installation:
nix run .#daedalus-pilot-prototype -- preview
```

The prototype `.pkg` installs `/Applications/Daedalus Pilot`, containing Preview,
Pre-Prod and Flight `.app` shells plus one sibling `Shared` payload. Only small
launch binaries, helper bundles, metadata, icons and configs are repeated. The
Electron entry and each helper executable must be below 2 MiB; large framework
binaries, backends and application code are shared. Each app owns its Frameworks
directory and four real helper bundles named after its CFBundleName, with distinct
profile helper IDs. This fixes Electron's `Unable to find helper app` failure
when renamed app shells used Flight's shared helper names. The Linux
`pilot-darwin-helper-layout` check exercises all three profiles, helper metadata,
executable locations and the unmaterialized shells after relocation, with a
mock plist editor; it does not run Electron or certify native macOS behavior.
Each shell keeps the existing launcher/Frontend/config/package identity
layout, so no macOS TypeScript path or network selector is added. Shared regular
files are hard-linked into real directories in each shell, preserving the normal
internal framework version symlinks. External framework symlinks caused native
helper bundle lookup, ICU, pak and V8 resource failures. The
`daedalus-pilot-layout` is the canonical build output: Shared once, profile-owned
shell files, and `pilot-layout.tsv`. Nix generates profile names and concrete
shared paths in that manifest. The installer consumes this output directly;
the optional runnable prototype separately materializes its app views. There is
no build/materialize/copy/prune cycle in the installer dependency chain.
Postinstall probes recursive hard-link copying, including inode identity and
internal symlinks. If system `cp -l` is broken or silently copies files, it falls
back to portable `ln`, `mkdir` and `readlink`, with no Nix dependency on the target
system. Conflicting files are preserved and cause installation to fail.
`pilot-darwin-hardlink-layout` runs on Linux and both Darwin architectures. It
executes the generated postinstall wrapper and tests a renamed profile, failing
and copying-only copy tools, repeated materialization, missing payloads and
conflicts. Darwin runs these cases with system Bash. The native
`pilot-darwin-package` check expands the actual installer, checks its unmaterialized
layout, executes the shipped postinstall against a target path containing spaces,
then verifies contents, executable bits, private metadata, payload inode sharing
and symlinks against the original layout. Package components explicitly disable
bundle relocation, and the check rejects relocation metadata in the actual
package. Native installation exposed why this matters: Installer moved a Shared
helper to an old diagnostic extraction with the same bundle ID. All Pilot
components must remain together inside the product directory.
These checks avoid depending on Nix
archive/cache inode sharing. Archiving the complete hard-linked tree failed
native extraction: expected symlinks became temporary `.BC.D_*` names despite a
successful installer exit status. Always verify the installed framework links.
Each shell contains one profile's configuration files directly in
`Contents/Resources`. Electron, watchdog, node and wallet use that same directory;
there is no second configuration copy under `Resources/profiles`. Electron
resolves `genesis.json` beside its runtime config.

CI assembles unsigned app shells and an unsigned `.pkg`; it does not sign or
verify signatures on the assembled Pilot bundles. Signing and notarization happen
only during release. Package generation
rejects an existing Pilot prototype directory instead of replacing live shared
code. Close all prototype profiles before removing a previous prototype. Existing
standalone app bundles are outside the product directory and are not removed.

Native Apple Silicon verification on 2026-11-04: the rebuilt unsigned package
(566,576,469 bytes) installed into an empty Pilot directory. All 190 archived
symlinks matched the package manifest. Each profile had 37,606 common files sharing
inodes with Shared, real framework directories, intact internal links, correctly
named helpers and one configuration/genesis directory. All three profiles ran
concurrently as the desktop user with healthy GPU, network and renderer helpers
and distinct running LaunchServices bundle identities. Preview reached node
`chainDbReady` and wallet API readiness. Pre-Prod and Flight reported no existing
chain and remained at initial setup; their synchronization was not tested. All
three were stopped cleanly afterward. No installed-file repairs were needed for
this package. Intel, visual Dock/pin/relaunch, signing and notarization checks
remain open.

The subsequent code-quality refactor was also verified on Apple Silicon,
macOS 26.5.1 with system Bash 3.2.57. Linux packaging checks and native fallback
fixtures passed, including failing and copying-only `cp` implementations. The
566,356,278-byte package passed the native extraction/postinstall check and was
then installed into an empty Pilot directory. Independent verification against
the canonical layout passed for all private contents, executable bits, Shared
contents, payload inodes and symlinks. Old diagnostic bundles were left in place
to verify that disabled relocation keeps the product together. The apps were
left stopped; this refactor did not repeat the earlier GUI/startup checks. An
actual older macOS host was not available; fallback failures were injected under
Apple's system Bash.

After a successful native build:

1. Verify all three bundle IDs, displayed names and icons. Check shared payload
   files have matching device/inode numbers in Shared and all three apps; framework
   directories must be real, with only their standard internal symlinks retained.
   Confirm the installed package contains no `/bin/daedalus` payload.
   Confirm each app's default, renderer, GPU and plugin helpers have matching
   profile names and bundle IDs and their executables resolve inside that app.
2. With legacy profiles stopped, open all three launcher apps through Finder,
   alongside standalone Mainnet. Verify the original settings and data paths.
3. Inspect the **running Electron** Dock entries: each must have the matching
   bundle identity/icon and activate its own window. Launcher identity alone is
   insufficient evidence.
4. Pin each running app, quit it and use the Dock pin to relaunch. Confirm the
   same profile configuration/state is selected and its backend is started.
5. Check app movement/removal/repair and a stale Dock pin. Record failures; the
   final relocation/removal and signed package lifecycle remain T031.

Only after both architectures provide the running Dock/relaunch proof should
production signing/notarization and final shared `.pkg` lifecycle be integrated.
