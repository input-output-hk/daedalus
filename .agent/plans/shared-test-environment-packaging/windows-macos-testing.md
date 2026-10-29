# Windows and macOS Pilot test handoff

These are initial test artifacts. Self-updates are disabled until shared release
routing is implemented. Clean installation and native desktop behavior are the
next acceptance gate; upgrade races, rollback, signing/notarization and full
migration certification remain open. Production Mainnet outputs are unchanged.

## CI targets

Hydra exposes these explicit nonrequired artifact jobs:

| Job | Output | Build host |
|-----|--------|------------|
| `pilot-tests.windows` | `installer-x86_64-windows-pilot` | x86_64 Linux cross build |
| `pilot-tests.windows-archive` | `windows-pilot-archive-check` | x86_64 Linux archive inspection |
| `pilot-tests.darwin-intel` | `installer-pilot-prototype` | Intel macOS |
| `pilot-tests.darwin-arm` | `installer-pilot-prototype` | Apple Silicon macOS |

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
   and confirm profile data and Mainnet remain intact.
7. Record interrupted install/removal and unknown-file behavior as follow-up
   acceptance; do not mark lifecycle certification complete from a clean test.

## macOS prototype

Run on each native macOS architecture:

```sh
nix build .#installer-pilot-prototype
# Optional launch from the Nix output before installation:
nix run .#daedalus-pilot-prototype -- preview
```

The prototype `.pkg` installs `/Applications/Daedalus Pilot`, containing Preview,
Pre-Prod and Flight `.app` shells plus one sibling `Shared` payload. Only small
launch binaries, metadata, icons and configs are repeated. The Electron entry
executable must be below 2 MiB; frameworks, backends and application code are
shared. Each shell keeps the existing launcher/Frontend/config/package identity
layout, so no macOS TypeScript path or network selector is added.

Native builds ad-hoc sign each shell and run strict deep signature verification.
This is not a Developer ID signed/notarized distribution. Package generation
rejects an existing Pilot prototype directory instead of replacing live shared
code. Close all prototype profiles before removing a previous prototype. Existing
standalone app bundles are outside the product directory and are not removed.

The Linux session cannot build the Darwin dependencies required even during Nix
evaluation. Native CI must establish successful build/signature verification
before calling the macOS prototype runnable. No macOS runtime result is claimed.

After a successful native build:

1. Verify all three bundle IDs, displayed names, icons, signatures and symlink
   targets. Confirm the installed package contains no `/bin/daedalus` payload.
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
