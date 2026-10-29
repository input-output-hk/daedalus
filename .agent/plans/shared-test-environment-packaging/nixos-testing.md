# Run shared Pilot on NixOS

Run from `/home/sam/work/iohk/daedalus/shared-test-environment-packaging`.

Prepare the shared sandbox helper and trusted manifest once for the current
build, then launch Preview:

```sh
nix build .#nixos-sandbox-setup-pilot -o result-pilot-sandbox
sudo ./result-pilot-sandbox/bin/nixos-sandbox-setup-pilot
nix run .#nixos-package-pilot -- preview
```

The setup command copies only the package's sandbox helper (root-owned, mode
0755) and identity manifest into `/var/lib/daedalus/pilot`. Rerun it when changing
the Pilot build so the manifest names the matching Electron executable. It does
not change profile wallet or chain data. Host support and user-namespace sandbox
requirements remain the existing NixOS contract.

Use `preprod` or `mainnet-flight` in place of `preview`. No profile is selected by
default. Each launcher chooses its explicit watchdog configuration; TypeScript
does not select the environment. All three share one Electron/application/backend
payload, preserve their existing data directories and can be launched separately.
`DAEDALUS_ELECTRON_FLAGS` remains available, for example for display-server flags.
Pilot configs explicitly preserve the existing Electron settings directory as
well as wallet/chain data: Linux uses `${XDG_CONFIG_HOME:-$HOME/.config}/Daedalus <profile name>`.
This keeps saved terms/analytics consent and locale settings. Sam confirmed this
preferences-path fix on his NixOS host.

The package also contains `daedalus-preview`, `daedalus-preprod` and
`daedalus-mainnet-flight` executables and corresponding desktop entries. Installing
the package in a Nix profile exposes those entries through the normal NixOS
desktop integration. Existing `nixos-package-preview`, `nixos-package-preprod`,
`nixos-package-mainnet-flight` and standalone Mainnet outputs remain available.

Check direct host launch for each profile, their visible network/Flight identity,
separate running windows and state paths, and coexistence with standalone Mainnet.
The automated package check uses stubs at process boundaries; it cannot certify
a graphical session or the installed host's sandbox behavior.
