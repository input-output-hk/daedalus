# Run Daedalus with Quetzal

Run from `/home/sam/work/iohk/daedalus/002-quetzal-wallet` on x86_64 Linux.

## Qualify the candidate

The current pin is `84f2f4d691df3a32928766733def609164888475`, updated after Sam
reported upstream conformance passing. Daedalus-specific validation for this pin
is recorded in `validation.md`. A fully launch-compatible revision must support:

- `serve --node-socket PATH --database PATH --testnet BYRON_GENESIS --port PORT`
- `--tls-ca-cert PATH --tls-sv-cert PATH --tls-sv-key PATH`, including client
  certificate authentication and TLS 1.3 (the current renderer's contract).
- `--shutdown-handler`: stay alive with stdin open, then flush state and exit
  promptly when watchdog closes stdin. Do not interpret it as a valued option.

The previous pin passed mTLS and orderly stdin-EOF shutdown checks. Native
watchdog/client qualification is recorded by revision in `validation.md`.
To update, set `inputs.quetzal.url` in `flake.nix` to the full reviewed
commit and run `nix flake lock`. Only the Quetzal input should change in the lock.
Use a committed Git revision for the candidate so its source identity is
available to the package version and generated configuration.

```bash
nix build -L .#quetzal .#checks.x86_64-linux.quetzal-config-contract --no-link
nix build -L .#quetzal-preview-config --out-link result-quetzal-preview-config
```

The config uses the ordinary Daedalus `--sync-tolerance 300s` and selected-network
`--token-metadata-server` arguments. The earlier Shelley genesis filename alias
has been removed; Quetzal obtains the native era/genesis information from the
node without requiring a neighboring file with a particular name.

## Start the experiment

Use the packaged NixOS launcher from a desktop terminal:

```bash
cd /home/sam/work/iohk/daedalus/002-quetzal-wallet
nix run .#nixos-package-preview-quetzal
```

For preprod, use `.#nixos-package-preprod-quetzal`. This uses the existing
package launcher, which starts watchdog as the parent of Electron, cardano-node
and Quetzal. It does not require entering a development shell or running Yarn.
The production frontend is compiled with the selected Quetzal version.

Close the existing preview Daedalus instance first: the shared network state
uses the same watchdog instance lock. The Quetzal package reuses
`${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus/preview` (or `preprod`), including
existing chain data, TLS, logs and normal frontend settings. Its own database is
`wallets/quetzal.sled/` (sled uses a directory, with a nested Byron store).
Existing SQLite files remain alongside it, unchanged. Sharing the directory does
not import SQLite wallet records into sled. The ordinary package outputs retain
their original backend and database paths.

### Development-shell alternative

```bash
nix develop .#quetzal-preview
# The shell installs JS dependencies and rebuilds Electron native modules.
nix build .#cardano-watchdog --out-link result-watchdog
export PATH="$PWD/result-watchdog/bin:$PATH"
yarn dev
```

Use `.#quetzal-preprod` for preprod. The usual `.#preview`, `.#preprod`, and
release outputs retain cardano-wallet. The Quetzal binary is selected by an
absolute Nix path in the generated watchdog config; there is no backend fallback.

Keep each shell in its worktree root so `DAEDALUS_DEV_ROOT` points there. State
is `state-quetzal-preview/` or `state-quetzal-preprod/`, including node chain,
wallets, TLS, logs, and Electron `frontend/` user data. No installed wallet or
existing development database is reused. Do not copy cardano-wallet databases
into these directories. Node synchronization or Mithril bootstrap may take time.

## Live checks

Record the Daedalus commit, exact Quetzal revision and binary hash, network,
commands, and results in `validation.md`. Keep generated keys and runtime data
out of Git. Upstream conformance receipts do not substitute for these checks.

1. Confirm `cardano-watchdog` owns the node, Quetzal and Electron processes and
   logs the selected testnet. The UI should leave the backend connection screen.
2. Confirm network information, parameters and clock render correctly; inspect
   node and wallet sync progress independently.
3. In DevTools, use the actual initialized client:

   ```javascript
   const api = window.daedalus.api.ada;
   await api.getNetworkInfo();
   await api.getWallets();
   ```

   Inspect the client methods if the console API changes; direct HTTPS requests
   alone do not qualify renderer mapping or UI behavior.
4. Create a fresh disposable wallet through the UI. Confirm it appears in the
   list, open Receive, and verify addresses. Never paste recovery words or a
   spending password into shared logs or validation receipts.
5. Open transactions, assets, staking and governance views. Record response
   shape/semantic errors separately from pending chain synchronization.
6. Quit normally, confirm all supervised children exit, and restart. Confirm
   the disposable wallet and its addresses persist. Test the ordinary restart
   action and backend recovery; record any stop-timeout forced kills.
7. Restore a disposable test wallet and check its name, balance, addresses and
   transaction history against the reference on the same network.
8. Funded sending/delegation and physical hardware signing require dedicated
   fixtures/devices. Mark them unexecuted until exercised; do not infer success.

Quetzal configurations disable the dApp connector and its hardware capability
policy because this backend has not qualified those Daedalus-specific extensions.

## Failure handling

Retain logs from `state-quetzal-<network>/Logs/pub/`. If launch or a test fails,
record the exact command/error and ask Sam before changing approach. Do not
switch to HTTP, a proxy, a different backend, or silently patch the upstream
candidate. UI, persistence and shutdown qualification remain pending until a
real compatible candidate can run.
