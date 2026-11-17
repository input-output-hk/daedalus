# Validation record

Date: 2026-11-09
Daedalus base: `8d961fa48`
Quetzal candidate: `a5af746d46ebd643599aec436986afe26db34cb9`
Quetzal binary SHA-256:
`800300579511bfa135dfc4d220f032f0ebeeb902e4cddbf7d0d17563ff402219`

## Passed

- Spec Kit prerequisite helper resolves the correct sibling worktree and existing
  spec, plan and tasks.
- `nix flake lock`: only one input added, the immutable non-flake Quetzal source.
- `nix eval --json .#devShells.x86_64-linux --apply builtins.attrNames`:
  both opt-in shells appear alongside normal shells.
- `nix build .#quetzal .#checks.x86_64-linux.quetzal-config-contract --no-link -L`:
  compiled the real release binary and passed both networks' config checks.
- `nix build .#quetzal-preview-config .#checks.x86_64-linux.quetzal-config-contract --no-link --json`:
  passed after final state-isolation changes.
- Contract assertions cover executable selection, isolated node/wallet/frontend/
  log/TLS paths, expected genesis alias, launch args, health configuration,
  source revision, disabled dApp capability, and reference shell defaults.
- `git diff --check`: passed.

Package output:
`/nix/store/h2lrik10kwhlp02ja86ck43ir56nsa9s-quetzal-0.1.0-a5af746d4`
Preview config output:
`/nix/store/6jrf7zzqkfr35v6xzlbsgdj4kwwjcyn9-cfg-files`
Contract output:
`/nix/store/8rbdc3lnsm5iz479srsavj5ymxmrxk00-quetzal-config-contract`

## Failed: real wallet launch contract

Executed the built binary, independently of TLS and without starting a node or
opening a wallet database:

```text
quetzal serve --shutdown-handler
Error: missing value for --shutdown-handler
exit status: 1
```

Daedalus unconditionally injects this flag after its TLS arguments and uses
stdin EOF for shutdown. Quetzal's initial parser expects a value for every flag,
then rejects unrecognized options. Its server also lacks stdin-EOF handling.
Even assuming TLS is fixed, the initial candidate cannot launch under watchdog.

Sam has been asked whether to include shutdown compatibility upstream or
explicitly authorize a Daedalus lifecycle adaptation. No alternative lifecycle,
HTTP fallback, proxy or upstream patch was implemented after this failure.

## Pending actual live qualification

- Authenticated node/backend readiness and renderer-client requests.
- Disposable wallet creation, address retrieval and restart persistence.
- Orderly supervised shutdown with no orphaned node/wallet/Electron processes.
- UI sync/create/restore/receive/settings walkthrough.
- Funded transactions, staking/governance operations and physical hardware.

TLS compatibility is assumed per Sam's instruction and will be supplied upstream.
It was not retested or bypassed. Live tests await a compatible pinned candidate.
No renderer/watchdog application source changed, and no commit was created;
TypeScript/Jest and full Electron builds were not run for this Nix-only change.
Upstream workspace tests were not rerun: this package builds the actual binary,
while the existing upstream conformance report remains separate evidence.

## Final review

- `nix fmt -- --fail-on-change perSystem/common.nix perSystem/devshells/daedalus.nix perSystem/quetzal.nix flake.nix`: passed, no formatting changes required.
- Spec/plan/task consistency reviewed: all functional requirements map to
  implementation tasks or explicitly pending live checks. No constitution
  exceptions or unrecorded transport/lifecycle adaptations.
- `update-agent-context.sh codex`: completed; inspected the diff and preserved
  existing project instructions. The explicit Spec Kit/quickstart pointer is
  the only AGENTS.md addition.
- No application data, wallet keys, or source files in master were modified.

## Updated candidate after upstream mTLS push

Fetched latest master and updated `flake.nix` plus `flake.lock` to
`61fb677d711a021c547e29e32648d0e077b9e95f`. This section supersedes the initial
TLS prerequisite above. Earlier failed-attempt evidence is retained.

- PASS: release binary, preview configuration and both-network configuration
  contract rebuilt with `nix build .#quetzal .#quetzal-preview-config .#checks.x86_64-linux.quetzal-config-contract --no-link --json -L`.
- PASS: authenticated `GET /v2/wallets` returned HTTP 200 and an empty JSON list
  using TLS 1.3 with `TLS_AES_256_GCM_SHA384`, matching renderer transport settings.
- PASS: the TLS listener rejected a missing client certificate.
- PASS: the TLS listener rejected a client certificate signed by an unrelated CA.
- PASS: the private temporary wallet/PKI fixture was removed and its Quetzal child
  exited after test cleanup. No node was attached during this transport check.
- PASS: Nix formatting and `git diff HEAD --check`.
- STILL FAILS: `quetzal serve --shutdown-handler` reports
  `Error: missing value for --shutdown-handler`. Source inspection also confirms
  no stdin-EOF handler. The default watchdog launch remains incompatible.

Package output:
`/nix/store/n5g3cl6j9m36vqgij1b2118vmwzhf9vn-quetzal-0.1.0-61fb677d7`
Binary SHA-256:
`7b6d6fbcc6e65bd27d06825ed7777113f07a1b28a208860cd4157e64f0e5b99e`
Preview configuration:
`/nix/store/rdn03rz8mi25s34m35r6ijgfxhhsqw4y-cfg-files`
Configuration contract:
`/nix/store/4n70m7byrdibm17m00ncf7nhbnh4yvrx-quetzal-config-contract`

Transport check command (session-local test fixture):

```bash
nix shell --inputs-from . nixpkgs#nodejs nixpkgs#openssl --command \
  node /tmp/daedalus-002-quetzal-mtls-check.cjs \
  /nix/store/n5g3cl6j9m36vqgij1b2118vmwzhf9vn-quetzal-0.1.0-61fb677d7/bin/quetzal
```

This was an isolated real-server transport check, not a renderer/UI, native-node,
watchdog-lifecycle or persistence qualification. Those tasks remain pending.
No backend fallback or lifecycle change has been made.

## Native watchdog run — f72b38b25 (2026-11-10)

Candidate: `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7`.
Binary SHA-256:
`da5799929020d9d0d965d00e7342802f12e936deee590847eacae885c494d4d5`.
Package: `/nix/store/fcq4rs00zrwk8rkxmd54idp3329rhsjm-quetzal-0.1.0-f72b38b25`.

The standard sync-tolerance/token-metadata arguments were restored. The obsolete
Shelley genesis alias was removed. Both test-network configuration contracts
passed with the restored arguments.

### Daedalus development configuration defect found and fixed

The first real launch failed before the API listener: the development config
passed `nodeConfigFiles/genesis-byron.json`, but that derivation contains
`genesis.json`. The node was using its own valid original genesis path, so it
started while the wallet exited with ENOENT. Corrected the development-shell
Byron path for both reference and experimental backends; packaged paths retain
their existing alias. Added actual genesis-file existence and magic checks for
preview and preprod to the generated-configuration contract. Those checks pass.

Fixed preview configuration:
`/nix/store/mw42wkpqc75wjzfrckibzicgz62q0dhs-cfg-files`.
Contract: `/nix/store/fx9i4xyin7gky8pf3sga8r6my8sb8n14-quetzal-config-contract`.

### Passed against real processes and renderer HTTP request functions

- Built cardano-watchdog; its package's unit/integration tests passed.
- Repeated real-server mTLS checks: trusted certificate succeeds with the
  renderer's TLS 1.3 cipher; missing and unrelated-CA certificates are rejected.
- A fresh preview cardano-node 11.1.3 and real Quetzal start under the unchanged
  watchdog with dynamically assigned API port and watchdog-generated mTLS PKI.
- `/network/information` reports preview magic 2 and `syncing` with numeric
  progress after the startup window. The node was in the Alonzo era.
- Daedalus's actual `createWallet`, `getWallets` and `getAddresses` renderer
  request functions work. The disposable phrase comes from Daedalus's own
  `generateMnemonic(24)`; no recovery words or password were printed.
- The new wallet has a 40-hex ID and at least 20 `addr_test1` addresses.
- Watchdog wallet-only restart preserves identity and exact addresses.
- Full watchdog/node/wallet shutdown and relaunch preserves the wallet.
- Quetzal logs `stdin EOF` and `Quetzal shutdown complete`; all captured child
  PIDs are gone after stopping. Node shutdown reports no forced kill.

The focused headless test removed Electron and Mithril from a temporary runtime
config, selected an unused node TCP port, and used a fresh private /tmp state.
It did not modify the packaged watchdog or renderer source. The renderer's
HTTP request functions were loaded through SWC with existing master-worktree
node_modules; full AdaApi domain/store mapping and graphical UI were not tested.

### Still fails: native protocol parameters on a fresh Alonzo node

`GET /v2/network/parameters` returns:

```text
state query failed: failure decoding CBOR data
```

This is an API error from the real backend, preserved as a failed check. The
harness continues independent wallet/restart checks and exits with status 1;
it does not suppress the error or claim the entire run passed. See
[upstream-parameters-handoff.md](upstream-parameters-handoff.md).

Session-local check command:

```bash
nix shell --inputs-from . nixpkgs#nodejs --command \
  env NODE_PATH=/home/sam/work/iohk/daedalus/master/node_modules \
  node /tmp/daedalus-002-quetzal-native-check.cjs \
  /nix/store/mw42wkpqc75wjzfrckibzicgz62q0dhs-cfg-files \
  /nix/store/g25xmgx498l0b2l9kpvq1qqpqxqsv87f-cardano-watchdog-0.1.0/bin/cardano-watchdog
```

Private last-run logs/state are at `/tmp/daedalus-quetzal-native-47lmR0`.
Keep that directory private: it contains disposable wallet state and TLS keys.
Earlier attempts were stopped and their supervised children cleaned up.

The independent mnemonic-helper preflight originally selected an unavailable
`@polkadot/util-crypto` package. Corrected the harness to use the actual Daedalus
mnemonic generator; no package was installed and this was not a backend failure.
The standalone transport harness also needed the now-mandatory explicit
`--mainnet` network flag; the corrected same test passed.

### Remaining scope

Graphical UI checks remain unexecuted: this session has neither DISPLAY nor
WAYLAND_DISPLAY. Funded receive/send/delegation, token-registry fixture behavior,
physical devices and realistic full-chain restart performance remain unqualified.
The native network-parameters failure must be resolved or explained upstream
before the live compatibility run can be marked fully successful.

## Packaged NixOS launcher variant — 2026-11-10

Added `nixos-package-preview-quetzal` and `nixos-package-preprod-quetzal` using
the existing NixOS package factory. These package the production frontend and
launch watchdog as parent of Electron, node and Quetzal. The frontend gets the
actual Quetzal version; generated config selects libexec/quetzal and disables
dApp capabilities. No `yarn dev` or shell PATH setup is needed to run the output.

- PASS: main-program evaluation returns `daedalus-preview-quetzal` and
  `daedalus-preprod-quetzal`; the ordinary preview remains `daedalus-preview`.
- PASS: generated-config contracts for both package variants and both development
  shells, including selected executable, source identity, frontend data paths,
  disabled dApp capabilities, and normal reference defaults.
- PASS: formatting and diff whitespace checks.
- PASS: full preview package build, including native Electron modules and
  production main/renderer bundles. An upstream GitHub archive download
  initially returned HTTP 429; Nix's own retry continued successfully to other
  dependencies. No source or dependency fallback was introduced.

The launcher sets its state root to `Daedalus-Quetzal`, with separate wallet,
chain, TLS, logs and frontend user data. Existing cardano-wallet databases are
not imported. The packaged graphical launch remains unexecuted in this session
because no desktop display is available. The previously recorded native Alonzo
network-parameters API failure remains a distinct upstream qualification issue.

Built preview package:
`/nix/store/8vnl5g6drwwp7jn23vwwsh2d17q5m7cx-daedalus-nixos-preview-quetzal`.
The assembled launcher and frontend wrapper pass `sh -n`. Executable checks
confirm the launcher, watchdog, node, Quetzal and frontend exist. The package's
actual config selects `libexec/quetzal`, its symlink resolves to the pinned
Quetzal binary, and the frontend wrapper execs the production Electron build.
The launcher exports the isolated `Daedalus-Quetzal` root and execs watchdog.
The production main bundle is present. No graphical launch was attempted.

## Shared packaged state correction — 2026-11-10

Sam explicitly superseded the isolated packaged-state design. The launcher now
uses the normal `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus/<network>` root,
normal frontend user data, and shared node chain/log/TLS/watchdog state. Quetzal's
`--database` points at `wallets/quetzal.sled/`, a sled directory with its own
nested Byron store. The reference wallet's SQLite files remain siblings.
Development shells retain their isolated root, with the same nested sled path.
The project constitution/spec/plan/contracts and quickstart were updated to
record the explicit user requirement.

- PASS: both networks' development and packaged config contracts; packaged
  Electron args match the ordinary package and the database path is nested.
- PASS: full `nixos-package-preview-quetzal` rebuild and assembled wrapper syntax.
- PASS: assembled launcher exports the ordinary `Daedalus` state root and execs
  watchdog; actual package config selects libexec/quetzal and the nested DB.
- PASS: real Quetzal startup with authenticated TLS and EOF shutdown alongside a
  valid SQLite fixture. Its sibling file's bytes and SQL row were unchanged;
  the only added entry in the parent `wallets` directory was `quetzal.sled`.
  Both primary and Byron sled stores stayed inside that directory.
- PASS: final diff whitespace check.

Built package:
`/nix/store/0fq3mcr7mmwbafj1aw3jfqwjrjgdppgb-daedalus-nixos-preview-quetzal`.
Contract output:
`/nix/store/lzbq3bssn91wpzkfmisdpym11m4ws9zy-quetzal-config-contract`.
The shared-wallet-directory fixture used the real pinned Quetzal binary via:

```bash
nix shell --inputs-from . nixpkgs#nodejs nixpkgs#openssl nixpkgs#sqlite --command \
  node /tmp/daedalus-002-quetzal-shared-wallets-check.cjs \
  /nix/store/fcq4rs00zrwk8rkxmd54idp3329rhsjm-quetzal-0.1.0-f72b38b25/bin/quetzal
```

No existing installed state was opened during verification; only a disposable
private fixture was used. Sharing application state does not itself import
SQLite wallet records. Close the existing same-network Daedalus before switching
backends because both launches intentionally use the same watchdog instance lock.

## Upstream conformance update — 2026-11-17

Updated Quetzal from `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7` to upstream
master `84f2f4d691df3a32928766733def609164888475` after Sam reported that upstream
conformance now passes. `git ls-remote` confirmed HEAD/master at this revision.
The GitHub API proxy denied access to commit metadata for the Quetzal repository;
no upstream conformance receipt was independently retrieved or rerun.

- PASS: `nix flake lock`; Nix reported only the Quetzal input updated.
- PASS: `nix build -L .#quetzal .#checks.x86_64-linux.quetzal-config-contract .#nixos-package-preview-quetzal --no-link --json`.
  This compiles the new backend and production Electron bundles and checks both
  preview/preprod development and packaged configurations. Webpack reported four
  warnings for the renderer bundle, with no build errors.
- PASS: packaged launcher `sh -n`, executable Quetzal symlink resolving to the new
  binary, and assembled configuration revision/backend/database/dApp assertions.
- PASS: built binary `--help` lists the expected TLS, network, metadata,
  sync-tolerance and stdin-EOF shutdown options. This is CLI inspection, not a
  live transport/lifecycle test.
- PASS: `nix fmt -- --fail-on-change flake.nix` and `git diff --check`.

Quetzal binary:
`/nix/store/ah0vm3b99lyw5pj3s7ks5xl8mzy823s1-quetzal-0.1.0-84f2f4d69/bin/quetzal`.
SHA-256: `0d8a52e4d1e0eb1e1308f4a72b00e3d664c71aad27d5c3aa6b03bc8a11f5e951`.
Configuration contract:
`/nix/store/3ppck243v3m64wj7f7n4zw15rx4nsf4n-quetzal-config-contract`.
Preview package:
`/nix/store/6khk5nhi9d82ijdmzpny350rmw3i3rfa-daedalus-nixos-preview-quetzal`.

No live node/backend/renderer requests or graphical UI checks were rerun for
this revision; this session has no DISPLAY or WAYLAND_DISPLAY. In particular,
the earlier native `/network/parameters` failure is historical evidence for the
previous pin, and its resolution in Daedalus remains unverified here. Funded
workflows and hardware checks remain pending. Existing application state was
not opened, and no commit or push was made.
