# Feature Specification: Test Daedalus with Quetzal

**Feature Branch**: `002-quetzal-wallet`
**Created**: 2026-11-09
**Status**: Specified
**Input**: Test Quetzal as a cardano-wallet replacement in a worktree using Spec Kit.

## User Scenarios & Testing

### US1 — Launch an isolated Quetzal-backed Daedalus (P1)

A developer explicitly selects Quetzal for preview or preprod while normal
Daedalus shells and release packages retain their existing backend.

**Independent test**: Evaluate the selected configuration and launch it through
cardano-watchdog with a real node and Quetzal executable.

**Acceptance scenarios**:
1. Given a Quetzal development shell, when Daedalus starts, then watchdog launches
   Quetzal against the selected node socket and a fresh, isolated database.
2. Given the usual development shell or installer, when its configuration is
   generated, then its existing cardano-wallet backend and state paths remain.
3. Given Quetzal selection, when the watchdog generates certificates, then the
   backend and frontend use the existing mutual-TLS contract.

### US2 — Exercise existing wallet workflows (P1)

A tester uses Daedalus's existing API client and UI to inspect network status,
create a disposable wallet, retrieve addresses, and reopen it after a restart.

**Independent test**: Execute live API smoke checks and record manual UI results.

**Acceptance scenarios**:
1. Given the live backend, when the existing API client requests network
   information, network parameters and wallet lists, then responses are accepted
   without API rewrites, including while the fresh node is initially syncing.
2. Given a disposable wallet, when it is created and its addresses requested,
   then both operations succeed and it remains after backend restart.
3. Given shutdown or restart, when the watchdog stops the wallet, then it exits
   within the existing stop timeout without orphaned processes.

### US3 — Reproduce and report compatibility (P2)

A developer can reproduce the experiment from a pinned source revision, and
separate upstream conformance claims from actual Daedalus integration evidence.

**Independent test**: Follow quickstart and inspect recorded commands/results.

### Edge cases

Unsupported platforms; missing executable; unsupported CLI options; missing TLS
support in the selected upstream revision; wrong genesis network; conflicting
ports; unavailable node; incompatible old wallet databases; backend restart.
Failures must remain visible and must not select HTTP or the reference backend.

## Requirements

- **FR-001**: Provide explicit opt-in Quetzal development environments for preview
  and preprod on x86_64 Linux, the currently supported upstream build platform.
- **FR-002**: Pin the exact Quetzal source revision and build it with Nix.
- **FR-003**: Preserve normal shell and release configuration behavior.
- **FR-004**: Packaged variants MUST reuse the selected network's existing node,
  frontend, TLS and log state. Store Quetzal data only at `wallets/quetzal.sled/`
  alongside unchanged SQLite files. Development shells retain isolated state.
- **FR-005**: Preserve watchdog ownership, mutual TLS, health probes, restart,
  and orderly shutdown. No proxy or HTTP fallback.
- **FR-006**: Use the existing renderer API client without compatibility shims.
- **FR-007**: Document reproducible startup and smoke/UI checks, including
  backend revision, actual results, and outstanding upstream prerequisites.
- **FR-008**: Do not migrate cardano-wallet databases or enable dApp launch based
  on Quetzal's HTTP conformance claims.

## Success Criteria

- **SC-001**: Configuration checks prove Quetzal selection, shared packaged state
  and a separate sled database on both test networks, with unchanged reference
  defaults. A real startup/shutdown fixture proves sibling SQLite files unchanged.
- **SC-002**: A live Quetzal/node pair passes network information and parameters, wallet list,
  disposable wallet creation and address retrieval through Daedalus's client.
- **SC-003**: The disposable wallet survives restart, and shutdown leaves no
  wallet or node child running.
- **SC-004**: The quickstart identifies commands and evidence for every result,
  marking any unexecuted UI/hardware/funded-transaction scenarios explicitly.

## Assumptions and clarification

- 2026-11-09: Sam will fix upstream TLS; assume the existing cardano-wallet
  `--tls-ca-cert`, `--tls-sv-cert`, `--tls-sv-key` contract works. Do not implement
  TLS or a transport workaround in Daedalus. Actual live results still require
  a revision containing that fix.
- 2026-11-10: TLS was qualified at `61fb677d7`. Candidate `f72b38b25` adds
  standard CLI compatibility, orderly stdin EOF, metadata fetching and native
  network progress. Test the ordinary launch options without compatibility shims.
- Preview is the first live target; preprod is also configurable. Mainnet and
  production packaging are outside this experiment.
- API conformance is a hypothesis for integration, not evidence of compatible
  CLI behavior, lifecycle, database formats, or Daedalus-specific extensions.
- Use disposable test wallets; funded signing and physical hardware checks may
  require user fixtures and are reported separately.

## Packaged launch clarification — 2026-11-10

Sam wants the `nix run .#nixos-package-preview` launch path with Quetzal, rather
than a `yarn dev` workflow. Add `nixos-package-preview-quetzal` (and the matching
preprod variant) using the existing NixOS package factory, watchdog and bundled
production frontend. Preserve the ordinary outputs. These remain opt-in local
Nix packages, not new release installers. Per Sam's subsequent clarification,
reuse `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus/<network>` and the normal
Electron user-data location. Keep Quetzal's sled directory at
`wallets/quetzal.sled/`, leaving SQLite files untouched. This is directory/state
sharing, not SQLite wallet-record import.
