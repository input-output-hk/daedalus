# Implementation Plan: Quetzal-backed Daedalus experiment

**Branch**: `002-quetzal-wallet` | **Date**: 2026-11-09 | **Spec**: [spec.md](spec.md)

## Summary

Add opt-in NixOS packages and development shells that substitute a pinned Quetzal
executable in the generated watchdog config. Keep existing renderer and watchdog
transport/lifecycle contracts. Packaged variants share normal network/frontend
state and use `wallets/quetzal.sled/` beside untouched SQLite files; development
shells remain isolated. Disable unqualified dApp capabilities.

## Technical Context

- Languages: Nix configuration, existing Rust backend and watchdog, TypeScript
  renderer API functions for live validation.
- Dependencies: existing crane/fenix toolchain and cardano-node 11.1.3;
  Quetzal source input pinned in flake.lock.
- Storage: sled at `wallets/quetzal.sled/`; packaged variants share the existing
  node chain and frontend settings, while development shells remain isolated.
- Platform: x86_64 Linux; preview and preprod networks only.
- Tests: Nix generated-config contracts; real binary CLI check; live API/client
  smoke test, persistence/restart and UI walkthrough after upstream prerequisites.
- Scope: developer experiment; no production packaging, database migration,
  transport rewrite, upstream Quetzal edits or dApp certification.

## Constitution Check

Before research: PASS. Nix builds, explicit selection, full-node architecture,
mutual TLS and state isolation are required. No application rewrite.
After design: PASS. Renderer and watchdog remain unchanged; upstream TLS and
stdin shutdown support are explicit prerequisites for live qualification.

## Project Structure

- `flake.nix`, `flake.lock`: immutable Quetzal source input.
- `perSystem/quetzal.nix`: Linux package, configuration outputs and contract check.
- `perSystem/common.nix`: optional experimental config parameter; guarded against
  release/non-Linux/mainnet selection; separate state, args, identity and policy.
- `perSystem/devshells/daedalus.nix`: opt-in shells built from the existing factory.
- `.specify/`: portable Spec Kit scripts/templates and project constitution.
- `specs/002-quetzal-wallet/`: spec, research, plan, tasks, quickstart and evidence.

## Execution and validation

1. Build configuration contracts for both networks and reference defaults.
2. Build Quetzal through Nix and run its real parser with the supervisor's flags.
3. Run the real Daedalus/node/backend only when prerequisites permit; preserve
   TLS and report the exact blocker instead of substituting another approach.
4. Follow the quickstart to verify renderer client requests, UI and persistence.

## Complexity Tracking

No constitution exceptions. No new application data model or HTTP contract;
existing wallet API and watchdog configuration contracts remain authoritative.

## Candidate update — 2026-11-10

Candidate `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7` implements stdin EOF,
standard CLI options, metadata fetching and network sync reporting. Restore the
usual sync-tolerance/token-metadata arguments and remove the experimental
Shelley genesis alias; upstream now obtains era/genesis parameters from the node.
Rebuild configuration contracts and exercise the real native-node/watchdog path.
Global chain replay remains a performance qualification item, not a proven
correctness failure. No supervisor or renderer compatibility shim is planned.

## Packaged launch plan — 2026-11-10

Refactor the existing `nixosPackage` factory in `perSystem/linux.nix` to accept
an explicit Quetzal variant. Use the same production frontend, launcher and
watchdog; choose Quetzal in the generated package config and expose its binary
under libexec. Generate separate package/main-program/desktop names and set the
launcher state root to `Daedalus-Quetzal`. Allow experimental config generation
for Linux preview/preprod Nix packages as well as development shells; keep all
other guards and disabled dApp policy. Give the frontend an isolated user-data
path and accurate compiled wallet-version metadata. Add package-config contract
coverage and build the requested preview package. No database import or fallback.

## Shared-state correction — 2026-11-10

Sam explicitly requests the packaged Quetzal variant share the normal selected
network state (node chain, frontend settings, logs and TLS). Revert its launcher
root to `Daedalus`, preserve the normal Electron user-data behavior, and put
Quetzal's database at `wallets/quetzal.sled/`. Sled uses a directory, including
its own nested Byron store. Existing SQLite files remain siblings and must not
be opened, migrated or modified by Quetzal. Keep development shells isolated.
Update the constitution/spec/contracts, test a real sled startup alongside a
SQLite fixture, and rebuild the packaged preview launcher. Sharing application
state does not imply importing SQLite wallet records.
