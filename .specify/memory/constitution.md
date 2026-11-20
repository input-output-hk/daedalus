# Daedalus Constitution

## Core principles

1. Preserve the full-node architecture, existing Electron/React/MobX conventions,
   type-safe IPC, and hardware-device signing boundary.
2. Use Nix for builds, development tools, and reproducible dependency pins.
   Keep experimental backend selection explicit; preserve release defaults.
3. Preserve mutual TLS between the frontend, watchdog probes, and wallet backend.
   Do not weaken transport security to accommodate a replacement implementation.
4. Per Sam's explicit shared-state request, packaged Quetzal variants reuse the
   selected network's application/node state. Keep Quetzal's sled directory at
   `wallets/quetzal.sled/`, separate from untouched SQLite files. Development
   shells retain isolated state. Never infer database-format compatibility from
   HTTP API compatibility.
5. Specify, plan, and enumerate tasks before implementing a new feature. Record
   actual integration evidence and distinguish mocks, assumptions, and live tests.

## Validation and workflow

Use focused Nix contract checks and real integration tests for changed boundaries.
Run applicable formatting checks. Before committing application changes, run
TypeScript compile and Jest as required by repository working agreements.
If an approach or test fails, explain the failure and ask before implementing a
different approach. No automatic commits, pushes, or external publication.

## Governance

The user's current instructions and AGENTS.md take precedence. Amend this
constitution explicitly when requirements change; do not silently relax gates.

**Version**: 1.1.0 | **Ratified**: 2026-11-09 | **Last Amended**: 2026-11-10
