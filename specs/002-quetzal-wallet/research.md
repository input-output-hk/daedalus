# Research — Quetzal integration

## Source identities

- Daedalus base: `8d961fa48` (master at worktree creation).
- Quetzal initial candidate: `a5af746d46ebd643599aec436986afe26db34cb9`.
- Source discovery: agentic-nix Git ingestion and scoped searches; exact source
  files inspected from the path returned by `nix flake metadata`.
- Upstream: https://github.com/se7en-labs-inc/quetzal

## Findings and decisions

1. Daedalus's parent watchdog owns node/wallet lifecycle, certificate generation,
   dynamic API port selection and authenticated health probes. The renderer
   speaks TLS 1.3 with client certificates. Keep this architecture unchanged.
2. Quetzal's initial candidate accepts `serve`, `--database`, `--node-socket`,
   `--testnet <Byron genesis>` and `--port`. Its parser rejects other flags.
   Omit cardano-wallet-only `--sync-tolerance` and `--token-metadata-server` from
   the experimental configuration. Its fixed sync tolerance is already 300s.
3. Quetzal reads `shelley-genesis.json` beside the supplied Byron file; Daedalus
   emits `genesis-shelley.json`. Add an identical alias in experimental configs.
4. Watchdog also injects `--shutdown-handler`, then stops the wallet by closing
   stdin. The initial Quetzal main does not implement this lifecycle contract.
   Keep the watchdog contract intact and expose this upstream prerequisite;
   do not claim API conformance proves process compatibility.
5. Sam explicitly directed us to assume upstream TLS works and will fix it.
   Preserve all three existing TLS flags. Do not add a proxy, HTTP mode, or
   insecure fallback. Live qualification awaits a candidate supporting them.
6. Use the existing Daedalus crane/fenix inputs to build the Quetzal workspace
   from a non-flake source input. This avoids importing a second toolchain and
   nixpkgs graph. Limit new outputs to upstream's x86_64 Linux platform.
7. Provide `quetzal-preview` and `quetzal-preprod` shells. Use the absolute Nix
   binary path, distinct state roots, actual backend source identity, and
   disabled dApp connector policy. Normal shells/packages retain defaults.
8. Both frontend user data and backend state must be isolated. Set experimental
   state to `state-quetzal-<network>` in this worktree and Electron user data to a
   subdirectory there. Do not reuse installed databases or settings.

## Evidence boundaries

The README reports 643 conformance obligations and 3,807 passing checks. Those
are upstream claims for its frozen candidate, not a Daedalus integration run.
Build/configuration, actual CLI execution, real API calls, and manual UI checks
must each have their own recorded outcomes.

## Updated findings — 2026-11-10

At `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7`, upstream adds the full CLI parser,
stdin EOF and signal shutdown with worker cancellation/store flushing, token
registry metadata, native node era/genesis queries and network sync percentages.
The omission of standard launch arguments and genesis alias above describe only
the historical candidate; those accommodations have now been removed.
The global pool follower still intersects at genesis: realistic restart time
remains an unqualified performance concern. No correctness failure is inferred
from that fact alone. All integration evidence is tracked in validation.md.
