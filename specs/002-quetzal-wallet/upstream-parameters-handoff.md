# Quetzal handoff: fresh preview protocol-parameters query fails

## Reproduction identities

- Quetzal `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7` built through Daedalus Nix.
- cardano-node 11.1.3, revision `938cba990357ae7c4b7f95c8f75dd9d31174bbeb`.
- Real public preview, network magic 2, fresh database, no Mithril snapshot.
- Daedalus base `8d961fa48`, worktree `002-quetzal-wallet`.
- Working authenticated TLS 1.3, watchdog-generated CA/server/client certificates.

## Observed result

After node startup, Quetzal connects to the node. Daedalus's renderer
`getNetworkInfo` request reports `syncing`, numeric progress and `node_era: alonzo`.
The renderer's `getNetworkParameters` request then fails with:

```text
state query failed: failure decoding CBOR data
```

Independent wallet create/list/address calls and both wallet-only and complete
process restarts succeed. EOF shutdown flushes stores and exits cleanly. The
parameters error is retained as a failed check, not waived by those successes.

## Launch and client path

The temporary headless config derives from `.#quetzal-preview-config`, with
Electron/Mithril removed, a fresh state root and an unused node TCP port.
Watchdog starts both actual binaries and injects the dynamic API port, all three
TLS flags, and `--shutdown-handler`. Wallet arguments include `serve`, the real
socket/database paths, `--sync-tolerance 300s`, `--testnet` pointing at the
existing preview Byron genesis, and the selected token metadata server.

The failed call is the unmodified renderer request function in
`source/renderer/app/api/network/requests/getNetworkParameters.ts`:
`GET /v2/network/parameters`, with the watchdog's actual client certificate.
The private exact config and logs remain in the local path recorded in
validation.md; do not publish runtime wallet/PKI data.

## Investigation target and acceptance

The HTTP handler calls `StateQuerier::protocol_params` before era timings and
genesis projections. That function calls `queries_v16::get_current_pparams` for
the node's current era. Start by checking the actual Alonzo response against
that decoder. This is an investigation lead, not a proven root cause.

Add a real native-node acceptance case for a fresh unsynced preview node in
Alonzo, alongside existing Babbage/Conway controls. Network information and
network parameters must both be consumable by the actual client during initial
sync. Decode era-appropriate values; do not substitute fabricated defaults or
label a connected-but-behind node ready. Recheck era transitions as well.
