# DRep-state sample fixture — provenance

**File:** [drep-state-preprod-epoch295-sample.json](./drep-state-preprod-epoch295-sample.json)
**Captured:** 2026-06-16 · **Network:** preprod (`--testnet-magic 1`) · **Epoch:** 295
**Binaries:** `cardano-cli 11.0.0.0` / `cardano-node 11.0.1` (git rev `97036a66bcf8c89f687ae57a048eecc0389977ef`), the running Daedalus 11.0.0#dev preprod bundle.

This is a **static, point-in-time** capture of the **public on-chain DRep directory** — not live data and not user-specific. It is a manual-testing / support reference and the first real `drep-state` fixture candidate for [task-166](../governance-drep-discovery-plan-tasks.json). It will go stale (expiry/status are relative to epoch 295); re-capture against a current epoch when you need fresh data.

## How it was produced

Exact command (= the argv `GovernanceQueryService` builds, network flag appended after the subcommand per the FP-1 fix, socket via env):

```bash
export CARDANO_NODE_SOCKET_PATH=~/.local/share/Daedalus/preprod/cardano-node.socket
cardano-cli latest query drep-state --all-dreps --include-stake --output-json --testnet-magic 1 \
  > drep-state-preprod-epoch295-sample.json
```

`cardano-cli` is not on the bare WSL PATH — it ships inside the app's Nix bundle. Run it inside `yarn nix:preprod`, or invoke the running app's bundled binary directly (see the "Reproducible capture procedure" in [slice-1-final-pass-review.md](../task-plans/slice-1-final-pass-review.md)).

## Shape & profile (validates the parser against real data)

Raw CLI output is a JSON array of `[credential, state]` tuples:

```json
[
  { "scriptHash": "08f112e23a54ce7dee3bc4d5eb51d523225908b6fdf84ff8c317b748" },
  { "anchor": null, "deposit": 500000000, "expiry": 233, "stake": null }
]
```

- **258 DReps total** — 68 active / 190 inactive at epoch 295.
- **Credentials:** `keyHash` ×212, `scriptHash` ×46 (both branches of `_credentialToDRepId`).
- **Stake:** 175 present, 83 `null` (→ "—"). One entry has `stake: 0` → renders `₳ 0`, distinct from `null`'s "—".
- **Anchors:** 116 present (key shape `{ url, dataHash }`), 142 `null`. The anchor `url` + `dataHash` are the on-chain *pointer*; the off-chain JSON-LD still needs fetch + hash-verify (slice-4).
- **Max stake** ≈ 400.5 T lovelace — within `Number.MAX_SAFE_INTEGER`, but json-bigint `storeAsString` keeps lovelace exact regardless.

## Related

- [slice-1-final-pass-review.md](../task-plans/slice-1-final-pass-review.md) → "Manual Verification — Results" (FP-11) and "Persisting the directory snapshot" (task-168).
- task-168: a `Logs/pub/DRep-state-snapshot.json` writer would make Daedalus emit a public DRep snapshot automatically on each refresh (bypassing `filterLogData`; never including user vote targets).

> **Shape note — this fixture (task-166) and the task-168 snapshot are different shapes.** This file is **raw `cardano-cli` output**: `[credential, state]` tuples with `keyHash`/`scriptHash` + `expiry`/`stake`/`anchor`. The **task-166** spec value of this file is exactly that it is raw — `GovernanceQueryService.spec` feeds raw output into `_parseDRepState`, so a committed fixture must stay raw. **task-168** instead emits `fetchDRepList()`'s **parsed `DRepListQueryPayload`** (derived CIP-129 `drepId` + `status` + `votingPower` + `epoch`). Related, but do **not** point the parser tests (task-166) at a task-168 snapshot — the parser would receive already-parsed data.
