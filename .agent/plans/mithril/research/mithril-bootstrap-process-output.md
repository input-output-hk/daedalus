# Mithril Bootstrap Process Output

## Overview

When `mithril-client cardano-db download --include-ancillary <digest> --json` runs, it emits structured JSON lines on stdout. Each line is either a **step announcement** or a **progress update** belonging to one of two concurrent download streams.

## The 7-Step Workflow

| Step | Message | Description |
|------|---------|-------------|
| 1 | Checking local disk info… | Validates sufficient free space on the target volume |
| 2 | Fetching the certificate and verifying the certificate chain… | Downloads the Mithril certificate for the chosen snapshot and walks the certificate chain back to genesis to confirm authenticity |
| 3 | Downloading and unpacking the cardano db snapshot | Transfers and decompresses the snapshot data; this is where the two progress streams (`Files` and `Ancillary`) are emitted |
| 4 | Downloading and verifying digests… | Downloads the digest manifest (hash list of every immutable file) for local integrity checks |
| 5 | Verifying the cardano database | Computes hashes of every downloaded immutable file and compares them against the digest manifest |
| 6 | Computing the cardano db snapshot message | Computes the aggregate message that the Mithril multi-signature was produced over |
| 7 | Verifying the cardano db signature… | Verifies the Mithril multi-signature against the computed message and certificate, confirming the snapshot is authentic |

Step announcements use this shape:
```json
{"timestamp": "…", "step_num": 3, "total_steps": 7, "message": "Downloading and unpacking the cardano db snapshot"}
```

Steps 1–3 are the download phase. Steps 4–7 are the verification phase. No progress lines are emitted during steps 4–7; they run silently until the next step announcement (or process exit).

## Progress Streams During Step 3

Step 3 emits two interleaved progress streams, differentiated by the `"label"` field.

### `"Files"` stream — Immutable DB chunks

Tracks the download and decompression of the core blockchain data, delivered as thousands of small archive files (one per immutable chunk).

```json
{"label": "Files", "timestamp": "…", "files_downloaded": 1200, "files_total": 5457, "seconds_left": 72.0, "seconds_elapsed": 20.0}
```

| Field | Type | Meaning |
|-------|------|---------|
| `label` | `"Files"` | Identifies this as the immutable-files stream |
| `files_downloaded` | number | Immutable chunk files fetched so far |
| `files_total` | number | Total immutable chunk files in the snapshot |
| `seconds_elapsed` | number | Wall-clock seconds since this stream started |
| `seconds_left` | number | Estimated seconds remaining (CLI-computed) |

### `"Ancillary"` stream — Volatile/ledger state

Tracks the download and decompression of the ancillary archive. This stream starts while the Files stream is still running (they overlap) and reports byte-level progress because it is a single large archive rather than many small files.

```json
{"label": "Ancillary", "timestamp": "…", "bytes_downloaded": 154304512, "bytes_total": 683052510, "seconds_left": 29.9, "seconds_elapsed": 8.3}
```

| Field | Type | Meaning |
|-------|------|---------|
| `label` | `"Ancillary"` | Identifies this as the ancillary stream |
| `bytes_downloaded` | number | Bytes fetched so far |
| `bytes_total` | number | Total bytes in the compressed ancillary archive |
| `seconds_elapsed` | number | Wall-clock seconds since this stream started (resets to 0 independently of the Files stream) |
| `seconds_left` | number | Estimated seconds remaining (CLI-computed) |

### Stream timing overlap

The Ancillary stream starts before the Files stream finishes. In the observed preprod log, Ancillary began at ~`files_downloaded: 5440` of 5457 and both streams interleaved lines until the Files stream reached 5457/5457. After that, only Ancillary lines continued until the ancillary download completed.

Each stream maintains its own independent `seconds_elapsed` / `seconds_left` counters (Ancillary resets to 0 when it starts).

## What Are Ancillary Files?

Ancillary data is the **volatile database state** that cardano-node needs in addition to the immutable blockchain history. It includes the current ledger state (account balances, staking snapshots, protocol parameters) and any data from the most recent epoch that has not yet been finalized into an immutable chunk.

Without ancillary data, the node would have to replay transactions from the last immutable point forward to reconstruct this state — a process that can take significant time. Including ancillary data in the snapshot lets the node start almost immediately after download completes.

The `--include-ancillary` flag tells the Mithril client to fetch this data alongside the immutable chunks. Ancillary files are verified differently from the immutable chunks: they are signed with IOG-owned keys rather than verified through the Mithril multi-signature certificate chain, as noted by the CLI's caution message at the start of the log.

## Post-Download Output

After all 7 steps complete successfully, the CLI emits a final JSON object with:

```json
{
  "db_directory": "/path/to/db",
  "run_docker_cmd": "docker run …",
  "snapshot_converter_cmd_to_legacy": "mithril-client tools utxo-hd snapshot-converter … --utxo-hd-flavor Legacy --commit",
  "snapshot_converter_cmd_to_lmdb": "mithril-client tools utxo-hd snapshot-converter … --utxo-hd-flavor LMDB --commit",
  "timestamp": "…"
}
```

This tells the caller where the downloaded DB lives and provides ready-made commands to convert the UTxO HD storage format if needed.

## Daedalus handoff note

After Mithril exits successfully, any additional `cardano-node` / `cardano-wallet` restart noise seen in the Daedalus developer console is not emitted by `mithril-client`; it comes from Daedalus' own Cardano lifecycle management.

- `completed` now means Mithril restore work is done.
- `starting-node` is the explicit Daedalus-owned handoff phase while Cardano is being started against the restored chain.
- Unexpected Cardano exits during this handoff are treated as Mithril `node-start` failures, and the generic Cardano crash-restart loop is suppressed while Mithril is active so startup cannot bypass the bootstrap gate.

## Snapshot Metadata (from `snapshot show`)

Before download, `mithril-client cardano-db snapshot show <digest> --json` returns metadata including:

| Field | Example | Relevance |
|-------|---------|-----------|
| `total_db_size_uncompressed` | `17292954987` (~17 GB) | Total uncompressed size of the full snapshot |
| `immutables.average_size_uncompressed` | `2851847` (~2.8 MB) | Average size per immutable chunk file |
| `ancillary.size_uncompressed` | `1736128067` (~1.7 GB) | Uncompressed size of the ancillary data |
| `digests.size_uncompressed` | `1898689` (~1.9 MB) | Size of the digest manifest |
| `cardano_node_version` | `"10.6.2"` | Node version the snapshot is compatible with |
| `beacon.epoch` / `beacon.immutable_file_number` | `276` / `5455` | Chain tip at snapshot creation |
