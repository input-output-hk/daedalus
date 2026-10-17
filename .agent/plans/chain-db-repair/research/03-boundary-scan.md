# Boundary Scan: Locating a Hole in Minutes

A #3416 hole is a break in hash linkage between two chunks: chunk N ends early, and the first block of chunk N+1 names a previous block that is not on disk. cardano-node finds this during full chunk validation, but that reads and checksums every block. Measured on a mainnet install on 2026-10-05, full validation took 1 h 26 min to reach the damaged chunk 8948. A full-range `mithril-client cardano-db verify` of the same install took 3 h 23 min on 2026-09-30. Both read the whole database, about 217 GB.

Checking the linkage alone needs only two small reads per chunk.

## Method

Each chunk has a `.secondary` index of fixed 56-byte entries, one per block, big-endian:

| Field | Bytes |
|---|---|
| block offset in the `.chunk` file | 8 |
| header offset within the block | 2 |
| header size | 2 |
| CRC32 of the block | 4 |
| header hash | 32 |
| slot number (or epoch for a Byron EBB) | 8 |

For each non-empty chunk, take the first entry, read that many header bytes from the `.chunk` file at block offset plus header offset, and decode the start of the header: `[header_body, kes_signature]`, where `header_body` begins `[block_number, slot, prev_hash, ...]`. Compare `prev_hash` with the header hash in the last entry of the previous non-empty chunk. A difference is a hole between them.

The header hash in each entry is blake2b-256 of the header bytes, which gives a self-check that the offsets and decoding are right.

The prototype is `03-boundary-scan-prototype.py` in this folder. It is read-only.

## Results, 2026-10-16

All runs read Windows installs from WSL over the 9P mount of the Windows drive, which is the slowest access path on that machine (the full-range verify on the same path ran at about 18 MB/s).

| Run | Boundaries linked | Broken | Empty chunks | Byron, skipped | Header self-check | Time |
|---|---|---|---|---|---|---|
| Mainnet, chunks 0 to 9273 | 9,066 | 0 | 0 | 207 | 9,066 of 9,066 | 184.8 s |
| Preprod, chunks 0 to 6269 | 6,102 | 0 | 164 | 3 | 6,102 of 6,102 | 97.5 s |
| Synthetic hole: mainnet chunk 9100 with its last 500 index entries removed, then 9101 | 0 | 1 | 0 | 0 | 1 of 1 | under 0.1 s |

The mainnet install had seven holes until 2026-10-05, when the node's full validation truncated its ImmutableDB at the first one, and it resynced from there. So the zero in the first row is the expected answer, and that run measures time and false positives, not detection. The synthetic case models a #3416 hole exactly: the incomplete chunk's own files are consistent and simply end early, and the next chunk starts where the complete chunk would have ended. Preprod has 164 chunks with no blocks, which the scan carries across.

## What it does not cover

- Corruption inside a chunk, such as a flipped bit that fails a block's CRC (the #3409 class). That needs `cardano-db verify` or the node's own validation.
- A hole inside the last chunk, which the node is still appending to, or between the ImmutableDB and the VolatileDB.
- Byron-era boundaries. Holes come from partial syncs near the tip, so this is not a practical limit.
- A missing `.secondary` or `.chunk` file. The prototype raises on it; the watchdog implementation must report it as damage.

## Not yet established

- A positive result on a real hole made by 11.4.0 code rather than a synthetic one (the 11.4.0 reproduction on preprod is described in the PRD's testing strategy).
- Timing on native NTFS from a Windows build of the watchdog. The 9P figures above are an upper bound for that machine, not a measurement.
- Whether files the node holds open can be read while it runs on Windows. The plan runs the scan with the node stopped, so this does not block it.
