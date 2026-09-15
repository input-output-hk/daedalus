## Task ID and Title

`task-034` — Confirm the pointer against the user's own chain before writing a
row.

## Why Chosen Now

`task-033` returns what an index said. Nothing yet decides whether any of it is
true, and `task-035` cannot write a row until something does. This is the task
that makes Koios an index rather than an oracle, and the graph names it critical
for that reason.

## Interaction Mode

`agent_execution`.

## Scope

Four checks over one pointer: three against the bytes the index returned, and
one against the block those bytes claim to be in, read out of the user's own
immutable database. Plus the CIP-25 payload, extracted from the confirmed bytes.

## Non-Goals

- **`volatile/` is not read.** The last k blocks live there in a different
  on-disk format. A pointer into that window resolves later rather than not at
  all, and reading it would be its own task against a different layout.
- **No CIP-68 confirmation.** A CIP-68 datum lives at a spendable UTxO, so the
  mint transaction says nothing about its current value. Confirming it means a
  local-state-query against the node, which this plan does not build.
- No row is written here. This module answers; `task-035` writes.
- No chain-sync client, no Ouroboros, no node socket. This is file reading and
  CBOR.

## Dependencies

`task-033`.

## The CBOR decoder, settled

The task graph asks for this to be named here rather than smuggled into an
acceptance criterion, and the PRD carries it as open question 2.

**Settled: no new runtime dependency. A structural reader is written in the
repository, at `source/main/assets/cborSpan.ts`, and it returns byte ranges
rather than values.**

The requirement is real and cannot be dropped. A transaction id is blake2b-256
over the body **as it was originally encoded**, and the auxiliary-data hash in
body key 7 is blake2b-256 over the auxiliary data as it was originally encoded.
Re-encoding either and hashing the result is wrong for any transaction whose
encoder did not produce canonical bytes, and nothing in a decoded value says
whether that is the case. Neither `cbor@5.0.2` nor `borc@2.1.2` exposes where a
decoded item started and ended.

Three things make writing one the right answer rather than the cheap one.

The module is not a decoder. It walks the structure, measures it and returns
offsets; the caller reads values from those offsets one type at a time. That is
a much smaller thing to get right than a decoder, and it is the whole of what is
needed.

Its failure mode is one-directional. Every use of it sits inside a check whose
failure means no row is written, so a boundary it gets wrong produces a hash
that does not match. A defect here cannot admit a bad record; it can only fail
to admit a good one.

And the byte ranges are needed in three places, not one: the transaction body,
the auxiliary data, and each transaction body inside the block. A library that
returned only the top-level item's range would still leave two of the three
unserved.

The alternative, adding a decoder that exposes ranges, is a runtime dependency
on a package that would parse attacker-influenced bytes in the main process, in
a branch that has added none. That is the trade this settles, and it is recorded
here and in the PRD's status log rather than inside a criterion.

## Research Consulted

- `asset-metadata-cache-prd.md:955-975`, what the local check establishes and
  what it does not.
- `asset-metadata-cache-prd.md:1560-1580`, open questions 1 and 2, the volatile
  window and the decoder.
- `research/01-koios-pointer-option.md:301-340`, the three byte checks and why a
  point is enough to read a block from local files but not to retrieve one over
  chain-sync.

## Docs, Workflows, and Skills Consulted

- None of the `.agent/` workflow documents covers the immutable database.
  `chainStorageValidate.ts:69` is the only place in the repository that names the
  directory, and it only checks whether it exists.

## Live Repo Findings Verified For Planning

Everything below was read off the preprod immutable database on this machine,
`~/.local/share/Daedalus/preprod/chain/immutable`, 17 GiB across 6,092 chunks,
on 2026-09-16.

1. **The chain path is already resolved in the main process.**
   `source/main/index.ts:360-369`: `<stateDir>/chain`, or `<custom>/chain` when
   the user has moved it, with the custom path in electron-store under
   `CUSTOM-CHAIN-PATH`.
2. **A chunk is three files and the primary index is self-describing.**
   `00000.primary` is 86,409 bytes: one version byte, then 21,602 big-endian
   `uint32` offsets. `(86409 - 1) / 4 - 2 = 21600`, which is the chunk size, so
   it does not have to be carried as a per-network constant.
3. **The version byte is 1** on every chunk checked.
4. **A secondary entry is 56 bytes** and the fields are, in order, an 8-byte
   block offset, a 2-byte header offset, a 2-byte header size, a 4-byte
   checksum, a 32-byte header hash and an 8-byte slot. Confirmed by reading
   chunk 6090's entries and finding slots that ascend within the chunk's range.
5. **Relative slot zero is the epoch boundary block's.** For block 47 of chunk
   6090, at slot 131,545,218, the chunk base is 131,544,000, and the primary
   index entry that points at that block is at index 1,219, which is
   `(slot mod 21600) + 1`. Its value is 2,632, which is `47 * 56`.
6. **A chunk file is blocks concatenated with no framing.** A block runs from
   its own offset to the next entry's, or to the end of the file.
7. **A block is `[eraTag, block]`** and the era tag at the preprod tip is 7. The
   block is a five-element array whose second element is the transaction bodies.
8. **The pointer agrees with an independent index, exactly.** For transaction
   `907243e6…6dea1c`, `POST https://preprod.koios.rest/api/v1/tx_cbor` answered
   `block_hash` `2f7684ce…a6dfb` and `absolute_slot` 131,545,218, both matching
   the local secondary index entry, and its `cbor` field is **byte-identical**
   to the transaction reassembled from the local block. That is the strongest
   check available here: two independent sources agreeing byte for byte.
9. **The three byte checks reproduce.** blake2b-256 over the body span equals
   the claimed transaction id; blake2b-256 over the auxiliary-data span equals
   body key 7, `ad5f1ffc…d5f992`; and body key 9 names policy
   `67ab0c92…3bd0b` with one asset at +1 and another at -1.
10. **The transaction that mints also burns.** The same key 9 carries a -1 for a
    sibling asset name. So the quantity has to be read, not just the key: a burn
    is not a mint and must not confirm.
11. **`blake2b` is already a dependency**, at 2.1.3, and is used by
    `assetVerification.ts:2` and `utils/assetFingerprint.ts:1`. It guards its
    input with a realm-sensitive `instanceof Uint8Array`, which is why both call
    sites wrap with `Uint8Array.from`.
12. **Bigint literals do not compile here.** `tsconfig.json` targets `es2019`, so
    `0n` is `error TS2737` and `BigInt(0)` is required.
13. **`no-continue` is enforced**, which shapes any loop with several early
    exits.

## Files Expected To Change

- `source/main/assets/cborSpan.ts` (new)
- `source/main/assets/cborSpan.spec.ts` (new)
- `source/main/assets/immutableBlockReader.ts` (new)
- `source/main/assets/immutableBlockReader.realfs.spec.ts` (new)
- `source/main/assets/chainPointerVerification.ts` (new)
- `source/main/assets/chainPointerVerification.realfs.spec.ts` (new)
- `source/main/assets/chainPointer.fixture.ts` (new)
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**Three modules, because they fail differently.** The reader knows about bytes
and offsets and nothing about Cardano. The immutable reader knows about the
on-disk layout and nothing about transactions. The confirmation knows about
transactions and calls the other two.

**The immutable reader answers four ways, and the difference is the point.**
`found`, `beyond-immutable-tip`, `absent` and `unreadable`. Only `absent` says
the pointer is wrong: the slot is inside what the database covers and there is
no such block. `beyond-immutable-tip` is the volatile window and resolves later.
`unreadable` decides nothing about the pointer at all.

**The volatile window is measured against the database's own tip, not against
k.** The tip is the last entry of the highest chunk. That needs no per-network
security parameter, no slot-length arithmetic and no wall-clock estimate, and it
is exactly the question being asked: does this database hold that slot yet.

**The chunk size is derived from chunk zero** rather than hardcoded, for the
same reason.

**Four checks, in this order.** The three byte checks first, because they are
cheap and a failure means the bytes are wrong whatever the chain says. Then the
block read, which is the only one that costs I/O and the only one that
establishes the transaction was ever accepted.

**The mint check reads the quantity.** Finding 10 is why: the recorded
transaction mints one asset and burns another under the same policy in the same
field, so a check that only matched the key would confirm the burned one.

**The auxiliary data is checked both ways.** A transaction that claims a hash
must carry data matching it, and one that carries data must claim it. Either
half alone would let a pointer carry metadata that is not bound to the
transaction.

**CIP-25 is read from the confirmed bytes, in all three historical shapes.**
Shelley's bare metadata map, Allegra and Mary's two-element array, and Alonzo's
tag 259 map. The asset name is looked for as hex and as text, because CIP-25
version 1 and version 2 spell it differently.

## Acceptance Criteria

1. A recorded pointer and its block confirm, and the CIP-25 name comes back.
2. A pointer to a block inside the volatile window resolves to `pending` with
   the immutable tip, and no row is written, asserted with a fixture at the
   boundary: confirmed at the tip, pending one slot past it.
3. A tampered transaction body fails the transaction-id check.
4. Tampered auxiliary data fails the key 7 check, with the body still hashing to
   its claimed id, so the case cannot pass through the first check.
5. A mint field naming a different policy fails, and so does a zero quantity and
   a burn.
6. A transaction whose bytes are valid but which is not in the block it names
   fails.
7. All three historical auxiliary-data shapes decode.
8. An unrecognized immutable chunk shape fails closed rather than guessing, and
   the format version it was read against is recorded in the module.
9. No confirmation is returned on any failing path.
10. No new runtime dependency; `package.json` and `yarn.lock` unchanged.
11. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
    `nix build`.
12. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

- The positive case runs against a **real block**, recorded from the preprod
  database, in a miniature immutable database written to a temporary directory
  in the layout the reader documents. That is what makes the layout claim
  checkable rather than self-consistent: the same bytes were confirmed against
  Koios, which knows nothing about this code.
- The transaction the index would have supplied is reassembled from the block
  rather than recorded separately, because the two were measured to be
  byte-identical. Recording both would have doubled the fixture to store one
  fact twice.
- Each tampering case changes one byte in a place chosen so that only the check
  under test fails. The auxiliary-data case changes the last byte of the
  transaction, which is inside the auxiliary data, so the body still hashes
  correctly and only key 7 disagrees.
- The auxiliary-data shapes are driven with synthetic transactions built with
  `cbor.encode`, because the recorded block is one era. Building the inputs with
  a library the module under test does not use is deliberate: an agreement
  between the two is evidence about the reader.
- `cborSpan.spec.ts` drives the reader directly against inputs `cbor.encode`
  produces and against hand-written bytes it will not: indefinite-length arrays,
  maps, byte strings and text strings, every argument width, the float widths,
  truncation, a reserved additional-information value, a length that could not
  fit the input, and nesting past the depth cap.
- `immutableBlockReader.realfs.spec.ts` drives the four outcomes and every
  fail-closed path: an unknown version byte, an implausible chunk size, a
  secondary index that is not whole entries, a missing chunk file, a slot that
  is not a slot, and no database at all.
- Beyond the committed suite, the whole path was run once against the real
  17 GiB preprod database on this machine with a live `tx_cbor` response. That
  run is evidence for this record and is not a committed test, because CI has no
  chain.
- All six Nix checks.

## Risks and Open Questions

- **The format is undocumented and could change.** A node release that bumps the
  primary index version makes this reader refuse rather than misread, and the
  chain channel goes quiet until it is updated. That is the intended failure
  direction and it is not detectable from here in advance.
- **A freshly minted NFT resolves late.** See the closing note; it is the common
  case rather than the exception.
- **The block read is synchronous file I/O in the main process.** A chunk file
  is on the order of a megabyte and is read once per confirmation. It runs
  behind the resolver's existing queue, and nothing waits on it, but it is
  synchronous and it is on the main process's thread.
- Nothing here needs a decision from the project owner beyond the decoder
  settlement above, which is recorded rather than asked because the requirement
  it satisfies is unchanged and no dependency is added.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-034.targetPaths` widened to the
  structural reader, the fixture and the three specs; the decoder settlement
  recorded in its implementation notes; `task-034.status` to `completed`.
- The PRD's open question 2 is closed by the settlement above, and open question
  1's resolution is implemented as described. Both are recorded in the PRD's
  status log at the end of the phase.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-034-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-034-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

An index that lies fails a check, an index that is unreachable leaves rows
absent, and an index that is right about a block the user already holds is
believed.

## Closing note: what a user sees for an asset minted in the last twelve hours

The immutable database excludes the last k blocks, which on mainnet is 2,160
blocks at roughly twenty seconds, so about twelve hours. A pointer into that
window cannot be confirmed, and nothing is written.

**Until it can be, the asset shows its decoded name, marked as minter-chosen,
and its CIP-14 fingerprint.** It gains its CIP-25 name on a later demand, once
the block has passed out of the volatile window and into the immutable database.
Its amount is in raw units throughout, before and after, because a chain row
never carries decimal places.

This is the common case rather than the exception. A freshly minted NFT is
exactly the asset a user has just acquired and most wants to see named, so for
NFTs the late path is the usual path. Reading `volatile/` would close the window
and is deliberately not in scope.

## Final Outcome

Complete.

## Self-Review

Two things here would have been easy and wrong. The first is measuring the
volatile window with k and a slot length: it needs a per-network constant, it is
an estimate, and it answers a different question from the one being asked, which
is whether this database holds that slot. Reading the tip answers it exactly.
The second is trusting the index's own copy of the CIP-25 payload once the block
has confirmed the transaction. The block confirms the bytes, so the payload is
read from the bytes; taking it from the response would have made the whole
confirmation decorative for the one field anybody sees.
