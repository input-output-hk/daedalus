# Field Log Signatures of a Chain Database Hole

Source: four Daedalus support diagnostics bundles received 2026-10-15, labeled A to D here. All four are Windows mainnet installs running Daedalus 11.5.0 with cardano-node 11.1.3 and cardano-wallet 2026.9.16. Each had run Mithril partial syncs under 11.4.0, which left an incomplete chunk in the ImmutableDB followed by complete later chunks (public issue #3416). Identifiers, addresses, IP addresses, transaction ids, block hashes and user paths are replaced with `<redacted>` below. Slot numbers, block numbers and chunk numbers are public chain data and are kept.

## Bundle layout

Every bundle has the same files: `node.log` and three rotations (cardano-node stdout and stderr, plain text with ANSI color codes, about 5 MB each), `cardano-wallet.log` and three rotations (plain text), `watchdog.log` (`<ISO timestamp>  INFO|WARN <message>`), `Daedalus.json` with timestamped rotations (main process, JSON), and `Daedalus-versions.json`, `State-snapshot.json`, `System-info.json`.

The node log rotation keeps minutes to hours of output, because the node logs every rejected peer: bundle A retains 2 minutes, D 10 minutes, B 2 hours. Only C's node log goes back weeks. So the first appearance of a signature is not visible for A, B and D, and a parser can never assume it has seen the start of an episode.

## Signature 1: the node rejects canonical blocks with `BadInputsUTxO`

Seen in A, B, D, and in C before its later symptoms. The ledger replayed from a snapshot taken before the hole, so the outputs created inside the hole never entered the UTxO set, and the node rejects every canonical block that spends one of them. The inputs named in these failures were created by transactions in blocks inside each install's hole (checked against a chain indexer for A, B and D).

The failure is logged once per peer connection. Each peer serves the canonical block, the node rejects it, and the connection is dropped, so the line repeats for as long as the node runs. Representative line, namespace `ChainSync.Client.Exception`:

```
[2026-10-14 23:16:58.7759Z][daedalus:ChainSync.Client.Exception](Warning,30927) Peer is: (ConnectionId {localAddress = <redacted>, remoteAddress = <redacted>}). An exception was thrown by the Chain Sync Client. InvalidBlock (Point (At (Block {blockPointSlot = SlotNo 200361988, blockPointHash = <redacted>}))) <redacted> (ExtValidationErrorLedger (HardForkLedgerErrorFromEra S (S (S (S (S (S (Z (WrapLedgerErr {unwrapLedgerErr = BlockTransitionErrorInternal (LedgersFailure (LedgerFailure (ConwayUtxowFailure (UtxoFailure (ValueNotConservedUTxO Mismatch (RelEQ) {supplied: MaryValue (Coin ...) ...})))) :| [LedgersFailure (LedgerFailure (ConwayUtxowFailure (UtxoFailure (BadInputsUTxO (NonEmptySet (fromList [TxIn (TxId {unTxId = SafeHash "<redacted>"}) (TxIx {unTxIx = 1})]))))))])})))))))))
```

`BadInputsUTxO` is the second element of a `:|` list whose first element is `ValueNotConservedUTxO` for the same block. The same text also appears under `Net.Mux.Remote.ExceptionExit`, `Net.InboundGovernor.Remote.MuxErrored` and `Net.ConnectionManager.Remote.ConnectionHandler.Error` for the same connection. At startup the node may log it once more under `ChainDB.InitChainSelEvent.Validation.InvalidBlock` (Error severity). No `ChainDB.AddBlockEvent` trace carries it in these logs.

| Bundle | Lines containing `BadInputsUTxO` | `ChainSync.Client.Exception` lines | Rejected block slot | Distinct peers that served it |
|---|---|---|---|---|
| A | 467 | 116 | 200361799 | 12 |
| B | 7,646 | 1,911 | 200361988 | 26 |
| C | 4,569 | 1,142 | 200080900, 200080902 | 2, 12 |
| D | 5,256 | 1,313 | 200189886 | 26 |

The node process keeps running and its tip does not advance. The watchdog logs no exit or restart.

Lines run up to 34,731 bytes (bundle A), with `BadInputsUTxO` up to 5,405 bytes into the line. The watchdog's per-line cap is 1 MiB (`watchdog/src/supervisor.rs:618`), so whole lines reach the parser.

**Matching rule.** A line under `ChainSync.Client.Exception` that contains both `InvalidBlock (Point (At (Block {blockPointSlot = SlotNo <n>` and `BadInputsUTxO`. Count distinct `remoteAddress` values per rejected slot, and trigger when one slot has been rejected by 3 distinct peers. A single misbehaving peer cannot reach that threshold, and every field install exceeded it. Strip ANSI escape sequences before matching.

## Signature 2: the node cannot open the ledger because its point is missing from the ImmutableDB

Seen in C only, 18 times, between 2026-10-12 07:24 and 2026-10-15 01:53 UTC. The ledger snapshot's point lies inside the hole. The node writes this to stderr with no timestamp or namespace, and the message runs straight into the following `Node configuration:` text with no newline:

```
cardano-node.exe: Critical invariant violation: block Point (At (Block {blockPointSlot = SlotNo 200102375, blockPointHash = <redacted>})) that was in immutable db is gone before we could open ledgerdbNode configuration: NodeConfiguration {...}
```

`watchdog.log` stops at `node startup phase: openedVolatileDb` on each of these starts. `openedLedgerDb` and `chainDbReady` never follow, and the watchdog logs no `node_exited`, so the node either exits without the watchdog noticing or hangs after the exception. C has 40 `cardano-node started` lines against 13 `chainDbReady` lines, and 5 `user-initiated node restart` lines. Each failed attempt ended only when the user quit or restarted. There is no crash-restart loop and no `node_unrecoverable`.

**Matching rule.** Substrings `Critical invariant violation` and `that was in immutable db is gone before we could open ledgerdb` on node output. Read the slot from `blockPointSlot = SlotNo <n>`. One occurrence is enough.

## Signature 3: cardano-wallet finds a discontinuity in the chain the node serves

Seen in C only, after its final Mithril sync on 2026-10-15. The ledger snapshot is after the hole, so the node opens and syncs, but a wallet whose checkpoint is before the hole receives a chain that skips the missing blocks. It is logged in `cardano-wallet.log`, twice per event (one line per wallet worker), 11 times:

```
[cardano-wallet.wallet-engine:Error:53] [2026-10-15 02:03:38.48 UTC] Worker has exited: Unhandled exception: user error (restoreBlocks: given chain isn't a valid continuation. Wallet is at: <redacted><-[<redacted>-200080892#14050855] but the given chain continues starting from: <redacted><-[<redacted>-200102408#14051883])
```

The line names both edges of the hole: the wallet is at slot 200080892, block 14050855, and the next block it is given is at slot 200102408, block 14051883, so 1,027 blocks are missing. The node keeps running; the affected wallet's worker exits.

**Matching rule.** Substring `restoreBlocks: given chain isn't a valid continuation` on wallet output. Read the slot and block number of both edges from `-<slot>#<blockNo>]`. One occurrence is enough.

## Sequence in bundle C

C shows all three signatures in turn, each after another Mithril sync moved the ledger snapshot relative to its hole: `BadInputsUTxO` from 2026-10-11 06:24 to 2026-10-12 07:01 UTC, the critical invariant violation from 2026-10-12 07:24 to 2026-10-15 01:53, then the wallet discontinuity from 2026-10-15 02:03. The user spent about 3.8 days retrying.

## Mithril sync history in `watchdog.log`

The partial sync logs the local and certified chunk numbers (`watchdog/src/mithril.rs:816`, `:828`, `:832`):

```
2026-10-13T21:09:09.863293Z  INFO Mithril forced by caller: local=9062, certified=9272; skipping not-needed check
```

| Bundle | `local=` values | `certified=` values |
|---|---|---|
| A | 9062, 9273, 9274 | 9272, 9272, 9275 |
| B | 0, 9274 | 9272, 9275 |
| C | 9134, 9199, 9199, 9232, 9260, 9263, 9267 | 9170, 9198, 9230, 9259, 9262, 9266, 9277 |
| D | 9108, 9120, 9128, 8944, 9176, 9208, 9212, 8944, 9256, 9264 | 9110, 9122, 9131, 9147, 9179, 9211, 9235, 9243, 9260, 9267 |

On 11.4.0 and earlier, every such line with `local` above 0 and below `certified` marks a chunk left incomplete. C's hole at chunk 9263 (slots 200080800 to 200102399) matches both its critical-invariant slot and the two edges in its wallet line. These lines are a hint at most: log rotation and user clean-up remove them, so their absence proves nothing.

## Lines that do not appear

No bundle contains `ChecksumMismatch`, `InvalidChecksum`, any `InitFailure*`, `ChunkValidation.Invalid`, `MissingChunk`, `InvalidFileFormat`, or an ImmutableDB truncation. C logs 308 `StartedValidatingChunk` lines over 40 starts, which is the routine check of the most recent chunks. None of the logs shows a full validation of every chunk, which would have found the hole and truncated the chain there.
