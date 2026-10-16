# Chain Database Repair: Planning Brief

Plan a damaged-chain-database flow for Daedalus: detect from cardano-node and cardano-wallet log output that the local chain database has a hole in its ImmutableDB, show the user a screen that says so, and offer a rebuild through Mithril that keeps their wallets. Plan a targeted repair (replace only the damaged chunks, then install a fresh certified ledger after asking the user) as a second stage behind the same trigger, gated on a measurement spike.

## Problem framing

Daedalus 11.4.0 Mithril partial syncs started their download one chunk above the highest local chunk. When the local chain was behind the certified snapshot, that left the incomplete highest local chunk in place and installed the following chunks after it, so the blocks missing from its tail never reached the ImmutableDB (public issue #3416). The sync then installed the `clean` marker, so cardano-node skipped chunk validation on the next start and nothing noticed the hole until something read through it. 11.5.0 fixes the download range and creates no new holes. It repairs none.

Four Windows support diagnostics received on 2026-10-15, all from 11.5.0 users, show this damage done by earlier 11.4.0 partial syncs. What the user sees depends on where the ledger snapshot sits relative to the hole:

- Ledger snapshot before the hole: the ledger replays across it, so the outputs created inside the hole never enter the UTxO set, and the node rejects canonical blocks that spend them with `BadInputsUTxO`.
- Ledger snapshot inside the hole: the node refuses to open the database with `Critical invariant violation ... that was in immutable db is gone before we could open ledgerdb`.
- Ledger snapshot after the hole: the node syncs, but cardano-wallet fails with `restoreBlocks: given chain isn't a valid continuation` when it reads across the hole.

The wallet databases are intact in all four. In 11.5.0 the renderer never sends `start_mithril` with `wipe_chain: true`, so these users have no rebuild inside the app; the only remedy today is deleting the chain folder by hand.

## Constraints carried from earlier decisions

- Owner decision, 2026-09-29: chunk swaps that repair this class of damage need no prompt. The replacement content is fixed public chain data verified against the Mithril certificate, so the user has nothing to decide. The user is told when a swap cannot fix it and a resync is needed.
- Owner ruling, 2026-09-30: log lines may start a check or a repair and may hint at which chunks to check first, but their absence proves nothing, because logs rotate and users clear them. Detection that has to be complete uses `mithril-client cardano-db verify`.
- The background scan for installs that carry holes without symptoms is a separate effort and stays out of this plan, apart from sharing the chunk swap with it.
- Experiments that damage or repair a chain database run on preprod or preview, never on a mainnet install that is in use.
