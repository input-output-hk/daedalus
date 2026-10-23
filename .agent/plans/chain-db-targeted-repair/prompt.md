# Chain Database Targeted Repair: Planning Brief

Plan the targeted repair of a damaged chain database for Daedalus. It was phases 6 and 7 of the [chain-db-repair](../chain-db-repair/prompt.md) plan and moved to its own plan on 2026-10-22, with its tasks, requirements and decisions. When the watchdog reports `chain_damage`, find the hole with a boundary scan, replace only the damaged chunks with certified copies, and, after asking the user, install a fresh certified ledger. Run it before the full rebuild offer and fall back to that offer. Gate it on a measurement spike.

## Problem framing

The chain-db-repair plan ships on its own, in a patch release: detection of the three field signatures, a full Mithril rebuild that is safe to offer, and a full-screen offer with a matching action in Diagnostics. The rebuild downloads the whole database, about 217 GB on mainnet, to repair what is, in every field case, one incomplete chunk. A boundary scan of the chunk indexes covered a whole mainnet install in about 3 minutes and found a synthetic hole, against hours for a full read ([research/03](../chain-db-repair/research/03-boundary-scan.md)).

## Constraints carried from earlier decisions

- Owner decision, 2026-09-29: chunk swaps that repair this class of damage need no prompt. The replacement content is fixed public chain data verified against the Mithril certificate, so the user has nothing to decide. The user is told when a swap cannot fix it and a resync is needed.
- Owner ruling, 2026-09-30: log lines may start a check or a repair and may hint at which chunks to check first, but their absence proves nothing, because logs rotate and users clear them.
- Owner decisions, 2026-10-16: the repair asks before the partial sync that installs a fresh ledger, and the background scan for installs without symptoms will use this plan's boundary scan once it is measured.
- The detection, the rebuild and the offer screen come from the chain-db-repair plan. This plan builds on them and falls back to them.
- Owner direction, 2026-10-22: the work that follows the rebuild includes resyncing cardano-wallet, because a wallet that crossed a hole without an error keeps missing the hole's blocks after a rebuild. Whether that resync belongs in this plan or elsewhere is an open question in the PRD.
- The research stays in `../chain-db-repair/research/`, and this plan references it there.
- Experiments that damage or repair a chain database run on preprod or preview, never on a mainnet install that is in use.
