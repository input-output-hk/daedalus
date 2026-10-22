# Chain Database Targeted Repair PRD

## Overview

When cardano-node or cardano-wallet output shows a hole in the local chain database, Daedalus reports `chain_damage` and offers to rebuild the whole database with Mithril, keeping the user's wallets. The [chain-db-repair](../chain-db-repair/chain-db-repair-prd.md) plan delivers that detection, the rebuild and the offer screen. This plan adds a targeted repair that runs before the offer: it finds the hole with a fast boundary scan, replaces only the damaged chunks with certified copies, and, after asking the user, installs a fresh certified ledger. If it works for a symptom, it runs first, and the full rebuild becomes the fallback.

The repair is gated on a spike. Phase 1 measures the scan natively on Windows and establishes, for each symptom, whether replacing the chunks recovers the node and the wallet and whether a fresh ledger is also needed. Phase 2 builds the repair if it does.

## Problem Statement

The rebuild replaces the whole database, about 217 GB on mainnet (measured 2026-09-30), and Daedalus cannot be used until the download finishes. The damage it repairs is much smaller. A #3416 hole is an incomplete chunk followed by complete ones, and in every field case and in #3416 itself one chunk is incomplete, out of 9,274 on the mainnet install measured in [research/03](../chain-db-repair/research/03-boundary-scan.md).

Locating the hole is what makes a whole-database check slow. On a mainnet install, the node's full chunk validation took 1 h 26 min to reach the damaged chunk, and a full-range `cardano-db verify` took 3 h 23 min. A boundary scan that reads two index entries and one header per chunk covered the same install in 184.8 s over a 9P mount, the slowest access path on that machine ([research/03](../chain-db-repair/research/03-boundary-scan.md)).

Replacing the chunks may not be enough on its own. Where the ledger snapshot sat relative to the hole decides the symptom ([research/01](../chain-db-repair/research/01-field-log-signatures.md)). Filling the hole is expected to be enough for the wallet symptom, `wallet_chain_discontinuity`. The two node symptoms, `node_rejects_canonical_block` and `ledger_point_missing`, are expected to also need a ledger that never replayed across the hole, which the existing partial sync installs. Phase 1 establishes which, for each kind.

## Goals

- Locate every hole in the ImmutableDB from the chunk indexes with a boundary scan in the watchdog, and measure the scan natively on Windows.
- Establish on preprod, for each symptom, whether replacing the incomplete chunks with certified copies recovers the node and the wallet, and whether a fresh certified ledger is also needed.
- If it does, run the targeted repair first on `chain_damage`, swap the chunks without a prompt, ask before installing a fresh ledger, and fall back to the rebuild offer.
- Log each replaced chunk in `watchdog.log` and list it in Diagnostics.

## Non-Goals

- Replacing the rebuild. It stays the fallback whenever the repair cannot locate or fix the damage (decision 4).
- Finding holes on installs that show no symptoms. The background scan of decision 1 is separate work, which will use this plan's boundary scan once phase 1 has measured it (decision 6).
- Corruption inside a chunk (`ChecksumMismatchError`, the #3409 class). `cardano-db verify` remains the check for it (decision 6).
- A hole inside the last chunk, which the node is still appending to, or between the ImmutableDB and the VolatileDB. The boundary scan sees neither ([research/03](../chain-db-repair/research/03-boundary-scan.md)).
- Byron-era chunk boundaries. Holes come from partial syncs near the tip, so the scan skips Byron headers.

## Inputs And Source Material

- [chain-db-repair-prd.md](../chain-db-repair/chain-db-repair-prd.md): the detection, the rebuild and the offer screen this plan builds on, and the decisions there that the repair follows (14, 19, 22 and 34).
- [research/01-field-log-signatures.md](../chain-db-repair/research/01-field-log-signatures.md): the three signatures, and how each field install behaved.
- [research/03-boundary-scan.md](../chain-db-repair/research/03-boundary-scan.md) and `../chain-db-repair/research/03-boundary-scan-prototype.py`: the scan's method, its measurements, and what it does not cover.
- A preprod experiment with `mithril-client` 0.13.9 against the release-preprod aggregator: `cardano-db verify --start N --end N` run over a stopped node's chain directory names a tampered chunk file, and `cardano-db download --start N --end N --allow-override` replaces only that file, byte for byte. An unscoped `verify --allow-missing` reported the tampered file in its JSON but exited 0, so a caller reads the JSON, not the exit code.
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`: the partial sync that installs a fresh certified ledger.
- `.agent/system/architecture.md`, `.agent/workflows/ipc.md`, `.agent/workflows/frontend.md`, `.agent/skills/i18n-messaging/SKILL.md`.
- Public issue #3416.

## Locked Planning Decisions

Decisions 1 to 3 are the chain-db-repair plan's owner decisions of the same numbers and apply here unchanged. Decisions 4 to 6 were decisions 8, 11 and 12 of that plan.

Owner decisions:

1. **2026-09-29.** Replacing damaged chunks with certified copies needs no prompt. The replacement is fixed public chain data verified against the Mithril certificate, so the user has nothing to decide. Each repair is logged in `watchdog.log` and shown in Diagnostics. The user is told when a swap cannot fix the damage and a resync is needed. The same decision schedules a background full-range `cardano-db verify` once on the first start of the release with the #3416 fix, and again on node chain errors.
2. **2026-09-30.** A log line may start a check or a repair, and may hint at where to look. Its absence proves nothing, because logs rotate and users clear them. In this plan, signatures only trigger. Damage is located by the boundary scan or `cardano-db verify`, never from the `local=` lines in `watchdog.log`.
3. **2026-10-16.** Build the visible rebuild offer for installs already showing a signature, and plan the targeted repair behind the same trigger.

Plan decisions:

4. The targeted repair replaces chunks without asking (decision 1) and asks before installing a fresh ledger (decision 5). It falls back to the offer screen when it cannot locate or fix the damage.

Owner decisions on the questions raised while planning:

5. **2026-10-16.** The targeted repair asks before the partial sync that installs a fresh ledger, because the node stays offline while it runs. Decision 1 still covers the chunk swap, which needs no prompt. A kind that phase 1 shows needs no fresh ledger is repaired without a prompt.
6. **2026-10-16.** The background scan from decision 1 finds holes with the boundary scan instead of a full-range `cardano-db verify`, once phase 1 has measured the Rust scan on Windows. `verify` remains the check for corruption inside a chunk. The scan reads chunk indexes, not logs, so it keeps decision 2.

Owner decision of 2026-10-22:

7. **2026-10-22.** The targeted repair is planned here, apart from the detection, the rebuild and the offer screen, which ship first, on their own, in a patch release (chain-db-repair decision 35). The repair builds on what that release delivers and falls back to it. Chain-db-repair decision 35 replaces its decision 10, which held the rebuild back until the targeted repair was ready.

## Requirements

### Functional Requirements

FR1 to FR5 were FR17 to FR21 of the chain-db-repair plan.

- [ ] FR1. A boundary scan in the watchdog reports every break in hash linkage between non-empty post-Byron chunks, and every missing `.chunk`, `.primary` or `.secondary` file.
- [ ] FR2. On `chain_damage`, with the wallet and node stopped, the watchdog does the following, then restarts the node and wallet:
  - runs the scan;
  - for each break, downloads every chunk from the last non-empty chunk before the break through the chunk before the one where the break shows, with certificate verification, into staging;
  - swaps the downloaded chunks in only after every download has verified;
  - for each kind that needs a fresh certified ledger, asks, then runs the partial sync that installs one (decision 5).
- [ ] FR3. The renderer shows a "Repairing blockchain data" screen with the phase and progress. Before the partial sync it asks whether to install a fresh verified ledger, with Install and Not now. Not now leaves the replaced chunks in place, restarts the node and wallet on the old ledger, and shows nothing more until the app restarts; Diagnostics keeps the detection and the manual rebuild.
- [ ] FR4. If the scan finds no break or a step before the swap fails, the chain is left as it was. If the ledger install fails after the swap, the replaced chunks stay, since they are certified copies. Either way the offer screen appears. A repair runs at most once per app session.
- [ ] FR5. Each replaced chunk is logged in `watchdog.log` and listed in Diagnostics.

### Non-Functional Requirements

- The boundary scan reads only index entries and first headers. Over a 9P mount it measured 185 s for mainnet. Native reads must be no slower, which phase 1 measures.
- Copy states no size, duration or percentage that the code does not measure.
- New copy is added in en-US, and in ja-JP as the en-US text with the `!!!` prefix, as chain-db-repair decision 30 set for that plan.
- All three desktop platforms, with Windows junctions and macOS and Linux symlinks for custom chain locations.

## Technical Design

### What This Plan Builds On

The chain-db-repair plan delivers these, and the repair reuses them:

- The `chain_damage` event (`Event::ChainDamage` in `watchdog/src/protocol.rs`), with kinds `node_rejects_canonical_block`, `ledger_point_missing` and `wallet_chain_discontinuity`, the slots, and `node_held`. `ChainDamageDetector` in `watchdog/src/chain_damage.rs` raises it from node and wallet output, and `check_before_start` in `watchdog/src/ledger_point.rs` raises it before a node start and holds the node. The repair starts from this event.
- The rebuild, `start_mithril` with `wipe_chain: true` (`run_pipeline` in `watchdog/src/mithril.rs`): the probe before anything is deleted, the free-space check, the record of an unfinished rebuild or install (`chain-rebuild.json` in the state directory), and the wait for the user's choice after a failure (`await_chain_choice` in `watchdog/src/supervisor.rs`). It stays the fallback.
- The partial sync (`run_pipeline` forced, without `wipe_chain`), which installs a fresh certified ledger and replaces `ledger/` and `lsm/` (`install_staged` in `watchdog/src/mithril.rs`). It records itself as an install of kind `install` and deletes `clean` before it changes the database (chain-db-repair decisions 19 and 22).
- Staging inside the chain folder, in the `mithril-staging` entry on the chain's own volume, with the `network-magic` marker that names its network (`prepare_staging` in `watchdog/src/mithril.rs`, chain-db-repair decisions 14 and 34). Installs rename within that volume (`rename_with_retry`, `move_file`). `full_install_target` resolves links and junctions, and `check_replaceable` checks that a folder holds only database entries.
- In main: `chainDamage`, `lastChainDamage` and `nodeHeld` on `WatchdogState` (`source/common/types/watchdog.types.ts`), set in `source/main/WatchdogManager.ts`, and `MITHRIL_COMMAND_CHANNEL` (`source/common/ipc/api.ts`), which forwards commands to the watchdog verbatim.
- In the renderer: the `chain-damaged` loading phase, `rebuildChain()` and `dismissChainDamage()` in `source/renderer/app/stores/BackendStore.ts`; `ChainDamageContainer`, `ChainDamageView` and `RebuildConfirmView` on the full-screen loading backdrop; and the Blockchain data row with its Rebuild blockchain data action in `source/renderer/app/components/status/DaedalusDiagnostics.tsx`.
- In tests: the lines the mock node and mock wallet print from a file (`MOCK_NODE_EXTRA_LINES`, `MOCK_WALLET_EXTRA_LINES`), and the mock `mithril-client` binaries in `watchdog/src/bin/`.

### Components Affected

- `watchdog/src/chain_scan.rs` (new): the boundary scan (FR1).
- `watchdog/src/chain_repair.rs` (new): the repair pipeline (FR2, FR4, FR5).
- `watchdog/src/supervisor.rs`: runs the repair on `chain_damage`, with the wallet and node stopped.
- `watchdog/src/protocol.rs`: the `chain_repair_status` event and the `confirm_repair_ledger` command.
- `watchdog/tests/integration.rs`.
- `source/common/types/watchdog.types.ts`, `source/main/WatchdogManager.ts` and `source/common/ipc/api.ts`: the `chainRepair` field and the command.
- `source/renderer/app/stores/BackendStore.ts` and a new `components/loading/chain-damage/ChainRepairView.tsx`: the repairing screen and the ledger confirmation (FR3).
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`: the chunks replaced this session (FR5).
- `source/renderer/app/i18n/locales/{en-US,ja-JP,defaultMessages}.json`.

### Data / IPC / API Changes

Phase 2 adds `chain_repair_status { phase, chunks, progress }` with phases `scanning`, `downloading`, `swapping`, `awaiting_ledger_confirmation`, `syncing`, `completed` and `failed`, a `chainRepair` field on the polled state, and a `confirm_repair_ledger { install }` command sent over `MITHRIL_COMMAND_CHANNEL`. There is no new IPC channel.

### UI / Store / Process Changes

The repairing screen and its ledger confirmation follow `ChainDamageView` and `RebuildConfirmView` on the same full-screen backdrop. Scanning, downloading and swapping inform, with Quit as the only action (decision 1). `awaiting_ledger_confirmation` shows Install and Not now, which send `confirm_repair_ledger` (decision 5). A repair that fails shows the offer screen.

### Repair Sequence

1. On `chain_damage`, stop the wallet and the node.
2. Scan the ImmutableDB as in [research/03](../chain-db-repair/research/03-boundary-scan.md). A break before chunk M means the chunks from N (the last non-empty chunk before M) to M-1 are incomplete. In every field case and in #3416 itself, that is chunk N alone.
3. For each break, run `mithril-client cardano-db download --start N --end M-1` into a staging directory inside the chain folder (chain-db-repair decision 14), without ancillary files. The client verifies the files against the certificate.
4. When every download has verified, move each chunk's `.chunk`, `.primary` and `.secondary` into `immutable/`.
5. If the kind needs a fresh ledger, emit `awaiting_ledger_confirmation` and wait for the user (decision 5). On Install, run the existing partial sync (`run_pipeline` forced, no wipe), which installs a fresh certified ledger and replaces `ledger/` and `lsm/` (`install_staged` in `watchdog/src/mithril.rs`). Filling the hole is enough for the wallet symptom. The two node symptoms also need a ledger that never replayed across the hole. On Not now, go to step 6 and do not ask again this session.
6. Restart the node and wallet. If a signature reappears in the same session, show the offer screen and do not repair again.

Phase 1 decides whether step 5 is needed for each kind, whether the wallet recovers without a resync, and how long the scan takes natively on Windows. The answers go in `research/01-targeted-repair-spike.md`.

## Implementation Strategy

1. **Targeted repair spike.** Rust boundary scan with tests and Windows timing, a recovery experiment per symptom on preprod, and a decision record.
2. **Targeted repair and release preparation.** Built if phase 1 succeeds: the repair pipeline, the repairing screen with the ledger confirmation, Diagnostics history, verification, and the CHANGELOG entry.

The plan lands as one epic branch cut from `origin/master` once the chain-db-repair plan has merged, with one draft pull request. Each task is one commit, and the branch is pushed once per phase.

The phases and tasks were phases 6 and 7 of the chain-db-repair plan, renumbered here:

| This plan | chain-db-repair plan | Title |
|---|---|---|
| phase-1 | phase-6 | Targeted repair spike |
| task-001 | task-019 | Boundary scan in Rust |
| task-002 | task-020 | Recovery experiment per symptom |
| task-003 | task-021 | Decision record |
| phase-2 | phase-7 | Targeted repair and release preparation |
| task-004 | task-022 | Repair pipeline in the watchdog |
| task-005 | task-023 | Repairing screen and ledger confirmation |
| task-006 | task-024 | Repair history in Diagnostics |
| task-007 | task-025 | Verify the targeted repair |
| task-008 | none | CHANGELOG entry for the targeted repair; task-018 there keeps the entry for the rebuild |

## Testing Strategy

Watchdog unit tests (`chain_scan.rs`, on chunk fixtures):

- Two consecutive mainnet chunks link, and the same pair with the first chunk's last 500 index entries removed reports one break.
- An empty chunk between two linked chunks is carried across without a break.
- A missing `.chunk`, `.primary` or `.secondary` is reported as damage.
- A header whose blake2b-256 differs from the hash in its index entry is reported as a scan failure, not as a hole.

Watchdog integration tests, with a mock `mithril-client`:

- A scan with one break downloads that chunk, swaps it, runs the partial sync, and restarts the node and wallet.
- A failed download leaves every chain file unchanged and keeps `chain_damage` set.
- A scan with no break downloads nothing and keeps `chain_damage` set.
- A kind that needs a fresh ledger stops at `awaiting_ledger_confirmation`. `confirm_repair_ledger { install: false }` restarts the node without the partial sync and does not ask again this session.
- A signature after a repair in the same session leads to the offer screen without a second repair.

Jest: `chain_repair_status` in `WatchdogManager.spec.ts` and `BackendStore.spec.ts`, the repairing view in each phase with its Install and Not now actions, and the replaced chunks in `DaedalusDiagnostics.spec.tsx`.

Storybook: the repairing view in each phase and the ledger confirmation, in en-US and ja-JP.

Nix checks, never the yarn scripts alone: `watchdog-clippy`, `watchdog-test`, `jest`, `compile`, `lint`, `stylelint`, `i18n`, `storybook`, `treefmt`, `prettier-version-parity`.

Manual, on preprod (Linux under WSL2 and Windows). Experiments that damage or repair a chain database run on preprod or preview, never on a mainnet install that is in use. The damaged chains come from the reproduction of an 11.4.0 hole in the chain-db-repair plan's Testing Strategy: its steps 1 to 3 give a chain whose ledger lies after the hole, its step 6 one whose ledger snapshot lies inside the hole, and deleting `ledger` and `lsm` as in its step 5 gives a ledger that replays across the hole.

1. Time the Rust scan from a Windows build of the watchdog over a preprod and a mainnet install, and record the times in `research/01-targeted-repair-spike.md`.
2. On copies of each damaged chain, with the node stopped, download the incomplete chunks with `mithril-client cardano-db download --start N --end M-1` into an empty directory, without `--include-ancillary`, and move the three files of each chunk into `immutable/`. For the wallet symptom, try the swap alone first, then with the partial sync. For the node symptoms, swap and run the partial sync. Record for each reproducible symptom whether the node reaches the tip, whether the wallet finishes without a resync, and whether the partial sync was needed.
3. With the repair built, repeat step 2 through the app on Linux and on Windows, including Not now at the ledger confirmation.

## Rollout / Migration / Rollback

- No data migration and no new persisted state. A repair's once-per-session limit lives in memory. The swap changes only the chunk files it replaces with certified copies, and the ledger changes only through the existing partial sync.
- The repair ships after the chain-db-repair plan's patch release, which it builds on (decision 7). Its CHANGELOG entry describes the targeted repair and its ledger confirmation. The damaged-data screen, the rebuild and the Diagnostics action are in the earlier release's entry.
- Rollback is a revert. `chain_repair_status` and `confirm_repair_ledger` are additive. Without the repair, `chain_damage` leads to the offer screen and the rebuild, as in the chain-db-repair release.

## Open Questions

1. Whether this plan includes a wallet resync. A wallet restore can cross a hole without signature 3, because cardano-wallet checks only the first block of each batch it applies against the wallet's checkpoint (`lib/wallet/src/Cardano/Wallet.hs:1494` at cardano-wallet rev `33fcb5e4104d`). A batch holds up to as many blocks as the wallet's chain-sync pipelining allows, which it sets by block height: 1,000 up to height 5,200,000, 200 up to 6,100,000, 125 up to 6,500,000 and 100 above (`lib/network-layer/src/Cardano/Wallet/Network/Implementation/Ouroboros.hs:391-395` at the same rev). On preprod the first block of chunk 6331, at slot 136,749,698 in the reproduction of the chain-db-repair plan, is at height 5,311,346 (read 2026-10-22), so a batch there holds up to 200 blocks. The mainnet holes in [research/01](../chain-db-repair/research/01-field-log-signatures.md) lie above height 14,000,000, where a batch holds up to 100. A wallet that crossed a hole this way holds none of the hole's blocks. Neither the rebuild nor this plan's chunk swap adds them, because the wallet continues from its checkpoint after the hole; only a wallet resync does. No task in this plan resyncs a wallet. Task-002 records whether a wallet that stopped at the hole with signature 3 recovers without a resync.
2. Whether the chunk swap records itself and deletes `clean` before it changes the database, as every install in the chain-db-repair plan does (its decisions 19 and 22). FR4 covers a failure before the swap and one after it, not a swap that stops part-way. With `clean` in place, the node validates only its most recent chunks at the next start, so it would not find a chunk left partly replaced.

**Status:** Draft
**Date:** 2026-10-22
**Author:** Adam Dean
