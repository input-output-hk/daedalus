# Chain Database Repair PRD

## Overview

When cardano-node or cardano-wallet output shows that the local chain database has a hole in its ImmutableDB, Daedalus tells the user what is wrong and offers to rebuild the database with Mithril, keeping their wallets. This plan builds that in three parts: detection in the watchdog from three log signatures seen in the field, a rebuild path that is safe to offer, and a full-screen offer in the renderer with a matching action in Diagnostics.

A fourth part is gated on a spike: a targeted repair that finds the hole with a fast boundary scan, replaces only the damaged chunks with certified copies, and, after asking the user, installs a fresh certified ledger. If it works for a symptom, it runs first, and the full rebuild becomes the fallback. All four parts ship in one release (decision 10).

## Problem Statement

Daedalus 11.4.0 and earlier started a Mithril partial sync one chunk above the highest local chunk. When the local chain was behind the certified snapshot, the incomplete local chunk stayed in place and complete chunks were installed after it, so the blocks missing from its tail never reached the ImmutableDB (public issue #3416). The sync also installed the `clean` marker (`watchdog/src/mithril.rs:529`), so the node skipped full chunk validation and nothing noticed the hole until something read through it. 11.5.0 corrects the download range (`watchdog/src/mithril.rs:110`) and creates no new holes, but repairs none.

Four Windows mainnet installs on 11.5.0 reported this damage in support diagnostics received on 2026-10-15 ([research/01](research/01-field-log-signatures.md)). Three symptoms appear, depending on where the ledger snapshot sat relative to the hole. Installs are labeled A to D as in research/01. Install C showed all three in turn, as successive Mithril syncs moved its ledger snapshot:

| Ledger snapshot | What the user sees | Log signature | Installs |
|---|---|---|---|
| Before the hole | Sync never progresses | `BadInputsUTxO` under `ChainSync.Client.Exception`; the most-rejected block in each install came from 12 to 26 distinct peers | A, B, D; C earlier |
| Inside the hole | Startup never gets past opening the database | `Critical invariant violation ... that was in immutable db is gone before we could open ledgerdb` | C |
| After the hole | The node syncs and a wallet stops syncing | `restoreBlocks: given chain isn't a valid continuation` from cardano-wallet | C |

None of these reaches the user as an explanation. In the second case the watchdog logs no node exit and the loading screen waits at the VolatileDB phase until the user quits. One user retried for 3.8 days across several Mithril syncs and restarts. Their only remedy is deleting the chain folder by hand.

The watchdog already has a full rebuild (`start_mithril` with `wipe_chain: true`), but nothing in the renderer sends it (`source/renderer/app/stores/BackendStore.ts:312-318`). It is also not safe to offer as written ([research/02](research/02-code-paths-at-11.5.0.md)):

- It deletes the chain before checking that the aggregator answers.
- If the download fails, it starts the node on the empty directory, which syncs from genesis without asking.
- It refuses any chain folder the user picked outside the state directory.

The wallet databases are intact in all four installs. These users need the chain database replaced or patched, and a screen that tells them so.

## Goals

- Recognize each of the three signatures from the node and wallet output the watchdog already pipes: on the first occurrence of the second and third, and on the third distinct peer for the first.
- Show a full-screen explanation with a rebuild action. It takes precedence over every other loading screen except quitting and a running Mithril sync, including after the wallet is ready.
- Make the rebuild safe to offer:
  - nothing is deleted until the aggregator answers;
  - a rebuild whose download fails returns to the first-run choice instead of a silent genesis sync;
  - custom chain locations rebuild in place.
- Record each detection in `watchdog.log` and Diagnostics, and offer the same rebuild manually from Diagnostics.
- Establish on preprod whether a targeted repair recovers each symptom. If it does, run it first, ask before it installs a fresh ledger, and fall back to the rebuild.

## Non-Goals

- Finding holes on installs that show no symptoms. The background scan in the 2026-09-29 decision is separate work. That work will use this plan's boundary scan as its detector once phase 6 has measured it (decision 12).
- Corruption inside a chunk (`ChecksumMismatchError`, the #3409 class). The event carries a `kind`, so another kind can be added later, but no parser for it is written here.
- Persisting a detection across restarts. Each node run detects from its own output, and the signatures appear within minutes of a start. A stored marker could outlive a manual fix.
- Changing the node's own validation after an unclean start. It already finds a hole and truncates the chain there, at a cost of hours.
- Showing download size or duration. The probe reads only the certified chunk number (`watchdog/src/mithril.rs:40-47`), and there is no measured figure to show.

## Inputs And Source Material

- [research/01-field-log-signatures.md](research/01-field-log-signatures.md): the three signatures, with redacted lines, counts, matching rules, and how each install behaved.
- [research/02-code-paths-at-11.5.0.md](research/02-code-paths-at-11.5.0.md): the rebuild path, supervisor states, log parsing, event plumbing, and screen selection at `22440487d`.
- [research/03-boundary-scan.md](research/03-boundary-scan.md) and `research/03-boundary-scan-prototype.py`: locating a hole from the chunk indexes in about 3 minutes on mainnet, against hours for a full read.
- A preprod experiment with `mithril-client` 0.13.9 against the release-preprod aggregator: `cardano-db verify --start N --end N` run over a stopped node's chain directory names a tampered chunk file, and `cardano-db download --start N --end N --allow-override` replaces only that file, byte for byte. An unscoped `verify --allow-missing` reported the tampered file in its JSON but exited 0, so a caller reads the JSON, not the exit code.
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` and `.agent/plans/mithril/mithril-snapshot-ux.md`: the partial sync and the existing Mithril screens.
- `.agent/system/architecture.md`, `.agent/workflows/ipc.md`, `.agent/workflows/frontend.md`, `.agent/skills/i18n-messaging/SKILL.md`.
- Public issue #3416.

## Locked Planning Decisions

Owner decisions:

1. **2026-09-29.** Replacing damaged chunks with certified copies needs no prompt. The replacement is fixed public chain data verified against the Mithril certificate, so the user has nothing to decide. Each repair is logged in `watchdog.log` and shown in Diagnostics. The user is told when a swap cannot fix the damage and a resync is needed. The same decision schedules a background full-range `cardano-db verify` once on the first start of the release with the #3416 fix, and again on node chain errors.
2. **2026-09-30.** A log line may start a check or a repair, and may hint at where to look. Its absence proves nothing, because logs rotate and users clear them. In this plan, signatures only trigger. Damage is located by the boundary scan or `cardano-db verify`, never from the `local=` lines in `watchdog.log`.
3. **2026-10-16.** Build the visible rebuild offer for installs already showing a signature, and plan the targeted repair behind the same trigger.

Plan decisions:

4. Detection lives in the watchdog. It already pipes the output of both processes; the renderer sees only polled state.
5. The first signature fires when three distinct peers reject the same block. One misbehaving peer cannot reach that threshold. In every field install, at least one rejected block came from 12 or more peers.
6. The rebuild deletes the old chain before downloading the new one. Staging the download beside the old chain would keep it if the download failed, but it needs room for two full databases, about 217 GB each on mainnet (measured 2026-09-30). The old chain is already damaged. A failed or cancelled download leads back to the first-run choice.
7. The offer screen asks before rebuilding, because the rebuild deletes local data and takes the app out of use for a full download. "Not now" dismisses the screen until the app restarts; Diagnostics keeps the detection and the manual rebuild.
8. The targeted repair replaces chunks without asking (decision 1) and asks before installing a fresh ledger (decision 11). It falls back to the offer screen when it cannot locate or fix the damage.
9. There is one `chain_damage` event with a `kind` field rather than one event per signature, so a later kind reuses the screen and the state.

Owner decisions on the questions raised while planning:

10. **2026-10-16.** The rebuild offer does not ship ahead of the targeted repair. Phases 1 to 7 go out in one release. If phase 6 shows the targeted repair cannot recover a symptom, its decision record says so, and the owner decides then whether to release without it.
11. **2026-10-16.** The targeted repair asks before the partial sync that installs a fresh ledger, because the node stays offline while it runs. Decision 1 still covers the chunk swap, which needs no prompt. A kind that phase 6 shows needs no fresh ledger is repaired without a prompt.
12. **2026-10-16.** The background scan from decision 1 finds holes with the boundary scan instead of a full-range `cardano-db verify`, once phase 6 has measured the Rust scan on Windows. `verify` remains the check for corruption inside a chunk. The scan reads chunk indexes, not logs, so it keeps decision 2.
13. **2026-10-16.** An install that fails after a rebuild keeps the existing strict wait: the Mithril error view with Retry, no node start, and no `chain_status { has_chain: false }`.

## Requirements

### Functional Requirements

Detection (watchdog):

- [ ] FR1. Emit `chain_damage` with kind `node_rejects_canonical_block` when one rejected slot has been reported by 3 distinct `remoteAddress` values in `ChainSync.Client.Exception` lines that contain `BadInputsUTxO`. Carry that slot.
- [ ] FR2. Emit kind `ledger_point_missing` on the first node line that contains both `Critical invariant violation` and `that was in immutable db is gone before we could open ledgerdb`. Carry `blockPointSlot`.
- [ ] FR3. Emit kind `wallet_chain_discontinuity` on the first wallet line that contains `restoreBlocks: given chain isn't a valid continuation`. Carry the first slot after the gap and the wallet's last good slot.
- [ ] FR4. Emit each kind at most once per node process (per wallet process for FR3), and write a `WARN` line to `watchdog.log` with the kind and slots.

Rebuild (watchdog):

- [ ] FR5. With `wipe_chain`, probe the aggregator before deleting anything. A probe failure leaves the chain untouched and reports `PROBE_FAILED` as today. A rebuild always installs a full bootstrap and skips the check that the local chain is already current, so a chain that is level with the certified tip is still replaced.
- [ ] FR6. If the download fails or is cancelled after the wipe, do not start the node. Report `chain_status { has_chain: false }` and wait as on first run, so the renderer shows the bootstrap decision. Until `set_chain_path` changes the chain directory, a `start_mithril` sent in that wait is also a rebuild, so it deletes what a failed wipe left and installs a full bootstrap. A failure to wipe the old chain reports `WIPE_FAILED` and ends in the same wait. An install that fails after a rebuild keeps the existing strict wait: the Mithril error view with Retry, no node start and no `has_chain: false`.
- [ ] FR7. Resolve the chain directory the way full bootstrap does (`full_install_target`). Refuse with `CHAIN_DIR_NOT_REPLACEABLE` when it holds anything other than chain database files (`check_replaceable`). Otherwise, delete the real directory's contents and keep any link or junction that points to it. A user-picked folder outside the state directory rebuilds in place.
- [ ] FR8. Pass the `code` of `mithril_error` through to the renderer as `lastErrorCode`.

Offer screen (main and renderer):

- [ ] FR9. `chainDamage` in the polled state: set by `chain_damage`, cleared by `node_started` and by `mithril_status` with phase `preparing`.
- [ ] FR10. A loading phase `chain-damaged` that takes precedence over everything except `stopping` and a running Mithril pipeline, `ready` and `error` included.
- [ ] FR11. Choice view: a symptom sentence for the kind, what the rebuild does, that wallets and funds are safe and no recovery phrase is needed, and a detail line with the kind and slot for support. Actions: Rebuild blockchain data, Not now.
- [ ] FR12. Confirm view: what will be deleted and downloaded, and that the app is unusable until it finishes. Actions: Rebuild, Back. Rebuild sends `{ cmd: 'start_mithril', wipe_chain: true }`, and the existing Mithril progress view takes over. Rebuild also marks the chain storage location as confirmed, because the rebuild targets the location in use. A rebuild that fails and leaves no chain therefore shows the Mithril error view with the reason and then the bootstrap decision, without the chain storage location picker.
- [ ] FR13. Not now hides the screen until the app restarts.
- [ ] FR14. The Mithril error view shows a specific message for `PROBE_FAILED`, `INVALID_CHAIN_PATH`, `CHAIN_DIR_NOT_REPLACEABLE`, `CHAIN_DIR_OTHER_NETWORK` and `WIPE_FAILED`, and its Retry and Skip buttons are translated (they are hard-coded English at `MithrilSyncContainer.tsx:50-70`).
- [ ] FR15. Diagnostics shows the last detection of the session (kind, slot, time) and has a Rebuild blockchain data action behind the same confirmation, whether anything was detected.
- [ ] FR16. All new copy exists in en-US and ja-JP.

Targeted repair (gated on phase 6):

- [ ] FR17. A boundary scan in the watchdog reports every break in hash linkage between non-empty post-Byron chunks, and every missing `.chunk`, `.primary` or `.secondary` file.
- [ ] FR18. On `chain_damage`, with the wallet and node stopped, the watchdog does the following, then restarts the node and wallet:
  - runs the scan;
  - for each break, downloads every chunk from the last non-empty chunk before the break through the chunk before the one where the break shows, with certificate verification, into staging;
  - swaps the downloaded chunks in only after every download has verified;
  - for each kind that needs a fresh certified ledger, asks, then runs the partial sync that installs one (decision 11).
- [ ] FR19. The renderer shows a "Repairing blockchain data" screen with the phase and progress. Before the partial sync it asks whether to install a fresh verified ledger, with Install and Not now. Not now leaves the replaced chunks in place, restarts the node and wallet on the old ledger, and shows nothing more until the app restarts; Diagnostics keeps the detection and the manual rebuild.
- [ ] FR20. If the scan finds no break or a step before the swap fails, the chain is left as it was. If the ledger install fails after the swap, the replaced chunks stay, since they are certified copies. Either way the offer screen appears. A repair runs at most once per app session.
- [ ] FR21. Each replaced chunk is logged in `watchdog.log` and listed in Diagnostics.

### Non-Functional Requirements

- No trigger from a single peer, or from an invalid block that the rest of the network does not serve.
- Parsers handle ANSI escape sequences, lines up to the watchdog's 1 MiB cap (field lines reach 35 KB), and a message that runs into the following text without a newline (signature 2).
- Lines that match no signature pay only a substring check. The node logs every rejected peer, thousands of lines per hour while stuck.
- The rebuild never deletes a directory that fails `check_replaceable`, and never ends in a genesis sync the user did not choose.
- Copy states no size, duration or percentage that the code does not measure.
- All three desktop platforms, with Windows junctions and macOS and Linux symlinks for custom chain locations.
- The boundary scan reads only index entries and first headers. Over a 9P mount it measured 185 s for mainnet. Native reads must be no slower, which phase 6 measures.

## Technical Design

### Components Affected

- `watchdog/src/chain_damage.rs` (new): the three signature parsers and a `ChainDamageDetector` that holds per-slot peer sets for signature 1 (bounded: the most recent 16 slots, 3 peers each) and the once-per-process flags.
- `watchdog/src/supervisor.rs`: node output calls the detector from `pipe_to_log`, next to `try_parse_startup_status` (`supervisor.rs:556-579`). Wallet output turns parsing on with a wallet-only parser (today `pipe_to_log(..., false, None)` at `supervisor.rs:1805-1816`). After a failed or cancelled post-wipe download, it waits in the no-chain state (`supervisor.rs:914`) instead of restarting (`supervisor.rs:1211-1213`).
- `watchdog/src/protocol.rs`: `Event::ChainDamage`.
- `watchdog/src/mithril.rs`: the wipe moves after the probe and goes through `full_install_target` and `check_replaceable`. A distinct `PipelineResult` marks a failure after the wipe.
- `watchdog/src/bin/mock_node.rs` and the wallet mock: an environment variable naming a file of extra lines to print, so integration tests can inject field lines.
- `watchdog/tests/integration.rs`, `watchdog/tests/chain_storage.rs`.
- `source/common/types/watchdog.types.ts`: the `ChainDamage` and `ChainDamageKind` types, `chainDamage` and `lastErrorCode` on `WatchdogState`, and `'chain-damaged'` on `LoadingPhase`.
- `source/main/WatchdogManager.ts` and `source/main/ipc/index.ts`: event handling, state defaults, and the fallback state.
- `source/renderer/app/stores/BackendStore.ts`: the observable, the phase, `rebuildChain()` and `dismissChainDamage()`.
- `source/renderer/app/containers/loading/LoadingPage.tsx`, a new `containers/loading/ChainDamageContainer.tsx`, and new `components/loading/chain-damage/ChainDamageView.tsx` with `.scss` and `ChainDamage.messages.ts`.
- `source/renderer/app/containers/loading/MithrilSyncContainer.tsx` and `components/loading/mithril/MithrilErrorView.tsx`: error codes and translated buttons.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` and `containers/status/DaedalusDiagnosticsDialog.tsx`.
- `source/renderer/app/i18n/locales/{en-US,ja-JP,defaultMessages}.json`.
- `storybook/stories/loading/chain-damage/ChainDamageView.stories.tsx`.
- Phase 7: `watchdog/src/chain_scan.rs` and `watchdog/src/chain_repair.rs` (new), and a `ChainRepairView` in the renderer.

### Data / IPC / API Changes

New watchdog event, one JSON line as today:

```json
{"event":"chain_damage","kind":"node_rejects_canonical_block","slot":200361988,"last_good_slot":null}
{"event":"chain_damage","kind":"ledger_point_missing","slot":200102375,"last_good_slot":null}
{"event":"chain_damage","kind":"wallet_chain_discontinuity","slot":200102408,"last_good_slot":200080892}
```

`WatchdogState` gains:

```ts
type ChainDamageKind =
  | 'node_rejects_canonical_block'
  | 'ledger_point_missing'
  | 'wallet_chain_discontinuity';

type ChainDamage = {
  kind: ChainDamageKind;
  slot: number | null;
  lastGoodSlot: number | null;
  detectedAt: string;
};

chainDamage: ChainDamage | null;
lastErrorCode: string | null;
```

There is no new IPC channel. The renderer reads the state through the existing 2-second poll, and the rebuild uses `MITHRIL_COMMAND_CHANNEL`, which already forwards `wipe_chain` verbatim.

Phase 7 adds `chain_repair_status { phase, chunks, progress }` with phases `scanning`, `downloading`, `swapping`, `awaiting_ledger_confirmation`, `syncing`, `completed` and `failed`, a `chainRepair` field on the polled state, and a `confirm_repair_ledger { install }` command sent over `MITHRIL_COMMAND_CHANNEL`.

### UI / Store / Process Changes

`BackendStore.loadingPhase` gains one check directly after `isStopping`:

```ts
if (this.chainDamage && !this._chainDamageDismissed && !this._mithrilActive) {
  return 'chain-damaged';
}
```

Because the check runs before the unrecoverable-error and `ready` checks, a wallet discontinuity detected after the wallet is ready takes over the window. `LoadingPage` renders `ChainDamageContainer` on the same full-screen backdrop that `MithrilSyncContainer` uses. After Rebuild, `mithril_status: preparing` clears `chainDamage` and the phase becomes `mithril-syncing`, so the existing progress view shows the download. A failure lands on the existing error view, which this plan translates and makes code-aware.

The choice and confirm views follow `SyncingConnectingMithrilPrompt` (`components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:118-147`, `:199-230`).

Draft en-US copy, to be confirmed by phase 5 testing (the wallet sentence depends on the wallet result in Testing Strategy step 4, task-017 in the tasks file):

| Message | Draft |
|---|---|
| Title | Blockchain data needs to be rebuilt |
| Symptom, `node_rejects_canonical_block` | Daedalus cannot finish syncing because part of the blockchain data stored on this computer is missing. |
| Symptom, `ledger_point_missing` | Cardano node cannot start because part of the blockchain data stored on this computer is missing. |
| Symptom, `wallet_chain_discontinuity` | A wallet cannot finish syncing because part of the blockchain data stored on this computer is missing. |
| Body | Your wallets and funds are safe, and you do not need your recovery phrase. Daedalus can rebuild the blockchain data by downloading a verified copy with Mithril. |
| Detail | Detected: {kind} at slot {slot} |
| Actions | Rebuild blockchain data / Not now |
| Confirm title | Rebuild blockchain data? |
| Confirm body | Daedalus will delete the blockchain data on this computer and download a complete verified copy. This is a large download, and Daedalus cannot be used until it finishes. Your wallets stay on this computer and finish syncing afterward. |
| Confirm actions | Rebuild / Back |
| `PROBE_FAILED` | Daedalus could not reach the Mithril service. Your blockchain data has not been changed. Check your internet connection and try again. |
| `CHAIN_DIR_NOT_REPLACEABLE` | The blockchain data folder contains other files, so Daedalus did not delete it. Move those files elsewhere and try again. |
| `INVALID_CHAIN_PATH` | Daedalus cannot rebuild the blockchain data in this folder. Choose a different blockchain data location and try again. |
| `WIPE_FAILED` | Daedalus could not delete the blockchain data files. Close any other programs that might be using the blockchain folder, then retry. |
| `CHAIN_DIR_OTHER_NETWORK` | The blockchain data folder holds data for a different Cardano network, so Daedalus did not change it. Choose a different blockchain data location and try again. |
| Diagnostics | Blockchain data: No damage detected / Damage detected at slot {slot} ({time}). Action: Rebuild blockchain data |

New `defaultMessage` values carry the `!!!` prefix until reviewed, per `.agent/skills/i18n-messaging/SKILL.md:33`.

### Targeted Repair (phases 6 and 7)

1. On `chain_damage`, stop the wallet and the node.
2. Scan the ImmutableDB as in [research/03](research/03-boundary-scan.md). A break before chunk M means the chunks from N (the last non-empty chunk before M) to M-1 are incomplete. In every field case and in #3416 itself, that is chunk N alone.
3. For each break, run `mithril-client cardano-db download --start N --end M-1` into `<state_dir>/chain-repair/<N>`, without ancillary files. The client verifies the files against the certificate.
4. When every download has verified, move each chunk's `.chunk`, `.primary` and `.secondary` into `immutable/`.
5. If the kind needs a fresh ledger, emit `awaiting_ledger_confirmation` and wait for the user (decision 11). On Install, run the existing partial sync (`run_pipeline` forced, no wipe), which installs a fresh certified ledger and replaces `ledger/` and `lsm/` (`watchdog/src/mithril.rs:510-523`). Filling the hole is enough for the wallet symptom. The two node symptoms also need a ledger that never replayed across the hole. On Not now, go to step 6 and do not ask again this session.
6. Restart the node and wallet. If a signature reappears in the same session, show the offer screen and do not repair again.

Phase 6 decides whether step 5 is needed for each kind, whether the wallet recovers without a resync, and how long the scan takes natively on Windows. The answers go in `research/04-targeted-repair-spike.md`.

## Implementation Strategy

1. **Detection.** Event, parsers and detector, wallet output parsing, and mock-driven integration tests.
2. **Safe rebuild.** Probe before delete, no silent genesis sync, custom chain locations, and error codes passed to the renderer.
3. **Offer screen.** State plumbing, store phase and actions, choice and confirm views, translated and code-aware error view, copy and stories.
4. **Diagnostics.** Detection row and manual rebuild.
5. **Verification of the rebuild.** Nix checks and a reproduction of an 11.4.0 hole on preprod (Linux and Windows). The release waits for phase 7 (decision 10).
6. **Targeted repair spike.** Rust boundary scan with tests and Windows timing, a recovery experiment per symptom on preprod, and a decision record.
7. **Targeted repair and release preparation.** Built if phase 6 succeeds: the repair pipeline, the repairing screen with the ledger confirmation, Diagnostics history, verification, and the CHANGELOG entry.

The plan lands as one epic branch cut from `origin/master`, with one draft pull request. The plan workspace is the first commit, each task is one commit, and the branch is pushed once per phase.

## Testing Strategy

Watchdog unit tests (`chain_damage.rs`):

- Each signature, using the redacted field lines in research/01: lines with ANSI codes, the 35 KB line from install A, and signature 2 running into `Node configuration:` with no newline.
- Signature 1 fires on the third distinct peer for one slot and not on three lines from one peer. It ignores `InvalidBlock` lines without `BadInputsUTxO`, and keeps separate counts for two different slots.
- Each kind fires once per process.

Watchdog integration tests:

- The mock node prints signature 2 and the watchdog emits `chain_damage`. The mock node prints signature 1 from three peers and the event follows. The wallet mock prints signature 3 and the event follows.
- `wipe_chain` with a failing probe leaves the chain untouched.
- `wipe_chain` with a failing download emits `chain_status { has_chain: false }` and starts no node.
- `wipe_chain` on a link or junction replaces the target's contents and keeps the link.
- `wipe_chain` on a user-picked folder outside the state directory succeeds.
- `wipe_chain` on a folder holding a foreign file fails with `CHAIN_DIR_NOT_REPLACEABLE` and deletes nothing.
- Existing tests keep passing, notably `wipe_chain_full_bootstrap` (`watchdog/tests/integration.rs:1182`).

Jest:

- `WatchdogManager.spec.ts`: `chain_damage` sets `chainDamage`; `node_started` and `preparing` clear it; `mithril_error` keeps `code`.
- `BackendStore.spec.ts`: the phase precedence over `ready` and `error`; dismissal; `rebuildChain()` sends `wipe_chain: true`.
- A component spec for the choice-to-confirm flow.

Storybook: one story per kind, the confirm view, and the error view for each code in FR14, in en-US and ja-JP.

Nix checks, never the yarn scripts alone: `watchdog-clippy`, `watchdog-test`, `jest`, `compile`, `lint`, `stylelint`, `i18n`, `storybook`, `treefmt`, `prettier-version-parity`.

Manual, on preprod (Linux under WSL2 and Windows), reproducing a hole with 11.4.0 code:

1. List preprod snapshots and pick the one whose `immutable_file_number` is two below the newest. Download it with `--include-ancillary` into a scratch directory. Delete its `ledger` directory. Its highest chunk N is the unfinalized one from the ancillary archive.
2. With Daedalus Pre-Prod 11.4.0 closed, swap that directory in as the chain directory. Start the app and run Mithril Sync from Diagnostics while the node replays. `watchdog.log` shows `Mithril forced by caller: local=N, certified=C` with C greater than N.
3. Quit cleanly. `mithril-client cardano-db verify --start N --end N` reports three tampered files, and the boundary scan reports a break before chunk N+1.
4. Install the build under test. Restore a wallet from its recovery phrase. When its restore reaches chunk N, signature 3 appears and the offer screen follows. Rebuild, and record whether the wallet finishes without a resync. The confirmation copy depends on this result.
5. On a fresh copy of the damaged chain, delete `ledger` and `lsm` so the node replays from genesis across the hole. Record whether a later block that spends an output created in the hole arrives and triggers signature 1. On preprod's low transaction volume it may not, and the field-line tests cover detection.
6. Signature 2 has not been reproduced on demand. The field line covers detection, and the rebuild replaces every file it could involve.
7. On Windows, repeat steps 4 and 5 with the chain in a user-picked folder reached through a junction.

## Rollout / Migration / Rollback

- No data migration. Nothing is persisted: the detection lives in memory, and the rebuild uses the existing Mithril bootstrap.
- Phases 1 to 7 ship in one release (decision 10).
- The CHANGELOG entry describes the behavior a user sees: the damaged-data screen, the targeted repair and its ledger confirmation, the rebuild, and the Diagnostics action.
- Rollback is a revert. The new event is additive. The rebuild changes affect only `wipe_chain`, which nothing sends in 11.5.0.
- An affected install sees the screen on the first start after upgrading that produces a signature. Installs with holes but no symptoms see nothing until one appears, or until the background scan from decision 1 ships.

## Open Questions

None. Decisions 10 to 13 settled the questions raised while planning.

**Status:** In Progress
**Date:** 2026-10-16
**Author:** Adam Dean
