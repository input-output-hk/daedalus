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

None of these reaches the user as an explanation. In the second case cardano-node 11.1.3 logs `Replaying ledger from snapshot ...` and then writes nothing on either output stream until it is asked to stop. Only then does it write the error, within milliseconds of the request, and exit cleanly, so the loading screen waits until the user quits. The app keeps a detection only while it runs and clears it when a node starts (`chainDamage` in `source/main/WatchdogManager.ts`), so a detection made while quitting is lost and the next start waits again. One user retried for 3.8 days across several Mithril syncs and restarts. Their only remedy is deleting the chain folder by hand.

The watchdog already has a full rebuild (`start_mithril` with `wipe_chain: true`), but nothing in the renderer sends it (`source/renderer/app/stores/BackendStore.ts:312-318`). It is also not safe to offer as written ([research/02](research/02-code-paths-at-11.5.0.md)):

- It deletes the chain before checking that the aggregator answers.
- If the download fails, it starts the node on the empty directory, which syncs from genesis without asking.
- It refuses any chain folder the user picked outside the state directory.

The wallet databases are intact in all four installs. These users need the chain database replaced or patched, and a screen that tells them so.

## Goals

- Recognize each of the three signatures from the node and wallet output the watchdog already pipes: on the first occurrence of the second and third, and on the third distinct peer for the first.
- Recognize the second signature before the node starts as well, from the database files, and report it without starting the node, so the offer screen appears instead of a loading screen that waits (decision 24).
- Tell the user when the node has written nothing for 5 minutes while it starts, and offer a restart (decision 25).
- Say on the Mithril progress view when a download is waiting for the network (decision 28).
- Show a full-screen explanation with a rebuild action. It takes precedence over every other loading screen except quitting and a running Mithril sync, including after the wallet is ready.
- Make the rebuild safe to offer:
  - nothing is deleted until the aggregator answers;
  - nothing is deleted unless the volume that holds the chain has room for the download;
  - a rebuild whose download fails returns to the first-run choice instead of a silent genesis sync;
  - custom chain locations rebuild in place.
- Record each detection in `watchdog.log` and Diagnostics, and offer the same rebuild manually from Diagnostics.
- Establish on preprod whether a targeted repair recovers each symptom. If it does, run it first, ask before it installs a fresh ledger, and fall back to the rebuild.

## Non-Goals

- Finding holes on installs that show no symptoms. The background scan in the 2026-09-29 decision is separate work. That work will use this plan's boundary scan as its detector once phase 6 has measured it (decision 12).
- Corruption inside a chunk (`ChecksumMismatchError`, the #3409 class). The event carries a `kind`, so another kind can be added later, but no parser for it is written here.
- Persisting a detection across restarts. Each node run detects the first and third signatures from its own output, within minutes of a start, and the check before each node start finds the second from the database files (decision 24). A stored marker could outlive a manual fix. The one thing that persists is the record of a confirmed rebuild, or of an install, that has not finished (FR6), because by then the old chain is deleted or partly replaced.
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
14. **2026-10-16.** Mithril staging lives inside the chain folder, on the chain's own volume, for every Mithril install into that location, so an install is a rename and never a copy between volumes. A rebuild checks the free space on that volume before it deletes anything, counting the space the wipe frees, and refuses with `INSUFFICIENT_SPACE` when the download would not fit. Decision 6 stands: the old chain is still deleted before the download starts. The database size comes from `total_db_size_uncompressed` in `mithril-client snapshot show`, and the check requires that size plus 10 percent, the margin `mithril-client` adds to a partial download, to fit in the free space plus what the wipe and a staging leftover will free. The staging entry, `mithril-staging`, is never chain data: the replaceability check, the wipe, the has-chain check and the storage location picker all accept or ignore it, and leftovers are deleted before each download, and at startup unless they belong to another network.
15. **2026-10-17.** Retry on the Mithril error view after a partial sync failed during its install step (`INSTALL_FAILED` with `rebuild: false`) offers the rebuild behind the confirmation. The watchdog waits for another Mithril sync there, and a plain Retry usually finds nothing to download. Nothing is deleted before the confirmation, and confirming sends `start_mithril` with `wipe_chain: true`.

Plan decisions that apply decisions 13 to 15, 2026-10-17:

16. Decision 13's strict wait applies to every install that fails part-way: a partial sync's and a first bootstrap's as well as a rebuild's. The Mithril error view then offers Retry and no action that starts a node. Retry repeats a rebuild, and after a partial sync or a first bootstrap it leads to the confirmation of decision 15.
17. A chain storage location picker reached from a failed or refused rebuild leads into the rebuild confirmation, which names the location. The location change and the rebuild are sent together once the user confirms, so nothing is deleted in the new location, and no node starts there, before that confirmation.
18. `INSUFFICIENT_SPACE` and `CHAIN_DIR_NOT_REPLACEABLE` offer Retry and Choose another location.

Plan decisions of 2026-10-17:

19. The failure of a partial sync's install part-way is still known after an app restart. The partial sync records its install, as kind `install`, before it changes the database, and deletes the `clean` marker before anything else in the database changes. The next start reports the install as not finished and starts no node, and a Mithril sync chosen there rebuilds.
20. An install renames within the chain's volume and never copies there. A rename that fails without crossing volumes is tried again with growing pauses for up to 60 seconds, and then the install fails with `INSTALL_FAILED`. Only a rename to another volume falls back to a copy, as when the chain's `immutable` folder is a link to another drive. A failed install deletes what is left of its download, because only a rebuild, which downloads everything again, follows it.
21. Without a chain, the Mithril error view offers Choose how to sync and never Skip to blockchain sync, including after a refused first bootstrap. The bootstrap decision names the sync from genesis; Skip does not.
22. A record beside a chain directory that has a `clean` marker means that the install or rebuild it describes left the database whole. Every rebuild and install deletes `clean` before it changes the database, and an install moves it in last. At startup such a record is removed, and the node starts as usual.
23. The record names its kind, `rebuild` or `install`, so the renderer never says that old data was removed after an install that removed none, such as an interrupted first bootstrap. A record keeps the kind it was first written with until it is removed, so a rebuild that continues an unfinished install stays an install.

Owner decisions of 2026-10-21:

24. **2026-10-21.** Before every node start, the watchdog decides whether the node would stop on the second signature. When that is certain, it does not start the node and reports `chain_damage` with kind `ledger_point_missing` and the slot, so the offer screen appears. The node holds the error back until it is asked to stop, so detection from output comes only when the user quits, and the next start waits again. A refusal keeps a node from starting and leads to a full download, so the check fails open: anything it cannot read or does not recognize starts the node as before. The detection from output stays as a second path.
25. **2026-10-21.** When the node writes nothing on either output stream for 5 minutes while it starts, before `chainDbReady`, the loading screen says so and offers to restart the node or keep waiting.

Plan decisions that apply decisions 24 and 25, 2026-10-21:

26. While the watchdog holds the node after the check, the offer screen shows even after an earlier Not now, and offers no Not now: no node runs behind it, so dismissing it would leave a loading screen that waits for nothing. Diagnostics stays open to the user, and a node restart from there runs the check again.
27. When the silence prompt and the Mithril prompt on the loading screen both apply, the silence prompt shows. The Mithril prompt returns once the silence prompt is dismissed or cleared.

Owner decisions of 2026-10-21 on the rebuild's download and confirmation:

28. **2026-10-21.** The Mithril progress view says when a download is waiting for the network. mithril-client 0.13.9 reports no error and no progress while the network is unavailable, and continues the download by itself when the network returns. mithril-client is therefore left to resume on its own, with no timeout from the watchdog. The watchdog infers the wait from the download's progress counts, which stop growing, and a check that the aggregator cannot be reached.
29. **2026-10-21.** The rebuild confirmation stays as it is.

## Requirements

### Functional Requirements

Detection (watchdog):

- [ ] FR1. Emit `chain_damage` with kind `node_rejects_canonical_block` when one rejected slot has been reported by 3 distinct `remoteAddress` values in `ChainSync.Client.Exception` lines that contain `BadInputsUTxO`. Carry that slot.
- [ ] FR2. Emit kind `ledger_point_missing` on the first node line that contains both `Critical invariant violation` and `that was in immutable db is gone before we could open ledgerdb`. Carry `blockPointSlot`.
- [ ] FR2a. Before every node start, check whether the node would stop on the second signature, mirroring ouroboros-consensus 4.2.1.0, the version cardano-node 11.1.3 is built with. `watchdog/src/ledger_point.rs` names the modules and functions it mirrors. With a `clean` marker and no `--validate-db`, the node validates only its most recent chunks: from the highest-numbered `.chunk` file down, the first chunk whose first block matches the CRC32 in its secondary index holds the tip. Ledger snapshots are tried highest slot first. One whose chunk is past the tip's is skipped, one in the tip's chunk is left to the node, and one the LSM backend would not load is skipped: a file in place of the directory, a missing or invalid `meta`, another backend, a missing or unreadable `utxoSize`, or a `state` whose CRC32 differs from the checksum in `meta`. For the first snapshot that loads, the slot is looked up as the node looks it up, in the primary index of its chunk at relative slot `slot % chunk_slots + 1`. When that slot is empty and the secondary index agrees, the watchdog reports `chain_damage` with kind `ledger_point_missing`, the slot and `node_held: true`, writes the WARN line of FR4, and does not start the node. The chunk size is ten times `protocolConsts.k` of the Byron genesis that the node configuration names: 21600 slots on mainnet and preprod, 4320 on preview. The check compares slots, not hashes, because the block's hash is inside the encoded ledger state. It starts the node, and logs why, when anything is unreadable, unexpected or ambiguous: no `clean` marker, `--validate-db`, another LedgerDB backend, `.epoch` files, names that may read as numbers in another form, a tip chunk whose first block does not match its index, index files that are missing, malformed or disagree, a missing chunk file or LSM table snapshot, or a slot that an epoch boundary block may share. While the node is held, the watchdog waits as after `node_unrecoverable`: `restart_node`, `start_node` and `restart_wallet` run the check again, `start_mithril` runs the sync, settings changes are applied, and `stop` stops. A Mithril view that an install left open is closed with `mithril_status` `completed` before the report.
- [ ] FR3. Emit kind `wallet_chain_discontinuity` on the first wallet line that contains `restoreBlocks: given chain isn't a valid continuation`. Carry the first slot after the gap and the wallet's last good slot.
- [ ] FR4. Emit each kind at most once per node process (per wallet process for FR3), and write a `WARN` line to `watchdog.log` with the kind and slots.

Rebuild (watchdog):

- [ ] FR5. With `wipe_chain`, probe the aggregator before deleting anything. A probe failure leaves the chain untouched and reports `PROBE_FAILED` as today. Then check the free space on the volume that holds the chain folder, counting the space the wipe and a staging leftover free. If the download would not fit, refuse with `INSUFFICIENT_SPACE` before anything is deleted (decision 14). The size comes from `total_db_size_uncompressed` in `mithril-client snapshot show`, and the download needs that size plus 10 percent, the margin `mithril-client` adds to a partial download. When the size or the free space cannot be read, the install goes ahead. For a chain directory that does not exist yet, the free space is read on the nearest folder that does. A rebuild always installs a full bootstrap and skips the check that the local chain is already current, so a chain that is level with the certified tip is still replaced.
- [ ] FR6. If the download fails or is cancelled after the wipe, do not start the node. Report `chain_status { has_chain: false }` and wait as on first run, so the renderer shows the bootstrap decision. Until `set_chain_path` changes the chain directory, a `start_mithril` sent in that wait is also a rebuild, so it deletes what a failed wipe left and installs a full bootstrap. A failure to wipe the old chain reports `WIPE_FAILED` and ends in the same wait. An install that fails part-way, a rebuild's or any other's, keeps the strict wait (decision 16): no node start, no `chain_status`, and `start_node` is ignored, until a Mithril sync installs a database or a rebuild deletes the chain and installs nothing. Settings changes sent in that wait are applied, so a location chosen there is the one the next sync installs into. A sync that changes nothing ends with the node started again on the chain it had, unless the chain directory then holds no chain, as after a rebuild sent with a new location and refused; the watchdog then reports `chain_status { has_chain: false }` and waits. The rebuild intent persists across an app restart: the watchdog records it before the wipe and clears it when the install succeeds, and while it is recorded it reports `chain_status { has_chain: false, rebuild_pending: true }` at start, or `install_pending: true` for a record of kind `install`. Every `mithril_error` from a rebuild carries `rebuild: true`. The record is `chain-rebuild.json` in the state directory, naming the chain directory and its kind (`{"chain_path": ..., "kind": "rebuild"}`, or `"install"`). A record without a kind, as earlier versions wrote it, reads as `rebuild`. It is written to a temporary file in the same directory and renamed into place, before the wipe. A rebuild writes kind `rebuild`, unless it continues a record of kind `install`, which keeps its kind (decision 23). If it cannot be written, the rebuild is refused with `REBUILD_MARKER_FAILED` before anything is deleted. Every other install, a first bootstrap and a partial sync included, writes the same record, of kind `install`, before it changes the database (decision 19), so an install that fails or is cut short counts as no chain at the next start. If that write fails, the install is refused with `MARKER_FAILED`, the code of the cutover record written at the same point. A record matches the chain directory when the two name the same path as the platform compares paths: trailing separators are ignored, and so is case on Windows. A record for another chain directory is ignored and kept. A record that cannot be read or parsed counts only while the configured chain directory has no `clean` marker, which a rebuild deletes first and an install moves in last; otherwise it is logged and deleted. At startup, a record of the configured chain directory beside a `clean` marker is logged and removed as well, and the node starts as usual (decision 22). Choosing to sync from genesis in the wait that follows a rebuild or an unfinished install (`start_node`) also removes the record, because the user has then chosen the replay. A delete of the record that fails, after an install, at startup or on that choice, is tried again after 100, 250, 500 and 1000 ms before it is logged. While the record is present, no node starts and every Mithril sync of that directory is a rebuild.
- [ ] FR7. Resolve the chain directory the way full bootstrap does (`full_install_target`). Refuse with `CHAIN_DIR_NOT_REPLACEABLE` when it holds anything other than chain database files (`check_replaceable`). Otherwise, delete the real directory's contents and keep any link or junction that points to it. A user-picked folder outside the state directory rebuilds in place. The `mithril-staging` entry inside the chain folder does not count as other content: `check_replaceable` accepts it, the wipe leaves it, and the has-chain check reads only `protocolMagicId` and `immutable`. At startup a staging leftover is deleted unless the chain directory, or the database staged in it, belongs to another network, whose Daedalus may be downloading into it. An install renames each staged entry. A rename that fails without crossing volumes is tried again with pauses that start at 100 ms and double up to 5 s, for up to 60 s in all (`rename_retry_secs` in the Mithril configuration). The install then fails with `INSTALL_FAILED` without copying (decision 20). Only a rename to another volume falls back to a copy, which copies `clean` last. An install that fails deletes what is left of its download. A partial sync deletes `clean` before it changes anything in the database and moves the new one in last (decision 19). Paths given to `mithril-client` and `snapshot-converter` lose the verbatim prefix: `read_link` on a Windows junction returns `\\?\D:\...` or `\\?\UNC\server\share\...`, which they receive as `D:\...` or `\\server\share\...`. A path whose plain form would be 248 UTF-16 units long or longer, too long for the classic Windows limit for a directory, is passed in its verbatim form.
- [ ] FR7a. The storage location picker accepts a chain folder that holds only database entries plus a `mithril-staging` leftover, instead of refusing it as `chain-subdirectory-not-database`.
- [ ] FR8. Pass the `code` of `mithril_error` through to the renderer as `lastErrorCode`, and its `rebuild` flag as `lastErrorRebuild`. `mithril_status` carries `rebuild` on every phase: true when the pipeline is a rebuild, asked for with `wipe_chain` or forced by an unfinished one.

Offer screen (main and renderer):

- [ ] FR9. `chainDamage` in the polled state: set by `chain_damage`, cleared by `node_started` and by `mithril_status` with phase `preparing`. `lastChainDamage` is set together with it but is not cleared by either, only by `mithril_status` `completed` with `rebuild: true`, whether the rebuild was asked for or forced. `rebuildPending` in the polled state is set from `chain_status.rebuild_pending` and by an `INSTALL_FAILED` with `rebuild: true`, and cleared by `node_started` and by a completed pipeline. `installPending` is set from `chain_status.install_pending` and by an `INSTALL_FAILED` with `rebuild: false`, and cleared the same way. An `INSTALL_FAILED` with `rebuild: true` while `installPending` is set leaves `installPending` set and does not set `rebuildPending`, as the record keeps its kind. `installRetryRequired` is set by `INSTALL_FAILED` and cleared by `node_started` and by `chain_status`. `lastErrorRebuild` is cleared by the next `start_mithril`, by `start_node` and by a completed pipeline. `start_node` also clears the Mithril phase, so the error view does not stay up while the node syncs. While `installRetryRequired` is set, `start_node` changes nothing, because the watchdog ignores it. `nodeHeld` is set from the `node_held` of `chain_damage` and cleared by `node_started` and by `mithril_status` with phase `preparing`. A held report also ends a requested node restart, because the old node has stopped and the watchdog holds the new one.
- [ ] FR10. A loading phase `chain-damaged` that takes precedence over everything except `stopping` and a running Mithril pipeline, `ready` and `error` included. The screen shows on every page, including staking, settings, voting, governance and profile, which otherwise render without waiting for the node.
- [ ] FR11. Choice view: a symptom sentence for the kind, what the rebuild does, that wallets and funds are safe and no recovery phrase is needed, and a detail line with the kind and slot for support. Actions: Rebuild blockchain data, Not now.
- [ ] FR12. Confirm view: what will be deleted and downloaded, and that the app is unusable until it finishes. Actions: Rebuild, Back. Rebuild sends `{ cmd: 'start_mithril', wipe_chain: true }`, and the existing Mithril progress view takes over. Rebuild also marks the chain storage location as confirmed, because the rebuild targets the location in use. A rebuild that fails and leaves no chain therefore shows the Mithril error view with the reason and then the bootstrap decision, without the chain storage location picker. The same holds after an app restart in the middle of a rebuild (`rebuildPending`) or of an install (`installPending`): the picker is skipped and the bootstrap decision shows. When the chain is gone after a failed rebuild (`rebuildPending` without `installRetryRequired`), the error view says the old blockchain data was removed and Retry repeats the rebuild. After an unfinished install (`installPending`), the error view and the bootstrap decision say instead that a download of the blockchain data did not finish installing, never that old data was removed. Whenever the watchdog reports no chain (`hasChain === false`), and with either flag, the error view's secondary action is Choose how to sync, which opens the bootstrap decision, which labels the sync from genesis explicitly. The error view offers no Skip to blockchain sync there, because the node would start a sync from genesis the user did not choose (decision 21). That includes a refused or failed first bootstrap and a rebuild refused in a newly chosen location. After an install failed part-way (`installRetryRequired`), the view offers Retry, Choose another location where FR14 allows it, and no action that starts a node. After a rebuild's install failed, the removal note says only that Retry downloads the data again.
- [ ] FR13. Not now hides the screen until the app restarts. While the watchdog holds the node (`nodeHeld`), the screen shows even after Not now and offers no Not now (decision 26).
- [ ] FR14. The Mithril error view shows a specific message for `PROBE_FAILED`, `INVALID_CHAIN_PATH`, `CHAIN_DIR_NOT_REPLACEABLE`, `CHAIN_DIR_OTHER_NETWORK`, `WIPE_FAILED`, `INSUFFICIENT_SPACE`, `REBUILD_MARKER_FAILED` and `MARKER_FAILED`, and its Retry and Skip buttons are translated (they are hard-coded English at `MithrilSyncContainer.tsx:50-70`). While the view says the old data was removed or that an install did not finish, and while Retry is the only action after a failed install, the messages for `PROBE_FAILED`, `INSUFFICIENT_SPACE`, `CHAIN_DIR_NOT_REPLACEABLE`, `CHAIN_DIR_OTHER_NETWORK` and `REBUILD_MARKER_FAILED` leave out their statement that nothing was changed. After a failed or refused rebuild (`lastErrorRebuild`), Retry repeats the rebuild with `wipe_chain` for every code except `INVALID_CHAIN_PATH` and `CHAIN_DIR_OTHER_NETWORK`, a failed install included. For `INVALID_CHAIN_PATH` and `CHAIN_DIR_OTHER_NETWORK`, where the location is the cause, Retry opens the chain storage location picker. `INSUFFICIENT_SPACE` and `CHAIN_DIR_NOT_REPLACEABLE` offer Choose another location beside Retry (decision 18). A picker reached from a failed or refused rebuild says, when the folder picked holds blockchain data, that the rebuild deletes that data and downloads it again, instead of saying that the data will be reused. A location confirmed on that picker is not applied at once. The rebuild confirmation follows and names it, Rebuild sends the location and `start_mithril` with `wipe_chain` in one request, and Back drops the location (decision 17). After a failed sync that was not a rebuild, the picker applies the location at once, as on first run. When an install that was not a rebuild failed (`INSTALL_FAILED` with `rebuild: false`), a partial sync's as in decision 15 or a first bootstrap's, Retry shows the confirmation of FR12 and confirming rebuilds. When any other error did not come from a rebuild, Retry runs Mithril as before. After any `INSTALL_FAILED`, the view offers no Skip (decision 16).
- [ ] FR15. Diagnostics shows the last detection of the session (kind, slot, time) from `lastChainDamage`, so a node start does not erase it, and has a Rebuild blockchain data action behind the same confirmation, whether anything was detected. While damage is detected the partial Mithril Sync action is disabled, with a hint that points to Rebuild blockchain data, because a partial sync moves the symptom without repairing the hole. The copied Diagnostics text includes the Blockchain data row.
- [ ] FR16. All new copy exists in en-US and ja-JP.

Startup silence (watchdog, main and renderer):

- [ ] FR22. While the node process runs and before `chainDbReady`, when no byte has arrived on either output stream, a partial line included, for `startup_silence_secs` (300 by default), the watchdog logs `node: no output for N s while starting` and emits `node_silent { silent_secs }` once. Output that arrives after it emits `node_output_resumed`, and a new silence can be reported after that. A timer tick that arrives more than six tick intervals after the previous one, 30 s at the default, by the monotonic or the wall clock, is taken for the machine sleeping and restarts the silence count instead of prompting.
- [ ] FR23. The loading screen shows a prompt while a silence is reported, with Restart Cardano node, which sends `restart_node`, and Keep waiting, which hides it until the next silence. The silence clears on `node_output_resumed`, `node_started`, `node_exited`, `node_shutdown_ms`, `node_startup_status`, `node_socket_ready`, `node_unrecoverable` and `stopped`. The prompt takes the Mithril prompt's place while both apply (decision 27).

Download waiting for the network (watchdog, main and renderer):

- [ ] FR24. While mithril-client downloads, until its file count reaches its total, the watchdog notes when the highest `files_downloaded`, the highest `bytes_downloaded` or the step number in its progress last grew. mithril-client reports immutable files as a file count and the ancillary archive as a byte count, one kind per line, so each count is tracked on its own. After `download_stall_secs` (60 by default in the Mithril configuration) without growth, the watchdog checks whether the aggregator in `aggregator_url` can be reached: it resolves the host and connects to the port, with one 5 s timeout over both. It checks again every `network_recheck_secs` (20 by default) while the download stays stalled. When the aggregator cannot be reached it logs that once and emits `mithril_waiting_for_network { waiting: true }`. Growth, a check that reaches the aggregator, and the end of the download emit `waiting: false`. mithril-client is never stopped or timed out for a stall.
- [ ] FR25. While `mithrilWaitingForNetwork` is set, the Mithril progress view says that downloading will continue when the network returns, and the progress bar stays as it is. Main sets the flag from `mithril_waiting_for_network` and clears it on any Mithril phase other than `downloading` and `verifying`, on `mithril_error`, on `node_started` and on `stopped`.

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
- `watchdog/src/ledger_point.rs` (new): the check before a node start (FR2a).
- `watchdog/src/supervisor.rs`: every node start in `run` passes the check of FR2a first, and the wait for `chainDbReady` in `run_node_wallet` watches for silence (FR22). Node output calls the detector from `pipe_to_log`, next to `try_parse_startup_status`. In `run_node_wallet`, the wallet's output streams pass `pipe_to_log` a wallet-only detector. After a failed or cancelled post-wipe download, `run` reports no chain and waits for the user's choice in `await_chain_choice` instead of starting the node again.
- `watchdog/src/protocol.rs`: `Event::ChainDamage`.
- `watchdog/src/mithril.rs`: the download watches its progress for a stall and checks the aggregator (FR24). The wipe moves after the probe and goes through `full_install_target` and `check_replaceable`. A distinct `PipelineResult` marks a failure after the wipe. The free-space check on the chain's volume runs before the wipe, staging moves inside the chain folder, and the record of a confirmed rebuild or an install, with its kind, is written before the chain changes and cleared when the install succeeds. The install renames without copying on one volume.
- `watchdog/src/config.rs`: `rename_retry_secs` in the Mithril configuration, 60 by default. `read_node_storage` reads the chunk size and the LedgerDB backend from the node configuration and its Byron genesis, and `startup_silence_secs` in the node configuration is 300 by default. `download_stall_secs` and `network_recheck_secs` in the Mithril configuration are 60 and 20 by default.
- `watchdog/src/bin/mock_node.rs` and the wallet mock: an environment variable naming a file of extra lines to print, so integration tests can inject field lines.
- `watchdog/tests/integration.rs`, `watchdog/tests/chain_storage.rs`.
- `source/common/types/watchdog.types.ts`: the `ChainDamage` and `ChainDamageKind` types, `chainDamage`, `lastChainDamage`, `lastErrorCode`, `lastErrorRebuild`, `rebuildPending`, `installPending` and `installRetryRequired` on `WatchdogState`, and `'chain-damaged'` on `LoadingPhase`.
- `source/main/WatchdogManager.ts` and `source/main/ipc/index.ts`: event handling, state defaults, and the fallback state.
- `source/main/ipc/chainStorageChannel.ts` and `source/common/ipc/api.ts`: the chain storage confirm request carries an optional `rebuild`.
- `source/renderer/app/stores/BackendStore.ts`: the observables, the phase, `rebuildChain()`, `retryMithril()`, `showBootstrapDecision()` and `dismissChainDamage()`.
- `source/renderer/app/containers/Root.tsx`: the page early return skips the `chain-damaged` phase.
- `source/renderer/app/containers/loading/LoadingPage.tsx`, a new `containers/loading/ChainDamageContainer.tsx`, and new `components/loading/chain-damage/ChainDamageView.tsx` and `RebuildConfirmView.tsx` with `.scss` and `ChainDamage.messages.ts`.
- `source/renderer/app/containers/loading/SyncingConnectingPage.tsx` and a new `components/loading/syncing-connecting/NodeSilencePrompt.tsx`: the silence prompt.
- `source/renderer/app/containers/loading/MithrilSyncContainer.tsx` and `components/loading/mithril/MithrilProgressView.tsx`: the line for a download waiting for the network.
- `source/renderer/app/containers/loading/MithrilSyncContainer.tsx`, `components/loading/mithril/MithrilErrorView.tsx` and `MithrilDecisionView.tsx`: error codes, translated buttons, and the notes on removed data and on an unfinished install.
- `source/renderer/app/containers/loading/ChainStorageContainer.tsx` and `components/chain-storage/ChainStorageLocationPicker.tsx`: the notice on data found before a rebuild.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` and `containers/status/DaedalusDiagnosticsDialog.tsx`.
- `source/renderer/app/i18n/locales/{en-US,ja-JP,defaultMessages}.json`.
- `storybook/stories/loading/chain-damage/ChainDamageView.stories.tsx`.
- Phase 7: `watchdog/src/chain_scan.rs` and `watchdog/src/chain_repair.rs` (new), and a `ChainRepairView` in the renderer.

### Data / IPC / API Changes

New watchdog event, one JSON line as today:

```json
{"event":"chain_damage","kind":"node_rejects_canonical_block","slot":200361988,"last_good_slot":null,"node_held":false}
{"event":"chain_damage","kind":"ledger_point_missing","slot":200102375,"last_good_slot":null,"node_held":false}
{"event":"chain_damage","kind":"wallet_chain_discontinuity","slot":200102408,"last_good_slot":200080892,"node_held":false}
{"event":"chain_damage","kind":"ledger_point_missing","slot":136771192,"last_good_slot":null,"node_held":true}
```

The last line is the check before a node start (FR2a). The startup silence (FR22) adds two events:

```json
{"event":"node_silent","silent_secs":300}
{"event":"node_output_resumed"}
```

A download waiting for the network (FR24) adds one:

```json
{"event":"mithril_waiting_for_network","waiting":true}
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
lastChainDamage: ChainDamage | null;
lastErrorCode: string | null;
lastErrorRebuild: boolean;
rebuildPending: boolean;
installPending: boolean;
installRetryRequired: boolean;
nodeHeld: boolean;
nodeSilentSecs: number | null;
nodeSilenceEpisode: number;
mithrilWaitingForNetwork: boolean;
```

`nodeSilenceEpisode` counts the silences reported, so a prompt dismissed for one silence shows again for the next.

`mithril_error` gains `rebuild: bool`, which is always present, `chain_status` gains `rebuild_pending: bool` and `install_pending: bool`, which are always sent, and `INSUFFICIENT_SPACE` and `REBUILD_MARKER_FAILED` are new error codes. `MARKER_FAILED` keeps its meaning: the cutover record after the download could not be written. `mithril_status` gains `rebuild: bool` on every phase:

```json
{"event":"mithril_error","code":"REBUILD_MARKER_FAILED","message":"...","rebuild":true}
{"event":"chain_status","has_chain":false,"rebuild_pending":true,"install_pending":false}
{"event":"chain_status","has_chain":false,"rebuild_pending":false,"install_pending":true}
{"event":"mithril_status","phase":"completed","rebuild":true}
```

The chain storage confirm request gains an optional `rebuild`. With it, main sends `start_mithril` with `wipe_chain` straight after `set_chain_path`, so the watchdog holds the rebuild while the node stops for the location change and runs it in place of the restart.

A Mithril sync that finds the local chain current ends with `mithril_status` `completed` after `node_started`, so the renderer leaves its Mithril view.

There is no new IPC channel. The renderer reads the state through the existing 2-second poll, and the rebuild uses `MITHRIL_COMMAND_CHANNEL`, which already forwards `wipe_chain` verbatim.

Phase 7 adds `chain_repair_status { phase, chunks, progress }` with phases `scanning`, `downloading`, `swapping`, `awaiting_ledger_confirmation`, `syncing`, `completed` and `failed`, a `chainRepair` field on the polled state, and a `confirm_repair_ledger { install }` command sent over `MITHRIL_COMMAND_CHANNEL`.

### UI / Store / Process Changes

`BackendStore.loadingPhase` gains one check directly after `isStopping`:

```ts
if (
  this.chainDamage &&
  (this.nodeHeld || !this._chainDamageDismissed) &&
  !this._mithrilActive
) {
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
| `CHAIN_DIR_NOT_REPLACEABLE` | The blockchain data folder contains other files, so Daedalus did not delete it. Move those files elsewhere and retry, or choose another location. |
| `CHAIN_DIR_NOT_REPLACEABLE`, old data removed, install unfinished, or Retry only | The blockchain data folder contains other files, so Daedalus cannot install the blockchain data there. Move those files elsewhere and retry, or choose another location. |
| `INVALID_CHAIN_PATH` | Daedalus cannot rebuild the blockchain data in this folder. Choose a different blockchain data location and try again. |
| `WIPE_FAILED` | Daedalus could not delete the blockchain data files. Close any other programs that might be using the blockchain folder, then retry. |
| `CHAIN_DIR_OTHER_NETWORK` | The blockchain data folder holds data for a different Cardano network, so Daedalus did not change it. Choose a different blockchain data location and try again. |
| `INSUFFICIENT_SPACE` | There is not enough free space on the drive that holds the blockchain data. Your blockchain data has not been changed. Free some space on that drive and retry, or choose another location. |
| `REBUILD_MARKER_FAILED` | Daedalus could not record the rebuild in its data folder, so it changed nothing. Check that the data folder is writable and retry. |
| `MARKER_FAILED` | Daedalus could not write to its data folder before installing the downloaded blockchain data, so it did not install it. Check that the data folder is writable and retry. |
| `PROBE_FAILED`, `INSUFFICIENT_SPACE`, `CHAIN_DIR_OTHER_NETWORK` and `REBUILD_MARKER_FAILED`, old data removed, install unfinished, or Retry only | The same message without its sentence that the blockchain data has not been changed, or that nothing was changed. |
| Choose another location | Choose another location |
| Confirm body, picked location | Daedalus will keep the blockchain data in {location}, delete any blockchain data already there, and download a complete verified copy. This is a large download, and Daedalus cannot be used until it finishes. Your wallets stay on this computer and finish syncing afterward. |
| Old data removed, error view | The old blockchain data was removed. Retry downloads it again with Mithril, or choose how to sync. |
| Old data removed, error view, Retry only | The old blockchain data was removed. Retry downloads it again with Mithril. |
| Old data removed, decision view | The old blockchain data was removed, so the blockchain needs to be synced again. |
| Install unfinished, error view | A download of the blockchain data did not finish installing. Retry downloads it again with Mithril, or choose how to sync. |
| Install unfinished, error view, Retry only | A download of the blockchain data did not finish installing. Retry downloads it again with Mithril. |
| Install unfinished, decision view | A download of the blockchain data did not finish installing, so the blockchain needs to be synced again. |
| Location picker, data found before a rebuild | Existing blockchain data found. The rebuild deletes the blockchain data in this folder and downloads it again. |
| Secondary action after removal | Choose how to sync |
| Diagnostics, Mithril Sync blocked | Unavailable while damaged blockchain data is detected. Use Rebuild blockchain data instead. |
| Diagnostics | Blockchain data: No damage detected / Damage detected at slot {slot} ({time}). Action: Rebuild blockchain data |
| Silence prompt title | Cardano node is not reporting progress |
| Silence prompt body | Cardano node has reported nothing for {minutes} minutes while starting. It may still be working. If it is stuck, restarting it can help. |
| Silence prompt actions | Restart Cardano node / Keep waiting |
| Mithril download waiting for the network | Downloading will continue when the network returns. |

New `defaultMessage` values carry the `!!!` prefix until reviewed, per `.agent/skills/i18n-messaging/SKILL.md:33`.

### Targeted Repair (phases 6 and 7)

1. On `chain_damage`, stop the wallet and the node.
2. Scan the ImmutableDB as in [research/03](research/03-boundary-scan.md). A break before chunk M means the chunks from N (the last non-empty chunk before M) to M-1 are incomplete. In every field case and in #3416 itself, that is chunk N alone.
3. For each break, run `mithril-client cardano-db download --start N --end M-1` into a staging directory inside the chain folder (decision 14), without ancillary files. The client verifies the files against the certificate.
4. When every download has verified, move each chunk's `.chunk`, `.primary` and `.secondary` into `immutable/`.
5. If the kind needs a fresh ledger, emit `awaiting_ledger_confirmation` and wait for the user (decision 11). On Install, run the existing partial sync (`run_pipeline` forced, no wipe), which installs a fresh certified ledger and replaces `ledger/` and `lsm/` (`watchdog/src/mithril.rs:510-523`). Filling the hole is enough for the wallet symptom. The two node symptoms also need a ledger that never replayed across the hole. On Not now, go to step 6 and do not ask again this session.
6. Restart the node and wallet. If a signature reappears in the same session, show the offer screen and do not repair again.

Phase 6 decides whether step 5 is needed for each kind, whether the wallet recovers without a resync, and how long the scan takes natively on Windows. The answers go in `research/04-targeted-repair-spike.md`.

## Implementation Strategy

1. **Detection.** Event, parsers and detector, wallet output parsing, mock-driven integration tests, and the check of the ledger snapshot's block before each node start.
2. **Safe rebuild.** Probe before delete, no silent genesis sync, custom chain locations, and error codes passed to the renderer.
3. **Offer screen.** State plumbing, store phase and actions, choice and confirm views, translated and code-aware error view, copy and stories, and the startup silence prompt.
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

Watchdog unit tests (`ledger_point.rs`, on databases built in temporary directories): a snapshot whose block is missing is refused and one whose block is present is not; a snapshot past the tip is skipped and one in the tip's chunk is left to the node; a suffixed name is read, and names with one number are tried in name order; no snapshots, no `clean` marker and an empty ImmutableDB start the node; snapshots the LSM backend would not load are skipped; an empty last chunk file, or a chunk file under a name the node does not open, moves the tip down; and every undecided case starts the node, among them a missing chunk file or LSM table snapshot, malformed or disagreeing index files, an epoch boundary slot and a `meta` number the check does not read. `config.rs` tests read the chunk size and the backend. The silence watch is tested with given clocks: one report per silence, a new one after output, and a long gap between ticks taken for sleep. The download watch is tested with given times: each count tracked on its own, a check after the stall and again at the recheck interval, a wait reported only when the aggregator cannot be reached, and growth, a reachable aggregator or the end of the download ending it. The aggregator's host and port are read from its URL.

Watchdog integration tests:

- A database whose ledger snapshot's block is missing emits `chain_damage` with `node_held: true` and starts no node, `restart_node` checks it again, and a rebuild sent then installs and starts the node. A database whose snapshot block is present starts the node.
- A node that leaves a line unterminated emits `node_silent` once, and `node_output_resumed` when the line completes.
- A download that stalls while the aggregator cannot be reached emits `mithril_waiting_for_network` with `waiting: true`, and `waiting: false` when its progress grows again, and the install finishes. A download that stalls while the aggregator can be reached emits nothing.

- The mock node prints signature 2 and the watchdog emits `chain_damage`. The mock node prints signature 1 from three peers and the event follows. The wallet mock prints signature 3 and the event follows.
- `wipe_chain` with a failing probe leaves the chain untouched.
- `wipe_chain` on a volume without room for the download fails with `INSUFFICIENT_SPACE` and deletes nothing.
- `wipe_chain` with a failing download emits `chain_status { has_chain: false }` and starts no node.
- `wipe_chain` on a link or junction replaces the target's contents and keeps the link.
- `wipe_chain` on a user-picked folder outside the state directory succeeds.
- `wipe_chain` on a folder holding a foreign file fails with `CHAIN_DIR_NOT_REPLACEABLE` and deletes nothing.
- An install that fails part-way, a rebuild's or a first bootstrap's, waits for a Mithril install: `start_node` starts nothing and reports nothing, and a rebuild sent next installs and starts the node. A rebuild's record is kept until then.
- `wipe_chain` whose record cannot be written fails with `REBUILD_MARKER_FAILED` and deletes and downloads nothing.
- A first bootstrap cut short in its install counts as no chain, with the install pending, at the next start. A rebuild chosen after that deletes what the install left, and the record keeps the kind `install` when the rebuild's download fails.
- A partial sync whose install fails part-way has deleted `clean`, keeps its record of kind `install` and removes its download. After a restart it reports `install_pending` and starts no node, and the Mithril sync chosen there rebuilds and removes the record. A partial sync that installs leaves no record.
- A record beside a chain marked clean is removed at startup, and the node starts on that chain unchanged.
- `mithril_status` carries `rebuild: true` for a requested rebuild and for one forced by the wait after a failed rebuild or by a pending record, and `false` for a first bootstrap, a partial sync and a sync that was not needed.
- A rebuild sent with a new location starts no node there before it and installs there. Refused, it leaves the watchdog waiting for a choice instead of starting a node in the new location.
- Startup keeps a staging entry that belongs to another network and deletes this network's.
- Existing tests keep passing, notably `wipe_chain_full_bootstrap` (`watchdog/tests/integration.rs:1182`).

Watchdog unit tests also cover the record: its path comparison, its atomic write, its kind and a record without one, a corrupt record with and without `clean`, a record removed beside a clean chain at startup, and a record delete tried again. They cover a rename tried again until it succeeds, one that gives up once its time is spent, no copy after a failed rename on one volume, and a partial install that deletes `clean` before it changes anything. They also cover the plain forms of verbatim drive and network paths and the 248-unit limit, and the free-space check for a folder that does not exist yet and for a rebuild that fits only through the space its wipe frees.

Jest:

- `WatchdogManager.spec.ts`: `chain_damage` sets `chainDamage` and `lastChainDamage`; `node_started` and `preparing` clear only the first; `completed` with `rebuild: true` clears the second, also after the bootstrap decision is accepted following a failed rebuild; `mithril_error` keeps `code` and `rebuild`; `chain_status` keeps `rebuild_pending` and `install_pending`; `start_node` clears the Mithril phase; `INSTALL_FAILED` sets `installRetryRequired`, and `rebuildPending` when it comes from a rebuild, or `installPending` when it does not or when `installPending` was already set, and `start_node` then changes nothing.
- `chainStorageChannel.spec.ts`: a location confirmed with `rebuild` is followed by the rebuild command.
- `BackendStore.spec.ts`: the phase precedence over `ready` and `error`; dismissal; `rebuildChain()` sends `wipe_chain: true` and logs only plain values; each branch of `retryMithril()`; the bootstrap decision after a rebuild that removed the data, and after a restart with `rebuildPending` or `installPending`; whether the location picker leads to a rebuild.
- `Root.spec.tsx`: the damaged data screen replaces each of the five pages that otherwise render without a connected node.
- `chainStorageValidate.realfs.spec.ts`: the picker accepts database entries plus a staging leftover.
- `ChainStorageLocationPicker.spec.tsx`: data found before a rebuild is named as deleted, not reused.
- `MithrilSyncContainer.spec.tsx` and `BackendStore.spec.ts`: Retry after a failed install that was not a rebuild asks for the confirmation, Back sends nothing, and confirming sends `wipe_chain`. After a failed install, the view offers no action that starts a node. Without a chain the view offers Choose how to sync and no Skip, after a refused or failed first bootstrap and after a rebuild refused in a new location as well. An unfinished install shows its own note on the error and decision views, and `MithrilErrorView.spec.tsx` checks that no message says nothing changed while Retry is the only action. Choose another location appears for `INSUFFICIENT_SPACE` and `CHAIN_DIR_NOT_REPLACEABLE`, a location picked after a rebuild error is held for the confirmation that names it and sent with the rebuild, Back drops it, and after a sync that was not a rebuild the location is applied at once.
- A component spec for the choice-to-confirm flow, and the choice without Not now while the node is held.
- `WatchdogManager.spec.ts`: `node_held` sets `nodeHeld`, which `node_started` and `preparing` clear and which ends a requested restart; `node_silent` sets `nodeSilentSecs` and counts the episode, and the events of FR23 clear it.
- `BackendStore.spec.ts`: the damaged data screen while the node is held, after Not now as well; the silence prompt for each new silence, Keep waiting, and the restart it sends.
- `NodeSilencePrompt.spec.tsx`: the copy and both actions.
- `WatchdogManager.spec.ts`, `BackendStore.spec.ts` and `MithrilProgressView.spec.tsx`: `mithrilWaitingForNetwork` and the line on the progress view.

Storybook: one story per kind, the confirm view, the error view for each code in FR14, the decision and error views after removed data and after an unfinished install, and the location picker with data found before a rebuild, in en-US and ja-JP.

Nix checks, never the yarn scripts alone: `watchdog-clippy`, `watchdog-test`, `jest`, `compile`, `lint`, `stylelint`, `i18n`, `storybook`, `treefmt`, `prettier-version-parity`.

Manual, on preprod (Linux under WSL2 and Windows), reproducing a hole with 11.4.0 code:

1. List preprod snapshots and pick the one whose `immutable_file_number` is two below the newest. Download it with `--include-ancillary` into a scratch directory. Delete its `ledger` directory. Its highest chunk N is the unfinalized one from the ancillary archive.
2. With Daedalus Pre-Prod 11.4.0 closed, swap that directory in as the chain directory. Start the app and run Mithril Sync from Diagnostics while the node replays. `watchdog.log` shows `Mithril forced by caller: local=N, certified=C` with C greater than N.
3. Quit cleanly. `mithril-client cardano-db verify --start N --end N` reports three tampered files, and the boundary scan reports a break before chunk N+1.
4. Install the build under test. Restore a wallet from its recovery phrase. When its restore reaches chunk N, signature 3 appears and the offer screen follows. Rebuild, and record whether the wallet finishes without a resync. The confirmation copy depends on this result.
5. On a fresh copy of the damaged chain, delete `ledger` and `lsm` so the node replays from genesis across the hole. Record whether a later block that spends an output created in the hole arrives and triggers signature 1. On preprod's low transaction volume it may not, and the field-line tests cover detection.
6. Signature 2 is reproduced with a preprod chain whose ledger snapshot at slot 136771192 lies in the missing tail of chunk 6331, which holds slots 136749698 to 136749780 only. Without the check of FR2a, the node replays from that snapshot and writes nothing until it is stopped. With it, Daedalus shows the offer screen without starting the node, and Rebuild from there installs a database and starts the node. The check refuses that chain. It starts the node on a copy of it whose chunk 6331 is whole, and on a healthy preprod chain.
7. On Windows, repeat steps 4 and 5 with the chain in a user-picked folder reached through a junction.
8. On Windows, rebuild through a junction, and in a folder on a second drive picked with Choose another location. `watchdog.log` shows the download staged on that drive, `mithril-client` and `snapshot-converter` given plain drive paths without `\\?\`, and an install that renames rather than copies.
9. Give Daedalus Mainnet and Daedalus Pre-Prod the same storage folder. Start a first Mithril download in one and launch the other while it downloads. `watchdog.log` of the second shows the first one's staging kept, not deleted. The startup cleanup recognizes another network's staging by the `protocolMagicId` of the staged database, so the result depends on when `mithril-client` writes that file.

## Rollout / Migration / Rollback

- No data migration. The only persisted state is the record of a confirmed rebuild, or of an install, that has not finished (FR6). A record written before kinds existed reads as a rebuild's. The detection lives in memory, and the rebuild uses the existing Mithril bootstrap.
- Phases 1 to 7 ship in one release (decision 10).
- The CHANGELOG entry describes the behavior a user sees: the damaged-data screen, the targeted repair and its ledger confirmation, the rebuild, and the Diagnostics action.
- Rollback is a revert. The new event is additive. The rebuild changes affect `wipe_chain`, which nothing sends in 11.5.0, and the staging location of every Mithril install (decision 14), which a revert restores.
- An affected install sees the screen on the first start after upgrading that produces a signature. Installs with holes but no symptoms see nothing until one appears, or until the background scan from decision 1 ships.

## Open Questions

None. Decisions 10 to 15 settled the questions raised while planning, decisions 16 to 18 apply decisions 13 to 15, decisions 19 to 23 apply FR6, FR12 and decision 13, decisions 26 and 27 apply decisions 24 and 25, and decisions 28 and 29 record the owner's choices on the rebuild's download and confirmation.

**Status:** In Progress
**Date:** 2026-10-21
**Author:** Adam Dean
