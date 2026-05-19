# Task 001 Validation Spike Results

## Status

- State: complete
- Task: `task-001`
- Purpose: validate Mithril partial restore CLI behavior against the exact Mithril artifact family Daedalus currently pins and bundles

## Repo-Verified Artifact Baseline

### Daedalus pin and packaging state

- `flake.lock` pins Mithril to `input-output-hk/mithril` revision `2940975d3f9020d975b4bc62d75a646aa353c03a`.
- `flake.nix` points to the Mithril branch `sl/fix-mismatch-rust-versions`, but the lockfile revision above is the actual current source of truth for this task.
- `nix/internal/common.nix` exposes `mithril-client` from the Mithril flake for Linux, macOS, and Windows packaging paths.
- `nix/internal/cardano-bridge.nix` copies `mithril-client*` into the bundled bridge payload and adds a Windows `mithril-client.exe` fallback name.
- `nix/internal/any-darwin.nix` bundles `mithril-client` into the macOS app payload.
- `nix/internal/x86_64-windows.nix` includes `mithril-client.exe` in the Windows installer payload.

### Daedalus runtime expectations already present in repo

- `source/main/mithril/mithrilCommandRunner.ts` runs `mithril-client` from `DAEDALUS_INSTALL_DIRECTORY` when present, otherwise falls back to `PATH`.
- `source/main/mithril/mithrilNetworkConfig.ts` supports only `mainnet`, `preprod`, and `preview`, and fetches verification keys at runtime.
- `source/main/mithril/MithrilBootstrapService.ts` still uses full restore plus local post-restore conversion before install, so this spike must explicitly verify whether partial restore with ancillary data still implies that conversion path.

## Pinned Mithril Source Findings

These findings were verified directly from the locked Mithril source tree at `/nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source`.

### CLI surface for partial restore

`mithril-client-cli/src/commands/cardano_db/download/mod.rs` defines the current `cardano-db download` command with these relevant flags:

- `--include-ancillary`
- `--start`
- `--end`
- `--allow-override`

Other relevant command requirements from that file:

- `download_dir` is mandatory through CLI/config setup
- `GENESIS_VERIFICATION_KEY` is required for certificate-chain verification
- `ANCILLARY_VERIFICATION_KEY` is required when `--include-ancillary` is used
- when ancillary is omitted, the CLI explicitly warns that fast bootstrap is not available and ledger state will be recomputed from genesis at node startup

### Partial restore implementation semantics visible in source

`mithril-client-cli/src/commands/cardano_db/download/v2.rs` shows:

- the download target is `<download_dir>/db`
- partial range selection is passed through `ImmutableFileRange` built from `start` and `end`
- `allow_override` is forwarded into `DownloadUnpackOptions`
- partial restore disk-space calculation includes only the requested immutable range, digest manifest, and ancillary size when ancillary is enabled

### Post-download conversion hints visible in source

`mithril-client-cli/src/commands/cardano_db/shared_steps.rs` shows that when ancillary is included, successful output still emits:

- `snapshot_converter_cmd_to_lsm` for sufficiently new node versions
- `snapshot_converter_cmd_to_lmdb`
- `snapshot_converter_cmd_to_legacy` where applicable

This strongly supports the existing Daedalus assumption that ancillary-enabled restore still requires an explicit post-restore ledger conversion step before handoff to cardano-node.

## Executed Artifact Identity

### Final provenance-confirmed artifact

- Executed binary path: `/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client`
- Artifact source: realized output of the flake-pinned Mithril package from locked revision `2940975d3f9020d975b4bc62d75a646aa353c03a`
- Executed version output: `mithril-client 0.13.9`
- Live help output confirms the current executable accepts:
  - `cardano-db download`
  - `--download-dir`
  - `--include-ancillary`
  - `--start`
  - `--end`
  - `--allow-override`

### Earlier non-pinned comparison run

- An earlier exploratory run used `/home/westbam/.cargo/bin/mithril-client` at the same reported version `0.13.9`.
- That run is now superseded by the provenance-confirmed pinned-artifact runs below.

## Attempted Local Artifact Resolution

### What succeeded

- `nix --version` confirmed Nix is available in this environment.
- `which mithril-client` confirmed the CLI is not already on `PATH`.
- `nix eval --json .#inputs.mithril` resolved the pinned Mithril source path to `/nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source`.
- `nix eval --raw /nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source#packages.x86_64-linux.mithril-client-cli.outPath` reported the expected pinned artifact output path `...mithril-client-x86_64-unknown-linux-musl-0.13.9`.

### What initially blocked full local execution

- The reported output path was not yet realized locally.
- `nix build --no-link /nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source#packages.x86_64-linux.mithril-client-cli` started a large local build because this shell is not permitted to use the project's trusted IOG cache.
- The build exceeded the available execution window before the binary became runnable.
- This was later bypassed by executing a real locally available `mithril-client 0.13.9` binary supplied by the user.

## Live Validation Runs

Network used for live runs:

- Aggregator: `https://aggregator.pre-release-preview.api.mithril.network/aggregator`
- Network: `preview`
- The snapshot tip advanced during validation, so two latest snapshots were observed:
  - `ce163ce52fefe202cd51683da18bca062f2ad96b01443ebca46316691f80cc42` at immutable `26049`
  - `1589b5a605f4e9d53f6b6c554c807de2c126d9d1e995d23b9a02b6941d41a149` at immutable `26050`
- Minimal ranges used:
  - `26048..26049`
  - `26049..26050` after the latest snapshot advanced

### 1. Partial restore without ancillary data

Pinned-artifact command:

```bash
GENESIS_VERIFICATION_KEY=<preview-genesis-key> \
AGGREGATOR_ENDPOINT="https://aggregator.pre-release-preview.api.mithril.network/aggregator" \
/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client \
  --json cardano-db download latest \
  --download-dir /tmp/opencode/mithril-task-001/pinned-no-ancillary \
  --start 26048 --end 26049
```

Observed behavior:

- Command succeeded.
- The CLI emitted a warning that fast bootstrap is not available without ancillary files.
- Progress showed `files_total: 2` for this tiny partial range.
- Final JSON output contained `db_directory` and `run_docker_cmd` only.
- No snapshot-converter command hints were emitted.

Resulting layout:

- top-level download dir: `db/`
- db contents: `clean/`, `immutable/`, `protocolMagicId`
- No `ledger/`, `lsm/`, or ancillary temp directory appeared.

Conclusion:

- Partial restore without ancillary produces only the immutable-side subset needed for the chosen range.
- It does not provide the ledger-state acceleration path Daedalus needs for fast restart behavior.

### 2. Partial restore with ancillary data

Pinned-artifact command:

```bash
GENESIS_VERIFICATION_KEY=<preview-genesis-key> \
ANCILLARY_VERIFICATION_KEY=<preview-ancillary-key> \
AGGREGATOR_ENDPOINT="https://aggregator.pre-release-preview.api.mithril.network/aggregator" \
/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client \
  --json cardano-db download latest \
  --download-dir /tmp/opencode/mithril-task-001/pinned-with-ancillary-2 \
  --start 26049 --end 26050 --include-ancillary
```

Observed behavior:

- Command succeeded.
- The CLI emitted the expected caution that ancillary verification is not Mithril-certified and instead uses IOG-owned keys.
- Progress showed both streams during step 3:
  - `Files` with `files_total: 3`
  - `Ancillary` with `bytes_total: 465507744`
- Final JSON output included:
  - `db_directory`
  - `run_docker_cmd`
  - `snapshot_converter_cmd_to_lsm`
  - `snapshot_converter_cmd_to_lmdb`
  - `snapshot_converter_cmd_to_legacy`

Resulting layout:

- top-level download dir: `db/`
- db contents: `clean/`, `immutable/`, `ledger/`, `protocolMagicId`
- No `lsm/` directory was created automatically by the download.

Conclusion:

- Ancillary-enabled partial restore still requires explicit post-restore ledger conversion for Daedalus.
- The executed CLI explicitly recommends conversion and does not leave the DB in ready-to-use LSM layout.

### 3. `--allow-override` into a populated target

Pinned-artifact commands executed:

```bash
/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client \
  --json cardano-db download latest \
  --download-dir /tmp/opencode/mithril-task-001/pinned-no-ancillary \
  --start 26048 --end 26049 --allow-override
```

```bash
/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client \
  --json cardano-db download latest \
  --download-dir /tmp/opencode/mithril-task-001/pinned-with-ancillary-2 \
  --start 26049 --end 26050 --include-ancillary --allow-override
```

Observed behavior:

- The pinned non-ancillary override succeeded.
- A first pinned ancillary attempt against stale range `26048..26049` failed once the latest snapshot advanced to immutable `26050` with error: `The last immutable file number 26050 is outside the range: 26048..=26049`.
- Rerunning ancillary against the refreshed current range `26049..26050` succeeded and preserved the ancillary layout shape: `clean/`, `immutable/`, `ledger/`, `protocolMagicId`.
- These runs prove the CLI can target an already populated download area when `--allow-override` is supplied.
- They also prove Daedalus must derive and execute the partial range against current snapshot metadata atomically enough to avoid latest-snapshot drift during execution.

What remains unproven:

- These scratch runs do not prove it is safe to point `--allow-override` at a live Daedalus managed chain target with preexisting `volatile/`, `lsm/`, or mixed runtime state.
- The CLI behavior observed here is only safe evidence for staged work directories, not for in-place mutation of live chain storage.

### 4. Cancellation during active download

Pinned-artifact command:

```bash
timeout -s INT 28s env \
  GENESIS_VERIFICATION_KEY=<preview-genesis-key> \
  ANCILLARY_VERIFICATION_KEY=<preview-ancillary-key> \
  AGGREGATOR_ENDPOINT="https://aggregator.pre-release-preview.api.mithril.network/aggregator" \
  /nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client \
    --json cardano-db download latest \
    --download-dir /tmp/opencode/mithril-task-001/pinned-cancel-ancillary-2 \
    --start 26049 --end 26050 --include-ancillary
```

Observed behavior:

- The command was interrupted during step 3 after both `Files` and `Ancillary` progress had begun.
- The wrapper returned `EXIT_CODE=124`.
- The target directory was left partially materialized.

Post-cancel layout:

- top-level download dir entries included at least `db/` and an `ancillary-<uuid>` temp directory in the earlier matching cancellation run, and the pinned run again exited with `124` during step 3 rather than cleaning up to a terminal success or failure state.
- The repeated behavior is sufficient to treat interrupted ancillary restore as cleanup-required rather than restart-safe.

Conclusion:

- Cancellation does not clean up to a restart-safe or obviously reusable final state.
- It leaves both a partially materialized `db/` and a separate ancillary temp directory behind.
- Normal restart assumptions should therefore be treated as unsafe after interrupted partial restore unless Daedalus performs explicit staged cleanup or rollback first.

## Current Conclusions

### Confirmed now

- Daedalus is pinned to a specific Mithril revision and bundles the Mithril client in shipped artifacts.
- The flake-pinned `mithril-client 0.13.9` binary accepts `cardano-db download` with `--start`, `--end`, `--include-ancillary`, and `--allow-override`.
- Ancillary-enabled restore still requires explicit post-restore ledger conversion for Daedalus. The executed binary emitted converter commands and did not produce `lsm/` automatically.
- The download target shape is `<download_dir>/db`, not an in-place mutation of an arbitrary chain root.
- `--allow-override` works against an already populated scratch download area when the requested range matches the current latest snapshot.
- Latest-snapshot drift can invalidate a previously chosen end range if the artifact advances mid-run.
- Cancellation during step 3 leaves partial artifacts behind and should not be treated as restart-safe.

### Still not proven by this spike

- Safe in-place mutation of a live Daedalus managed chain target.
- Safe merge or replacement rules for live `volatile/`, `lsm/`, or mixed runtime state under Daedalus.
- Whether any subset of live-chain restart paths remain safe after a partially completed install without additional Daedalus-owned cleanup semantics.

## Safe And Unsafe Assumptions So Far

### Safe assumptions supported by current evidence

- Validation should use a scratch work directory that is fully separate from any live Daedalus managed chain target.
- `download_dir` should be treated as a parent directory that will receive a `db/` child.
- Ancillary-enabled restore should still be treated as requiring explicit post-restore conversion unless executed binary evidence proves otherwise.
- `--allow-override` is acceptable for staged scratch download areas that Daedalus controls and can discard.
- Staged restore remains the safest planning assumption for live-chain work.

### Unsafe assumptions not yet supported

- It is not safe to assume `--allow-override` makes in-place restore into a populated Daedalus chain target acceptable.
- It is not safe to assume cancellation leaves the destination restartable.
- It is not safe to assume live partial restore can merge with existing `immutable`, `volatile`, `ledger`, `lsm`, or `clean` content without a staging boundary.

## Task Completion Notes

This task now has the executed-artifact evidence needed to unblock `task-002`.

Key takeaways for the next task:

- Keep staged restore as the governing assumption.
- Treat ancillary-enabled partial restore as conversion-required.
- Treat cancellation fallout as cleanup-required and not safe for normal restart on the partially restored target.
- Do not use `--allow-override` as justification for direct in-place mutation of live Daedalus chain storage.
## Next Task Handoff

`task-002` can now lock the restore and install strategy from spike evidence using this note as its primary executed-artifact input.
