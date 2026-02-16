# Mithril bootstrap client notes

## Sources
- https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node
- https://mithril.network/doc/manual/getting-started/network-configurations

## CLI behavior reminders
- `mithril-client cardano-db snapshot list --json` returns a JSON array of snapshot metadata.
- `mithril-client cardano-db snapshot show <digest> --json` returns a JSON object with details.
- `mithril-client cardano-db snapshot download --include-ancillary <digest> --json` emits JSON progress lines on stdout when `--json` is used.
- After restore, conversion to LMDB flavor uses:
  `mithril-client tools utxo-hd snapshot-converter --db-directory <db> --cardano-node-version <nodeVersion> --utxo-hd-flavor LMDB --commit`.

## Network config values
- Mainnet: `https://aggregator.release-mainnet.api.mithril.network/aggregator`
- Preprod: `https://aggregator.release-preprod.api.mithril.network/aggregator`
- Preview: `https://aggregator.pre-release-preview.api.mithril.network/aggregator`
- Genesis/ancillary verification keys are hosted under the same `mithril-infra/configuration/<network>` path.

## Notes
- `GENESIS_VERIFICATION_KEY` and `ANCILLARY_VERIFICATION_KEY` in Mithril docs are shown as URLs; the CLI accepts URLs and fetches keys when those env vars are set.
