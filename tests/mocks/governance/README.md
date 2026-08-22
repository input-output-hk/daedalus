# Governance test fixtures

`anchor-cip119-sample.json` is a **synthetic** CIP-119 anchor body with an abbreviated `@context`. It is not a copy of
any real DRep's metadata.

`anchor-cip119-sample.hash` is its Blake2b-256 digest, generated from the committed bytes with
`blake2b(32).update(bytes).digest('hex')`. Regenerate it whenever the JSON changes, and always after prettier has
formatted the JSON.

`anchor-malformed.txt` is bytes that fetch cleanly but do not parse.

The real CIP-119 vectors are `https://sipo.tokyo/drep/SIPO.jsonld` (mainnet) and the Cardano Academy preprod
`.jsonld`. The real on-chain `(url, hash)` pair used for cache-key and hash-mismatch tests is at
`.agent/plans/governance/drep-discovery/research/drep-state-preprod-epoch295-sample.json:2852-2855`. No offline copy of
either body exists in this repo.

Cached anchor bodies are public on-chain-referenced directory data keyed by a content hash. No DRep id is stored with
them and none reaches a logger.
