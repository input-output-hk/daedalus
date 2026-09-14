# Asset Metadata Cache: Planning Brief

## The request

Give Daedalus a local asset metadata cache so that native tokens display with the names, tickers and
decimal places their issuers published, instead of as a fingerprint and a raw integer.

Four outcomes are asked for, in this order:

1. **Intelligent asset names.** ASCII names for NFTs, tickers for fungible tokens.
2. **Optimistically apply decimal places when the source can be cryptographically verified**, on
   displayed and on entered amounts, subject to the two safety rules in the PRD's send-path section.
3. **Show a warning or advisory when it cannot.**
4. **Keep it simple.**

## The architecture, decided

**A simple local SQLite asset cache in Daedalus. No Haskell changes to cardano-wallet.** A
wallet-side design carries a Haskell change, a coordinated release and a continuing maintenance
burden on a component this feature does not otherwise touch. That cost is not repaid by the
benefit, so the plan is written against the local cache.

The table carries: policy id, asset id, decoded name and ticker, image,
decimals, a verified boolean, a metadata column carrying website and similar for fungibles and
traits and project information for NFTs, and a last-updated slot or time.

## The enabling fact

The reason this needs no wallet change is that Daedalus can stop calling
`GET /wallets/{id}/assets` entirely.

- Wallet holdings already arrive on `GET /wallets/{id}` as `assets.available` and `assets.total`,
  built at `Cardano/Wallet/Api/Http/Shelley/Server.hs:1154-1160` from `available ^. #tokens` and
  `total ^. #tokens` off the checkpoint balance. That is cheap and incrementally maintained through
  `DeltaWallet` and `deltaUTxO`, with no history scan.
- `containers/wallet/WalletTokensPage.tsx:41` already reads `activeWallet.assets.total` for the
  token list, so holdings do not depend on the assets endpoint today.
- The expensive full-history read and the uncached, un-timed-out inline metadata fetch both exist
  only on the assets endpoint. Dropping the poll removes both from the hot path with no wallet
  change at all.
- Nothing in Daedalus needs the assets list as a list. `stores/AssetsStore.ts:63-70` reduces it to a
  keyed lookup map and `getAsset` at `:72-73` is a point read of that map. The enumeration exists
  purely to populate a client-side cache, which is what this design replaces.

## The verification chain

Verified live against `tokens.cardano.org` for subject
`c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544`.

- The server returns `policy`, the native script as hex, plus per-property signatures and sequence
  numbers alongside `name`, `description`, `ticker`, `url`, `logo` and `decimals`. The batch endpoint
  returns `policy` whether or not it is requested.
- The full local proof is three steps. Strip the two-byte era wrapper from the `policy` field and
  take blake2b-224 over `0x00` concatenated with the inner native script, which must equal the
  subject's policy id. Take blake2b-224 of each `publicKey` that signed the property to get the
  attesting key set, and evaluate the decoded native script against that set, with time-lock nodes
  evaluating to true unconditionally. Then verify the ed25519 signature over the registry's
  attestation payload for that property.
- `verified = true` must mean that whole chain passed. It must not mean that the server returned a
  signature, which would collapse back to trusting the operator.
- Ed25519 verification must be strict. `cardano-crypto.js` accepts a signature whose scalar `S`
  has had the group order added to it, so the verification path uses Node's built-in
  `crypto.verify`, which rejects one and adds nothing to the dependency tree.

## Constraint framing

- **Keep it simple.** A plan that grows a small, maintainable cache back into a framework fails the
  brief. A section that would be one line is one line.
- **The wallet is not touched.** It keeps its `--token-metadata-server` wiring
  (`source/main/index.ts:215-218`). This design simply stops exercising it, so nothing needs to be
  disabled, coordinated or released on the wallet side.
- **In scope, on the tracked request.** `se7en-labs-inc/daedalus#35` asks for images, for "a fallback
  path to on-chain values such as CIP-68 metadata or CIP-25 metadata", and for the transaction
  builder to account for decimals. That issue is the request of record and this brief is written to
  match it. The PRD's phase 7 covers the CIP-25 and CIP-68 path.
- **Out of scope:** building a CIP-25/CIP-68 *indexer*, CIP-88, and any cardano-wallet change.
  Reading those records through a confirmed pointer is in scope; standing up an index is not.
- **The documentation is partly drifted.** `.agent/workflows/ipc.md` shows `ipcRenderer.send` and
  `ipcRenderer.invoke`; the real mechanism is `IpcChannel` deriving three channel names from one
  base. Channel shapes are verified against `source/common/ipc` and `source/main/ipc` rather than
  taken from the workflow document.
- **Crypto dependencies are checked, not assumed.** blake2b-224 and ed25519 are confirmed present in
  the dependency tree before any new package is proposed.
- **Every factual claim carries its evidence inline** as a `path:line`, a command, or a count.

## What the plan produces

A schema with real DDL, and a named IPC channel with its request and response shapes and its on-disk
location per platform. A fetch and verify path with its batching, timeout and offline behavior. The
three user-facing goals, each traced to the code that changes, plus the cold-cache and send-path
behavior. The list of what is deleted from Daedalus, and a costed task graph.
