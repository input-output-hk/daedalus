# Exact CBOR And Era Coverage Evidence

Status: task-004 completed; implementation review approved in iteration 4.
The PRD remains normative for product behavior. The fixture manifest is the
machine-readable input for tasks 302 and 303; it is not a production parser.

## Conclusions

- Exact transaction identity must come from original CBOR spans. Neither tested
  Cardano SDK version is an acceptable strict framing, duplicate-key, unknown-field,
  or full-consumption boundary.
- The checked-in Conway fixture provides ledger-decoded evidence for exact body,
  witness, `isValid`, ordinary-output, collateral-return, and null auxiliary-data
  spans. A pinned historical ledger golden provides a non-null auxiliary-data span
  and demonstrates that retired body key 6 must fail the Conway field policy.
- Daedalus retains `@cardano-sdk/core@0.41.4` as a non-authoritative semantic helper.
  Candidate `0.47.0` closes no demonstrated task-303 representation gap and accepts
  more strict-reject probes, so `package.json` and `yarn.lock` remain unchanged.
- Conway is conditionally ready only at the fixture/inventory layer. No backend,
  production parser, or product support is implemented by this task.
- Dijkstra is `unsupported/readiness-blocked`. Pinned package evidence materially
  changes the envelope, body, native scripts, redeemers, Plutus language, and
  protocol parameters while cardano-wallet retains unsupported/TODO paths.

## Immutable Sources

| Evidence                             | Package tag                       | Commit                                     | Blob                                       | SHA-256                                                            |
| ------------------------------------ | --------------------------------- | ------------------------------------------ | ------------------------------------------ | ------------------------------------------------------------------ |
| Conway CDDL                          | `cardano-ledger-conway-1.22.1.0`  | `226b002d5b5e83e24355f8a28ab214f3259eabda` | `d19cc469c510ccc27ac64735e5102a2611244566` | `ab2325fea52b97ab7792ccf9fefcc6dafb543e0bc795e9b761d3f2989b223271` |
| Conway regression source             | `cardano-ledger-conway-1.22.1.0`  | `226b002d5b5e83e24355f8a28ab214f3259eabda` | `c97a9aab069d86a6581449b4ed155e3270e3fddc` | `42755d7dd3a0776de01c64cf5b31283585e4d118fb14ffc4fc775a90d9e385f9` |
| Historical ledger transaction golden | `cardano-ledger-conway-1.22.1.0`  | `226b002d5b5e83e24355f8a28ab214f3259eabda` | `07fa93f4aec98354d0a942ded4727be0e09e3f0c` | `4d36863697dee9beb3f57d74af3e8cf7ff96641d0458b03790eb4861d917bdb1` |
| Dijkstra CDDL                        | `cardano-ledger-dijkstra-0.2.0.1` | `94e9618c91a16ec08db477632a158b630722089b` | `1b41a0f312f033fd9967812192c945950144dbde` | `0b7062eab1011c80dae7e0849f1414ec79183405473ffccb96c34d545bea2ee1` |

The tags exactly match the sibling cardano-wallet constraints in `cabal.project`.
This replaces planning-time evidence from a moving ledger branch. In particular,
the pinned Dijkstra CDDL adds body keys `23`, `25`, and `26` and protocol-update
keys `34` through `37`; it does not contain the broader provisional key sets seen
in later development sources. Downstream work must use the pinned inventory until
an explicit dependency and task-graph update changes that baseline.

## Exact Span Fixtures

| Fixture                               | Bytes | Body      | Witness     | `isValid`   | Auxiliary           | Outputs                  | Collateral return | Body Blake2b-256                                                   |
| ------------------------------------- | ----: | --------- | ----------- | ----------- | ------------------- | ------------------------ | ----------------- | ------------------------------------------------------------------ |
| `conway-regression-collateral`        |   346 | `[1,281)` | `[281,344)` | `[344,345)` | `[345,346)` null    | `[122,159)`, `[159,196)` | `[197,234)`       | `b327eaa52a6cce81b367951a19a7fb72807419461606ddf6c7e09ab7c7b3d327` |
| `alonzo-ledger-golden-auxiliary-data` |   865 | `[1,686)` | `[686,820)` | `[820,821)` | `[821,865)` tag 259 | `[80,223)`               | absent            | `ad8033bc3f0da247fb074361ad195cafd5b8bda105319325450f19d06723200a` |
| `conway-babbage-map-outputs`          |   352 | `[1,287)` | `[287,350)` | `[350,351)` | `[351,352)` null    | `[122,161)`, `[161,200)` | `[201,240)`       | `c0627e2dc7eb49f3272cb8e362431273b8e7a9b31c39348e1ba2fc3a41e2bcb4` |
| `conway-untagged-body-sets`           |   340 | `[1,275)` | `[275,338)` | `[338,339)` | `[339,340)` null    | `[116,153)`, `[153,190)` | `[191,228)`       | `3c88918d4ae72f0c5f2321a553b260b1b77a62949ea43763188148f0a20d61d0` |

The first fixture is the exact 346-byte value assembled from lines 49-59 of the
pinned Conway regression, where `decodeFullAnnotatorFromHexText` is required to
accept it as a Conway transaction. It includes tag-258 normal and collateral input
sets, two outputs with adjacent independently asserted boundaries, body key 16
collateral return, witness fields 3-5, `isValid=false`, and null auxiliary data.

The second fixture is the complete pinned 865-byte ledger golden. It supplies exact
tag-259 auxiliary-data and `isValid=true` span evidence, but its historical body key
6 is intentionally rejected by the Conway field policy. It is not mislabeled as a
Conway-positive transaction.

The Babbage-output fixture deterministically replaces the three source array outputs
with CDDL-equivalent `{0: address, 1: value}` maps, without changing an address or
value byte. The untagged fixture removes only the first two tag-258 wrappers, at body
keys 0 and 13. `cardano-cli 11.0.0.0` revision
`01a89dad991e5a19990150b4e1de348a1481a37a` accepts both complete values as Conway
transactions through `debug transaction view`; its independently returned tx IDs
equal the true Blake2b-256 body hashes in the table.

Expected offsets were fixed from `cbor-diag-cli 0.1.8 --to annotated` output before
the Jest assertions were finalized. `span-annotations.json` records the independent
ranges, exact command, and SHA-256 of each annotated output. The test-local scanner
only checks those fixed values and cannot rewrite the manifest. Body hashes are
independently computed with `blakejs` and the separate `blake2b` package in true
32-byte mode. Blake2b-512 truncation is explicitly not accepted as Blake2b-256
evidence.

## Encoding And Field Matrix

`source/common/cardano/fixtures/exact-cbor/manifest.json` freezes the detailed
incoming encoding-family policy. The significant boundaries are:

- full Conway root consumption with the four envelope components;
- definite and source-admitted indefinite forms without normalization;
- source-admitted integer widths while preserving exact bytes;
- arbitrary source-permitted map order;
- rejection of repeated encoded or decoded scalar map keys;
- explicit deferral of complex semantic uniqueness to task 303;
- tag 258 only at declared set locations, with tag presence/order preserved;
- tag 24 containing exactly one complete embedded item at declared locations;
- only CDDL-declared Plutus-data, rational, constructor, and auxiliary tags;
- preservation of Alonzo/Babbage outputs, both Conway redeemer forms, and the three
  auxiliary-data forms when the complete body remains valid for Conway.

`source-inventory.json` is generated directly from the two pinned CDDL files by
`scripts/extract-cardano-cddl-inventory.cjs`. It preserves every top-level definition
block, including multiline generic headers such as `constr<a0>`, derives certificate,
governance-action, and native-script discriminants from their source unions, and
structures every numeric body, witness, Babbage-output, auxiliary-data,
sub-transaction, and protocol-parameter field with type, required/optional state,
cardinality, and source comment. Regeneration against fresh tag checkouts must compare
byte-for-byte with the committed artifact.

The source-derived Conway inventory contains body keys
`0,1,2,3,4,5,7,8,9,11,13,14,15,16,17,18,19,20,21,22`, witness keys `0`-`7`, all
17 certificate tags, redeemer tags `0`-`5`, governance-action tags `0`-`6`, Plutus
V1-V3, both output forms, both set forms, and both redeemer forms. Reserved body
keys `6`, `10`, and `12` reject. Task 303 owns exhaustive union alternatives,
semantic uniqueness, and commitment checks; task 800 owns systematic mutation,
differential, property, and fuzz breadth.

The focused suite additionally freezes one targeted vector for each policy family:
definite/indefinite containers, invalid nested indefinite strings, non-minimal values
and lengths, map order, exact and byte-distinct semantic duplicates, tagged/untagged
and empty non-empty sets, complete and trailing tag-24 embedded CBOR, tag 24 in valid
and invalid output locations, valid/invalid rational tag 30 including a wrong output
location, both redeemer forms, all three auxiliary-data forms, and an unknown tag. The
strict oracle also validates witness-map shape and redeemers, `isValid`, auxiliary
data, output field/tag locations and embedded-item consumption, collateral return,
known body/witness keys, complete root consumption, and body input-set tags and
cardinality. This is targeted contract evidence, not task-800 mutation breadth.

## SDK Comparison

Both versions ran through `scripts/exact-cbor-sdk-probe.cjs` with the same ordered
manifest cases and normalized result shape.

| Case                                     | Strict                       | SDK 0.41.4        | SDK 0.47.0        |
| ---------------------------------------- | ---------------------------- | ----------------- | ----------------- |
| pinned Conway fixture                    | accept                       | accepts/preserves | accepts/preserves |
| historical key-6 span fixture            | reject                       | rejects           | accepts/preserves |
| trailing root byte                       | reject                       | accepts/preserves | accepts/preserves |
| duplicate fee via non-minimal scalar key | reject                       | accepts/preserves | accepts/preserves |
| unknown body key                         | reject                       | rejects           | accepts/preserves |
| wrong input-set tag                      | reject                       | rejects           | rejects           |
| indefinite root envelope                 | accept by frozen wire policy | rejects           | rejects           |
| Babbage map outputs                      | accept                       | accepts/preserves | accepts/preserves |
| untagged body sets                       | accept                       | accepts/preserves | accepts/preserves |

Installed identity is `0.41.4`, git head
`0d9fa5f83b6e9cf34412eaf94cc1e4541f3b0159`, with the exact Yarn lock tarball,
SHA-1, and SRI in the manifest. Candidate identity is `0.47.0`, git head
`8fb86af308d2045d71b0c18d78abaf0336902983`; its downloaded tarball reproduced
SHA-1 `795716681420d08e939567bf635df06fd0c8da3b` and the committed SRI. The isolated
npm lock is committed as `sdk-0.47.0-package-lock.json`; its SHA-256 is
`4856d1faeb85ff6e6cb90df2a100003bb0017e1f175f7ebd77498f0a4365e9d4`.
The setup script starts from an absent destination, runs `npm ci --ignore-scripts`,
downloads the exact candidate tarball, verifies SHA-1 and SRI, and verifies installed
version/git head. The probe independently checks the committed lock hash, measures
whether CBOR decoding consumed exactly one root item, and records only body fields
whose values are actually defined.

The candidate accepts four strict-reject cases versus two for the installed SDK.
Parser strictness alone was never an upgrade criterion, but the candidate also
demonstrates no required Conway semantic field absent from `0.41.4`. The no-migration
gate therefore selects `retain-0.41.4-as-non-authoritative-helper`. Strict framing,
unknown-field handling, tags, and duplicates remain task-302/task-303 work.

## Era Status

| Era      | Wire fixture                  | SDK representation           | Backend planned      | Backend implemented | Production parser | Product support | Task-004 conclusion           |
| -------- | ----------------------------- | ---------------------------- | -------------------- | ------------------- | ----------------- | --------------- | ----------------------------- |
| Conway   | recognized                    | required fixture represented | yes                  | no                  | no                | no              | conditional readiness         |
| Dijkstra | not recognized for production | informational only           | no complete contract | no                  | no                | no              | unsupported/readiness-blocked |

The pinned Dijkstra CDDL has a normal three-item envelope and a four-item mempool
compatibility form requiring `true`; key 14 changes from required signers to guards;
keys 23, 25, and 26 add sub-transactions, direct deposits, and account-balance
intervals; sub-transaction key 24 binds required top-level guards; auxiliary-data key
5 carries Plutus V4 scripts; native script tag 6 and redeemer tag 6 add guards; only
map redeemers remain; the protocol minor becomes `uint .size 4`; and protocol-update
keys 34-37 add reference-script limits and cost controls. The sibling wallet still
contains unsupported Dijkstra era API comments, transaction generator/signing pending
tests, and native-script/mint conversion TODO failures.

Dijkstra promotion requires an explicit tracker/PRD update plus pinned final sources,
backend tasks 200/201/206/209, parser task 302, complete semantic task 303, context
task 304, capability task 301, differential/fuzz task 800, and security/dependency
review tasks 805/807. Partial CDDL or local-state-query type presence is insufficient.

## Protocol Context Boundary

The manifest assigns fees, maximum transaction/value sizes, min-UTxO, collateral,
execution prices/limits, cost models/language views, reference-script fees/limits,
deposits, governance parameters/state, protocol version, and pinned Dijkstra additions
to tasks 303, 304, and 800 as appropriate. Runtime authority is the full era-specific
parameter value returned by cardano-wallet node LSQ at exact captured chain point `W`
under the task-003 `W/G/P` protocol. Task-004 makes no live-network policy claim and
does not fetch arbitrary current parameters.

## Reproduction

```bash
git clone --depth 1 --branch cardano-ledger-conway-1.22.1.0 https://github.com/IntersectMBO/cardano-ledger.git /tmp/cardano-ledger-conway
git clone --depth 1 --branch cardano-ledger-dijkstra-0.2.0.1 https://github.com/IntersectMBO/cardano-ledger.git /tmp/cardano-ledger-dijkstra
node scripts/extract-cardano-cddl-inventory.cjs --conway-cddl /tmp/cardano-ledger-conway/eras/conway/impl/cddl/data/conway.cddl --dijkstra-cddl /tmp/cardano-ledger-dijkstra/eras/dijkstra/impl/cddl/data/dijkstra.cddl --output /tmp/task-004-source-inventory.json
cmp source/common/cardano/fixtures/exact-cbor/source-inventory.json /tmp/task-004-source-inventory.json
node scripts/setup-exact-cbor-sdk-candidate.cjs --destination /tmp/task-004-sdk-candidate --lock source/common/cardano/fixtures/exact-cbor/sdk-0.47.0-package-lock.json --manifest source/common/cardano/fixtures/exact-cbor/manifest.json
yarn test:jest source/common/cardano/exactCborValidation.spec.ts --runInBand --coverage=false
node scripts/exact-cbor-sdk-probe.cjs --sdk-root node_modules/@cardano-sdk/core --label worktree-node-modules --manifest source/common/cardano/fixtures/exact-cbor/manifest.json --output source/common/cardano/fixtures/exact-cbor/sdk-0.41.4-results.json
node scripts/exact-cbor-sdk-probe.cjs --sdk-root /tmp/task-004-sdk-candidate/node_modules/@cardano-sdk/core --label isolated-candidate --manifest source/common/cardano/fixtures/exact-cbor/manifest.json --output source/common/cardano/fixtures/exact-cbor/sdk-0.47.0-results.json
node -e "const fs=require('fs');for(const [source,target] of process.argv.slice(1).map(x=>x.split('='))){const f=require('./'+source);fs.writeFileSync(target,JSON.stringify({type:'Tx ConwayEra',description:'task-004 fixture',cborHex:f.cborHex}))}" source/common/cardano/fixtures/exact-cbor/conway-babbage-outputs.json=/tmp/task-004-babbage-envelope.json source/common/cardano/fixtures/exact-cbor/conway-untagged-sets.json=/tmp/task-004-untagged-envelope.json
cardano-cli debug transaction view --tx-file /tmp/task-004-babbage-envelope.json
cardano-cli debug transaction view --tx-file /tmp/task-004-untagged-envelope.json
```

Evidence was produced with Node `v24.16.0`, npm `12.0.2`, Yarn `1.22.21`, and
`cbor-diag-cli 0.1.8`. No secrets, wallet data, live transaction, or network-specific
protocol parameter was used.
