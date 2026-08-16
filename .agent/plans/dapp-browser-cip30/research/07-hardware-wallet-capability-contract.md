# Hardware Wallet Capability Contract

Status: approved static phase-0 evidence for completed task-006. This artifact does not
certify a physical device, firmware, Ledger Cardano app, display, transport,
cancellation path, returned hash, or signature. The machine-readable source of
truth is `source/common/hardware/fixtures/capability-matrix/manifest.json`.

## Decision

- Recommend exact `@cardano-foundation/ledgerjs-hw-app-cardano@8.0.0` to
  task-600. Task-006 does not change `package.json` or `yarn.lock`.
- Keep all hardware connector methods product-disabled. Static evidence can
  establish library representability only; task-607 owns physical promotion.
- Treat every vendor transaction API as semantic reconstruction. A downstream
  adapter may call a device only after reproducing the complete body and proving
  its Blake2b-256 equals the immutable broker body hash. Otherwise it rejects
  before device interaction.
- Never pass Trezor Connect's `coseSignature` or `coseKey` to a dApp. Daedalus
  must validate raw identity/public-key/signature material and construct the
  frozen task-002 COSE locally.

Connect 9.7.2 reports `version: 1` in its returned header object but encodes the
vendor `coseSignature` unprotected map with only `hashed: false`. That byte result
cannot satisfy task-002. The frozen matrix therefore makes vendor COSE
non-pass-through regardless of model/firmware and requires local reconstruction.

## Immutable Artifacts

| Artifact | Identity | Static conclusion |
|---|---|---|
| Installed Ledger | `7.1.4`, SHA-1 `e3e484edf950a871d3d3c87750077565162eee9f` | App compatibility is capped at major 7; Conway, message signing, Babbage outputs, one voter/one vote, treasury and donation are represented; proposal procedures and combined certificate tags 10-13 are absent. |
| Ledger candidate | `8.0.0`, SHA-1 `7f6b1dcfcc5b397156507b0c82d25d7595687a68` | Adds app-major-8 routing, unrestricted transaction mode, multiple voters/votes and preserves current public/deep imports; proposal procedures remain absent. |
| Trezor Connect | `9.7.2`, SHA-1 `bb6e06f1a28bac41266ef936fea38f653122afa3` | Represents ordinary/Plutus/Babbage transaction fields and message signing, but no voting/proposal/treasury/donation transaction fields. Vendor COSE is not the Daedalus wire result. |

The Ledger candidate tarball SHA-256 is
`76a2dc5d058d920f4b31562097880ddcbbcb59cf598b69b5a076ce85dd6c0706`.
The committed isolated dependency lock SHA-256 is
`20494ce8d821e7bb2d84a6a7516601d9063898415ae06aebd95a313566c2a75a`.
The lock includes exact resolved versions and integrities and is required before
the candidate probe is trusted. The probe derives version, resolved URL and SRI
from that lock, checks the installed npm lock against it, and verifies the SRI
against npm's content-addressed cached tarball before loading candidate code.
The SHA-1 and tarball SHA-256 are then computed from those verified bytes. For
root Yarn artifacts, the SHA-1 is mechanically parsed from the locked resolved
URL. Every result also records a deterministic SHA-256 of every file in the
installed package tree.

No generated identity claims a repository git head. Neither the root Yarn v1
entry nor the candidate npm v3 entry mechanically identifies a registry git
head, and package metadata alone is not independent provenance. A future probe
may add informational `registryGitHead` only if it can derive and verify it from
registry evidence; it must not be treated as generated package identity.
For root Yarn artifacts, the probe also requires Yarn's installed cache metadata
to match the locked version, resolved URL, and SRI before loading package code;
the installed package-tree digest then binds the code actually inspected.

The Ledger 8 candidate loads its public entry, every Daedalus deep import, and
the app-v7/app-v8 validator paths in isolation. The current full TypeScript
program is expected to fail with exactly three TS2339 diagnostics for removed
`utils.hex_to_buf` uses; these are the bounded task-600 migration and task-006
does not patch the production consumers. Independently, actual main and renderer
webpack Node-API compilations pass with an in-memory candidate alias, cache off,
watch off, and isolated temporary output directories. Production `node_modules`
is never modified. Physical execution remains `not_run`.

Trezor runtime identity is not summarized as one package version or a selected
file list. The capability probe recursively walks every installed dependency
reachable from the Connect root using actual Node resolution. The committed,
sorted graph records every node, edge, resolved installation path, version, and
deterministic package-tree SHA-256. Every node also records every incoming Yarn
selector and the matching lock resolved URL, URL SHA-1, and SRI. The SRI is the
committed lock integrity when present; legacy Yarn-v1 entries without an
`integrity` line use the resolved URL's SHA-1 expressed in SRI form and identify
that weaker source explicitly. Generation fails for a resolved non-optional
node if its exact selector, version, resolved URL, SHA-1, or SRI identity cannot
be established. The complete lock identity participates in the graph digest.
It proves that Daedalus
root resolution selects `@trezor/transport@1.5.4` while Connect-owned resolution
selects the distinct nested `@trezor/transport@1.6.2`. The focused test executes
the graph generator again and requires byte-identical output.

The artifact manifest freezes both `runtimeGraphSha256` and
`configIdentitySha256`. The latter hashes the Connect config, AbstractMethod
default firmware ranges, model/coin constants, Cardano runtime schemas, and both
operation implementations. These identities are available for later task-607
evidence binding without claiming physical certification in task-006.

Model operation gates are generated from loaded Ledger compatibility functions
and loaded Trezor `supportedFirmware` plus `DEFAULT_FIRMWARE_RANGE`; omitted
T3W1 message support remains `unresolved`, not inferred from a generic default.
Request/response shape and nested alternatives come from loaded Trezor runtime
schemas and Ledger declarations, parsers, constants, output/certificate/message
implementations, and request-compatibility code. The focused Jest test compares
manifest sections to those generated values rather than restating gate tables.
Where the matrix intentionally chooses a stricter certification cardinality
than a parser permits, Jest asserts that the frozen bound does not exceed the
extracted parser bound instead of presenting it as the vendor maximum.

## Exact-Body Boundary

Task-004 admits representation choices that semantic vendor APIs can normalize:
Alonzo arrays versus Babbage maps, tagged versus untagged sets, arbitrary
source-permitted map order, admitted non-minimal widths, definite/indefinite
containers, auxiliary-data hash commitments, and nested ordering/cardinality.

The default for both vendors is `reject_pre_device`. A downstream adapter may
promote only a specific representation after an executable reconstruction proves
the complete vendor body hash equals the original task-004 body hash. Every
promoted transaction still requires task-607 evidence that the device-returned
hash equals that same immutable hash. Type presence is never sufficient.

Dijkstra remains unsupported. Ledger proposal procedures and Trezor governance
voting/proposals/treasury/donation remain pre-device failures for these artifacts.

## Orthogonal Evidence

Every capability row independently records:

- library representability;
- deterministic probe result;
- emulator evidence;
- physical certification;
- adapter implementation;
- product enablement.

Task-006 sets only static representability/probe evidence. Emulator and physical
evidence remain `not_run`, adapter state remains `not_implemented`, and product
enablement remains `disabled`. A failed or unresolved earlier dimension cannot
be overridden by a later positive dimension.

Tasks 600-606 consume the matrix revision and cases but cannot certify or enable
rows. Task-607 alone records reviewed physical outcomes against the actual
production lock, adapter commit, model, app/firmware version, and case IDs. Later
release policy separately owns product enablement.

## CIP-8 And CIP-95

- The task-002 decoded request maximum is 65,536 bytes. A 65,537-byte request is
  `APIError.InvalidRequest` before hardware capability checks.
- A valid request within that product limit that exceeds a smaller vendor/device
  limit is operation-specific `DataSignError.ProofGeneration` (or
  `TxSignError.ProofGeneration` for transaction signing).
- Synthetic tests using the exact task-002 payload/key/identity/signature require
  byte equality with the frozen COSE golden.
- Physical payment, stake, direct-DRep and matching-type-6-DRep cases use unique
  device keys. They prove frozen encoding, original payload, expected identity,
  public-key association, Ed25519 verification, and locally reconstructed COSE,
  not impossible cross-key equality with the synthetic golden.

## Privacy And Physical Evidence

The task-607 schema accepts only closed enums, bounded numbers, booleans, opaque
IDs and fixed-length digests. Committed evidence cannot contain screenshots,
free-form prompts/errors, USB paths, serials, labels, addresses, xpubs, host
paths, raw transactions/messages/signatures/keys, seeds, PINs or passphrases.
Necessary raw evidence stays in access-controlled storage and is referenced by a
digest, access-policy ID and retention class.

## Reproduction

```bash
rm -rf /tmp/opencode/task-006-ledger-8
mkdir -p /tmp/opencode/task-006-ledger-8
cp source/common/hardware/fixtures/capability-matrix/ledger-8.0.0-package-lock.json /tmp/opencode/task-006-ledger-8/package-lock.json
printf '%s\n' '{"name":"task-006-ledger-8","version":"1.0.0","private":true,"dependencies":{"@cardano-foundation/ledgerjs-hw-app-cardano":"8.0.0"}}' > /tmp/opencode/task-006-ledger-8/package.json
npm ci --ignore-scripts --prefix /tmp/opencode/task-006-ledger-8
node scripts/hardware-wallet-capability-probe.cjs --vendor=ledger --root=node_modules/@cardano-foundation/ledgerjs-hw-app-cardano --label=installed-ledger-7.1.4 --lock=yarn.lock --output=/tmp/ledger-7.json
node scripts/hardware-wallet-capability-probe.cjs --vendor=ledger --root=/tmp/opencode/task-006-ledger-8/node_modules/@cardano-foundation/ledgerjs-hw-app-cardano --label=isolated-ledger-8.0.0-candidate --lock=source/common/hardware/fixtures/capability-matrix/ledger-8.0.0-package-lock.json --output=/tmp/ledger-8.json
node scripts/hardware-wallet-capability-probe.cjs --vendor=trezor --root=node_modules/@trezor/connect --label=installed-trezor-connect-9.7.2 --lock=yarn.lock --output=/tmp/trezor.json --runtime-output=/tmp/trezor-runtime.json
node scripts/hardware-wallet-ledger-consumer-probe.cjs /tmp/opencode/task-006-ledger-8/node_modules/@cardano-foundation/ledgerjs-hw-app-cardano /tmp/ledger-8-consumers.json
cp hardware-wallet-tests/capability-matrix/evidence.schema.json /tmp/hardware-evidence.schema.json
node scripts/generate-hardware-wallet-cases.cjs source/common/hardware/fixtures/capability-matrix/manifest.json /tmp/hardware-cases.json /tmp/hardware-evidence.schema.json /tmp/hardware-evidence-examples.json /tmp/hardware-input-recipes.json
yarn test:jest source/common/hardware/hardwareCapabilityMatrix.spec.ts --runInBand --coverage=false
```

To regenerate committed provenance, substitute the corresponding files under
`source/common/hardware/fixtures/capability-matrix/` for the `/tmp` output paths.
The commands intentionally populate npm's content-addressed cache so the probe
can cryptographically verify candidate SRI without a network request during
regeneration. No USB/HID transport is opened by any probe or focused test.

Legacy Yarn-v1 lock blocks do not all contain SHA-512 integrity lines. Because
task-006 may not rewrite production dependency files, their resolved URL SHA-1
is retained and normalized as `sha1-...` SRI rather than guessing SHA-512 or
silently omitting integrity. A future dependency-lock refresh can remove this
weaker fallback by committing registry SRI for every reachable block.
