# Asset Metadata Cache PRD

## Overview

Give Daedalus a local asset metadata cache, so native tokens display with the names, tickers and
decimal places their issuers published, rather than as a fingerprint and a raw integer.

The cache is a SQLite database in the Electron main process, keyed on the registry subject, and read
by the renderer over one IPC channel. It has two sources with different jobs. Direct batch queries
to the Cardano token registry supply tickers, names and decimal places that can be verified locally
against the minting policy. A pointer service supplies the CIP-25 and CIP-68 records that name an
NFT, against a pointer the user's own node confirms. Decimal places are applied to displayed and
entered amounts only when the metadata is cryptographically bound to the token's own minting policy,
which means only when they came from the registry. Everything else is displayed in the raw units the
chain holds.

The work also removes the one-minute poll of `GET /wallets/{id}/assets` that currently populates the
in-memory equivalent of this cache, along with the dead code around it.

## Problem Statement

**Published decimal places are fetched and then not used.** `api.ts:3386-3388` files the registry's
`decimals` under `recommendedDecimals` and takes the value that actually formats amounts from
`localData.decimals`, which is per-user browser storage written only by the asset settings dialog
(`stores/AssetsStore.ts:108-110`, `api/utils/localStorage.ts:318-327`). A token denominated in six
decimal places therefore displays and transacts as a raw integer until a user opens a dialog they
have no reason to open. The correct value was on screen the whole time, under the word
"recommended".

**Names are shown even when they are not names.** `components/assets/Asset.tsx:177-178` reads

```ts
const name = metadata?.name || (assetName && `ASCII: ${hexToString(assetName)}`) || '';
```

`hexToString` is `Buffer.from(valueInHex, 'hex').toString()` (`utils/strings.ts:18-19`), which
decodes as UTF-8 unconditionally. For an asset whose name is a hash rather than text, which is the
common case for NFTs, the row shows the literal string `ASCII: ` followed by replacement characters.

**The list is populated by a poll of an endpoint nothing needs.** `AssetsStore.ts:13` sets a
one-minute interval and `:33` starts it, and `:157-170` fans out one `GET /wallets/{id}/assets`
request per wallet on every tick. That endpoint is the expensive one: it reads the full transaction
history and performs an uncached, un-timed-out inline metadata fetch. Neither is needed. Holdings
already arrive on the wallet object, built at
`Cardano/Wallet/Api/Http/Shelley/Server.hs:1154-1160` from the checkpoint balance, and
`containers/wallet/WalletTokensPage.tsx:41` already reads them.

**The same subject is fetched once per wallet, every minute.** `_refreshAssetsData` iterates
`this.stores.wallets.all` and issues one request per wallet on every tick, and nothing behind it
deduplicates by subject: cardano-wallet's `TokenMetadata.hs` holds no `IORef`, `TVar`, `MVar` or memo
structure, only a comment capping a response at 10 MiB. Five wallets holding the same token are five
fetches a minute for the same subject, roughly 7,200 a day for that one asset, on top of five
full-history `listAssets` reads a minute. Each wallet has its own database file (`databaseFile wid`
at `DB/Layer.hs:362`), so the `withMVar` lock is per wallet and the result is five independent stalls
rather than contention on a shared one. Measured 2026-09-10.

**The amount field accepts a decimal separator, then strips it.** Found while checking the send path
for this work and independent of it. `components/wallet/send-form/AssetInput.tsx:116`
passes `decimalPlaces={decimals}`, and `decimals` is `undefined` for every asset the user has never
configured (`domains/Asset.ts:18`). With `decimalPlaces` undefined, `react-polymorph@1.0.4` accepts
a decimal separator, because its valid-input regular expression is built from the separator at
`node_modules/react-polymorph/lib/components/NumericInput.js:195-199` and only `allowOnlyIntegers`
removes it. `bigNumberToFixed` at `:393` calls `number.toFixed(undefined, roundingMode)`, which
`bignumber.js@9.3.1` returns at full precision rather than throwing. The keypress guard at
`AssetInput.tsx:129` fires only when `decimals === 0`, so it does not fire either.
`WalletSendForm.tsx:235` then converts the field value with `formattedAmountToNaturalUnits`, which
strips separators by string substitution (`utils/formatters.ts:163-167`). Entering `1.5` for such an
asset produces a natural-unit amount of `15`. Measured in this tree:
`new BigNumber('1.23456789').toFixed(undefined, 4)` returns `'1.23456789'`, and `undefined > 0` is
`false`, so `NumericInput.js:285` and `:319` take the branch
that appends the separator to the displayed value.

On the Cardano ledger every quantity is an integer, and decimal places are presentation only, the
same relation lovelace has to ADA. A token whose decimal places are unknown is therefore correctly
displayed in raw units, and a separator typed into that field denotes nothing the form can act on.
That bounds the harm. A user who knows their token carries six decimal places types `1.5`, meaning
one and a half tokens, and sends 15 raw units: a large underpayment, and never a tenfold overspend.
The fix lands in the first phase because it is small and independently revertible, not because it
preempts other work.

The screen that follows the field is already honest about both representations. The send confirmation
renders the formatted amount at
`containers/wallet/dialogs/send-confirmation/DialogContentWithAssets.tsx:75` and the raw amount at
`:109-110`, under the `unformattedAmountLabel` string and the tooltip that explains the difference
(`.../send-confirmation/messages.ts:87-98`). The confirmation shows both; the input converts one into
the other by stripping a character. The fix is to make the input consistent with the confirmation
rather than to add a warning beside it.

## Goals

- Show a token's ticker where the registry publishes one, its CIP-25 or CIP-68 name where the chain
  carries one, and its decoded asset name where those bytes are printable text.
- Apply the registry's decimal places to displayed and entered amounts automatically when, and only
  when, the metadata is cryptographically bound to the token's minting policy.
- Tell the user, in one place, when published decimal places exist but could not be verified.
- Stop polling `GET /wallets/{id}/assets`, and delete the code that only existed to serve it.
- Keep the whole thing small enough that one person can hold it in their head.

## Non-Goals

- **Any cardano-wallet change.** The wallet keeps its `--token-metadata-server` wiring
  (`source/main/index.ts:215-218`) and its own metadata handling. This design stops exercising them.
  Nothing needs to be disabled, coordinated or released on the wallet side, and
  `mock-token-metadata-server` stays in the dev shell (`perSystem/devshells/daedalus.nix:44`) and in
  the packaging (`nix/internal/any-darwin.nix:245`) untouched.
- **A chain index, and CIP-88.** CIP-25 and CIP-68 are read through the pointer path in locked
  decision 9, one asset at a time on demand, against a pointer the user's own node confirms. That is
  a lookup, not an index. Nothing in this plan enumerates the chain, and CIP-88 is not read at all.
- **Any NFT display surface.** No gallery, no trait list, no collection view.
- **Rendering the NFT half of the `metadata` column.** The chain channel writes the CIP-25 or
  CIP-68 payload into that column, because the column is JSON and holds it without a schema
  migration. What reads it is name resolution. Traits and project information are stored where they
  arrive and are rendered nowhere.
- **A mirror of the registry.** The cache holds what the user's own wallets hold and what their own
  transaction history renders. It never enumerates the corpus.
- **Deciding which token is the one the user means.** `verified` proves that the policy which minted
  the token authored the metadata. It proves nothing about identity: 660 registry tickers collide
  across 4,256 subjects, 53 percent of the corpus, and many of those collisions are fully
  policy-bound, so two different policies can both pass verification while both claiming the same
  ticker. Anyone can mint a policy, sign metadata claiming any ticker, and pass, because passing is
  exactly what verification checks. This cache stores the ticker, so the limit applies to it
  directly: a ticker in a cached row is a claim by whoever minted the policy, not an identification.
  Choosing between competing claimants is a trust-surface problem, handled in the dApp connector
  security work rather than here.
- **Changing how the asset settings dialog stores a user's explicit choice.** Per-user browser
  storage stays where it is and keeps winning over everything.

## Inputs And Source Material

- `.agent/system/architecture.md`
- `.agent/workflows/ipc.md`, read against `source/common/ipc/lib/IpcChannel.ts`,
  `source/main/ipc/lib/MainIpcChannel.ts` and `source/renderer/app/ipc/lib/RendererIpcChannel.ts`,
  because the document's diagram does not describe the mechanism in use. See Documentation
  divergences below.
- `.agent/workflows/frontend.md`, `.agent/workflows/test.md`, `.agent/plans/readme.md`
- `source/main/governance/`: `AnchorFetchService.ts`, `AnchorVerificationService.ts`,
  `anchorCache.ts`. The closest precedent in the repository for a main-process service that fetches
  a document over the network, verifies it cryptographically, and caches the result on disk under
  a size bound.
- `source/main/ipc/governanceAnchorChannel.ts` and `source/renderer/app/ipc/governanceChannel.ts`:
  the channel pair this design copies.
- `source/renderer/app/stores/AssetsStore.ts`, `source/renderer/app/api/api.ts`,
  `source/renderer/app/api/assets/`, `source/renderer/app/utils/assets.ts`,
  `source/renderer/app/domains/Asset.ts`
- `source/renderer/app/components/assets/`, `source/renderer/app/components/wallet/tokens/`,
  `source/renderer/app/components/wallet/send-form/AssetInput.tsx`
- `source/main/config.ts`, `source/main/index.ts`, `nix/internal/launcher-config.nix`
- CIP-14 golden vectors from `cardano-wallet`, at
  `lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78`
- Token registry corpus measurements taken 2026-09-10 and 2026-09-11 against a clone of
  `cardano-foundation/cardano-token-registry` at commit `c9cf09f4`, 7,976 mapping files, and against
  the live `tokens.cardano.org` API. These back the refresh window, the policy-closure tiering and
  the ticker-collision finding below.
- A second registry measurement taken 2026-09-14 at HEAD `363982b9`, over a uniform sample of 600
  mapping files. It backs the script-shape counts and the decimals-verification table below.
- `research/01-koios-pointer-option.md` in this plan folder, which costs Koios as a pointer service
  against a local chain scan. It is the evidence behind locked decisions 9 to 11, and it holds the
  endpoint shapes, the call counts and the scan measurements cited below.
- The SMASH server setting, read as the template for the metadata source setting rather than as
  background: `source/renderer/app/components/settings/categories/StakePoolsSettings.tsx` and its
  container `source/renderer/app/containers/settings/categories/StakePoolsSettingsPage.tsx`,
  `SMASH_SERVERS_LIST`, `SMASH_SERVER_TYPES` and `SMASH_URL_VALIDATOR` at
  `source/renderer/app/config/stakingConfig.ts:12-45`,
  `source/renderer/app/api/staking/requests/checkSmashServerHealth.ts`, `getSmashServerIdFromUrl`
  at `source/renderer/app/utils/staking.ts:17-28`, and the stored selection at
  `source/renderer/app/api/utils/localStorage.ts:328-333`.

## Locked Planning Decisions

**1. A local SQLite cache in Daedalus. No Haskell.** A wallet-side design carries a Haskell change,
a coordinated release and a continuing maintenance burden on a component this feature does not
otherwise touch, and none of that is repaid by the benefit. This document designs against the local
cache and does not reopen the question.

**2. The primary key is the registry subject: policy id followed by asset name, both lowercase
hex.** That is what the registry keys on, and it is the order already used at `api.ts:3216`, `:3227`
and `:3384`, and at `AssetsStore.ts:67`. `utils/assets.ts:301-307` concatenates the other way round,
asset name followed by policy id; it has no callers and is deleted rather than carried, so the new
table inherits no ordering ambiguity.

**3. `verified` is a boolean, and it collapses three classes into two.** The underlying data has
three classes: policy-bound, where the signing key is provably required by the minting policy;
signed, where a signature exists, but the key is not bound to the policy; and unattested. The boolean
maps the first to true and the other two to false. The collapse is deliberate, and it is the right
default, because only the policy-bound case justifies changing a displayed balance without asking.
A registry operator who can sign with an unbound key can assert any decimal places they like.

**4. `verified` is computed in the main process and never carried from the wire.** No field of the
registry response sets it. It is the output of the verification chain, written at the same moment as
the row. The renderer receives a boolean it cannot influence.

**5. Decimal places are the only thing verification gates.** A ticker or a name that fails
verification is still shown. The worst case of a wrong name is a confusing label; the worst case of
a wrong decimal place count is a transaction for the wrong amount. Gating everything on verification
would hide most of the registry's useful content to guard against the mild failure.

**6. The intelligent-name work lands first, on its own, before any cache exists.** Asset names live
in the asset name bytes, which Daedalus already has for every token it holds. That half of goal one
needs no network, no cache, no verification and no IPC.

**7. The unresolved-decimals amount field degrades to raw units. It does not block and does not
refuse.** Stated in full under The cold cache and the send path.

**8. One advisory, in one place.** A wallet that decorates every token with security chrome trains
its users to ignore chrome. Stated in full under Goal three.

**9. Koios is adopted as the pointer service for on-chain metadata.** Three reasons.

Daedalus already ships external metadata dependencies of exactly this class. SMASH supplies stake
pool metadata, configured per network at `nix/internal/launcher-config.nix:31-35` and threaded
through `source/main/config.ts:70` and `source/main/preload.ts:40`, and `tokens.cardano.org`
supplies token metadata. Adding Koios is a change within an established pattern, not the
introduction of a new kind of dependency.

Coverage. The registry held 7,976 subjects at commit `c9cf09f4`, 2026-09-07. Koios answers any
asset on mainnet, including CIP-25 and CIP-68, which is the only route to naming an NFT at all.

Koios is open source, so a user can run their own instance and point Daedalus at it, exactly as
they already can for SMASH.

**10. Koios is additive to `tokens.cardano.org`, not a replacement.** Koios strips the registry
signatures. Its `token_registry_metadata` returns bare `name`, `description`, `ticker`, `url`,
`logo` and `decimals`, with no `policy`, no `sequenceNumber` and no signatures, so every input to
the verification chain in decision 3 is absent from it. Verified decimal places keep coming direct
from `tokens.cardano.org`. The end state is two external sources with different jobs: the registry
for what must be verifiable, Koios for what must be covered. No row takes its decimal places from
Koios, and nothing in this design lets Koios stand in for the registry.

**11. The metadata source is user-selectable, in the shape SMASH already uses.** Three options: the
default public Koios instance for the network, a custom URL for a user running their own Koios or
using another operator's, and a direct option that derives the same pointers by scanning the chain
the user already holds, with no third party. That is the same three-way shape as
`SMASH_SERVER_TYPES` at `source/renderer/app/config/stakingConfig.ts:29-33`, whose `DIRECT` member
is documented at `:23` as fetching metadata directly from URLs registered on chain.

The preset and the custom URL ship together, and they ship first. The direct option needs the local
scan machinery, which is the largest build of the three, and it is a named later phase rather than
scope now. The enum carries its member from the start, so adding it changes which fetch path runs
and costs no settings migration.

## Requirements

### Functional Requirements

Names:

- [ ] Show a token's registry ticker where one exists, then its registry name, then its CIP-25 or
      CIP-68 name where the chain channel has resolved one, then its decoded asset name where every
      decoded byte is printable ASCII, then nothing
- [ ] Stop rendering `ASCII: ` followed by replacement characters for asset names that are not text
- [ ] Compute CIP-14 fingerprints locally, so a token row has its primary identifier with a cold
      cache and no network

Cache and verification:

- [ ] Create the SQLite database under the state directory on first use, with the schema below
- [ ] Resolve metadata for the subjects the user holds and for the subjects rendered in their
      transaction list, and for nothing else
- [ ] Run the three-step verification chain in the main process before any row is written
- [ ] Write `verified = 1` only when all steps pass; write the row with `verified = 0` otherwise
- [ ] Bound the image store by entry count and by total bytes, and evict least recently fetched
- [ ] Answer from disk when the network is unavailable
- [ ] Re-read a row older than seven days on the next demand for it, not on a timer, and rewrite it
      only when a property's `sequenceNumber` has risen
- [ ] Offer a manual refresh for a single subject in the asset settings dialog
- [ ] Resolve the CIP-25 or CIP-68 record for a held asset the registry does not answer, in two
      calls, and confirm the pointer against the user's own node before writing the row
- [ ] Write chain-sourced rows with `source = 'chain'`, the mint block's slot, `decimals` NULL and
      `verified = 0`
- [ ] Fetch a CIP-25 record once and never re-read it while its minting policy is closed

Metadata source:

- [ ] Default to the public Koios instance named by the launcher configuration for the network
- [ ] Accept a custom URL, checked against a pattern and then against a live probe of `/tip` before
      it is stored
- [ ] Map a stored URL back to its preset, so a user who types the default sees the preset selected
- [ ] Persist the selection per profile and read it back on start
- [ ] Carry the direct option in the enum from the start, rendered as unavailable until the local
      scan lands

Decimal places:

- [ ] Resolve the decimal places used for formatting and parsing in this order: an explicit user
      setting, then a verified registry value, then none
- [ ] Never format with an unverified registry value; keep offering it in the settings dialog as the
      recommended value, which is what it is today
- [ ] Label the send amount field with the unit it is accepting, because the interpretation of that
      field flips for verified tokens at the moment of the migration
- [ ] Show a one-time notice on first run after the update, leading with what changed about entering
      an amount
- [ ] Extend `isNonRecommendedDecimalSettingUsed` so the disagreement it already renders can say
      whether the value being disagreed with was verified

Advisory:

- [ ] State in the asset settings dialog, and only there, when published decimal places exist but
      could not be verified against the minting policy

Removal:

- [ ] Delete the assets poll, the assets endpoint client, and the in-memory metadata carry-over
- [ ] Delete the whole-list spinner that replaces the token list while metadata resolves
- [ ] Delete `getUniqueId`, `getUnknownAsset` and `FALLBACK_TOKEN_METADATA_SERVER_URL`

### Non-Functional Requirements

- **The verification implementation is not accepted until it verifies the full policy-bound registry
  corpus with zero failures.** The run recorded under Goal two reports zero failures across all
  4,579 policy-bearing entries in the registry at commit `363982b9`. The implementation is measured
  against the same gate.
- **No new runtime dependency.** blake2b-224 and ed25519 verification are both already in the
  dependency tree. See Crypto primitives below.
- **The database is derived data and deleting it is safe.** It holds nothing the user authored and
  nothing that cannot be fetched again. Corruption is handled by deleting the file and recreating
  it, not by repairing it.
- **No fixed-interval poll replaces the one being removed.** Resolution is demand-driven. A failed
  batch records a retry-after time; it does not spin.
- **The main process never blocks on the network while holding the database open.** Fetch, then
  verify, then write.
- **An index is never trusted for a fact the user's own node holds.** A chain-sourced row is written
  only after the transaction the pointer names has been read out of the user's own chain and checked
  to mint that asset under that policy and to carry that metadata. An index that lies fails that
  check; an index that is unreachable leaves the row absent.
- **`yarn lint`, `yarn compile`, `yarn stylelint`, `yarn i18n:manage` and `yarn test:jest` stay
  green on every commit**, all of them required checks at `perSystem/checks.nix`.
- **New TypeScript carries no `@ts-ignore`.** The 1,073 existing directives in `source`, counted on
  2026-09-10, are accepted migration debt. New code does not add to them.
- **MobX rules hold.** `configure({ enforceActions: 'observed' })` is live, so every post-`await`
  mutation in the store is wrapped in `runInAction`, and reactions go through
  `this.registerReactions([...])`.

## Technical Design

### The schema

Three tables. The metadata row is small and read on every render; the image is large and read
almost never. SQLite reads a whole row to reach any column of it, so keeping those two together
would put tens of kilobytes of base64 in the path of every ticker lookup. The third records what has
been attempted for a subject, so a miss is not re-asked forever.

```sql
CREATE TABLE IF NOT EXISTS asset_metadata (
  subject          TEXT    NOT NULL PRIMARY KEY,
  policy_id        TEXT    NOT NULL,
  asset_name       TEXT    NOT NULL,
  ticker           TEXT,
  name             TEXT,
  decimals         INTEGER,
  verified         INTEGER NOT NULL DEFAULT 0,
  metadata         TEXT,
  source           TEXT    NOT NULL,
  sequence_number  INTEGER,
  slot             INTEGER,
  updated_at       INTEGER NOT NULL,
  CHECK (subject = policy_id || asset_name),
  CHECK (verified IN (0, 1)),
  CHECK (decimals IS NULL OR (decimals >= 0 AND decimals <= 20)),
  CHECK (source IN ('registry', 'chain')),
  CHECK (source <> 'registry' OR slot IS NULL),
  CHECK (source <> 'chain' OR sequence_number IS NULL)
) STRICT;

CREATE INDEX IF NOT EXISTS asset_metadata_policy_id ON asset_metadata (policy_id);

CREATE TABLE IF NOT EXISTS asset_image (
  subject      TEXT    NOT NULL PRIMARY KEY
               REFERENCES asset_metadata (subject) ON DELETE CASCADE,
  media_type   TEXT    NOT NULL,
  bytes        BLOB    NOT NULL,
  byte_length  INTEGER NOT NULL,
  fetched_at   INTEGER NOT NULL
) STRICT;

CREATE INDEX IF NOT EXISTS asset_image_fetched_at ON asset_image (fetched_at);

CREATE TABLE IF NOT EXISTS asset_resolution (
  subject        TEXT    NOT NULL PRIMARY KEY,
  state          TEXT    NOT NULL,
  attempted_at   INTEGER NOT NULL,
  retry_after    INTEGER NOT NULL,
  failure_count  INTEGER NOT NULL DEFAULT 0,
  CHECK (state IN ('pending', 'resolved', 'unregistered', 'failed'))
) STRICT;
```

`STRICT`, `CHECK` and foreign keys are all enforced by the bundled engine. Measured in Electron
41.3.0: SQLite 3.51.3, a `CHECK` violation and a type violation both raise, and
`PRAGMA foreign_keys=ON` takes effect.

**Why the key is the subject, and why `asset_resolution` is keyed the same way.** Because what is
being cached is a property of the asset, not of the wallet that happens to hold it, and the current
code shows the cost of forgetting that. Under the poll, five wallets holding the same token produce
five fetches a minute for the same subject, roughly 7,200 a day for one asset, because nothing
between the store and the registry deduplicates by subject. A subject-keyed table dissolves that by
construction: one row however many wallets hold the token, and one resolution attempt however many of
them are open. `asset_resolution` is keyed on the subject for the same reason. A subject that failed
to resolve failed for every wallet at once, so a wallet-scoped backoff would retry a known-failing
subject once per wallet and then wait out that many separate intervals.

**Names: three columns, not one, and `decoded_name` is not among them.** `ticker` and `name` come
from the registry, are separate signed properties, and mean different things: a ticker is a short
trading symbol for a fungible token, a name is a display name. They stay separate columns because
collapsing them would lose which one the issuer actually published.

`decoded_name` is not stored. It is a pure function of `asset_name`, which is half the primary key,
and `domains/Asset.ts:22-25` already computes exactly that as a MobX computed. Storing it would
create a second copy of a derivable value that can disagree with the function that derives it, and
would tie the name shown for an NFT to a cache row that, for an NFT, the registry will almost never
have. Decoding happens at render time, works with a cold cache, and works offline.

**`verified` and the three classes.** Per locked decision 3. A row is `verified = 1` only when the
policy field decodes to a native script whose blake2b-224 digest is the subject's policy id, that
script **evaluates to true against the set of keys that signed the property**, and the ed25519
signature over that property verifies. The middle class, a valid signature from a key set the policy
does not satisfy, is written as `verified = 0` alongside the unattested class. The distinction is
not recoverable from the row and is deliberately not stored: nothing in the design treats the two
differently, and a column nothing reads is a column that goes stale.

**Script evaluation is the registry's rule, and a key-hash lookup is not.** Looking for "the key
hash the script requires" is undefined for `any` and `atLeast` scripts, where no single key is
required and one signer does not satisfy the policy. Marking such an entry verified would
reintroduce exactly the operator trust `verified` exists to remove. The reference implementation
evaluates the whole script against the attesting key set
(`token-metadata-creator/src/Cardano/Metadata/Types.hs:253-287`, `evaluatePolicy`):

```haskell
isValidScript (RequireTimeAfter _lockStart)  = True
isValidScript (RequireTimeBefore _lockExp)   = True
isValidScript (RequireSignature hash)        = Set.member hash keyHashes
isValidScript (RequireAllOf xs)              = all isValidScript xs
isValidScript (RequireAnyOf xs)              = any isValidScript xs
isValidScript (RequireMOf m xs)              = m <= sum (fmap (\x -> if isValidScript x then 1 else 0) xs)
```

**Time locks evaluate to `True` unconditionally.** Both `RequireTimeAfter` and `RequireTimeBefore`
are satisfied regardless of the current slot. An implementer who checks `invalid_hereafter` against
the clock fails every already-expired policy, which is 4,104 of 4,578 policy-bearing entries,
including this document's own worked example, whose lock expired in 2022. The evaluator is ten
lines; write it rather than special-casing.

Measured over a uniform sample of the registry (600 of its mapping files, registry HEAD
`363982b9`, 2026-09-14), script shapes were 368 `all + before + sig`, 30 bare `sig`, 5 `all + sig`,
1 `atLeast`, and 1 carrying both time bounds. Exactly one entry had more than one distinct signing
key. So the key-hash shortcut and the evaluator agree on today's corpus, and the divergence is
latent rather than live. It is still wrong, and the registry accepts any script.

The policy-bearing rate in that sample overstates the corpus. 405 of its 600 entries carry a
parseable policy, 67.5 percent, against 4,579 of 7,977 across the whole registry at the same commit,
57.4 percent. The full-corpus count under Goal two is the one to use. These script-shape counts stay
sampled, so they describe the shapes that occur rather than their exact frequency.

**`sequence_number`, `slot` and `updated_at` are three different facts.** The registry gives a
per-property `sequenceNumber` and no slot. CIP-25 gives a real slot and no sequence number. The two
`CHECK` constraints on `source` make it impossible to write a row that claims both. `updated_at` is
the local clock at which the row was written and is a cache-freshness fact, not a chain fact, so it
is always present and never conflated with the other two. Because the registry versions each
property independently, `sequence_number` holds the maximum across the properties the row actually
stores; a bump in any of them is a reason to re-read the subject.

Rows resolved from the token registry carry `source = 'registry'` and a `sequence_number`. Rows
resolved through the chain channel carry `source = 'chain'` and the mint block's `slot`. Both are
written, which is what the column and its two `CHECK` constraints were shaped for, so the chain
channel changes what is written and not the shape it is written into.

**One row per subject, and the registry wins.** The primary key is the subject, so a subject has one
row whichever channel answered it. The chain channel is consulted only for subjects the registry
does not answer, and the two sets are close to disjoint in practice: the registry exists for
fungible tokens, and CIP-25 and CIP-68 exist for NFTs. Where both could answer, the registry row
stands, because it is the only one of the two that can carry verified decimal places. That is a
precedence rule rather than a schema change, and the `CHECK` constraints already make a row claiming
both a sequence number and a slot impossible to write.

**`metadata` is JSON.** For a registry row it holds the registry's `url` and `description`, plus
any additional properties the registry carries for that subject. Both are present in the live
response. For a chain row it holds the CIP-25 or CIP-68 payload as returned, which is where traits
and project information land. Name resolution reads it. Nothing else does.

**`image` bounds itself at the transport, not only in the store.** Registry logos are inline base64
PNG with a median entry of 36,191 bytes and 316 MiB across the corpus, so the only safe policy is
never to ask for them in bulk. The bulk query does not request the `logo` property at all; a logo is
requested one subject at a time, and only when a component has decided to render one for an asset
the user holds. The store then applies two bounds, in the shape already used for DRep anchors at
`source/main/governance/anchorCache.ts:24-25`: at most 2,000 entries and at most 64 MiB, evicting
the least recently fetched. At the median entry size the two bounds bite at roughly the same point,
and neither alone protects against the other's pathological case. A single entry over 256 KiB is
discarded rather than stored.

A CIP-25 or CIP-68 image is a URI rather than inline bytes, usually `ipfs://`. The chain channel
stores that URI in the `metadata` column and fetches nothing. Retrieving it would mean an IPFS
gateway, which is a further external dependency and is not in scope here, so `asset_image` holds
registry logos only.

### Where it lives and how it is reached

**The Electron main process, on `node:sqlite`.** Verified in this repository on 2026-09-10:

```
ELECTRON_DISABLE_SANDBOX=true ELECTRON_RUN_AS_NODE=1 ./node_modules/.bin/electron \
  -e "console.log(process.versions.node, Object.keys(require('node:sqlite')))"
```

prints `24.15.0` and `DatabaseSync,StatementSync,Session,constants,backup` under Electron 41.3.0
(`package.json:223`), with no flag and no experimental warning. A follow-up probe opened a file
database, set `PRAGMA journal_mode=WAL`, round-tripped a `BLOB` as a `Uint8Array`, and enforced both
a `CHECK` constraint and `STRICT` typing.

It is in main rather than the renderer for two reasons. Every filesystem-backed store in Daedalus
already lives in main. And the verification is the whole value of the feature, so it belongs behind
the process boundary rather than in the page.

The sandboxing posture is not a third reason, and reading it off the wrong object is an easy
mistake. `windowOptions.webPreferences` sets `nodeIntegration: isTest`
(`source/main/config.ts:104`), which would make Node built-ins unavailable to the renderer, but that
export at `source/main/config.ts:99-110` has no importer; the window actually constructed at
`source/main/windows/main.ts:67` uses a local object declared at `:45` with `nodeIntegration: true`
(`:51`) and `contextIsolation: false` (`:54`). The renderer can reach Node built-ins today. The
placement decision stands on the two reasons above, which do not depend on the sandboxing posture.

**On disk.** `path.join(stateDirectoryPath, 'asset-metadata-cache', 'assets.sqlite')`, with
`stateDirectoryPath` from `source/main/config.ts:125`, matching `anchorCacheDirectoryPath` at
`source/main/governance/anchorCache.ts:36-37`. The state directory is set per platform by the
launcher configuration at `nix/internal/launcher-config.nix:193-199`:

| Platform | Database path |
|---|---|
| Linux | `$XDG_DATA_HOME/Daedalus/<network>/asset-metadata-cache/assets.sqlite` |
| macOS | `~/Library/Application Support/<spacedName>/asset-metadata-cache/assets.sqlite` |
| Windows | `%APPDATA%\<spacedName>\asset-metadata-cache\assets.sqlite` |

On Linux `XDG_DATA_HOME` defaults to `~/.local/share` (`nix/internal/x86_64-linux.nix:361`). The
database gets its own directory because WAL mode creates `-wal` and `-shm` siblings, and because
deleting one directory is then the entire reset procedure.

**The channel mechanism, verified against source rather than the workflow document.** A channel is
one `IpcChannel` subclass instance per side, constructed from a shared name constant.
`source/common/ipc/lib/IpcChannel.ts:91-93` derives the three wire names from that constant:

```ts
this._broadcastChannel = `${channelName}-broadcast`;
this._requestChannel = `${channelName}-request`;
this._responseChannel = `${channelName}-response`;
```

`ipcRenderer.invoke` and `ipcMain.handle` appear nowhere in `source/`; `ipcRenderer.send` appears
exactly twice, both in `stores/WindowStore.ts:17` and `:25`, neither through a channel. The pair to
copy is `source/main/ipc/governanceAnchorChannel.ts:11-14` and
`source/renderer/app/ipc/governanceChannel.ts:8-10`, with the push direction taken from
`source/main/ipc/nodePushChannel.ts:16-19`.

### Data / IPC / API Changes

Three channels, declared in `source/common/ipc/api.ts` next to the governance block at `:536-543`.

```ts
export const ASSET_METADATA_CHANNEL = 'ASSET_METADATA_CHANNEL';
export type AssetMetadataRendererRequest = {
  requestId: string;
  subjects: Array<string>;
};
export type AssetMetadataMainResponse = {
  requestId: string;
  entries: Array<AssetMetadataEntry>;
  unresolved: Array<{ subject: string; state: AssetResolutionState }>;
};

export const ASSET_METADATA_UPDATE_CHANNEL = 'ASSET_METADATA_UPDATE_CHANNEL';
export type AssetMetadataUpdateMainRequest = {
  entries: Array<AssetMetadataEntry>;
};
export type AssetMetadataUpdateRendererResponse = void;

export const ASSET_IMAGE_CHANNEL = 'ASSET_IMAGE_CHANNEL';
export type AssetImageRendererRequest = {
  requestId: string;
  subject: string;
};
export type AssetImageMainResponse =
  | { requestId: string; status: 'absent' }
  | { requestId: string; status: 'present'; mediaType: string; bytes: Uint8Array };
```

`AssetMetadataEntry` lives in a new `source/common/types/asset-metadata.types.ts`:

```ts
export type AssetResolutionState =
  | 'pending'
  | 'resolved'
  | 'unregistered'
  | 'failed';

export type AssetMetadataEntry = {
  subject: string;
  policyId: string;
  assetName: string;
  ticker: string | null;
  name: string | null;
  decimals: number | null;
  verified: boolean;
  source: 'registry' | 'chain';
  hasImage: boolean;
  metadata: Record<string, unknown> | null;
};
```

**Absence has to say which absence it is.** A subject with no row can mean three different things:
never asked, asked and the registry did not know it, or asked and the request failed. Collapsing
them means the renderer cannot distinguish "we have not looked yet" from "this asset is not in the
registry and never will be", and the CIP-30 connector work has an open decision that turns on
exactly that boundary. The response therefore carries `unresolved` alongside `entries`, keyed by the
`asset_resolution` state.

**`source` is on the entry because a node-confirmed chain row is not the same as an unattested
one.** A chain row is written only after the pointed-at transaction has been read out of the user's
own chain and checked to mint that asset under that policy, yet it carries `verified = 0` because
the registry attestation chain never ran for it. Without `source`, the strongest local proof in the
design is indistinguishable from the weakest, and any surface that marks absence of provenance would
mark it for names Daedalus proved against the user's own immutable database.

The request channel is a read of what the cache holds right now. It never waits for the network: a
subject with no row comes back in `unresolved`, and resolution for it is scheduled. The update
channel is the push that carries rows as they resolve, so the renderer's map fills in without asking
again. The image channel is separate and per subject, which is the mechanism that keeps logos off
the path of every other read.

**`IpcChannel` does not correlate requests with responses, so these channels carry a `requestId`.**
Both `send` (`source/common/ipc/lib/IpcChannel.ts:101-120`) and `request` (`:126-145`) do
`receiver.once(this._responseChannel, ...)` and resolve on the next message on that channel,
whatever request it answers. The doc comment states it outright: "waits for the next response on the
same channel". Neither `MainIpcChannel` nor `RendererIpcChannel` adds correlation.

The governance channel this design copies is a single-shot, user-initiated lookup, so two requests
are never in flight and the defect never bites there. A bulk subject-keyed read is the opposite
shape: overlapping reads are expected, not exceptional, and two in-flight requests can each resolve
with the other's payload. Because the map feeds `decimals`, a mis-correlated response is a
wrong-decimals path, not merely a wrong label.

Every request carries a `requestId` and the responder echoes it. The renderer client discards a
response whose `requestId` it did not issue and keeps waiting. This is local to the three new
channels; correcting `IpcChannel` itself for every existing channel is a larger change and is not
in scope here. The defect is latent across all of them.

**A metadata read never blocks a render.** This is deliberate, and it is the reason the cache cannot
serve a signing-confirmation dialog as currently specified. The CIP-30 connector work requires
subjects absent from the cache to be resolved in one batch issued *before* a confirmation dialog
renders, with a hard timeout, and none of these three channels does resolve-and-wait. The 10-second
timeout plus retry plus doubling backoff is also not a confirmation-dialog budget, and the backoff
actively suppresses a subject that failed once. Reconciling the two is cheap now and expensive after
phase 3; see Open Questions.

No HTTP contract with cardano-wallet changes. Two of its endpoints stop being called.

### The fetch and verify path

**The endpoint.** `POST {metadataUrl}/metadata/query`, with `metadataUrl` read from
`launcherConfig.metadataUrl` (`source/main/config.ts:71`, populated per network at
`nix/internal/launcher-config.nix:37-41`) and the same literal fallback already used at
`source/main/index.ts:217`, `https://tokens.cardano.org`.

**Selfnode needs an explicit case; the launcher value does not cover it.**
`nix/internal/launcher-config.nix:448-450` assigns `metadataUrl` under
`lib.optionalAttrs (network != "selfnode")`, so on selfnode the key is absent and the literal
fallback points the fetch path at mainnet rather than at the bundled `mock-token-metadata-server`.
The fetcher therefore resolves its endpoint as `launcherConfig.metadataUrl`, then the configured
source setting, then the mock when `network === 'selfnode'`, then the mainnet literal. Manual QA
gains a selfnode case; without one nothing catches this, because a mainnet registry answers
plausibly and the failure is silent.

Request body:

```json
{ "subjects": ["<subject hex>", "..."],
  "properties": ["name", "ticker", "decimals", "url", "description"] }
```

Confirmed live on 2026-09-10 for subject
`c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544`: each entry carries `subject`,
`policy` and, per property, `{ value, sequenceNumber, signatures: [{ signature, publicKey }] }`.
`policy` is returned whether or not it is requested, which is what makes verification possible from
the bulk query alone. `logo` is deliberately absent from the property list.

**Subject derivation.** Two sources, both already in the renderer. Holdings come from
`activeWallet.assets.total`, built at `api.ts:3225-3232` and already read at
`WalletTokensPage.tsx:41` and `WalletSummaryPage.tsx:122`. Rendered transactions come from
`WalletTransactionsList.tsx:240`, which can name assets the wallet no longer holds. The store
collects both, subtracts the subjects it already has rows for, and sends the remainder.

**Batching is sized in request bytes, not in subjects.** The endpoint enforces a request-body cap
near 8,192 bytes and does not enforce a subject count. Measured against
`https://tokens.cardano.org/metadata/query` on 2026-09-14:

| subjects | body bytes | HTTP |
|---|--:|---|
| 90 | 7,156 | 200 |
| 100 | 7,892 | 200 |
| 110 | 8,614 | **413** |

A subject is 56 hex characters of policy id plus up to 64 of asset name, so 100 worst-case subjects
is roughly 12.3 KB and always fails. Two of eight sampled 100-subject batches returned 413 on asset
name length alone; splitting one into 50 plus 50 at 6,282 bytes each returned 200 for both. The
batch builder therefore accumulates subjects until the serialized body would exceed **6 KB** and
sends what it has. Batches are issued one after another rather than in parallel, so a wallet with
many tokens opens one socket at a time.

**Timeout and retry.** A 10-second timeout per request, matching `ANCHOR_TIMEOUT_MS` at
`source/main/governance/AnchorFetchService.ts:9`. A timeout or a 5xx gets one retry after a short
backoff, then the batch is abandoned and each of its subjects gets an `asset_resolution` row in
state `failed` with a `retry_after` that doubles per consecutive failure up to a ceiling.

**A 4xx is never retried.** 413 is deterministic, not transient: retrying a batch that is too large
produces the same 413 forever, and an exponentially doubling `retry_after` turns a fixable sizing
error into subjects that are never resolved and never diagnosed. Any 4xx marks the batch as a
client-side defect, is logged at warn with the body size and subject count but not the subject list,
and is not scheduled for retry. A 413 specifically causes the batch to be split once and re-sent, so
a sizing miscalculation degrades rather than fails.

**A subject the registry does not know is recorded, not re-asked.** The registry omits unknown
subjects from the response rather than returning a negative; sending two subjects and receiving one
is a success, not a failure. Without a record, every demand re-schedules the same subject, which
re-creates the polling behavior this plan exists to delete, and does so worst for wallets holding
NFTs, which the registry essentially never covers. Every subject in a 200 response that came back
with no entry is therefore written to `asset_resolution` in state `unregistered`, with the same
`retry_after` discipline. The chain channel consults that state to decide what to look up.

There is no interval timer anywhere in the design.

**Offline.** The fetch fails, the cache answers from disk, and rows that are absent stay absent. No
dialog and no toast: a missing ticker is not something the user can act on. Failures are logged at
debug through `source/main/utils/logging`, without the subject list.

**Where verification runs.** Entirely in the main process, between the response arriving and the row
being written, per locked decision 4.

1. **Policy binding.** Strip the two leading bytes from the `policy` field and take blake2b-224 over
   `0x00` concatenated with the remainder. The digest must equal the subject's first 28 bytes.
2. **Script evaluation.** Take blake2b-224 of each `publicKey` that signed the property under
   consideration to get the attesting key set, then evaluate the decoded native script against that
   set using the rules in the schema section. Time-lock nodes evaluate to `True` unconditionally.
3. **Signature.** The ed25519 signature must verify, strictly, over the registry's attestation
   payload for that property, sequence number included.

Steps 1 and 2 were reproduced locally against the subject above using only packages already in the
tree:

```
node -e "const b=require('blake2b');const p='820182018282051a0303eb448200581c39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f';
console.log(b(28).update(Buffer.concat([Buffer.from([0]),Buffer.from(p.slice(4),'hex')])).digest('hex'));
console.log(b(28).update(Buffer.from('5817526d712f71e33a31ac3429fb7ce70b3e17e727044d9a2a51493e7894ba48','hex')).digest('hex'));"
```

The first line prints `c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9`, which is the
subject's policy id. The second prints `39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f`,
which is the key hash this particular script requires.

**Step 3's payload.** The construction is in the registry's own implementation
(`token-metadata-creator/src/Cardano/Metadata/Types.hs`, `hashSubject` at `:128-129`,
`hashProperty` at `:149-150`, `hashSequenceNumber` at `:530-533`, `isAttestedBy` at `:453-458`). The
signed message is the 32-byte digest:

```
blake2b256( blake2b256(CBOR(subject))
         || blake2b256(CBOR(propertyName))
         || blake2b256(CBOR(value))
         || blake2b256(CBOR(sequenceNumber)) )
```

Verified live on 2026-09-14 for `name`, `ticker`, `url`, `description` and `decimals` on subject
`c76ef54…42544544`. The trap is that the subject and the property name must be CBOR text-string
encoded before hashing. Hashing them as raw UTF-8 reproduces no signature in the corpus.

**The `logo` property does not follow the same value encoding.** `logo` signs `CBOR.encodeBytes`
over the **base64-decoded** PNG, not over the base64 text. Both paths were tested against the live
logo for that subject: base64-as-text fails, decoded-bytes-as-CBOR-bytes verifies. Phase 5 fetches
and renders logos, so without this written down the first implementation sees every logo signature
fail and concludes the corpus is broken.

**Crypto primitives, checked rather than assumed.** No new dependency is needed.

| Primitive | Package | Evidence |
|---|---|---|
| blake2b-224 and blake2b-256 | `blake2b`, declared at `package.json:210` as `2.1.3` | `blake2b(28)` and `blake2b(32)` give the digests; already imported in the main process at `source/main/governance/AnchorVerificationService.ts:1` |
| ed25519 verify | **Node's built-in `crypto.verify`**, no package | `crypto.verify(null, message, keyObject, signature)` over an `ed25519` raw public key. Strict: rejects non-canonical `S`. Runs in the main process, where verification already lives |
| bech32 | `bech32`, declared at `package.json:207` as 2.0.0 | Direct dependency; used for CIP-14 below |

**Strict verification.** Tested on 2026-09-14 against a real registry signature with its scalar
`S` replaced by `S + L`:

| verifier | valid signature | `S + L` |
|---|---|---|
| `cardano-crypto.js` 5.3.6-rc.6 | accepts | **accepts** |
| `@noble/curves` 1.9.7 | accepts | rejects |
| Node built-in `crypto.verify(null, ...)` | accepts | rejects |

`cardano-crypto.js` is not a strict verifier, so it is not used for this. Node's built-in is strict,
already available, and adds nothing to the dependency tree, which keeps the "no new runtime
dependency" requirement intact. Promoting `@noble/curves` is explicitly **not** taken: `yarn.lock`
carries two major versions of it (1.9.7 at `:2069`, 2.2.0 at `:2076`), both pulled by `@trezor/*`
and `@ethereumjs/*`, and declaring one in an application that drives hardware wallets is not a free
change.

Malleability is not exploitable in this design; it lets an attacker restate a signature over the
same value, not forge a different one. Strictness is still required, because `verified` is the whole
basis on which decimal places are applied to amounts.

**One dependency discrepancy to resolve during implementation.** `package.json:210` declares
`blake2b` at `2.1.3` and `yarn.lock:6666-6671` resolves that entry to 2.1.3, but the tree also
hoists `blake2b@^2.1.4` for `@cardano-sdk/crypto`. Pin what the code imports rather than relying on
hoisting; a clean install may not give 2.1.4.

**Fingerprints move from the wallet to local computation.** `fingerprint` is today a field of the
assets endpoint response (`api/assets/types.ts:18`), so dropping the endpoint drops the only source
of the identifier the token row renders most prominently (`Asset.tsx:199-203`). CIP-14 is
computable with the packages above: bech32 with the human-readable part `asset` over blake2b-160 of
the policy id bytes concatenated with the asset name bytes. Verified in this tree against the eight
golden vectors from
`cardano-wallet`'s `lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78`,
including the empty asset name and the 32-byte asset name cases, all matching. The Testing Strategy
uses the same eight vectors. This is a
load-bearing task, not an optimization: without it the token row loses its identifier.

### The metadata source setting

Per locked decision 11. The setting is a structural copy of the SMASH server setting, which ships
today and answers the same question: a default the project chooses, a URL the user can override, and
an option that uses no third party at all. Copying it rather than inventing a shape means the
selection, the validation, the error state and the persistence behave the way a part of Daedalus
already behaves, and it is the reason this surface is sized against an existing implementation
rather than from scratch.

**The presets, the types and the validator.** `source/renderer/app/config/assetsConfig.ts` gains the
asset equivalent of `SMASH_SERVERS_LIST` at `source/renderer/app/config/stakingConfig.ts:12-28`,
`SMASH_SERVER_TYPES` at `:29-33` and `SMASH_URL_VALIDATOR` at `:43-45`:

```ts
export const ASSET_METADATA_SERVERS_LIST: Record<
  AssetMetadataSourceType,
  { name: string; url: string }
> = {
  koios: { name: 'Koios', url: koiosUrl },
  // Pointers are derived from the chain the user already holds.
  direct: { name: 'direct', url: 'direct' },
};

export const ASSET_METADATA_SOURCE_TYPES: Record<string, AssetMetadataSourceType> = {
  KOIOS: 'koios',
  CUSTOM: 'custom',
  DIRECT: 'direct',
};

export const ASSET_METADATA_URL_VALIDATOR = new RegExp(
  '^(direct|https://[a-zA-Z0-9-_~.]+(:[0-9]+)?(/[a-zA-Z0-9-_~.]+)*/?)$'
);
```

`koiosUrl` reaches the renderer the way `smashUrl` does. It is a per-network attribute beside
`smashServers` at `nix/internal/launcher-config.nix:31-35` and `tokenMetadataServers` at `:37-41`,
assigned beside `smashUrl` at `:456`, typed at `source/main/config.ts:70` and exposed on `global` at
`source/main/preload.ts:40`.

The validator differs from the SMASH one in a single respect, and the difference is load-bearing.
Koios serves under a path prefix, `/api/v1`, so the pattern admits a path where
`SMASH_URL_VALIDATOR` rejects one. Everything else is the same: `https://` only, an optional port,
and the literal `direct`.

**The health probe.** `checkSmashServerHealth` issues `GET /v2/smash/health` against a candidate URL
(`source/renderer/app/api/staking/requests/checkSmashServerHealth.ts`), and `api.ts:2225-2250`
accepts the URL only when the response reports the server available. Koios exposes `GET /tip`, which
returns the instance's current tip, and it serves the same purpose: a URL that does not answer it is
not a Koios instance, and an instance whose tip is far behind the user's own node is not one to read
pointers from. `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.ts` is the
same six lines against that path. The direct option skips the probe, exactly as `api.ts:2227-2228`
returns true for `SMASH_SERVERS_LIST.direct.url` without issuing a request.

**Mapping a URL back to a preset.** `getSmashServerIdFromUrl` at
`source/renderer/app/utils/staking.ts:17-28` reduces over the preset list and falls back to
`CUSTOM`, so a user who pastes the default URL sees the default selected rather than a custom entry
holding the same string. `getAssetMetadataSourceIdFromUrl` in `source/renderer/app/utils/assets.ts`
is the same reduction over the asset preset list.

**Persistence.** `getSmashServer`, `setSmashServer` and `unsetSmashServer` at
`source/renderer/app/api/utils/localStorage.ts:328-333` store the SMASH selection per profile, and
the asset equivalent is three more lines against a new key. On start the store reads the stored
value and falls back to the launcher configuration's Koios URL when there is none, which is what
`StakingStore._getSmashSettingsRequest` at `source/renderer/app/stores/StakingStore.ts:199-224` does
for SMASH. One thing is simpler here: SMASH has a cardano-wallet setting to keep in step, so that
store writes twice and reconciles the two on start. The metadata source has no server-side
counterpart, so the stored value is the only one and there is nothing to reconcile.

**The surface.** A settings category, in the shape of
`source/renderer/app/components/settings/categories/StakePoolsSettings.tsx` and its container
`source/renderer/app/containers/settings/categories/StakePoolsSettingsPage.tsx`. A `Select` over the
three types, an `InlineEditingInput` shown only for `CUSTOM` carrying the validator and the error
message, and a description under the selection that changes with the selected type. It reaches the
user through one entry each in `source/renderer/app/routes-config.ts:46-55`,
`source/renderer/app/Routes.tsx:136-140` and
`source/renderer/app/components/settings/menu/SettingsMenu.tsx:39-44`.

The SMASH component is 405 lines and its container is 41, and most of the component is
`defineMessages` and the three description branches. The asset copy is smaller than that, because it
has no sync gate and no server-side setting to push.

### The chain channel: CIP-25 and CIP-68 through a confirmed pointer

Koios is read as an index, not as an oracle. It returns a pointer to the transaction that minted the
asset. The user's own node holds the block at that pointer, the block is read locally, and the
transaction in it is checked to mint the asset under the claimed policy and to carry the claimed
metadata. An index that lies fails a local check rather than corrupting the cache, and an index that
is unreachable leaves rows absent rather than wrong.

**The disclosure, stated plainly.** Adopting a pointer service means a third-party operator learns,
keyed to the user's source IP, which assets that user holds and roughly when they acquired each one.
The research note records the operator's HAProxy `stick-table type ip` rate limiting
(`research/01-koios-pointer-option.md:67-76`), so the per-IP association is a design property of the
service, not an accident.

For fungible tokens this is close to what `tokens.cardano.org` already sees under the current
design, and the incremental signal is timing. For NFTs it is not: a set of NFT policy ids is close
to a wallet fingerprint, and NFTs are the entire reason the pointer channel exists. This is a new
disclosure, not a timing correlation on an existing one.

It is disclosed rather than engineered around. The mitigations that follow are proportionate and
stop there: the channel is consulted only for subjects the registry did not answer, so fungible
holdings never reach it; the user selects the source and can repoint it through the metadata
source setting, so a user who does not accept the disclosure can point it at their own instance or
disable the channel; and requests carry no wallet identifier, no address and no quantity, only
subjects. There is no proxy, no cover traffic and no batching jitter, because each of those costs
real complexity for a partial improvement against an operator who is already trusted not to be
malicious, only not to be omniscient.

The operational limits from the research note are carried into the design rather than left in it: a
per-IP request ceiling, explicit `429` handling that backs off rather than retries, and fail-closed
behavior that leaves rows absent when the ceiling is reached.

**Pointer resolution takes two calls, not one.** `asset_info` returns `policy_id`, `asset_name`,
`asset_name_ascii`, `fingerprint`, `minting_tx_hash`, `total_supply`, `mint_cnt`, `burn_cnt`,
`creation_time`, `minting_tx_metadata`, `token_registry_metadata` and `cip68_metadata`, and no block
hash, no slot and no block height. Neither does any other asset endpoint. A transaction hash is not
a chain point, so a second call is always needed. `tx_cbor` returns `block_hash`, `block_height`,
`absolute_slot` and the raw transaction CBOR in one response, so it replaces `tx_info` at no extra
cost and supplies the bytes the local check runs against.

Measured on 2026-09-11 against the minting transaction of subject
`c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544`: `asset_info` trimmed with a
PostgREST `select=` list returns 351 bytes against 78,821 untrimmed, because the base64 logo is 98.2
percent of the untrimmed response, and `tx_cbor` returns 3,251 bytes. Both endpoints take arrays, so
the two calls are per batch rather than per asset, and a wallet holding any number of assets up to
the batch size costs two requests.

**Volume is one resolution per asset for the life of an installation.** Policy closure is what makes
that true. Of the 4,578 registry entries carrying a parseable policy, 4,104, 89.6 percent, sit under
a policy whose `invalid_hereafter` slot has already passed, so they can never mint again and their
CIP-25 record is final. Combined with two-call bulk resolution, a user resolves each asset once and
never asks again.

**What the local check establishes, and what it does not.** Three checks run against the `tx_cbor`
bytes with no chain access at all, and all three were executed against a live response: blake2b-256
over the transaction body reproduces the transaction hash Koios claims, blake2b-256 over the
auxiliary data equals transaction body key 7, and transaction body key 9 names the subject's policy
id and asset name. That establishes the bytes are self-consistent and that the metadata is bound to
that transaction by its auxiliary-data hash. It does not establish that the transaction was ever
accepted into a block, because anyone can construct bytes that never were. Reading the block at
`block_hash` and `absolute_slot` out of the user's own immutable database closes that, and nothing
smaller does.

Two implementation notes carry from the research. The transaction id is blake2b-256 over the body as
originally encoded: re-encoding with a CBOR library's canonical mode produced the wrong hash and the
library's default encoding produced the right one, so the decoder chosen has to be able to hand back
the original byte range of a decoded item, and is checked for that before it is adopted. And the
Koios documentation describes `minting_tx_hash` as both the first and the latest mint. For an asset
under a closed policy there is one final state either way; for an asset with `mint_cnt > 1` under an
open policy the pointer is read as whichever transaction it names, and the local check confirms that
transaction or rejects it.

**CIP-68 is stored on weaker footing than CIP-25.** A
CIP-25 payload is in the mint transaction, so the block read confirms it. A CIP-68 payload is a
datum at the reference-NFT UTxO, which changes whenever that UTxO is spent, so the mint transaction
says nothing about its current value. Confirming it locally means querying the live UTxO set, which
is a local-state-query against the node and a third mechanism this plan does not build. CIP-68
values are therefore stored as Koios reports them. That is acceptable only because of what they are
used for: a name, and nothing else. An unconfirmed name is the same class of risk locked decision 5
already accepts for an unverified registry name, where the worst case is a confusing label.

**What is written.** A chain row carries `source = 'chain'`, the mint block's `absolute_slot` in
`slot`, `sequence_number` NULL, `decimals` NULL and `verified = 0`. `verified` means the registry
verification chain of locked decision 3 passed, and a chain row never runs it, so it never claims
it. Leaving `decimals` NULL is the mechanical form of locked decision 10: the decimals resolution
order under goal two is untouched by this channel, and no amount anywhere is formatted by a number
that came from Koios.

Where Koios answers with both `cip68_metadata` and `minting_tx_metadata` for a subject, the CIP-68
datum is the live record and is the one stored.

### The direct option, and why it is a later phase

The same pointers are derivable with no third party, by scanning the chain the user already holds.
The traversal is cheap for a structural reason: `transaction_bodies`, `transaction_witness_sets` and
`auxiliary_data_set` are separate top-level positions in the block array, and mint information is
`transaction_body` key 9, so a scan that reads the mint field never materializes the witness sets
that carry most of the bytes.

A full CBOR decode, materializing every witness set, was sampled at two 200-chunk regions of the
local preprod immutable database and projected across the whole database, 15.64 GiB across 6,092
chunks, on 2026-09-11: 48 to 58 seconds. That figure is a projection from the sampled throughput,
not a wall-clock run of the full database, and because it materializes witness sets rather than
skipping them, a mint-field scan, which never materializes witness sets, would take less time than
this, though that lower figure has not itself been measured. Mainnet is not synced on that machine,
so every mainnet figure is an extrapolation from that preprod projection and not a measurement:
roughly 9 to 11 minutes on the same fast disk, and an estimated 31 to 61 minutes on a modest or
contended one, once.

It is still the largest build of the three options. It needs era-dependent CBOR shapes across Byron,
Shelley, Allegra, Mary, Alonzo, Babbage and Conway, the chunk and index format, resumability, a
decision about what to index, and a first-run experience for a multi-minute pass that does not look
broken. What it buys is that it has no external dependency, no rate limit, no key and no operator,
and it answers for every asset on the chain rather than for whatever an index holds. That is worth
building, and it is not worth building first, which is the sequencing in locked decision 11: the
option appears in the settings enum from the start and is rendered unavailable until the scan lands.

### Freshness, and why the miss path matters more than the refresh path

**The registry barely changes.** Measured 2026-09-10 and 2026-09-11 against the registry at commit
`c9cf09f4`, 7,976 mapping files. 7,464 of them, 93.58 percent, sit at `sequenceNumber` 0 on every
property and have never been updated. The 512 that have moved, 6.42 percent, are concentrated at the
first step: 388 at 1, 80 at 2, 23 at 3, 10 at 4, 7 at 5 and 4 at 6. Git history runs 2021-02-17 to
2026-09-07, and in the last 365 days 375 commits touched `mappings/` while only 38 files were
modified rather than added. The churn is new registrations, not edits. That is roughly one change
somewhere in the whole registry every 9.6 days, and a 0.48 percent per-asset annual probability of
any change at all.

**The refresh window is seven days.** A row older than seven days is re-read on the next demand for
it, not on a timer. The re-read compares each property's `sequenceNumber` against the stored one and
rewrites the row only when one has risen; `updated_at` is stamped on every successful read whether
or not anything changed, which is what stops a subject the registry never updates from being re-read
on every render. The asset settings dialog also carries a manual refresh for that subject, for the
user who knows an issuer published something today and does not want to wait out the window.

The corpus average is not the rate that applies to a user's own tokens. Assets a real user holds skew
toward active projects, which are likelier than average to update, so the per-asset rate for held
assets is higher than 0.48 percent. Nobody has measured how much higher, and this document does not
estimate it.

**The demand path is the one to engineer carefully.** 375 commits against 38 modifications says the
live event is a user acquiring a token the cache has never seen, not a record changing under one they
already hold. Fetch-on-miss is therefore where the design effort goes: batching, backoff, and a token
list that renders correctly while a miss is outstanding. Refresh gets the smallest mechanism that
works.

### Freshness is per channel, not per asset

A minting policy can close, and closure is measurable. Of the 4,578 registry entries carrying a
`policy` field, every one parsed as a native script with zero failures. 4,104 of them, 89.6 percent,
are under a policy whose `invalid_hereafter` slot has already passed, so they can never mint again.
47 carry a time lock still in the future, so each becomes permanently closed at a known, storable
slot. The remaining 427, 9.3 percent, carry no time lock at all, and they are exactly the Shelley-era
`8200`-wrapped MultiSig scripts: `invalid_hereafter` arrived with Allegra, so a Shelley MultiSig
cannot express a lock. The two-byte era wrapper says whether closure can be expressed.

That 4,578 is a count of entries carrying a parseable policy. It is not the verified share, which
signature verification can only reduce, and which has been measured only over the sample reported
under Goal two rather than over the whole corpus.

Closure freezes one of the three metadata channels, which is what sets the refresh policy for each
of them separately:

| Channel | Where the data lives | Does a closed policy freeze it? |
|---|---|---|
| Registry (`ticker`, `name`, `decimals`, `url`) | Off-chain at `tokens.cardano.org` | No |
| CIP-25 (NFT name, image, files) | Auxiliary data of the mint transaction | Yes |
| CIP-68 (name, image, traits) | Datum at the reference-NFT UTxO | No |

A closed policy freezes minting, so it freezes CIP-25, whose payload is in the mint transaction and
cannot be republished once no further mint can occur. It does not freeze the registry record: the
attestation is signed by a key that satisfies the script, and that key still exists and can still
sign long after the minting window shuts. It does not freeze CIP-68 either, whose metadata is a datum
at a UTxO and changes whenever that UTxO is spent.

Plutus minting policies cannot be classified this way at all, because future behavior is not
statically determinable from the script. The corpus measured here is native-script only, so the share
of mainnet assets sitting under Plutus policies is unmeasured. This document does not assume it is
small.

**This plan stores all three channels, and the tiering sets each one's refresh policy separately.**
Registry rows carry `source = 'registry'` and the seven-day window, because closure does not freeze
the registry record. A CIP-25 row carries `source = 'chain'` and the mint block's slot, and under a
closed policy it is fetched once and never re-read, which is the fetch-once model locked decision 9
rests on. A CIP-68 row carries `source = 'chain'` as well and takes the seven-day window, because a
datum changes whenever its UTxO is spent and closure says nothing about that. No closure column is
added: closure is read from the policy script already in hand, where the two-byte era wrapper says
whether a lock can be expressed at all and `invalid_hereafter` says whether it has passed.

### UI / Store / Process Changes

#### Goal one: intelligent names

The name resolution order becomes: registry `ticker`, then registry `name`, then the chain name from
a CIP-25 or CIP-68 record, then the decoded asset name if every decoded byte is printable ASCII,
then nothing. The fingerprint is already rendered separately and is the fallback identity.

The printable-ASCII predicate is new and is the whole of the fix at
`components/assets/Asset.tsx:177-178`: decode `asset_name` from hex, and accept it as a name only if
every byte is in the range `0x20` to `0x7E`.

**The decoded name keeps a marking that separates it from a published name.** Deleting the literal
`ASCII: ` prefix outright, on the grounds that it labels a fact the user has no use for, is a
security regression and not a cosmetic change. Asset names are
chosen freely by whoever mints, so an attacker mints an asset whose name bytes spell `USDC` and,
with the prefix gone, it renders into the same `displayName` span as a genuine registry `USDC`,
distinguishable only by the fingerprint. Today the prefix is the only thing that marks the decoded
bytes as raw asset-name bytes rather than something an issuer published.

The prefix itself is not worth keeping; the distinction is. A minter-chosen decoded name is rendered
in a visually distinct treatment from a registry or CIP-25 name, and never in a form that reads as a
published name. The exact treatment is a design decision; the requirement is that a user can tell
the two apart without reading the fingerprint. This applies to every surface that renders a name,
not only the token row.

Removing the marking would also invalidate an assumption the CIP-30 connector work is written
against. That document currently records as a limitation that "an
unregistered asset arrives with no name and no ticker, which is a display problem rather than an
impersonation one". That is true only while the marking exists. Without it, the impersonation
surface extends from the registry corpus to the entire asset universe, and the connector inherits
the change with no reason to re-check it.

This lands first and alone, per locked decision 6. It needs no network, no cache, no verification
and no IPC, and it is the largest visible improvement in the plan per line changed.

Three gaps remain after it. Asset names that are valid UTF-8 but not ASCII stay hidden under this
predicate, which is the conservative choice while nothing renders them safely. Names that exist only
in CIP-25 or CIP-68 metadata wait for the chain channel. Fungible tokens whose only human-readable
identifier is a registry ticker get nothing until the cache lands, which is the rest of this plan.

#### Goal two: optimistic decimals when verified

The value used to format and to parse an amount resolves in this order:

1. An explicit user setting for that subject, from browser storage, unchanged from today.
2. The cached registry value, if and only if `verified = 1`.
3. None. Amounts are shown and entered in the raw units the chain holds.

An unverified registry value never formats anything. It stays available to the asset settings dialog
as the recommended value, which is exactly what it is today.

**How often this fires, measured over the whole corpus.** Every mapping file in the registry at
commit `363982b9`, 2026-09-14, 7,977 files, run through the attestation chain above: strip the era
wrapper, hash the native script to the subject's policy id, evaluate the script against the key
hashes that attested the `decimals` property, and verify the ed25519 signature with Node's built-in
`crypto.verify`.

| decimals published | verification outcome | count | share |
|---|---|--:|--:|
| 0 | no `policy` field | 3,027 | 37.9% |
| 0 | verified | 2,782 | 34.9% |
| none | policy present, no decimals | 947 | 11.9% |
| **nonzero** | **verified** | **850** | **10.7%** |
| nonzero | no `policy` field | 320 | 4.0% |
| none | no `policy` field | 51 | 0.6% |

4,579 of the 7,977 entries carry a `policy` field, 57.4 percent. Every one of them decoded as a
native script, hashed to its own subject's policy id, and verified: zero decode failures, zero hash
mismatches, zero keys the script did not require and zero signature failures across the whole
corpus. Both `8200` and `8201` wrapper forms hashed correctly. So the corpus the implementation
will be gated against verifies cleanly under the chain this document specifies, measured rather than
projected from a sample. The gate in the Non-Functional Requirements still applies to the
implementation, which has to reproduce this result.

The operative row is the fourth. **Only 10.7 percent of registry subjects have their displayed and
entered amounts change.** Everything else is either already raw or unverifiable. Set against the
11,177,037 distinct mainnet assets recorded in the research note (a planner-estimate count
retrieved 2026-09-11), the registry's subjects are 0.07 percent of the asset universe, so the share
of assets a typical user actually holds that gets verified decimals is smaller again and is not
measured here.

The two most expensive tasks in the plan sit on this path, and the send-path safety rules exist
entirely to serve it. The tokens that publish nonzero decimals are the ones where raw units are most
confusing, and the alternative is the status quo where the correct value is on screen under the word
"recommended" and does nothing.

**The risk in this migration is on the input path, not on the balance.** Because the ledger quantity
is an integer and decimal places are presentation, applying a verified decimal count changes only how
an unchanged integer is drawn. No balance moves, and nothing is spent or received differently.

What flips at the same moment is how the send field is interpreted. Before the migration a user sends
one and a half of a six-decimal token by typing `1500000`, because the field is in raw units. After
it, the same user types `1.5`. Someone acting on habit who types `1500000` afterward sends a million
and a half tokens, and that direction is an overspend, gated only by their balance. It is the mirror
of the separator defect above, and the larger of the two: an underpayment is bounded by what the user
meant to send, and this is bounded only by what they hold.

So the care goes on the input path during the transition. The amount field is labeled with the unit
it is now accepting, and the one-time notice leads with what changed about entering an amount rather
than with displayed balances. The notice is a dismissible banner on the token list, shown on first
run after the update, once per user and not per asset, and it says two things: amounts for verified
tokens are now entered and shown in the issuer's published units, and the per-token setting still
overrides them. Dismissal is recorded in browser storage alongside the other per-profile flags. A
user holding no tokens never sees it.

The send confirmation is a partial backstop and not a sufficient one. For that mistyped amount it
would show a formatted `1500000.000000` against a raw `1500000000000`
(`DialogContentWithAssets.tsx:75` and `:109-110`), which a user who reads it would catch. It is the
last screen before a signature, so it should not be the only thing standing between a habit and a
transaction.

**The disagreement case already has a renderer and needs one more input.**
`components/wallet/tokens/wallet-token/helpers.ts:6-24` compares `decimals` against
`recommendedDecimals` and is consumed at `WalletToken.tsx:52-55` and `AssetSettingsDialog.tsx:157`,
with its behavior pinned by `helpers.spec.ts`. It gains a third argument, whether the recommended
value was verified, because the two cases carry different copy. An explicit setting that contradicts
a policy-bound issuer value gets the direct wording; one that contradicts an unattested value gets
the weaker wording, since the existing copy at `assets.warning.notUsing` overstates that case. The
argument object is extended rather than a second helper added, and `helpers.spec.ts` is extended in
the same change.

#### Goal three: the advisory when verification fails

One place, one sentence: the asset settings dialog
(`components/assets/AssetSettingsDialog.tsx`), beside the decimal places field, saying that the
issuer's published decimal places for this token could not be verified against its minting policy
and are therefore not applied automatically. Nowhere else.

Not on the token row. The row already carries a warning icon at `Asset.tsx:215-228` for
decimal-setting disagreement. A second per-row badge for unverified metadata
would appear on a large share of rows, because a large share of registry subjects carry no policy
field at all, and a badge that appears on most rows is decoration rather than a warning. The signal
the user already gets is the absence of formatting: the amount shows in raw units, which is the
honest rendering of a number nobody can vouch for.

The settings dialog is the one screen where the user is being asked to make a decision about that
number, which is where the sentence belongs.

New strings follow the repository convention: ids shaped `namespace.context.messageKey`, every
message carrying a `description`, and every new `defaultMessage` prefixed `!!!`. `yarn i18n:manage`
is a required check.

### The cold cache and the send path

**The token list has nothing to wait for.** Holdings arrive on the wallet object, so every row
renders immediately with its locally computed fingerprint, its quantity in raw units, and its
decoded name where the bytes are printable. Tickers and formatted quantities appear as the cache
resolves, through the observable map the update channel feeds. There is no state in which the list
is empty because metadata has not arrived.

This deletes the whole-list spinner. `WalletTokensList.tsx:135-140` replaces the entire list with
one large spinner whenever `isLoadingAssets` is true, so a single unresolved token blanks every
resolved one. The condition becomes structurally unreachable once holdings and metadata are
separate, and the prop, the condition and the spinner branch all go.

`isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets` is computed at four sites, not one:
`WalletTokensPage.tsx:46`, `WalletSummaryPage.tsx:129`, `WalletSendPage.tsx:148` and
`WalletTransactionsList.tsx:244`. All four read `totalAssets` off the cache-derived list this plan
deletes, so all four go together or the branch does not compile. Each has a consumer that takes the
prop and renders on it: `WalletTokens.tsx` and `WalletTokensList.tsx`, `WalletSummary.tsx`,
`WalletSendForm.tsx`, and `Transaction.tsx:666`.

**One merge helper has to change, or the cold list is empty.**
`utils/assets.ts:122-140` builds a merged row by taking `uniqueId` from the asset lookup:

```ts
const { fingerprint, metadata, decimals, recommendedDecimals, uniqueId } =
  getAsset(policyId, assetName) || {};
```

and `getNonZeroAssetTokens` at `:142-149` then filters on `!!token.uniqueId`. So a token with no
cached metadata has no identity and is dropped, even though the token itself carries `uniqueId`
(`api/assets/types.ts:55`, set at `api.ts:3218` and `:3229`). Under the poll this was masked,
because the poll populated the map before anything rendered. With a cache that starts cold it would
empty the send form's asset list, the send confirmation and the transaction list. The fix is to take
`uniqueId`, `policyId`, `assetName` and `assetNameASCII` from the token and only `metadata`,
`decimals` and `recommendedDecimals` from the lookup, after which the filter has nothing left to do
and goes. `fingerprint` comes from the local CIP-14 computation.

**That fix covers the token list and not the transaction list.** `api.ts:3214-3232` holds the
wallet-balance mappings. The transaction path at `api.ts:3321-3329` builds its assets with only
`policyId`, `assetName`, `quantity` and `address`: no `uniqueId`, no `assetNameASCII`. So the merge
helper must derive `uniqueId` as `policyId + assetName` and `assetNameASCII` by decoding, rather
than reading either off the token, for any row originating in a transaction.

**Two further consumers lose historically-held assets when the endpoint goes.**

- **CSV export covers more than the rendered page.** `TransactionsStore.ts:390-408` passes
  `allFiltered`, every transaction matching the current filter rather than the rendered page, to
  `transactionsCsvGenerator`, which calls `getAsset(policyId, assetName)` at
  `transactionsCsvGenerator.ts:190` and falls back to the literal `'unknown fingerprint'`. Today
  `getAsset` is backed by the full-history endpoint, so an asset the wallet once held still
  resolves. After the change it resolves only if it happens to be cached. Subject derivation
  therefore includes the subjects of `allFiltered` at export time, resolved before the file is
  written, and `'unknown fingerprint'` is replaced by the locally computed CIP-14 fingerprint, which
  needs no cache at all.
- **`assets.all` has a third consumer.** `WalletSendPage.tsx:134` destructures `all: allAssets` and
  `:137` resolves the clicked token via `getAssetByUniqueId(activeAsset, allAssets)` at `:115-117`.
  If `all` is derived from the cache, a token with no cached row is absent and the send form opens
  without its asset preselected. `all` is therefore derived from holdings, not from the cache, in
  the same way the token list is.

**The send path degrades to raw units.** An unresolved row is cosmetic everywhere else and an
input-validation problem here. The defect it addresses is live today and independent of this work,
and its severity is set out in the Problem Statement: raw units are the honest display, so the fix is
to stop the field accepting a character it will then strip.

When the resolved decimal places for an asset are unknown, `AssetInput.tsx` passes
`allowOnlyIntegers` to the `NumericInput`. That selects the integers-only input pattern at
`node_modules/react-polymorph/lib/components/NumericInput.js:197` and `:199`, so a decimal separator
cannot be typed at all. The field is labeled with the token's ticker or fingerprint rather than a
formatted-amount placeholder. The field then reads and writes the same integer the chain holds,
which is exactly correct and needs no warning beyond the label. The keypress guard at
`AssetInput.tsx:128-137`, which today fires only when `decimals === 0`, becomes redundant and goes
with it.

It does not block and does not refuse, per locked decision 7. A user holding a token the registry
has never heard of must still be able to spend it, and raw units are the units it is actually
denominated in. Blocking would make an unverifiable token unspendable, which is a worse outcome than
the one it guards against.

This fix lands in the first phase, alongside goal one, ahead of everything else in the plan.

**Two safety rules govern a resolution that lands while the user is typing.** These are load-bearing
and are the condition on which optimistic decimals apply at all: both hold, or the send path
stays in raw units.

The amount that gets signed is a pure function of the display string, with no record of the
denomination it was typed in. `WalletSendForm.tsx:231-236` computes `selectedAssetsAmounts` as
`formattedAmountToNaturalUnits(assetFields[uniqueId].value)`, and that helper
(`utils/formatters.ts:156-169`) works by deleting `.`, `,` and whitespace from the string.
`AssetInput` is an `@observer` reading `decimals` live from `getAssetByUniqueId(uniqueId)`
(`AssetInput.tsx:77-83`), and under this design that value arrives asynchronously over the update
channel. So the denomination of an open field can change under the user mid-edit.
react-polymorph's `componentDidUpdate` (`NumericInput.js:153-162`) corrects only the caret, so the
string is not rewritten at the moment of the flip, but the next keystroke runs `processValueChange`
and routes through `bigNumberToFixed` at `:388-394`. A field holding `1500000` typed as raw units,
resolved to six decimals, then touched once, becomes `1,500,000.000000` and submits
`1500000000000`.

This is not the release-boundary case the Rollout section describes. It is the normal first-run
case, and it recurs every time a user acquires a token the cache has not yet seen.

1. **Snapshot `decimals` per asset row when the row is added to the form.** The row renders and
   validates against the snapshot, not against the live observable. A resolution arriving for an
   asset whose row is already open does not silently change the meaning of what is on screen.
2. **If a resolution changes `decimals` for an asset whose field is non-empty, clear the field and
   show a blocking notice on that row.** The notice names the token and states that its denomination
   changed, and the amount must be re-entered. Clearing is the only safe action: the typed digits
   are ambiguous once the denomination moves, and neither interpretation can be assumed. If the
   field is empty, the snapshot updates silently and nothing is shown.

Both rules apply equally when `decimals` moves from unknown to known and when a cached verified
value changes on re-read. Both are covered by the negative cases in the Testing Strategy.

### Components Affected

- `source/main/assets/`: new. The database module, the registry client, the verification module, the
  resolver that sequences them, and the image store.
- `source/main/ipc/assetMetadataChannel.ts`: new, plus one registration line in
  `source/main/ipc/index.ts` beside `handleGovernanceAnchorRequests()` at `:49`.
- `source/renderer/app/ipc/assetMetadataChannel.ts`: new.
- `source/common/ipc/api.ts`: three channel constants and their request and response types.
- `source/common/types/asset-metadata.types.ts`: new.
- `source/renderer/app/stores/AssetsStore.ts`: the poll and the request machinery are deleted;
  `details` and `getAsset` keep their signatures and become reads of an observable map fed by the
  update channel.
- `source/renderer/app/api/api.ts`: `getAssets` at `:783-820`, `storedAssetMetadata` at `:362`,
  passed at `:805` and written at `:3391`, and `_createAssetFromServerData` at `:3371-3404` are
  deleted.
- `source/renderer/app/api/assets/`: `requests/getAssets.ts` and `requests/getUnknownAsset.ts`
  deleted; `types.ts` loses `ApiAsset`, `ApiAssets`, `GetAssetsRequest`, `GetAssetsResponse` and
  `GetUnknownAssetRequest`.
- `source/renderer/app/utils/assets.ts`: `getAssetTokenFromToken` at `:122-140` takes identity from
  the token; `getNonZeroAssetTokens` at `:142-149` loses its filter; `getAssetToken` at `:74-97`,
  `getAssetTokens` at `:106-113` and `getUniqueId` at `:301-307` are deleted.
- `source/renderer/app/utils/assetFingerprint.ts`: new, CIP-14.
- `source/renderer/app/components/assets/Asset.tsx`: name resolution and the printable-ASCII
  predicate.
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`: the unverified advisory.
- `source/renderer/app/components/wallet/tokens/wallet-token/helpers.ts` and `helpers.spec.ts`: the
  third input.
- The `isLoadingAssets` removal spans four computation sites and six consumers.
  `source/renderer/app/containers/wallet/WalletTokensPage.tsx:46`,
  `.../WalletSummaryPage.tsx:129`, `.../WalletSendPage.tsx:148` and
  `source/renderer/app/components/wallet/transactions/WalletTransactionsList.tsx:244` compute it;
  `.../tokens/wallet-tokens-list/WalletTokensList.tsx`, `.../wallet-tokens/WalletTokens.tsx`,
  `.../wallet-no-tokens/WalletNoTokens.tsx`, `.../summary/WalletSummary.tsx`,
  `.../WalletSendForm.tsx` and `.../transactions/Transaction.tsx` take the prop and render on it.
  `WalletSendForm.spec.tsx:122` passes it and is updated with them.
- `source/renderer/app/containers/wallet/WalletTokensPage.tsx` and `WalletSummaryPage.tsx`: both move
  from `getAssetTokens(all, walletTokens)` to the holdings-driven `getNonZeroAssetTokens(tokens,
  getAsset)` already used at `WalletSendPage.tsx:144`,
  `dialogs/send-confirmation/SendConfirmation.container.tsx:47` and
  `components/wallet/transactions/WalletTransactionsList.tsx:240`. `WalletSummaryPage.tsx:123-125`
  re-sorts afterward, so the fingerprint sort inside the holdings-driven helper is harmless.
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`: the integers-only path.
- `source/main/config.ts`: `FALLBACK_TOKEN_METADATA_SERVER_URL` at `:167-168` deleted. It is
  imported nowhere, and it does not even name the same host as the real fallback at
  `source/main/index.ts:217`.
- `source/renderer/app/config/assetsConfig.ts`: the preset list, the source types and the URL
  validator.
- `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.ts`: new, the `/tip`
  probe.
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.tsx` and its `.scss`:
  new, in the shape of `StakePoolsSettings.tsx`.
- `source/renderer/app/containers/settings/categories/AssetMetadataSettingsPage.tsx`: new, in the
  shape of `StakePoolsSettingsPage.tsx`.
- `source/renderer/app/routes-config.ts:46-55`, `source/renderer/app/Routes.tsx:136-140`,
  `source/renderer/app/components/settings/menu/SettingsMenu.tsx:39-44` and its `.messages.ts`: one
  entry each for the new category.
- `source/renderer/app/api/utils/localStorage.ts`: the stored source selection, three lines beside
  `getSmashServer` at `:328-333`.
- `source/renderer/app/utils/assets.ts`: `getAssetMetadataSourceIdFromUrl`, the same reduction as
  `getSmashServerIdFromUrl` at `utils/staking.ts:17-28`.
- `source/main/assets/`: the Koios client and the local pointer check join the registry client, the
  verification module and the resolver.
- `source/main/config.ts` and `source/main/preload.ts`: `koiosUrl` beside `smashUrl` at `:70` and
  `:40`.
- `nix/internal/launcher-config.nix`: a per-network `koiosServers` attribute beside `smashServers`
  at `:31-35`, assigned beside `smashUrl` at `:456`.
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json` and `translations/messages.json`:
  regenerated.
- `.agent/workflows/ipc.md`: corrected, see below.

### Documentation divergences found

- `.agent/workflows/ipc.md` labels the renderer-to-main hop `ipcRenderer.send / invoke` and the
  main-side hop `ipcMain.handle / on`. Neither `ipcRenderer.invoke` nor `ipcMain.handle` appears
  anywhere in `source/`, and `ipcRenderer.send` appears twice, both outside the channel mechanism.
  The real mechanism is `IpcChannel` deriving three names from one base at
  `source/common/ipc/lib/IpcChannel.ts:91-93`.

## Implementation Strategy

1. **Land the standalone correctness fixes first.** Names, the integers-only amount field, the
   merge-helper identity fix, and the three dead symbols. No cache, no IPC, no new dependency, and
   each one is separately revertible.
2. **Build the main-process cache with no consumer.** Fingerprints, schema, registry client,
   verification, resolver, image store and eviction. Verification is the piece with the real
   uncertainty, and it is built and gated on its own.
3. **Wire the IPC and rewire the store.** Channels, handler, renderer clients, then `AssetsStore`,
   then the two enumerating call sites, then the removal of the endpoint and the poll. The poll is
   removed last within this phase, so the cache is proven to feed the store before the old source is
   taken away.
4. **Turn on optimistic decimals.** Resolution order, the extended disagreement helper, the one-time
   notice, and the settings-dialog advisory.
5. **Clean up the cold-cache rendering and add the logo.** Spinner removal, the image surface, and
   the translation pass.
6. **Verify and record.** Specs, the manual matrix, and the documentation corrections.
7. **Add the metadata source setting and the chain channel.** The settings surface first, defaulted
   to the Koios preset, with the direct option present in the enum and rendered unavailable. Then
   two-call pointer resolution, then the local confirmation against the user's own node, then
   ingestion. This phase is additive throughout: every phase before it works, and keeps working,
   with the registry alone.

## Testing Strategy

**Jest** carries nearly all of it, because nearly all of it is pure functions over fixed inputs.

- CIP-14 fingerprints against the eight golden vectors at
  `TokenFingerprintSpec.hs:40-78`, which cover the empty asset name, a short name and a 32-byte name.
- The printable-ASCII predicate against printable, empty, high-byte and invalid-UTF-8 asset names.
- Policy binding and key binding against the live-captured subject in the Technical Design, plus
  negative cases: a mismatched policy, a signature from a key the script does not require, and a
  missing `policy` field.
- Attestation payload construction and ed25519 verification, including a tampered value, a tampered
  sequence number and a signature that is valid for a different property.
- Decimals resolution across all combinations of user setting present or absent, cached value
  present or absent, and verified true or false.
- `isNonRecommendedDecimalSettingUsed` with the third input, extending the existing
  `helpers.spec.ts`.
- The merge helper with a cold lookup, asserting that a token with no cached row survives with its
  identity intact.
- The database module against a temporary file, asserting the `CHECK` constraints reject a row that
  claims both a sequence number and a slot, and that eviction respects both bounds.
- The metadata source URL validator against the Koios default with its `/api/v1` prefix, a custom
  instance, an `http://` URL, a URL carrying a query string, and the literal `direct`.
- `getAssetMetadataSourceIdFromUrl` against the preset URL, an unrelated URL and the empty string,
  mirroring the cases the SMASH reduction already has.
- Pointer resolution over a recorded `asset_info` and `tx_cbor` pair, and the three local checks
  against those bytes, with negative cases: tampered auxiliary data, a mint field naming a different
  policy, and a transaction hash that does not reproduce.
- A chain row asserting `decimals` is NULL and `verified` is 0, so the decimals resolution order is
  provably untouched by that channel.

**Corpus validation**, not a unit test and not in CI: verification is run over the full registry
corpus and must report zero failures across the policy-bound subjects before the verification tasks
are accepted. This is the gate in the Non-Functional Requirements.

**Manual QA**, because none of the above exercises the process boundary:

- First run with an empty database, a wallet holding several tokens, offline. Rows render with
  fingerprint and quantity, nothing spins, nothing errors.
- The same wallet online. Tickers and formatted amounts appear without a reload.
- A token with verified decimals, a token with unverified decimals, and a token the registry does
  not know, side by side in the list and in the send form.
- The send form for a token with unresolved decimals: the decimal separator cannot be typed, by
  keyboard or by paste.
- The migration notice on first run after the update, and its absence on the second.
- Delete the cache directory while Daedalus is running, then reopen the token list.
- The three source options in the settings page: the preset selected by default, a custom URL that
  answers `/tip` and is accepted, a custom URL that does not answer and is refused with the error
  rendered, and the direct option present and unavailable.
- An NFT the registry has never heard of: its fingerprint before the chain channel resolves it, its
  CIP-25 name afterward, and its amount still in raw units throughout.

**Cucumber `@unit` is available and is used for the send-path guards.** It is in the CI check set:
`perSystem/checks.nix:48` is `cucumber-unit = mkJsCheck "daedalus-cucumber-unit" "yarn test:unit"`,
and `package.json:23` shows `test:unit` is the Cucumber `@unit` suite. It is gated on every system
except `x86_64-darwin`.

The two send-path safety rules are the highest-consequence behavior in this plan and the hardest to
assert from a unit test, because they are about a value changing under an open form. They get
`@unit` scenarios: a field holding a raw-units amount when a resolution arrives, asserting the field
is cleared and the notice shown; and a field left empty when a resolution arrives, asserting it is
updated silently.

The **end-to-end** suite remains unusable: `spectron@14` resolves `electron-chromedriver@12` against
Electron 41.3.0, 23 of 48 feature files are disabled at feature level, and the e2e suite is not in
the check set.

**Platform verification** is needed for the database path on all three platforms, because
`stateDirectoryPath` differs per platform and the Windows path is the one no contributor exercises
daily. The test suites run on Linux and on aarch64-darwin, and the static checks on `x86_64-linux`
alone, so no CI job executes on Windows at all.

## Rollout / Migration / Rollback

**No feature flag.** The cache is either present and answering or absent and answering nothing, and
both states render correctly. A flag would add a code path that only the flag's off position
exercises.

**Forward migration is the one-time notice** described under goal two. There is no data migration:
per-user decimal settings in browser storage are read exactly as they are today and keep winning.

**Backward compatibility of the database** is handled by a `user_version` pragma checked on open. A
database written by a newer version than the running code is deleted and recreated rather than read,
because the file holds nothing that cannot be fetched again.

**The metadata source setting has a default and no migration.** A profile with no stored selection
reads the launcher configuration's Koios URL, which is what `StakingStore` already does for SMASH
at `source/renderer/app/stores/StakingStore.ts:199-224`. Nothing is written on upgrade.

**Rollback** is per phase. Phase 1 is four independent commits, each revertible alone. Phases 2 and
3 must roll back together once the poll is gone, since reverting the store without restoring the
endpoint client would leave the renderer with no source of metadata. Phases 4 to 6 are revertible
individually and leave a working cache behind. Phase 7 reverts on its own: registry rows are
untouched, chain rows stay on disk and stop being read, and deleting the cache directory clears
them.

**If Node's verifier turns out not to be strict enough** on some platform build, the redirect is to
promote `@noble/curves` from a transitive dependency to a declared one and use its ed25519
implementation, which rejected the malleable signature in the same test. The rest of the design is
unaffected, because the primitive sits behind one module.

## Open Questions

Three questions this document used to carry are closed. The attestation payload construction is
resolved and recorded in the fetch and verify path, verified live against five properties of a
mainnet subject. Strict ed25519 is resolved by using Node's built-in `crypto.verify` rather than
`cardano-crypto.js`, which accepts a malleable signature. Whether the logo surface earns its column
is closed by `se7en-labs-inc/daedalus#35`, which asks for images directly, so the table and the
image channel stay.

1. **The local chain reader is a build in its own right, and its blind spot is now stated rather
   than open.** Phase 7's confirmation step reads the block at the pointer out of the user's own
   chain database. Daedalus has no Ouroboros client and no chunk parser; `chainStorageValidate.ts:69`
   is the only place that even names the directory. The immutable database excludes the last k
   blocks, which live in `volatile/`. On mainnet k is 2,160 blocks at roughly 20 seconds average
   block time, so the window is on the order of **12 hours**.

   The resolution taken: an asset minted inside that window resolves late rather than not at all. Its
   row is not written, `asset_resolution` records `pending` with a `retry_after` inside the window,
   and the subject resolves on a later demand. Until then the asset shows its decoded name, marked as
   minter-chosen, and its fingerprint. Reading `volatile/` would close the window and is deliberately
   not in scope; it is a different on-disk format and would be its own task.

   The common case is the bad one: a freshly minted NFT is exactly the asset a user has just
   acquired and most wants to see named, so for NFTs the late path is the usual path rather than
   the exception. It remains the largest build in the plan and the place an estimate is most likely
   to be wrong.

2. **The CBOR decoder requirement relaxes a stated constraint.** The confirmation step needs a
   decoder that can hand back the original byte range of a decoded item, which neither `cbor@5.2.0`
   nor `borc@2.1.2` exposes. The task graph already relaxes the "no new runtime dependency"
   requirement to "none beyond a CBOR decoder" inside an acceptance criterion. That is a PRD-level
   decision taken in the wrong place. Name the decoder, or drop the byte-range requirement and say
   what replaces it.

3. **Does this cache serve the CIP-30 connector, or does the connector get its own path?** Three of
   this document's decisions are incompatible with the connector work as currently written: the
   scope requirement resolves for holdings and transaction-list subjects "and for nothing else",
   while the connector must resolve subjects the user has never held; the IPC model never waits on
   the network, while the connector requires resolve-before-render with a hard timeout; and the
   advisory is specified to live in the asset settings dialog and "nowhere else", while the
   connector needs an unresolved marker at confirmation time. None is expensive to change now. All
   three are expensive after phase 3. A shared answer also needs a row cap and an eviction policy on
   `asset_metadata`, which has neither, because connector traffic writes attacker-chosen subjects
   into a table currently bounded only by what the user holds.

4. **Does `verified` mean the row, or the `decimals` property?** It is defined per property and
   stored as one boolean per row. If a row's `ticker` verifies and its `decimals` carries no
   signature, does that row apply decimals automatically? No entry in the 600-subject sample mixes
   signed and unsigned properties, so this is theoretical today. It still needs one sentence in the
   schema section before implementation.

5. **Six rules are named but not yet specified here, and several are correctness rules.** A
   cross-check found these missing from the design, in rough order of consequence: the rule for what
   happens when two channels disagree on a single field, with a worked counterexample; the
   sort-stability rule for the token list once names start resolving asynchronously; the `pick`-list
   trap at `domains/Asset.ts:39-46`, where a field absent from the list is silently dropped; the
   CIP-25 requirement that a mint quantity be positive; the CIP-25 `version` key; and the
   v1-versus-v2 key encodings, ordered by `(slot, tx_index)`. Each needs a paragraph in the Technical
   Design before the phase it belongs to starts, rather than being rediscovered during
   implementation.

6. **Terminology needs one shared catalog with the connector work.** "unresolved" means
   undecidable decimals here and absent-from-cache there. "degraded state" is an input rule here and
   a display rule there. "channel" means both an IPC channel and a metadata channel within three
   paragraphs of this document. "source" is used here for a schema column and for a user-facing
   setting, which are not the same thing. A shared string catalog across the two will mis-fire
   until this is settled.

## Status Log

Append-only. Per `.agent/plans/readme.md`, every new entry goes at the end of this
section, in date order. A prior entry that turns out to be wrong is corrected by a
new entry saying so, never by rewriting it.

### 2026-09-10: Plan written

### 2026-09-11: Updated after the Koios research note

### 2026-09-14: Revised after review, before any implementation

Revised against the repository, cardano-wallet and the live registry, before any
implementation started.

Decisions changed:

- Optimistic decimals on the send path are adopted, conditional on two safety rules
  that an earlier draft of this document had dropped: snapshot `decimals` per
  asset row when the row is added, and clear the field with a blocking notice if a
  resolution changes `decimals` while the value is non-empty.
- Scope re-briefed against `se7en-labs-inc/daedalus#35`, which asks for images and a
  CIP-25 and CIP-68 fallback directly. The earlier brief placed those out of scope.
  Phase 7 and the image table stay, and whether the logo surface earns its column
  is closed by the request itself.
- The `ASCII: ` prefix is not simply deleted. It is today the only marking that
  separates a minter-chosen decoded name from a published one, and removing it would
  let an asset whose name bytes spell an existing ticker render as that ticker.

Defects corrected: batching is sized in request bytes with a 6 KB ceiling, because
the endpoint caps the body near 8,192 and 100 worst-case subjects is about 12.3 KB;
4xx is never retried; the three IPC channels carry a `requestId`, because
`IpcChannel` resolves on the next response rather than the matching one; subjects the
registry silently omits are recorded as `unregistered` rather than re-asked forever.

Open questions closed: the attestation payload construction is resolved and verified
live against five properties; strict ed25519 is resolved by using Node's built-in
`crypto.verify`, which rejects a malleable signature where `cardano-crypto.js` accepts
it, and which adds no dependency.

Claims corrected: the `windowOptions` evidence for main-process placement cited a dead
export and is removed, with the placement standing on its other two grounds; the
selfnode claim was inverted, and the fetcher now needs an explicit selfnode case; the
Cucumber premise was wrong, `cucumber-unit` is CI-gated and now carries the send-path
scenarios; the key-hash verification rule is replaced by the registry's script
evaluator, including that time locks evaluate to true unconditionally; three missed
consumers of the assets endpoint are named; privacy is stated for the first time.

Measured and now recorded, over every mapping file in the registry at commit
`363982b9` rather than over a sample: 10.7 percent of registry subjects publish
nonzero decimals and verify, so that is how often the decimals feature changes
what a user sees. 4,579 of 7,977 entries are policy-bound, 57.4 percent, and the
whole corpus verifies with zero failures of any kind.

No implementation has started.

### 2026-09-15: Phases 1 to 6 built

This entry corrects the one above, which said no implementation had started, and
fills a gap: phases 1 to 4 landed without an entry here.

Phases 1 to 6 of the task graph are complete except for the manual QA, which is
`blocked` on an operator with all three platforms. Thirty of the forty tasks are
`completed`, one is `blocked`, and the nine that remain are phase 7, the metadata
source setting and the chain channel.

What the six phases produced, against this document:

- The four standalone correctness fixes, including the raw-units amount field
  that used to accept a decimal separator and strip it.
- The main-process cache: the database, the registry client, policy and key
  binding, attestation verification, the resolver, and the bounded image table.
- The three channels, the store rebuilt on them, and the removal of the
  wallet assets endpoint and its one-minute poll.
- Verified decimal places applied on their own, with the three mitigations this
  document required: the unit label under the amount field, the per-row
  denomination snapshot that clears rather than reinterprets, and the one-time
  notice.
- The whole-list spinner and its condition, both now structurally unreachable,
  and the asset logo, which gives the image table its reader.
- Coverage across both processes, and a manual QA procedure.

Corrections to this document, recorded rather than edited in:

- Four messages added by phases 4 and 5 interpolated a count into an unpluralised
  noun and rendered "1 decimal places". They now take the ICU plural form. Two
  older messages beside them have the same defect and keep it, because changing an
  English default silently invalidates the Japanese translation for that id.
- The Testing Strategy's Manual QA list is missing one scenario, and it is the
  scenario that catches a silent failure: on selfnode the metadata endpoint is
  chosen in code, because the launcher configuration supplies one for every
  network except selfnode, and a wrong choice there produces a well-formed answer
  from the public registry. It is in the manual QA procedure with a discriminator.
- The `IpcChannel` correlation defect this document names is now also corrected in
  `.agent/workflows/ipc.md`, along with eleven other divergences between that
  document and the code, including a table of seven channels that do not exist.

### 2026-09-16: Phase 7 built

The metadata source setting and the chain channel are complete. Thirty-eight of
the forty tasks are `completed` and two are `blocked` on an operator with all
three platforms, which are the two manual QA passes.

What phase 7 produced:

- A settings category in the shape of the SMASH one, with a live probe against
  the candidate instance's `/tip` before a URL is stored, and two refusals that
  read differently: not an instance, and behind your node.
- A pointer client that resolves a batch in two requests, trims its responses,
  holds a request ceiling of a fifth of the published rate limit, and leaves rows
  absent on every refusal.
- The local confirmation, which is what makes the index an index. Three checks
  over the bytes and one against the block in the user's own immutable database.
- Chain rows, with `decimals` NULL and `verified` false on every one of them, and
  a name resolution rung between the published name and the decoded one.
- Per-channel freshness: a CIP-25 record under a policy that can never mint again
  is read once for the life of an installation.

Corrections to this document, recorded rather than edited in:

- **Open question 2 is closed.** The confirmation needs a decoder that hands back
  the original byte range of a decoded item, and neither `cbor@5.0.2` nor
  `borc@2.1.2` does. No runtime dependency was added. A structural reader is in
  the repository at `source/main/assets/cborSpan.ts`: it walks the structure,
  measures it and returns offsets, and the caller reads values from those offsets.
  Its failure mode is one-directional, because every use of it sits inside a check
  whose failure writes no row, and the byte ranges are needed in three places
  rather than one.
- **The endpoint ordering at the fetch and verify path is wrong.** It puts the
  configured source setting second in the registry fetcher's order, behind
  `launcherConfig.metadataUrl`, which is present on every network but selfnode, so
  the setting could never apply. The setting does not feed the registry fetcher at
  all: the two are different endpoints speaking different protocols. It supplies
  the pointer channel's base URL and nothing else.
- **`checkSmashServerHealth` is not a probe of the candidate URL.** It passes that
  URL to cardano-wallet as a query parameter and asks cardano-wallet to check it.
  There is no cardano-wallet endpoint that will probe a pointer source, so the
  asset probe issues its own request rather than reusing the repository's HTTP
  client, which sends over plain HTTP on selfnode and carries the wallet's client
  certificate.
- **Closure cannot be read from the registry's policy field for a chain row.** A
  chain row exists precisely for a subject the registry does not answer. The script
  is taken from the minting transaction's own witness set instead, which is
  available for exactly these rows and is the script the chain accepted. The
  measured 89.6 percent closure figure is about registry entries and does not
  transfer to this channel, which is unmeasured.
- **The volatile window is measured against the immutable database's own tip**
  rather than against k and a slot length. That needs no per-network constant and
  answers the question being asked, which is whether the database holds that slot
  yet.

---

**Status:** In Progress
**Date:** 2026-09-10, updated 2026-09-11, revised 2026-09-14, phases 1 to 6 built 2026-09-15, phase 7 built 2026-09-16
**Author:** Se7en Labs
