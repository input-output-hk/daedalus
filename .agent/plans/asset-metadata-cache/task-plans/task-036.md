## Task ID and Title

`task-036` — Per-channel freshness: a closed policy freezes CIP-25 and nothing
else.

## Why Chosen Now

`task-035` writes chain rows and every one of them takes the registry's
seven-day window, so a CIP-25 record that can never change is re-read fifty-two
times a year for as long as the asset is held. The fetch-once model the volume
estimate rests on is this rule and nothing else.

## Interaction Mode

`agent_execution`.

## Scope

One tier: a CIP-25 row under a minting policy that can never mint again is read
once and never again. Everything else keeps the window it has.

## Non-Goals

- No column. The verdict goes where the row's other channel-specific data
  already goes.
- No timer, no sweep, no background refresh. The window is consulted on demand
  and this changes only what that consultation answers.
- No change to the registry channel. A registry record can be updated by its
  issuer long after the minting window shuts, so closure never freezes one.
- No Plutus analysis. Future behaviour of a Plutus script is not statically
  determinable, so such a policy is reported open.

## Dependencies

`task-035`.

## The caveat the PRD carries, resolved

The PRD reads closure "from the policy script already in hand", and the script
is in hand there because the **registry** returns a `policy` field. A chain row
exists precisely for a subject the registry does not answer, so that script is
never in hand for the rows this task is about. As written, the rule could not be
applied to anything.

**Resolved: the script is taken from the minting transaction's own witness
set.** A mint has to be witnessed by the script that authorised it, and
`task-034` has already confirmed that transaction against the user's own chain,
so the copy in the witness set is both available and better than the registry's:
it is the script the chain accepted rather than the one a server published.

That also changes the corpus the measurement applies to. The PRD's figures, 89.6
percent of registry entries already closed, are about registry entries. Nothing
here measures what share of **chain-row** policies close, and the shares are
unlikely to be the same: registry entries skew towards fungible tokens under
one-off minting policies, and chain rows are NFTs, which include long-running
open collections. The rule is right either way; the volume claim it supports is
weaker for this channel than the PRD's number suggests, and that is recorded
rather than restated.

## Research Consulted

- `asset-metadata-cache-prd.md:1060-1075`, freshness per channel, the closure
  measurement and the Shelley MultiSig case.
- `asset-metadata-cache-prd.md:968-975`, why CIP-68 cannot be frozen.

## Docs, Workflows, and Skills Consulted

- `source/main/assets/assetVerification.ts`, which already decodes a native
  script and evaluates it, so the closure rule is a second fold over the same
  type rather than a second decoder.

## Live Repo Findings Verified For Planning

1. **The native script decoder already exists and is tested.**
   `assetVerification.ts:decodeNativeScript` handles all six forms with a depth
   cap, and `NativeScript` is the type to fold over. `evaluateNativeScript`
   beside it deliberately ignores both time locks, because it answers a
   different question.
2. **The witness set carries the script, and the recorded transaction proves the
   Plutus case.** For the preprod fixture the witness set has keys 0, 5 and 7:
   a signature, a redeemer and a **Plutus V3** script. There is no native script
   and there cannot be a closure verdict, which is exactly the case the rule has
   to report as open.
3. **Conway wraps a witness-set list in tag 258.** The recorded transaction's
   key 0 value begins `d9010281`, so an unwrap is needed before the list is
   read. Earlier eras write a bare array.
4. **A policy id is blake2b-224 over the script bytes with a leading zero
   byte**, which `nativeScriptPolicyId` already does, and the script's original
   bytes are available as a span.
5. **`_due` has no cheap access to the chain.** It runs on every read, and
   finding the immutable tip means listing a directory holding three files per
   chunk. So the verdict has to be decided when the row is written, where the
   tip is already in hand, and stored.
6. **A metadata column already holds channel-specific JSON.** For a registry row
   it is `{url, description}`; for a chain row `task-035` put the minter's
   payload there directly.
7. **`decodeNativeScript` is realm-sensitive.** It checks
   `keyHash instanceof Uint8Array`, and a `Buffer` produced by `cbor` under the
   jsdom test environment is not an instance of the test realm's `Uint8Array`.
   Every main-process spec that touches it carries `@jest-environment node`;
   `chainPointerVerification.realfs.spec.ts` did not, and has to.

## Files Expected To Change

- `source/main/assets/assetVerification.ts`
- `source/main/assets/assetVerification.spec.ts`
- `source/main/assets/chainPointerVerification.ts`
- `source/main/assets/chainPointerVerification.realfs.spec.ts`
- `source/main/assets/assetMetadataResolver.ts`
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`
- `source/main/ipc/assetMetadataChannel.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**One new fold, beside the one that already exists.**
`nativeScriptLatestSlot` returns the last slot at which a script could still be
satisfied, or null when it never stops being satisfiable. Every uncertainty
resolves towards open, because calling a policy closed stops the record ever
being read again: `n of k` is bounded by its latest member rather than its n-th
latest, an `any` with one unexpiring branch is unbounded, and an empty list is
unbounded.

**The verdict is decided at write time and stored, not recomputed.** Finding 5
is why. `confirmChainPointer` already holds the transaction and the reader, so
it answers `policyClosed` alongside the confirmation.

**The verdict is stored beside the record rather than in it.** A chain row's
metadata column becomes `{ record, closed }`. Putting `closed` next to the
minter's keys would let a minter write a key of that name, and a CIP-25 payload
whose author chose `closed: true` would freeze its own row. The channel unwraps
`record` on the way to the renderer, so the wire shape is unchanged and the
renderer never sees the bookkeeping.

**CIP-68 is never frozen.** A datum lives at a spendable output and changes when
that output is spent, which needs no minting at all, so a closed minting policy
says nothing about it.

**A forced read still reaches a frozen row.** The manual refresh in the asset
settings dialog skips the window and the retry backoff already, and it skips
this too, because it is the only way back for a row frozen in error.

## Acceptance Criteria

1. A CIP-25 row under a policy whose time lock has passed is not re-read.
2. A CIP-25 row under a policy with no time lock is re-read on the next demand
   after the window.
3. A CIP-68 row is re-read after the window regardless of the policy.
4. A registry row for a closed policy still takes the seven-day window.
5. A frozen row is re-read when the read is forced.
6. A Plutus minting policy is reported open, asserted against the recorded
   transaction rather than a synthetic one.
7. No fixed-interval timer is introduced.
8. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
9. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `nativeScriptLatestSlot` is driven as a pure fold, one case per form plus the
  three that decide whether anything is ever frozen: an `any` with an unexpiring
  branch, an `all` of unexpiring members, and an empty list.
- The closure verdict is driven through `confirmChainPointer` against synthetic
  transactions carrying a real native script in their witness set, with the
  policy id derived from the script rather than chosen, so the check has to find
  it the way it will in production.
- Criterion 6 is driven against the recorded preprod transaction, which mints
  under a Plutus V3 script. That is the case a synthetic fixture would not have
  produced by accident.
- Criteria 1 to 4 are driven at the resolver against a row written a fortnight
  in the past, so the window has elapsed and the freezing rule is the only thing
  that can hold a re-read back. Each asserts the request count on the pointer
  transport rather than the row.
- Criterion 5 drives the same frozen row with `force`.
- Criterion 7 needs no case: nothing here schedules anything, and the existing
  "schedules nothing to repeat" case still passes.
- All six Nix checks.

## Risks and Open Questions

- **The share of chain-row policies that close is unmeasured.** See the caveat
  section. The rule holds regardless; the volume estimate it supports is the
  registry's, not this channel's.
- **A frozen row is frozen against a bug too.** If the closure fold is ever
  wrong in the unsafe direction, the row is never re-read and only a manual
  refresh or deleting the cache directory recovers it. That is why every
  uncertain case resolves towards open.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-036.targetPaths` widened to the
  verification module, the confirmation and the channel; the caveat resolution
  recorded in its implementation notes; `task-036.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-036-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-036-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A CIP-25 record that can never change is read once for the life of an
installation, and nothing else changes its refresh behaviour.

## Final Outcome

Complete.

## Self-Review

The task as written could not be implemented: it reads closure from a script
that, for the rows it applies to, is never available. Finding that took reading
where the script comes from rather than accepting that it is "already in hand".
The witness set is a better source than the one the PRD assumed, and it is
available exactly for the rows that need it, which is the kind of coincidence
worth being suspicious of and checking: the recorded transaction proves it by
carrying a Plutus script instead, which the rule then has to report as open.
