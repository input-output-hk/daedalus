## Task ID and Title

`task-033` — Koios client: two-call pointer resolution, in bulk, with responses
trimmed.

## Why Chosen Now

`task-031` stores a base URL and `task-032` lets a user change it, and nothing
reads either. This is the first thing that does. It is also the last piece that
can be built without touching the chain: `task-034` confirms what this returns,
and `task-035` writes it.

## Interaction Mode

`agent_execution`.

## Scope

One module that turns a list of subjects into pointers and the transaction bytes
behind them, in two requests per batch, within a per-process request ceiling,
and leaves nothing behind when it cannot.

## Non-Goals

- **No confirmation.** Nothing this module returns is trusted. `task-034` checks
  the bytes and reads the block; this module's job ends at "here is what the
  index said".
- **No row is written.** The resolver wiring and the write land in `task-035`,
  where there is something confirmed to write. That also carries the selected
  URL across the process boundary, which has no reader until then.
- No `tx_info`. `tx_cbor` returns everything `tx_info` would and the bytes as
  well, so calling both would be a third request for nothing.
- No second scheduler. Batching is inside this module, in the shape the registry
  client already uses, and no interval timer is introduced.

## Dependencies

`task-010`, `task-030`.

## Research Consulted

- `research/01-koios-pointer-option.md:199-300`, what `asset_info` returns, the
  `select=` trimming and the two-call cost.
- `research/01-koios-pointer-option.md:40-80`, the tiering table and the
  operator's HAProxy rate limiting, which is where the ceiling and the `429`
  behaviour come from.
- `research/01-koios-pointer-option.md:232-260`, that Koios strips the registry
  signatures, which is why nothing from this channel can carry `decimals`.
- `asset-metadata-cache-prd.md:930-960`, the disclosure and the two-call design.

## Docs, Workflows, and Skills Consulted

- `source/main/assets/assetRegistryClient.ts` as the shape to follow: a
  transport interface, batching, one retry, a doubling backoff, and resolution
  rows instead of throws.

## Live Repo Findings Verified For Planning

1. **The transport is already written and is not registry-specific.**
   `assetRegistryClient.ts` holds a private `post` and `readResponse`: one
   wall-clock budget, a cap by declared length and by stream length, and a
   result rather than a throw. Nothing in either is about the registry except
   the constant naming the cap.
2. **The cap has to become a parameter.** `ASSET_REGISTRY_MAX_RESPONSE_BYTES` is
   1 MiB, sized from a 103,314-byte registry answer for 86 subjects. A batch of
   raw transactions is an order larger: one measured `tx_cbor` record is 3,251
   bytes and a batch is bounded by subject count. So the transport moves to its
   own module with the cap as a parameter, and `assetRegistryClient` re-exports
   the two type names and the instance under the names its callers use.
3. **Four modules and one spec name `RegistryTransport`.**
   `assetMetadataResolver.ts:15,36,155`, `assetImageStore.ts:13,83,117,136`,
   `assetMetadataChannel.ts:27,48` and `assetRegistryClient.spec.ts:25,26,64`.
   Re-exporting the type is what keeps all of them, and the 47 cases in that
   spec, unchanged.
4. **The request shapes are confirmed live, on 2026-09-14, against
   `https://preprod.koios.rest/api/v1`.**
   `POST /asset_info?select=…` with `{"_asset_list": [[policyHex, nameHex]]}`
   answers `200` with a one-element array carrying `policy_id`, `asset_name`,
   `fingerprint`, `minting_tx_hash`, `mint_cnt` and `cip68_metadata`.
   `POST /tx_cbor` with `{"_tx_hashes": [hash]}` answers `200` with `tx_hash`,
   `block_hash`, `absolute_slot`, `block_height` and `cbor`. The pair used was
   subject
   `67ab0c92c4ac1610895a1c965ee50aba41a8f1513b15240723b3bd0b10b5e99cd9a171db19a101e9bb4afcb3b449a0aa504fe05eed13708bf3000001`
   and its minting transaction `907243e6…6dea1c`, whose `block_hash`
   `2f7684ce…a6dfb` and `absolute_slot` 131545218 both match the local preprod
   immutable database.
5. **The published limits are 100 requests per 10 seconds and 5,000 a day**, on
   the public tier, enforced by source address
   (`research/01-koios-pointer-option.md:57-76`). A refusal is a bare `429` with
   no headers, so there is nothing to read a wait from.
6. **The registry client's `isRetryable` retries any 5xx and any transport
   failure except `too-large`.** A `429` is a 4xx there and is therefore already
   not retried; here it needs to be distinguished from other 4xx, because it is
   the one that says come back later rather than never.
7. **`AssetResolutionWrite` is the existing shape for "not now".**
   `assetMetadataDb.ts` constrains `state` to four values with a `CHECK`, and
   `failed` with a `retry_after` is what the registry client already writes for
   a batch it abandoned.

## Files Expected To Change

- `source/main/assets/httpTransport.ts` (new)
- `source/main/assets/assetRegistryClient.ts`
- `source/main/assets/koiosClient.ts` (new)
- `source/main/assets/koiosClient.spec.ts` (new)
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**Two calls, always, and per batch.** `asset_info` for the minting transaction
hash, then `tx_cbor` for the point and the bytes. The second call asks for the
distinct hashes rather than one per subject, so a collection minted in one
transaction costs one entry.

**The `select=` list is shorter than the research note's, by one field, and the
omission is the point.** `minting_tx_metadata` is not requested. The CIP-25
payload is in the transaction bytes that `task-034` confirms against the user's
own chain, so asking the index for its own copy would add an unconfirmed input
for a value already held. `cip68_metadata` **is** requested, because a CIP-68
datum lives at a spendable UTxO rather than in the mint transaction and no
reading of that transaction can produce it. `logo` is never requested.

**The ceiling is a rolling count in this process, and it fails closed.**
Twenty requests per ten seconds, a fifth of the published limit, checked before
every request including the retry. Reaching it abandons the batch, records a
`retry_after` ten minutes out, and **stops**: the ceiling is a property of the
process rather than of the batch, so working through the remaining batches would
be a queue of refusals. It resets on restart, which is stated rather than
persisted: a counter that survived a restart would make a defect survive one too,
and the demand model is one resolution per asset for the life of an
installation.

**A `429` is a throttle, not a failure.** It is excluded from the retry
predicate, so it is never asked again immediately, and it records the same ten
minute wait as the ceiling rather than entering the doubling backoff, because
the instance has said "too often" rather than "something is wrong".

**A pointer with no bytes behind it is dropped.** If `tx_cbor` does not return
the transaction a pointer names, the pointer cannot be confirmed and is not
carried forward as though it could be.

**Nothing is written and nothing throws.** Every path returns pointers, the
transactions behind them, and resolution rows, and the caller decides.

## Acceptance Criteria

1. A batch of subjects produces exactly two requests, and a batch of forty
   produces two rather than eighty.
2. The `asset_info` request carries a `select=` list, and asks for neither
   `logo` nor `minting_tx_metadata`.
3. The request body carries subjects and nothing else, asserted against a
   captured request rather than against the code.
4. A `429` backs off and is never retried immediately, asserted per status code.
5. Reaching the per-process ceiling leaves rows absent, issues nothing further,
   and surfaces nothing.
6. A timeout on either call records a `retry_after` and abandons the batch
   without spinning.
7. A custom base URL with a path prefix, and one with a trailing slash, both
   compose correctly, and the `direct` option issues nothing.
8. No new runtime dependency. `compile`, `lint` and `jest` pass, and the other
   three checks with them.
9. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

- The spec stubs the transport and records every call, so criteria 1 to 5 are
  asserted on what went out rather than on what came back.
- Criterion 1 is driven twice: one subject, and forty subjects sharing a policy.
  The second is the one that fails if batching were per asset.
- Criterion 3 parses the captured body and compares it to an exact object, so a
  field added later fails the case rather than passing unnoticed.
- Criterion 4 asserts the call count is one. A retry would make it two, and the
  503 case immediately below it asserts two, so the pair distinguishes "does not
  retry a 429" from "does not retry anything".
- Criterion 5 is driven twice: a ceiling of zero, which issues nothing at all,
  and a ceiling of one against two batches, which issues one request and stops.
- The negative parsing cases carry their weight here: an entry for a subject
  nobody asked about, a transaction nobody asked about, a body that is not an
  array, and a pointer whose transaction did not come back. Each asserts no
  pointer survives.
- `assetRegistryClient.spec.ts` is the regression guard for the extraction. Its
  47 cases, including the loopback-server transport group `task-026` added, must
  pass unchanged.
- All six Nix checks.

## Risks and Open Questions

- **The terms of service.** `research/01-koios-pointer-option.md` records that
  Koios's published terms prohibit automated access and systematic retrieval,
  and that whether `api.koios.rest` is inside the document's definition of "the
  Site" is ambiguous. Adopting Koios is a settled decision of the PRD and is not
  reopened here. What this task can do about it, it does: the ceiling is a fifth
  of the published limit, the responses are trimmed, and each asset is resolved
  once.
- **The ceiling resets on restart.** A defect that issued requests in a loop
  would be stopped within a session and would start again on the next one.
- **`minting_tx_hash` may be the first mint or the latest.** The documentation
  says both. For a closed policy there is one final state either way; for an
  open one the pointer is read as whatever it names and `task-034` confirms or
  rejects that transaction.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-033.targetPaths` corrected to the
  transport module and the spec, and `assetMetadataResolver.ts` removed from it,
  because the resolver wiring belongs with the write in `task-035`;
  `task-033.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-033-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-033-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A list of subjects becomes a list of pointers and the bytes behind them, for two
requests and within a ceiling.

## Final Outcome

Complete.

## Self-Review

The easy version of this module asks `asset_info` for everything it returns and
uses `minting_tx_metadata` as the CIP-25 record. That would be one field in a
select list and it would quietly undo the point of the whole channel: the record
would come from the index rather than from bytes the user's own node confirms,
and nothing downstream would be able to tell the difference. Leaving that field
out is the smallest change in this task and the one most worth defending.
