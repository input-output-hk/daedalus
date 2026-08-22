# Finding: a retired DRep leaves the wallet with nothing to say

**Status:** open, not scheduled
**Raised from:** DRep discovery review
**Scope:** governance, but the fix is upstream
**Severity:** low as a defect, higher as an experience. No funds at risk; a
wallet can appear stuck with no explanation.

---

## What retirement is

A DRep retires by submitting an unregistration certificate. The registration
ends and the DRep leaves the DRep set, which is a normal and permanent outcome
rather than a fault. Delegations pointing at that DRep remain recorded on chain:
the delegation does not follow the DRep out, it simply stops resolving to
anyone.

Somebody who delegated a year ago and opens Daedalus today can therefore be
delegated to a DRep that no longer exists.

## What Daedalus knows

Nothing.

`ApiDRepInfo.status` is `'active' | 'inactive'` and carries no third value.
Retired DReps are absent from `GET /v2/dreps` entirely rather than present with
a status, so the absence is all there is to read, and absence is ambiguous: a
DRep missing from the list is indistinguishable from one that has not been
fetched yet.

`GovernanceStore.lookupDRep` searches the suggested list, the full list and the
per-id fetch cache, and returns `null` when none of them has it.

## What the user sees

In the voting centre, `GovernanceWallets` renders the delegation target from
`drepEntry`, and a null entry means the fetch has not finished:

```
if (drepEntry == null) return "Loading DRep…";
```

For a retired DRep that never finishes. The wallet shows a row that says it is
loading something, forever, with no way to tell that the thing being waited for
will not arrive. The user is left believing the wallet is broken or slow, when
what has happened is that their representative retired and their voting power
is no longer being exercised by anyone. That is exactly the moment they would
want to be told to delegate again.

The detail page is better by accident. Reaching a retired DRep by id shows "This
DRep was not found in the latest on-chain data", which is true but does not name
retirement as the reason or say what to do about it.

There is also a dead branch waiting for this: `STALE_FAVORITE_STATUSES` in
`drep-directory/helpers.ts` tests for a `'retired'` status the type system says
cannot occur, so a favourited DRep that retires is not marked stale either.

## Why it cannot be fixed here

The wallet API does not expose the fact. Distinguishing "retired" from "not
fetched yet" needs one of:

- a status value from cardano-wallet, meaning an upstream change and a version
  bump, which is the clean answer
- a distinguishable failure from `GET /v2/dreps/{id}`, if a retired id returns
  something a client can tell apart from a transport failure. Worth testing
  against a real retired DRep before assuming either way
- reading the unregistration certificate from the chain independently, which
  means a second source of truth for something the wallet should be serving

## What a fix should produce, whichever route

A wallet delegated to a retired DRep should say so plainly, in the voting
centre, alongside the other standing states. It belongs in the same badge as
active, expiring soon and inactive, and it should read as needing action rather
than as an error, because the user has done nothing wrong. Delegation to a
retired DRep is the one state that is certainly not coming back on its own.

Until then, the honest interim would be a timeout on the resolution rather than
an indefinite "Loading", saying the DRep could not be found and suggesting a
fresh delegation. That is worth weighing on its own: it is cheap, it is not
correct, and a wrong explanation may be worse than a slow one.

## Relevant files

- `source/renderer/app/api/governance/types.ts` (`ApiDRepInfo.status`)
- `source/renderer/app/stores/GovernanceStore.ts` (`lookupDRep`)
- `source/renderer/app/components/governance/wallets/GovernanceWallets.tsx`
- `source/renderer/app/components/governance/drep-directory/helpers.ts`
  (`STALE_FAVORITE_STATUSES`)
- `source/renderer/app/components/governance/_shared/DRepStatusBadge.tsx`
