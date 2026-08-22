# Suggested cohort selection

How the twenty suggested DReps are chosen, and why the choice moved out of
cardano-wallet.

**Implementation:** `source/renderer/app/components/governance/_shared/drepCohort.ts`
**Store:** `GovernanceStore.suggestedDReps` (computed), `setCohortCriteria`, `rerollCohort`
**UI:** `DRepCohortCriteriaPanel`, behind the "Suggestion criteria" disclosure
**Tests:** `drepCohort.spec.ts`

---

## Why it moved

The cohort came from `GET /v2/dreps/suggested?count={n}`, which accepts a count
and nothing else. Whether a DRep was active, how much voting power it held, and
whether it had published anything verifiable were all the server's to decide, so
changing any of it meant an upstream commit to cardano-wallet rather than a
control the user could reach.

The endpoint is also documented as deterministic per session, which left the
reroll button able to return the same twenty DReps it had just shown.

## The criteria

Defaults, each one a control in the criteria panel rather than a constant in the
code.

| criterion | default | why |
|---|---|---|
| `activeOnly` | on | An inactive DRep's voting power does not count, so suggesting one wastes the delegation |
| `excludeLapsingSoon` | on | A DRep within six epochs of lapsing may stop counting before the delegation is worth anything |
| `maxVotingPowerShare` | 1.5% | The Target15 figure, the same threshold the voting power badge flags |
| `requireVerifiedMetadata` | on | A DRep that published nothing verifiable cannot be assessed by a reader |
| `size` | 20 | Enough to choose from without imposing choice overload |

`doNotList` is applied before the criteria and is never relaxed. It is the
DRep's own instruction rather than the user's preference.

## Pool size, measured

Against 1,000 registered mainnet DReps sampled through Koios at epoch 650, the
defaults leave **204 eligible**, about ten distinct cohorts of twenty before
entries repeat. Adequate, but not abundant, which is what makes the relaxation
path below necessary rather than theoretical.

## Considered and rejected: excluding DReps with no voting power

A DRep who has not delegated even their own stake to themselves looks like a
DRep with nothing at stake, and 165 of the 1,000 sampled hold no voting power at
all. The signal is real. It is also already spent.

| group | n | active | metadata | both |
|---|---|---|---|---|
| zero voting power | 165 | 9 (5%) | 4 (2%) | **0** |
| some voting power | 835 | 359 (42%) | 400 (47%) | 247 (29%) |

Not one zero-power DRep is currently eligible for the cohort. The pool is 204
either way, because a DRep who never self-delegated overwhelmingly also let
their registration lapse or never published metadata, and the criteria catch
that through those two better-founded proxies. Adding a third criterion would
change nothing, while adding a control to the panel that does nothing.

The stronger objection is what it would do when it did bite. The only DRep it
would exclude is one who is active, has published verifiable metadata, and has
not yet been delegated to by anyone including themselves, which describes a
newly registered participant. Excluding them means they cannot be suggested, so
they cannot receive a first delegation, so they never stop being excluded. That
is an incumbency bias, and it is the same thing the randomised draw exists to
prevent: it just measures popularity by delegation received instead of by
position in a list.

Zero voting power also does not mean quite what it appears to. It cannot
distinguish a DRep who did not bother to self-delegate from one who did and
holds almost no ada. Neither is disqualifying in the way an expired
registration is.

If the correlation ever weakens, meaning zero-power DReps start appearing in
the eligible pool in numbers, revisit this. The measurement above is the
baseline to compare against.

## When the pool is too small

Criteria are relaxed in a fixed order until the pool can fill the requested
size, and the UI names which ones had to be dropped. The order is
least-protective first:

1. `requireVerifiedMetadata`
2. `maxVotingPowerShare`
3. `excludeLapsingSoon`
4. `activeOnly`

Being unreadable is the least harmful thing to tolerate; suggesting a DRep whose
votes will not count is the most, so `activeOnly` is surrendered last. On the
mainnet numbers this order also happens to restore the most DReps first.

## Randomness

Selection uses a seeded shuffle (mulberry32 into Fisher-Yates) and no ambient
randomness anywhere. Two consequences, both deliberate: the draw is testable,
and rerolling can step the seed until the drawn set actually differs from the
one on screen, so the control does what its label says.

Random rather than ranked, because any stable ordering of suggestions makes the
top of the list worth holding, and a wallet that ranks DReps is a wallet
concentrating governance around whoever it ranks highest. This is the same
reasoning as the bands in [directory-ordering.md](./directory-ordering.md).

## Related

- [Directory ordering](./directory-ordering.md)
- [Search, filter and sort](./search-filter-sort.md)
- [api.md](./api.md)
