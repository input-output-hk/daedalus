# DRep directory ordering

How the full DRep list is ordered when no explicit sort is chosen, why, and
what evidence the decision rests on.

**Implementation:** `source/renderer/app/components/governance/_shared/drepCohort.ts`
(`DRepStandingBand`, `getDRepStandingBand`, `orderDRepsByStanding`)
**Tests:** `source/renderer/app/components/governance/_shared/drepCohort.spec.ts`
**Sort option:** `recommended`, labelled "Recommended (default)"

---

## The problem

Show-all put every registered DRep on screen in whatever order the wallet
returned them. On mainnet that is over a thousand entries with no structure,
which asks a reader to assess each one from scratch. The previous `randomized`
option did not even shuffle: it returned the list untouched, so the order was
an accident of the API response.

## The evidence

Sampled 1,000 registered mainnet DReps through Koios at epoch 650. Criteria
applied cumulatively:

| filter | remaining | share |
|---|---|---|
| all registered | 1000 | 100% |
| active | 368 | 37% |
| and more than 6 epochs from lapsing | 298 | 30% |
| and under 1.5% of delegated voting power | 280 | 28% |
| and carrying resolvable metadata | 204 | 20% |

Two findings drove the design. Only 37% of registered DReps are active, so the
majority of an unordered list cannot exercise voting power at all. And only 40%
have resolvable metadata, so an unordered list also puts a large number of
DReps on screen about whom nothing can be read.

## The order

Bands, in this sequence. Each band is a fact the entry already carries, not a
judgement about the person behind it.

1. **Suggestible.** Active, verified metadata, under the 1.5% concentration
   threshold, more than six epochs from lapsing. Everything the cohort is drawn
   on.
2. **Lapsing soon.** As above, but its voting power lapses unless it records
   activity by voting, updating metadata, or re-registering.
3. **Concentrated.** Active and accountable, but already holds a large share of
   governance.
4. **Unaccountable.** Active, but has published nothing that verifies against
   its anchor.
5. **Inactive.** Last, behind even a DRep that published nothing.

### Why inactive comes last rather than third

The first draft placed inactive DReps with metadata in the middle, ahead of the
concentrated and unaccountable bands, on the reasoning that they were at least
accountable. That was wrong. An inactive DRep's voting power does not count, so
delegating to one discards the delegation rather than merely spending it
poorly. A concentrated DRep still votes; a DRep with no metadata still votes.
An inactive one does not. Being unreadable is a worse position to delegate from
than being large, but being inert is worse than either.

### Why concentrated sits above unaccountable

Concentration is a property of the whole field rather than of the DRep, and
delegating to a large active DRep still produces a vote. Concentration is
flagged where it matters, on the voting power badge, rather than by burying the
DRep.

### Why order within a band is random

Any stable order inside a band makes a position near the top of it worth
holding, which is the popularity bias the cohort is drawn randomly to avoid.
Ordering by voting power would compound the concentration the 1.5% threshold
exists to flag; ordering by ID would hand an advantage to whoever chose a
convenient key.

The concentrated band is the exception and sorts by voting power ascending.
There the figure is exactly what separates its members, and the least
concentrated of them is the one a reader should meet first.

### Why the option is called "Recommended"

It was "Randomized (default)", which described the old non-behaviour and would
have been a lie about the new one. "Recommended" states that the wallet has a
view, which it does: the bands are the same policy the cohort criteria encode.
Naming it plainly is better than presenting a considered order as an arbitrary
one.

## What it does not do

The bands rank *positions to delegate from*, not DReps. Nothing here reads a
DRep's objectives, voting record, or affiliations, and nothing promotes one
named DRep over another within its band. Every explicit sort overrides the
bands entirely.

## Related

- [Search, filter and sort](./search-filter-sort.md)
- [Design decisions](./design-decisions.md)
