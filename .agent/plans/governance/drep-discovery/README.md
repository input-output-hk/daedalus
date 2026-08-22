# DRep Discovery Feature — Implementation Reference

This directory documents the **current implemented state** of DRep governance in the `drep-selection` branch.
It is a living reference for agents working in this area; update it when the implementation changes.

## Documents

| File | What it covers |
|------|---------------|
| [architecture.md](./architecture.md) | Component tree, data flow, layer responsibilities |
| [store.md](./store.md) | GovernanceStore: observables, actions, types, persistence |
| [search-filter-sort.md](./search-filter-sort.md) | Search (name + ID prefix), filters, sort — helpers.ts reference |
| [directory-ordering.md](./directory-ordering.md) | The default show-all order: the bands, the mainnet evidence, and why inactive DReps come last |
| [cohort-selection.md](./cohort-selection.md) | Client-side cohort: criteria, seeded draw, reroll, and relaxation when the pool is short |
| [metadata-field-policy.md](./metadata-field-policy.md) | Which CIP-119 fields are shown, why no images, and how unknown JSON-LD terms are handled |
| [smoke-test-runbook.md](./smoke-test-runbook.md) | The live checks automation cannot do, and what automation already covers |
| [navigation-handoff.md](./navigation-handoff.md) | delegationNavState pattern; the round-trip routing solution |
| [auto-favorite.md](./auto-favorite.md) | Auto-favorite on select: directory, detail, existing delegation |
| [api.md](./api.md) | cardano-wallet REST endpoints and TypeScript types |
| [routes.md](./routes.md) | ROUTES constants and which component handles each |
| [design-decisions.md](./design-decisions.md) | Non-obvious decisions: sentinel cards, scroll-to-top, verified names, etc. |
| [drep-discovery-pr-feedback.md](./drep-discovery-pr-feedback.md) | Triage of the PR #3355 review feedback: root causes, decisions required |
| [drep-discovery-pr-feedback-tasks.json](./drep-discovery-pr-feedback-tasks.json) | Task graph for the PR #3355 remediation work |

## Quick orientation

The feature lets users browse DReps (Delegated Representatives), inspect details, mark favorites, and select one
for voting-power delegation. The delegation transaction is submitted through the existing `VotingPowerDelegation`
form in the Voting section.

```
Voting → Governance form (VotingGovernancePage)
                 │ "Browse DReps"
                 ▼
         DRep Directory (DRepDirectoryPage → DRepDirectory)
                 │ "View details"
                 ▼
         DRep Detail (DRepDetailPage → DRepDetail)
                 │ "Select for delegation"
                 ▼
         Voting → Governance form  (back, with DRep pre-selected)
```

State that cannot survive a `history.push()` in hash history v4 is carried through
`GovernanceStore.delegationNavState` — see [navigation-handoff.md](./navigation-handoff.md).
