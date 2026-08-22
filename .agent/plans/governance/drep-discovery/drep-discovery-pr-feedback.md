# DRep Discovery: PR #3355 Review Feedback

Triage and execution plan for the ten review items raised on [PR #3355](https://github.com/input-output-hk/daedalus/pull/3355) between 2026-08-07 and 2026-08-10, against branch `feat/drep-discovery`.

Companion task graph: [drep-discovery-pr-feedback-tasks.json](./drep-discovery-pr-feedback-tasks.json).
Feature reference docs: [README.md](./README.md), [store.md](./store.md), [api.md](./api.md).
Original design intent: [designs/drep-discovery-design.md](./designs/drep-discovery-design.md), [designs/shared-design-tokens.md](./designs/shared-design-tokens.md).

## Source items

| Item | Date | Substance |
|---|---|---|
| 1 | 2026-08-07 | Expiry filter ("All" / "Expiring in 7-12 epochs") has unclear user value |
| 2 | 2026-08-07 | Favorites toggle rarely matches the 20-DRep random sample |
| 3 | 2026-08-07 | Directory list layout is sparse; "Threshold" and "Primary" badges read as contradictory |
| 4 | 2026-08-07 | Empty metadata sections on the detail page should be hidden |
| 5 | 2026-08-07 | "Current Votes" will outgrow its context box; wants a collapsible table below the profile |
| 6 | 2026-08-07 | Wants a total-DRep-stake denominator to drive a high-voting-power warning badge |
| 7 | 2026-08-10 | Transaction list labels an SPO-plus-abstain certificate as "Voting Power Delegation" |
| 8 | 2026-08-10 | Delegation Center still shows undelegated after an SPO certificate from the previous epoch |
| 9 | 2026-08-10 | A DRep-only certificate appeared to switch a pool-delegated wallet to undelegated |
| 10 | 2026-08-10 | `getDRepSummary` bech32 errors in the console, suspected race against an empty DRep list |

Items 8 and 9 arrived as one comment with item 7; the inline review comment on `GovernanceStore.ts:167` is tracked as item 11 below.

## Triage findings

Three items resolve to concrete causes in the current code. These are stated as findings where the code is unambiguous and as hypotheses where a live backend check is still needed.

### The startup crash is confirmed

`isGovernancePage` reads `this.stores.router.location.pathname` with no null guard (`source/renderer/app/stores/GovernanceStore.ts:165`). `setup()` registers a MobX reaction on that computed (`source/renderer/app/stores/GovernanceStore.ts:365`), which evaluates immediately, before the router holds a location. The reported stack trace names both lines. Fix is a guard plus a regression test.

### The summary endpoint exists, but not in the wallet that produced the error

`getDRepSummary` requests `GET /v2/dreps/summary` (`source/renderer/app/api/governance/requests/getDRepSummary.ts:8`). The branch's own API reference documents only three DRep endpoints ([api.md](./api.md)), which initially suggested the route did not exist. The pinned wallet says otherwise: `flake.nix:14` pins `cardano-foundation/cardano-wallet` at `26c79b4194c71167f28281237541203feaa28b40`, and that revision's `specifications/api/swagger.yaml` defines five DRep routes, including `/dreps/summary`, whose `ApiDRepSummary` schema carries exactly the `total_drep_stake`, `active_drep_count`, `inactive_drep_count` and `total_drep_count` fields the store reads at `GovernanceStore.ts:222-230`.

So the client is correct and `api.md` is incomplete. The reported `bad_request` names the `{drepId}` route's bech32 validation, which is what a wallet build predating `/dreps/summary` would return for the literal path segment `summary`. The likely explanation is an environment mismatch: the bridge in the local Nix store is `v2026-07-23`, the revision master pins, not the revision this branch pins.

Two related notes. The feature commit's message claims a bump "to se7en-labs-inc fork with /dreps and /dreps/summary endpoints", but the flake pins `cardano-foundation` and no fork appears in `flake.lock`; the branch was force-pushed, so the message may describe a state that no longer exists. And the race-condition hypothesis in item 10 does not hold either way: `fetchDRepSummary()` is called unconditionally from `fetchSuggestedDReps()` (`GovernanceStore.ts:191`), so it fires on every refresh regardless of list state.

Knock-on effect for item 6: the catch in `fetchDRepSummary` swallows the failure with the note that the badge is silently omitted (`GovernanceStore.ts:232`), so against a wallet without the route the high-voting-power badge cannot render and nothing says why.

### The delegation display bugs share a probable root cause

The active-delegation branch carefully refuses to read a pool target out of a voting-only status (`source/renderer/app/api/api.ts:3125`, with an explicit comment). The pending-delegation branch below it applies no such guard: `lastDelegatedStakePoolId` is assigned from `last(next).target` unconditionally (`source/renderer/app/api/api.ts:3145`, `:3161`).

Two consequences follow, and they match items 8 and 9:

- A pending voting-only entry carries no `target`, so it overwrites `lastDelegatedStakePoolId` with null and the pool disappears from the UI.
- `last(next)` reads only the final element, so when a pending SPO delegation and a pending vote delegation coexist, whichever sorts last wins and the other is dropped.

Existing coverage does not reach this: the only test touching the `next` array uses a single-element array with status `delegating` and a real pool target (`tests/jest/api/createWalletFromServerData.spec.ts:109`). No fixture exercises a voting-only pending entry.

This explains the display. It does not by itself establish whether the submitted certificate preserved the stake delegation on-chain, which item 9 also raises. The explorer links in the comment are the evidence to check first, and that check is a separate task because the answer changes the fix.

## Decisions

All six were settled on 2026-08-20.

1. **Expiry filter (item 1), and what "approaching expiry" means.** The threshold is **six epochs** everywhere, the number the status badge already uses. `dRepActivity` is 20 epochs on both mainnet and preprod, and an epoch is 432,000 slots at one second, so six epochs is 30 days of a DRep's 100-day window; twelve would have been 60 and was rejected. Copy leads with inactivity rather than expiry, since the counter resets whenever the DRep votes, and the exact elapsed count is not shown: it needs the `dRepActivity` protocol parameter, which the pinned wallet does not serve, and hardcoding it would let a governance action silently falsify a claim about a named DRep.

   No expiry filter exists in this branch, and none existed at the pre-force-push tip `d77311a54` either. `DRepFilterState` carries only status and metadata filters (`helpers.ts:179-190`). What does exist is an "Expiry (soonest first)" sort option and an "Expiring in {n} epochs" badge; the seven-to-twelve epoch window quoted in review matches `APPROACHING_EXPIRY_MIN`/`MAX` in `DRepCategoryBadge.tsx:42-43`. Rather than remove a control that is not there, the idea is inverted into a filter that hides DReps approaching expiry, which has a delegation use that selecting for them did not. New scope, taken deliberately. See fb-301.
2. **Directory layout (item 3).** User-selectable card and table views, with the choice persisted, mirroring the Stake Pools grid/table toggle and its table components rather than introducing new ones. Stake Pools does not persist its own choice today, so persistence is retrofitted there too; that part touches a shipped screen outside this PR. See fb-303 and fb-306.
3. **Badge vocabulary (item 3).** Split the two axes rather than renaming, and retire High value: its cohort-median rule commends the same quantity the Target15 threshold warns about, so voting power is communicated once, by the share figure. See fb-304.
4. **High-power threshold (item 6).** 1.5% of voting power held by real DReps. See fb-204 and the measurement below.
5. **Transaction label (item 7).** A single "Delegation Transaction" label for all governance transactions. See fb-106.
6. **Favourites (item 2).** Pinned in their own group above the randomized cohort, visible without any mode switch, rather than merged into the sample or left behind a toggle. See fb-302.

### Reusing what Daedalus already ships

The UX work takes its patterns from existing screens rather than inventing any:

| Need | Existing pattern |
|---|---|
| Card/table toggle | `StakePoolsSearchListViewButton.tsx` |
| Table | `StakePoolsTable`, `StakePoolsTableHeader`, `StakePoolsTableHeaderCell`, `useCreateColumns`, on react-table `useTable`/`useFlexLayout` with react-virtualized, inside `BorderedBox` |
| Collapsible section | the expandable row in `Transaction.tsx`, and `WalletToken` / `WalletTokenHeader` |
| Filter control | the existing selects in `DRepDirectoryFilters.tsx` |

### Measured concentration, mainnet, 2026-08-20

Taken from Koios rather than a mainnet sync, to answer the denominator question before fixing a threshold.

| Quantity | Value |
|---|---|
| Registered DReps | 1,062 |
| Voting power held by real DReps | 5.257B ADA |
| `always_abstain` | 9.750B ADA |
| `always_no_confidence` | 0.170B ADA |
| CR10 against the real-DRep denominator | 51.1% |

The denominator is the load-bearing choice. Against real DReps only, a 1.5% threshold cuts at roughly 79M ADA and flags 16 of 1,062. Fold `always_abstain` into the denominator and the same 1.5% cuts at 228M ADA and flags 5, missing 8 of the 13 DReps that the campaign's own 87M ADA figure targets. A threshold this narrow also disposes of the warning-fatigue concern: it marks about 1.5% of DReps, not a large share of the cohort.

The campaign's stated CR10 was 47.8% against a total near 5.8B ADA; the figures here are a later snapshot and differ slightly.

### The denominator, verified against a live wallet

Settled on preprod by running the branch's own wallet against a locally synced node and comparing with Koios:

| Source | Total DRep stake (lovelace) | DRep count |
|---|---|---|
| `GET /v2/dreps/summary` | 511,304,929,746,789 | 280 |
| Koios, real DReps only | 511,304,929,746,789 | 280 |
| Koios, including `always_abstain` | 933,975,895,416,176 | 282 |

The wallet's figure matches the real-DRep sum to the lovelace, and its count matches Koios's registered DRep count. `always_abstain` (422,670,965,669,387) and `always_no_confidence` (336,240,991,975) are excluded from both. `total_drep_stake` can therefore be used as the denominator directly, and the 1.5% threshold means what it is intended to mean with no adjustment.


## Item-by-item outcome

Recorded 2026-08-21, after the work below landed. Every claim here is checked against the code as shipped on this branch, not against intent.

| Item | Outcome |
|---|---|
| 1. Expiry filter | Inverted rather than removed. No expiry filter existed in this branch or at the pre-rewrite tip `d77311a54`; what existed was an expiry sort and a badge. A filter that hides DReps lapsing within six epochs now exists. Inert on the default view by design, since the cohort floor already excludes them. |
| 2. Favourites vs the 20-DRep sample | Fixed. Favourites resolve by id and pin above the cohort with a count, visible without any mode switch. |
| 3a. Directory whitespace | Fixed. A table view now sits alongside cards, with the choice remembered, mirroring the stake pools screen. |
| 3b. Threshold and Primary badges | Fixed. Metadata verification became a boolean badge and expiry moved to its own, so both facts show at once. The threshold moved from seven-to-twelve epochs to six. |
| 4. Empty metadata sections | Fixed. The rendering was already conditional but tested for null while the wallet passes empty strings through. Blank values now normalise to absent at the mapping. |
| 5. Current Votes outgrowing its box | Not buildable as scoped, decision recorded instead. No voting-history list exists anywhere in the branch; `CurrentDRepSummary` shows the current DRep and no votes. The design document now specifies a collapsible table below the profile for when it is built. |
| 6. Total DRep stake and a power threshold | Fixed. Every DRep states its share, with a warning above 1.5%, derived from the Target15 CR10 goal and verified against a live wallet. |
| 7. Transaction titled "Voting Power Delegation" | Fixed. Reads "Delegation Transaction", which is accurate for stake, vote and combined certificates alike. |
| 8. Delegation Center showing undelegated | Fixed. Two defects: the pending mapping read the array's final entry unconditionally, and the delegation centre's own epoch lookup accepted only the `delegating` status. |
| 9. DRep-only certificate appearing to drop SPO delegation | Fixed, and confirmed to be display-only. The epoch-306 transaction carried a single `vote_delegation` certificate, and the stake account still reports both its pool and its DRep. No stake was ever lost. |
| 10. `getDRepSummary` console errors | Environment, not code. The pinned wallet serves `/v2/dreps/summary`; the build in use did not. The summary failure is now visible in store state rather than swallowed, and `api.md` documents all five routes. |
| 11. Null `pathname` crash at startup (inline comment) | Fixed. The computed had no guard and `setup()` registers a reaction that evaluates before the router has a location. |

Three of the eleven, items 1, 3b and 5, referred to controls that do not exist in the branch as it stands, which is consistent with the branch having been force-pushed between the review and this work.

## Still needing a live run

Everything above is verified by the test suite, by on-chain inspection, or by reading the shipped code. Two things need a person in front of the running app:

- A preprod session confirming no governance console errors across directory load, refresh, detail view and delegation, against the branch's own wallet build rather than master's.
- The delegation flows from items 8 and 9 re-run end to end, since the fix is verified by unit tests and on-chain history but has not been exercised through the UI.

## Phases

| Phase | Focus | Risk |
|---|---|---|
| `fix-a` | Startup crash, delegation state mapping, transaction labeling | high |
| `fix-b` | Summary endpoint and the high-voting-power badge | medium |
| `ux-directory` | Expiry filter, favorites, layout density, badge vocabulary | medium |
| `ux-detail` | Empty metadata sections, Current Votes placement | low |
| `verify` | Cross-cutting regression and preprod smoke | medium |

Correctness precedes polish. Item 9 is treated as the highest-priority item in the set: if a DRep-only certificate genuinely drops stake delegation rather than merely displaying it wrongly, users lose rewards, and that outcome gates the release.

## Verification

Each task lands its own Jest coverage in the same change, per the vertical-slice convention the original plan established. Beyond that:

- `yarn check:all` and `yarn test:jest` for every task.
- Storybook for all `ux-*` tasks; governance fixtures already exist under `storybook/stories/governance/_utils/fixtures.ts`.
- Preprod smoke for `fix-a` and `fix-b`, since both depend on real wallet responses. No local chain data is present, so this needs either a Mithril bootstrap or selfnode.
- Item 9 additionally needs on-chain confirmation from a block explorer, not just UI observation.
