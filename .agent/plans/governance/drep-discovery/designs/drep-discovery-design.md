# DRep Discovery — Governance Section Via Renamed `Voting` Entry

**Paradigm:** Rename the existing `Voting` sidebar entry to `Governance` and expand that single section to cover the governance delegation form, DRep directory, DRep detail, Favorites, and existing Catalyst voting. Delegation handoff uses React Router `location.state` into the existing `/voting/governance` form. This is the locked design direction; see [README.md](./README.md) for rationale.

## Information Architecture

```mermaid
flowchart TD
  Nav[Sidebar: Governance (renamed from Voting)] --> Section[Governance section]
  Section --> Existing[/voting/governance]
  Section --> Dir[/governance/dreps]
  Section --> Detail[/governance/dreps/:drepId]
  Section --> Favs[/governance/favorites]
  Section --> Catalyst[Existing Catalyst voting]
  Dir -- Select for delegation --> Hand[Voting form pre-fill]
  Detail -- Select for delegation --> Hand
  Favs -- Select for delegation --> Hand
  Hand --> Existing
  Existing --> Confirm[Confirmation dialog]
  Confirm --> SW{Wallet type?}
  SW -- Software --> SwPwd[Spending password]
  SW -- Hardware --> HwDev[On-device confirm]
  SwPwd --> Submit[delegateVotes request]
  HwDev --> Submit2[VotingStore HW path]
```

New route literals (proposed for `routes-config.ts`):

```
GOVERNANCE: {
  ROOT: '/governance',
  DREPS: '/governance/dreps',
  DREP_DETAIL: '/governance/dreps/:drepId',
  FAVORITES: '/governance/favorites',
}
```

The existing `VOTING.GOVERNANCE = '/voting/governance'` route stays; the handoff is `history.push('/voting/governance', { selectedDRepId })` through React Router `location.state`. Query params and `VotingStore.pendingFormState` are explicitly out of scope.

**Sub-route defaults & active state:**
- `/governance` redirects to `/governance/dreps` (the Directory is the section landing page).
- The `Directory` tab is the active tab for **both** `/governance/dreps` and `/governance/dreps/:drepId` (detail view does not get its own tab).
- The `Favorites` tab is active only for `/governance/favorites`.

**Second entry point — "Browse DReps" from the delegation form.** In addition to the renamed sidebar entry, the existing `VotingPowerDelegation` form gains a secondary "Browse DReps" link/button next to the DRep ID input that navigates the user to `/governance/dreps`. This is the plan-mandated second entry affordance — users who land on the delegation form first should not have to learn a separate nav surface to discover the directory.

**Round-trip state preservation (binding).** Navigating out to `/governance/dreps` from `VotingPowerDelegation` preserves the form's currently-selected wallet and vote-type in React Router `location.state`, for example `history.push('/governance/dreps', { from: '/voting/governance', selectedWalletId, voteType })`. The directory/detail/favorites surfaces pass the same state back, plus `selectedDRepId`, when they navigate to `/voting/governance`. `VotingStore.pendingFormState` and query params are not used. Tests must cover wallet + vote-type restoration end-to-end.

For the two-hop sequence (Form → Directory → Detail → Form): the Directory passes state to Detail when pushing `/governance/dreps/:drepId`. Detail's "Select for delegation" combines the inherited state with `selectedDRepId` before pushing `/voting/governance`. Tests must cover wallet + vote-type restoration through both hops.

## Wireframes

### Directory route `/governance/dreps`

```
┌─ Sidebar ──┬─ Header: Governance ──────────────────────────────┐
│ Wallets    │  Directory  |  Favorites                          │
│ Staking    ├──────────────────────────────────────────────────┤
│► Governance│  [🔄 Refresh] Last updated 3 min ago              │
│ Settings   │  ╭──────────────────────────────────────────────╮ │
│            │  │ ⓘ Default view shows up to 200 eligible      │ │
│            │  │   DReps in randomized order, excluding the   │ │
│            │  │   35 largest. [Show all] · [Reshuffle]       │ │
│            │  ╰──────────────────────────────────────────────╯ │
│            │  [Search DReps by ID…]            [Filters ▾ (1)] │
│            │  ┌───────────────────────────────────────────────┐│
│            │  │ ☆ │ ●Active │ DRep ID drep1yg7s…aj8ras  📋   ││
│            │  │   │         │ Voting power: ₳ 688K  (on-chain)││
│            │  │   │         │ [View details]  [Select]       ││
│            │  └───────────────────────────────────────────────┘│
│            │  …repeat…                                          │
│            │  ◀  page 1 of 8  ▶                                │
└────────────┴────────────────────────────────────────────────────┘
```

The directory list is paginated at **25 cards per page**. Total page count derives from the filtered cohort size (e.g., 8 pages for a 200-DRep default cohort).

### Detail route `/governance/dreps/:drepId`

```
┌─ Governance > DRep detail ────────────────────────────────────┐
│ [← Back to directory]                                          │
│                                                                │
│ {default avatar} drep1yg7s…aj8ras  📋    ☆ Favorite           │
│ (CIP-105) drep185r8rr6j9evjs…uutaz3  📋                       │
│                                                                │
│ ┌── On-chain ──────────────────────────────────────────────┐  │
│ │ Status:        ● Active                                  │  │
│ │ Expires in:    34 epochs                                 │  │
│ │ Voting power:  ₳ 688,964.12                              │  │
│ │                (688,964,123,456 lovelace)                │  │
│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │
│ └──────────────────────────────────────────────────────────┘  │
│                                                                │
│ ┌── Anchor ────────────────────────────────────────────────┐  │
│ │ Anchor URL:    https://example.org/drep.json   (present) │  │
│ │ Anchor digest: b5e2…f3a1                                 │  │
│ │ Source label:  On-chain anchor reference                 │  │
│ │                (anchor-1 will fetch and verify profile)  │  │
│ └──────────────────────────────────────────────────────────┘  │
│                                                                │
│ [Select for delegation]                                        │
└────────────────────────────────────────────────────────────────┘
```

The On-chain box lists exactly the fields `DRepDetailOnchainSection` renders: Status, Expires in, Voting power, Current votes. There is deliberately no `Registered: epoch N` row — no local `drep-state` output carries a registration epoch, so restoring that row would need a new on-chain data source, not a UI change. `Current votes` ships today as a labeled row carrying the graceful `governance.drepDetail.votePositions.unavailable` value ("Vote positions are not available in this version.") until a `gov-state` query lands (slice-4 D1); the row itself is not deferred and must stay in this box.

### Favorites route `/governance/favorites`

Same card layout as Directory, but cohort banner replaced with: `"{n} DReps you've favorited. Favorites are stored on this device only."` Empty state: title + body copy + CTA back to Directory. No illustration ships — the earlier "prominent illustration" claim is resolved as dropped, not deferred.

**Stale favorites.** If a favorited DRep appears with `doNotList=true` after `anchor-2` lands, it remains in the favorites list with its current status badge and an inline caption: `governance.drepFavorites.staleCaption` → *"This DRep is no longer in the default cohort."* `DRepStatus` is the closed union `active | inactive`, so no `Retired` or `Excluded from default cohort` badge exists to show; the caption alone carries the signal. `Retired` stays deferred until a distinct unregistration signal exists. No automatic removal. The user unfavorites explicitly.

## Interaction Sequence (HW Wallet Happy Path)

```mermaid
sequenceDiagram
  participant U as User
  participant Dir as Directory
  participant Store as GovernanceStore
  participant Det as Detail
  participant Form as VotingPowerDelegation
  participant VS as VotingStore (HW)
  participant Dev as Hardware device

  U->>Dir: Open /governance/dreps
  Dir->>Store: ensureLoaded()
  Store-->>Dir: cohort + last-updated
  U->>Dir: Click "View details" on DRep X
  Dir->>Det: navigate /governance/dreps/X
  Det->>Store: getDetail(X)
  Store-->>Det: on-chain fields + anchor presence
  U->>Det: Click "Select for delegation"
  Det->>Form: navigate /voting/governance with { selectedDRepId: X } in location.state
  Form->>Form: pre-fill DRep input, validate
  U->>Form: Confirm wallet + submit
  Form->>VS: initialize HW delegation tx
  VS->>Dev: show DRep ID for confirmation
  Dev-->>U: prompt: confirm X
  U->>Dev: approve
  Dev-->>VS: signed witness
  VS-->>Form: success
  Form-->>U: success screen
```

## Component Hierarchy

Following existing convention (`source/renderer/app/components/<area>/<sub>/`):

```
components/voting/voting-governance/
  VotingPowerDelegation.tsx              ← *modify existing*: add "Browse DReps" link/button next to DRep ID input, wire to /governance/dreps with `location.state` preservation per IA section above

components/governance/
  layouts/
    GovernanceWithNavigation.tsx          ← analog of StakingWithNavigation
    GovernanceWithNavigation.scss
  drep-directory/
    DRepDirectory.tsx                     ← page container
    DRepDirectory.scss
    DRepDirectoryBanner.tsx               ← randomization + show-all banner
    DRepDirectoryFilters.tsx              ← filter dropdown
    DRepDirectorySearch.tsx
    DRepDirectoryList.tsx                 ← card list (mobile/dense)
    DRepDirectoryTable.tsx                ← table view (large screens, parity w/ StakePoolsTable)
    DRepCard.tsx                          ← single result, used by list + favorites
    DRepCard.scss
    DRepStatusBadge.tsx                   ← per shared tokens §1
    DRepCategoryBadge.tsx                 ← per shared tokens §1a (High value / Primary / Threshold / Non-metadata)
    DRepSourceLabel.tsx                   ← per shared tokens §2
    DRepIdDisplay.tsx                     ← CIP-129-primary + copy on cards; both forms in detail/search (§4)
    DRepRefreshIndicator.tsx              ← last-updated + spinner
    helpers.ts                            ← filter/sort helpers analog to stake-pools/helpers.ts
  drep-detail/
    DRepDetail.tsx
    DRepDetail.scss
    DRepDetailOnchainSection.tsx
    DRepDetailAnchorSection.tsx           ← shows "unverified anchor" in slice-4; verified content in anchor-1
    DRepDetailActions.tsx                 ← favorite + select-for-delegation
  drep-favorites/
    DRepFavorites.tsx
    DRepFavorites.scss
    DRepFavoritesEmptyState.tsx
  shared/
    DRepEmptyState.tsx                    ← noResults | selfnode | noSync variants
    DRepErrorBanner.tsx                   ← refresh failed | ranking unavailable
```

Container components (MobX `@observer`) live under `containers/governance/` mirroring the per-page structure.

## State / Empty / Loading / Error Treatments

| Scenario | Treatment |
|---|---|
| First load, no cached data (phase 1) | Full skeleton list, banner visible, refresh button disabled |
| First load completed, default cohort | List rendered (IDs + status; voting power may still be enriching), banner visible, "Last updated just now" |
| Voting-power enrich (phase 2) | List interactive, voting-power column shows `—`/skeleton + "Loading voting power…" until stake lands |
| Refresh in flight, cached data present | Spinner badge next to timestamp, list still interactive |
| Refresh failed (phase 1 timeout/parse) | `DRepErrorBanner` at top; cached list still shown; explicit retry |
| Ranking unavailable (phase 2 failed) | List shown, voting-power column `—`, banner with `error.rankingUnavailable` |
| Selfnode CLI unsupported | Replace list area with `DRepEmptyState selfnode` |
| Node syncing (soft warning) | Render the directory + query anyway; persistent `governance.drepDirectory.syncing` banner with live sync %; refetch when the node reaches tip. See [shared-design-tokens.md §6](shared-design-tokens.md). |
| Node syncing + query empty/era failure | Fallback: replace list area with `DRepEmptyState noSync` |
| No filter results | List area shows `DRepEmptyState noResults` with `Clear filters` and `Show all` actions |
| Favorites empty | `DRepFavoritesEmptyState` with CTA back to Directory |
| DRep detail load failure | Inline error in main pane; "Back to directory" link |

## Anchor Source-Labelling Treatment (anchor-1-ready)

`DRepDetailAnchorSection` always rendered. In slice-4 it shows only:

- Anchor URL (raw, no fetch)
- Anchor digest (truncated, copy button)
- Source label: `On-chain anchor reference` (per shared tokens §2)

In anchor-1 (givenName) and anchor-2 (remaining fields), after `GovernanceQueryService` + anchor fetch verify the content, the section adds a child `DRepDetailAnchorContent` rendering `givenName`, `objectives`, `motivations`, `qualifications`, `references[Link|Identity]`, `paymentAddress`. Each rendered field carries the `Verified off-chain content` label. `DRepCard` does **not** render verified anchor content even after anchor-1/anchor-2 (cards stay on-chain-only) — the verified enrichment surfaces in detail and favorites only.

**`image` / `imageObject` is deferred out of this render set** — dropped, not staged. Rendering it requires a second hash check over the image bytes on top of the anchor digest, and the inline-base64-versus-URL tradeoff against the anchor size cap is an open product question; until that decision is taken the detail view keeps the default avatar shown in the wireframe above.

**`references` are rendered per `@type`**, never as one flat list:

- `Link` entries render as outbound links with an explicit external-link icon, opened only through the hardened HTTPS-only `open-external-url` path.
- `Identity` entries render in a distinct sub-section under a caption stating that the identity is claimed by the DRep and unverified by Daedalus, with guidance to visit the URL and confirm the DRep ID is published there. Hash verification proves only that the registrant authored the blob, which an impersonator satisfies exactly, so an `Identity` entry must never inherit the plain `Link` treatment.
- Unrecognised, missing or `Other` `@type` values fall into one documented default bucket that inherits the `Link` treatment and never the `Identity` caption, so an unknown type can never read as a verified identity.

**`paymentAddress`** renders in the detail view only, read-only with a copy button, under a stated-payment-address label alongside the `Verified off-chain content` source label. The accompanying copy states that the address is the DRep's own claim and that delegating voting power requires no payment to any address. The value never pre-populates a send form or a delegation form field, and never appears on a card.

## Default-Cohort UX

- Banner copy (shared tokens §5) is sticky at the top of the directory list, including the Beyond MVG (BMVG) Simplified attribution as the secondary line.
- The default cohort IS the "Recommended" sort for this release. No separate Recommended tab and no per-card Recommended badge ship in Phase 1; the four-category badge (shared tokens §1a) is the per-DRep explanation surface.
- Default cohort eligibility hard floor: `drep-state` active AND remaining `drepActivity` > 6 epochs. Mock fixtures that surface "Expiring in 3 epochs" cards inside the default cohort are fixture-only — production renders MUST respect the 6-epoch floor. The "Expiring in {n} epochs" status badge from shared tokens §1 fires for the 7–12 epoch threshold window inside the default cohort, and may also appear for entries surfaced via search / show-all that fall below the floor.
- "Excluded from default cohort" badge appears on any top-35 DRep when it surfaces via search or show-all.
- Default cohort is randomized; the seed is held in `GovernanceStore` and persists for the app session. "Reshuffle" reseeds without re-querying.
- Filtering or searching switches the banner copy to remove the randomization claim.

## Filter / Search — Show-All Without Re-introducing Bias

`Show all` replaces the cohort with the **full registration list** — every registered DRep, including the top 35, entries below the 6-epoch floor and inactive entries — in the same seeded session order as the cohort, so the escape hatch introduces no ranking bias of its own. "Eligible" is a term of art in this plan (active AND remaining `drepActivity` > 6 epochs) and deliberately does not bound show-all: the same list also backs the ID search index, so narrowing it would make a legitimately registered DRep unreachable anywhere in the app. Once `doNotList` lands in `anchor-2`, a `doNotList=true` DRep is excluded from the default cohort but stays reachable through show-all, search and direct DRep ID entry. When `Show all` is active, sort options become available (default still `randomized`; user can pick `voting power desc`, `voting power asc`, `expiry asc`). Sort is opt-in only; the user must make an explicit choice. This preserves anti-bias intent while letting power users find specific large DReps.

**Popularity-sort guardrail.** When the user activates the `voting power desc` sort under Show-all, an inline disclosure appears directly above the list (message ID `governance.drepDirectory.showAll.sortBiasWarning`):

> "Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing."

The disclosure dismisses with the same user action that returns to default sort. Dismissal is not persisted — re-activating `voting power desc` shows it again.

Search is always available regardless of cohort and applies fuzzy match on DRep ID prefix only in v1 (slice-6). Verified `givenName` search is deferred until a bulk cohort anchor-prefetch phase populates names for the whole directory; per-DRep lazy anchor fetch (anchor-1) does not make names searchable across unvisited DReps. Search results are sorted by relevance only.

## Directory Identity: ID-Only in v1

v1 directory cards and search are **DRep-ID-only**. Card identity is the
CIP-129-primary truncated ID with a single copy button (`DRepIdDisplay`, shared
tokens §4); the full dual CIP-129 + CIP-105 rendering belongs to the detail view
and the deduped search row, not to the card. No name field exists on the card,
and no card may grow one in v1. The card remains fully usable on ID alone:
status badge (tokens §1), voting power (enriched by load Phase 2; `—` with a
loading/unavailable tooltip until stake lands), on-chain source label, and the
View details / Select for delegation CTAs carry the complete v1 interaction with
no name dependency.

Verified `givenName` (CIP-119) appears **only** in the detail view (anchor-1,
after fetch + hash verification) and in the delegation confirmation.
Directory-wide names and name search await a future bulk anchor-prefetch phase —
see [shared-design-tokens.md §11](shared-design-tokens.md) for the complete v1
ID-search contract and the plan Key Decisions row "Directory names (v1)".

## Hardware Wallet Confirmation

Handed off to existing `VotingPowerDelegation` confirmation. This direction adds nothing new to the HW flow; it inherits everything from shared tokens §7 (identity equality rule — CIP-129 + CIP-105 + signed payload all byte-equal) and §8 (HW sub-states). After a successful delegation, the user lands back on the voting confirmation screen; from there a `View DRep in directory` link returns to `/governance/dreps/:drepId` to inspect ongoing state.

## Accessibility

- `GovernanceWithNavigation` mirrors `StakingWithNavigation` keyboard pattern: arrow keys cycle sub-nav, `Enter` activates.
- Directory list: each card is a `<article>` with `role="group"` and ARIA label "{drepId}, {status}, voting power {amount}".
- Banner is a `<section aria-labelledby="cohort-heading">` with a visually hidden heading so SR users get explicit context.
- Focus management on detail navigation: focus moves to the back-link, then primary heading.
- All status/source visual cues are paired with icon + text (color is decorative).

## Pros / Cons / Risks

**Pros**
- Cleanest IA for future governance surfaces (proposals, constitution, dashboard) without introducing a second governance sidebar destination.
- Each sub-surface gets its own URL → deep-link from notifications/docs is natural.
- Largest screen real estate for detail → best fit for anchor-1/anchor-2 verified anchor content.

**Cons**
- Renames an existing sidebar entry, so established `Voting` muscle memory shifts.
- Most i18n surface area (three pages + nav label).

**Risks**
- Sidebar label rename requires walkthrough, localization, and snapshot updates across the existing voting/governance surface.
- Governance now spans the legacy `/voting/governance` delegation route and the `/governance/*` browse routes, so the `location.state` round-trip contract is release-critical.

## Implementation Effort Delta vs Foundation Baseline

Tasks referenced: task-107 (bare directory components), task-116 (detail), task-112 (selector integration), task-117 (detail route) / nav wiring in slice-1.

| Δ | Reason |
|---|---|
| +4–6h | `GovernanceWithNavigation` layout container + tests |
| +1–2h | Rename existing sidebar entry + active-state logic + a11y |
| +3–4h | Three sub-routes wiring + `location.state` round-trip preservation |
| +1–2h | Extra i18n IDs (nav label, page titles, breadcrumbs) |
| **Total ~9–14h on top of baseline** | |
