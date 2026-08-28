# DRep Discovery — Architecture

## Layer map

```
┌─────────────────────────────────────────────────────────┐
│  Containers (MobX inject + lifecycle)                   │
│  ├─ VotingGovernancePage   /voting/governance           │
│  ├─ DRepDirectoryPage      /governance/dreps            │
│  │  └─ /governance/favorites                            │
│  └─ DRepDetailPage         /governance/dreps/:drepId    │
├─────────────────────────────────────────────────────────┤
│  Presentational components                              │
│  ├─ VotingPowerDelegation  (delegation form)            │
│  ├─ DRepDirectory          (search + list + sentinels)  │
│  │  ├─ DRepDirectorySearch                              │
│  │  ├─ DRepDirectoryFilters                             │
│  │  ├─ DRepDirectoryBanner                              │
│  │  ├─ DRepDirectoryList   (paginated, scroll-to-top)   │
│  │  │  └─ DRepCard  ×N                                  │
│  │  └─ Sentinel cards (Abstain, No Confidence)          │
│  └─ DRepDetail             (full DRep info)             │
│     ├─ DRepDetailOnchainSection                         │
│     ├─ DRepDetailAnchorSection                          │
│     └─ DRepDetailActions   (Select + Favorite toggle)   │
├─────────────────────────────────────────────────────────┤
│  Shared components  (source/renderer/app/components/    │
│                      governance/_shared/)               │
│  ├─ DRepIdDisplay      (CIP-129 / CIP-105 + copy)       │
│  ├─ DRepStatusBadge    (Active / Inactive)              │
│  ├─ DRepCategoryBadge  (High-value / Threshold / …)     │
│  ├─ DRepSourceLabel    (Verified / Unverified / …)      │
│  ├─ DRepEmptyState     (no results, no sync, …)         │
│  └─ DRepErrorBanner                                     │
├─────────────────────────────────────────────────────────┤
│  GovernanceStore  (MobX)                                │
│  ├─ suggestedDReps, allDReps, favoriteDRepIds           │
│  ├─ delegationNavState  (round-trip routing state)      │
│  └─ fetchDRep / fetchSuggestedDReps / loadAllDReps      │
├─────────────────────────────────────────────────────────┤
│  API requests  (source/renderer/app/api/governance/)    │
│  ├─ GET /v2/dreps/suggested?count=N                     │
│  ├─ GET /v2/dreps                                       │
│  └─ GET /v2/dreps/:drepId                               │
└─────────────────────────────────────────────────────────┘
```

## Data flow — Directory view

1. `DRepDirectoryPage.componentDidMount` → `governanceStore.refresh()` (if Idle/Failed)
2. `refresh()` → `fetchSuggestedDReps()` → populates `suggestedDReps`
3. Show-all or search triggers `onLoadAllDReps` → `loadAllDReps()` → `fetchAllDReps()`
4. `DRepDirectory.visibleEntries` memo computes the display set from:
   - Name search: `searchDRepsByName(allDReps, query)`
   - ID prefix search: `searchDRepsByIdPrefix(searchIndex, query)`
   - Show-all: `filterDReps(allDReps, filters)` then `sortDReps(…)`
   - Default: `suggestedDReps` (no filter/sort applied to the cohort)

## Data flow — Delegation round-trip

See [navigation-handoff.md](./navigation-handoff.md).

## File locations

| Area | Path prefix |
|------|-------------|
| Containers | `source/renderer/app/containers/governance/` |
| Voting container | `source/renderer/app/containers/voting/` |
| Directory components | `source/renderer/app/components/governance/drep-directory/` |
| Detail components | `source/renderer/app/components/governance/drep-detail/` |
| Shared governance UI | `source/renderer/app/components/governance/_shared/` |
| Voting form | `source/renderer/app/components/voting/voting-governance/` |
| Store | `source/renderer/app/stores/GovernanceStore.ts` |
| API types | `source/renderer/app/api/governance/types.ts` |
| API requests | `source/renderer/app/api/governance/requests/` |
| Utils | `source/renderer/app/utils/governance/` |
| i18n | `source/renderer/app/i18n/locales/en-US.json` (keys: `governance.drep*`) |
