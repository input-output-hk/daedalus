# Routes and Page Components

## Route constants

**File:** `source/renderer/app/routes-config.ts`

```typescript
VOTING: {
  GOVERNANCE: '/voting/governance',   // Delegation form (VotingGovernancePage)
},
GOVERNANCE: {
  ROOT: '/governance',
  DREPS: '/governance/dreps',         // Directory (DRepDirectoryPage)
  DREP_DETAIL: '/governance/dreps/:drepId',  // Detail (DRepDetailPage)
  FAVORITES: '/governance/favorites', // Favorites view (DRepDirectoryPage, view='favorites')
},
```

## Route → component mapping

| Route | Component | Notes |
|-------|-----------|-------|
| `/voting/governance` | `VotingGovernancePage` | Delegation form; reads `delegationNavState` |
| `/governance/dreps` | `DRepDirectoryPage` | Directory view with suggested cohort |
| `/governance/favorites` | `DRepDirectoryPage` | Same page, `view='favorites'` |
| `/governance/dreps/:drepId` | `DRepDetailPage` | Detail for a single DRep |

`DRepDirectoryPage` handles both `/governance/dreps` and `/governance/favorites` via a
`path={[ROUTES.GOVERNANCE.DREPS, ROUTES.GOVERNANCE.FAVORITES]}` route array. It derives
`view` from `location.pathname.startsWith(ROUTES.GOVERNANCE.FAVORITES)`.

## Navigation actions

| User action | From page | Store mutation | `history.push` |
|-------------|-----------|---------------|----------------|
| "Browse DReps" | `VotingGovernancePage` | `setDelegationNavState({ from, selectedWalletId, voteType })` | `ROUTES.GOVERNANCE.DREPS` |
| "View details" | `DRepDirectoryPage` | none | `ROUTES.GOVERNANCE.DREP_DETAIL` (with drepId) |
| "Select for delegation" | `DRepDirectoryPage` | `setDelegationNavState({ ...inherited, selectedDRepId, ... })` | `inherited.from ?? ROUTES.VOTING.GOVERNANCE` |
| "Back to directory" | `DRepDetailPage` | none | `ROUTES.GOVERNANCE.DREPS` |
| "Select for delegation" | `DRepDetailPage` | `setDelegationNavState({ ...inherited, selectedDRepId, ... })` | `inherited.from ?? ROUTES.VOTING.GOVERNANCE` |
| Back to directory link | `DRepDetailPage` | none | `ROUTES.GOVERNANCE.DREPS` |
| "Back to directory" | Favorites empty state | none | `ROUTES.GOVERNANCE.DREPS` |

## Container responsibilities

### `DRepDirectoryPage`
- Calls `governanceStore.refresh()` on mount (if Idle or Failed)
- Sets up a MobX reaction on `isNodeInSync` → calls `refresh()` once on `false→true`
- Disposes the sync reaction in `componentWillUnmount`
- Routes "View details" to `history.push(ROUTES.GOVERNANCE.DREPS/${drepId})`
- Routes "Select" to `handleSelectForDelegation` (auto-favorites, sets nav state, navigates)

### `DRepDetailPage`
- Calls `governanceStore.fetchDRep(drepId)` on mount; stores result in local component state
- Calls `governanceStore.refresh()` on mount (if Idle or Failed)
- Same sync reaction pattern as directory
- Routes "Select for delegation" to `handleSelectForDelegation`
- Routes "Back to directory" to `ROUTES.GOVERNANCE.DREPS`

### `VotingGovernancePage`
- Reads `governance.delegationNavState` synchronously on render as `initialFormState`
- Does not clear `delegationNavState` — the store's lifecycle reaction handles cleanup
- Calls `governance.setDelegationNavState(...)` when "Browse DReps" is clicked
- Provides `onEnsureFavorited` to `VotingPowerDelegation` for auto-favoring existing delegations
