# GovernanceStore Reference

**File:** `source/renderer/app/stores/GovernanceStore.ts`

## Key types

```typescript
export type GovernanceRefreshState =
  | 'Idle' | 'Loading' | 'Refreshing' | 'Loaded' | 'Failed';

export interface AppDRepDirectoryEntry {
  drepId: string;          // CIP-129 canonical form
  votingPower: BigNumber | null;
  status: 'active' | 'inactive';
  drepActivity: number | null;  // epochs until expiry
  anchor: { url: string; hash: string } | null;
  verifiedName: string | null;  // clamped to 80 chars
  doNotList: boolean;
}

export interface AppDRepDetail extends AppDRepDirectoryEntry {
  metadata: {
    objectives: string | null;
    motivations: string | null;
    qualifications: string | null;
    paymentAddress: string | null;
    references: ApiDRepMetaReference[];
  } | null;
}

export interface DelegationNavState {
  from?: string;                       // ROUTES.VOTING.GOVERNANCE | ROUTES.GOVERNANCE.DREPS
  selectedWalletId?: string | null;
  voteType?: 'abstain' | 'no_confidence' | 'drep';
  selectedDRepId?: string;
  selectedDRepVerifiedName?: string | null;
  selectedDRepAnchorUrl?: string | null;
}

export interface GovernanceStoreError {
  type: string;   // e.g. 'SELFNODE_CLI_UNSUPPORTED'
  message: string;
  details?: string;
}
```

## Observable state

| Observable | Type | Description |
|-----------|------|-------------|
| `suggestedDReps` | `AppDRepDirectoryEntry[]` | Default 20-card cohort from `/v2/dreps/suggested` |
| `refreshState` | `GovernanceRefreshState` | Load state of the suggested cohort |
| `error` | `GovernanceStoreError \| null` | Last error from suggested fetch |
| `lastFetchedAt` | `number \| null` | Timestamp of last successful fetch |
| `allDReps` | `AppDRepDirectoryEntry[]` | Full index from `/v2/dreps` (lazy-loaded) |
| `allDRepsRefreshState` | `GovernanceRefreshState` | Load state of the full list |
| `allDRepsError` | `GovernanceStoreError \| null` | Last error from full-list fetch |
| `drepSummary` | `{ totalDRepStake, activeDRepCount, inactiveDRepCount, totalDRepCount } \| null` | Aggregate totals from `/v2/dreps/summary`; excludes the predefined abstain and no-confidence targets |
| `drepSummaryState` | `GovernanceRefreshState` | Load state of the summary, tracked apart from `refreshState` so a permanently failing summary is distinguishable from one never requested |
| `favoriteDRepIds` | `Set<string>` | DRep IDs the user has starred (CIP-129 form) |
| `delegationNavState` | `DelegationNavState \| null` | Round-trip routing state; see [navigation-handoff.md](./navigation-handoff.md) |

## Methods

### Data fetching

**`refresh()`**
Re-fetches the suggested cohort (also resets `allDReps` load state so show-all re-fetches on next open).
Called on component mount and when node syncs.

**`fetchSuggestedDReps(count = 20)`**
Issues `GET /v2/dreps/suggested?count={count}`. Guards against concurrent calls.
Retains previous data on failure so the UI doesn't blank.

**`loadAllDReps()`**
No-op if `allDRepsRefreshState` is already Loading or Loaded.
Otherwise calls `fetchAllDReps()`.

**`fetchDRep(drepId: string): Promise<AppDRepDirectoryEntry>`**
Issues `GET /v2/dreps/{drepId}`. Returns the enriched detail.
**Not an action** — does not mutate store state. Callers own the result.

### Favorites

**`toggleFavorite(drepId: string)`**
Adds `drepId` if absent, removes it if present.
Persists to localStorage via `api.localStorage.setDRepFavorites([...next])`.

**`loadFavorites()`**
Hydrates `favoriteDRepIds` from localStorage on store setup.
Silently ignores errors (non-critical per-device state).

### Navigation handoff

**`setDelegationNavState(state: DelegationNavState | null)`**
Replaces the round-trip state. Call before `history.push()` to carry form state across routes.
See [navigation-handoff.md](./navigation-handoff.md).

## Computed values

| Computed | Description |
|---------|-------------|
| `isLoading` | `refreshState === 'Loading'` |
| `isRefreshing` | `refreshState === 'Refreshing'` |
| `isLoaded` | `refreshState === 'Loaded'` |
| `hasError` | `refreshState === 'Failed'` |
| `isEmpty` | Loaded with zero suggested DReps |
| `isDRepSummaryAvailable` | Summary loaded and non-null; gates anything derived from total DRep stake |
| `isGovernancePage` | Current route starts with `/governance` (used by lifecycle reaction) |

## Lifecycle

- **`setup()`** — Calls `loadFavorites()`. Sets up a reaction that clears all store state
  when the user navigates away from governance pages entirely.
- **Suggested refresh** is triggered by containers on mount and on `isNodeInSync` flip.
- **All-DReps** fetch is triggered by `DRepDirectory` when show-all mode or search becomes active.

## Verified name clamping

Names from metadata are clamped at **80 characters** with an ellipsis appended.
This happens inside `fetchDRep` / `fetchSuggestedDReps` before the entry is stored.
The clamp prevents layout overflow in all display contexts.
