# DRep Selection Navigation Handoff

## The problem

React Router hash history v4 (`createHashHistory`) **silently discards `location.state`** on
every `history.push()`. Form state passed as location.state to the directory page is gone
the moment the user navigates back. There is no option or workaround in hash history v4.

## The solution: `GovernanceStore.delegationNavState`

A MobX `@observable` on `GovernanceStore` acts as a transport layer for form state across route
boundaries. It survives `history.push()` because it lives in the store, not in the route.

```typescript
@observable delegationNavState: DelegationNavState | null = null;

setDelegationNavState(state: DelegationNavState | null) {
  this.delegationNavState = state;
}
```

```typescript
interface DelegationNavState {
  from?: string;                       // return route (ROUTES.VOTING.GOVERNANCE)
  selectedWalletId?: string | null;    // wallet pre-selection
  voteType?: 'abstain' | 'no_confidence' | 'drep';
  selectedDRepId?: string;             // DRep picked in the directory
  selectedDRepVerifiedName?: string | null;
  selectedDRepAnchorUrl?: string | null;
}
```

## Full round-trip flow

```
VotingGovernancePage
  │
  │  user clicks "Browse DReps"
  │  handleBrowseDRepsClick({ selectedWalletId, voteType })
  │
  ├─ governance.setDelegationNavState({
  │    from: ROUTES.VOTING.GOVERNANCE,
  │    selectedWalletId,
  │    voteType
  │  })
  │
  └─ history.push(ROUTES.GOVERNANCE.DREPS)
           │
           ▼
  DRepDirectoryPage  (reads delegationNavState, does not clear it)
           │
           │  user clicks "Select for delegation" on a DRep row
           │  handleSelectForDelegation(drepId)
           │
           ├─ inherited = pickDelegationFormReturnState(delegationNavState)
           │  // { from, selectedWalletId, voteType }
           │
           ├─ governance.setDelegationNavState({
           │    ...inherited,
           │    selectedDRepId: drepId,
           │    selectedDRepVerifiedName: ...,
           │    selectedDRepAnchorUrl: ...,
           │  })
           │
           └─ history.push(inherited.from ?? ROUTES.VOTING.GOVERNANCE)
                    │
                    ▼
  VotingGovernancePage
           │
           │  const initialFormState = governance.delegationNavState ?? undefined;
           │  // form re-renders with selectedWalletId, selectedDRepId pre-filled
```

### Two-hop: Form → Directory → Detail → Form

If the user goes via the detail page instead of selecting from the list:

1. Directory: "View details" → `history.push(ROUTES.GOVERNANCE.DREP_DETAIL)` — **does NOT** touch `delegationNavState`
2. Detail page reads `delegationNavState` to inherit `from`, `selectedWalletId`, `voteType`
3. "Select for delegation" → sets full `delegationNavState` (same as directory path)
4. Navigates back to `from`

## `delegationFormState.ts`

**File:** `source/renderer/app/containers/governance/delegationFormState.ts`

Two validators guard the shape of `delegationNavState`:

**`pickDelegationFormReturnState(state)`**
Extracts only the fields needed for the return trip (`from`, `selectedWalletId`, `voteType`).
Used in directory and detail containers before merging the selected DRep.

**`pickDelegationFormNavigationState(state)`** (if present)
Validates the full inbound shape. Returns `null` for malformed state.

## Important invariants

- `setDelegationNavState` is the **only** way to update this state — no direct mutation
- The directory page **never clears** `delegationNavState` on mount (the state must survive the navigation)
- `VotingGovernancePage` reads it as `initialFormState` **synchronously on render** (not in a lifecycle hook)
- The state is cleared when the user navigates away from governance pages entirely (GovernanceStore lifecycle reaction)

## Why not use URL params?

DRep IDs are 58+ character bech32 strings. Sentinel values ('abstain', 'no_confidence') are plain strings.
Embedding them in the URL would be safe but adds noise, and the wallet ID is PII-adjacent.
The store observable is the cleanest option given the hash-history constraint.
