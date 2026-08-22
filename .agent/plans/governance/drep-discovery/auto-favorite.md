# Auto-Favorite Feature

When a user picks a DRep for delegation, it is automatically added to their favorites.
This removes friction: the DReps users care about enough to delegate to are almost always ones
they want in their favorites list.

## Three trigger points

### 1. Explicit selection in the directory (DRepDirectoryPage)

**File:** `source/renderer/app/containers/governance/DRepDirectoryPage.tsx`

```typescript
handleSelectForDelegation = (drepId: string) => {
  const governanceStore = this.props.stores?.governance;
  // ... set delegationNavState ...

  const isSentinel = drepId === 'abstain' || drepId === 'no_confidence';
  if (!isSentinel && !governanceStore?.favoriteDRepIds.has(drepId)) {
    governanceStore?.toggleFavorite(drepId);
  }

  this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE);
};
```

- Sentinel values (`'abstain'`, `'no_confidence'`) are never favorited (they have no DRep identity)
- Guard: only calls `toggleFavorite` when not already in favorites (avoids un-toggling)

### 2. Explicit selection in the detail page (DRepDetailPage)

**File:** `source/renderer/app/containers/governance/DRepDetailPage.tsx`

```typescript
handleSelectForDelegation = (drepId: string) => {
  const governanceStore = this.props.stores?.governance;
  // ... set delegationNavState with verifiedName + anchorUrl from detail ...

  if (!governanceStore?.favoriteDRepIds.has(drepId)) {
    governanceStore?.toggleFavorite(drepId);
  }

  this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE);
};
```

No sentinel check needed here — the detail page only shows real DReps.

### 3. Existing delegation when wallet is selected (VotingPowerDelegation + VotingGovernancePage)

**Problem:** Users who delegated to a DRep *before* the auto-favorite feature was introduced will
never have that DRep in favorites. They never go through `handleSelectForDelegation`.

**Solution:** `VotingPowerDelegation` fires an effect whenever `currentVoteDRepId` changes
(i.e., when the user selects a wallet that has an existing DRep delegation):

```typescript
// VotingPowerDelegation.tsx
useEffect(() => {
  if (currentVote?.kind !== 'drep' || !onEnsureFavorited) return;
  onEnsureFavorited(currentVote.drep.cip129 ?? currentVote.drep.raw);
}, [currentVoteDRepId, onEnsureFavorited]);
```

The `onEnsureFavorited` callback is provided by `VotingGovernancePage`:

```typescript
// VotingGovernancePage.tsx
onEnsureFavorited={(drepId) => {
  if (!governance.favoriteDRepIds.has(drepId)) {
    governance.toggleFavorite(drepId);
  }
}}
```

The guard inside the callback (`has()` check) prevents the double-toggle if the DRep is already
in favorites — which would *remove* it (since `toggleFavorite` is a true toggle).

The `cip129 ?? raw` fallback handles legacy DRep delegations stored in CIP-105 form.

## Favorite toggle button on the detail page

In addition to auto-favoring on delegation select, the detail page provides an explicit toggle
so users can manually add/remove a DRep from favorites without delegating.

**File:** `source/renderer/app/components/governance/drep-detail/DRepDetailActions.tsx`

```tsx
<button
  type="button"
  className={styles.favoriteToggle}
  aria-pressed={isFavorite}
  aria-label={intl.formatMessage(
    isFavorite ? messages.favoriteRemove : messages.favoriteAdd
  )}
  onClick={() => onToggleFavorite(drepId)}
>
  <span aria-hidden="true">{isFavorite ? '★' : '☆'}</span>
</button>
```

- `aria-pressed` is `true` (favorited) or `false` (not favorited) — accessible toggle semantics
- `aria-label` switches between "Add to favorites" / "Remove from favorites"
- Wired through `DRepDetail` → `DRepDetailActions` → `onToggleFavorite(drepId)`
- The container (`DRepDetailPage`) provides: `onToggleFavorite={(drepId) => stores.governance.toggleFavorite(drepId)}`

## Where favorites appear

Favorites are pinned in their own group above the suggested cohort on the default directory view, headed with a count, and are visible without switching to Show All or toggling a filter.

The default cohort is a random sample of twenty, so a favorite is usually not in it. Filtering the sample would therefore have shown nothing, which is what review item 2 on PR #3355 reported. Favorites are instead resolved by id through `GovernanceStore.favoriteEntries`, which reads the suggested list, the full list and the per-DRep cache in turn; `ensureFavorites()` fetches any the caches cannot answer for, and the directory page calls it on mount.

A favorite already present in the cohort is not pinned again, so no DRep appears twice. The group is absent entirely when the user has no favorites, and is not rendered in the favorites view, in search results or under Show All, where it would be redundant. An id that cannot be resolved is skipped rather than rendered half-built.

## Favorites persistence

`favoriteDRepIds` is a `Set<string>` (CIP-129 form) synced to localStorage.
See [store.md](./store.md) for `toggleFavorite` and `loadFavorites` details.
