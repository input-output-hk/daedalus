# Auto-Favorite Feature

A DRep is automatically added to favorites after its delegation submits successfully.
Selecting a candidate in the directory or detail page only carries the selection to the
delegation form; leaving that flow, cancelling, or failing to submit does not save it.

## Two trigger points

### 1. Successful delegation submission (VotingGovernancePage)

**File:** `source/renderer/app/containers/voting/VotingGovernancePage.tsx`

The confirmation dialog calls `onSubmit`, which awaits `voting.delegateVotes` and adds the
chosen DRep only when the returned result has `success: true`. `delegateVotes` also resolves
on failure, returning `success: false`, so completing the promise alone is not sufficient.
The original result is returned to the dialog so its success and error behavior is preserved.

- The same callback handles software and hardware wallets.
- Opening confirmation or preparing an unsigned transaction does not add a favorite.
- Sentinel values (`abstain`, `no_confidence`) are never favorited: they have no DRep identity.
- The `favoriteDRepIds.has(chosenOption)` guard avoids removing an existing favorite.
- Success means submission accepted by the wallet flow, not confirmation on chain.

### 2. Existing delegation when wallet is selected (VotingPowerDelegation + VotingGovernancePage)

A wallet with an existing DRep delegation keeps the convenience of automatically saving
that DRep, including delegations made before this feature was introduced.

**Solution:** `VotingPowerDelegation` fires an effect whenever `currentDRepId` changes
(i.e., when the user selects a wallet that has an existing DRep delegation):

```typescript
// VotingPowerDelegation.tsx
useEffect(() => {
  if (currentDRep?.kind !== 'drep' || !onEnsureFavorited) return;
  onEnsureFavorited(currentDRep.drep.cip129 ?? currentDRep.drep.raw);
}, [currentDRepId, onEnsureFavorited]);
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

In addition to auto-favoring after successful delegation submission, the detail page provides an explicit toggle
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
