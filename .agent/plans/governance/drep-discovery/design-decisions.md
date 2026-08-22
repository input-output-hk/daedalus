# Design Decisions

Non-obvious implementation choices and the reasoning behind them.

---

## Sentinel cards (Abstain, No Confidence)

The Cardano governance spec defines three valid vote delegation targets: a DRep ID, Abstain,
or No Confidence. Abstain and No Confidence are "sentinel" values — they have no DRep identity.

**Implementation:**
- Sentinel cards are rendered below the suggested cohort in the default (non-search, non-show-all) view
- They call `onSelectForDelegation('abstain')` and `onSelectForDelegation('no_confidence')`
- They have no favorite toggle, no metadata, and no "View details"
- The sentinel check `drepId === 'abstain' || drepId === 'no_confidence'` gates auto-favorite
- Sentinel cards are hidden during search and show-all (they are not DReps; filtering would confuse them)
- i18n strings for sentinel *labels* are kept out of directory/favorites namespaces to prevent
  accidental substring matches in search results

**Why not put sentinels in the DRep list?**
Treating them as list entries would require special-casing in search, filter, sort, and pagination.
Separate rendering is simpler and prevents sentinel label text from appearing in search result counts.

---

## Scroll-to-top on pagination and reroll

**File:** `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx`

```typescript
const containerRef = useRef<HTMLDivElement>(null);
const didMount = useRef(false);

useEffect(() => {
  if (!didMount.current) {
    didMount.current = true;
    return;
  }
  containerRef.current?.scrollIntoView?.({ block: 'start', behavior: 'smooth' });
}, [safePage, entries]);
```

- Effect fires on `safePage` or `entries` change (page navigation or reroll/cohort change)
- `didMount` ref skips the initial render so the user's scroll position isn't disrupted on first load
- `?.()` optional call on `scrollIntoView` — JSDOM doesn't implement it, so tests don't throw

**Why scroll to the container, not `window.scrollTo(0,0)`?**
The directory is embedded in a layout that may scroll independently. `scrollIntoView` targets
the list container regardless of how the scroll context is structured.

---

## Verified name sourcing and display

**Sourcing:** The wallet backend fetches and verifies off-chain metadata from the anchor URL,
comparing the document hash to the on-chain anchor hash. Only hash-matching names are served.

**Clamping:** Names are clamped to 80 characters in `GovernanceStore` before storage.
This prevents overflow in all card, list, and detail contexts without per-component truncation.

**Source label in confirmation dialog:** `VotingGovernancePage` extracts the *host* of the
anchor URL (e.g., "example.com") and passes it to `VotingPowerDelegationConfirmationDialog`
as `verifiedName.host`. This tells the user *where* the name claim came from.

```typescript
const resolveVerifiedName = (verifiedName, anchorUrl) => {
  if (verifiedName == null || anchorUrl == null) return null;
  try {
    return { host: new URL(anchorUrl).host, name: verifiedName };
  } catch {
    return null;
  }
};
```

Malformed anchor URLs return `null` rather than showing an empty host — the confirmation falls
back to showing only the DRep ID.

---

## CIP-129 as the canonical ID form

All internal state (store, favorites, delegationNavState) uses the CIP-129 bech32 form as the
canonical DRep ID. CIP-105 (the older form) is derived on demand for display and search index
pre-computation, but never stored.

**Why:** CIP-129 is the current standard. It encodes the credential type in a header byte,
making it the unambiguous form for identifying key-hash vs script-hash DReps.

**Compatibility:** `isSameVoteTarget` compares by credential hex + type, so CIP-105 and CIP-129
forms of the same DRep are recognized as equal when checking the current vote.

---

## Favorites as a plain Set (not a MobX observable Set)

`favoriteDRepIds` is declared as `@observable favoriteDRepIds: Set<string>`. MobX tracks
reference changes on the Set but not mutations. After `toggleFavorite` does `.add()` or
`.delete()`, the store explicitly re-assigns to trigger MobX observers:

```typescript
toggleFavorite(drepId: string) {
  const next = new Set(this.favoriteDRepIds);
  if (next.has(drepId)) {
    next.delete(drepId);
  } else {
    next.add(drepId);
  }
  this.favoriteDRepIds = next; // triggers observers
  api.localStorage.setDRepFavorites([...next]);
}
```

This is why the test mocks can use a plain `Set` — the auto-favorite callbacks check `.has()`
on the same Set instance that was passed to the Provider, not an observable proxy.

---

## No pagination state in the store

The directory's current page number lives in `DRepDirectoryList` component state, not in the
store or URL. This is intentional: page number is ephemeral display state that should reset on
cohort change, reroll, and search. Putting it in the store would require explicit resets in many
places.

---

## Search activates all-DReps fetch

Both name search and ID prefix search operate over `allDReps` (the full index), not just
`suggestedDReps`. This is necessary because:
- A searched DRep may not be in the 20-card suggested cohort
- Name search has no way to know which cohort a DRep is in

The `isSearchActive` flag triggers `onLoadAllDReps()` which calls `governanceStore.loadAllDReps()`.
The no-op guard in `loadAllDReps` means this is safe to call on every keystroke — it only
issues an HTTP request once.

---

## doNotList flag

DReps can set `do_not_list: true` in their off-chain metadata to opt out of the directory.
The store respects this by:
- Excluding `doNotList` entries from `suggestedDReps` and `allDReps` (filtered at the API layer)
- Keeping them in `favoriteDRepIds` if already there (user's choice overrides the DRep's preference)
- Marking them as stale in the favorites view with `isStaleFavorite`
- Still returning them from `fetchDRep` (detail page works for known IDs)
- Still opening them via exact ID search (so users can find and inspect a DRep they know about)
