# DRep Search, Filter, and Sort

**File:** `source/renderer/app/components/governance/drep-directory/helpers.ts`

## Search constants

```typescript
export const MIN_SEARCH_PREFIX_LENGTH = 8;  // Min chars after HRP for ID prefix search
export const MIN_NAME_SEARCH_LENGTH = 2;    // Min chars for name search
export const CIP105_DATA_LENGTH = 51;       // 28-byte payload + 6-char checksum
export const CIP129_DATA_LENGTH = 53;       // 29-byte payload + 6-char checksum
```

## Query classification: `getDRepQueryKind`

```typescript
type DRepQueryKind =
  | 'empty'           // blank or HRP-only ("drep1")
  | 'belowMinimum'    // too short for the detected type
  | 'prefix'          // HRP + 8–50 data chars (valid prefix range)
  | 'name'            // no HRP, 2+ chars → name search
  | 'exactValid'      // full-length ID with passing bech32 checksum
  | 'invalidFullForm' // full-length ID with failing checksum
```

**Classification rules (applied in order):**

1. `normalizeDRepQuery` trims, lowercases, detects HRP (`drep`, `drep_script`, or `null`)
2. If checksum-valid full ID (`CIP105_DATA_LENGTH` or `CIP129_DATA_LENGTH` data chars) → `exactValid`
3. If full-length data but bad checksum → `invalidFullForm`
4. HRP detected:
   - `< MIN_SEARCH_PREFIX_LENGTH` data chars → `belowMinimum`
   - `≥ MIN_SEARCH_PREFIX_LENGTH` → `prefix`
5. No HRP:
   - `< MIN_NAME_SEARCH_LENGTH` chars → `belowMinimum`
   - `≥ MIN_NAME_SEARCH_LENGTH` → `name`
6. Empty → `empty`

**`isSearchActive`** in `DRepDirectory.tsx`:
```typescript
const isSearchActive =
  queryKind === 'prefix' ||
  queryKind === 'exactValid' ||
  queryKind === 'invalidFullForm' ||
  queryKind === 'name';
```
`belowMinimum` and `empty` do NOT activate search — the directory renders its normal suggested view.

## ID prefix search: `searchDRepsByIdPrefix`

```typescript
function searchDRepsByIdPrefix(
  index: DRepSearchIndexEntry[],
  rawQuery: string
): AppDRepDirectoryEntry[]
```

- Only runs when `queryKind === 'prefix'`
- Matches the normalized query against the CIP-129 or CIP-105 form of each indexed entry
- **Deduplication:** one entry per credential (a CIP-105 query returns the same row as a CIP-129 query for the same DRep)
- Results sorted by `drepId` ascending for determinism

**Building the search index:**
```typescript
function buildDRepSearchIndex(entries: AppDRepDirectoryEntry[]): DRepSearchIndexEntry[]
```
Pre-computes the CIP-105 twin for each CIP-129 ID (bech32 re-encode, wrapped in try-catch).
`DRepDirectory` rebuilds the index only when `allDReps` changes (via `useMemo`).

## Exact match: `resolveExactDRepMatch`

```typescript
function resolveExactDRepMatch<T extends AppDRepDirectoryEntry>(
  rawQuery: string,
  drepIndex: Map<string, T>
): T | null
```

- Only called when `queryKind === 'exactValid'`
- Canonicalizes the query to CIP-129 form, looks it up in the Map
- If found, `DRepDirectory` calls `onViewDetails(match.drepId)` to auto-navigate
- A de-bounce ref (`lastOpenedQueryRef`) prevents re-triggering on the same query

## Name search: `searchDRepsByName`

```typescript
function searchDRepsByName(
  entries: AppDRepDirectoryEntry[],
  rawQuery: string
): AppDRepDirectoryEntry[]
```

- Only runs when `queryKind === 'name'`
- Case-insensitive substring match on `entry.verifiedName`
- **Entries with `verifiedName === null` are excluded from name results**
- No sorting applied (preserves original order)

## `visibleEntries` computation in `DRepDirectory`

```typescript
const visibleEntries = useMemo(() => {
  let base: AppDRepDirectoryEntry[];

  if (isSearchActive) {
    base = queryKind === 'name'
      ? searchDRepsByName(allDReps, searchQuery)
      : searchDRepsByIdPrefix(searchIndex, searchQuery);
  } else if (isShowAll) {
    base = allDReps;
  } else {
    base = suggestedDReps; // default: 20-card randomized cohort
  }

  const filtered = filterDReps(base, filters, { favoriteDRepIds });

  // Sort only in show-all mode (not search, not suggested)
  if (isSearchActive || !isShowAll) return filtered;
  return sortDReps(filtered, sort);
}, [allDReps, favoriteDRepIds, filters, isSearchActive, isShowAll,
    queryKind, searchIndex, searchQuery, sort, suggestedDReps]);
```

When search is active or show-all is off, sort is not applied (cohort order is preserved).

## Filter: `filterDReps`

```typescript
interface DRepFilterState {
  status: 'all' | 'active' | 'inactive';
  metadata: 'all' | 'withMetadata' | 'withoutMetadata';
  expiry: 'all' | 'hideLapsingSoon';
}
```

- `status` — matches `entry.status`
- `metadata` — reads `hasVerifiedMetadata(entry)` from `_shared/drepMetadata.ts`, the same predicate the badge reads. Verified metadata, not anchor presence: an anchor is a URL and a hash recorded on-chain and says nothing about whether the content behind it was retrieved or matched, so filtering on the anchor admitted DReps the card then labelled as having no metadata.
- `expiry` — `hideLapsingSoon` excludes `isLapsingSoon(entry.drepActivity)` from `_shared/drepExpiry.ts`, which is six or fewer epochs remaining. This is the same threshold the status badge and the delegation form's current-DRep panel use. Entries whose remaining epochs are unknown are kept rather than hidden. The filter is inert on the default cohort by design, since the cohort floor already excludes anything at or below six epochs; it does its work in show-all, search and the pinned favourites group.

There is no `favoritedOnly` filter. Favourites are pinned above the cohort instead, resolved by id; see [auto-favorite.md](./auto-favorite.md).

The default filter state (`DEFAULT_DREP_FILTER_STATE`) sets every option to `'all'`.
`isDefaultFilterState(filters)` returns `true` when nothing is filtered.

## Sort: `sortDReps`

| Option | Behaviour |
|--------|-----------|
| `'randomized'` | Returns input array untouched (preserves session-seeded order) |
| `'votingPowerDesc'` | Highest to lowest; null last |
| `'votingPowerAsc'` | Lowest to highest; null last |
| `'expiryAsc'` | Soonest to expire first; null last |

BigNumber comparison is used for voting power to avoid float precision loss at high lovelace values.
`sortDReps` does **not** mutate the input array.

## Stale-favorite detection: `isStaleFavorite`

```typescript
function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean
```

Returns `true` when:
- `entry.doNotList === true` — DRep has opted out of delegation listings

Stale entries are shown in the favorites view with a warning caption.
They remain in favorites (no auto-removal) but the user is warned they can't delegate to them.

## Show-all lazy load trigger

When `isShowAll` or `isSearchActive` becomes true, `DRepDirectory` calls `onLoadAllDReps`.
The container delegates to `governanceStore.loadAllDReps()`, which no-ops if already loading or loaded.
This keeps the full DRep list fetch lazy (not fetched on directory open unless needed).
