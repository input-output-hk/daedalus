import { Cardano } from '@cardano-sdk/core';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';

export const MIN_SEARCH_PREFIX_LENGTH = 8;

/**
 * Bech32 data-part lengths of complete DRep IDs (payload words plus the
 * 6-character checksum): 45+6 for the 28-byte CIP-105 form, 47+6 for the
 * 29-byte CIP-129 form. A query at or beyond the shorter one looks like a
 * full ID rather than a prefix.
 */
export const CIP105_DATA_LENGTH = 51;
export const CIP129_DATA_LENGTH = 53;

export type DRepQueryKind =
  | 'empty'
  | 'belowMinimum'
  | 'prefix'
  | 'exactValid'
  | 'invalidFullForm';

export interface NormalizedDRepQuery {
  full: string;
  hrp: 'drep' | 'drep_script' | null;
  data: string;
}

export function normalizeDRepQuery(raw: string): NormalizedDRepQuery {
  const full = raw.trim().toLowerCase();
  if (full.startsWith('drep_script1')) {
    return {
      full,
      hrp: 'drep_script',
      data: full.slice('drep_script1'.length),
    };
  }
  if (full.startsWith('drep1')) {
    return { full, hrp: 'drep', data: full.slice('drep1'.length) };
  }
  return { full, hrp: null, data: full };
}

/**
 * The 8-character minimum counts characters AFTER the HRP, per the shared
 * design tokens search contract. Checksum validity is checked first so the
 * kinds are mutually exclusive.
 */
export function getDRepQueryKind(raw: string): DRepQueryKind {
  const { full, hrp, data } = normalizeDRepQuery(raw);
  if (data.length === 0) return 'empty';
  if (Cardano.DRepID.isValid(full)) return 'exactValid';
  if (hrp !== null && data.length >= CIP105_DATA_LENGTH) {
    return 'invalidFullForm';
  }
  if (data.length < MIN_SEARCH_PREFIX_LENGTH) return 'belowMinimum';
  return 'prefix';
}

export interface DRepSearchIndexEntry {
  entry: AppDRepDirectoryEntry;
  cip129: string;
  cip105: string | null;
}

/**
 * Derives each entry's CIP-105 legacy form once so both encodings are
 * searchable. Entries whose id cannot be re-encoded stay searchable via
 * their canonical CIP-129 form.
 */
export function buildDRepSearchIndex(
  entries: AppDRepDirectoryEntry[]
): DRepSearchIndexEntry[] {
  return entries.map((entry) => {
    let cip105: string | null = null;
    try {
      cip105 = String(
        Cardano.DRepID.toCip105DRepID(Cardano.DRepID(entry.drepId))
      );
    } catch (_error) {
      cip105 = null;
    }
    return { entry, cip129: entry.drepId, cip105 };
  });
}

function compareDRepEntryIdAsc(
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number {
  if (a.drepId < b.drepId) return -1;
  if (a.drepId > b.drepId) return 1;
  return 0;
}

function stripHrp(id: string): string {
  return id.startsWith('drep_script1')
    ? id.slice('drep_script1'.length)
    : id.slice('drep1'.length);
}

/**
 * Prefix search over both encodings. One index row per credential, so a
 * query matching an entry via both forms yields exactly one result; order
 * is a deterministic drepId ascending (v1 relevance is prefix-only).
 */
export function searchDRepsByIdPrefix(
  index: DRepSearchIndexEntry[],
  rawQuery: string
): AppDRepDirectoryEntry[] {
  const kind = getDRepQueryKind(rawQuery);
  if (kind !== 'prefix' && kind !== 'exactValid') return [];
  const { full, hrp, data } = normalizeDRepQuery(rawQuery);
  return index
    .filter(({ cip129, cip105 }) => {
      if (hrp === 'drep_script') {
        return cip105 !== null && cip105.startsWith(full);
      }
      if (hrp === 'drep') {
        return (
          cip129.startsWith(full) ||
          (cip105 !== null && cip105.startsWith(full))
        );
      }
      return (
        stripHrp(cip129).startsWith(data) ||
        (cip105 !== null && stripHrp(cip105).startsWith(data))
      );
    })
    .map(({ entry }) => entry)
    .sort(compareDRepEntryIdAsc);
}

/**
 * Exact-match resolution: checksum-valid full IDs of either encoding are
 * canonicalized to CIP-129 (the store's key form) before lookup. Runs
 * entirely in the renderer — an invalid ID can never reach the main
 * process because nothing here performs IPC.
 */
export function resolveExactDRepMatch<T>(
  rawQuery: string,
  drepIndex: ReadonlyMap<string, T>
): T | null {
  const { full } = normalizeDRepQuery(rawQuery);
  if (!Cardano.DRepID.isValid(full)) return null;
  try {
    const canonical = String(
      Cardano.DRepID.toCip129DRepID(Cardano.DRepID(full))
    );
    return drepIndex.get(canonical) ?? null;
  } catch (_error) {
    return null;
  }
}

export type DRepStatusFilter = 'all' | 'active' | 'inactive';
export type DRepMetadataFilter = 'all' | 'withMetadata' | 'withoutMetadata';
export type DRepExpiryFilter = 'all' | 'thresholdWindow';

export interface DRepFilterState {
  status: DRepStatusFilter;
  metadata: DRepMetadataFilter;
  expiry: DRepExpiryFilter;
  excludeTop35: boolean;
  favoritedOnly: boolean;
}

export const DEFAULT_DREP_FILTER_STATE: DRepFilterState = {
  excludeTop35: false,
  expiry: 'all',
  favoritedOnly: false,
  metadata: 'all',
  status: 'all',
};

export const EMPTY_DREP_ID_SET: ReadonlySet<string> = new Set();

/**
 * The 7-12 remaining-epoch window is restated here on purpose: filter code
 * must never import from the badge module (badges are informational only).
 */
const EXPIRY_WINDOW_MIN = 7;
const EXPIRY_WINDOW_MAX = 12;

export interface DRepFilterContext {
  top35DRepIds: ReadonlySet<string>;
  favoriteDRepIds: ReadonlySet<string>;
}

export function filterDReps(
  entries: AppDRepDirectoryEntry[],
  filters: DRepFilterState,
  context: DRepFilterContext
): AppDRepDirectoryEntry[] {
  return entries.filter((entry) => {
    if (filters.status !== 'all' && entry.status !== filters.status) {
      return false;
    }
    if (filters.metadata === 'withMetadata' && entry.anchor == null) {
      return false;
    }
    if (filters.metadata === 'withoutMetadata' && entry.anchor != null) {
      return false;
    }
    if (
      filters.expiry === 'thresholdWindow' &&
      (entry.drepActivity == null ||
        entry.drepActivity < EXPIRY_WINDOW_MIN ||
        entry.drepActivity > EXPIRY_WINDOW_MAX)
    ) {
      return false;
    }
    if (filters.excludeTop35 && context.top35DRepIds.has(entry.drepId)) {
      return false;
    }
    if (filters.favoritedOnly && !context.favoriteDRepIds.has(entry.drepId)) {
      return false;
    }
    return true;
  });
}

export function isDefaultFilterState(filters: DRepFilterState): boolean {
  return (
    filters.status === 'all' &&
    filters.metadata === 'all' &&
    filters.expiry === 'all' &&
    !filters.excludeTop35 &&
    !filters.favoritedOnly
  );
}

export type DRepSortOption =
  | 'randomized'
  | 'votingPowerDesc'
  | 'votingPowerAsc'
  | 'expiryAsc';

/**
 * Opt-in show-all sorts. 'randomized' returns the input untouched (the
 * seeded session order comes from the store). Null voting power and null
 * activity always sort last; BigNumber comparison keeps lovelace lossless.
 */
export function sortDReps(
  entries: AppDRepDirectoryEntry[],
  sort: DRepSortOption
): AppDRepDirectoryEntry[] {
  if (sort === 'randomized') return entries;
  const sorted = [...entries];
  if (sort === 'expiryAsc') {
    sorted.sort((a, b) => {
      if (a.drepActivity != null && b.drepActivity != null) {
        if (a.drepActivity !== b.drepActivity) {
          return a.drepActivity - b.drepActivity;
        }
      } else if (a.drepActivity != null) {
        return -1;
      } else if (b.drepActivity != null) {
        return 1;
      }
      return compareDRepEntryIdAsc(a, b);
    });
    return sorted;
  }
  const direction = sort === 'votingPowerDesc' ? -1 : 1;
  sorted.sort((a, b) => {
    if (a.votingPower && b.votingPower) {
      const cmp = a.votingPower.comparedTo(b.votingPower);
      if (cmp !== 0) return cmp * direction;
    } else if (a.votingPower) {
      return -1;
    } else if (b.votingPower) {
      return 1;
    }
    return compareDRepEntryIdAsc(a, b);
  });
  return sorted;
}

/**
 * A favorited entry is stale once its status leaves the default-cohort
 * universe. Only the deferred retired status qualifies and no live entry
 * carries it yet; doNotList joins this check when anchor metadata lands.
 */
const STALE_FAVORITE_STATUSES: ReadonlySet<string> = new Set(['retired']);

export function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean {
  return STALE_FAVORITE_STATUSES.has(entry.status);
}
