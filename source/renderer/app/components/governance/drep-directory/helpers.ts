import { Cardano } from '@cardano-sdk/core';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { hasVerifiedMetadata } from '../_shared/drepMetadata';
import { getDRepStanding } from '../_shared/drepExpiry';
import type { DRepStanding } from '../_shared/drepExpiry';

export const MIN_SEARCH_PREFIX_LENGTH = 8;
export const MIN_NAME_SEARCH_LENGTH = 2;

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
  | 'name'
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
 *
 * Queries with no DRep HRP (no "drep1" / "drep_script1" prefix) are treated
 * as name searches when they meet the minimum length.
 */
export function getDRepQueryKind(raw: string): DRepQueryKind {
  const { full, hrp, data } = normalizeDRepQuery(raw);
  if (data.length === 0) return 'empty';
  if (Cardano.DRepID.isValid(full)) return 'exactValid';
  if (hrp !== null && data.length >= CIP105_DATA_LENGTH) {
    return 'invalidFullForm';
  }
  if (hrp === null) {
    return data.length >= MIN_NAME_SEARCH_LENGTH ? 'name' : 'belowMinimum';
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
 * Substring name search against the verified off-chain name. Only entries
 * that carry a verified name are searched; ID-only entries are excluded.
 */
export function searchDRepsByName(
  entries: AppDRepDirectoryEntry[],
  rawQuery: string
): AppDRepDirectoryEntry[] {
  const q = rawQuery.trim().toLowerCase();
  if (q.length < MIN_NAME_SEARCH_LENGTH) return [];
  return entries.filter(
    (entry) =>
      entry.verifiedName != null && entry.verifiedName.toLowerCase().includes(q)
  );
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

/**
 * The badge's own three states, plus everything.
 *
 * Filtering on the raw registration status let the filter and the badge
 * disagree: a DRep whose registration is active but whose voting power is
 * close to lapsing is badged "Inactive Soon", and picking "Active" returned it
 * anyway. Reading the same standing the badge renders means the two cannot say
 * different things about one card.
 */
export type DRepStatusFilter = 'all' | DRepStanding;
export type DRepMetadataFilter = 'all' | 'withMetadata' | 'withoutMetadata';
export interface DRepFilterState {
  status: DRepStatusFilter;
  metadata: DRepMetadataFilter;
}

export const DEFAULT_DREP_FILTER_STATE: DRepFilterState = {
  metadata: 'all',
  status: 'all',
};

export function filterDReps(
  entries: AppDRepDirectoryEntry[],
  filters: DRepFilterState
): AppDRepDirectoryEntry[] {
  return entries.filter((entry) => {
    if (
      filters.status !== 'all' &&
      getDRepStanding(entry.status, entry.drepActivity) !== filters.status
    ) {
      return false;
    }
    // Verified metadata, not merely an anchor: an anchor is a URL and a hash
    // recorded on chain, and says nothing about whether the content behind it
    // was retrieved or matched. Filtering on the anchor while the badge read
    // the verified name is what filtered a DRep in as having metadata and then
    // labelled it as having none.
    if (filters.metadata === 'withMetadata' && !hasVerifiedMetadata(entry)) {
      return false;
    }
    if (filters.metadata === 'withoutMetadata' && hasVerifiedMetadata(entry)) {
      return false;
    }
    return true;
  });
}

export function isDefaultFilterState(filters: DRepFilterState): boolean {
  return filters.status === 'all' && filters.metadata === 'all';
}

export type DRepSortOption = 'default' | 'votingPowerDesc' | 'votingPowerAsc';

/**
 * The opt-in sorts. 'default' returns the input untouched: the banded, seeded
 * order is built in the store rather than here.
 *
 * Voting power only. Ordering by how close a DRep is to going inactive was
 * offered and taken away again: nobody chooses a representative by how soon
 * their registration lapses, and the state that matters is already a badge on
 * every card and a filter beside it. Daedalus is a wallet rather than a
 * governance explorer, and every option here has to earn a reader's attention.
 *
 * Null voting power sorts last; BigNumber comparison keeps lovelace lossless.
 */
export function sortDReps(
  entries: AppDRepDirectoryEntry[],
  sort: DRepSortOption
): AppDRepDirectoryEntry[] {
  if (sort === 'default') return entries;
  const sorted = [...entries];
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
 * A favorited entry is stale once it leaves the default-cohort universe:
 * a verified anchor asking not to be listed, or the still-deferred retired
 * status that no live entry carries yet.
 */
const STALE_FAVORITE_STATUSES: ReadonlySet<string> = new Set(['retired']);

export function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean {
  return entry.doNotList || STALE_FAVORITE_STATUSES.has(entry.status);
}
