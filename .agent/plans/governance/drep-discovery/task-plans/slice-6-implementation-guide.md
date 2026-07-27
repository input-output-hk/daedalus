# Slice-6 Implementation Guide: DRep ID Search + Show-All / Reachability Filters

> **Companion PRD:** [slice-6-PRD.md](./slice-6-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/slice-6` (branch `wt/slice-6`, base
> `6f828d573`) on 2026-07-24. Re-verify an anchor only if the file was touched by
> an earlier step of this same guide.

---

## Implementation Order

```
task-121 (helpers + store computeds + search/filter components + directory pipeline
          + banner filtered mode + noResults + i18n + Jest + harness fixes)
```

Single task. Its only dependency, task-118, is `complete`.

## Cross-Cutting Renderer Note (applies to every step)

- **react-intl is 2.9.0**: use `injectIntl` / `intlShape` / `defineMessages` /
  `FormattedMessage`. Never `useIntl()` or any react-intl hook.
- **Locked invariants stated inline (do not skip):**
  - **#2 sanitization floor** — never pass a DRep ID, `abstain`/`no_confidence`
    literal, or any CIP-129/CIP-105 bech32 string to `logger.*`, analytics, or an
    electron-store write. **Never log the search query.** Every new code path in
    this slice makes ZERO logger/analytics/storage calls. The 23-test spy suite
    `tests/jest/security/governance-sanitization.spec.ts` must stay 23/23 and is
    NEVER edited.
  - **#5 lovelace losslessness** — voting-power sorting compares `BigNumber`s via
    `comparedTo`. Never call `.toNumber()`, never `Number(votingPower)`, never
    subtract BigNumbers into a `Number`.
  - **#6 CLI discipline** — this slice contains **no** main-process, IPC, or CLI
    change. Do NOT touch anything under `source/main/`. Search, filters, sorts,
    and show-all are pure renderer derivations of the already-loaded
    `drepList` / `drepIndex`. Jest pins that no channel request and no `refresh()`
    fires during search interactions.
  - **#7 default cohort binding** — the default view stays byte-identical to
    slice-5: `displayedDRepList` (`GovernanceStore.ts:178-180`) is untouched; the
    cohort rule (top-35 excluded, ≤200 eligible with `drepActivity > 6`,
    seeded-random) is not modified. Show-all and search are explicit user actions.
    No fixture may place a sub-floor DRep inside a *cohort*; sub-floor fixtures
    appear only in show-all/search reachability assertions.
  - **#8 badges informational** — never import `DRepCategoryBadge` or
    `getDRepCategory` from any filtering/ordering code. `helpers.ts` restates the
    7–12 window constants locally (Step 1 does this).
  - **#11 preliminary copy** — every NEW en-US and ja-JP string starts with `!!!`.
    Never strip an existing `!!!`.
  - **#12 reachability** — top-35, sub-floor, and inactive DReps must be reachable
    via show-all, via ≥8-char search, and via exact full-ID entry. Jest pins each
    path.
  - **#14 status grounding** — do not touch `DRepStatus`
    (`source/common/types/governance.types.ts:35`), `DRepStatusBadge.tsx`, or
    `DRepCategoryBadge.tsx`. No `expiring` status, no top-35 badge (PRD D-5/P-13).
- **Code comments**: only where logic is not self-evident; 1–3 plain lines stating
  the why/invariant. No task IDs, no review labels, no ALL-CAPS tags, no history.
- **Jest assertion style**: never `toHaveBeenCalledWith('str', { literal: 'obj' })`
  (prettier 2.1.2 oscillates) — always `expect.objectContaining({ … })` for object
  arguments.
- **Verification commands** (run from the worktree root
  `/workspaces/daedalus/.agent/worktrees/slice-6`):
  - **`npx` DOES NOT WORK in this devcontainer** (slice-4 finding F-6). Invoke
    every tool as `node_modules/.bin/<tool>` or `yarn <tool>`.
  - Typecheck: `node_modules/.bin/tsc --noEmit` — must exit 0 with ZERO errors
    (`yarn compile` is unreliable under Node 24 — do not use it).
  - Lint: `node_modules/.bin/eslint <touched paths> --ext .ts,.tsx`.
  - Focused Jest: `node_modules/.bin/jest <spec paths> --no-coverage --runInBand`.
  - Sanitization floor: `node_modules/.bin/jest
    tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand`
    → **23/23 at baseline and after the task; the suite file is never edited**.
  - Copy changes: `yarn i18n:manage` (works under Node 24 — ux-refinement F-7; it
    rewrites `source/renderer/app/i18n/defaultMessages.json` and
    `translations/messages.json` — those diffs ride with the task commit; never
    hand-edit or prettier those files or the locale JSONs).
  - Format: `node_modules/.bin/prettier --write` on the changed `.ts/.tsx/.scss`
    files ONLY (never JSON, locale files, or `.snap`). Collateral rewraps of
    pre-existing drift inside touched files are formatting-only — keep them.
- **Never commit `.scss.d.ts` files.** The global `declare module '*.scss'` types
  the two new SCSS modules.
- **Commit**: exactly one, subject-only Conventional Commits, no body, no trailers.
  Never push, never open a PR.

---

## task-121: DRep ID search + show-all / reachability filters

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/components/governance/drep-directory/helpers.ts` | CREATE |
| 2 | `source/renderer/app/components/governance/drep-directory/helpers.spec.ts` | CREATE |
| 3 | `source/renderer/app/stores/GovernanceStore.ts` | EDIT |
| 4 | `source/renderer/app/components/governance/drep-directory/DRepDirectorySearch.tsx` | CREATE |
| 5 | `source/renderer/app/components/governance/drep-directory/DRepDirectorySearch.scss` | CREATE |
| 6 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.tsx` | CREATE |
| 7 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.scss` | CREATE |
| 8 | `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx` | EDIT (replace file) |
| 9 | `source/renderer/app/components/governance/_shared/DRepEmptyState.scss` | EDIT (append) |
| 10 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | EDIT (replace file) |
| 11 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.scss` | EDIT (append) |
| 12 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx` | EDIT |
| 13 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss` | EDIT (append) |
| 14 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | EDIT |
| 15 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 16 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |
| 17 | `tests/jest/governance/GovernanceStore.spec.ts` | EDIT (append describe) |
| 18 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT |
| 19 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx` | EDIT (append) |
| 20 | `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | EDIT |
| 21 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (2 mock fields only) |
| 22 | `storybook/stories/governance/DRepDirectory.stories.tsx` | EDIT (compile fixes only) |

Do **NOT** touch: anything under `source/main/`, the sanitization suite,
`seededShuffle.ts`, `DRepCard.tsx`, `DRepIdDisplay.tsx`, `DRepStatusBadge.tsx`,
`DRepCategoryBadge.tsx`, `DRepDetail*`, `DRepDirectoryList.tsx`,
`governance.types.ts`, `DRepDirectoryBanner.stories.tsx` (its new props are
optional — Step 12), or any locale key that already exists.

### Step-by-Step

#### Step 1: Create `source/renderer/app/components/governance/drep-directory/helpers.ts`

Pattern precedent: `source/renderer/app/components/staking/stake-pools/helpers.ts`
(:1-24) — pure, exported filter functions co-located with the components.
`@cardano-sdk/core` is already a renderer dependency
(`VotingPowerDelegation.tsx:9`); add **no** new package. Full file contents:

```ts
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
    if (
      filters.favoritedOnly &&
      !context.favoriteDRepIds.has(entry.drepId)
    ) {
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
```

Invariant #2: nothing here logs. Invariant #5: only `comparedTo`. Invariant #8:
no badge import — the window constants are local.

#### Step 2: Create `helpers.spec.ts` (co-located)

Fixture ids are generated from credentials so both encodings are checksum-valid
twins of the same credential (`GovernanceQueryService.ts:624-641` uses the same
API with the same `as any` cast for the hash type). Full file contents:

```ts
import BigNumber from 'bignumber.js';
import { Cardano } from '@cardano-sdk/core';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import {
  EMPTY_DREP_ID_SET,
  DEFAULT_DREP_FILTER_STATE,
  buildDRepSearchIndex,
  filterDReps,
  getDRepQueryKind,
  isDefaultFilterState,
  normalizeDRepQuery,
  resolveExactDRepMatch,
  searchDRepsByIdPrefix,
  sortDReps,
} from './helpers';

// Distinct-from-the-first-byte hashes: prefix queries built from one id
// must not accidentally match another fixture's id.
const credHash = (n: number) =>
  n.toString(16).padStart(2, '0').repeat(28).slice(0, 56);

// Shared-prefix hashes (leading zeros): used ONLY by the ordering test,
// where one query must match several entries.
const sharedCredHash = (n: number) => n.toString(16).padStart(56, '0');

const cip129FromHash = (hash: string): string =>
  String(
    Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash,
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );

const cip129At = (n: number): string => cip129FromHash(credHash(n));

const cip105At = (n: number): string =>
  String(
    Cardano.DRepID.cip105FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );

const buildEntry = (
  n: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  anchor: null,
  drepActivity: 20,
  drepId: cip129At(n),
  status: 'active',
  votingPower: null,
  ...overrides,
});

describe('normalizeDRepQuery', () => {
  it('splits HRP-qualified queries and lowercases/trims input', () => {
    expect(normalizeDRepQuery('  DRep1AbCdEfGh ')).toEqual({
      data: 'abcdefgh',
      full: 'drep1abcdefgh',
      hrp: 'drep',
    });
    expect(normalizeDRepQuery('drep_script1xyz')).toEqual({
      data: 'xyz',
      full: 'drep_script1xyz',
      hrp: 'drep_script',
    });
    expect(normalizeDRepQuery('abcdefgh')).toEqual({
      data: 'abcdefgh',
      full: 'abcdefgh',
      hrp: null,
    });
  });
});

describe('getDRepQueryKind', () => {
  it('classifies empty and below-minimum queries counting post-HRP characters', () => {
    expect(getDRepQueryKind('')).toBe('empty');
    expect(getDRepQueryKind('drep1')).toBe('empty');
    // 7 characters after the HRP: below the 8-character minimum.
    expect(getDRepQueryKind('drep1abcdefg')).toBe('belowMinimum');
    expect(getDRepQueryKind('abcdefg')).toBe('belowMinimum');
  });

  it('classifies 8 post-HRP characters as a prefix', () => {
    expect(getDRepQueryKind('drep1abcdefgh')).toBe('prefix');
    expect(getDRepQueryKind('abcdefgh')).toBe('prefix');
  });

  it('classifies checksum-valid full IDs of both encodings as exactValid', () => {
    expect(getDRepQueryKind(cip129At(1))).toBe('exactValid');
    expect(getDRepQueryKind(cip105At(1))).toBe('exactValid');
  });

  it('classifies full-form-shaped strings with a bad checksum as invalidFullForm', () => {
    expect(getDRepQueryKind(`drep1${'q'.repeat(51)}`)).toBe('invalidFullForm');
    // Any single-character substitution breaks a bech32 checksum.
    const valid = cip129At(1);
    const corrupted =
      valid.slice(0, -1) + (valid.endsWith('q') ? 'p' : 'q');
    expect(getDRepQueryKind(corrupted)).toBe('invalidFullForm');
    // 50 post-HRP characters are still a (non-matching) prefix, not full-form.
    expect(getDRepQueryKind(`drep1${'q'.repeat(50)}`)).toBe('prefix');
    // Without an HRP a string can never be full-form.
    expect(getDRepQueryKind('q'.repeat(60))).toBe('prefix');
  });
});

describe('buildDRepSearchIndex', () => {
  it('derives the CIP-105 twin for every canonical CIP-129 id', () => {
    const index = buildDRepSearchIndex([buildEntry(1), buildEntry(2)]);

    expect(index).toHaveLength(2);
    expect(index[0].cip129).toBe(cip129At(1));
    expect(index[0].cip105).toBe(cip105At(1));
  });

  it('keeps entries with unencodable ids searchable via CIP-129', () => {
    const index = buildDRepSearchIndex([
      buildEntry(1, { drepId: 'not-a-bech32-id' }),
    ]);

    expect(index[0].cip105).toBeNull();
    expect(index[0].cip129).toBe('not-a-bech32-id');
  });
});

describe('searchDRepsByIdPrefix', () => {
  const entries = [buildEntry(1), buildEntry(2), buildEntry(3)];
  const index = buildDRepSearchIndex(entries);

  it('matches a CIP-129 prefix of at least 8 post-HRP characters', () => {
    const query = cip129At(1).slice(0, 'drep1'.length + 12);
    const result = searchDRepsByIdPrefix(index, query);

    expect(result.map((e) => e.drepId)).toContain(cip129At(1));
  });

  it('dedupes by credential: the CIP-105 form finds the same single row', () => {
    const via129 = searchDRepsByIdPrefix(
      index,
      cip129At(2).slice(0, 'drep1'.length + 20)
    );
    const via105 = searchDRepsByIdPrefix(
      index,
      cip105At(2).slice(0, 'drep1'.length + 20)
    );

    expect(via129).toHaveLength(1);
    expect(via105).toHaveLength(1);
    expect(via105[0].drepId).toBe(via129[0].drepId);
  });

  it('returns nothing for below-minimum, invalid-full-form, and empty queries', () => {
    expect(searchDRepsByIdPrefix(index, '')).toEqual([]);
    expect(searchDRepsByIdPrefix(index, 'drep1abcdefg')).toEqual([]);
    expect(searchDRepsByIdPrefix(index, `drep1${'q'.repeat(51)}`)).toEqual([]);
  });

  it('orders multi-match results deterministically by drepId ascending', () => {
    // Shared-prefix fixtures: one 8-character query matches all three.
    const sharedEntries = [3, 1, 2].map((n) =>
      buildEntry(n, { drepId: cip129FromHash(sharedCredHash(n)) })
    );
    const sharedIndex = buildDRepSearchIndex(sharedEntries);
    const query = sharedEntries[0].drepId.slice(0, 'drep1'.length + 8);
    const broad = searchDRepsByIdPrefix(sharedIndex, query);

    expect(broad).toHaveLength(3);
    expect(broad.map((e) => e.drepId)).toEqual(
      [...broad.map((e) => e.drepId)].sort()
    );
  });
});

describe('resolveExactDRepMatch', () => {
  const entry = buildEntry(1);
  const drepIndex = new Map([[entry.drepId, entry]]);

  it('resolves a canonical CIP-129 id', () => {
    expect(resolveExactDRepMatch(cip129At(1), drepIndex)).toBe(entry);
  });

  it('canonicalizes a CIP-105 form to the same entry', () => {
    expect(resolveExactDRepMatch(cip105At(1), drepIndex)).toBe(entry);
  });

  it('returns null for valid-but-unknown, invalid, and prefix inputs', () => {
    expect(resolveExactDRepMatch(cip129At(9), drepIndex)).toBeNull();
    expect(
      resolveExactDRepMatch(`drep1${'q'.repeat(51)}`, drepIndex)
    ).toBeNull();
    expect(
      resolveExactDRepMatch(cip129At(1).slice(0, 20), drepIndex)
    ).toBeNull();
  });
});

describe('filterDReps', () => {
  const context = {
    favoriteDRepIds: EMPTY_DREP_ID_SET,
    top35DRepIds: EMPTY_DREP_ID_SET,
  };

  it('filters by status', () => {
    const entries = [
      buildEntry(1),
      buildEntry(2, { status: 'inactive', drepActivity: 0 }),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, status: 'inactive' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('filters by anchor-presence metadata', () => {
    const entries = [
      buildEntry(1, {
        anchor: { hash: 'a'.repeat(64), url: 'https://example.org/1.json' },
      }),
      buildEntry(2),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, metadata: 'withMetadata' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(1)]);
    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, metadata: 'withoutMetadata' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('applies the 7-12 expiry window with strict edges', () => {
    const entries = [
      buildEntry(1, { drepActivity: 6 }),
      buildEntry(2, { drepActivity: 7 }),
      buildEntry(3, { drepActivity: 12 }),
      buildEntry(4, { drepActivity: 13 }),
      buildEntry(5, { drepActivity: null }),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, expiry: 'thresholdWindow' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2), cip129At(3)]);
  });

  it('excludes top-35 members via the injected id set', () => {
    const entries = [buildEntry(1), buildEntry(2)];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, excludeTop35: true },
        { ...context, top35DRepIds: new Set([cip129At(1)]) }
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('applies the favorited predicate against an injected set and yields nothing on the empty set', () => {
    const entries = [buildEntry(1), buildEntry(2)];
    const favoritedOnly = { ...DEFAULT_DREP_FILTER_STATE, favoritedOnly: true };

    expect(
      filterDReps(entries, favoritedOnly, {
        ...context,
        favoriteDRepIds: new Set([cip129At(2)]),
      }).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
    expect(filterDReps(entries, favoritedOnly, context)).toEqual([]);
  });

  it('detects the default filter state', () => {
    expect(isDefaultFilterState(DEFAULT_DREP_FILTER_STATE)).toBe(true);
    expect(
      isDefaultFilterState({ ...DEFAULT_DREP_FILTER_STATE, excludeTop35: true })
    ).toBe(false);
  });
});

describe('sortDReps', () => {
  it('returns the input untouched for the randomized default', () => {
    const entries = [buildEntry(2), buildEntry(1)];

    expect(sortDReps(entries, 'randomized')).toBe(entries);
  });

  it('orders voting power losslessly at one lovelace beyond Number precision', () => {
    // 9007199254740993 and 9007199254740992 collapse to the same float; a
    // coerced comparison would tie and mis-order via the id tie-break.
    const smaller = buildEntry(1, {
      votingPower: new BigNumber('9007199254740993'),
    });
    const larger = buildEntry(2, {
      votingPower: new BigNumber('9007199254740992'),
    });

    expect(sortDReps([larger, smaller], 'votingPowerDesc')[0].drepId).toBe(
      cip129At(1)
    );
    expect(sortDReps([smaller, larger], 'votingPowerAsc')[0].drepId).toBe(
      cip129At(2)
    );
  });

  it('sorts null voting power last in both directions and does not mutate input', () => {
    const withPower = buildEntry(1, { votingPower: new BigNumber('5') });
    const nullPower = buildEntry(2);
    const input = [nullPower, withPower];

    expect(sortDReps(input, 'votingPowerDesc').map((e) => e.drepId)).toEqual([
      cip129At(1),
      cip129At(2),
    ]);
    expect(sortDReps(input, 'votingPowerAsc').map((e) => e.drepId)).toEqual([
      cip129At(1),
      cip129At(2),
    ]);
    expect(input.map((e) => e.drepId)).toEqual([cip129At(2), cip129At(1)]);
  });

  it('sorts by soonest expiry first with null activity last', () => {
    const entries = [
      buildEntry(1, { drepActivity: 30 }),
      buildEntry(2, { drepActivity: 8 }),
      buildEntry(3, { drepActivity: null }),
    ];

    expect(sortDReps(entries, 'expiryAsc').map((e) => e.drepId)).toEqual([
      cip129At(2),
      cip129At(1),
      cip129At(3),
    ]);
  });
});
```

#### Step 3: Edit `source/renderer/app/stores/GovernanceStore.ts`

Current seams: module-level `compareByVotingPowerDesc` :69-84 (its drepId
tie-break :81-83), `drepIndex` :90, `cohortSeed` :110, `isCohortActive` :143-148,
`defaultCohort` :157-175 (inline canonical sort :169-173), `displayedDRepList`
:178-180.

**3a.** Immediately after `compareByVotingPowerDesc` closes (line 84), add a
module-level function:

```ts
/** Canonical, deterministic tie/canonicalization order shared by the derived views. */
function compareDRepIdAsc(
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number {
  if (a.drepId < b.drepId) return -1;
  if (a.drepId > b.drepId) return 1;
  return 0;
}
```

**3b.** In `defaultCohort` (:157-175), replace the inline canonical sort

```ts
    const canonical = [...selected].sort((a, b) => {
      if (a.drepId < b.drepId) return -1;
      if (a.drepId > b.drepId) return 1;
      return 0;
    });
```

with:

```ts
    const canonical = [...selected].sort(compareDRepIdAsc);
```

**3c.** After the `displayedDRepList` computed (:178-180 — ends with the closing
brace of `return this.defaultCohort ?? this.drepList;`), add:

```ts
  /**
   * Ids of the 35 largest DReps by voting power. Empty until ranking has
   * loaded - the ranking-unavailable banner promises that ranking-based
   * filters are disabled in that state.
   */
  @computed get top35DRepIds(): Set<string> {
    if (this.votingPowerState !== VotingPowerEnrichState.Loaded) {
      return new Set();
    }
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    return new Set(
      ranked.slice(0, COHORT_TOP_EXCLUSION).map((entry) => entry.drepId)
    );
  }

  /**
   * Show-all base list: every registration (top-35, sub-floor and inactive
   * included) in the same seeded-random session order as the cohort, so
   * enabling show-all never introduces ranking bias by default.
   */
  @computed get showAllList(): AppDRepDirectoryEntry[] {
    const canonical = [...this.drepList].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }
```

Nothing else in the store changes. No action is added; `reshuffleCohort()`
(:284-287) already reorders both derived lists because both read `cohortSeed`.

#### Step 4: Create `DRepDirectorySearch.tsx`

Full file contents
(`source/renderer/app/components/governance/drep-directory/DRepDirectorySearch.tsx`):

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import type { DRepQueryKind } from './helpers';
import styles from './DRepDirectorySearch.scss';

const messages = defineMessages({
  placeholder: {
    id: 'governance.drepDirectory.searchPlaceholder',
    defaultMessage: '!!!Search by DRep ID',
    description: 'Placeholder of the DRep ID search input',
  },
  minLengthHint: {
    id: 'governance.drepDirectory.search.minLengthHint',
    defaultMessage: '!!!Enter at least 8 characters to search by ID',
    description: 'Hint shown while the search query is below the minimum',
  },
  invalidId: {
    id: 'governance.drepDirectory.search.invalidId',
    defaultMessage: '!!!Invalid DRep ID',
    description: 'Error shown for a full-length DRep ID that fails validation',
  },
});

interface Props {
  value: string;
  queryKind: DRepQueryKind;
  onChange: (value: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectorySearch({ value, queryKind, onChange, intl }: Props) {
  return (
    <div className={styles.container}>
      <Input
        className={styles.input}
        value={value}
        onChange={onChange}
        placeholder={intl.formatMessage(messages.placeholder)}
        skin={InputSkin}
      />
      {queryKind === 'belowMinimum' && (
        <p className={styles.hint}>
          {intl.formatMessage(messages.minLengthHint)}
        </p>
      )}
      {queryKind === 'invalidFullForm' && (
        <p className={styles.error} role="alert">
          {intl.formatMessage(messages.invalidId)}
        </p>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectorySearch);
```

react-polymorph `Input` passes the new string value as the first `onChange`
argument. There is deliberately **no submit/Enter handler**: prefix matches must
never auto-open, and exact-match opening is reactive in `DRepDirectory` (Step 10).

#### Step 5: Create `DRepDirectorySearch.scss`

```scss
.container {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.input {
  max-width: 480px;
}

.hint {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
  margin: 0;
}

.error {
  font-size: 12px;
  color: var(--theme-error-color, #ea4c5b);
  margin: 0;
}
```

#### Step 6: Create `DRepDirectoryFilters.tsx`

Native `<select>` elements (PRD P-15 — deterministic under jsdom; slice-5 P-7
native-element precedent) with `aria-label`s; react-polymorph `Checkbox` for the
two toggles. Full file contents:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import type {
  DRepExpiryFilter,
  DRepFilterState,
  DRepMetadataFilter,
  DRepSortOption,
  DRepStatusFilter,
} from './helpers';
import styles from './DRepDirectoryFilters.scss';

const messages = defineMessages({
  showAll: {
    id: 'governance.drepDirectory.cohortBanner.showAll',
    defaultMessage: '!!!Show all DReps',
    description: 'Toggle that switches from the default cohort to all DReps',
  },
  statusLabel: {
    id: 'governance.drepDirectory.filter.active',
    defaultMessage: '!!!Status',
    description: 'Label of the status filter',
  },
  metadataLabel: {
    id: 'governance.drepDirectory.filter.metadata',
    defaultMessage: '!!!Metadata',
    description: 'Label of the metadata filter',
  },
  expiryLabel: {
    id: 'governance.drepDirectory.filter.expiry',
    defaultMessage: '!!!Expiry',
    description: 'Label of the expiry-threshold filter',
  },
  optionAll: {
    id: 'governance.drepDirectory.filter.all',
    defaultMessage: '!!!All',
    description: 'Neutral option of every filter dropdown',
  },
  statusActive: {
    id: 'governance.drepDirectory.status.active',
    defaultMessage: '!!!Active',
    description: 'Active status label',
  },
  statusInactive: {
    id: 'governance.drepDirectory.status.inactive',
    defaultMessage: '!!!Inactive',
    description: 'Inactive status label',
  },
  metadataWith: {
    id: 'governance.drepDirectory.filter.metadata.with',
    defaultMessage: '!!!With metadata',
    description: 'Metadata filter option: anchor present',
  },
  metadataWithout: {
    id: 'governance.drepDirectory.filter.metadata.without',
    defaultMessage: '!!!Without metadata',
    description: 'Metadata filter option: no anchor',
  },
  expiryThresholdWindow: {
    id: 'governance.drepDirectory.filter.expiry.thresholdWindow',
    defaultMessage: '!!!Expiring in 7–12 epochs',
    description: 'Expiry filter option: the threshold window',
  },
  excludeTop35: {
    id: 'governance.drepDirectory.filter.excludeTop35',
    defaultMessage: '!!!Exclude the 35 largest',
    description: 'Show-all filter that removes the top-35 by voting power',
  },
  sortLabel: {
    id: 'governance.drepDirectory.sort.label',
    defaultMessage: '!!!Sort',
    description: 'Label of the show-all sort dropdown',
  },
  sortRandomized: {
    id: 'governance.drepDirectory.sort.randomized',
    defaultMessage: '!!!Randomized (default)',
    description: 'Default seeded-random sort option',
  },
  sortVotingPowerDesc: {
    id: 'governance.drepDirectory.sort.votingPowerDesc',
    defaultMessage: '!!!Voting power (high to low)',
    description: 'Voting power descending sort option',
  },
  sortVotingPowerAsc: {
    id: 'governance.drepDirectory.sort.votingPowerAsc',
    defaultMessage: '!!!Voting power (low to high)',
    description: 'Voting power ascending sort option',
  },
  sortExpiryAsc: {
    id: 'governance.drepDirectory.sort.expiryAsc',
    defaultMessage: '!!!Expiry (soonest first)',
    description: 'Soonest-expiry-first sort option',
  },
});

interface Props {
  filters: DRepFilterState;
  onFiltersChange: (filters: DRepFilterState) => void;
  isShowAll: boolean;
  onShowAllChange: (isShowAll: boolean) => void;
  sort: DRepSortOption;
  onSortChange: (sort: DRepSortOption) => void;
  isRankingAvailable: boolean;
  isSearchActive: boolean;
  intl: intlShape.isRequired;
}

function DRepDirectoryFilters({
  filters,
  onFiltersChange,
  isShowAll,
  onShowAllChange,
  sort,
  onSortChange,
  isRankingAvailable,
  isSearchActive,
  intl,
}: Props) {
  return (
    <div className={styles.container}>
      <Checkbox
        className={styles.toggle}
        label={intl.formatMessage(messages.showAll)}
        checked={isShowAll}
        onChange={onShowAllChange}
        skin={CheckboxSkin}
      />
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.statusLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.statusLabel)}
        value={filters.status}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            status: event.target.value as DRepStatusFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="active">
          {intl.formatMessage(messages.statusActive)}
        </option>
        <option value="inactive">
          {intl.formatMessage(messages.statusInactive)}
        </option>
      </select>
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.metadataLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.metadataLabel)}
        value={filters.metadata}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            metadata: event.target.value as DRepMetadataFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="withMetadata">
          {intl.formatMessage(messages.metadataWith)}
        </option>
        <option value="withoutMetadata">
          {intl.formatMessage(messages.metadataWithout)}
        </option>
      </select>
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.expiryLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.expiryLabel)}
        value={filters.expiry}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            expiry: event.target.value as DRepExpiryFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="thresholdWindow">
          {intl.formatMessage(messages.expiryThresholdWindow)}
        </option>
      </select>
      {isShowAll && isRankingAvailable && (
        <Checkbox
          className={styles.toggle}
          label={intl.formatMessage(messages.excludeTop35)}
          checked={filters.excludeTop35}
          onChange={(checked: boolean) =>
            onFiltersChange({ ...filters, excludeTop35: checked })
          }
          skin={CheckboxSkin}
        />
      )}
      {isShowAll && !isSearchActive && (
        <>
          <span className={styles.filterLabel}>
            {intl.formatMessage(messages.sortLabel)}
          </span>
          <select
            className={styles.select}
            aria-label={intl.formatMessage(messages.sortLabel)}
            value={sort}
            onChange={(event) =>
              onSortChange(event.target.value as DRepSortOption)
            }
          >
            <option value="randomized">
              {intl.formatMessage(messages.sortRandomized)}
            </option>
            {isRankingAvailable && (
              <option value="votingPowerDesc">
                {intl.formatMessage(messages.sortVotingPowerDesc)}
              </option>
            )}
            {isRankingAvailable && (
              <option value="votingPowerAsc">
                {intl.formatMessage(messages.sortVotingPowerAsc)}
              </option>
            )}
            <option value="expiryAsc">
              {intl.formatMessage(messages.sortExpiryAsc)}
            </option>
          </select>
        </>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryFilters);
```

Notes: duplicate message ids across `defineMessages` blocks
(`status.active`/`status.inactive` also live in `DRepStatusBadge`;
`cohortBanner.showAll` also lands in `DRepEmptyState`, Step 8) follow the shipped
repo pattern (`governance.drepDirectory.title` is defined in both `DRepDirectory`
and `DRepDirectoryBanner`). Voting-power sort options are simply **omitted** when
ranking is unavailable — the shipped `rankingUnavailable` banner promises
"Ranking-based filters disabled". The favorited control is deliberately absent
(PRD D-3 — task-122 renders it).

#### Step 7: Create `DRepDirectoryFilters.scss`

```scss
.container {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 12px;
}

.toggle {
  white-space: nowrap;
}

.filterLabel {
  font-size: 13px;
  color: var(--theme-text-secondary, #6b7384);
}

.select {
  font-size: 13px;
  padding: 4px 8px;
  border-radius: 4px;
  border: 1px solid var(--theme-input-border, #c6cdd6);
  background: var(--theme-input-bg, transparent);
  color: inherit;
}
```

#### Step 8: Replace `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx`

Current file is 38 lines with the single `noSync` variant (union at :16). Replace
the entire file with:

```tsx
import React from 'react';
import {
  FormattedMessage,
  defineMessages,
  injectIntl,
  intlShape,
} from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepEmptyState.scss';

const messages = defineMessages({
  noSync: {
    id: 'governance.drepDirectory.empty.noSync',
    defaultMessage:
      '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.',
    description: 'Directory fallback while the node has not reached the tip',
  },
  noResults: {
    id: 'governance.drepDirectory.empty.noResults',
    defaultMessage: '!!!No DReps match your filters. {ClearFilters} or {ShowAll}.',
    description: 'Empty state when search/filters match nothing',
  },
  clearFilters: {
    id: 'governance.drepDirectory.empty.noResults.clearFilters',
    defaultMessage: '!!!Clear filters',
    description: 'Action that resets search, filters and sort',
  },
  showAll: {
    id: 'governance.drepDirectory.cohortBanner.showAll',
    defaultMessage: '!!!Show all DReps',
    description: 'Toggle that switches from the default cohort to all DReps',
  },
});

// Only noSync and noResults ship for now; the designed selfnode variant
// joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults';

interface Props {
  variant: DRepEmptyStateVariant;
  onClearFilters?: () => void;
  onShowAll?: () => void;
  intl: intlShape.isRequired;
}

function DRepEmptyState({ variant, onClearFilters, onShowAll, intl }: Props) {
  if (variant === 'noResults') {
    return (
      <div className={styles.container} data-variant={variant}>
        <p className={styles.message}>
          <FormattedMessage
            {...messages.noResults}
            values={{
              ClearFilters: (
                <Link
                  className={styles.actionLink}
                  label={intl.formatMessage(messages.clearFilters)}
                  hasIconAfter={false}
                  onClick={onClearFilters}
                  skin={LinkSkin}
                />
              ),
              ShowAll: (
                <Link
                  className={styles.actionLink}
                  label={intl.formatMessage(messages.showAll)}
                  hasIconAfter={false}
                  onClick={onShowAll}
                  skin={LinkSkin}
                />
              ),
            }}
          />
        </p>
      </div>
    );
  }

  return (
    <div className={styles.container} data-variant={variant}>
      <p className={styles.message}>{intl.formatMessage(messages.noSync)}</p>
    </div>
  );
}

export default injectIntl(DRepEmptyState);
```

#### Step 9: Append to `DRepEmptyState.scss`

```scss
.actionLink {
  font-size: inherit;
  white-space: nowrap;
}
```

#### Step 10: Replace `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`

The current file is 227 lines (Props :58-72, `hasRetainedData` :89, bare-empty
case :132-143, default branch :145-182, banner render :188-194). Replace the
entire file with the version below. Preserved exactly: the loading / noSync /
failed / bare-empty branches, the error banner, the refreshing badge, the
ranking-unavailable banner, the syncing banner SVG block, and the banner wiring.
New: four props, view state, the search/filter row, the visible-entries pipeline,
reactive exact-match open, the sort-bias disclosure, noResults wiring, and the
filtered-mode banner props.

```tsx
import React, { useEffect, useMemo, useRef, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepDirectoryBanner from './DRepDirectoryBanner';
import DRepDirectorySearch from './DRepDirectorySearch';
import DRepDirectoryFilters from './DRepDirectoryFilters';
import DRepEmptyState from '../_shared/DRepEmptyState';
import DRepErrorBanner from '../_shared/DRepErrorBanner';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
} from '../../../stores/GovernanceStore';
import { GovernanceQueryErrorType } from '../../../../../common/types/governance.types';
import {
  DEFAULT_DREP_FILTER_STATE,
  EMPTY_DREP_ID_SET,
  buildDRepSearchIndex,
  filterDReps,
  getDRepQueryKind,
  isDefaultFilterState,
  resolveExactDRepMatch,
  searchDRepsByIdPrefix,
  sortDReps,
} from './helpers';
import type { DRepFilterState, DRepSortOption } from './helpers';
import styles from './DRepDirectory.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title of the DRep directory page',
  },
  loading: {
    id: 'governance.drepDirectory.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state message',
  },
  empty: {
    id: 'governance.drepDirectory.empty',
    defaultMessage: '!!!No DReps found on-chain.',
    description: 'Empty directory state',
  },
  error: {
    id: 'governance.drepDirectory.error',
    defaultMessage: '!!!Could not load DRep data.',
    description: 'Error state message',
  },
  retry: {
    id: 'governance.drepDirectory.retry',
    defaultMessage: '!!!Retry',
    description: 'Retry button label',
  },
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Refreshing state badge label',
  },
  syncing: {
    id: 'governance.drepDirectory.syncing',
    defaultMessage:
      '!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.',
    description: 'Persistent soft-warning banner while the node is syncing',
  },
  sortBiasWarning: {
    id: 'governance.drepDirectory.showAll.sortBiasWarning',
    defaultMessage:
      '!!!Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing.',
    description: 'Disclosure shown while voting-power-descending sort is active',
  },
});

interface Props {
  drepList: AppDRepDirectoryEntry[];
  drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>;
  showAllList: AppDRepDirectoryEntry[];
  top35DRepIds: ReadonlySet<string>;
  favoriteDRepIds?: ReadonlySet<string>;
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  isNodeInSync: boolean;
  syncProgress: number | null;
  votingPowerState: VotingPowerEnrichState;
  isCohortActive: boolean;
  onRefresh: () => void;
  onReshuffle: () => void;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  drepList,
  drepIndex,
  showAllList,
  top35DRepIds,
  favoriteDRepIds = EMPTY_DREP_ID_SET,
  refreshState,
  error,
  lastFetchedAt,
  isNodeInSync,
  syncProgress,
  votingPowerState,
  isCohortActive,
  onRefresh,
  onReshuffle,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const [searchQuery, setSearchQuery] = useState('');
  const [isShowAll, setIsShowAll] = useState(false);
  const [filters, setFilters] = useState<DRepFilterState>(
    DEFAULT_DREP_FILTER_STATE
  );
  const [sort, setSort] = useState<DRepSortOption>('randomized');

  const queryKind = getDRepQueryKind(searchQuery);
  const isSearchActive =
    queryKind === 'prefix' ||
    queryKind === 'exactValid' ||
    queryKind === 'invalidFullForm';
  const isRankingAvailable =
    votingPowerState === VotingPowerEnrichState.Loaded;

  // Search always covers the full membership so excluded and non-cohort
  // DReps stay reachable regardless of the current view.
  const searchIndex = useMemo(
    () => buildDRepSearchIndex(showAllList),
    [showAllList]
  );

  const visibleEntries = useMemo(() => {
    const base = isSearchActive
      ? searchDRepsByIdPrefix(searchIndex, searchQuery)
      : isShowAll
      ? showAllList
      : drepList;
    const filtered = filterDReps(base, filters, {
      favoriteDRepIds,
      top35DRepIds,
    });
    // Search results keep relevance order; the cohort keeps its seeded order.
    if (isSearchActive || !isShowAll) return filtered;
    return sortDReps(filtered, sort);
  }, [
    drepList,
    favoriteDRepIds,
    filters,
    isSearchActive,
    isShowAll,
    searchIndex,
    searchQuery,
    showAllList,
    sort,
    top35DRepIds,
  ]);

  // A checksum-valid full ID that resolves in the index bypasses the result
  // list and opens the detail view directly, once per distinct query.
  const lastOpenedQueryRef = useRef<string | null>(null);
  useEffect(() => {
    if (queryKind !== 'exactValid') return;
    if (lastOpenedQueryRef.current === searchQuery) return;
    const match = resolveExactDRepMatch(searchQuery, drepIndex);
    if (match) {
      lastOpenedQueryRef.current = searchQuery;
      onViewDetails(match.drepId);
    }
  }, [queryKind, searchQuery, drepIndex, onViewDetails]);

  const isFilteredView =
    isSearchActive ||
    isShowAll ||
    sort !== 'randomized' ||
    !isDefaultFilterState(filters);

  const handleShowAllChange = (nextShowAll: boolean) => {
    setIsShowAll(nextShowAll);
    // Sorts exist only under show-all; leaving it restores the default order.
    if (!nextShowAll) setSort('randomized');
  };

  const handleClearFilters = () => {
    setSearchQuery('');
    setFilters(DEFAULT_DREP_FILTER_STATE);
    setSort('randomized');
  };

  const handleShowAllFromEmptyState = () => {
    handleClearFilters();
    setIsShowAll(true);
  };

  const hasRetainedData = showAllList.length > 0;
  const showErrorBanner = error && hasRetainedData;

  // While syncing, an empty or unavailable directory is expected — fall back
  // to the noSync empty state instead of a bare error or "No DReps found".
  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    (refreshState === GovernanceRefreshState.Loaded ||
      (refreshState === GovernanceRefreshState.Failed &&
        error?.type !== GovernanceQueryErrorType.SelfnodeCliUnsupported));

  const renderContent = () => {
    switch (true) {
      case refreshState === GovernanceRefreshState.Loading:
        return (
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        );

      case showNoSyncFallback:
        return <DRepEmptyState variant="noSync" />;

      case refreshState === GovernanceRefreshState.Failed:
        return (
          <div className={styles.stateContainer}>
            <p className={styles.errorMessage}>
              {intl.formatMessage(messages.error)}
            </p>
            {error && <p className={styles.errorDetails}>{error.message}</p>}
            {error?.details && (
              <p className={styles.errorDetails}>{error.details}</p>
            )}
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      case showAllList.length === 0 &&
        refreshState === GovernanceRefreshState.Loaded:
        return (
          <div className={styles.stateContainer}>
            <p>{intl.formatMessage(messages.empty)}</p>
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      default:
        return (
          <>
            <div className={styles.controlsRow}>
              <DRepDirectorySearch
                value={searchQuery}
                queryKind={queryKind}
                onChange={setSearchQuery}
              />
              <DRepDirectoryFilters
                filters={filters}
                onFiltersChange={setFilters}
                isShowAll={isShowAll}
                onShowAllChange={handleShowAllChange}
                sort={sort}
                onSortChange={setSort}
                isRankingAvailable={isRankingAvailable}
                isSearchActive={isSearchActive}
              />
            </div>
            {showErrorBanner && error && (
              <div className={styles.errorBanner}>
                <div>
                  <p className={styles.errorMessage}>
                    {intl.formatMessage(messages.error)}
                  </p>
                  <p className={styles.errorDetails}>{error.message}</p>
                  {error.details && (
                    <p className={styles.errorDetails}>{error.details}</p>
                  )}
                </div>
                <Button
                  label={intl.formatMessage(messages.retry)}
                  onClick={onRefresh}
                  skin={ButtonSkin}
                />
              </div>
            )}
            {refreshState === GovernanceRefreshState.Refreshing && (
              <div className={styles.refreshingBadge}>
                <LoadingSpinner />
                {intl.formatMessage(messages.refreshing)}
              </div>
            )}
            {votingPowerState === VotingPowerEnrichState.Failed && (
              <DRepErrorBanner variant="rankingUnavailable" />
            )}
            {isShowAll && !isSearchActive && sort === 'votingPowerDesc' && (
              <div className={styles.sortBiasWarning} role="status">
                {intl.formatMessage(messages.sortBiasWarning)}
              </div>
            )}
            {visibleEntries.length === 0 ? (
              <DRepEmptyState
                variant="noResults"
                onClearFilters={handleClearFilters}
                onShowAll={handleShowAllFromEmptyState}
              />
            ) : (
              <DRepDirectoryList
                entries={visibleEntries}
                onSelectForDelegation={onSelectForDelegation}
                onViewDetails={onViewDetails}
                votingPowerState={votingPowerState}
              />
            )}
          </>
        );
    }
  };

  return (
    <div className={styles.container}>
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
        isCohortActive={isCohortActive}
        onReshuffle={onReshuffle}
        isFilteredView={isFilteredView}
        displayedCount={visibleEntries.length}
      />
      {!isNodeInSync && (
        <div className={styles.syncingBanner} role="status">
          <svg
            className={styles.syncingIcon}
            aria-hidden="true"
            width="16"
            height="16"
            viewBox="0 0 16 16"
          >
            <path
              d="M8 1.5 15 14H1L8 1.5z"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.5"
              strokeLinejoin="round"
            />
            <path d="M8 6v4" stroke="currentColor" strokeWidth="1.5" />
            <circle cx="8" cy="12" r="0.9" fill="currentColor" />
          </svg>
          <span>
            {intl.formatMessage(messages.syncing, {
              progress: Math.floor(syncProgress ?? 0),
            })}
          </span>
        </div>
      )}
      {renderContent()}
    </div>
  );
}

export default injectIntl(DRepDirectory);
```

Two deliberate behavior changes vs. the old file (PRD P-12): `hasRetainedData`
and the bare-empty case now check `showAllList` (the full list) instead of the
`drepList` prop (the cohort), so a non-empty network with an empty *cohort* shows
`noResults` with a working Show-all escape hatch instead of the misleading
"No DReps found on-chain".

#### Step 11: Append to `DRepDirectory.scss`

```scss
.controlsRow {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-start;
  gap: 16px;
  margin-bottom: 12px;
}

.sortBiasWarning {
  font-size: 13px;
  padding: 8px 12px;
  border-radius: 4px;
  background: var(--badge-warning-bg, rgba(230, 162, 60, 0.12));
  color: var(--badge-warning-fg, #b26a00);
  margin-bottom: 12px;
}
```

#### Step 12: Edit `DRepDirectoryBanner.tsx`

Current seams: `defineMessages` :10-43 (`source` message ends :42), Props :45-54,
cohort block :85-96, source line :97-101.

**12a.** In `defineMessages`, after the `source` message (before the closing
`});`), add:

```ts
  filtered: {
    id: 'governance.drepDirectory.cohortBanner.filtered',
    defaultMessage:
      '!!!Showing {n} DReps matching your filters. Default randomized order does not apply.',
    description: 'Banner line replacing the cohort claim while filtered',
  },
```

**12b.** In `interface Props`, after `showSource?: boolean;` (:52), add:

```ts
  // Both default to the pure-default-view state so existing call sites and
  // stories keep compiling unchanged.
  isFilteredView?: boolean;
  displayedCount?: number;
```

**12c.** In the destructuring, after `showSource = true,`, add:

```ts
  isFilteredView = false,
  displayedCount = 0,
```

**12d.** Change the cohort-block gate (:85) from `{isCohortActive && (` to:

```tsx
      {isCohortActive && !isFilteredView && (
```

and the source-line gate (:97) from `{isCohortActive && showSource && (` to:

```tsx
      {isCohortActive && !isFilteredView && showSource && (
```

**12e.** After the source-line block's closing `)}`, add:

```tsx
      {isFilteredView && (
        <p className={styles.filteredLine}>
          {intl.formatMessage(messages.filtered, { n: displayedCount })}
        </p>
      )}
```

The filtered line intentionally renders regardless of `isCohortActive`
(searching while ranking failed still shows an accurate count), and the BMVG
citation renders only alongside the cohort claim it explains (PRD P-10).

#### Step 13: Append to `DRepDirectoryBanner.scss`

```scss
.filteredLine {
  font-size: 13px;
  color: var(--theme-text-secondary, #6b7384);
  margin: 0;
}
```

#### Step 14: Edit `DRepDirectoryPage.tsx`

The render (:82-97) currently passes `drepList={governanceStore.displayedDRepList}`
first. Add three props directly after that line:

```tsx
        drepIndex={governanceStore.drepIndex}
        showAllList={governanceStore.showAllList}
        top35DRepIds={governanceStore.top35DRepIds}
```

No other change — `handleSelectForDelegation` (:58-66) and `handleViewDetails`
(:68-73) stay byte-identical (invariants #4/#10: the delegation handoff and the
detail hop keep travelling only through `location.state` / the route path).

#### Step 15: Locale JSONs (21 keys per locale, all `!!!`, alphabetical key order)

`source/renderer/app/i18n/locales/en-US.json` — insert each block at the named
neighbor (current anchors: `.cohortBanner` :313, `.reshuffle` :314, `.source`
:315, `.empty` :318, `.empty.noSync` :319, `.error.rankingUnavailable` :321,
`.lastUpdated` :322, `.retry` :329, `.source.onChain` :330):

Between `"governance.drepDirectory.cohortBanner"` and
`"governance.drepDirectory.cohortBanner.reshuffle"`:

```json
  "governance.drepDirectory.cohortBanner.filtered": "!!!Showing {n} DReps matching your filters. Default randomized order does not apply.",
```

Between `"governance.drepDirectory.cohortBanner.reshuffle"` and
`"governance.drepDirectory.cohortBanner.source"`:

```json
  "governance.drepDirectory.cohortBanner.showAll": "!!!Show all DReps",
```

Between `"governance.drepDirectory.empty"` and
`"governance.drepDirectory.empty.noSync"`:

```json
  "governance.drepDirectory.empty.noResults": "!!!No DReps match your filters. {ClearFilters} or {ShowAll}.",
  "governance.drepDirectory.empty.noResults.clearFilters": "!!!Clear filters",
```

Between `"governance.drepDirectory.error.rankingUnavailable"` and
`"governance.drepDirectory.lastUpdated"`:

```json
  "governance.drepDirectory.filter.active": "!!!Status",
  "governance.drepDirectory.filter.all": "!!!All",
  "governance.drepDirectory.filter.excludeTop35": "!!!Exclude the 35 largest",
  "governance.drepDirectory.filter.expiry": "!!!Expiry",
  "governance.drepDirectory.filter.expiry.thresholdWindow": "!!!Expiring in 7–12 epochs",
  "governance.drepDirectory.filter.metadata": "!!!Metadata",
  "governance.drepDirectory.filter.metadata.with": "!!!With metadata",
  "governance.drepDirectory.filter.metadata.without": "!!!Without metadata",
```

Between `"governance.drepDirectory.retry"` and
`"governance.drepDirectory.source.onChain"`:

```json
  "governance.drepDirectory.search.invalidId": "!!!Invalid DRep ID",
  "governance.drepDirectory.search.minLengthHint": "!!!Enter at least 8 characters to search by ID",
  "governance.drepDirectory.searchPlaceholder": "!!!Search by DRep ID",
  "governance.drepDirectory.showAll.sortBiasWarning": "!!!Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing.",
  "governance.drepDirectory.sort.expiryAsc": "!!!Expiry (soonest first)",
  "governance.drepDirectory.sort.label": "!!!Sort",
  "governance.drepDirectory.sort.randomized": "!!!Randomized (default)",
  "governance.drepDirectory.sort.votingPowerAsc": "!!!Voting power (low to high)",
  "governance.drepDirectory.sort.votingPowerDesc": "!!!Voting power (high to low)",
```

`source/renderer/app/i18n/locales/ja-JP.json` — same neighbors, same order:

```json
  "governance.drepDirectory.cohortBanner.filtered": "!!!フィルターに一致する{n}のDRepを表示しています。デフォルトのランダム順は適用されません。",
```

```json
  "governance.drepDirectory.cohortBanner.showAll": "!!!すべてのDRepを表示",
```

```json
  "governance.drepDirectory.empty.noResults": "!!!フィルターに一致するDRepはありません。{ClearFilters}または{ShowAll}。",
  "governance.drepDirectory.empty.noResults.clearFilters": "!!!フィルターをクリア",
```

```json
  "governance.drepDirectory.filter.active": "!!!ステータス",
  "governance.drepDirectory.filter.all": "!!!すべて",
  "governance.drepDirectory.filter.excludeTop35": "!!!最大35件を除外",
  "governance.drepDirectory.filter.expiry": "!!!失効",
  "governance.drepDirectory.filter.expiry.thresholdWindow": "!!!7〜12エポックで失効",
  "governance.drepDirectory.filter.metadata": "!!!メタデータ",
  "governance.drepDirectory.filter.metadata.with": "!!!メタデータあり",
  "governance.drepDirectory.filter.metadata.without": "!!!メタデータなし",
```

```json
  "governance.drepDirectory.search.invalidId": "!!!無効なDRep IDです",
  "governance.drepDirectory.search.minLengthHint": "!!!IDで検索するには8文字以上入力してください",
  "governance.drepDirectory.searchPlaceholder": "!!!DRep IDで検索",
  "governance.drepDirectory.showAll.sortBiasWarning": "!!!投票権順に並べ替えています。デフォルトのランダム順は人気バイアスを減らすために設計されています。偏りのない閲覧にはデフォルトに戻すことをご検討ください。",
  "governance.drepDirectory.sort.expiryAsc": "!!!失効(近い順)",
  "governance.drepDirectory.sort.label": "!!!並べ替え",
  "governance.drepDirectory.sort.randomized": "!!!ランダム(デフォルト)",
  "governance.drepDirectory.sort.votingPowerAsc": "!!!投票権(低い順)",
  "governance.drepDirectory.sort.votingPowerDesc": "!!!投票権(高い順)",
```

Then run `yarn i18n:manage` and commit its `defaultMessages.json` /
`translations/messages.json` rewrites with this task. Invariant #11: every
string keeps `!!!`. Key provenance (PRD D-4/P-13): six ids come from the tokens
§9 inventory (`searchPlaceholder`, `cohortBanner.showAll`, `filter.active`,
`filter.metadata`, `empty.noResults`, `showAll.sortBiasWarning`); the other
fifteen are minted in the §9 naming style and recorded in the PRD. Do NOT add
`filter.favorited` (task-122) or any `category.highValue`/status key.

#### Step 16: Extend `tests/jest/governance/GovernanceStore.spec.ts`

Append a sibling top-level describe after the existing
`describe('GovernanceStore default cohort', …)` block closes (end of file). It
reuses the file's module-level `mockRequest` / `mockStakeRequest` mocks and the
imported `GovernanceStore` / `VotingPowerEnrichState` / `flushAsync` symbols the
earlier describes already use, plus its own local builders:

```ts
describe('GovernanceStore search and show-all seams', () => {
  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });

  const drepIdAt = (i: number) =>
    `drep1seam${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[],
    stakeByDRepId: Record<string, string>
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  it('exposes the 35 largest ids once ranking has loaded', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    expect(store.top35DRepIds.size).toBe(35);
    for (let i = 0; i < 35; i++) {
      expect(store.top35DRepIds.has(drepIdAt(i))).toBe(true);
    }
    expect(store.top35DRepIds.has(drepIdAt(35))).toBe(false);
  });

  it('exposes no top-35 set when the ranking phase failed', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.isRankingUnavailable).toBe(true);
    expect(store.top35DRepIds.size).toBe(0);
  });

  it('keeps full membership in showAllList including top-35, sub-floor and inactive entries', async () => {
    // Sub-floor and inactive entries appear here to prove show-all
    // reachability - they are never placed inside a cohort fixture.
    const dreps = [
      ...Array.from({ length: 36 }, (_, i) => buildDrep(i)),
      buildDrep(36, { drepActivity: 3 }),
      buildDrep(37, { drepActivity: 0, status: 'inactive' }),
    ];
    const store = await loadStore(dreps, stakeFor(38));

    const ids = new Set(store.showAllList.map((e) => e.drepId));
    expect(store.showAllList).toHaveLength(38);
    expect(ids.has(drepIdAt(0))).toBe(true);
    expect(ids.has(drepIdAt(36))).toBe(true);
    expect(ids.has(drepIdAt(37))).toBe(true);
  });

  it('orders showAllList from the session seed and reshuffles without any IPC query', async () => {
    const dreps = Array.from({ length: 20 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(20));
    const before = store.showAllList.map((e) => e.drepId);

    expect(store.showAllList.map((e) => e.drepId)).toEqual(before);

    store.reshuffleCohort();

    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(mockStakeRequest).toHaveBeenCalledTimes(1);
    const after = store.showAllList.map((e) => e.drepId);
    expect([...after].sort()).toEqual([...before].sort());
  });
});
```

(If `DRepDirectoryEntry` is not already imported at the top of the spec, it is —
the slice-5 describe uses the same type; add nothing.)

#### Step 17: Extend `DRepDirectory.spec.tsx`

**17a.** Add `Cardano` to the imports (after the `BigNumber` import, line 2):

```ts
import { Cardano } from '@cardano-sdk/core';
```

**17b.** After `paginatedEntries` (:35-38), add real-bech32 fixtures (component
search tests need checksum-valid ids; the legacy fake-id fixtures remain valid
for every non-search test because CIP-129 prefix matching is plain `startsWith`):

```ts
// Distinct from the first hash byte so a prefix of one id never matches another.
const credHash = (n: number) =>
  n.toString(16).padStart(2, '0').repeat(28).slice(0, 56);
const realDrepId = (n: number): string =>
  String(
    Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );
const realCip105Id = (n: number): string =>
  String(
    Cardano.DRepID.cip105FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );
const realEntry = (
  n: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  anchor: null,
  drepActivity: 20,
  drepId: realDrepId(n),
  status: 'active',
  votingPower: new BigNumber(`${1_000_000_000 - n}`),
  ...overrides,
});
```

**17c.** In the `renderComponent` helper (:40-84): add to the destructured
defaults (defaults may reference earlier parameters):

```ts
  showAllList = drepList,
  drepIndex = new Map(showAllList.map((e) => [e.drepId, e])),
  top35DRepIds = new Set<string>(),
```

to the type block:

```ts
  showAllList?: AppDRepDirectoryEntry[];
  drepIndex?: Map<string, AppDRepDirectoryEntry>;
  top35DRepIds?: Set<string>;
```

and to the rendered component props:

```tsx
        showAllList={showAllList}
        drepIndex={drepIndex}
        top35DRepIds={top35DRepIds}
```

(`favoriteDRepIds` is not threaded through the harness — the component default
covers it, and slice-6 tests the favorited predicate at the helpers level only,
per PRD D-3.)

**17d.** Two existing assertions must be widened because the new status select's
`<option>` elements carry the same localized labels as the status badge, which
makes single-element `getByText` throw a multiple-match error. The pinned
behavior (a status badge renders) is preserved:

- Line 94: `expect(screen.getByText('!!!Active')).toBeInTheDocument();` →
  `expect(screen.getAllByText('!!!Active')[0]).toBeInTheDocument();`
- Line 205: `expect(screen.getByText('アクティブ')).toBeInTheDocument();` →
  `expect(screen.getAllByText('アクティブ')[0]).toBeInTheDocument();`

No other pre-slice-6 assertion changes (`'!!!Voting power:'` uses exact-string
matching and does not collide with the sort option labels).

**17e.** Append these tests inside `describe('DRepDirectory', …)`:

```tsx
  it('shows the min-length hint below 8 post-HRP characters and leaves the list unfiltered', () => {
    renderComponent({ drepList: [realEntry(1), realEntry(2)] });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, { target: { value: 'drep1abcdefg' } });

    expect(
      screen.getByText('!!!Enter at least 8 characters to search by ID')
    ).toBeInTheDocument();
    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('filters by prefix at 8 characters and never auto-selects, even on Enter with one match', () => {
    const onViewDetails = jest.fn();
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    const uniquePrefix = realDrepId(1).slice(0, 'drep1'.length + 20);
    fireEvent.change(input, { target: { value: uniquePrefix } });
    fireEvent.keyDown(input, { key: 'Enter', code: 'Enter' });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();
  });

  it('opens the detail view once for an exact CIP-129 match', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1) },
    });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('canonicalizes an exact CIP-105 match to the CIP-129 detail id', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realCip105Id(1) },
    });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('shows the invalid-ID error for a full-form string with a bad checksum and never navigates', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: `drep1${'q'.repeat(51)}` },
    });

    expect(screen.getByText('!!!Invalid DRep ID')).toBeInTheDocument();
    expect(onViewDetails).not.toHaveBeenCalled();
    // FormattedMessage splits the noResults copy across nested nodes; take
    // the first match instead of requiring a single element.
    expect(
      screen.getAllByText(/No DReps match your filters/)[0]
    ).toBeInTheDocument();
  });

  it('reaches top-35 and non-cohort entries through show-all', () => {
    // drepList (the cohort) holds only entry 1; the full list adds a top-35
    // and a sub-floor entry that must surface when show-all is on.
    const cohortEntry = realEntry(1);
    const top35Entry = realEntry(2);
    const subFloorEntry = realEntry(3, { drepActivity: 3 });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, top35Entry, subFloorEntry],
      top35DRepIds: new Set([top35Entry.drepId]),
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(3);
  });

  it('finds and opens a non-cohort entry by ID with show-all off', () => {
    // Entry 2 exists only in showAllList (and, via the harness default, in
    // drepIndex) - never in the cohort. Search must run over the full
    // membership and exact-match lookup over the index, or non-cohort DReps
    // are unreachable without the show-all toggle.
    const onViewDetails = jest.fn();
    const cohortEntry = realEntry(1);
    const nonCohortEntry = realEntry(2);
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, nonCohortEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, {
      target: { value: realDrepId(2).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();

    fireEvent.change(input, { target: { value: realDrepId(2) } });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(2));
  });

  it('applies facet filters through the native selects', () => {
    renderComponent({
      drepList: [
        realEntry(1),
        realEntry(2, { status: 'inactive', drepActivity: 0 }),
      ],
    });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'inactive' },
    });

    // Card count is the unambiguous signal (the select option shares the
    // '!!!Inactive' label with the badge).
    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('excludes the top-35 under show-all via the exclusion toggle', () => {
    const top35Entry = realEntry(1);
    const rest = realEntry(2);
    renderComponent({
      drepList: [rest],
      showAllList: [top35Entry, rest],
      top35DRepIds: new Set([top35Entry.drepId]),
    });

    fireEvent.click(screen.getByText('!!!Show all DReps'));
    expect(screen.getAllByText('!!!View details')).toHaveLength(2);

    fireEvent.click(screen.getByText('!!!Exclude the 35 largest'));
    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('shows the sort-bias disclosure only while voting-power-descending is active', () => {
    renderComponent({ drepList: [realEntry(1)] });

    fireEvent.click(screen.getByText('!!!Show all DReps'));
    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'votingPowerDesc' },
    });

    expect(screen.getByText(/Sorted by voting power/)).toBeInTheDocument();

    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'randomized' },
    });

    expect(
      screen.queryByText(/Sorted by voting power/)
    ).not.toBeInTheDocument();
  });

  it('switches the banner to the filtered line with a live count under show-all', () => {
    renderComponent({
      drepList: [realEntry(1)],
      showAllList: [realEntry(1), realEntry(2)],
      isCohortActive: true,
    });

    expect(screen.getByText(/Default view shows/)).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(
      screen.getByText(/Showing 2 DReps matching your filters/)
    ).toBeInTheDocument();
    expect(screen.queryByText(/Default view shows/)).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Reshuffle order')).not.toBeInTheDocument();
  });

  it('recovers from zero results via the Clear filters action', () => {
    renderComponent({ drepList: [realEntry(1)] });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'inactive' },
    });
    expect(
      screen.getAllByText(/No DReps match your filters/)[0]
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Clear filters'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('renders the search surface in ja-JP', () => {
    renderComponent({ locale: 'ja-JP' });

    expect(
      screen.getByPlaceholderText('!!!DRep IDで検索')
    ).toBeInTheDocument();
    expect(screen.getByText('!!!すべてのDRepを表示')).toBeInTheDocument();
  });
```

Note: the Clear-filters test asserts recovery after a facet change, which also
covers that the existing status-select value resets. If `getByLabelText` finds
two `'!!!Status'` matches in a future state, tighten with
`screen.getByRole('combobox', { name: '!!!Status' })` — the selects are the only
comboboxes on the page today.

#### Step 18: Append to `DRepDirectoryBanner.spec.tsx`

The existing `renderBanner` helper spreads explicit props; extend its options
type and pass-through with `isFilteredView` and `displayedCount` (optional, same
pattern as `showSource`), then append inside the describe:

```tsx
  it('replaces the cohort claim, Reshuffle and citation with the filtered line', () => {
    renderBanner({ isFilteredView: true, displayedCount: 7 });

    expect(
      screen.getByText(
        '!!!Showing 7 DReps matching your filters. Default randomized order does not apply.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Default view shows up to 200/)
    ).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Reshuffle order')).not.toBeInTheDocument();
    expect(screen.queryByText(/Beyond MVG/)).not.toBeInTheDocument();
  });

  it('shows the filtered line even when the cohort is inactive', () => {
    renderBanner({
      isCohortActive: false,
      isFilteredView: true,
      displayedCount: 1,
    });

    expect(
      screen.getByText(/Showing 1 DReps matching your filters/)
    ).toBeInTheDocument();
  });
```

#### Step 19: Extend `DRepDirectoryPage.spec.tsx`

**19a.** `buildGovernanceStore` gains three fields (the container now reads
them). Add alphabetically inside the returned object:

```ts
  drepIndex: new Map([[drepEntry.drepId, drepEntry]]),
  showAllList: [drepEntry],
  top35DRepIds: new Set<string>(),
```

**19b.** Append inside the describe (the pre-IPC pin at the container boundary —
searching must trigger no store fetch):

```tsx
  it('never triggers a store fetch from search interactions', () => {
    const { governance } = renderPage();

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, { target: { value: 'drep1abcdefgh' } });
    fireEvent.change(input, { target: { value: `drep1${'q'.repeat(51)}` } });

    expect(governance.refresh).not.toHaveBeenCalled();
    expect(governance.reshuffleCohort).not.toHaveBeenCalled();
  });
```

(The mocked `refreshState` is `Loaded`, so mount does not refresh — the
assertion isolates the search interactions. If `fireEvent` is not yet imported
in this file it is — the slice-5 Reshuffle test uses it.)

#### Step 20: Extend `VotingGovernancePage.spec.tsx` (mock fields only)

`buildStores().governance` (:87-97) **already contains**
`drepIndex: new Map([[VALID_DREP_ID, drepEntry]])` (:89) — do NOT add it again
(a second `drepIndex` key is a TS1117 duplicate-key compile error). Add only the
two missing fields alphabetically:

```ts
    showAllList: [drepEntry],
    top35DRepIds: new Set<string>(),
```

**No assertion in this file changes.**

#### Step 21: Update `storybook/stories/governance/DRepDirectory.stories.tsx` (compile fixes only — no new stories)

**21a.** In `renderDirectory`, add three props to the `<DRepDirectory>` JSX
(after the `drepList={entries}` line):

```tsx
    drepIndex={new Map(entries.map((e) => [e.drepId, e]))}
    showAllList={entries}
    top35DRepIds={new Set<string>()}
```

**21b.** The `'Ranking unavailable'` story's inline `<DRepDirectory>` JSX gets
the same three props.

Banner stories need no change (the new banner props are optional — Step 12b).
No local `IntlProvider` anywhere; the global toggle comment stays.

#### Step 22: Verify, format, commit

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/eslint \
  source/renderer/app/components/governance/drep-directory/helpers.ts \
  source/renderer/app/components/governance/drep-directory/helpers.spec.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectorySearch.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx \
  source/renderer/app/components/governance/_shared/DRepEmptyState.tsx \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/containers/governance/DRepDirectoryPage.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx --ext .ts,.tsx
node_modules/.bin/jest \
  source/renderer/app/components/governance/drep-directory/helpers.spec.ts \
  tests/jest/governance/GovernanceStore.spec.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts \
  --no-coverage --runInBand   # 23/23, file untouched
yarn i18n:manage              # after the locale edits; commit its rewrites
node_modules/.bin/prettier --write <the changed .ts/.tsx/.scss files above>  # never JSON/.snap
git diff --stat               # confirm ZERO files under source/main/
```

Also grep the final diff for sinks — only pre-existing hits may appear in
context lines:

```bash
git diff | grep -nE '^\+.*(logger\.|analytics|electron-store)' || echo clean
```

Commit (subject only):

```
feat(gov): task-121 add drep id search with show-all and reachability filters
```

### Acceptance (task-121)

- [ ] 8-char post-HRP minimum: 7 chars → hint + unfiltered list; 8 chars →
      prefix filter (both boundaries Jest-pinned in helpers AND the component)
- [ ] Prefix matches never auto-select or open — including Enter on a unique
      match; exact checksum-valid full IDs (both encodings) open the detail view
      once with the canonical CIP-129 id
- [ ] Dual-encoding search deduped by credential (one row via either form,
      helpers-pinned); invalid full-form → "Invalid DRep ID" + zero
      channel/refresh calls (pre-IPC validation, container-pinned)
- [ ] Filters: status, metadata, expiry window (6/7/12/13 edges), top-35
      exclusion, default-cohort/show-all view, favorited predicate (injected
      set; no UI — PRD D-3), search — each Jest-covered
- [ ] Top-35, sub-floor, and inactive DReps reachable via show-all and via
      search; a non-cohort-only entry is found by ≥8-char prefix search and
      opened by exact full-ID entry with show-all off (component-pinned); store
      `showAllList` keeps full membership; reshuffle reorders it with zero IPC
- [ ] Show-all sorts opt-in with BigNumber-lossless comparison and nulls-last;
      sort-bias disclosure tied to votingPowerDesc
- [ ] Banner filtered line (D-4 key) replaces cohort claim + Reshuffle + BMVG
      whenever the default view no longer applies; noResults state offers
      working Clear-filters / Show-all
- [ ] 21 `!!!` keys per locale via `yarn i18n:manage`; no `filter.favorited`
- [ ] Floor suite 23/23; tsc zero errors; scoped eslint clean; zero
      `source/main/` changes; one subject-only commit

---

## i18n Keys (whole slice — 21 per locale, all `!!!`-prefixed)

| Key | Source | en source (after `!!!`) |
|---|---|---|
| `governance.drepDirectory.searchPlaceholder` | §9 :167 | Search by DRep ID |
| `governance.drepDirectory.cohortBanner.showAll` | §9 :169 | Show all DReps |
| `governance.drepDirectory.filter.active` | §9 :173 | Status |
| `governance.drepDirectory.filter.metadata` | §9 :174 | Metadata |
| `governance.drepDirectory.empty.noResults` | §9 :176 | No DReps match your filters. {ClearFilters} or {ShowAll}. |
| `governance.drepDirectory.showAll.sortBiasWarning` | §9 :218 | Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing. |
| `governance.drepDirectory.cohortBanner.filtered` | minted (PRD D-4) | Showing {n} DReps matching your filters. Default randomized order does not apply. |
| `governance.drepDirectory.search.minLengthHint` | minted (§11 :241 copy) | Enter at least 8 characters to search by ID |
| `governance.drepDirectory.search.invalidId` | minted (task-103 copy twin) | Invalid DRep ID |
| `governance.drepDirectory.empty.noResults.clearFilters` | minted | Clear filters |
| `governance.drepDirectory.filter.all` | minted | All |
| `governance.drepDirectory.filter.expiry` | minted | Expiry |
| `governance.drepDirectory.filter.expiry.thresholdWindow` | minted | Expiring in 7–12 epochs |
| `governance.drepDirectory.filter.metadata.with` | minted | With metadata |
| `governance.drepDirectory.filter.metadata.without` | minted | Without metadata |
| `governance.drepDirectory.filter.excludeTop35` | minted | Exclude the 35 largest |
| `governance.drepDirectory.sort.label` | minted | Sort |
| `governance.drepDirectory.sort.randomized` | minted | Randomized (default) |
| `governance.drepDirectory.sort.votingPowerDesc` | minted | Voting power (high to low) |
| `governance.drepDirectory.sort.votingPowerAsc` | minted | Voting power (low to high) |
| `governance.drepDirectory.sort.expiryAsc` | minted | Expiry (soonest first) |

NOT added this slice: `filter.favorited` (task-122 — PRD D-3), any favorites
key, `category.highValue`, any status key, any top-35-badge key (PRD P-13).

---

## Cross-Cutting Acceptance

- [ ] `node_modules/.bin/tsc --noEmit` exits 0
- [ ] Sanitization floor suite 23/23; the suite file byte-identical
- [ ] Zero new `logger.*` / analytics / electron-store calls anywhere in the
      diff; the search query never reaches any sink
- [ ] Zero changes under `source/main/`; `git diff --stat` confirms
- [ ] `displayedDRepList`, `defaultCohort` semantics, `fetchDRepList`,
      `_enrichVotingPower`, and `reshuffleCohort` behavior unchanged (Step 3
      touches only the canonical-sort extraction and adds two computeds)
- [ ] No `.toNumber()`/`Number(` on any voting-power value in the diff
- [ ] Every new string `!!!`-prefixed in BOTH locales; no existing `!!!` removed
- [ ] One subject-only commit; never push

---

## References

- PRD: [slice-6-PRD.md](./slice-6-PRD.md) (D-1…D-5, P-1…P-16, invariants table)
- Precedent guide: [slice-5-implementation-guide.md](./slice-5-implementation-guide.md)
  (devcontainer/tooling conventions carried forward)
- Design contracts: [shared-design-tokens.md](../designs/shared-design-tokens.md)
  §11 :237-246, §9 :157-222, §5 :80-87;
  [drep-discovery-design.md](../designs/drep-discovery-design.md) :157-186,
  :217-236
- Research: [slice-5-findings.md](../research/slice-5-findings.md) (F-2, F-4,
  F-5, F-6), [slice-4-findings.md](../research/slice-4-findings.md) (F-6),
  [ux-refinement-findings.md](../research/ux-refinement-findings.md) (F-7, F-9)
